// Line-first diffing. CodeMirror's merge view diffs character by character
// and, past a size limit, falls back to a rough match that can mark hundreds
// of unchanged lines as changed when a few edits are spread over a large file.
// Here whole lines are matched first (like git), and characters are only
// compared inside the lines that differ, for the changed-text highlight.

import { Change, diff, type DiffConfig } from "@codemirror/merge";

/** A changed region in line numbers (0-based, end exclusive). */
export interface LineBlock {
  fromA: number;
  toA: number;
  fromB: number;
  toB: number;
}

// Lines are diffed by giving each distinct line one UTF-16 code unit and
// diffing the resulting strings. Surrogates are skipped so that one unit
// always stands for one line.
const FIRST_UNIT = 1;
const SURROGATES_START = 0xd800;
const SURROGATES_END = 0xdfff;
const LAST_UNIT = 0xffff;

/**
 * How far (in lines) the line diff searches before settling for a rougher
 * match. Keeps two very different large files from blocking the UI, and is
 * deterministic, unlike a timeout, so that every view of a file agrees.
 */
const LINE_DIFF: DiffConfig = { scanLimit: 5000 };

/** Within changed lines, CodeMirror's own defaults. */
const CHARACTER_DIFF: DiffConfig = { scanLimit: 500 };

function encodeLines(lineLists: string[][]): string[] | null {
  const units = new Map<string, number>();
  let next = FIRST_UNIT;
  const encoded: string[] = [];
  for (const lines of lineLists) {
    let out = "";
    for (const line of lines) {
      let unit = units.get(line);
      if (unit === undefined) {
        if (next === SURROGATES_START) next = SURROGATES_END + 1;
        if (next > LAST_UNIT) return null;
        unit = next++;
        units.set(line, unit);
      }
      out += String.fromCharCode(unit);
    }
    encoded.push(out);
  }
  return encoded;
}

/** Line-level differences between two versions of a file. */
export function lineBlocks(aLines: string[], bLines: string[]): LineBlock[] {
  const encoded = encodeLines([aLines, bLines]);
  if (!encoded) {
    // More distinct lines than code units: treat everything between the
    // common start and end as one change.
    let start = 0;
    while (start < aLines.length && start < bLines.length && aLines[start] === bLines[start]) start++;
    let endA = aLines.length;
    let endB = bLines.length;
    while (endA > start && endB > start && aLines[endA - 1] === bLines[endB - 1]) {
      endA--;
      endB--;
    }
    return start === endA && start === endB ? [] : [{ fromA: start, toA: endA, fromB: start, toB: endB }];
  }
  return diff(encoded[0], encoded[1], LINE_DIFF).map(({ fromA, toA, fromB, toB }) => ({
    fromA,
    toA,
    fromB,
    toB,
  }));
}

/** Lines with their line break, so that they join back into exactly `text`. */
function linesWithBreaks(text: string): string[] {
  return text.match(/[^\n]*\n|[^\n]+$/g) ?? [];
}

function offsets(lines: string[]): number[] {
  const starts = [0];
  for (const line of lines) starts.push(starts[starts.length - 1] + line.length);
  return starts;
}

/** A drop-in for CodeMirror's character diff (`DiffConfig.override`). */
export function lineFirstDiff(a: string, b: string): Change[] {
  const aLines = linesWithBreaks(a);
  const bLines = linesWithBreaks(b);
  const startsA = offsets(aLines);
  const startsB = offsets(bLines);
  const changes: Change[] = [];
  for (const block of lineBlocks(aLines, bLines)) {
    const fromA = startsA[block.fromA];
    const toA = startsA[block.toA];
    const fromB = startsB[block.fromB];
    const toB = startsB[block.toB];
    if (fromA === toA || fromB === toB) {
      changes.push(new Change(fromA, toA, fromB, toB));
      continue;
    }
    for (const change of diff(a.slice(fromA, toA), b.slice(fromB, toB), CHARACTER_DIFF)) {
      changes.push(new Change(change.fromA + fromA, change.toA + fromA, change.fromB + fromB, change.toB + fromB));
    }
  }
  return changes;
}

/** Pass as `diffConfig` to the merge views. */
export const lineFirstDiffConfig: DiffConfig = { override: lineFirstDiff };
