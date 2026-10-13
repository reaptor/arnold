// "Reset selected lines": put the selected part of a change back to how it is
// in the base version, as an ordinary (undoable) edit of the working text.

import { diff } from "@codemirror/merge";

/** A changed region in line numbers (0-based, end exclusive). */
export interface LineBlock {
  fromA: number;
  toA: number;
  fromB: number;
  toB: number;
}

/** Selected lines (0-based, inclusive) on the base side (`a`) or the working side (`b`). */
export interface LineSelection {
  side: "a" | "b";
  from: number;
  to: number;
}

/** A replacement in the working text, by character offset. */
export interface TextEdit {
  from: number;
  to: number;
  insert: string;
}

// Lines are diffed by giving each distinct line one UTF-16 code unit and
// diffing the resulting strings. Surrogates are skipped so that one unit
// always stands for one line.
const FIRST_UNIT = 1;
const SURROGATES_START = 0xd800;
const SURROGATES_END = 0xdfff;
const LAST_UNIT = 0xffff;

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
  return diff(encoded[0], encoded[1]).map(({ fromA, toA, fromB, toB }) => ({ fromA, toA, fromB, toB }));
}

function touches(block: LineBlock, selection: LineSelection, lineCount: number): boolean {
  const [from, to] = selection.side === "a" ? [block.fromA, block.toA] : [block.fromB, block.toB];
  if (to > from) return selection.from < to && selection.to >= from;
  // Nothing of this change is visible on the selected side (lines that exist
  // only on the other side). It counts as selected when the selection covers
  // the lines on both sides of the place where those lines belong; at the very
  // start or end of the text there is only one such line.
  const before = Math.max(from - 1, 0);
  const after = Math.min(from, lineCount - 1);
  return selection.from <= before && selection.to >= after;
}

/**
 * Edits that reset the selected lines of `workText` to `baseText`.
 *
 * - Added lines: only the selected ones are removed.
 * - Removed lines: only the ones selected on the base side are put back
 *   (all of them when the gap is selected on the working side).
 * - A change with the same number of lines on both sides is reset line by
 *   line: only the selected lines go back.
 * - Lines replaced by a different number of lines have no line-to-line
 *   correspondence, so that change is reset as a whole.
 */
export function resetEdits(baseText: string, workText: string, selection: LineSelection): TextEdit[] {
  const aLines = baseText.split("\n");
  const bLines = workText.split("\n");
  const starts: number[] = [];
  let offset = 0;
  for (const line of bLines) {
    starts.push(offset);
    offset += line.length + 1;
  }
  // One past the end, so that `starts[n] - 1` is the end of the text.
  starts.push(workText.length + 1);

  const replaceLines = (from: number, to: number, lines: string[]): TextEdit => {
    if (to < bLines.length) {
      return { from: starts[from], to: starts[to], insert: lines.map((line) => line + "\n").join("") };
    }
    if (from === 0) return { from: 0, to: workText.length, insert: lines.join("\n") };
    // The replaced lines run to the end of the text: take the preceding newline with them.
    return { from: starts[from] - 1, to: workText.length, insert: lines.map((line) => "\n" + line).join("") };
  };

  const selectedLineCount = selection.side === "a" ? aLines.length : bLines.length;
  const edits: TextEdit[] = [];
  for (const block of lineBlocks(aLines, bLines)) {
    if (!touches(block, selection, selectedLineCount)) continue;
    const removed = block.toA - block.fromA;
    const added = block.toB - block.fromB;
    if (removed === 0 && selection.side === "b") {
      const from = Math.max(block.fromB, selection.from);
      const to = Math.min(block.toB, selection.to + 1);
      edits.push(replaceLines(from, to, []));
    } else if (added === 0 && selection.side === "a") {
      const from = Math.max(block.fromA, selection.from);
      const to = Math.min(block.toA, selection.to + 1);
      edits.push(replaceLines(block.fromB, block.fromB, aLines.slice(from, to)));
    } else if (removed === added) {
      const first = selection.side === "a" ? block.fromA : block.fromB;
      for (let i = 0; i < added; i++) {
        if (first + i < selection.from || first + i > selection.to) continue;
        edits.push(replaceLines(block.fromB + i, block.fromB + i + 1, [aLines[block.fromA + i]]));
      }
    } else {
      edits.push(replaceLines(block.fromB, block.toB, aLines.slice(block.fromA, block.toA)));
    }
  }
  return edits;
}

/** Applies edits (as produced by `resetEdits`) to a text. */
export function applyEdits(text: string, edits: TextEdit[]): string {
  let result = text;
  for (const edit of [...edits].sort((x, y) => y.from - x.from)) {
    result = result.slice(0, edit.from) + edit.insert + result.slice(edit.to);
  }
  return result;
}
