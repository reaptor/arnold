// Line-level work on a diff: "reset selected lines" puts the selected part of
// a change back to how it is in the base version (an ordinary, undoable edit
// of the working text); staging some lines is the same reset seen from the
// other side, the index taking the selected lines of the working tree.

import { type LineBlock, lineBlocks } from "./lineDiff";

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
 *   correspondence. Selecting all of the change on one side resets it as a
 *   whole; selecting part of it works line by line as Git Extensions does
 *   with a hunk: selected working lines are removed, selected base lines
 *   are put back after the change's working lines, and the rest stays.
 */
export function resetEdits(baseText: string, workText: string, selection: LineSelection): TextEdit[] {
  // When both end with a newline, it stays at the end: the lines are compared
  // without the empty "line" after it, which could otherwise be matched with a
  // blank line further up and have lines put after the final newline.
  if (baseText.endsWith("\n") && workText.endsWith("\n")) {
    const base = baseText.slice(0, -1);
    const work = workText.slice(0, -1);
    const last = (selection.side === "a" ? base : work).split("\n").length - 1;
    const within = { ...selection, from: Math.min(selection.from, last), to: Math.min(selection.to, last) };
    return lineEdits(base, work, within);
  }
  return lineEdits(baseText, workText, selection);
}

function lineEdits(baseText: string, workText: string, selection: LineSelection): TextEdit[] {
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
      const [first, end] = selection.side === "a" ? [block.fromA, block.toA] : [block.fromB, block.toB];
      const from = Math.max(first, selection.from);
      const to = Math.min(end, selection.to + 1);
      if (from === first && to === end) {
        edits.push(replaceLines(block.fromB, block.toB, aLines.slice(block.fromA, block.toA)));
      } else if (selection.side === "b") {
        edits.push(replaceLines(from, to, []));
      } else {
        edits.push(replaceLines(block.toB, block.toB, aLines.slice(from, to)));
      }
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

/**
 * The text `baseText` gets when the selected lines of the change from it to
 * `workText` are taken over: staging selected lines of the working tree
 * (`baseText` is the index). `selection` is on the diff's sides as shown,
 * base on side `a`.
 */
export function takeSelectedLines(baseText: string, workText: string, selection: LineSelection): string | null {
  const flipped: LineSelection = { ...selection, side: selection.side === "a" ? "b" : "a" };
  const edits = resetEdits(workText, baseText, flipped);
  return edits.length === 0 ? null : applyEdits(baseText, edits);
}

/** The text `workText` gets when the selected lines go back to `baseText`; null when they touch no change. */
export function dropSelectedLines(baseText: string, workText: string, selection: LineSelection): string | null {
  const edits = resetEdits(baseText, workText, selection);
  return edits.length === 0 ? null : applyEdits(workText, edits);
}

/**
 * Makes the change from `from` to `to` in `target` as well, a text that
 * started out as `from` and has been changed since (the working tree, when
 * staged lines are reset in the index). Null when `target` has changed the
 * lines involved or the lines right next to them, as `git apply` would
 * refuse then too.
 */
export function carryOver(from: string, to: string, target: string): string | null {
  const fromLines = from.split("\n");
  const toLines = to.split("\n");
  const targetLines = target.split("\n");
  const changes = lineBlocks(fromLines, toLines);
  const edited = lineBlocks(fromLines, targetLines);
  // Closed line ranges, so that a change right next to another also counts as touching it.
  const touches = (a: LineBlock, b: LineBlock) => a.fromA <= b.toA && b.fromA <= a.toA;
  if (changes.some((change) => edited.some((edit) => touches(change, edit)))) return null;

  const result = [...targetLines];
  for (const change of [...changes].reverse()) {
    // Lines before the change moved by what the target added or removed above them.
    const shift = edited
      .filter((edit) => edit.toA <= change.fromA)
      .reduce((sum, edit) => sum + (edit.toB - edit.fromB) - (edit.toA - edit.fromA), 0);
    result.splice(change.fromA + shift, change.toA - change.fromA, ...toLines.slice(change.fromB, change.toB));
  }
  return result.join("\n");
}
