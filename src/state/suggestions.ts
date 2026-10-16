// Edits to a pull request's files, as Azure DevOps code suggestions: each
// block of changed lines becomes a comment on the lines it replaces (in the
// pull request's latest push) holding a ```suggestion block with the new
// lines, which the author can apply with a click. Lines only added have
// nothing to replace, so they go with the line before them (or after, at the
// top of the file).

import { type LineBlock, lineBlocks } from "../editor/lineDiff";

export interface Suggestion {
  path: string;
  /** The lines replaced, 1-based and inclusive, in the pull request's text. */
  startLine: number;
  endLine: number;
  /** Those lines, and what they become. */
  old: string[];
  new: string[];
  /** The change it comes from, for putting it back once suggested. */
  block: LineBlock;
}

/** A file's lines, without the empty one after a final line break. */
export function linesOf(text: string): string[] {
  if (text === "") return [];
  return (text.endsWith("\n") ? text.slice(0, -1) : text).split("\n");
}

/**
 * The suggestions turning `committed` (the file in the pull request) into
 * `working` (the edited file). A change to an empty file has no line to go
 * on, so it is left out.
 */
export function suggestionsFor(path: string, committed: string, working: string): Suggestion[] {
  const a = linesOf(committed);
  const b = linesOf(working);
  const result: Suggestion[] = [];
  for (const block of lineBlocks(a, b)) {
    const added = b.slice(block.fromB, block.toB);
    if (block.toA > block.fromA) {
      result.push({ path, startLine: block.fromA + 1, endLine: block.toA, old: a.slice(block.fromA, block.toA), new: added, block });
    } else if (block.fromA > 0) {
      const before = a[block.fromA - 1];
      result.push({ path, startLine: block.fromA, endLine: block.fromA, old: [before], new: [before, ...added], block });
    } else if (a.length > 0) {
      result.push({ path, startLine: 1, endLine: 1, old: [a[0]], new: [...added, a[0]], block });
    }
  }
  return result;
}

/** The comment for `suggestion`: the message, if any, then the suggestion block. */
export function suggestionComment(suggestion: Suggestion, message: string): string {
  const intro = message.trim() ? `${message.trim()}\n\n` : "";
  const body = suggestion.new.length > 0 ? `${suggestion.new.join("\n")}\n` : "";
  return `${intro}\`\`\`suggestion\n${body}\`\`\``;
}

/** Where the comment goes: the replaced lines, to the end of the last one. */
export const suggestionPlace = (suggestion: Suggestion) => ({
  side: "head" as const,
  startLine: suggestion.startLine,
  startOffset: 1,
  endLine: suggestion.endLine,
  endOffset: (suggestion.old.at(-1)?.length ?? 0) + 1,
});

/**
 * `working` with the changes of `blocks` (suggestions made of it) put back
 * to how `committed` has them; its other edits stay.
 */
export function withoutBlocks(committed: string, working: string, blocks: readonly LineBlock[]): string {
  const a = linesOf(committed);
  const b = linesOf(working);
  for (const block of [...blocks].sort((x, y) => y.fromB - x.fromB)) {
    b.splice(block.fromB, block.toB - block.fromB, ...a.slice(block.fromA, block.toA));
  }
  const lineBreak = working.endsWith("\n") || (working === "" && committed.endsWith("\n"));
  return b.length === 0 ? "" : b.join("\n") + (lineBreak ? "\n" : "");
}
