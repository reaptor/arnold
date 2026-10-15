// Lines of one side of a diff matched to the other side.

import type { Chunk } from "@codemirror/merge";
import type { Text } from "@codemirror/state";

/**
 * The line of `to` that matches `line` of `from`, given the diff's chunks
 * (`fromA`: `from` is the base side). A line inside a change matches the
 * change's last line on the other side.
 */
export function matchingLine(line: number, chunks: readonly Chunk[], from: Text, to: Text, fromA: boolean): number {
  const pos = from.line(Math.min(Math.max(line, 1), from.lines)).from;
  let offset = 0;
  for (const chunk of chunks) {
    const [start, end, otherStart, otherEnd] = fromA
      ? [chunk.fromA, chunk.toA, chunk.fromB, chunk.toB]
      : [chunk.fromB, chunk.toB, chunk.fromA, chunk.toA];
    if (pos < start) break;
    if (pos < end) {
      return otherEnd > otherStart
        ? to.lineAt(Math.min(otherEnd - 1, to.length)).number
        : Math.max(to.lineAt(Math.min(otherStart, to.length)).number - 1, 1);
    }
    offset = otherEnd - end;
  }
  return to.lineAt(Math.min(Math.max(pos + offset, 0), to.length)).number;
}
