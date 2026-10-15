import type { Chunk } from "@codemirror/merge";
import { Text } from "@codemirror/state";
import { describe, expect, it } from "vitest";
import { matchingLine } from "./matchingLine";

// Base a..g; the working text replaces "c" with two lines and drops "f".
const base = Text.of(["a", "b", "c", "d", "e", "f", "g"]);
const working = Text.of(["a", "b", "c1", "c2", "d", "e", "g"]);
const start = (doc: Text, line: number) => doc.line(line).from;
const chunks = [
  { fromA: start(base, 3), toA: start(base, 4), fromB: start(working, 3), toB: start(working, 5) },
  { fromA: start(base, 6), toA: start(base, 7), fromB: start(working, 7), toB: start(working, 7) },
] as unknown as Chunk[];

describe("matchingLine", () => {
  it("follows unchanged lines across changes", () => {
    expect(matchingLine(2, chunks, base, working, true)).toBe(2);
    expect(matchingLine(4, chunks, base, working, true)).toBe(5);
    expect(matchingLine(7, chunks, base, working, true)).toBe(7);
    expect(matchingLine(5, chunks, working, base, false)).toBe(4);
  });

  it("puts a line inside a change under the change's last line on the other side", () => {
    expect(matchingLine(3, chunks, base, working, true)).toBe(4);
    expect(matchingLine(4, chunks, working, base, false)).toBe(3);
  });

  it("puts a removed line under the line before the removal", () => {
    expect(matchingLine(6, chunks, base, working, true)).toBe(6);
  });
});
