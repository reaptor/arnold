import { describe, expect, it } from "vitest";
import { applyTextEdits } from "./rename";

const at = (line: number, character: number) => ({ line, character });

describe("applyTextEdits", () => {
  it("applies edits on several lines, last first", () => {
    const text = "module Lib\n\nlet greeting name = name\nlet x = greeting \"a\"\n";
    const edits = [
      { range: { start: at(2, 4), end: at(2, 12) }, newText: "welcome" },
      { range: { start: at(3, 8), end: at(3, 16) }, newText: "welcome" },
    ];
    expect(applyTextEdits(text, edits)).toBe("module Lib\n\nlet welcome name = name\nlet x = welcome \"a\"\n");
  });

  it("keeps Windows line endings", () => {
    expect(applyTextEdits("a\r\nold b\r\n", [{ range: { start: at(1, 0), end: at(1, 3) }, newText: "new" }])).toBe("a\r\nnew b\r\n");
  });
});
