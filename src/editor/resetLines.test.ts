import { describe, expect, it } from "vitest";
import { lineBlocks } from "./lineDiff";
import { applyEdits, carryOver, dropSelectedLines, resetEdits, takeSelectedLines, type LineSelection } from "./resetLines";

const reset = (base: string, work: string, selection: LineSelection) =>
  applyEdits(work, resetEdits(base, work, selection));

const lines = (...items: string[]) => items.join("\n") + "\n";

describe("lineBlocks", () => {
  it("finds changed line ranges", () => {
    const a = ["one", "two", "three", "four"];
    const b = ["one", "TWO", "three", "new", "four"];
    expect(lineBlocks(a, b)).toEqual([
      { fromA: 1, toA: 2, fromB: 1, toB: 2 },
      { fromA: 3, toA: 3, fromB: 3, toB: 4 },
    ]);
  });
});

describe("resetEdits from the working side", () => {
  const base = lines("a", "b", "c", "d", "e");

  it("resets only the selected line of a same-size change", () => {
    const work = lines("a", "B", "C", "d", "e");
    expect(reset(base, work, { side: "b", from: 1, to: 1 })).toBe(lines("a", "b", "C", "d", "e"));
    expect(reset(base, work, { side: "b", from: 2, to: 2 })).toBe(lines("a", "B", "c", "d", "e"));
    expect(reset(base, work, { side: "b", from: 1, to: 2 })).toBe(base);
  });

  it("removes only the selected added lines", () => {
    const work = lines("a", "b", "new 1", "new 2", "new 3", "c", "d", "e");
    expect(reset(base, work, { side: "b", from: 3, to: 3 })).toBe(lines("a", "b", "new 1", "new 3", "c", "d", "e"));
    expect(reset(base, work, { side: "b", from: 2, to: 3 })).toBe(lines("a", "b", "new 3", "c", "d", "e"));
    expect(reset(base, work, { side: "b", from: 0, to: 7 })).toBe(base);
  });

  it("resets a replacement of different size as a whole when all of it is selected", () => {
    const work = lines("a", "x", "y", "z", "d", "e");
    expect(reset(base, work, { side: "b", from: 1, to: 3 })).toBe(base);
  });

  it("removes only the selected lines of part of a replacement of different size", () => {
    const work = lines("a", "x", "y", "z", "d", "e");
    expect(reset(base, work, { side: "b", from: 2, to: 2 })).toBe(lines("a", "x", "z", "d", "e"));
  });

  it("leaves changes outside the selection alone", () => {
    const work = lines("A", "b", "c", "d", "E");
    expect(reset(base, work, { side: "b", from: 0, to: 0 })).toBe(lines("a", "b", "c", "d", "E"));
    expect(reset(base, work, { side: "b", from: 2, to: 3 })).toBe(work);
  });

  it("restores deleted lines when the selection spans the gap", () => {
    const work = lines("a", "b", "e");
    expect(reset(base, work, { side: "b", from: 1, to: 2 })).toBe(base);
    // A cursor on one neighbouring line is not enough.
    expect(reset(base, work, { side: "b", from: 1, to: 1 })).toBe(work);
  });

  it("handles changes at the very end, with and without a final newline", () => {
    expect(reset("a\nb", "a\nb\nc", { side: "b", from: 2, to: 2 })).toBe("a\nb");
    // Lines removed after the last line: the cursor on that line is enough.
    expect(reset("a\nb\nc", "a", { side: "b", from: 0, to: 0 })).toBe("a\nb\nc");
    expect(reset("a\nb\n", "a\nb", { side: "b", from: 1, to: 1 })).toBe("a\nb\n");
    expect(reset("a\nb", "a\nB\n", { side: "b", from: 1, to: 2 })).toBe("a\nb");
  });

  it("resets a whole file", () => {
    expect(reset("", "new file\ncontent\n", { side: "b", from: 0, to: 2 })).toBe("");
    expect(reset(base, "", { side: "b", from: 0, to: 0 })).toBe(base);
  });
});

describe("resetEdits from the base side", () => {
  const base = lines("a", "b", "c", "d", "e");

  it("restores only the selected deleted lines", () => {
    const work = lines("a", "e");
    expect(reset(base, work, { side: "a", from: 2, to: 2 })).toBe(lines("a", "c", "e"));
    expect(reset(base, work, { side: "a", from: 1, to: 3 })).toBe(base);
  });

  it("puts selected lines of part of a replacement back after its working lines", () => {
    const work = lines("a", "x", "e");
    expect(reset(base, work, { side: "a", from: 2, to: 2 })).toBe(lines("a", "x", "c", "e"));
    expect(reset(base, work, { side: "a", from: 1, to: 3 })).toBe(base);
  });

  it("resets the matching line of a same-size change", () => {
    const work = lines("a", "B", "C", "d", "e");
    expect(reset(base, work, { side: "a", from: 2, to: 2 })).toBe(lines("a", "B", "c", "d", "e"));
  });

  it("ignores selections away from any change", () => {
    const work = lines("a", "b", "c", "d", "E");
    expect(reset(base, work, { side: "a", from: 0, to: 2 })).toBe(work);
  });
});

describe("resetting everything", () => {
  it("always gives back the base text", () => {
    // Small deterministic generator, so failures are reproducible.
    let seed = 42;
    const random = (n: number) => {
      seed = (seed * 1103515245 + 12345) & 0x7fffffff;
      return seed % n;
    };
    const randomText = () => {
      const count = random(8);
      const body = Array.from({ length: count }, () => "line " + random(5)).join("\n");
      return random(2) ? body + "\n" : body;
    };
    for (let i = 0; i < 500; i++) {
      const base = randomText();
      const work = randomText();
      for (const side of ["a", "b"] as const) {
        const lineCount = (side === "a" ? base : work).split("\n").length;
        expect(reset(base, work, { side, from: 0, to: lineCount - 1 })).toBe(base);
      }
    }
  });
});

describe("staging selected lines", () => {
  const index = lines("a", "b", "c", "d");
  const work = lines("a", "B", "c", "new", "d");

  it("takes only the selected change into the index", () => {
    expect(takeSelectedLines(index, work, { side: "b", from: 1, to: 1 })).toBe(lines("a", "B", "c", "d"));
    expect(takeSelectedLines(index, work, { side: "b", from: 3, to: 3 })).toBe(lines("a", "b", "c", "new", "d"));
    expect(takeSelectedLines(index, work, { side: "b", from: 0, to: 4 })).toBe(work);
  });

  it("takes a removal selected on the base side", () => {
    const removed = lines("a", "c", "d");
    expect(takeSelectedLines(index, removed, { side: "a", from: 1, to: 1 })).toBe(removed);
  });

  it("stages one added line next to an edited one, keeping the old line", () => {
    const head = lines("x", "end");
    const edited = lines("x // edited", "added 1", "added 2", "end");
    expect(takeSelectedLines(head, edited, { side: "b", from: 2, to: 2 })).toBe(lines("x", "added 2", "end"));
    expect(dropSelectedLines(head, edited, { side: "b", from: 2, to: 2 })).toBe(lines("x // edited", "added 1", "end"));
  });

  it("keeps the final newline when a blank line comes before the staged lines", () => {
    const head = lines("x");
    const edited = lines("x", "", "/// doc", "let y", "body");
    expect(takeSelectedLines(head, edited, { side: "b", from: 3, to: 3 })).toBe(lines("x", "let y"));
  });

  it("stages lines of a file new to the index", () => {
    expect(takeSelectedLines("", lines("one", "two"), { side: "b", from: 0, to: 0 })).toBe(lines("one"));
  });

  it("returns null when the selection has no change", () => {
    expect(takeSelectedLines(index, work, { side: "b", from: 0, to: 0 })).toBeNull();
    expect(dropSelectedLines(index, work, { side: "b", from: 2, to: 2 })).toBeNull();
  });

  it("unstages by dropping the selected lines from the index", () => {
    const head = lines("a", "b", "c", "d");
    const staged = lines("a", "B", "c", "D");
    expect(dropSelectedLines(head, staged, { side: "b", from: 3, to: 3 })).toBe(lines("a", "B", "c", "d"));
  });
});

describe("carryOver", () => {
  const before = lines("1", "2", "3", "4", "5", "6", "7", "8");

  it("repeats a change in a text edited elsewhere", () => {
    const after = lines("1", "2", "3", "4", "5", "6", "SEVEN", "8");
    const target = lines("0", "1", "2", "3", "4", "5", "6", "7", "8");
    expect(carryOver(before, after, target)).toBe(lines("0", "1", "2", "3", "4", "5", "6", "SEVEN", "8"));
  });

  it("removes and inserts lines", () => {
    const after = lines("1", "3", "4", "5", "6", "7", "inserted", "8");
    expect(carryOver(before, after, before)).toBe(after);
  });

  it("refuses when the target changed the same lines or next to them", () => {
    const after = lines("1", "2", "3", "FOUR", "5", "6", "7", "8");
    expect(carryOver(before, after, lines("1", "2", "3", "four", "5", "6", "7", "8"))).toBeNull();
    expect(carryOver(before, after, lines("1", "2", "3", "4", "5!", "6", "7", "8"))).toBeNull();
  });
});
