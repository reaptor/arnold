import { describe, expect, it } from "vitest";
import { applyEdits, lineBlocks, resetEdits, type LineSelection } from "./resetLines";

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

  it("resets a replacement of different size as a whole", () => {
    const work = lines("a", "x", "y", "z", "d", "e");
    expect(reset(base, work, { side: "b", from: 2, to: 2 })).toBe(base);
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
