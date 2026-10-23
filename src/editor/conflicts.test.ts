import { describe, expect, it } from "vitest";
import { findConflicts, type Resolution, resolvedText } from "./conflicts";

const lines = (...items: string[]) => items.join("\n") + "\n";

const resolve = (text: string, choice: Resolution, which = 0) => {
  const conflict = findConflicts(text)[which];
  return text.slice(0, conflict.from) + resolvedText(conflict, choice, text) + text.slice(conflict.to);
};

const MERGED = lines("one", "<<<<<<< HEAD", "2", "=======", "TWO", "zwei", ">>>>>>> feature/x", "three");

describe("findConflicts", () => {
  it("finds the sides and labels of each conflict", () => {
    const [conflict] = findConflicts(MERGED);
    expect(conflict).toMatchObject({
      start: 2,
      base: null,
      middle: 4,
      end: 7,
      current: "HEAD",
      incoming: "feature/x",
      ours: ["2"],
      theirs: ["TWO", "zwei"],
    });
    expect(MERGED.slice(conflict.from, conflict.to)).toBe(lines("<<<<<<< HEAD", "2", "=======", "TWO", "zwei", ">>>>>>> feature/x"));
  });

  it("reads the common ancestor git writes with diff3 and leaves it out of the sides", () => {
    const text = lines("<<<<<<< ours", "a", "||||||| base", "o", "=======", "b", ">>>>>>> theirs");
    expect(findConflicts(text)).toMatchObject([{ base: 3, middle: 5, ours: ["a"], theirs: ["b"] }]);
  });

  it("finds several conflicts and skips markers that make none", () => {
    const text = lines("<<<<<<< stray", "x", "<<<<<<<", "a", "=======", ">>>>>>>", "=======", "<<<<<<< HEAD", "=======", "b", ">>>>>>> m");
    const found = findConflicts(text);
    expect(found.map((conflict) => [conflict.start, conflict.ours, conflict.theirs])).toEqual([
      [3, ["a"], []],
      [8, [], ["b"]],
    ]);
  });

  it("takes text that only looks like a marker as text", () => {
    expect(findConflicts(lines("<<<<<<<< x", "=======", ">>>>>>>"))).toEqual([]);
  });
});

describe("resolvedText", () => {
  it("keeps the current side, the incoming side or both", () => {
    expect(resolve(MERGED, "current")).toBe(lines("one", "2", "three"));
    expect(resolve(MERGED, "incoming")).toBe(lines("one", "TWO", "zwei", "three"));
    expect(resolve(MERGED, "both")).toBe(lines("one", "2", "TWO", "zwei", "three"));
  });

  it("keeps a last line without a line break so", () => {
    const text = "one\n<<<<<<< HEAD\n2\n=======\nTWO\n>>>>>>> x";
    expect(resolve(text, "incoming")).toBe("one\nTWO");
    expect(resolve(text, "both")).toBe("one\n2\nTWO");
  });

  it("removes the conflict when the side kept is empty", () => {
    expect(resolve(lines("a", "<<<<<<< HEAD", "=======", "b", ">>>>>>> x", "c"), "current")).toBe(lines("a", "c"));
  });
});
