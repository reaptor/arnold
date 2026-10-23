import { describe, expect, it } from "vitest";
import { linesOf, suggestionComment, suggestionPlace, suggestionsFor, withoutBlocks } from "./suggestions";

const committed = "one\ntwo\nthree\nfour\nfive\n";

describe("suggestionsFor", () => {
  it("replaces changed lines with the new ones", () => {
    const [only, ...rest] = suggestionsFor("a.fs", committed, "one\nTWO\n2b\nthree\nfour\nfive\n");
    expect(rest).toEqual([]);
    expect(only).toMatchObject({ startLine: 2, endLine: 2, old: ["two"], new: ["TWO", "2b"] });
    expect(suggestionPlace(only)).toEqual({ side: "head", startLine: 2, startOffset: 1, endLine: 2, endOffset: 4 });
  });

  it("puts added lines with the line before them, or after at the top", () => {
    const [after, top] = [
      suggestionsFor("a.fs", committed, "one\ntwo\nthree\nnew\nfour\nfive\n")[0],
      suggestionsFor("a.fs", committed, "zero\none\ntwo\nthree\nfour\nfive\n")[0],
    ];
    expect(after).toMatchObject({ startLine: 3, endLine: 3, old: ["three"], new: ["three", "new"] });
    expect(top).toMatchObject({ startLine: 1, endLine: 1, old: ["one"], new: ["zero", "one"] });
  });

  it("suggests removing lines with an empty block", () => {
    const [removed] = suggestionsFor("a.fs", committed, "one\nfour\nfive\n");
    expect(removed).toMatchObject({ startLine: 2, endLine: 3, old: ["two", "three"], new: [] });
    expect(suggestionComment(removed, "")).toBe("```suggestion\n```");
  });

  it("keeps separate changes apart, and leaves an empty file out", () => {
    const blocks = suggestionsFor("a.fs", committed, "ONE\ntwo\nthree\nfour\nFIVE\n");
    expect(blocks.map((s) => [s.startLine, s.endLine])).toEqual([[1, 1], [5, 5]]);
    expect(suggestionsFor("a.fs", "", "new\n")).toEqual([]);
  });
});

describe("suggestionComment", () => {
  it("puts the message before the suggestion", () => {
    const [only] = suggestionsFor("a.fs", committed, "one\nTWO\nthree\nfour\nfive\n");
    expect(suggestionComment(only, "  Capitals here. \n")).toBe("Capitals here.\n\n```suggestion\nTWO\n```");
  });
});

describe("withoutBlocks", () => {
  it("puts back only the suggested changes", () => {
    const working = "ONE\ntwo\nthree\nfour\nFIVE\nsix\n";
    const [first, last] = suggestionsFor("a.fs", committed, working);
    expect(withoutBlocks(committed, working, [first.block])).toBe("one\ntwo\nthree\nfour\nFIVE\nsix\n");
    expect(withoutBlocks(committed, working, [first.block, last.block])).toBe(committed);
  });

  it("keeps the file's final line break as it is", () => {
    const working = "one\nTWO\nthree\nfour\nfive";
    const [only] = suggestionsFor("a.fs", committed, working);
    expect(withoutBlocks(committed, working, [only.block])).toBe("one\ntwo\nthree\nfour\nfive");
    expect(linesOf("")).toEqual([]);
  });
});
