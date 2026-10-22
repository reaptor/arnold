import { SearchQuery } from "@codemirror/search";
import { EditorState } from "@codemirror/state";
import { describe, expect, it } from "vitest";
import { countText, matchesOf, MOST_COUNTED, nextMatch, previousMatch } from "./findBar";

const state = EditorState.create({ doc: "let a = 1\nlet ab = a + 1\nLET b = ab" });
const literal = (search: string, options: Partial<{ caseSensitive: boolean; wholeWord: boolean; regexp: boolean }> = {}) =>
  new SearchQuery({ search, literal: true, ...options });

describe("matchesOf", () => {
  it("finds every match, ignoring case unless asked", () => {
    expect(matchesOf(literal("let"), state)).toEqual([
      { from: 0, to: 3 },
      { from: 10, to: 13 },
      { from: 25, to: 28 },
    ]);
    expect(matchesOf(literal("let", { caseSensitive: true }), state)).toHaveLength(2);
  });

  it("finds whole words only when asked", () => {
    expect(matchesOf(literal("a"), state)).toHaveLength(4);
    expect(matchesOf(literal("a", { wholeWord: true }), state)).toEqual([
      { from: 4, to: 5 },
      { from: 19, to: 20 },
    ]);
  });

  it("leaves out a regular expression's empty matches", () => {
    expect(matchesOf(literal("x*", { regexp: true }), state)).toEqual([]);
  });

  it("finds nothing for an invalid query", () => {
    expect(matchesOf(literal("(", { regexp: true }), state)).toEqual([]);
    expect(matchesOf(literal(""), state)).toEqual([]);
  });
});

describe("stepping", () => {
  const query = literal("let");

  it("goes to the next match from a position, wrapping around", () => {
    expect(nextMatch(query, state, 0)).toEqual({ from: 0, to: 3 });
    expect(nextMatch(query, state, 3)).toEqual({ from: 10, to: 13 });
    expect(nextMatch(query, state, 26)).toEqual({ from: 0, to: 3 });
  });

  it("goes to the previous match before a position, wrapping around", () => {
    expect(previousMatch(query, state, 10)).toEqual({ from: 0, to: 3 });
    expect(previousMatch(query, state, 11)).toEqual({ from: 10, to: 13 });
    expect(previousMatch(query, state, 0)).toEqual({ from: 25, to: 28 });
  });

  it("finds nothing without matches", () => {
    expect(nextMatch(literal("zzz"), state, 0)).toBeNull();
    expect(previousMatch(literal("zzz"), state, 0)).toBeNull();
  });
});

describe("countText", () => {
  const matches = [
    { from: 0, to: 3 },
    { from: 10, to: 13 },
  ];

  it("tells which match is selected", () => {
    expect(countText(matches, { from: 10, to: 13 }, true)).toBe("2 of 2");
  });

  it("tells how many there are when none is selected", () => {
    expect(countText(matches, { from: 4, to: 4 }, true)).toBe("2 results");
    expect(countText(matches.slice(0, 1), { from: 4, to: 4 }, true)).toBe("1 result");
  });

  it("says when nothing was found, and nothing before searching", () => {
    expect(countText([], { from: 0, to: 0 }, true)).toBe("No results");
    expect(countText([], { from: 0, to: 0 }, false)).toBe("");
  });

  it("stops counting at the limit", () => {
    const many = Array.from({ length: MOST_COUNTED + 1 }, (_, index) => ({ from: index * 2, to: index * 2 + 1 }));
    expect(countText(many, { from: 2, to: 3 }, true)).toBe(`2 of ${MOST_COUNTED}+`);
  });
});
