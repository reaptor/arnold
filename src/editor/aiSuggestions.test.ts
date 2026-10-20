import { describe, expect, it, vi } from "vitest";

// The cards render Markdown, which needs a browser; only the text matching is tested here.
vi.mock("../components/Markdown", () => ({}));

const { locate, replaceLines } = await import("./aiSuggestions");

const text = "module Orders\n\nlet total xs =\n    xs |> List.sum\n\nlet total xs =\n    xs |> List.sum\n";

describe("locate", () => {
  it("finds the lines nearest to the line given", () => {
    const original = "let total xs =\n    xs |> List.sum";
    expect(locate(text, original, 3)).toEqual({ from: 3, to: 4 });
    expect(locate(text, original, 7)).toEqual({ from: 6, to: 7 });
  });

  it("ignores trailing spaces, line ends and, failing that, indentation", () => {
    expect(locate("a\r\n  b  \r\nc\r\n", "  b\n", 1)).toEqual({ from: 2, to: 2 });
    expect(locate(text, "xs |> List.sum", 4)).toEqual({ from: 4, to: 4 });
  });

  it("is null when the lines are not there, or blank", () => {
    expect(locate(text, "let total ys =", 3)).toBeNull();
    expect(locate(text, "\n", 2)).toBeNull();
  });
});

describe("replaceLines", () => {
  it("replaces whole lines, keeping the text's line ends", () => {
    expect(replaceLines("a\r\nb\r\nc\r\n", 2, 2, "B1\nB2\n")).toBe("a\r\nB1\r\nB2\r\nc\r\n");
  });

  it("removes lines for an empty replacement", () => {
    expect(replaceLines("a\nb\nc\n", 2, 2, "")).toBe("a\nc\n");
    expect(replaceLines("a\nb", 2, 2, "")).toBe("a");
  });
});
