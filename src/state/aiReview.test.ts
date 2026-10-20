import { describe, expect, it, vi } from "vitest";

// The review state reads the rest of the app's state; only its pure parts are tested here.
vi.mock("./ai", () => ({}));
vi.mock("./review", () => ({}));
vi.mock("../ipc/commands", () => ({}));
vi.mock("../components/Markdown", () => ({}));

const { numbered, parseReview } = await import("./aiReview");

describe("parseReview", () => {
  const suggestion = {
    path: "src/Orders.fs",
    line: 4,
    original: "    xs |> List.sum",
    replacement: "    xs |> List.sumBy id",
    title: "Use sumBy",
    why: "As `src/Lines.fs:9` does.",
    basis: "pattern",
  };

  it("reads the summary and suggestions, also from a fence with words around it", () => {
    const text = `Here you go:\n\`\`\`json\n${JSON.stringify({ summary: "Fine.", suggestions: [suggestion] })}\n\`\`\``;
    expect(parseReview(text)).toEqual({ summary: "Fine.", suggestions: [suggestion] });
  });

  it("leaves out suggestions without lines or without a change, and tidies paths", () => {
    const parsed = parseReview(
      JSON.stringify({
        summary: "",
        suggestions: [
          { ...suggestion, path: "b/src/Orders.fs", basis: "taste" },
          { ...suggestion, original: "  " },
          { ...suggestion, replacement: suggestion.original },
          { ...suggestion, path: "" },
          "nonsense",
        ],
      }),
    );
    expect(parsed.suggestions).toEqual([{ ...suggestion, basis: "proven" }]);
  });

  it("fails on an answer without JSON", () => {
    expect(() => parseReview("No suggestions.")).toThrow("no suggestions");
    expect(() => parseReview("{ not json }")).toThrow("could not be read");
  });
});

describe("numbered", () => {
  it("numbers lines as the review instructions describe", () => {
    const lines = Array.from({ length: 10 }, (_, index) => `line ${index + 1}`).join("\n");
    expect(numbered(`${lines}\n`).split("\n").slice(8)).toEqual([" 9| line 9", "10| line 10"]);
  });
});
