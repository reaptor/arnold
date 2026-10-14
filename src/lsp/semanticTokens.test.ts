import { Text } from "@codemirror/state";
import { describe, expect, it } from "vitest";
import { decode } from "./semanticTokens";

const legend = {
  tokenTypes: ["namespace", "type", "function", "property", "variable", "keyword", "cexpr", "class", "method", "member"],
  tokenModifiers: ["mutable", "readonly", "defaultLibrary"],
};

describe("decode", () => {
  const doc = Text.of(["module Shop.Cart", "", "let total (cart: Cart) = cart.Items |> List.sum"]);
  const text = (from: number, to: number) => doc.sliceString(from, to);

  it("turns relative positions into document ranges with Rider's kinds", () => {
    // prettier-ignore
    const data = [
      0, 7, 4, 0, 0,   // Shop        namespace
      0, 5, 4, 0, 0,   // Cart        namespace
      2, 4, 5, 2, 0,   // total       function
      0, 7, 4, 4, 0,   // cart        variable
      0, 6, 4, 1, 0,   // Cart        type
      0, 8, 4, 4, 0,   // cart        variable
      0, 5, 5, 3, 0,   // Items       property
    ];
    const ranges = decode(data, legend, doc);
    expect(ranges.map((range) => [text(range.from, range.to), range.kind])).toEqual([
      ["Shop", "type"],
      ["Cart", "type"],
      ["total", "function"],
      ["cart", "plain"],
      ["Cart", "type"],
      ["cart", "plain"],
      ["Items", "field"],
    ]);
  });

  it("skips token types without a color and tokens past the end", () => {
    const data = [0, 0, 6, 5, 0, 2, 0, 3, 6, 0, 5, 0, 3, 1, 0];
    expect(decode(data, legend, doc).map((range) => text(range.from, range.to))).toEqual(["let"]);
  });

  it("keeps a token inside its line", () => {
    const ranges = decode([0, 12, 40, 1, 0], legend, doc);
    expect(text(ranges[0].from, ranges[0].to)).toBe("Cart");
  });

  it("leaves out the dot of a member and joins a name the server split in two", () => {
    const source = Text.of(["[<Literal>]", "let Max = Math.PI"]);
    // prettier-ignore
    const data = [
      0, 2, 1, 7, 0,   // L           class
      0, 1, 6, 8, 0,   // iteral      method
      1, 4, 3, 4, 6,   // Max         variable, readonly + defaultLibrary
      0, 6, 4, 7, 0,   // Math        class
      0, 4, 3, 9, 0,   // .PI         member
    ];
    const ranges = decode(data, legend, source);
    expect(ranges.map((range) => [source.sliceString(range.from, range.to), range.kind])).toEqual([
      ["Literal", "type"],
      ["Max", "constant"],
      ["Math", "type"],
      ["PI", "field"],
    ]);
  });
});
