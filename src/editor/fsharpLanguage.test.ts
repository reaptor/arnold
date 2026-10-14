import { StringStream } from "@codemirror/language";
import { describe, expect, it } from "vitest";
import { fSharpParser } from "./fsharpLanguage";

/** The words of `source` with the style the parser gives them (null styles left out). */
function styles(source: string): string[] {
  const state = fSharpParser.startState!(4);
  const out: string[] = [];
  for (const line of source.split("\n")) {
    const stream = new StringStream(line, 4, 4);
    while (!stream.eol()) {
      const style = fSharpParser.token(stream, state);
      const text = stream.current();
      if (style && style !== "number" && style !== "string" && /\w/.test(text)) out.push(`${text}:${style}`);
      stream.start = stream.pos;
    }
  }
  return out;
}

describe("F# highlighting without a server", () => {
  it("colors function bindings and their later uses as functions, values as values", () => {
    expect(
      styles(`let vatRate = rate
let total (order: Order) : decimal =
    order |> netTotal |> total
let mutable sum = 0
let add = fun x -> x`),
    ).toEqual([
      "let:keyword",
      "vatRate:variable",
      "rate:variable",
      "let:keyword",
      "total:functionDefinition",
      "order:variable",
      "Order:type",
      "decimal:type",
      "order:variable",
      "netTotal:variable",
      "total:functionName",
      "let:keyword",
      "mutable:keyword",
      "sum:variable",
      "let:keyword",
      "add:functionDefinition",
      "fun:keyword",
      "x:variable",
      "x:variable",
    ]);
  });

  it("colors members like Rider: methods as functions, properties as fields", () => {
    expect(
      styles(`member this.Total(x) = this.Lines.Length
member _.Name = name
static member Create () = s.ToUpper()`),
    ).toEqual([
      "member:keyword",
      "this:variable",
      "Total:functionDefinition",
      "x:variable",
      "this:variable",
      "Lines:fieldName",
      "Length:fieldName",
      "member:keyword",
      "_:variable",
      "Name:fieldName",
      "name:variable",
      "static:keyword",
      "member:keyword",
      "Create:functionDefinition",
      "s:variable",
      "ToUpper:functionName",
    ]);
  });

  it("colors types, record fields, union cases and module functions", () => {
    expect(
      styles(`type Order = { Id: int; Lines: OrderLine list }
type Shape =
    | Circle of radius: float
items |> List.map Pricing.withVat |> System.Math.Round`),
    ).toEqual([
      "type:keyword",
      "Order:type",
      "Id:fieldName",
      "int:type",
      "Lines:fieldName",
      "OrderLine:type",
      "list:type",
      "type:keyword",
      "Shape:type",
      "Circle:type",
      "of:keyword",
      "radius:variable",
      "float:type",
      "items:variable",
      "List:type",
      "map:functionName",
      "Pricing:type",
      "withVat:functionName",
      "System:type",
      "Math:type",
      "Round:fieldName",
    ]);
  });

  it("keeps true/false as keywords and core functions as functions", () => {
    expect(styles(`if ok = true then printfn "%d" (decimal 1)`)).toEqual([
      "if:keyword",
      "ok:variable",
      "true:keyword",
      "then:keyword",
      "printfn:functionName",
      "decimal:functionName",
    ]);
  });
});
