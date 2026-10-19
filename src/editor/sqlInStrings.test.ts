import { ensureSyntaxTree } from "@codemirror/language";
import { EditorState } from "@codemirror/state";
import { csharp } from "@replit/codemirror-lang-csharp";
import { describe, expect, it } from "vitest";
import { fSharpLanguage } from "./fsharpLanguage";
import { holesOf, looksLikeSql, stringLiterals } from "./sqlInStrings";

/** The text between the quotes of each string literal in `source`. */
function contents(source: string, language = fSharpLanguage.extension): string[] {
  const state = EditorState.create({ doc: source, extensions: language });
  ensureSyntaxTree(state, state.doc.length, 5000);
  return stringLiterals(state, 0, state.doc.length).map((literal) => state.sliceDoc(literal.contentFrom, literal.contentTo));
}

describe("SQL in strings", () => {
  it("takes strings that start with an SQL statement", () => {
    expect(looksLikeSql("SELECT Id, Total FROM dbo.Orders WHERE Id = @id")).toBe(true);
    expect(looksLikeSql("\n    select o.Id\n    from Orders o")).toBe(true);
    expect(looksLikeSql("INSERT INTO Orders (Id) VALUES (@id)")).toBe(true);
    expect(looksLikeSql("UPDATE [dbo].[Orders] SET Status = 'Open'")).toBe(true);
    expect(looksLikeSql("DELETE FROM Orders WHERE Id = @id")).toBe(true);
    expect(looksLikeSql("WITH recent AS (SELECT 1) SELECT * FROM recent")).toBe(true);
    expect(looksLikeSql("-- open orders\nSELECT * FROM Orders")).toBe(true);
    expect(looksLikeSql("EXEC dbo.OpenOrders @CustomerId = 1")).toBe(true);
    expect(looksLikeSql("CREATE TABLE Orders (Id INT)")).toBe(true);
  });

  it("leaves sentences and other text alone", () => {
    expect(looksLikeSql("Select an item from the list")).toBe(false);
    expect(looksLikeSql("Delete from cart?")).toBe(false);
    expect(looksLikeSql("select the file")).toBe(false);
    expect(looksLikeSql("Order {0} updated")).toBe(false);
    expect(looksLikeSql("update")).toBe(false);
  });

  it("finds interpolation holes, not escaped braces", () => {
    const text = "SELECT * FROM T WHERE Id = {id} AND Json = '{{}}' AND X = {f {a}}";
    expect(holesOf(text, "$", '"').map(([from, to]) => text.slice(from, to))).toEqual(["{id}", "{f {a}}"]);
    expect(holesOf(text, "", '"')).toEqual([]);
    const template = "SELECT * FROM T WHERE Id = ${id}";
    expect(holesOf(template, "", "`").map(([from, to]) => template.slice(from, to))).toEqual(["${id}"]);
  });

  it("reads F# strings, a triple-quoted one over several lines as one", () => {
    const source = `let one = "SELECT 1"
let many = """
    SELECT Id
    FROM Orders
    """
let verbatim = @"SELECT 2"
let interpolated = $"SELECT * FROM T WHERE Id = {id}"
let list = [ "a"; "b" ]`;
    expect(contents(source)).toEqual([
      "SELECT 1",
      "\n    SELECT Id\n    FROM Orders\n    ",
      "SELECT 2",
      "SELECT * FROM T WHERE Id = {id}",
      "a",
      "b",
    ]);
  });

  it("keeps the `$` of an F# interpolated string, for its holes", () => {
    const state = EditorState.create({ doc: 'let q = $"SELECT * FROM T WHERE Id = {id}"', extensions: fSharpLanguage.extension });
    ensureSyntaxTree(state, state.doc.length, 5000);
    const [literal] = stringLiterals(state, 0, state.doc.length);
    expect(state.sliceDoc(literal.from, literal.contentFrom)).toBe('$"');
  });

  it("reads C# strings, interpolated ones whole", () => {
    const source = `class C { void M() {
  var a = "SELECT 1";
  var b = $@"SELECT *
    FROM T WHERE Id = {id}";
} }`;
    expect(contents(source, csharp())).toEqual(["SELECT 1", "SELECT *\n    FROM T WHERE Id = {id}"]);
  });
});
