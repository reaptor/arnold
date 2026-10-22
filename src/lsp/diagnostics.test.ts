import { Text } from "@codemirror/state";
import { describe, expect, it } from "vitest";
import { toDiagnostic, unusedOpenPositions } from "./diagnostics";

const doc = Text.of(["module Orders", "", "  open System.IO", "let x = 1", "let y = (x + 1)"]);
const at = (line: number, column: number) => doc.line(line).from + column;
const range = { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } };

describe("toDiagnostic", () => {
  it("fades a whole unused open, keyword included", () => {
    const item = { range, severity: 4, code: "FSAC0001", message: "Unused open statement", tags: [1] };
    expect(toDiagnostic(item, at(3, 7), at(3, 16), doc)).toEqual({
      from: at(3, 2),
      to: at(3, 16),
      severity: "hint",
      message: "Unused open statement",
      markClass: "cm-unnecessary",
      // The server's own start, for asking it for fixes.
      lsp: { item, shift: 5 },
    });
  });

  it("marks removable parentheses so only the parentheses fade", () => {
    const item = { range, severity: 4, code: "FSAC0004", message: "Parentheses can be removed.", tags: [1] };
    expect(toDiagnostic(item, at(5, 8), at(5, 15), doc).markClass).toBe("cm-unnecessaryParens");
    // A range that does not start and end with parentheses fades as a whole.
    expect(toDiagnostic(item, at(5, 9), at(5, 14), doc).markClass).toBe("cm-unnecessary");
  });

  it("offers quick fixes only where the editor can be typed in", () => {
    const item = { range, severity: 4, code: "FSAC0001", message: "Unused open statement", tags: [1] };
    expect(toDiagnostic(item, at(3, 7), at(3, 16), doc).actions).toBeUndefined();
    expect(toDiagnostic(item, at(3, 7), at(3, 16), doc, true).actions?.map((action) => action.name)).toEqual(["Quick fixes"]);
  });

  it("keeps other diagnostics as they are", () => {
    const item = { range, severity: 1, code: 1, message: "Type mismatch" };
    expect(toDiagnostic(item, at(4, 8), at(4, 9), doc)).toEqual({
      from: at(4, 8),
      to: at(4, 9),
      severity: "error",
      message: "Type mismatch",
      lsp: { item, shift: 0 },
    });
  });

  it("treats a missing severity as an error", () => {
    expect(toDiagnostic({ range, message: "?" }, 0, 1, doc).severity).toBe("error");
  });
});

describe("unusedOpenPositions", () => {
  it("finds where the unused opens start", () => {
    const unused = { range, severity: 4, code: "FSAC0001", message: "Unused open statement", tags: [1] };
    const other = { range, severity: 1, code: 1, message: "Type mismatch" };
    const diagnostics = [toDiagnostic(other, at(4, 8), at(4, 9), doc), toDiagnostic(unused, at(3, 7), at(3, 16), doc)];
    expect(unusedOpenPositions(diagnostics)).toEqual([at(3, 2)]);
  });
});
