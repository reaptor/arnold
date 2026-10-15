import { Text } from "@codemirror/state";
import { describe, expect, it } from "vitest";
import { toDiagnostic } from "./diagnostics";

const doc = Text.of(["module Orders", "", "  open System.IO", "let x = 1"]);
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
    });
  });

  it("keeps other diagnostics as they are", () => {
    const item = { range, severity: 1, code: 1, message: "Type mismatch" };
    expect(toDiagnostic(item, at(4, 8), at(4, 9), doc)).toEqual({
      from: at(4, 8),
      to: at(4, 9),
      severity: "error",
      message: "Type mismatch",
    });
  });

  it("treats a missing severity as an error", () => {
    expect(toDiagnostic({ range, message: "?" }, 0, 1, doc).severity).toBe("error");
  });
});
