import { describe, expect, it } from "vitest";
import { countProblems, problemFiles, toProblems } from "./problems";

const at = (line: number, character: number) => ({ start: { line, character }, end: { line, character: character + 1 } });

describe("toProblems", () => {
  it("keeps errors, warnings and messages in the order of the file, but not hints", () => {
    const problems = toProblems([
      { range: at(4, 2), severity: 2, code: 1182, message: "unused" },
      { range: at(1, 0), message: "no severity given" },
      { range: at(1, 0), severity: 4, code: "FSAC0001", message: "Unused open statement" },
      { range: at(3, 8), severity: 3, message: "info" },
    ]);
    expect(problems).toEqual([
      { severity: "error", message: "no severity given", code: null, line: 1, character: 0 },
      { severity: "info", message: "info", code: null, line: 3, character: 8 },
      { severity: "warning", message: "unused", code: "1182", line: 4, character: 2 },
    ]);
  });
});

describe("problemFiles", () => {
  const byFile = {
    "src/Orders.fs": toProblems([{ range: at(0, 0), severity: 2, message: "w" }]),
    "src/Api/Billing.fs": toProblems([
      { range: at(2, 0), severity: 1, message: "e" },
      { range: at(3, 0), severity: 2, message: "w" },
    ]),
  };

  it("counts every severity", () => {
    expect(countProblems(byFile)).toEqual({ error: 1, warning: 2, info: 0 });
  });

  it("lists the files by path with the problems shown, leaving out files without any", () => {
    expect(problemFiles(byFile, new Set(["error"]))).toEqual([{ path: "src/Api/Billing.fs", problems: [byFile["src/Api/Billing.fs"][0]] }]);
    expect(problemFiles(byFile, new Set(["error", "warning"])).map((file) => file.path)).toEqual(["src/Api/Billing.fs", "src/Orders.fs"]);
  });
});
