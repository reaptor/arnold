// The Problems panel, like Rider's and Visual Studio's error list: every
// error, warning and message FsAutoComplete reported, file by file. The
// server checks the files it has open, which are the files shown and the
// changed F# files Arnold hands it in the background (review.ts).

import type { ServerDiagnostic } from "../lsp/codeActions";

export type ProblemSeverity = "error" | "warning" | "info";

export interface Problem {
  severity: ProblemSeverity;
  message: string;
  code: string | null;
  /** Where it starts: 0-based line and UTF-16 column, as the server counts; line -1 for a whole file (a build's project problems). */
  line: number;
  character: number;
}

/** LSP severities 1–3; hints (4) are left out, as faded code and the like are not problems. */
const SEVERITY: Record<number, ProblemSeverity> = { 1: "error", 2: "warning", 3: "info" };

/** The problems among a file's server diagnostics, in the order of the file. */
export function toProblems(diagnostics: readonly ServerDiagnostic[]): Problem[] {
  const problems: Problem[] = [];
  for (const diagnostic of diagnostics) {
    // A missing severity is treated as an error, as the spec suggests.
    const severity = SEVERITY[diagnostic.severity ?? 1];
    if (!severity) continue;
    problems.push({
      severity,
      message: diagnostic.message,
      code: diagnostic.code == null ? null : String(diagnostic.code),
      line: diagnostic.range.start.line,
      character: diagnostic.range.start.character,
    });
  }
  return problems.sort((a, b) => a.line - b.line || a.character - b.character);
}

/**
 * FsAutoComplete's problems (`server`) and the last build's (`built`) in one
 * list per file, the build's first; a problem both report is listed once.
 */
export function mergeProblems(
  server: Record<string, readonly Problem[]>,
  built: Record<string, readonly Problem[]>,
): Record<string, Problem[]> {
  const merged: Record<string, Problem[]> = {};
  for (const path of new Set([...Object.keys(built), ...Object.keys(server)])) {
    const fromBuild = built[path] ?? [];
    const fromServer = (server[path] ?? []).filter((problem) => !fromBuild.some((other) => same(problem, other)));
    merged[path] = [...fromBuild, ...fromServer].sort((a, b) => a.line - b.line || a.character - b.character);
  }
  return merged;
}

/** F# codes come as FS0039 from a build and may come as 39 from the server; messages on one line from a build. */
const sameCode = (a: string | null, b: string | null) => a?.replace(/^FS0*/i, "") === b?.replace(/^FS0*/i, "");
const oneLine = (message: string) => message.replace(/\s+/g, " ").trim();
const same = (a: Problem, b: Problem) =>
  a.severity === b.severity && a.line === b.line && a.character === b.character && sameCode(a.code, b.code) && oneLine(a.message) === oneLine(b.message);

export interface ProblemCounts {
  error: number;
  warning: number;
  info: number;
}

export function countProblems(byFile: Record<string, readonly Problem[]>): ProblemCounts {
  const counts: ProblemCounts = { error: 0, warning: 0, info: 0 };
  for (const problems of Object.values(byFile)) for (const problem of problems) counts[problem.severity]++;
  return counts;
}

/** The files with problems of the severities `shown`, by path, with those problems. */
export function problemFiles(
  byFile: Record<string, readonly Problem[]>,
  shown: ReadonlySet<ProblemSeverity>,
): { path: string; problems: Problem[] }[] {
  return Object.keys(byFile)
    .sort((a, b) => a.localeCompare(b))
    .map((path) => ({ path, problems: byFile[path].filter((problem) => shown.has(problem.severity)) }))
    .filter((file) => file.problems.length > 0);
}
