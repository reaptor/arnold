// The errors, warnings and messages in what `dotnet build` prints, for the
// Problems panel (build.ts).

import { relativeTo } from "../lsp/uri";
import type { Problem, ProblemSeverity } from "./problems";

/**
 * MSBuild's canonical message format:
 * `origin(line,column): [subcategory] error|warning|info [code]: text [project]`,
 * where the location may be missing, a line alone or a range.
 */
const MESSAGE =
  /^\s*(.+?)(?:\((\d+)(?:,(\d+))?(?:,\d+,\d+)?\))?\s*:\s*(?:[^:]*?\s)?(error|warning|info)(?:\s+([A-Za-z]+\d+))?\s*:\s*(.*?)(?:\s+\[([^\]]+?\.(?:\w+proj|slnx?))(?:::[^\]]*)?\])?\s*$/;

const isAbsolute = (path: string) => path.startsWith("/") || /^[A-Za-z]:[\\/]/.test(path);

/** A path as the Problems panel lists it: relative to `root` when inside it, with forward slashes. */
function shownPath(path: string, root: string): string {
  return (isAbsolute(path) && relativeTo(root, path)) || path.replace(/\\/g, "/");
}

/**
 * The problem a line of `dotnet build` (run in `root`) reports, with the file
 * it is in; null for any other line. A problem without a place in a file
 * (`line` -1) is listed under the project that reported it.
 */
export function parseBuildLine(line: string, root: string): { path: string; problem: Problem } | null {
  const match = MESSAGE.exec(line);
  if (!match) return null;
  const [, origin, lineText, columnText, severity, code, message, project] = match;
  // A tool (CSC, MSBUILD) rather than a file reported it.
  const tool = !lineText && !/[\\/.]/.test(origin);
  const file = tool && project ? project : origin.trim();
  return {
    path: shownPath(file, root),
    problem: {
      severity: severity as ProblemSeverity,
      message,
      code: code ?? null,
      line: lineText ? Number(lineText) - 1 : -1,
      character: columnText ? Number(columnText) - 1 : 0,
    },
  };
}
