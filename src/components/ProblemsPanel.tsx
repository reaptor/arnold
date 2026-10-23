// The Problems panel under the diff, like Rider's and Visual Studio's error
// list: what FsAutoComplete and the last build reported, file by file; a
// click goes there. Its header builds the solution (Ctrl+Shift+B).

import { createSignal, For, Show } from "solid-js";
import { isMac } from "../platform";
import { build, canBuild, cancelBuild, startBuild } from "../state/build";
import { countProblems, mergeProblems, type Problem, problemFiles, type ProblemSeverity } from "../state/problems";
import { goToLine, state } from "../state/review";
import { BuildIcon, CloseIcon, ErrorIcon, InfoIcon, StopIcon, WarningIcon } from "./icons";

const [problemsOpen, setProblemsOpen] = createSignal(localStorage.getItem("arnold.problemsOpen") === "true");

export { problemsOpen };

/** Opens or closes the panel: the status bar's counts, Ctrl+Shift+M (Cmd+Shift+M on macOS) as in VS Code. */
export function toggleProblems(open = !problemsOpen()) {
  setProblemsOpen(open);
  localStorage.setItem("arnold.problemsOpen", String(open));
}

export const PROBLEMS_SHORTCUT = isMac ? "⌘⇧M" : "Ctrl+Shift+M";

/** Builds the solution: Ctrl+Shift+B as in Visual Studio and VS Code, and Cmd+Shift+B on macOS. */
export const BUILD_SHORTCUT = isMac ? "⌘⇧B" : "Ctrl+Shift+B";

const SEVERITY_ICON = { error: ErrorIcon, warning: WarningIcon, info: InfoIcon } as const;

const FILTERS: { severity: ProblemSeverity; one: string; many: string }[] = [
  { severity: "error", one: "error", many: "errors" },
  { severity: "warning", one: "warning", many: "warnings" },
  { severity: "info", one: "message", many: "messages" },
];

/** The severities shown, as Visual Studio's Errors, Warnings and Messages toggles; remembered. */
const [shown, setShown] = createSignal<ReadonlySet<ProblemSeverity>>(
  new Set((localStorage.getItem("arnold.problemsShown") ?? "error,warning,info").split(",") as ProblemSeverity[]),
);

function toggleShown(severity: ProblemSeverity) {
  const next = new Set(shown());
  if (next.has(severity)) next.delete(severity);
  else next.add(severity);
  setShown(next);
  localStorage.setItem("arnold.problemsShown", [...next].join(","));
}

/** FsAutoComplete's problems and the last build's, by file. */
const allProblems = () => mergeProblems(state.diagnostics, build.problems);

const counts = () => countProblems(allProblems());

const fileName = (path: string) => path.slice(path.lastIndexOf("/") + 1);
const folderOf = (path: string) => path.slice(0, Math.max(path.lastIndexOf("/"), 0));

/** The solution or project being built, as the status bar and the panel name it. */
const buildTarget = () => (build.target ? build.target.slice(build.target.lastIndexOf("/") + 1) : "the solution");

/** How the last build went, for the panel's header. */
function buildOutcome() {
  const time = build.seconds === null ? "" : ` · ${build.seconds} s`;
  if (build.outcome === "succeeded") return `Build succeeded${time}`;
  if (build.outcome === "failed") return `Build failed${time}`;
  if (build.outcome === "cancelled") return "Build cancelled";
  return "";
}

/** The error and warning counts in the status bar, a click opens or closes the panel; and the build under way. */
export function ProblemsStatus() {
  return (
    <Show when={state.languageSetup}>
      <Show when={build.running}>
        <span class="build-status" role="status">
          <span class="spinner" aria-hidden="true" />
          <span class="build-label">Building {buildTarget()}…</span>
          <button class="icon-button" onClick={cancelBuild} title="Cancel build">
            <StopIcon />
          </button>
        </span>
      </Show>
      <button
        class="problems-button"
        classList={{ open: problemsOpen() }}
        title={`Problems reported by FsAutoComplete and the last build (${PROBLEMS_SHORTCUT})`}
        onClick={() => toggleProblems()}
      >
        <ErrorIcon class="problem-error" />
        <span>{counts().error}</span>
        <WarningIcon class="problem-warning" />
        <span>{counts().warning}</span>
      </button>
    </Show>
  );
}

export function ProblemsPanel() {
  const files = () => problemFiles(allProblems(), shown());
  const ready = () => state.languageSetup?.phase === "ready";
  const empty = () => {
    if (!state.languageSetup) return "No F# projects here: there is no language server to report problems.";
    if (build.running) return `Building ${buildTarget()}…`;
    if (build.error) return build.error;
    if (build.outcome === "failed") return `The build failed without reporting errors: ${build.tail.at(-1) ?? "dotnet build printed nothing"}`;
    if (build.outcome === "succeeded") return "The build succeeded without problems.";
    if (!ready() && counts().error + counts().warning + counts().info === 0) return "Problems show once FsAutoComplete has loaded the projects.";
    return "No problems in the files FsAutoComplete has checked.";
  };
  const go = (path: string, problem: Problem) => void goToLine(path, Math.max(problem.line, 0) + 1, null, problem.character);

  return (
    <div class="problems-panel">
      <header class="problems-header">
        <span class="problems-title">Problems</span>
        <For each={FILTERS}>
          {(filter) => {
            const count = () => counts()[filter.severity];
            const Icon = SEVERITY_ICON[filter.severity];
            return (
              <button
                class="problems-filter"
                classList={{ active: shown().has(filter.severity) }}
                aria-pressed={shown().has(filter.severity)}
                title={`${shown().has(filter.severity) ? "Hide" : "Show"} ${filter.many}`}
                onClick={() => toggleShown(filter.severity)}
              >
                <Icon class={`problem-${filter.severity}`} />
                {count()} {count() === 1 ? filter.one : filter.many}
              </button>
            );
          }}
        </For>
        <span class="toolbar-space" />
        <Show when={build.running}>
          <span class="problems-scope">Building {buildTarget()}…</span>
        </Show>
        <Show when={!build.running && !build.outcome}>
          <span class="problems-scope" title="FsAutoComplete checks the files it has open: the files shown and the changed F# files">
            Shown and changed files
          </span>
        </Show>
        <Show when={build.outcome}>
          <span
            class="problems-scope"
            classList={{ "build-failed": build.outcome === "failed" }}
            title="The last build's problems, and those FsAutoComplete reported on the files it checked since"
          >
            {buildOutcome()}
          </span>
        </Show>
        <Show
          when={build.running}
          fallback={
            <button class="problems-build" disabled={!canBuild()} onClick={() => void startBuild()} title={`Build ${buildTarget()} (${BUILD_SHORTCUT})`}>
              <BuildIcon />
              Build
            </button>
          }
        >
          <button class="problems-build" onClick={cancelBuild} title="Stop the build">
            <span class="spinner" aria-hidden="true" />
            Cancel build
          </button>
        </Show>
        <button class="icon-button" onClick={() => toggleProblems(false)} title={`Close (${PROBLEMS_SHORTCUT})`}>
          <CloseIcon />
        </button>
      </header>
      <div class="problems-list">
        <Show when={files().length > 0} fallback={<p class="problems-empty">{empty()}</p>}>
          <For each={files()}>
            {(file) => (
              <>
                <div class="problems-file" title={file.path}>
                  <span class="problems-file-name">{fileName(file.path)}</span>
                  <span class="problems-file-folder">{folderOf(file.path)}</span>
                  <span class="problems-file-count">{file.problems.length}</span>
                </div>
                <For each={file.problems}>
                  {(problem) => {
                    const Icon = SEVERITY_ICON[problem.severity];
                    return (
                      <button class="problem" onClick={() => go(file.path, problem)} title={problem.message}>
                        <Icon class={`problem-${problem.severity}`} />
                        <span class="problem-message">{problem.message}</span>
                        <Show when={problem.code}>
                          <span class="problem-code">{problem.code}</span>
                        </Show>
                        <Show when={problem.line >= 0}>
                          <span class="problem-place">
                            ({problem.line + 1}, {problem.character + 1})
                          </span>
                        </Show>
                      </button>
                    );
                  }}
                </For>
              </>
            )}
          </For>
        </Show>
      </div>
    </div>
  );
}
