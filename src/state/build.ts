// Building the solution on request (Ctrl+Shift+B), as Rider's and Visual
// Studio's Build does: `dotnet build` in the folder the language server runs
// in. Its errors, warnings and messages are listed in the Problems panel with
// FsAutoComplete's; those FsAutoComplete reported before the build are
// dropped as it starts, as they may be stale (the server reports again on
// the files it checks).

import { createStore, reconcile } from "solid-js/store";
import * as ipc from "../ipc/commands";
import { parseBuildLine } from "./buildOutput";
import type { Problem } from "./problems";
import { setState, state } from "./review";

export interface BuildState {
  running: boolean;
  /** The solution or project being built, once dotnet has started on it. */
  target: string | null;
  /** How the last build ended; null while one runs or before the first. */
  outcome: "succeeded" | "failed" | "cancelled" | null;
  /** How long the last build took, in seconds. */
  seconds: number | null;
  /** Its problems by file path: relative to the folder built when inside it. */
  problems: Record<string, Problem[]>;
  /** The last lines it printed, for a build that fails without saying why. */
  tail: string[];
  /** Why it could not run at all (no .NET SDK, nothing to build). */
  error: string | null;
}

/** A fresh state with no build: the store changes the objects it is given in place. */
const idle = (): BuildState => ({ running: false, target: null, outcome: null, seconds: null, problems: {}, tail: [], error: null });

const [build, setBuild] = createStore<BuildState>(idle());
export { build };

const TAIL_LINES = 5;

/** The folder a build runs in: where the language server runs, once it is prepared. */
function buildRoot(): string | null {
  const setup = state.languageSetup;
  return setup && setup.phase !== "preparing" ? setup.path : null;
}

/** Whether a build can be started now. */
export const canBuild = () => !build.running && buildRoot() !== null;

let run = 0;

/** Builds the solution, unless one is being built or there is nothing to build yet. */
export async function startBuild() {
  const root = buildRoot();
  if (!root || build.running) return;
  const id = ++run;
  const started = Date.now();
  setState("diagnostics", reconcile({}));
  setState("problems", reconcile({}));
  setBuild(reconcile({ ...idle(), running: true }));
  const seen = new Set<string>();
  const onEvent = (event: ipc.BuildEvent) => {
    if (id !== run) return;
    if (event.kind === "started") {
      setBuild("target", event.target);
      return;
    }
    setBuild("tail", (tail) => [...tail.slice(-(TAIL_LINES - 1)), event.line]);
    const found = parseBuildLine(event.line, root);
    if (!found) return;
    // Projects built for several frameworks, and projects built by more than one target, repeat theirs.
    const key = JSON.stringify([found.path, found.problem]);
    if (seen.has(key)) return;
    seen.add(key);
    setBuild("problems", found.path, (problems = []) => sortProblems([...problems, found.problem]));
  };
  const finish = (outcome: NonNullable<BuildState["outcome"]>, error: string | null = null) =>
    setBuild({ running: false, outcome, error, seconds: Math.round((Date.now() - started) / 1000) });
  try {
    const { succeeded } = await ipc.buildSolution(root, onEvent);
    if (id === run) finish(succeeded ? "succeeded" : "failed");
  } catch (error) {
    if (id !== run) return;
    if (String(error) === "cancelled") finish("cancelled");
    else finish("failed", String(error));
  }
}

/** Stops the build under way; what it reported so far stays listed. */
export function cancelBuild() {
  if (build.running) void ipc.cancelBuild();
}

/** Forgets the last build (another checkout is being prepared), stopping one under way. */
export function forgetBuild() {
  run++;
  if (build.running) void ipc.cancelBuild();
  setBuild(reconcile(idle()));
}

const sortProblems = (problems: Problem[]) =>
  problems.sort((a, b) => a.line - b.line || a.character - b.character);
