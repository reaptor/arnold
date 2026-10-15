import { createEffect, createSignal, For, onCleanup, Show } from "solid-js";
import { projectCounts, projectName, type SetupStep, setupOutput, state } from "../state/review";

const STEP_MARK: Record<SetupStep["state"], string> = {
  running: "",
  done: "✓",
  failed: "!",
  skipped: "–",
};

/**
 * Status bar item for F# language features: what is being done to get them
 * going, and a popup with the steps, problems and output.
 */
export function LanguageStatus() {
  const [open, setOpen] = createSignal(false);
  let root!: HTMLDivElement;
  let output: HTMLPreElement | undefined;

  const closeOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) setOpen(false);
  };
  const closeOnEscape = (event: KeyboardEvent) => {
    if (event.key === "Escape") setOpen(false);
  };
  window.addEventListener("pointerdown", closeOutside);
  window.addEventListener("keydown", closeOnEscape);
  onCleanup(() => {
    window.removeEventListener("pointerdown", closeOutside);
    window.removeEventListener("keydown", closeOnEscape);
  });

  const setup = () => state.languageSetup;
  const phase = () => setup()?.phase;
  const working = () => phase() === "preparing" || phase() === "starting" || phase() === "loading";
  const busy = () => working() || (phase() === "ready" && state.lspStatus !== null);
  const problems = () => setup()?.problems ?? [];
  const runningStep = () => setup()?.steps.find((step) => step.state === "running")?.title;

  const label = () => {
    const detail = state.lspStatus ? `: ${state.lspStatus}` : "…";
    switch (phase()) {
      case "preparing":
        return `Preparing F# features: ${runningStep() ?? "…"}`;
      case "starting":
        return `Starting the F# language server${detail}`;
      case "loading": {
        // FsAutoComplete's own progress only says "Loading N Projects".
        const { total, finished, failed } = projectCounts(setup()!);
        if (total === 0) return `Loading F# projects${detail}`;
        const current = setup()!.currentProject;
        return (
          `Loading F# projects: ${finished} of ${total}` +
          (failed ? ` (${failed} failed)` : "") +
          (current ? ` · ${projectName(current)}` : "")
        );
      }
      case "ready":
        if (state.lspStatus) return `F#: ${state.lspStatus}`;
        return problems().length ? `F# ready, ${problems().length} ${problems().length === 1 ? "problem" : "problems"}` : "F# ready";
      default:
        return "F# features off";
    }
  };

  const explanation = () => {
    switch (phase()) {
      case "ready":
        return problems().length
          ? "Hover, go to definition (F12) and error markers work in F# files, but something failed below, so parts of the code may show false errors."
          : "Hover, go to definition (F12) and error markers work in F# files.";
      case "failed":
        return "Keep reviewing: the diff works as usual, but hover, go to definition and error markers are off for this commit.";
      default:
        return `${
          state.languageSetup?.inPlace
            ? "The reviewed branch is checked out in the opened folder, so FsAutoComplete runs there. Arnold"
            : "Arnold copies the reviewed commit to .arnold/worktrees in the repository folder (git does not see it) and"
        } does only what FsAutoComplete needs there: restoring packages, and building projects in other languages that F# code uses. Keep reviewing meanwhile; hover, go to definition and error markers appear once this is done.`;
    }
  };

  // Follow the output as it grows.
  createEffect(() => {
    setupOutput();
    if (output) output.scrollTop = output.scrollHeight;
  });

  return (
    <Show when={setup()}>
      {(current) => (
        <div class="language-status" ref={root}>
          <button
            class="language-button"
            classList={{
              working: working(),
              warning: phase() === "failed" || (phase() === "ready" && problems().length > 0),
            }}
            title="F# language features: click for details"
            onClick={() => setOpen(!open())}
          >
            <Show when={busy()} fallback={<span class="language-mark">{phase() === "ready" ? "✓" : "!"}</span>}>
              <span class="spinner" aria-hidden="true" />
            </Show>
            <span class="language-label">{label()}</span>
          </button>
          <Show when={open()}>
            <div class="language-popup" role="dialog" aria-label="F# language features">
              <h3>F# language features</h3>
              <p>{explanation()}</p>
              <ol class="setup-steps">
                <For each={current().steps}>
                  {(step) => (
                    <li class={`step-${step.state}`}>
                      <Show when={step.state === "running"} fallback={<span class="step-mark">{STEP_MARK[step.state]}</span>}>
                        <span class="spinner" aria-hidden="true" />
                      </Show>
                      <span>
                        {step.title}
                        <Show when={step.note}>
                          <span class="step-note"> ({step.note})</span>
                        </Show>
                      </span>
                    </li>
                  )}
                </For>
              </ol>
              <Show when={problems().length > 0}>
                <ul class="setup-problems">
                  <For each={problems()}>
                    {(problem) => (
                      <li>
                        {problem.text}
                        <Show when={problem.details}>
                          <details>
                            <summary>Build log</summary>
                            <pre class="setup-output">{problem.details}</pre>
                          </details>
                        </Show>
                      </li>
                    )}
                  </For>
                </ul>
              </Show>
              <Show when={current().path}>
                <p class="setup-path">
                  Folder: <code>{current().path}</code>
                </p>
              </Show>
              <Show when={setupOutput().length > 0}>
                <details open={problems().length > 0}>
                  <summary>Output of dotnet</summary>
                  <pre class="setup-output" ref={output}>
                    {setupOutput().join("\n")}
                  </pre>
                </details>
              </Show>
            </div>
          </Show>
        </div>
      )}
    </Show>
  );
}
