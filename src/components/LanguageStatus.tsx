import { createEffect, createSignal, For, onCleanup, Show } from "solid-js";
import { type SetupStep, setupOutput, state } from "../state/review";

const STEP_MARK: Record<SetupStep["state"], string> = {
  running: "",
  done: "✓",
  failed: "!",
  skipped: "–",
};

/**
 * Status bar item for F# language features: a static "F# ready" or "F# not
 * ready" once there is something to say, and a popup with the steps, problems
 * and output. Getting them going runs quietly in the background.
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
  const problems = () => setup()?.problems ?? [];
  // Nothing is shown while it gets going, unless something already went wrong.
  const notReady = () => phase() === "failed" || problems().length > 0;
  const shown = () => notReady() || phase() === "ready";

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
            ? "The reviewed code is checked out (in the opened folder or a worktree), so FsAutoComplete runs there and sees your edits. Arnold"
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
    <Show when={shown() && setup()}>
      {(current) => (
        <div class="language-status" ref={root}>
          <button
            class="language-button"
            classList={{ warning: notReady() }}
            title={`F# language features${state.lspStatus ? ` (${state.lspStatus})` : ""}: click for details`}
            onClick={() => setOpen(!open())}
          >
            <span class="language-mark">{notReady() ? "!" : "✓"}</span>
            <span class="language-label">{notReady() ? "F# not ready" : "F# ready"}</span>
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
                            <summary>{problem.detailsTitle ?? "Build log"}</summary>
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
