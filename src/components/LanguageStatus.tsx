import { createEffect, createSignal, For, onCleanup, onMount, Show } from "solid-js";
import { folderName } from "../state/favorites";
import { askToTrust, keepRestricted, openFile, type SetupStep, setupOutput, state, trustBuildCode } from "../state/review";
import { CheckIcon, CloseIcon, FileIcon } from "./icons";

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
  const off = () => phase() === "consent" || phase() === "restricted";
  // Nothing is shown while it gets going, unless something already went wrong.
  const notReady = () => phase() === "failed" || problems().length > 0;
  const shown = () => off() || notReady() || phase() === "ready";

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
            classList={{ warning: notReady() || off() }}
            title={`F# language features${state.lspStatus ? ` (${state.lspStatus})` : ""}: click for details`}
            onClick={() => setOpen(!open())}
          >
            <span class="language-mark">{notReady() || off() ? "!" : "✓"}</span>
            <span class="language-label">{off() ? "F# off" : notReady() ? "F# not ready" : "F# ready"}</span>
          </button>
          <Show when={open() && off()}>
            <div class="language-popup" role="dialog" aria-label="F# language features">
              <h3>F# language features are off</h3>
              <p>
                Getting them going runs the repository's own build code (restoring packages and .NET tools, building
                projects), and that waits until you trust it. Meanwhile the diff works as usual, with syntax colors but
                without hover, go to definition or error markers.
              </p>
              <button
                class="language-trust"
                onClick={() => {
                  setOpen(false);
                  askToTrust();
                }}
              >
                Turn on F# features…
              </button>
            </div>
          </Show>
          <Show when={open() && !off()}>
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

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

/**
 * Asks before the repository's build code runs for F# language features:
 * the first time in a repository, and again when the reviewed branch or pull
 * request changes build files (listed, a click opens one).
 */
export function TrustDialog() {
  const consent = () => state.languageSetup!.consent!;
  const files = () => consent().files;
  const changed = () => files() === null || files()!.length > 0;
  let first: HTMLButtonElement | undefined;
  onMount(() => first?.focus());
  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && keepRestricted()}
      onKeyDown={(event) => event.key === "Escape" && keepRestricted()}
    >
      <div class="modal complete-dialog trust-dialog" role="alertdialog" aria-label="Run the repository's build code">
        <h2>
          {changed() ? `${consent().branch} changes build files` : `Trust the build code of ${folderName(consent().repo)}?`}
        </h2>
        <p>
          For F# language features Arnold restores the projects' packages and .NET tools, builds the projects in other
          languages that F# code uses, and has FsAutoComplete load the projects. That runs code the repository chooses
          (MSBuild targets, .NET tools, Paket) on this computer, as you, with access to your files and credentials.
        </p>
        <Show
          when={changed()}
          fallback={<p>Trust the repository only if you trust everyone who can push to it.</p>}
        >
          <Show
            when={files()}
            fallback={<p class="complete-warning">Arnold could not read which files the branch changes, so it may change build files.</p>}
          >
            {(listed) => (
              <>
                <p>
                  Whoever wrote {consent().branch} decides what {listed().length === 1 ? "this file runs" : `these ${plural(listed().length, "file")} run`}.
                  Read {listed().length === 1 ? "it" : "them"} first:
                </p>
                <ul class="trust-files">
                  <For each={listed()}>
                    {(path) => (
                      <li>
                        <button
                          class="trust-file"
                          title={`Show ${path}`}
                          onClick={() => {
                            keepRestricted();
                            void openFile(path);
                          }}
                        >
                          <FileIcon />
                          {path}
                        </button>
                      </li>
                    )}
                  </For>
                </ul>
              </>
            )}
          </Show>
        </Show>
        <div class="committed-choices">
          <button ref={first} class="committed-choice" onClick={keepRestricted}>
            <CloseIcon />
            <span>
              Keep F# features off
              <small>Syntax colors only, without hover, go to definition or error markers. "F# off" in the status bar asks again.</small>
            </span>
          </button>
          <button class="committed-choice primary" onClick={trustBuildCode}>
            <CheckIcon />
            <span>
              {changed() ? "Run the build code" : "Trust and turn on F# features"}
              <small>
                {changed()
                  ? `${consent().firstTime ? `Trusts ${consent().repo} too. ` : ""}Remembered for this commit of ${consent().branch}; a new one that changes build files asks again.`
                  : `Remembered for ${consent().repo}. Branches and pull requests that change build files ask again.`}
              </small>
            </span>
          </button>
        </div>
      </div>
    </div>
  );
}
