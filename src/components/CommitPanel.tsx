import { createSignal, For, onMount, Show } from "solid-js";
import { isMac } from "../platform";
import { answerFormatCheck, answerProtectedBranch, canCommit, commit, message, setCommitMessage } from "../state/commitMode";
import { state } from "../state/review";
import { BranchIcon, CloseIcon, CommitIcon, FormatIcon } from "./icons";

const COMMIT_KEY = isMac ? "⌘Enter" : "Ctrl+Enter";

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

/** Commit mode, at the bottom of Staged: the commit message and the Commit button. */
export function CommitBox() {
  const staged = () => state.work?.staged.length ?? 0;
  const hint = () => {
    if (state.committing) return state.committing;
    if (staged() === 0) return "Stage files or lines to commit them.";
    if (!message().trim()) return `Write a message to commit ${plural(staged(), "staged file")}.`;
    return `${plural(staged(), "staged file")} on ${state.work?.branch ?? "detached HEAD"}`;
  };
  return (
    <form
      class="commit-box"
      onSubmit={(event) => {
        event.preventDefault();
        void commit();
      }}
    >
      <textarea
        class="commit-message"
        placeholder="Commit message"
        aria-label="Commit message"
        rows={3}
        value={message()}
        onInput={(event) => setCommitMessage(event.currentTarget.value)}
        onKeyDown={(event) => {
          if (event.key === "Enter" && (event.metaKey || event.ctrlKey)) {
            event.preventDefault();
            void commit();
          }
        }}
      />
      <div class="commit-bar">
        <span class="commit-hint" title={hint()}>
          {hint()}
        </span>
        <button type="submit" class="primary" disabled={!canCommit()} title={`Commit the staged changes (${COMMIT_KEY})`}>
          <CommitIcon />
          <span>Commit</span>
        </button>
      </div>
    </form>
  );
}

/** Most files listed in the Not formatted dialog. */
const LISTED_FILES = 12;

/**
 * Before a commit, when the repository uses Fantomas and the code base is
 * not formatted: format and commit, commit as it is, or stop.
 */
export function UnformattedDialog() {
  let first: HTMLButtonElement | undefined;
  onMount(() => first?.focus());
  const check = () => state.unformatted!;
  const files = () => check().unformatted;
  const cancel = () => answerFormatCheck("cancel");
  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && cancel()}
      onKeyDown={(event) => event.key === "Escape" && cancel()}
    >
      <div class="modal complete-dialog unformatted-dialog" role="dialog" aria-label="Not formatted">
        <h2>{check().failure ? "Formatting not checked" : "The code base is not formatted"}</h2>
        <Show
          when={!check().failure}
          fallback={
            <>
              <p>This repository uses Fantomas, but checking the formatting failed:</p>
              <pre class="unformatted-output">{check().failure}</pre>
            </>
          }
        >
          <Show when={files().length > 0}>
            <p>Fantomas would reformat {plural(files().length, "file")}:</p>
            <ul class="unformatted-files">
              <For each={files().slice(0, LISTED_FILES)}>{(path) => <li>{path}</li>}</For>
              <Show when={files().length > LISTED_FILES}>
                <li class="pr-subtitle">and {files().length - LISTED_FILES} more</li>
              </Show>
            </ul>
          </Show>
          <Show when={check().errors.length > 0}>
            <p class="complete-warning">Fantomas could not format {plural(check().errors.length, "file")}:</p>
            <pre class="unformatted-output">{check().errors.join("\n")}</pre>
          </Show>
        </Show>
        <div class="committed-choices">
          <Show when={files().length > 0}>
            <button ref={first} class="committed-choice primary" onClick={() => answerFormatCheck("format")}>
              <FormatIcon />
              <span>
                Run Fantomas and commit
                <small>Format {plural(files().length, "file")} and stage only their formatting, then commit</small>
              </span>
            </button>
          </Show>
          <button
            ref={(button) => files().length === 0 && (first = button)}
            class="committed-choice"
            onClick={() => answerFormatCheck("commit")}
          >
            <CommitIcon />
            <span>
              Commit without formatting
              <small>Commit the staged changes as they are</small>
            </span>
          </button>
          <button class="committed-choice" onClick={cancel}>
            <CloseIcon />
            <span>
              Cancel
              <small>Nothing is formatted or committed</small>
            </span>
          </button>
        </div>
      </div>
    </div>
  );
}

/**
 * Before a commit, when Azure DevOps has required policies on the branch, so
 * it takes changes only through pull requests: commit on a new branch
 * instead, or stop.
 */
export function ProtectedBranchDialog() {
  const asked = () => state.protectedBranch!;
  const [name, setName] = createSignal(asked().suggested);
  const busy = () => state.committing?.startsWith("Creating ") ?? false;
  const taken = () => state.repo?.branches.some((branch) => !branch.isRemote && branch.name === name().trim()) ?? false;
  const problem = () => (taken() ? `A branch named ${name().trim()} exists already.` : asked().error);
  const cancel = () => !busy() && answerProtectedBranch({ kind: "cancel" });
  let input: HTMLInputElement | undefined;
  onMount(() => input?.select());
  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && cancel()}
      onKeyDown={(event) => event.key === "Escape" && cancel()}
    >
      <form
        class="modal complete-dialog protected-dialog"
        role="dialog"
        aria-label="Branch takes pull requests only"
        onSubmit={(event) => {
          event.preventDefault();
          if (!busy() && name().trim() && !taken()) answerProtectedBranch({ kind: "branch", name: name().trim() });
        }}
      >
        <h2>{asked().branch} takes pull requests only</h2>
        <p>
          Azure DevOps has required branch policies on {asked().remote}/{asked().remoteBranch}, so pushing commits to it is
          refused:
        </p>
        <ul class="protected-policies">
          <For each={asked().policies}>{(policy) => <li>{policy}</li>}</For>
        </ul>
        <p>Commit on a new branch instead, and open a pull request from it. The staged and unstaged changes come along.</p>
        <label class="complete-message">
          <span>New branch</span>
          <input
            ref={input}
            type="text"
            value={name()}
            placeholder="Name of the branch to commit on"
            spellcheck={false}
            autocapitalize="off"
            onInput={(event) => setName(event.currentTarget.value)}
            required
          />
        </label>
        <Show when={problem()}>
          <p class="connect-problem">{problem()}</p>
        </Show>
        <div class="committed-choices">
          <button type="submit" class="committed-choice primary" disabled={busy() || !name().trim() || taken()}>
            <BranchIcon />
            <span>
              {busy() ? state.committing : "Create branch and commit"}
              <small>
                Check out {name().trim() || "the new branch"} from {asked().branch} and commit the staged changes there
              </small>
            </span>
          </button>
          <button type="button" class="committed-choice" disabled={busy()} onClick={cancel}>
            <CloseIcon />
            <span>
              Cancel
              <small>Nothing is committed</small>
            </span>
          </button>
        </div>
      </form>
    </div>
  );
}
