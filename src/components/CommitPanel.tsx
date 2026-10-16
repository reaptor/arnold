import { onMount, Show } from "solid-js";
import { isMac } from "../platform";
import {
  canCommit,
  commit,
  createPullRequestFromCommitted,
  message,
  reviewBase,
  reviewCommitted,
  setCommitMessage,
} from "../state/commitMode";
import { currentRemote } from "../state/pullRequests";
import { setState, state } from "../state/review";
import { BranchIcon, CheckIcon, CommitIcon, PullRequestIcon } from "./icons";

const COMMIT_KEY = isMac ? "⌘Enter" : "Ctrl+Enter";

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

/** Commit mode, under the diff: the commit message and the Commit button. */
export function CommitBox() {
  const staged = () => state.work?.staged.length ?? 0;
  const hint = () => {
    if (state.committing) return "Committing…";
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
        <span class="commit-hint">{hint()}</span>
        <button type="submit" class="primary" disabled={!canCommit()} title={`Commit the staged changes (${COMMIT_KEY})`}>
          <CommitIcon />
          <span>{state.committing ? "Committing…" : "Commit"}</span>
        </button>
      </div>
    </form>
  );
}

/**
 * After a commit: on to reviewing the branch against the base branch, or
 * to creating a pull request from it; or stay to commit more.
 */
export function CommittedDialog() {
  let first: HTMLButtonElement | undefined;
  onMount(() => first?.focus());
  const made = () => state.committed!;
  const base = () => reviewBase();
  const branch = () => made().branch;
  /** Reviewing compares two branches; a commit on the base branch itself has nothing to compare. */
  const reviewable = () => !!branch() && !!base() && branch() !== base();
  const left = () => (state.work?.unstaged.length ?? 0) + (state.work?.staged.length ?? 0);
  const stay = () => setState("committed", null);
  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && stay()}
      onKeyDown={(event) => event.key === "Escape" && stay()}
    >
      <div class="modal complete-dialog committed-dialog" role="dialog" aria-label="Committed">
        <h2>
          <CheckIcon />
          Committed
        </h2>
        <p>
          <code>{made().sha.slice(0, 8)}</code> {made().subject}
          <br />
          <span class="pr-subtitle">
            on {branch() ?? "a detached HEAD"}
            {left() > 0 ? ` · ${plural(left(), "uncommitted file")} left` : ""}
          </span>
        </p>
        <Show when={!reviewable()}>
          <p class="complete-warning">
            {branch()
              ? `This commit is on ${branch()}, the base branch, so there is no branch to review against it.`
              : "The checkout is not on a branch, so there is no branch to review or create a pull request from."}
          </p>
        </Show>
        <div class="committed-choices">
          <button
            ref={(button) => reviewable() && (first = button)}
            class="committed-choice primary"
            disabled={!reviewable()}
            onClick={() => void reviewCommitted()}
          >
            <BranchIcon />
            <span>
              Review the branch
              <small>
                {branch() ?? "—"} against {base() ?? "—"}, in Branches
              </small>
            </span>
          </button>
          <Show when={currentRemote()}>
            <button
              class="committed-choice"
              disabled={!reviewable()}
              onClick={() => void createPullRequestFromCommitted()}
            >
              <PullRequestIcon />
              <span>
                Create a pull request
                <small>
                  from {branch() ?? "—"} into {base() ?? "—"} on Azure DevOps, then review it in Pull request
                </small>
              </span>
            </button>
          </Show>
          <button ref={(button) => !reviewable() && (first = button)} class="committed-choice" onClick={stay}>
            <CommitIcon />
            <span>
              Keep committing
              <small>{left() > 0 ? "Stay in Commit mode for the changes left" : "Stay in Commit mode"}</small>
            </span>
          </button>
        </div>
      </div>
    </div>
  );
}
