import { createSignal, For, type JSX, onCleanup, onMount, Show } from "solid-js";
import type { CompletionPolicies, MergeStrategy, UnmetPolicy, Vote } from "../ipc/commands";
import {
  completionPolicies,
  setState,
  state,
  updatePullRequest,
  votePullRequest,
  yourVote,
  type ReviewedPullRequest,
} from "../state/review";
import { CheckIcon, ChevronDownIcon, MergeIcon } from "./icons";

/** Shows why a change from a menu did not go through, in the error banner. */
const fail = (error: unknown) => setState("error", String(error));

/** A button with a menu: the main part does the usual thing, the arrow opens the rest. */
function SplitButton(props: {
  class?: string;
  label: JSX.Element;
  title: string;
  /** Without it the main part opens the menu too. */
  onMain?(): void;
  busy: boolean;
  disabled?: boolean;
  /** Menu entries; none means no arrow. */
  children?: (close: () => void) => JSX.Element;
}) {
  const [open, setOpen] = createSignal(false);
  let root!: HTMLDivElement;

  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) setOpen(false);
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  const close = () => setOpen(false);
  return (
    <div
      class={`split-button ${props.class ?? ""}`}
      classList={{ busy: props.busy }}
      ref={root}
      onKeyDown={(event) => event.key === "Escape" && close()}
    >
      <button
        class="split-main"
        classList={{ alone: !props.children }}
        onClick={() => (props.onMain ? props.onMain() : setOpen(!open()))}
        disabled={props.busy || props.disabled}
        title={props.title}
      >
        {props.label}
      </button>
      <Show when={props.children}>
        <button
          class="split-more"
          onClick={() => setOpen(!open())}
          disabled={props.busy}
          aria-haspopup="menu"
          aria-expanded={open()}
          title="More"
        >
          <ChevronDownIcon />
        </button>
      </Show>
      <Show when={open() && props.children}>
        <ul class="menu action-menu" role="menu" data-tauri-drag-region="false">
          {props.children!(close)}
        </ul>
      </Show>
    </div>
  );
}

function MenuItem(props: { onClick(): void; checked?: boolean; disabled?: boolean; title?: string; children: JSX.Element }) {
  return (
    <li
      role="menuitem"
      classList={{ disabled: props.disabled, checked: props.checked }}
      title={props.title}
      onClick={() => !props.disabled && props.onClick()}
    >
      <span class="menu-check">{props.checked ? <CheckIcon /> : null}</span>
      <span class="menu-label">{props.children}</span>
    </li>
  );
}

const VOTES: { vote: Exclude<Vote, "none">; label: string; done: string }[] = [
  { vote: "approved", label: "Approve", done: "Approved" },
  { vote: "approvedWithSuggestions", label: "Approve with suggestions", done: "Approved with suggestions" },
  { vote: "waitingForAuthor", label: "Wait for author", done: "Waiting for author" },
  { vote: "rejected", label: "Reject", done: "Rejected" },
];

const VOTE_CLASS: Record<Vote, string> = {
  approved: "vote-approved",
  approvedWithSuggestions: "vote-approved",
  waitingForAuthor: "vote-waiting",
  rejected: "vote-rejected",
  none: "",
};

/** Your vote, as in Azure DevOps: "Approve" until you voted, then what you voted. */
function VoteButton() {
  const voted = () => VOTES.find((entry) => entry.vote === yourVote());
  return (
    <SplitButton
      class={`vote-button ${VOTE_CLASS[yourVote()]}`}
      label={
        <>
          <CheckIcon />
          <span class="action-label">{state.updatingPullRequest === "vote" ? "Voting…" : (voted()?.done ?? "Approve")}</span>
        </>
      }
      title={voted() ? `You voted: ${voted()!.done.toLowerCase()}` : "Approve the pull request"}
      onMain={voted() ? undefined : () => void votePullRequest("approved")}
      busy={state.updatingPullRequest !== null}
    >
      {(close) => (
        <>
          <For each={VOTES}>
            {(entry) => (
              <MenuItem
                checked={yourVote() === entry.vote}
                onClick={() => {
                  close();
                  if (yourVote() !== entry.vote) void votePullRequest(entry.vote);
                }}
              >
                {entry.label}
              </MenuItem>
            )}
          </For>
          <li class="menu-separator" />
          <MenuItem
            disabled={yourVote() === "none"}
            onClick={() => {
              close();
              void votePullRequest("none");
            }}
          >
            Reset feedback
          </MenuItem>
        </>
      )}
    </SplitButton>
  );
}

const change = (kind: "abandon" | "reactivate") => updatePullRequest({ kind }).catch(fail);
const setDraft = (draft: boolean) => updatePullRequest({ kind: "draft", draft }).catch(fail);

/**
 * Complete (or Publish for a draft, Reactivate for an abandoned one), with
 * Mark as draft and Abandon in its menu, as in Azure DevOps.
 */
function CompleteButton(props: { pullRequest: ReviewedPullRequest; onComplete(): void }) {
  const busy = () => state.updatingPullRequest !== null;
  const doing = (kind: string, text: string) => (state.updatingPullRequest === kind ? text : null);
  const status = () => props.pullRequest.status;
  return (
    <Show
      when={status() === "active"}
      fallback={
        <Show when={status() === "abandoned"} fallback={<span class="tag tag-completed">Completed</span>}>
          <span class="tag tag-abandoned">Abandoned</span>
          <SplitButton
            class="complete-button"
            label={<span class="action-label">{doing("reactivate", "Reactivating…") ?? "Reactivate"}</span>}
            title="Open the pull request again"
            onMain={() => void change("reactivate")}
            busy={busy()}
          />
        </Show>
      }
    >
      <SplitButton
        class="complete-button primary"
        label={
          <>
            <MergeIcon />
            <span class="action-label">
              {props.pullRequest.draft
                ? (doing("draft", "Publishing…") ?? "Publish")
                : (doing("complete", "Completing…") ?? doing("draft", "Saving…") ?? doing("abandon", "Abandoning…") ?? "Complete")}
            </span>
          </>
        }
        title={
          props.pullRequest.draft
            ? "Publish the draft: reviewers are notified and it can be completed"
            : `Merge ${props.pullRequest.sourceBranch} into ${props.pullRequest.targetBranch}`
        }
        onMain={props.pullRequest.draft ? () => void setDraft(false) : props.onComplete}
        busy={busy()}
      >
        {(close) => (
          <>
            <Show when={!props.pullRequest.draft}>
              <MenuItem
                title="Back to a draft: not ready for review, cannot be completed"
                onClick={() => {
                  close();
                  void setDraft(true);
                }}
              >
                Mark as draft
              </MenuItem>
            </Show>
            <MenuItem
              title="Close the pull request without merging; it can be reactivated"
              onClick={() => {
                close();
                void change("abandon");
              }}
            >
              Abandon
            </MenuItem>
          </>
        )}
      </SplitButton>
    </Show>
  );
}

/** Voting on and completing the pull request under review, for the toolbar. */
export function PullRequestActions() {
  const [completing, setCompleting] = createSignal(false);
  return (
    <Show when={state.pullRequest}>
      {(pullRequest) => (
        <div class="pr-actions" data-tauri-drag-region="false">
          <Show when={pullRequest().status === "active"}>
            <VoteButton />
          </Show>
          <CompleteButton pullRequest={pullRequest()} onComplete={() => setCompleting(true)} />
          <Show when={completing()}>
            <CompleteDialog pullRequest={pullRequest()} onClose={() => setCompleting(false)} />
          </Show>
        </div>
      )}
    </Show>
  );
}

// ------------------------------------------------------------- completing

const STRATEGIES: { strategy: MergeStrategy; label: string; description: string }[] = [
  {
    strategy: "noFastForward",
    label: "Merge (no fast forward)",
    description: "Keeps every commit of the source branch and adds a merge commit.",
  },
  {
    strategy: "squash",
    label: "Squash commit",
    description: "One new commit on the target branch with all of the changes.",
  },
  {
    strategy: "rebase",
    label: "Rebase and fast-forward",
    description: "Rebases the source commits onto the target branch, without a merge commit.",
  },
  {
    strategy: "rebaseMerge",
    label: "Semi-linear merge",
    description: "Rebases the source commits onto the target branch, then adds a merge commit.",
  },
];

interface CompleteChoices {
  mergeStrategy: MergeStrategy;
  deleteSourceBranch: boolean;
  completeWorkItems: boolean;
}

/** Remembered between pull requests, as Azure DevOps does. */
const CHOICES_KEY = "arnold.completeChoices";

function loadChoices(): CompleteChoices {
  const defaults: CompleteChoices = { mergeStrategy: "noFastForward", deleteSourceBranch: true, completeWorkItems: true };
  try {
    const saved = JSON.parse(localStorage.getItem(CHOICES_KEY) ?? "{}");
    return {
      mergeStrategy: STRATEGIES.some((entry) => entry.strategy === saved.mergeStrategy) ? saved.mergeStrategy : defaults.mergeStrategy,
      deleteSourceBranch: typeof saved.deleteSourceBranch === "boolean" ? saved.deleteSourceBranch : defaults.deleteSourceBranch,
      completeWorkItems: typeof saved.completeWorkItems === "boolean" ? saved.completeWorkItems : defaults.completeWorkItems,
    };
  } catch {
    return defaults;
  }
}

/** The reviewers who are holding the pull request up, for a warning. */
function holdingUp(pullRequest: ReviewedPullRequest): string | null {
  const names = (vote: Vote) => pullRequest.reviewers.filter((reviewer) => reviewer.vote === vote).map((reviewer) => reviewer.name);
  const parts = [
    names("rejected").length ? `rejected by ${names("rejected").join(", ")}` : "",
    names("waitingForAuthor").length ? `waiting for the author according to ${names("waitingForAuthor").join(", ")}` : "",
  ].filter(Boolean);
  return parts.length ? `This pull request is ${parts.join(" and ")}.` : null;
}

/** The branch policies of the target branch, as far as completing goes. */
type PolicyCheck = { state: "checking" } | { state: "read"; policies: CompletionPolicies } | { state: "failed"; error: string };

const UNMET_STATUS: Record<UnmetPolicy["status"], string> = {
  queued: "queued",
  running: "running",
  rejected: "not met",
  broken: "broken",
};

/** Azure DevOps' "Complete pull request" dialog: how to merge and what to do afterwards. */
function CompleteDialog(props: { pullRequest: ReviewedPullRequest; onClose(): void }) {
  const initial = loadChoices();
  const [strategy, setStrategy] = createSignal(initial.mergeStrategy);
  const [deleteSource, setDeleteSource] = createSignal(initial.deleteSourceBranch);
  const [workItems, setWorkItems] = createSignal(initial.completeWorkItems);
  const [message, setMessage] = createSignal(`Merged PR ${props.pullRequest.id}: ${props.pullRequest.title}`);
  const [error, setError] = createSignal<string | null>(null);
  const [check, setCheck] = createSignal<PolicyCheck>({ state: "checking" });
  const radios = new Map<MergeStrategy, HTMLInputElement>();

  const read = () => {
    const now = check();
    return now.state === "read" ? now.policies : null;
  };
  /** Unknown policies allow everything: the host still checks them when completing. */
  const allowed = (entry: MergeStrategy) => read()?.mergeStrategies.includes(entry) ?? true;
  const unmet = () => read()?.unmet ?? [];
  const failure = () => {
    const now = check();
    return now.state === "failed" ? now.error : null;
  };

  const checkPolicies = async () => {
    setCheck({ state: "checking" });
    try {
      const policies = await completionPolicies();
      setCheck({ state: "read", policies });
      // The remembered merge type may not be allowed on this branch.
      if (!policies.mergeStrategies.includes(strategy()) && policies.mergeStrategies.length > 0) {
        const focused = document.activeElement === radios.get(strategy());
        setStrategy(policies.mergeStrategies[0]);
        if (focused) radios.get(strategy())?.focus();
      }
    } catch (text) {
      setCheck({ state: "failed", error: String(text) });
    }
  };

  onMount(() => {
    radios.get(strategy())?.focus();
    void checkPolicies();
  });

  const busy = () => state.updatingPullRequest === "complete";
  const canComplete = () =>
    !busy() && !!props.pullRequest.sourceCommit && check().state !== "checking" && unmet().length === 0 && allowed(strategy());
  const close = () => !busy() && props.onClose();

  const submit = async (event: Event) => {
    event.preventDefault();
    const sourceCommit = props.pullRequest.sourceCommit;
    if (!sourceCommit || !canComplete()) return;
    const choices: CompleteChoices = {
      mergeStrategy: strategy(),
      deleteSourceBranch: deleteSource(),
      completeWorkItems: workItems(),
    };
    localStorage.setItem(CHOICES_KEY, JSON.stringify(choices));
    setError(null);
    try {
      await updatePullRequest({ kind: "complete", sourceCommit, message: message(), ...choices });
      props.onClose();
    } catch (text) {
      setError(String(text));
    }
  };

  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && close()}
      onKeyDown={(event) => event.key === "Escape" && close()}
    >
      <form class="modal complete-dialog" role="dialog" aria-label="Complete pull request" onSubmit={submit}>
        <h2>
          <MergeIcon />
          Complete pull request !{props.pullRequest.id}
        </h2>
        <p class="pr-subtitle">
          Merges <strong>{props.pullRequest.sourceBranch}</strong> into <strong>{props.pullRequest.targetBranch}</strong>{" "}
          in Azure DevOps, as of commit <code>{props.pullRequest.sourceCommit?.slice(0, 8)}</code>, the one shown here.
        </p>
        <Show when={props.pullRequest.mergeStatus === "conflicts"}>
          <p class="connect-problem">
            Azure DevOps found merge conflicts with {props.pullRequest.targetBranch}; they have to be resolved before
            the pull request can be completed.
          </p>
        </Show>
        <Show when={holdingUp(props.pullRequest)}>{(text) => <p class="complete-warning">{text()}</p>}</Show>
        <Show when={unmet().length > 0}>
          <div class="complete-warning complete-unmet">
            <p>
              Required branch policies of {props.pullRequest.targetBranch} are not met yet, so Azure DevOps does not
              complete the pull request:
            </p>
            <ul class="protected-policies">
              <For each={unmet()}>
                {(policy) => (
                  <li>
                    {policy.name} <span class="complete-policy-status">({UNMET_STATUS[policy.status] ?? policy.status})</span>
                  </li>
                )}
              </For>
            </ul>
            <button type="button" class="link-button" onClick={() => void checkPolicies()}>
              Check again
            </button>
          </div>
        </Show>
        <Show when={failure()}>
          {(text) => (
            <p class="complete-warning">
              Could not read the branch policies of {props.pullRequest.targetBranch} ({text()}); Azure DevOps checks
              them when completing.
            </p>
          )}
        </Show>
        <Show when={read()?.mergeStrategies.length === 0}>
          <p class="connect-problem">
            The branch policies of {props.pullRequest.targetBranch} allow none of the merge types, so the pull request
            cannot be completed.
          </p>
        </Show>

        <fieldset class="complete-strategies">
          <legend>Merge type</legend>
          <For each={STRATEGIES}>
            {(entry) => (
              <label
                class="complete-option"
                classList={{ disabled: !allowed(entry.strategy) }}
                title={allowed(entry.strategy) ? undefined : `Not allowed by the branch policies of ${props.pullRequest.targetBranch}`}
              >
                <input
                  ref={(input) => radios.set(entry.strategy, input)}
                  type="radio"
                  name="strategy"
                  checked={strategy() === entry.strategy}
                  disabled={!allowed(entry.strategy)}
                  onChange={() => setStrategy(entry.strategy)}
                />
                <span>
                  {entry.label}
                  <small>
                    {allowed(entry.strategy)
                      ? entry.description
                      : `Not allowed by the branch policies of ${props.pullRequest.targetBranch}.`}
                  </small>
                </span>
              </label>
            )}
          </For>
        </fieldset>

        <Show when={strategy() !== "rebase"}>
          <label class="complete-message">
            <span>{strategy() === "squash" ? "Commit message" : "Merge commit message"}</span>
            <textarea rows={3} value={message()} onInput={(event) => setMessage(event.currentTarget.value)} />
          </label>
        </Show>

        <fieldset class="complete-after">
          <legend>After merging</legend>
          <label class="complete-option">
            <input type="checkbox" checked={workItems()} onChange={(event) => setWorkItems(event.currentTarget.checked)} />
            <span>Complete associated work items</span>
          </label>
          <label class="complete-option">
            <input type="checkbox" checked={deleteSource()} onChange={(event) => setDeleteSource(event.currentTarget.checked)} />
            <span>
              Delete <strong>{props.pullRequest.sourceBranch}</strong>
            </span>
          </label>
        </fieldset>

        <Show when={error()}>{(text) => <p class="connect-problem">{text()}</p>}</Show>

        <div class="connect-actions">
          <span class="toolbar-space" />
          <button type="button" onClick={close} disabled={busy()}>
            Cancel
          </button>
          <button type="submit" class="primary" disabled={!canComplete()}>
            {busy() ? "Completing…" : check().state === "checking" ? "Checking policies…" : "Complete merge"}
          </button>
        </div>
      </form>
    </div>
  );
}
