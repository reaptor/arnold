import { onMount, Show } from "solid-js";
import {
  pullBranch,
  pushAndCreatePullRequest,
  pushAndViewPullRequest,
  pushBranch,
  reviewBase,
  reviewBranch,
  whyNoPull,
  whyNoPush,
  whyNoReview,
} from "../state/commitMode";
import { currentRemote, loadPullRequestsOnce, pullRequestFrom } from "../state/pullRequests";
import { fetching, headBranch, pushHead, pushingHead, state, whyNoPushHead } from "../state/review";
import { ArrowDownIcon, ArrowUpIcon, BranchIcon, PullRequestIcon } from "./icons";

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

/** What to do with the branch once its work is committed: review it, push it, open a pull request. */
export function BranchActions() {
  const branch = () => state.work?.branch ?? "the branch";
  const base = () => reviewBase() ?? "the base branch";
  const sync = () => state.work?.sync ?? null;
  const toPush = () => {
    const current = sync();
    if (!current || whyNoPush()) return "";
    return current.published
      ? `${plural(current.ahead, "commit")} to ${current.remote}/${current.remoteBranch}`
      : `${branch()} to ${current.remote} as a new branch${current.ahead > 0 ? ` (${plural(current.ahead, "commit")})` : ""}`;
  };
  const pullTitle = () => {
    const current = sync()!;
    const remote = `${current.remote}/${current.remoteBranch}`;
    const known = !current.published
      ? `${remote} does not exist yet (as of the last fetch).`
      : current.behind > 0
        ? `${remote} has ${plural(current.behind, "commit")} that ${branch()} lacks (as of the last fetch).`
        : `${branch()} has everything ${remote} had at the last fetch.`;
    return `Pull: fetch every branch of every remote, then bring ${branch()} up to ${remote}\n${known}\nA merge is only made when there are no uncommitted changes; never forced.`;
  };
  const prBlocked = () => whyNoReview() ?? (sync()?.behind ? whyNoPush() : null);
  /** The branch's active pull request, if it has one: viewed rather than created again. */
  const existing = () => (state.work?.branch ? pullRequestFrom(state.work.branch, reviewBase() ?? undefined) : null);
  const prTitle = () => {
    const pushFirst = whyNoPush() ? "" : `Push ${toPush()}, then `;
    const pullRequest = existing();
    if (pullRequest) {
      return `${pushFirst}${pushFirst ? "review" : "Review"} pull request !${pullRequest.id}: ${pullRequest.title}\n${pullRequest.sourceBranch} into ${pullRequest.targetBranch}, open already`;
    }
    return prBlocked() ?? `${pushFirst}${pushFirst ? "create" : "Create"} a pull request from ${branch()} into ${base()} on Azure DevOps`;
  };
  const prLabel = () => {
    if (whyNoPush()) return existing() ? "View PR" : "Create PR";
    return existing() ? "Push & view PR" : "Push & create PR";
  };
  onMount(loadPullRequestsOnce);
  return (
    <div class="branch-actions" data-tauri-drag-region="false">
      <button
        type="button"
        disabled={!!state.committing || !!whyNoReview()}
        onClick={() => void reviewBranch()}
        title={whyNoReview() ?? `Review ${branch()} against ${base()}, in Branches`}
      >
        <BranchIcon />
        <span class="action-label">Review against {base()}</span>
      </button>
      <button
        type="button"
        disabled={!!state.committing || fetching() || !!whyNoPull()}
        onClick={() => void pullBranch()}
        title={whyNoPull() ?? pullTitle()}
      >
        <ArrowDownIcon />
        <span class="action-label">Pull</span>
        <Show when={sync()?.behind}>
          <span class="push-count">{sync()?.behind}</span>
        </Show>
      </button>
      <button
        type="button"
        disabled={!!state.committing || !!whyNoPush()}
        onClick={() => void pushBranch()}
        title={whyNoPush() ?? `Push ${toPush()}`}
      >
        <ArrowUpIcon />
        <span class="action-label">Push</span>
        <Show when={!whyNoPush() && sync()?.ahead}>
          <span class="push-count">{sync()?.ahead}</span>
        </Show>
      </button>
      <Show when={currentRemote()}>
        {(remote) => (
          <button
            type="button"
            class="create-pr-button"
            disabled={!!state.committing || (existing() ? state.openingPullRequest !== null : !!prBlocked())}
            onClick={() => {
              const pullRequest = existing();
              if (pullRequest) void pushAndViewPullRequest(remote().remote, pullRequest.id);
              else void pushAndCreatePullRequest();
            }}
            title={prTitle()}
          >
            <PullRequestIcon />
            <span class="action-label">{prLabel()}</span>
          </button>
        )}
      </Show>
    </div>
  );
}

/** Push in Branches: sends the compared branch's commits to its remote. */
export function PushHeadButton() {
  const sync = () => headBranch()?.sync ?? null;
  const title = () => {
    const why = whyNoPushHead();
    if (why) return why;
    const current = sync()!;
    const what = current.published
      ? `${plural(current.ahead, "commit")} of ${state.head} to ${current.remote}/${current.remoteBranch}`
      : `${state.head} to ${current.remote} as a new branch${current.ahead > 0 ? ` (${plural(current.ahead, "commit")})` : ""}`;
    const edits = state.comparison?.uncommitted ?? 0;
    return `Push ${what}${edits > 0 ? `\nUncommitted edits are not pushed: commit them first (${plural(edits, "file")}).` : ""}`;
  };
  return (
    <div class="branch-actions" data-tauri-drag-region="false">
      <button type="button" disabled={pushingHead() || !!whyNoPushHead()} onClick={() => void pushHead()} title={title()}>
        <ArrowUpIcon />
        <span class="action-label">{pushingHead() ? "Pushing…" : "Push"}</span>
        <Show when={!whyNoPushHead() && sync()?.ahead}>
          <span class="push-count">{sync()?.ahead}</span>
        </Show>
      </button>
    </div>
  );
}
