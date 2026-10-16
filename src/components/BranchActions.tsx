import { onMount, Show } from "solid-js";
import {
  pushAndCreatePullRequest,
  pushAndViewPullRequest,
  pushBranch,
  reviewBase,
  reviewBranch,
  whyNoPush,
  whyNoReview,
} from "../state/commitMode";
import { currentRemote, loadPullRequestsOnce, pullRequestFrom } from "../state/pullRequests";
import { state } from "../state/review";
import { ArrowUpIcon, BranchIcon, PullRequestIcon } from "./icons";

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
