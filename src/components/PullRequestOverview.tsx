import { createEffect, createMemo, createSignal, For, on, onCleanup, Show } from "solid-js";
import { composerElement, isOpen } from "../editor/threads";
import type { CompletionPolicies, LinkedWorkItems, PrThread, Reviewer, UnmetPolicy, WorkItem } from "../ipc/commands";
import {
  completionPolicies,
  createGeneralThread,
  isChanged,
  linkedWorkItems,
  type ReviewedPullRequest,
  revealThread,
  setView,
  showOverview,
  state,
  threads,
} from "../state/review";
import { ago } from "./CommitPicker";
import { openInBrowser } from "./DevOpsSetup";
import { ThreadCard, threadsByFile, where } from "./DiscussionPicker";
import { splitPath } from "./FileDock";
import { CommentIcon, ExternalIcon, FilesIcon } from "./icons";
import { azureHostLinks, Markdown } from "./Markdown";
import { PullRequestNotifyButton } from "./PullRequestPanel";

const VOTE_LABELS: Record<Reviewer["vote"], { text: string; kind: string }> = {
  approved: { text: "Approved", kind: "approved" },
  approvedWithSuggestions: { text: "Approved with suggestions", kind: "approved" },
  waitingForAuthor: { text: "Waiting for author", kind: "waiting" },
  rejected: { text: "Rejected", kind: "rejected" },
  none: { text: "No vote", kind: "none" },
};

/** The pull request's state, as Azure DevOps names it. */
function statusTag(pullRequest: ReviewedPullRequest): { text: string; kind: string } {
  if (pullRequest.status === "completed") return { text: "Completed", kind: "completed" };
  if (pullRequest.status === "abandoned") return { text: "Abandoned", kind: "abandoned" };
  return pullRequest.draft ? { text: "Draft", kind: "draft" } : { text: "Active", kind: "active" };
}

/** How merging into the target branch went when Azure DevOps last tried. */
function mergeText(pullRequest: ReviewedPullRequest): Check {
  switch (pullRequest.mergeStatus) {
    case "succeeded":
      return { text: `No merge conflicts with ${pullRequest.targetBranch}`, kind: "ok" };
    case "conflicts":
      return { text: `Merge conflicts with ${pullRequest.targetBranch}`, kind: "failed" };
    case "queued":
      return { text: "Azure DevOps is checking for merge conflicts…", kind: "pending" };
    case "rejectedByPolicy":
      return { text: "The merge was rejected by a policy", kind: "failed" };
    case "failure":
      return { text: "Azure DevOps could not try the merge", kind: "failed" };
    default:
      return { text: "Merge not tried yet", kind: "pending" };
  }
}

/** A line of the Status card: `kind` colors it (ok, pending, failed); `state` goes on its right. */
interface Check {
  text: string;
  kind: string;
  state?: string;
  title?: string;
}

function policyState(policy: UnmetPolicy): { state: string; kind: string } {
  switch (policy.status) {
    case "running":
      return { state: "Running", kind: "pending" };
    case "queued":
      return { state: "Queued", kind: "pending" };
    case "broken":
      return { state: "Broken", kind: "failed" };
    case "expired":
      return { state: "Expired", kind: "failed" };
    default:
      return policy.approval ? { state: "Waiting", kind: "pending" } : { state: "Failed", kind: "failed" };
  }
}

/**
 * The required policies of the target branch the pull request does not meet
 * yet; read again whenever a vote, push or state could have changed them.
 */
function requiredPolicies(pullRequest: () => ReviewedPullRequest) {
  const [policies, setPolicies] = createSignal<CompletionPolicies | "loading" | { error: string } | null>(null);
  const key = () => {
    const now = pullRequest();
    if (now.status !== "active") return null;
    return [now.id, now.sourceCommit, now.draft, ...now.reviewers.map((reviewer) => `${reviewer.id}=${reviewer.vote}`)].join(" ");
  };
  createEffect(
    on(key, (now) => {
      if (now === null) return setPolicies(null);
      let current = true;
      onCleanup(() => (current = false));
      setPolicies((before) => (before === null || before === "loading" || "error" in before ? "loading" : before));
      completionPolicies().then(
        (read) => current && setPolicies(read),
        (error) => current && setPolicies({ error: String(error) }),
      );
    }),
  );
  return policies;
}

/** The work items linked to the pull request, read again when another one is opened. */
function workItemsOf(pullRequest: () => ReviewedPullRequest) {
  const [items, setItems] = createSignal<LinkedWorkItems | { error: string } | null>(null);
  createEffect(
    on(
      () => `${pullRequest().remote} ${pullRequest().id}`,
      () => {
        let current = true;
        onCleanup(() => (current = false));
        setItems(null);
        linkedWorkItems().then(
          (read) => current && setItems(read),
          (error) => current && setItems({ error: String(error) }),
        );
      },
    ),
  );
  return items;
}

/**
 * Where a pull request opens: its title, state, description, reviewers and
 * checks, and every discussion, each with a way to reply and, for a file's,
 * to show it in the diff.
 */
export function PullRequestOverview() {
  return <Show when={state.pullRequest}>{(pullRequest) => <Overview pullRequest={pullRequest()} />}</Show>;
}

function Overview(props: { pullRequest: ReviewedPullRequest }) {
  const [writing, setWriting] = createSignal(false);
  const policies = requiredPolicies(() => props.pullRequest);
  const workItems = workItemsOf(() => props.pullRequest);

  const general = createMemo(() => threads().filter((thread) => thread.path === null));
  const byFile = createMemo(() => threadsByFile(threads()));
  const active = () => threads().filter(isOpen).length;

  const totals = () => {
    let added = 0;
    let removed = 0;
    for (const file of state.comparison?.files ?? []) {
      added += file.additions ?? 0;
      removed += file.deletions ?? 0;
    }
    return { files: state.comparison?.files.length ?? 0, commits: state.comparison?.commits.length ?? 0, added, removed };
  };
  const showFiles = () => {
    showOverview(false);
    void setView("all");
  };
  /** What stands between the pull request and completing it, and what does not. */
  const checks = (): Check[] => {
    const list: Check[] = [];
    if (props.pullRequest.status === "active") list.push(mergeText(props.pullRequest));
    const read = policies();
    if (read === "loading") list.push({ text: "Checking the branch policies…", kind: "pending" });
    else if (read && "error" in read) list.push({ text: "Could not read the branch policies", kind: "pending", title: read.error });
    else if (read && read.unmet.length === 0) list.push({ text: "All required policies are met", kind: "ok" });
    else if (read) list.push(...read.unmet.map((policy) => ({ text: policy.name, ...policyState(policy) })));
    const open = active();
    list.push({ text: open === 0 ? "No active discussions" : `${open} active discussion${open === 1 ? "" : "s"}`, kind: open === 0 ? "ok" : "pending" });
    return list;
  };
  const reviewers = () => [...props.pullRequest.reviewers].sort((a, b) => Number(a.group) - Number(b.group));

  return (
    <div class="pr-overview" data-tauri-drag-region="false">
      <div class="pr-overview-page">
        <header class="pr-overview-head">
          <h1>
            {props.pullRequest.title}
            <span class="pr-overview-id">!{props.pullRequest.id}</span>
          </h1>
          <div class="pr-overview-meta">
            <span class={`tag tag-state-${statusTag(props.pullRequest).kind}`}>{statusTag(props.pullRequest).text}</span>
            <span>
              <strong>{props.pullRequest.author}</strong> wants to merge{" "}
              <code class="pr-overview-branch">{props.pullRequest.sourceBranch}</code> into{" "}
              <code class="pr-overview-branch">{props.pullRequest.targetBranch}</code>
            </span>
            <span class="pr-overview-when" title={new Date(props.pullRequest.created).toLocaleString()}>
              {ago(props.pullRequest.created)}
            </span>
            <span class="toolbar-space" />
            <Show when={props.pullRequest.status === "active"}>
              <PullRequestNotifyButton pullRequest={props.pullRequest} />
            </Show>
            <button onClick={() => openInBrowser(props.pullRequest.url)} title={props.pullRequest.url}>
              Open in Azure DevOps
              <ExternalIcon />
            </button>
          </div>
        </header>

        <div class="pr-overview-columns">
          <main class="pr-overview-main">
            <section class="pr-overview-card">
              <h2>Description</h2>
              <Show when={props.pullRequest.description.trim()} fallback={<p class="pr-overview-empty">No description.</p>}>
                <Markdown class="pr-overview-description" text={props.pullRequest.description} />
              </Show>
            </section>

            <WorkItems linked={workItems()} pullRequestUrl={props.pullRequest.url} />

            <section class="pr-overview-discussions">
              <div class="pr-overview-section-head">
                <h2>Discussions</h2>
                <span class="pr-overview-count">
                  {active()} active · {threads().length - active()} resolved or closed
                </span>
              </div>
              <Show
                when={writing()}
                fallback={
                  <button class="discussion-new" onClick={() => setWriting(true)}>
                    <CommentIcon />
                    Comment on the pull request…
                  </button>
                }
              >
                <div
                  class="thread draft"
                  ref={(host) =>
                    host.append(
                      composerElement({
                        key: "general",
                        placeholder: "Comment on the pull request as a whole…",
                        submitLabel: "Comment",
                        autofocus: true,
                        onSubmit: async (text) => {
                          await createGeneralThread(text);
                          setWriting(false);
                        },
                        onCancel: () => setWriting(false),
                      }),
                    )
                  }
                />
              </Show>
              <Show when={threads().length === 0}>
                <p class="pr-overview-empty">No one has commented on this pull request yet.</p>
              </Show>
              <Show when={general().length > 0}>
                <h3>Pull request</h3>
                <For each={general()}>{(thread) => <ThreadCard thread={thread} />}</For>
              </Show>
              <For each={byFile()}>
                {([path, list]) => (
                  <>
                    <h3 title={path}>
                      <span class="discussion-file">{splitPath(path).name}</span>
                      <span class="discussion-folder">{splitPath(path).directory}</span>
                    </h3>
                    <For each={list}>{(thread) => <FileThread path={path} thread={thread} />}</For>
                  </>
                )}
              </For>
            </section>
          </main>

          <aside class="pr-overview-side">
            <section class="pr-overview-card">
              <h2>Status</h2>
              <ul class="pr-overview-checks">
                <For each={checks()}>
                  {(check) => (
                    <li class={`check-${check.kind}`} title={check.title}>
                      <span>{check.text}</span>
                      <Show when={check.state}>
                        <span class="pr-overview-check-state">{check.state}</span>
                      </Show>
                    </li>
                  )}
                </For>
              </ul>
            </section>

            <section class="pr-overview-card">
              <h2>Reviewers</h2>
              <Show when={reviewers().length > 0} fallback={<p class="pr-overview-empty">No reviewers.</p>}>
                <ul class="pr-overview-reviewers">
                  <For each={reviewers()}>
                    {(reviewer) => (
                      <li>
                        <span class="pr-overview-reviewer" title={reviewer.group ? "A group" : undefined}>
                          {reviewer.name}
                          <Show when={reviewer.you}>
                            <span class="pr-overview-you"> (you)</span>
                          </Show>
                        </span>
                        <span class={`vote-${VOTE_LABELS[reviewer.vote].kind}`}>{VOTE_LABELS[reviewer.vote].text}</span>
                      </li>
                    )}
                  </For>
                </ul>
              </Show>
            </section>

            <section class="pr-overview-card">
              <h2>Changes</h2>
              <p>
                {totals().files} file{totals().files === 1 ? "" : "s"} <span class="added">+{totals().added}</span>{" "}
                <span class="removed">−{totals().removed}</span> in {totals().commits} commit{totals().commits === 1 ? "" : "s"}
              </p>
              <button class="pr-overview-files" onClick={showFiles}>
                <FilesIcon />
                Show all files
              </button>
            </section>
          </aside>
        </div>
      </div>
    </div>
  );
}

/**
 * The work items linked to the pull request, if any: the first with its
 * title and description, the others one line each.
 */
function WorkItems(props: { linked: LinkedWorkItems | { error: string } | null; pullRequestUrl: string }) {
  const read = () => (props.linked && "kind" in props.linked && props.linked.kind === "read" ? props.linked.items : []);
  const ids = () => (props.linked && "kind" in props.linked && props.linked.kind === "noAccess" ? props.linked.ids : []);
  const error = () => (props.linked && "error" in props.linked ? props.linked.error : null);
  const first = () => read()[0];
  const count = () => read().length || ids().length;
  return (
    <Show when={count() > 0 || error()}>
      <section class="pr-overview-card pr-overview-work-items">
        <div class="pr-overview-section-head">
          <h2>Work items</h2>
          <Show when={count() > 1}>
            <span class="pr-overview-count">{count()} linked</span>
          </Show>
        </div>
        <Show when={error()}>
          {(text) => <p class="pr-overview-empty">Could not read the linked work items: {text()}</p>}
        </Show>
        <Show when={first()}>
          {(item) => (
            <>
              <WorkItemLine item={item()} lead />
              <Show when={item().description.trim()} fallback={<p class="pr-overview-empty">No description.</p>}>
                <Markdown class="pr-overview-description" text={item().description} html={!item().markdown} />
              </Show>
            </>
          )}
        </Show>
        <Show when={read().length > 1}>
          <ul class="pr-overview-work-item-list">
            <For each={read().slice(1)}>
              {(item) => (
                <li>
                  <WorkItemLine item={item} />
                </li>
              )}
            </For>
          </ul>
        </Show>
        <Show when={ids().length > 0}>
          <p class="pr-overview-work-item-ids">
            <For each={ids()}>
              {(id) => {
                const url = azureHostLinks(props.pullRequestUrl)?.workItem(id);
                return url ? (
                  <button class="pr-overview-work-item-link" onClick={() => openInBrowser(url)} title={url}>
                    <span class="pr-overview-id">#{id}</span>
                    <ExternalIcon />
                  </button>
                ) : (
                  <span>#{id}</span>
                );
              }}
            </For>
          </p>
          <p class="pr-overview-empty">The token may not read work items (it needs Work Items → Read).</p>
        </Show>
      </section>
    </Show>
  );
}

/** A work item's type, number, title and state; a click opens it in Azure DevOps. */
function WorkItemLine(props: { item: WorkItem; lead?: boolean }) {
  return (
    <div class="pr-overview-work-item" classList={{ lead: props.lead }}>
      <span class="tag">{props.item.kind}</span>
      <button class="pr-overview-work-item-link" onClick={() => openInBrowser(props.item.url)} title={props.item.url}>
        <span class="pr-overview-id">#{props.item.id}</span>
        <span class="pr-overview-work-item-title">{props.item.title}</span>
        <ExternalIcon />
      </button>
      <span class="pr-overview-work-item-state">{props.item.state}</span>
    </div>
  );
}

/** A discussion about a file: where it is, a button to show it in the diff, and its card. */
function FileThread(props: { path: string; thread: PrThread }) {
  return (
    <div class="discussion-item">
      <button
        class="discussion-jump"
        disabled={!isChanged(props.path)}
        onClick={() => void revealThread(props.thread)}
        title={isChanged(props.path) ? "Show it in the diff" : "Not in the diff"}
      >
        {where(props.thread)} · Show in diff
      </button>
      <ThreadCard thread={props.thread} />
    </div>
  );
}
