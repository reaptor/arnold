import { createMemo, createSignal, For, onMount, Show } from "solid-js";
import type { PullRequest } from "../ipc/commands";
import { ago } from "./CommitPicker";
import { Markdown } from "./Markdown";

interface Props {
  pullRequests: PullRequest[];
  /** The one being reviewed. */
  current: number | null;
  /** The one being opened. */
  opening: number | null;
  onPick(pullRequest: PullRequest): void;
  /** Escape in the search field. */
  onEscape?(): void;
  /** Bigger rows, for the main panel. */
  roomy?: boolean;
}

/** Whether `pullRequest` matches every word of `query` (title, number, branches, author). */
export function matches(pullRequest: PullRequest, query: string): boolean {
  const haystack = [
    pullRequest.title,
    `!${pullRequest.id}`,
    `#${pullRequest.id}`,
    pullRequest.sourceBranch,
    pullRequest.targetBranch,
    pullRequest.author,
  ]
    .join(" ")
    .toLowerCase();
  return query
    .toLowerCase()
    .split(/\s+/)
    .every((word) => haystack.includes(word));
}

/** How the reviewers voted, in a few characters: "✓ 2", "waiting 1", "✕ 1", each with its color. */
export function votes(pullRequest: PullRequest): { parts: { text: string; kind: string }[]; title: string } | null {
  const people = pullRequest.reviewers;
  const count = (...kinds: string[]) => people.filter((reviewer) => kinds.includes(reviewer.vote)).length;
  const approved = count("approved", "approvedWithSuggestions");
  const waiting = count("waitingForAuthor");
  const rejected = count("rejected");
  const parts = [
    { count: approved, text: `✓ ${approved}`, kind: "approved" },
    { count: waiting, text: `waiting ${waiting}`, kind: "waiting" },
    { count: rejected, text: `✕ ${rejected}`, kind: "rejected" },
  ].filter((part) => part.count > 0);
  if (parts.length === 0) return null;
  const title = people
    .filter((reviewer) => reviewer.vote !== "none")
    .map((reviewer) => `${reviewer.name}: ${VOTE_NAMES[reviewer.vote]}`)
    .join("\n");
  return { parts, title };
}

/** Approved by someone, and no one is waiting for the author or has rejected it. */
export function isApproved(pullRequest: PullRequest): boolean {
  const voted = (...kinds: string[]) => pullRequest.reviewers.some((reviewer) => kinds.includes(reviewer.vote));
  return voted("approved", "approvedWithSuggestions") && !voted("waitingForAuthor", "rejected");
}

const VOTE_NAMES: Record<string, string> = {
  approved: "approved",
  approvedWithSuggestions: "approved with suggestions",
  waitingForAuthor: "waiting for author",
  rejected: "rejected",
};

/** The description's text without its Markdown marks, on one line, for the tooltip. */
function plainText(markdown: string): string {
  return markdown
    .replace(/!\[[^\]]*\]\([^)]*\)/g, "")
    .replace(/\[([^\]]*)\]\([^)]*\)/g, "$1")
    .replace(/^\s{0,3}(#{1,6}|>|[-*+]|\d+\.)\s+/gm, "")
    .replace(/(\*{1,3}|`+|~~)(\S(?:.*?\S)?)\1/g, "$2")
    .replace(/(^|\W)_{1,3}(\S(?:.*?\S)?)_{1,3}(?=\W|$)/g, "$1$2")
    .replace(/\s+/g, " ")
    .trim();
}

/** A searchable list of pull requests; arrow keys move, Enter or a click picks. */
export function PullRequestList(props: Props) {
  const [query, setQuery] = createSignal("");
  const [active, setActive] = createSignal(0);
  let input!: HTMLInputElement;
  let list!: HTMLUListElement;

  const shown = createMemo(() => props.pullRequests.filter((pullRequest) => matches(pullRequest, query())));

  // Not scrolled into view: a toolbar popup is moved into the window only after this.
  onMount(() => input.focus({ preventScroll: true }));

  const move = (position: number) => {
    setActive(position);
    list.children[position]?.scrollIntoView({ block: "nearest" });
  };
  const onKeyDown = (event: KeyboardEvent) => {
    if (event.key === "Escape") props.onEscape?.();
    else if (event.key === "Enter") {
      const pick = shown()[active()];
      if (pick) props.onPick(pick);
    } else if (event.key === "ArrowDown") move(Math.min(active() + 1, shown().length - 1));
    else if (event.key === "ArrowUp") move(Math.max(active() - 1, 0));
    else return;
    event.preventDefault();
  };

  return (
    <div class="pr-list" classList={{ roomy: props.roomy }}>
      <input
        ref={input}
        class="picker-search"
        placeholder="Find by title, number, branch or author"
        value={query()}
        onInput={(event) => {
          setQuery(event.currentTarget.value);
          setActive(0);
        }}
        onKeyDown={onKeyDown}
      />
      <ul class="picker-list" ref={list}>
        <For each={shown()} fallback={<li class="picker-empty">No matching pull request</li>}>
          {(pullRequest, position) => (
            <li
              classList={{
                active: position() === active(),
                current: pullRequest.id === props.current,
                busy: props.opening !== null,
              }}
              onPointerEnter={() => setActive(position())}
              onClick={() => props.onPick(pullRequest)}
              title={plainText(pullRequest.description) || pullRequest.title}
            >
              <div class="pr-row">
                <div class="pr-line">
                  <span class="picker-name">{pullRequest.title}</span>
                  <Show when={pullRequest.draft}>
                    <span class="tag">Draft</span>
                  </Show>
                  <Show when={isApproved(pullRequest)}>
                    <span class="tag tag-approved">✓ Approved</span>
                  </Show>
                  <Show
                    when={props.opening === pullRequest.id}
                    fallback={
                      <span class="commit-meta" title={new Date(pullRequest.created).toLocaleString()}>
                        {ago(pullRequest.created)}
                      </span>
                    }
                  >
                    <span class="commit-meta pr-opening">Opening…</span>
                  </Show>
                </div>
                <Show when={pullRequest.description.trim()}>
                  <Markdown class="pr-description" text={pullRequest.description} />
                </Show>
                <div class="pr-line pr-details">
                  <span class="pr-id">!{pullRequest.id}</span>
                  <span class="pr-branches" title={`${pullRequest.sourceBranch} into ${pullRequest.targetBranch}`}>
                    {pullRequest.sourceBranch} → {pullRequest.targetBranch}
                  </span>
                  <span class="pr-author">{pullRequest.author}</span>
                  <Show when={votes(pullRequest)}>
                    {(summary) => (
                      <span class="pr-votes" title={summary().title}>
                        <For each={summary().parts}>{(part) => <span class={`vote-${part.kind}`}>{part.text}</span>}</For>
                      </span>
                    )}
                  </Show>
                </div>
              </div>
            </li>
          )}
        </For>
      </ul>
    </div>
  );
}
