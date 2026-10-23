// What changed on a pull request the user follows since it was last looked
// at, in words for a notification. The checking and notifying is in
// prNotifications.ts.

import type { PrThread, PullRequest, Vote } from "../ipc/commands";

/** A pull request as it was at the last check. */
export interface Seen {
  pullRequest: PullRequest;
  /** Its comments, as `thread:comment`; null when they could not be read. */
  comments: Set<string> | null;
}

/** News of one pull request: a line per change, and the discussion to go to for the first new comment. */
export interface News {
  lines: string[];
  thread: number | null;
}

export const commentKeys = (threads: readonly PrThread[]) =>
  new Set(threads.flatMap((thread) => thread.comments.map((comment) => `${thread.id}:${comment.id}`)));

const VOTED: Record<Vote, string> = {
  approved: "approved",
  approvedWithSuggestions: "approved with suggestions",
  waitingForAuthor: "is waiting for the author",
  rejected: "rejected",
  none: "reset their vote",
};

const STATUS: Record<string, string> = {
  completed: "Completed",
  abandoned: "Abandoned",
  active: "Reactivated",
};

/** What changed on `now` since `before`; `threads` are its discussions now, null when they could not be read. */
export function newsOf(before: Seen, now: PullRequest, threads: readonly PrThread[] | null): News {
  const was = before.pullRequest;
  const lines: string[] = [];
  if (now.status !== was.status && STATUS[now.status]) lines.push(STATUS[now.status]);
  if (was.draft && !now.draft && !now.yours) lines.push("Published: ready for review");
  const reviewing = (pullRequest: PullRequest) => pullRequest.reviewers.some((reviewer) => reviewer.you);
  if (!now.yours && reviewing(now) && !reviewing(was)) lines.push("You were added as a reviewer");
  for (const reviewer of now.reviewers) {
    if (reviewer.you) continue;
    const earlier = was.reviewers.find((candidate) => candidate.id === reviewer.id)?.vote ?? "none";
    if (reviewer.vote !== earlier && reviewer.vote !== "none") lines.push(`${reviewer.name} ${VOTED[reviewer.vote]}`);
  }
  // Someone else's push; the user's own is no news.
  if (!now.yours && was.sourceCommit && now.sourceCommit && now.sourceCommit !== was.sourceCommit) {
    lines.push("New changes pushed");
  }
  if (now.yours && now.mergeStatus === "conflicts" && was.mergeStatus !== "conflicts") {
    lines.push(`Merge conflicts with ${now.targetBranch}`);
  }
  const comments = threads && before.comments ? newComments(before.comments, threads) : null;
  if (comments) lines.unshift(comments.line);
  return { lines, thread: comments?.thread ?? null };
}

/** Comments of others that are not in `seen`, in a line; null when there are none. */
function newComments(seen: Set<string>, threads: readonly PrThread[]): { line: string; thread: number } | null {
  const added = threads
    .flatMap((thread) => thread.comments.map((comment) => ({ thread: thread.id, comment })))
    .filter(({ thread, comment }) => !comment.you && !seen.has(`${thread}:${comment.id}`))
    .sort((a, b) => a.comment.published.localeCompare(b.comment.published));
  const first = added[0];
  if (!first) return null;
  if (added.length === 1) return { line: `${first.comment.author}: ${excerpt(first.comment.content)}`, thread: first.thread };
  const authors = [...new Set(added.map(({ comment }) => comment.author))];
  return { line: `${added.length} new comments from ${listed(authors)}`, thread: first.thread };
}

/** News of a pull request that is new since the last check (followed as one of all in the repository). */
export function addedNews(now: PullRequest): News {
  if (now.yours) return { lines: [], thread: null };
  const reviewing = now.reviewers.some((reviewer) => reviewer.you);
  return { lines: [reviewing ? `${now.author} added you as a reviewer` : `New pull request by ${now.author}`], thread: null };
}

/** "A", "A and B", "A, B and C". */
function listed(names: string[]): string {
  return names.length < 2 ? (names[0] ?? "") : `${names.slice(0, -1).join(", ")} and ${names[names.length - 1]}`;
}

const EXCERPT = 140;

/** A comment's text in short, on one line; a suggestion says so. */
function excerpt(markdown: string): string {
  const text = plainText(markdown.replace(/^\s*(`{3,}|~{3,})\s*suggestion\b[\s\S]*?^\s*\1\s*$/gm, " (suggested change) "));
  return text.length > EXCERPT ? `${text.slice(0, EXCERPT - 1).trimEnd()}…` : text || "(suggested change)";
}

/** Markdown's text without its marks, on one line. */
export function plainText(markdown: string): string {
  return markdown
    .replace(/!\[[^\]]*\]\([^)]*\)/g, "")
    .replace(/\[([^\]]*)\]\([^)]*\)/g, "$1")
    .replace(/^\s{0,3}(#{1,6}|>|[-*+]|\d+\.)\s+/gm, "")
    .replace(/(\*{1,3}|`+|~~)(\S(?:.*?\S)?)\1/g, "$2")
    .replace(/(^|\W)_{1,3}(\S(?:.*?\S)?)_{1,3}(?=\W|$)/g, "$1$2")
    .replace(/\s+/g, " ")
    .trim();
}
