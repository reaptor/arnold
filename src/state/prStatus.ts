// Where a pull request stands and whether it waits on the user, for the
// dashboard of the pull requests of every favorite repository.

import type { PullRequest, Vote } from "../ipc/commands";

/** The colour a status is shown in. */
export type StatusKind = "draft" | "blocked" | "waiting" | "approved" | "review";

export interface PrStatus {
  kind: StatusKind;
  label: string;
}

/** Someone who opens pull requests, for the dashboard's filter. */
export interface Creator {
  /** The host's id; empty when the host gave none. */
  id: string;
  name: string;
}

const voted = (pullRequest: PullRequest, ...votes: Vote[]) =>
  pullRequest.reviewers.some((reviewer) => votes.includes(reviewer.vote));

const mergeFailed = (pullRequest: PullRequest) =>
  pullRequest.mergeStatus === "rejectedByPolicy" || pullRequest.mergeStatus === "failure";

/** Where the pull request stands, the most pressing first: draft, cannot merge, rejected, waiting for its author, approved, in review. */
export function prStatus(pullRequest: PullRequest): PrStatus {
  if (pullRequest.draft) return { kind: "draft", label: "Draft" };
  if (pullRequest.mergeStatus === "conflicts") return { kind: "blocked", label: "Merge conflicts" };
  if (mergeFailed(pullRequest)) return { kind: "blocked", label: "Merge failed" };
  if (voted(pullRequest, "rejected")) return { kind: "blocked", label: "Rejected" };
  if (voted(pullRequest, "waitingForAuthor")) return { kind: "waiting", label: "Waiting for author" };
  if (voted(pullRequest, "approved", "approvedWithSuggestions")) return { kind: "approved", label: "Approved" };
  return { kind: "review", label: "In review" };
}

/**
 * What the pull request waits on the user (the token's owner) for, or null.
 * Their own: conflicts to resolve, reviewers' requests, a failed merge, or
 * completing it once approved. Someone else's: their vote, when they are
 * asked to review it by name.
 */
export function attention(pullRequest: PullRequest): string | null {
  if (pullRequest.yours) {
    if (pullRequest.mergeStatus === "conflicts") return "Resolve conflicts";
    if (pullRequest.draft) return null;
    if (voted(pullRequest, "rejected", "waitingForAuthor")) return "Changes requested";
    if (mergeFailed(pullRequest)) return "Merge failed";
    if (voted(pullRequest, "approved", "approvedWithSuggestions")) return "Ready to complete";
    return null;
  }
  const you = pullRequest.reviewers.find((reviewer) => reviewer.you);
  return !pullRequest.draft && you?.vote === "none" ? "Your review" : null;
}

/** Whether `creator` opened `pullRequest`: by the host's id, ignoring case, or by name when either has none. */
export const opened = (creator: Creator, pullRequest: PullRequest) =>
  creator.id && pullRequest.authorId
    ? creator.id.toLowerCase() === pullRequest.authorId.toLowerCase()
    : creator.name === pullRequest.author;

/**
 * Whether `pullRequest` was opened by someone not among `hidden`. The filter
 * keeps who is left out rather than who is picked, so people never seen
 * before are shown when they open their first pull request.
 */
export const byCreators = (pullRequest: PullRequest, hidden: readonly Creator[]): boolean =>
  !hidden.some((creator) => opened(creator, pullRequest));

/** Those waiting on the user first, otherwise in the host's order (newest first). */
export const attentionFirst = (pullRequests: readonly PullRequest[]): PullRequest[] =>
  [...pullRequests].sort((a, b) => Number(attention(b) !== null) - Number(attention(a) !== null));

/** Everyone who opened one of `pullRequests`, with how many each, the user first, then by name. */
export function creatorsOf(pullRequests: readonly PullRequest[]): (Creator & { count: number; you: boolean })[] {
  const found = new Map<string, Creator & { count: number; you: boolean }>();
  for (const pullRequest of pullRequests) {
    const key = (pullRequest.authorId || pullRequest.author).toLowerCase();
    const known = found.get(key);
    if (known) {
      known.count += 1;
      known.you ||= pullRequest.yours;
    } else {
      found.set(key, { id: pullRequest.authorId, name: pullRequest.author, count: 1, you: pullRequest.yours });
    }
  }
  return [...found.values()].sort(
    (a, b) => Number(b.you) - Number(a.you) || a.name.localeCompare(b.name, undefined, { sensitivity: "base" }),
  );
}
