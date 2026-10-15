// Which commits of the branch the diff shows. As in an Azure DevOps pull
// request the picked commits are always consecutive: their changes are one
// diff, from the parent of the oldest to the newest.

import type { Commit, CommitRange } from "../ipc/commands";

/** Positions in `commits` (newest first) of the newest and oldest picked commit. */
interface Span {
  first: number;
  last: number;
}

function spanOf(commits: readonly Commit[], range: CommitRange | null): Span | null {
  if (commits.length === 0) return null;
  if (!range) return { first: 0, last: commits.length - 1 };
  const first = commits.findIndex((commit) => commit.sha === range.newest);
  const last = commits.findIndex((commit) => commit.sha === range.oldest);
  return first === -1 || last === -1 ? { first: 0, last: commits.length - 1 } : { first, last };
}

function rangeOf(commits: readonly Commit[], span: Span): CommitRange | null {
  if (span.first === 0 && span.last === commits.length - 1) return null;
  return { newest: commits[span.first].sha, oldest: commits[span.last].sha };
}

/** Whether the commit at `index` is part of the diff; null (all changes) picks them all. */
export function isPicked(commits: readonly Commit[], range: CommitRange | null, index: number): boolean {
  const span = spanOf(commits, range);
  return span !== null && index >= span.first && index <= span.last;
}

/**
 * The range after ticking or unticking the commit at `index`. Ticking one
 * outside the range stretches it to that commit, picking those in between.
 * Unticking an end shrinks the range; unticking one in the middle keeps the
 * newer part. Unticking the only one left, or picking every commit, gives
 * null: all of the changes.
 */
export function toggleCommit(commits: readonly Commit[], range: CommitRange | null, index: number): CommitRange | null {
  const span = spanOf(commits, range);
  if (!span || index < 0 || index >= commits.length) return range;
  let { first, last } = span;
  if (index < first || index > last) {
    first = Math.min(first, index);
    last = Math.max(last, index);
  } else if (first === last) {
    return null;
  } else if (index === first) {
    first++;
  } else if (index === last) {
    last--;
  } else {
    last = index - 1;
  }
  return rangeOf(commits, { first, last });
}

/** Just the commit at `index`. */
export const onlyCommit = (commits: readonly Commit[], index: number): CommitRange | null =>
  rangeOf(commits, { first: index, last: index });

/** How many commits the range holds. */
export function pickedCount(commits: readonly Commit[], range: CommitRange | null): number {
  const span = spanOf(commits, range);
  return span ? span.last - span.first + 1 : 0;
}
