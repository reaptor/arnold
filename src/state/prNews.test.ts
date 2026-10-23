import { describe, expect, it } from "vitest";
import type { PrComment, PrThread, PullRequest, Reviewer } from "../ipc/commands";
import { addedNews, commentKeys, newsOf, type Seen } from "./prNews";

const reviewer = (name: string, vote: Reviewer["vote"] = "none", you = false): Reviewer => ({
  id: name.toLowerCase(),
  name,
  vote,
  group: false,
  you,
});

const pullRequest = (change: Partial<PullRequest> = {}): PullRequest => ({
  id: 7,
  title: "Add discounts",
  description: "",
  author: "Ada",
  authorId: "ada",
  yours: false,
  created: "2026-10-01T10:00:00Z",
  draft: false,
  status: "active",
  sourceBranch: "feature/discounts",
  targetBranch: "main",
  sourceCommit: "a1",
  targetCommit: "b1",
  url: "https://dev.azure.com/contoso/Shop/_git/shop/pullrequest/7",
  reviewers: [reviewer("Bo"), reviewer("You", "none", true)],
  mergeStatus: "succeeded",
  ...change,
});

const comment = (id: number, author: string, content: string, published: string): PrComment => ({
  id,
  parentId: 0,
  author,
  content,
  published,
  edited: false,
  you: author === "You",
});

const thread = (id: number, comments: PrComment[], path: string | null = "Orders.fs"): PrThread => ({
  id,
  path,
  position: null,
  status: "active",
  comments,
});

const seen = (now: PullRequest, threads: PrThread[] = []): Seen => ({ pullRequest: now, comments: commentKeys(threads) });

describe("newsOf", () => {
  const first = thread(11, [comment(1, "Bo", "Looks good", "2026-10-01T11:00:00Z")]);

  it("has nothing when nothing changed", () => {
    expect(newsOf(seen(pullRequest(), [first]), pullRequest(), [first])).toEqual({ lines: [], thread: null });
  });

  it("quotes a single new comment and points at its discussion", () => {
    const answered = thread(11, [...first.comments, comment(2, "Cy", "Why **not** `net`?", "2026-10-01T12:00:00Z")]);
    expect(newsOf(seen(pullRequest(), [first]), pullRequest(), [answered])).toEqual({
      lines: ["Cy: Why not net?"],
      thread: 11,
    });
  });

  it("counts several new comments, leaving out the user's own", () => {
    const threads = [
      thread(11, [...first.comments, comment(2, "You", "Done", "2026-10-01T12:00:00Z")]),
      thread(12, [comment(1, "Cy", "Typo", "2026-10-01T13:00:00Z")]),
      thread(13, [comment(1, "Bo", "And here", "2026-10-01T12:30:00Z")], null),
    ];
    expect(newsOf(seen(pullRequest(), [first]), pullRequest(), threads)).toEqual({
      lines: ["2 new comments from Bo and Cy"],
      thread: 13,
    });
  });

  it("says that a suggestion is one", () => {
    const suggested = thread(12, [comment(1, "Cy", "```suggestion\n    let net = 1\n```", "2026-10-01T13:00:00Z")]);
    expect(newsOf(seen(pullRequest(), [first]), pullRequest(), [first, suggested]).lines).toEqual(["Cy: (suggested change)"]);
  });

  it("tells nothing of comments when they could not be read before or now", () => {
    const answered = thread(11, [...first.comments, comment(2, "Cy", "Hm", "2026-10-01T12:00:00Z")]);
    expect(newsOf({ pullRequest: pullRequest(), comments: null }, pullRequest(), [answered]).lines).toEqual([]);
    expect(newsOf(seen(pullRequest(), [first]), pullRequest(), null).lines).toEqual([]);
  });

  it("tells of others' votes, pushes and publishing, and of completion", () => {
    const before = pullRequest({ draft: true });
    const after = pullRequest({
      reviewers: [reviewer("Bo", "approved"), reviewer("You", "approved", true), reviewer("Cy", "waitingForAuthor")],
      sourceCommit: "a2",
    });
    expect(newsOf(seen(before), after, []).lines).toEqual([
      "Published: ready for review",
      "Bo approved",
      "Cy is waiting for the author",
      "New changes pushed",
    ]);
    expect(newsOf(seen(after), { ...after, status: "completed" }, null).lines).toEqual(["Completed"]);
    expect(newsOf(seen(after), { ...after, status: "abandoned" }, null).lines).toEqual(["Abandoned"]);
  });

  it("tells the user they were added as a reviewer", () => {
    const before = pullRequest({ reviewers: [reviewer("Bo")] });
    expect(newsOf(seen(before), pullRequest(), []).lines).toEqual(["You were added as a reviewer"]);
  });

  it("leaves out the user's own pushes and tells the author of merge conflicts", () => {
    const before = pullRequest({ yours: true });
    const after = pullRequest({ yours: true, sourceCommit: "a2", mergeStatus: "conflicts" });
    expect(newsOf(seen(before), after, []).lines).toEqual(["Merge conflicts with main"]);
  });
});

describe("addedNews", () => {
  it("tells of a new pull request, a reviewer that they are one, but not its author", () => {
    expect(addedNews(pullRequest()).lines).toEqual(["Ada added you as a reviewer"]);
    expect(addedNews(pullRequest({ reviewers: [reviewer("Bo")] })).lines).toEqual(["New pull request by Ada"]);
    expect(addedNews(pullRequest({ yours: true })).lines).toEqual([]);
  });
});
