import { describe, expect, it } from "vitest";
import type { PullRequest, Reviewer } from "../ipc/commands";
import { attention, attentionFirst, byCreators, creatorsOf, prStatus } from "./prStatus";

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
  reviewers: [reviewer("Bo")],
  mergeStatus: "succeeded",
  ...change,
});

describe("prStatus", () => {
  it("says the most pressing first", () => {
    expect(prStatus(pullRequest({ draft: true, mergeStatus: "conflicts" })).label).toBe("Draft");
    expect(prStatus(pullRequest({ mergeStatus: "conflicts", reviewers: [reviewer("Bo", "approved")] }))).toEqual({
      kind: "blocked",
      label: "Merge conflicts",
    });
    expect(prStatus(pullRequest({ reviewers: [reviewer("Bo", "rejected"), reviewer("Cy", "approved")] })).label).toBe("Rejected");
    expect(prStatus(pullRequest({ reviewers: [reviewer("Bo", "waitingForAuthor"), reviewer("Cy", "approved")] })).kind).toBe("waiting");
    expect(prStatus(pullRequest({ reviewers: [reviewer("Bo", "approvedWithSuggestions")] })).kind).toBe("approved");
    expect(prStatus(pullRequest()).label).toBe("In review");
  });
});

describe("attention", () => {
  it("asks for the user's review when they are a reviewer who has not voted", () => {
    expect(attention(pullRequest({ reviewers: [reviewer("You", "none", true)] }))).toBe("Your review");
    expect(attention(pullRequest({ reviewers: [reviewer("You", "approved", true)] }))).toBeNull();
    expect(attention(pullRequest({ draft: true, reviewers: [reviewer("You", "none", true)] }))).toBeNull();
    expect(attention(pullRequest())).toBeNull();
  });

  it("says what the user's own pull request needs from them", () => {
    const yours = (change: Partial<PullRequest>) => pullRequest({ yours: true, ...change });
    expect(attention(yours({ mergeStatus: "conflicts", draft: true }))).toBe("Resolve conflicts");
    expect(attention(yours({ draft: true, reviewers: [reviewer("Bo", "rejected")] }))).toBeNull();
    expect(attention(yours({ reviewers: [reviewer("Bo", "waitingForAuthor"), reviewer("Cy", "approved")] }))).toBe(
      "Changes requested",
    );
    expect(attention(yours({ mergeStatus: "rejectedByPolicy" }))).toBe("Merge failed");
    expect(attention(yours({ reviewers: [reviewer("Bo", "approved")] }))).toBe("Ready to complete");
    expect(attention(yours({}))).toBeNull();
  });

  it("lists those waiting on the user first, otherwise keeping the order", () => {
    const list = [
      pullRequest({ id: 1 }),
      pullRequest({ id: 2, reviewers: [reviewer("You", "none", true)] }),
      pullRequest({ id: 3 }),
      pullRequest({ id: 4, yours: true, mergeStatus: "conflicts" }),
    ];
    expect(attentionFirst(list).map((each) => each.id)).toEqual([2, 4, 1, 3]);
  });
});

describe("creators", () => {
  const list = [
    pullRequest({ id: 1, author: "Grace", authorId: "GRACE" }),
    pullRequest({ id: 2, author: "Ada", authorId: "ada" }),
    pullRequest({ id: 3, author: "Me", authorId: "me", yours: true }),
    pullRequest({ id: 4, author: "Grace", authorId: "grace" }),
  ];

  it("leaves out the hidden by the host's id, ignoring case, and keeps everyone else", () => {
    expect(list.filter((each) => byCreators(each, [{ id: "grace", name: "Grace H" }])).map((each) => each.id)).toEqual([2, 3]);
    expect(list.filter((each) => byCreators(each, [])).length).toBe(4);
    expect(byCreators(pullRequest({ authorId: "", author: "Bo" }), [{ id: "", name: "Bo" }])).toBe(false);
  });

  it("shows someone never seen before", () => {
    const hidden = [{ id: "ada", name: "Ada" }, { id: "me", name: "Me" }];
    expect(byCreators(pullRequest({ author: "Newcomer", authorId: "newcomer" }), hidden)).toBe(true);
  });

  it("lists who opened them, the user first", () => {
    expect(creatorsOf(list).map((each) => [each.name, each.count, each.you])).toEqual([
      ["Me", 1, true],
      ["Ada", 1, false],
      ["Grace", 2, false],
    ]);
  });
});
