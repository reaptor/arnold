import { describe, expect, it } from "vitest";
import type { Branch, BranchSync } from "../ipc/commands";
import { checkoutChoices, sortBranches } from "./branchInfo";

const branch = (name: string, updatedAt: number, sync: Partial<BranchSync> | null = null): Branch => ({
  name,
  isRemote: name.startsWith("origin/"),
  worktreePath: null,
  sync: sync && { remote: "origin", remoteBranch: name, published: true, ahead: 0, behind: 0, ...sync },
  updatedAt,
  lastAuthor: "Ada",
});

describe("branch pickers", () => {
  it("sort by last commit, newest first, or by name", () => {
    const list = [branch("main", 10), branch("feature/b", 30), branch("Feature/a", 30), branch("fix-10", 5), branch("fix-9", 5)];
    expect(sortBranches(list, "recent").map((b) => b.name)).toEqual(["Feature/a", "feature/b", "main", "fix-9", "fix-10"]);
    expect(sortBranches(list, "name").map((b) => b.name)).toEqual(["Feature/a", "feature/b", "fix-9", "fix-10", "main"]);
  });

  it("offer every local branch and the remote branches no local one stands for", () => {
    const list = [
      branch("main", 1, {}),
      branch("agent/x", 1, { remoteBranch: "agent/renamed" }),
      branch("loose", 1),
      branch("origin/main", 1),
      branch("origin/agent/renamed", 1),
      branch("origin/loose", 1),
      branch("origin/agent/new", 1),
    ];
    expect(checkoutChoices(list).map((b) => b.name)).toEqual(["main", "agent/x", "loose", "origin/agent/new"]);
  });
});
