import { describe, expect, it } from "vitest";
import type { Commit } from "../ipc/commands";
import { isPicked, onlyCommit, pickedCount, toggleCommit } from "./commitRange";

// Newest first, as the backend lists them.
const commits: Commit[] = ["e", "d", "c", "b", "a"].map((sha) => ({ sha, shortSha: sha, subject: sha, author: "x", date: "" }));
const range = (newest: string, oldest: string) => ({ newest, oldest });
const picked = (r: ReturnType<typeof toggleCommit>) =>
  commits.filter((_, index) => isPicked(commits, r, index)).map((commit) => commit.sha).join("");

describe("commit ranges", () => {
  it("picks everything for all changes", () => {
    expect(picked(null)).toBe("edcba");
    expect(pickedCount(commits, null)).toBe(5);
  });

  it("stretches over the commits in between when ticking one outside", () => {
    expect(picked(toggleCommit(commits, onlyCommit(commits, 1), 3))).toBe("dcb");
    expect(picked(toggleCommit(commits, range("c", "b"), 0))).toBe("edcb");
  });

  it("shrinks from an end and keeps the newer part when unticking the middle", () => {
    expect(picked(toggleCommit(commits, null, 0))).toBe("dcba");
    expect(picked(toggleCommit(commits, null, 4))).toBe("edcb");
    expect(toggleCommit(commits, null, 2)).toEqual(range("e", "d"));
  });

  it("falls back to all changes when nothing or everything is picked", () => {
    expect(toggleCommit(commits, onlyCommit(commits, 2), 2)).toBeNull();
    expect(toggleCommit(commits, range("e", "b"), 4)).toBeNull();
    expect(onlyCommit([commits[0]], 0)).toBeNull();
  });

  it("treats a range that is no longer on the branch as all changes", () => {
    expect(picked(range("x", "y"))).toBe("edcba");
  });
});
