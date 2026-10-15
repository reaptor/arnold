import { describe, expect, it } from "vitest";
import type { Commit } from "../ipc/commands";
import { defaultDescription, defaultTitle } from "./newPullRequest";

const commit = (subject: string): Commit => ({ sha: subject, shortSha: subject, subject, author: "Agent", date: "" });

describe("a new pull request", () => {
  it("is titled after its only commit", () => {
    expect(defaultTitle("agent/fix-totals", [commit("Round totals")])).toBe("Round totals");
    expect(defaultDescription([commit("Round totals")])).toBe("");
  });

  it("is titled after the branch and lists several commits, oldest first", () => {
    const commits = [commit("Third"), commit("Second"), commit("First")];
    expect(defaultTitle("claude/add-loyalty_discount", commits)).toBe("Add loyalty discount");
    expect(defaultTitle("feature", commits)).toBe("Feature");
    expect(defaultTitle("x/--", commits)).toBe("x/--");
    expect(defaultDescription(commits)).toBe("- First\n- Second\n- Third");
  });
});
