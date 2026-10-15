// What a new pull request from a reviewed branch starts out with, as Azure
// DevOps fills it in: the commit's subject for one commit, else the
// branch's name with the commits listed in the description.

import type { Commit } from "../ipc/commands";

/** `commits` newest first, as a comparison has them. */
export function defaultTitle(branch: string, commits: readonly Commit[]): string {
  if (commits.length === 1) return commits[0].subject;
  const name = branch.split("/").at(-1) ?? branch;
  const words = name.replace(/[-_]+/g, " ").trim();
  return words ? words[0].toUpperCase() + words.slice(1) : branch;
}

/** The commits' subjects as a Markdown list, oldest first; nothing for one commit. */
export function defaultDescription(commits: readonly Commit[]): string {
  if (commits.length < 2) return "";
  return [...commits]
    .reverse()
    .map((commit) => `- ${commit.subject}`)
    .join("\n");
}

/** Azure DevOps turns down longer descriptions. */
export const DESCRIPTION_LIMIT = 4000;

const DRAFT_KEY = "arnold.createAsDraft";

export function rememberedDraft(): boolean {
  return localStorage.getItem(DRAFT_KEY) === "true";
}

export function rememberDraft(draft: boolean) {
  localStorage.setItem(DRAFT_KEY, String(draft));
}
