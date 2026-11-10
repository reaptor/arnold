// Consent to run a repository's build code. Getting F# language features
// going restores .NET tools and packages and builds projects, which runs
// MSBuild targets, tools and Paket as the repository says: code from the
// reviewed commit, which in a pull request is its author's. Nothing of it
// runs until the user trusts the repository, and a branch or pull request
// that changes such files asks again, listing them.

import type { ChangedFile } from "../ipc/commands";

const KEY = "arnold.trust";

/** Approvals of single branches kept; the oldest go first. */
const MAX_APPROVALS = 200;

/** Branches of someone else's pull requests kept; the oldest go first. */
const MAX_FOREIGN = 200;

interface Stored {
  /** Trusted repositories, by root path. */
  repos: string[];
  /** Branches whose build files were looked at and run, as `approvalKey` gives them. */
  approved: string[];
  /**
   * Local branches someone else's pull request was checked out on, as
   * `branchKey` gives them: their build code is the pull request author's,
   * so each of their commits asks, also once the pull request is closed.
   */
  foreign: string[];
}

function load(): Stored {
  try {
    const stored = JSON.parse(localStorage.getItem(KEY) ?? "{}");
    const strings = (value: unknown) => (Array.isArray(value) ? value.filter((item) => typeof item === "string") : []);
    return { repos: strings(stored.repos), approved: strings(stored.approved), foreign: strings(stored.foreign) };
  } catch {
    return { repos: [], approved: [], foreign: [] };
  }
}

function save(stored: Stored) {
  localStorage.setItem(KEY, JSON.stringify(stored));
}

const approvalKey = (root: string, branch: string, commit: string) => `${root}\n${branch}\n${commit}`;

const branchKey = (root: string, branch: string) => `${root}\n${branch}`;

/** Whether someone else's pull request was checked out on `branch` of the repository at `root`. */
export const isForeign = (root: string, branch: string) => load().foreign.includes(branchKey(root, branch));

/** Someone else's pull request is checked out on `branch` of the repository at `root`. */
export function markForeign(root: string, branch: string) {
  const stored = load();
  const key = branchKey(root, branch);
  if (stored.foreign.at(-1) === key) return;
  stored.foreign = [...stored.foreign.filter((other) => other !== key), key].slice(-MAX_FOREIGN);
  save(stored);
}

export const isTrusted = (root: string) => load().repos.includes(root);

/** Whether the build files `branch` changes up to `commit` were approved already. */
export const isApproved = (root: string, branch: string, commit: string) =>
  load().approved.includes(approvalKey(root, branch, commit));

/** Trusts the repository at `root`, and with `branch` its changed build files at `commit`. */
export function trust(root: string, branch?: { name: string; commit: string }) {
  const stored = load();
  if (!stored.repos.includes(root)) stored.repos.push(root);
  if (branch) {
    const key = approvalKey(root, branch.name, branch.commit);
    stored.approved = [...stored.approved.filter((other) => other !== key), key].slice(-MAX_APPROVALS);
  }
  save(stored);
}

/**
 * Whether a file can make restoring, building or loading the projects run
 * code: project and solution files, MSBuild's `Directory.*` files, imported
 * `.props`/`.targets`, a project's `.user` file (imported when it exists),
 * MSBuild's `.rsp` response files, the .NET tool manifest, Paket's files, `global.json`,
 * NuGet's settings and Elmish Land's.
 */
export function isBuildFile(path: string) {
  const name = (path.split("/").pop() ?? path).toLowerCase();
  return (
    /\.(fsproj|csproj|vbproj|proj|props|targets|sln|slnx|slnf|rsp)$/.test(name) ||
    /\.(fsproj|csproj|vbproj|proj)\.user$/.test(name) ||
    name.startsWith("directory.") ||
    name.startsWith("paket.") ||
    path.toLowerCase().split("/").includes(".paket") ||
    ["dotnet-tools.json", "global.json", "nuget.config", "elmish-land.json"].includes(name)
  );
}

/** The build files a diff changes (by their new path; renamed ones also by their old). */
export const changedBuildFiles = (files: readonly ChangedFile[]) =>
  files.filter((file) => isBuildFile(file.path) || (file.oldPath !== null && isBuildFile(file.oldPath))).map((file) => file.path);
