import { Channel, invoke as tauriInvoke } from "@tauri-apps/api/core";
import { listen, type UnlistenFn } from "@tauri-apps/api/event";

export interface Branch {
  name: string;
  isRemote: boolean;
  /** Where the branch is checked out, if anywhere. */
  worktreePath: string | null;
}

export interface RepoInfo {
  root: string;
  branches: Branch[];
  currentBranch: string | null;
  defaultBranch: string | null;
  worktrees: string[];
}

/** `unchanged` is used only by the UI, for files opened from outside the diff. */
export type FileStatus = "added" | "modified" | "deleted" | "renamed" | "unchanged";

export interface ChangedFile {
  path: string;
  oldPath: string | null;
  status: FileStatus;
  additions: number | null;
  deletions: number | null;
  binary: boolean;
}

export interface Commit {
  sha: string;
  shortSha: string;
  /** First line of the message. */
  subject: string;
  author: string;
  /** Author date, ISO 8601. */
  date: string;
}

/** A run of consecutive commits of the branch, by sha. */
export interface CommitRange {
  oldest: string;
  newest: string;
}

export interface Comparison {
  base: string;
  head: string;
  /** Where the diff starts: the merge-base, or the parent of the oldest commit of `range`. */
  mergeBase: string;
  /** Where the diff ends; everything shown comes from it: the tip of `head`, or the newest of `range`. */
  headCommit: string;
  /** The commit `head` pointed at. */
  tipCommit: string;
  /** The commits shown, or null for all of the changes. */
  range: CommitRange | null;
  /** The commits of `head` since the merge-base, newest first. */
  commits: Commit[];
  files: ChangedFile[];
}

export interface FilePair {
  base: string | null;
  working: string | null;
  binary: boolean;
  tooLarge: boolean;
}

/** Calls the backend; in development each call and its outcome is echoed to the terminal. */
async function invoke<T>(command: string, args?: Record<string, unknown>): Promise<T> {
  if (!import.meta.env.DEV) return tauriInvoke<T>(command, args);
  const trace = (outcome: string) => void tauriInvoke("dev_trace", { message: `${command}: ${outcome}` });
  try {
    const result = await tauriInvoke<T>(command, args);
    trace("ok");
    return result;
  } catch (error) {
    trace(`FAILED ${typeof error === "object" ? JSON.stringify(error) : error}`);
    throw error;
  }
}

/** Development aid: prints a line in the terminal running `tauri dev`. */
export function devTrace(message: string) {
  if (import.meta.env.DEV) void tauriInvoke("dev_trace", { message });
}

export const startupPath = () => invoke<string | null>("startup_path");

export const openRepo = (path: string) => invoke<RepoInfo>("open_repo", { path });

/** All of the changes of `head`, or with `range` only what those commits of it changed. */
export const compareBranches = (root: string, base: string, head: string, range: CommitRange | null) =>
  invoke<Comparison>("compare_branches", { root, base, head, range });

export const listFiles = (root: string, comparison: Comparison) =>
  invoke<string[]>("list_files", { root, commit: comparison.headCommit });

export const readFilePair = (root: string, comparison: Comparison, file: ChangedFile) =>
  invoke<FilePair>("read_file_pair", {
    root,
    mergeBase: comparison.mergeBase,
    commit: comparison.headCommit,
    path: file.path,
    oldPath: file.oldPath,
  });

// ------------------------------------------------------------ pull requests

/** Where a personal access token comes from: saved from Arnold, or the environment variable. */
export type TokenSource = "keychain" | "environment";

/** A remote of the repository whose pull requests Arnold can list. */
export interface PrRemote {
  /** The git remote's name (`origin`). */
  remote: string;
  provider: "azureDevOps";
  /** `contoso / Shop / shop-api`. */
  name: string;
  organization: string;
  webUrl: string;
  /** Where a personal access token is created. */
  tokenSettingsUrl: string;
  token: TokenSource | null;
}

export type Vote = "approved" | "approvedWithSuggestions" | "none" | "waitingForAuthor" | "rejected";

export interface PullRequest {
  id: number;
  title: string;
  description: string;
  author: string;
  /** ISO 8601. */
  created: string;
  draft: boolean;
  status: string;
  sourceBranch: string;
  targetBranch: string;
  /** What the host last compared; null while it is still working on a new push. */
  sourceCommit: string | null;
  targetCommit: string | null;
  /** The pull request's page on the host. */
  url: string;
  reviewers: { name: string; vote: Vote; group: boolean }[];
}

/** A discussion on a pull request: about lines of a file, or (without a path) about all of it. */
export interface PrThread {
  id: number;
  path: string | null;
  /** As of the pull request's latest push; null when the host lost track of the lines. */
  position: { side: "base" | "head"; startLine: number; endLine: number } | null;
  /** `active`, `pending`, `fixed`, `wontFix`, `closed`, `byDesign` or `unknown`. */
  status: string;
  comments: PrComment[];
}

export interface PrComment {
  id: number;
  /** The comment answered; 0 for none. */
  parentId: number;
  author: string;
  /** Markdown. */
  content: string;
  /** ISO 8601. */
  published: string;
  edited: boolean;
}

/** How the pull request commands fail. */
export type PrError =
  | { kind: "noToken" }
  | { kind: "tokenRejected" | "notFound" | "network" | "other"; message: string };

export const isPrError = (error: unknown): error is PrError =>
  typeof error === "object" && error !== null && "kind" in error;

export const pullRequestRemotes = (root: string) => invoke<PrRemote[]>("pull_request_remotes", { root });

export const listPullRequests = (root: string, remote: string) =>
  invoke<PullRequest[]>("list_pull_requests", { root, remote });

/** Checks the token with the host and, if it works, saves it. Resolves to the active pull requests. */
export const setPullRequestToken = (root: string, remote: string, token: string) =>
  invoke<PullRequest[]>("set_pull_request_token", { root, remote, token });

/** Resolves to where a token still comes from (the environment), if anywhere. */
export const removePullRequestToken = (root: string, remote: string) =>
  invoke<TokenSource | null>("remove_pull_request_token", { root, remote });

/** Reads the pull request and fetches its commits into the repository if they are missing. */
export const openPullRequest = (root: string, remote: string, id: number) =>
  invoke<PullRequest>("open_pull_request", { root, remote, id });

/** The repository's branches, read again. */
export const listBranches = (root: string) => invoke<Branch[]>("list_branches", { root });

/** The discussions of a pull request, positioned as of its latest push. */
export const pullRequestThreads = (root: string, remote: string, id: number) =>
  invoke<PrThread[]>("pull_request_threads", { root, remote, id });

/** A branch or remote-tracking branch moved (commit, fetch, reset). */
export const onRefsChanged = (handler: () => void): Promise<UnlistenFn> => listen("refs-changed", () => handler());

// ------------------------------------------------------------ language servers

/** Progress of `prepareWorkspace`. */
export type SetupEvent =
  | { kind: "step"; title: string }
  | { kind: "skipped"; title: string; reason: string }
  | { kind: "output"; line: string }
  | { kind: "warning"; text: string };

/**
 * Copies `commit` (the tip of `head`) to a folder of the app's own and
 * restores, and where needed builds, what the F# language server needs.
 * Resolves to that folder, or null when the commit has no F# projects.
 * Starting another preparation makes this one fail with "cancelled".
 */
export function prepareWorkspace(root: string, head: string, commit: string, onEvent: (event: SetupEvent) => void) {
  const channel = new Channel<SetupEvent>();
  channel.onmessage = onEvent;
  return invoke<{ path: string | null }>("prepare_workspace", { root, head, commit, onEvent: channel });
}

export type LspEvent =
  | { kind: "status"; text: string | null }
  | { kind: "diagnostics"; uri: string; errors: number; warnings: number }
  | { kind: "projectLoading"; project: string }
  | { kind: "projectLoaded"; project: string }
  | { kind: "projectFailed"; project: string; message: string; details: string | null }
  | { kind: "workspaceLoaded" }
  | { kind: "exited" };

/** Starts a language server; resolves to its id once it is running. */
export function lspStart(
  language: string,
  root: string,
  onMessage: (message: string) => void,
  onEvent: (event: LspEvent) => void,
) {
  const messages = new Channel<string>();
  messages.onmessage = onMessage;
  const events = new Channel<LspEvent>();
  events.onmessage = onEvent;
  return invoke<number>("lsp_start", { language, root, onMessage: messages, onEvent: events });
}

// Not traced: these are far too frequent.
export const lspSend = (id: number, message: string) => tauriInvoke<void>("lsp_send", { id, message });

export const lspOpenBackground = (id: number, uri: string, language: string, text: string) =>
  tauriInvoke<void>("lsp_open_background", { id, uri, language, text });

export const lspStop = (id: number) => invoke<void>("lsp_stop", { id });

