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
  /** Differs on disk from the tip of the branch: edits not committed yet (a comparison read from a checkout). */
  uncommitted?: boolean;
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
  /**
   * The checkout whose files the diff ends at, instead of `headCommit`:
   * `head` is checked out there and all of its changes are shown, so they
   * can be edited. Null when the diff is between commits.
   */
  checkout: string | null;
  /** Files with uncommitted changes in `checkout`. */
  uncommitted: number;
}

export interface FilePair {
  base: string | null;
  working: string | null;
  /**
   * When the working side is read from a checkout: its text at the branch's
   * last commit, which uncommitted edits differ from. Null when that commit
   * lacks the file.
   */
  committed?: string | null;
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

/**
 * All of the changes of `head`, or with `range` only what those commits of
 * it changed. `checkout`: where `head` is checked out, when not as a branch
 * (a pull request's worktree); its files are then compared.
 */
export const compareBranches = (
  root: string,
  base: string,
  head: string,
  range: CommitRange | null,
  checkout: string | null = null,
) => invoke<Comparison>("compare_branches", { root, base, head, range, checkout });

/** A worktree of Arnold's own in `.arnold/worktrees`, detached at a commit. */
export interface Worktree {
  path: string;
  /** The commit checked out there. */
  head: string;
  /** It has uncommitted changes. */
  edited: boolean;
}

/**
 * The worktree pull request `id` is edited in, moved to `commit` when it has
 * no edits (or with `discard`, after throwing them away). `create` makes it
 * when there is none; otherwise that resolves to null.
 */
export const pullRequestWorktree = (root: string, id: number, commit: string, create: boolean, discard = false) =>
  invoke<Worktree | null>("pull_request_worktree", { root, id, commit, create, discard });

/** Every file of the compared commit, or of the checkout the comparison reads. */
export const listFiles = (root: string, comparison: Comparison) =>
  invoke<string[]>("list_files", { root, commit: comparison.headCommit, checkout: comparison.checkout });

export const readFilePair = (root: string, comparison: Comparison, file: ChangedFile) =>
  invoke<FilePair>("read_file_pair", {
    root,
    mergeBase: comparison.mergeBase,
    commit: comparison.headCommit,
    path: file.path,
    oldPath: file.oldPath,
    checkout: comparison.checkout,
  });

/** Undoes every uncommitted change to `paths` in `checkout`: back to the last commit; a file no commit has is deleted. */
export const undoFileEdits = (checkout: string, paths: string[]) => invoke<void>("undo_file_edits", { checkout, paths });

/** What `ignorePath` did: the `.gitignore` line, whether it was new, and whether git tracks the path all the same. */
export interface Ignored {
  pattern: string;
  added: boolean;
  tracked: boolean;
}

/** Ignores `path` (a file, or a folder) in `checkout`'s root `.gitignore`. */
export const ignorePath = (checkout: string, path: string, folder: boolean) =>
  invoke<Ignored>("ignore_path", { checkout, path, folder });

/** Whether git tracks `path` (a file, or any file in a folder) in `checkout`. */
export const pathTracked = (checkout: string, path: string) => invoke<boolean>("path_tracked", { checkout, path });

/** Stops tracking `path` (a file, or a folder's files) in `checkout`, keeping it on disk; resolves to how many files. */
export const untrackPath = (checkout: string, path: string) => invoke<number>("untrack_path", { checkout, path });

/** Where a branch is checked out. */
export interface BranchCheckout {
  /** The local branch checked out: the one asked for, or `x` for a remote branch `origin/x`. */
  branch: string;
  path: string;
  /** Whether it was checked out just now, in a new worktree. */
  created: boolean;
}

/**
 * Where `branch` is checked out; checked out in a worktree of its own
 * (`.arnold/worktrees/<branch>`) when it is checked out nowhere. The opened
 * folder is never switched.
 */
export const branchWorktree = (root: string, branch: string) => invoke<BranchCheckout>("branch_worktree", { root, branch });

// ------------------------------------------------------- uncommitted changes

/** The uncommitted changes of a checkout. */
export interface WorkingStatus {
  /** Branch checked out; null when HEAD is detached. */
  branch: string | null;
  /** The commit checked out; null before the first commit. */
  head: string | null;
  /** Working tree against the index; untracked files are `added`. */
  unstaged: ChangedFile[];
  /** Index against HEAD: what a commit takes. */
  staged: ChangedFile[];
  /** Where the branch is pushed and how far it is ahead; null when detached or without a remote. */
  sync: BranchSync | null;
}

/** Where the checked-out branch is pushed, and how it stands against it. */
export interface BranchSync {
  /** Its upstream's remote, else `origin` (or the only remote). */
  remote: string;
  /** The branch's name there. */
  remoteBranch: string;
  /** The remote has the branch (as of the last fetch or push). */
  published: boolean;
  /** Commits a push would send. */
  ahead: number;
  /** Commits of the remote's branch the local one lacks: a push would be refused. */
  behind: number;
}

/** Which diff of an uncommitted file: index → working tree, or HEAD → index. */
export type WorkingSide = "unstaged" | "staged";

export interface NewCommit {
  sha: string;
  subject: string;
  branch: string | null;
}

/** Also starts watching the checkout: `onCheckoutChanged` follows every change to its files and index. */
export const workingStatus = (checkout: string) => invoke<WorkingStatus>("working_status", { checkout });

export const stopWatchingCheckout = () => invoke<void>("stop_watching_checkout");

/** `committed`: the last commit → the working tree, whatever is staged. */
export const readWorkingPair = (checkout: string, file: ChangedFile, side: WorkingSide | "committed") =>
  invoke<FilePair>("read_working_pair", { checkout, path: file.path, oldPath: file.oldPath, side });

export const worktreeFiles = (checkout: string) => invoke<string[]>("worktree_files", { checkout });

export const stageFiles = (checkout: string, paths: string[]) => invoke<void>("stage_files", { checkout, paths });

export const unstageFiles = (checkout: string, paths: string[]) => invoke<void>("unstage_files", { checkout, paths });

/** Tracked files back to the index; untracked ones are deleted. */
export const discardFiles = (checkout: string, paths: string[]) => invoke<void>("discard_files", { checkout, paths });

/** Back to HEAD in the index and the working tree; a file HEAD lacks is only unstaged. */
export const resetFilesToHead = (checkout: string, paths: string[]) =>
  invoke<void>("reset_files_to_head", { checkout, paths });

/** Gives the file this text in the index: some of its lines staged or unstaged. */
export const setIndexText = (checkout: string, path: string, text: string) =>
  invoke<void>("set_index_text", { checkout, path, text });

export const saveWorktreeFile = (checkout: string, path: string, text: string) =>
  invoke<void>("save_worktree_file", { checkout, path, text });

export const commitStaged = (checkout: string, message: string) =>
  invoke<NewCommit>("commit_staged", { checkout, message });

/** Pushes the checked-out branch to its upstream, or to `origin` as a new branch that becomes its upstream. Never forced. */
export const pushCheckoutBranch = (checkout: string) => invoke<void>("push_checkout_branch", { checkout });

/** What Fantomas found: files it would change, and files it could not format. */
export interface FormatCheck {
  unformatted: string[];
  errors: string[];
}

/** Runs `fantomas --check` on the checkout; null when the repository does not use Fantomas. */
export const checkFormatting = (checkout: string) => invoke<FormatCheck | null>("check_formatting", { checkout });

/** What formatting before a commit did. */
export interface Formatted {
  /** Files whose formatting is staged. */
  staged: string[];
  /** Files Fantomas could not format; their staged text is as it was. */
  failed: string[];
}

/**
 * Formats the files with the repository's Fantomas and stages only the
 * formatting: other unstaged changes, also of those files, stay unstaged.
 */
export const formatAndStage = (checkout: string, paths: string[]) =>
  invoke<Formatted>("format_and_stage", { checkout, paths });

/** A file or the index of the watched checkout changed. */
export const onCheckoutChanged = (handler: () => void): Promise<UnlistenFn> =>
  listen("checkout-changed", () => handler());

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
  reviewers: Reviewer[];
  /** How merging went when the host last tried: `succeeded`, `conflicts`, `rejectedByPolicy`, `failure`, `queued` or `notSet`. */
  mergeStatus: string;
}

export interface Reviewer {
  /** The host's id. */
  id: string;
  name: string;
  vote: Vote;
  /** A group (team) rather than a person. */
  group: boolean;
  /** The owner of the token: whoever votes from Arnold. */
  you: boolean;
}

/** How a completed pull request is merged; Azure DevOps' own names. */
export type MergeStrategy = "noFastForward" | "squash" | "rebase" | "rebaseMerge";

/** A change to a pull request as a whole. */
export type PrChange =
  | {
      kind: "complete";
      /** The source commit reviewed: the host refuses if the branch has moved on since. */
      sourceCommit: string;
      mergeStrategy: MergeStrategy;
      message: string;
      deleteSourceBranch: boolean;
      completeWorkItems: boolean;
    }
  | { kind: "abandon" }
  | { kind: "reactivate" }
  | { kind: "draft"; draft: boolean };

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
  /** Written by the token's owner, who may delete it. */
  you: boolean;
}

/** Where a new thread goes: a file, and lines of it unless it is about the whole file. */
export interface NewThreadPlace {
  path: string;
  lines: LineRange | null;
}

/** Lines and 1-based character offsets, the end just past the last character. */
export interface LineRange {
  side: "base" | "head";
  startLine: number;
  startOffset: number;
  endLine: number;
  endOffset: number;
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

/** Votes as the token's owner (`none` resets the vote); resolves to the pull request as it is now. */
export const voteOnPullRequest = (root: string, remote: string, id: number, vote: Vote) =>
  invoke<PullRequest>("vote_on_pull_request", { root, remote, id, vote });

/** Completes, abandons, reactivates, drafts or publishes a pull request; resolves to it as it is now. */
export const updatePullRequest = (root: string, remote: string, id: number, change: PrChange) =>
  invoke<PullRequest>("update_pull_request", { root, remote, id, change });

/** What creating a pull request from a reviewed branch would do; nothing is sent yet. */
export interface NewPullRequestPlan {
  /** The branches on the host, without `refs/heads/`. */
  sourceBranch: string;
  targetBranch: string;
  /** Where the host's source branch is now; null when it has none. */
  serverCommit: string | null;
  /**
   * `none`: the host's branch is at the reviewed commit; `create`: pushed as a
   * new branch; `update`: fast-forwarded; `conflict`: the host's branch has
   * commits that were not reviewed, so nothing can be pushed.
   */
  push: "none" | "create" | "update" | "conflict";
  /** Commits the pull request will have. */
  commits: number;
  /** Commits the push sends. */
  pushCommits: number;
  /** An active pull request between the same branches; the host allows only one. */
  existing: PullRequest | null;
}

export interface NewPullRequest {
  head: string;
  base: string;
  /** The commit reviewed: what is pushed, wherever `head` points by now. */
  commit: string;
  title: string;
  /** Markdown. */
  description: string;
  draft: boolean;
  /** A work item to link the pull request to. */
  workItem: number | null;
}

export interface WorkItem {
  id: number;
  title: string;
  /** `Bug`, `User Story`, `Task`, ... */
  kind: string;
  /** `New`, `Active`, ... */
  state: string;
  /** Its page on the host. */
  url: string;
}

/** `noAccess`: the token may not read work items, which linking one does not need. */
export type WorkItemLookup = { kind: "found"; item: WorkItem } | { kind: "notFound" } | { kind: "noAccess" };

/** Looks a work item of the remote's organization up, to show what a new pull request is linked to. */
export const workItem = (root: string, remote: string, id: number) =>
  invoke<WorkItemLookup>("work_item", { root, remote, id });

export const planPullRequest = (root: string, remote: string, head: string, base: string, commit: string) =>
  invoke<NewPullRequestPlan>("plan_pull_request", { root, remote, head, base, commit });

/** Pushes the reviewed commit if the host lacks it (never forced), then opens the pull request. */
export const createPullRequest = (root: string, remote: string, request: NewPullRequest) =>
  invoke<PullRequest>("create_pull_request", { root, remote, request });

/** The repository's branches, read again. */
export const listBranches = (root: string) => invoke<Branch[]>("list_branches", { root });

/** The discussions of a pull request, positioned as of its latest push. */
export const pullRequestThreads = (root: string, remote: string, id: number) =>
  invoke<PrThread[]>("pull_request_threads", { root, remote, id });

/** Starts a discussion: about `place` in a file, or about the pull request as a whole. */
export const createPullRequestThread = (root: string, remote: string, id: number, place: NewThreadPlace | null, content: string) =>
  invoke<PrThread>("create_pull_request_thread", { root, remote, id, place, content });

/** Answers comment `parent` of thread `thread`. */
export const replyToPullRequestThread = (
  root: string,
  remote: string,
  id: number,
  thread: number,
  parent: number,
  content: string,
) => invoke<null>("reply_to_pull_request_thread", { root, remote, id, thread, parent, content });

/** Deletes comment `comment` of thread `thread`: one the token's owner wrote. */
export const deletePullRequestComment = (root: string, remote: string, id: number, thread: number, comment: number) =>
  invoke<null>("delete_pull_request_comment", { root, remote, id, thread, comment });

/** A branch or remote-tracking branch moved (commit, fetch, reset). */
export const onRefsChanged = (handler: () => void): Promise<UnlistenFn> => listen("refs-changed", () => handler());

/** The repository's config changed: a remote was added, removed or changed, or an upstream set. */
export const onConfigChanged = (handler: () => void): Promise<UnlistenFn> => listen("config-changed", () => handler());

// ------------------------------------------------------------ language servers

/** Progress of `prepareWorkspace`. */
export type SetupEvent =
  | { kind: "step"; title: string }
  | { kind: "skipped"; title: string; reason: string }
  | { kind: "output"; line: string }
  | { kind: "warning"; text: string };

/**
 * Restores, and where needed builds, what the F# language server needs for
 * `commit` (the tip of `head`): with `inPlace` (`head` is checked out in
 * `root`) in `root` itself, else in a copy in `root`'s `.arnold/worktrees`.
 * Resolves to the folder, or null when the commit has no F# projects.
 * Starting another preparation makes this one fail with "cancelled".
 */
export function prepareWorkspace(
  root: string,
  head: string,
  commit: string,
  inPlace: boolean,
  onEvent: (event: SetupEvent) => void,
) {
  const channel = new Channel<SetupEvent>();
  channel.onmessage = onEvent;
  return invoke<{ path: string | null }>("prepare_workspace", { root, head, commit, inPlace, onEvent: channel });
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

