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
export type FileStatus = "added" | "modified" | "deleted" | "renamed" | "untracked" | "unchanged";

export interface ChangedFile {
  path: string;
  oldPath: string | null;
  status: FileStatus;
  additions: number | null;
  deletions: number | null;
  binary: boolean;
}

export interface Comparison {
  base: string;
  head: string;
  mergeBase: string;
  /** Checkout holding `head`; when null the diff is read-only. */
  worktreePath: string | null;
  files: ChangedFile[];
}

export interface FilePair {
  base: string | null;
  working: string | null;
  binary: boolean;
  tooLarge: boolean;
}

export interface WorktreeChanged {
  root: string;
  paths: string[];
  gitStateChanged: boolean;
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
    trace(`FAILED ${error}`);
    throw error;
  }
}

/** Development aid: prints a line in the terminal running `tauri dev`. */
export function devTrace(message: string) {
  if (import.meta.env.DEV) void tauriInvoke("dev_trace", { message });
}

export const startupPath = () => invoke<string | null>("startup_path");

export const openRepo = (path: string) => invoke<RepoInfo>("open_repo", { path });

export const compareBranches = (root: string, base: string, head: string) =>
  invoke<Comparison>("compare_branches", { root, base, head });

export const listFiles = (root: string, comparison: Comparison) =>
  invoke<string[]>("list_files", { root, head: comparison.head, worktree: comparison.worktreePath });

export const readFilePair = (root: string, comparison: Comparison, file: ChangedFile) =>
  invoke<FilePair>("read_file_pair", {
    root,
    mergeBase: comparison.mergeBase,
    head: comparison.head,
    worktree: comparison.worktreePath,
    path: file.path,
    oldPath: file.oldPath,
  });

export const onWorktreeChanged = (handler: (event: WorktreeChanged) => void): Promise<UnlistenFn> =>
  listen<WorktreeChanged>("worktree-changed", (event) => handler(event.payload));

// ------------------------------------------------------------ language servers

export type LspEvent =
  | { kind: "status"; text: string | null }
  | { kind: "diagnostics"; uri: string; errors: number; warnings: number }
  | { kind: "exited" };

/** Starts a language server; resolves to its id once it is running. */
export function lspStart(language: string, root: string, onMessage: (message: string) => void) {
  const channel = new Channel<string>();
  channel.onmessage = onMessage;
  return invoke<number>("lsp_start", { language, root, onMessage: channel });
}

// Not traced: these are far too frequent.
export const lspSend = (id: number, message: string) => tauriInvoke<void>("lsp_send", { id, message });

export const lspOpenBackground = (id: number, uri: string, language: string, text: string) =>
  tauriInvoke<void>("lsp_open_background", { id, uri, language, text });

export const lspStop = (id: number) => invoke<void>("lsp_stop", { id });

export const onLspEvent = (handler: (event: LspEvent) => void): Promise<UnlistenFn> =>
  listen<LspEvent>("lsp-event", (event) => handler(event.payload));
