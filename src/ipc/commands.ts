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

export interface Comparison {
  base: string;
  head: string;
  mergeBase: string;
  /** The commit `head` pointed at; everything shown comes from it. */
  headCommit: string;
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
  invoke<string[]>("list_files", { root, commit: comparison.headCommit });

export const readFilePair = (root: string, comparison: Comparison, file: ChangedFile) =>
  invoke<FilePair>("read_file_pair", {
    root,
    mergeBase: comparison.mergeBase,
    commit: comparison.headCommit,
    path: file.path,
    oldPath: file.oldPath,
  });

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

