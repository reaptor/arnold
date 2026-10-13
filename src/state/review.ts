// Application state and the actions that change it. Components read `state`
// and call these functions; the CodeMirror editor is driven from here.

import type { EditorView } from "@codemirror/view";
import { ask, open as openDialog } from "@tauri-apps/plugin-dialog";
import { createStore, reconcile } from "solid-js/store";
import { DiffEditor, type DiffDocument, type DiffMode, type Snapshot } from "../editor/diffEditor";
import { languageIdFor } from "../editor/languages";
import * as ipc from "../ipc/commands";
import type { ChangedFile, Comparison, FilePair, RepoInfo } from "../ipc/commands";
import * as lsp from "../lsp/clients";
import { joinPath, pathToUri, relativeTo, uriToPath } from "../lsp/uri";

interface State {
  repo: RepoInfo | null;
  base: string;
  head: string;
  comparison: Comparison | null;
  /** Files opened from outside the diff (by go-to-definition). */
  visited: ChangedFile[];
  /** Path of the file shown in the editor. */
  selected: string | null;
  mode: DiffMode;
  /** Files with edits that are not saved yet. */
  dirty: Record<string, boolean>;
  /** Errors and warnings the language server reported, by file path. */
  problems: Record<string, { errors: number; warnings: number }>;
  /** What the language server is doing, or why it is not running. */
  lspStatus: string | null;
  /** Number of files that "back" can return to. */
  backCount: number;
  busy: boolean;
  error: string | null;
  /** Shown in place of the editor (nothing selected, binary file, ...). */
  placeholder: string | null;
  /** The open file has unsaved edits and also changed on disk. */
  diskConflict: boolean;
  menu: { x: number; y: number } | null;
}

export const [state, setState] = createStore<State>({
  repo: null,
  base: "",
  head: "",
  comparison: null,
  visited: [],
  selected: null,
  mode: (localStorage.getItem("arnold.mode") as DiffMode | null) ?? "split",
  dirty: {},
  problems: {},
  lspStatus: null,
  backCount: 0,
  busy: false,
  error: null,
  placeholder: "Open a repository to start reviewing.",
  diskConflict: false,
  menu: null,
});

/** What is known about a file that has been opened in this comparison. */
interface Session {
  pair: FilePair;
  /** Working text as last read from or written to disk. */
  savedText: string;
  snapshot: Snapshot | null;
}

const sessions = new Map<string, Session>();
/** Text last handed to the language server for files no editor shows, by path. */
const backgroundSent = new Map<string, string>();
/** More files than this are not checked in the background. */
const MAX_BACKGROUND_FILES = 50;
/** Files left by go-to-definition, most recent last. */
const backStack: string[] = [];
let editor: DiffEditor | null = null;
let loadToken = 0;

const fail = (error: unknown) => setState("error", String(error));

const fileAt = (path: string | null) =>
  state.comparison?.files.find((file) => file.path === path) ??
  state.visited.find((file) => file.path === path);

const isEditable = (file: ChangedFile, pair: FilePair) =>
  state.comparison?.worktreePath != null && pair.working !== null && file.status !== "deleted";

const documentOf = (file: ChangedFile, session: Session): DiffDocument => ({
  path: file.path,
  base: session.pair.base ?? "",
  working: session.pair.working ?? "",
  editable: isEditable(file, session.pair),
  plain: file.status === "unchanged",
});

export function attachEditor(host: HTMLElement) {
  editor = new DiffEditor(host, {
    onChange(text) {
      const path = state.selected;
      const session = path ? sessions.get(path) : undefined;
      if (path && session) setState("dirty", path, text !== session.savedText);
    },
    onSave: () => void save(),
    onContextMenu: (x, y) => setState("menu", { x, y }),
  });
  lsp.onDisplayFile(displayUri);
  lsp.onStartFailed((reason) => setState("lspStatus", String(reason)));
}

// ---------------------------------------------------------------- repository

export async function chooseRepo() {
  const picked = await openDialog({ directory: true, title: "Open a git repository" });
  if (typeof picked === "string") await openRepoAt(picked);
}

export async function openRepoAt(path: string) {
  setState({ busy: true, error: null });
  try {
    const repo = await ipc.openRepo(path);
    localStorage.setItem("arnold.lastRepo", repo.root);
    const remembered = JSON.parse(localStorage.getItem(`arnold.branches.${repo.root}`) ?? "{}");
    const has = (name: unknown) => repo.branches.some((branch) => branch.name === name);
    const base = has(remembered.base) ? remembered.base : (repo.defaultBranch ?? repo.branches[0]?.name ?? "");
    const head = has(remembered.head) ? remembered.head : defaultHead(repo, base);
    resetSessions();
    setState({ repo, base, head, comparison: null, selected: null });
    await refresh();
  } catch (error) {
    fail(error);
  } finally {
    setState("busy", false);
  }
}

function defaultHead(repo: RepoInfo, base: string): string {
  if (repo.currentBranch && repo.currentBranch !== base) return repo.currentBranch;
  const candidates = repo.branches.filter((branch) => !branch.isRemote && branch.name !== base);
  return (candidates.find((branch) => branch.worktreePath) ?? candidates[0])?.name ?? base;
}

export async function setBranches(change: { base?: string; head?: string }) {
  if (!state.repo) return;
  if (!(await confirmDiscard("Switch branches"))) return;
  setState(change);
  localStorage.setItem(
    `arnold.branches.${state.repo.root}`,
    JSON.stringify({ base: state.base, head: state.head }),
  );
  resetSessions();
  setState({ selected: null });
  await refresh();
}

function resetSessions() {
  sessions.clear();
  backgroundSent.clear();
  backStack.length = 0;
  editor?.close();
  void lsp.stop();
  setState("dirty", reconcile({}));
  setState("problems", reconcile({}));
  setState({ visited: [], backCount: 0, diskConflict: false, lspStatus: null });
}

async function confirmDiscard(action: string): Promise<boolean> {
  const unsaved = Object.values(state.dirty).filter(Boolean).length;
  if (unsaved === 0) return true;
  return ask(`${unsaved} file(s) have unsaved edits that will be lost.`, {
    title: action,
    kind: "warning",
    okLabel: "Discard edits",
  });
}

// ---------------------------------------------------------------- comparison

/** Re-reads the changed-file list and brings the open file up to date. */
export async function refresh() {
  const repo = state.repo;
  if (!repo || !state.base || !state.head) return;
  try {
    const comparison = await ipc.compareBranches(repo.root, state.base, state.head);
    if (repo !== state.repo) return;
    setState("comparison", reconcile(comparison, { key: "path" }));
    // A visited file that has since been edited is now part of the diff.
    setState("visited", (visited) =>
      visited.filter((file) => !comparison.files.some((changed) => changed.path === file.path)),
    );
    setState("error", null);

    const selected = fileAt(state.selected);
    if (selected) {
      await revalidateOpenFile(selected);
    } else {
      const unsavedSelection = state.selected !== null && state.dirty[state.selected];
      if (!unsavedSelection) await selectFile(comparison.files[0]?.path ?? null);
    }
    void checkChangedFiles();
  } catch (error) {
    fail(error);
  }
}

export async function selectFile(path: string | null) {
  stashOpenFile();
  setState({ selected: path, diskConflict: false, menu: null });
  const file = fileAt(path);
  if (!file) {
    editor?.close();
    setState("placeholder", state.comparison ? "No differences." : null);
    return;
  }
  await showFile(file);
}

function stashOpenFile() {
  const session = state.selected ? sessions.get(state.selected) : undefined;
  if (session) session.snapshot = editor?.snapshot() ?? null;
}

async function showFile(file: ChangedFile) {
  const repo = state.repo;
  const comparison = state.comparison;
  if (!repo || !comparison) return;
  const token = ++loadToken;
  try {
    const pair = await ipc.readFilePair(repo.root, comparison, file);
    if (token !== loadToken) return;
    if (pair.binary || pair.tooLarge) {
      editor?.close();
      setState("placeholder", pair.binary ? "Binary file." : "This file is too large to show.");
      return;
    }

    const diskText = pair.working ?? "";
    let session = sessions.get(file.path);
    if (!session) {
      session = { pair, savedText: diskText, snapshot: null };
      sessions.set(file.path, session);
    } else if (state.dirty[file.path]) {
      // Keep the unsaved text; only the base side is refreshed.
      setState("diskConflict", diskText !== session.savedText);
      session.pair = { ...session.pair, base: pair.base };
    } else {
      if (diskText !== session.savedText) session.snapshot = null;
      session.pair = pair;
      session.savedText = diskText;
    }
    setState("placeholder", null);
    const document = documentOf(file, session);
    editor?.open(document, state.mode, session.snapshot ?? undefined);
    void attachLanguageServer(file, document);
  } catch (error) {
    fail(error);
  }
}

/** After a refresh: pick up changes made on disk to the file being shown. */
async function revalidateOpenFile(file: ChangedFile) {
  const repo = state.repo;
  const comparison = state.comparison;
  const session = sessions.get(file.path);
  if (!repo || !comparison) return;
  if (!session) return showFile(file);

  const token = ++loadToken;
  const pair = await ipc.readFilePair(repo.root, comparison, file);
  if (token !== loadToken || pair.binary || pair.tooLarge) return;
  const diskText = pair.working ?? "";
  const baseChanged = (pair.base ?? "") !== (session.pair.base ?? "");
  const diskChanged = diskText !== session.savedText;
  if (!baseChanged && !diskChanged) return;

  if (state.dirty[file.path]) {
    if (diskChanged) setState("diskConflict", true);
    if (baseChanged) {
      session.pair = { ...session.pair, base: pair.base };
      const document = documentOf(file, session);
      editor?.open(document, state.mode, editor.snapshot() ?? undefined);
      void attachLanguageServer(file, document);
    }
    return;
  }
  session.pair = pair;
  session.savedText = diskText;
  session.snapshot = null;
  editor?.reload(documentOf(file, session));
}

/** Resolves a disk conflict on the open file. */
export async function resolveDiskConflict(choice: "reload" | "keep") {
  const file = fileAt(state.selected);
  const session = file && sessions.get(file.path);
  if (!file || !session || !state.repo || !state.comparison) return;
  const pair = await ipc.readFilePair(state.repo.root, state.comparison, file);
  session.savedText = pair.working ?? "";
  if (choice === "reload") {
    session.pair = pair;
    session.snapshot = null;
    setState("dirty", file.path, false);
    editor?.reload(documentOf(file, session));
  }
  setState("diskConflict", false);
}

// ------------------------------------------------------------------- editing

export function setMode(mode: DiffMode) {
  localStorage.setItem("arnold.mode", mode);
  setState("mode", mode);
  editor?.setMode(mode);
  editor?.focus();
}

export async function save() {
  const path = state.selected;
  const session = path ? sessions.get(path) : undefined;
  const worktree = state.comparison?.worktreePath;
  const text = editor?.text();
  if (!path || !session || !worktree || text == null || !state.dirty[path]) return;
  try {
    await ipc.writeFile(worktree, path, text, session.pair.eol, session.pair.bom);
    session.savedText = text;
    session.pair = { ...session.pair, working: text };
    setState("dirty", path, false);
    setState("diskConflict", false);
    // Update the file list now rather than waiting for the file watcher.
    void refresh();
  } catch (error) {
    fail(error);
  }
}

export function resetSelectedLines() {
  setState("menu", null);
  editor?.resetSelectedLines();
  editor?.focus();
}

export const nextChange = () => editor?.nextChange();
export const previousChange = () => editor?.previousChange();

/** Puts a whole file back to the base version, on disk. */
export async function resetFileAt(path: string | null) {
  setState("menu", null);
  const file = fileAt(path);
  const repo = state.repo;
  const comparison = state.comparison;
  if (!file || file.status === "unchanged" || !repo || !comparison?.worktreePath) return;

  const removes = file.status === "added" || file.status === "untracked";
  const effect = removes
    ? "The file does not exist in the base version, so it will be deleted."
    : "All changes to it, including unsaved edits, will be lost.";
  const confirmed = await ask(`Reset ${file.path} to ${comparison.base}?\n\n${effect}`, {
    title: "Reset file",
    kind: "warning",
    okLabel: removes ? "Delete file" : "Reset file",
  });
  if (!confirmed) return;

  try {
    await ipc.resetFile(repo.root, comparison, file);
    sessions.delete(file.path);
    setState("dirty", file.path, false);
    if (state.selected === file.path) {
      editor?.close();
      setState({ diskConflict: false });
    }
    await refresh();
  } catch (error) {
    fail(error);
  }
}

// ----------------------------------------------------------- language server

/** Connects the file being shown to its language server, once that is running. */
async function attachLanguageServer(file: ChangedFile, document: DiffDocument) {
  const worktree = state.comparison?.worktreePath;
  const language = languageIdFor(file.path);
  if (!worktree || !language || !document.editable) return;
  const client = await lsp.clientFor(worktree, language);
  // The user may have moved on while the server was starting.
  if (!client || state.selected !== file.path) return;
  editor?.setLanguageSupport(client.plugin(pathToUri(joinPath(worktree, file.path)), language));
  ipc.devTrace(`language server attached to ${file.path}`);
  // The file shown before this one is closed now; it goes back to the background.
  void checkChangedFiles();
}

let checking = false;
let checkAgain = false;

/**
 * Hands every changed F# file that is not on screen to the language server,
 * so the files dock shows errors for the whole change. Files are only sent
 * again when their text has changed.
 */
async function checkChangedFiles() {
  if (checking) {
    checkAgain = true;
    return;
  }
  checking = true;
  try {
    do {
      checkAgain = false;
      const repo = state.repo;
      const comparison = state.comparison;
      const worktree = comparison?.worktreePath;
      if (!repo || !comparison || !worktree) return;
      const files = comparison.files
        .filter((file) => languageIdFor(file.path) === "fsharp" && file.status !== "deleted" && !file.binary)
        .slice(0, MAX_BACKGROUND_FILES);
      if (files.length === 0 || !(await lsp.clientFor(worktree, "fsharp"))) return;

      for (const file of files) {
        if (comparison !== state.comparison) break;
        if (file.path === state.selected) {
          // The editor owns this file; send it afresh once it is left.
          backgroundSent.delete(file.path);
          continue;
        }
        const session = sessions.get(file.path);
        const unsaved = state.dirty[file.path] ? session?.snapshot?.doc.toString() : undefined;
        const text = unsaved ?? (await ipc.readFilePair(repo.root, comparison, file)).working;
        if (text == null || backgroundSent.get(file.path) === text) continue;
        backgroundSent.set(file.path, text);
        await lsp.openInBackground(pathToUri(joinPath(worktree, file.path)), "fsharp", text);
      }
    } while (checkAgain);
  } finally {
    checking = false;
  }
}

/**
 * Shows the file a language server points at (go-to-definition) and returns
 * its editor. Files outside the diff open on their own, without a diff.
 */
async function displayUri(uri: string): Promise<EditorView | null> {
  const worktree = state.comparison?.worktreePath;
  const absolute = uriToPath(uri);
  const path = worktree && absolute ? relativeTo(worktree, absolute) : null;
  if (path === null) {
    setState("lspStatus", `That definition is outside this repository: ${absolute ?? uri}`);
    return null;
  }
  if (path !== state.selected) {
    if (!fileAt(path)) {
      const unchanged: ChangedFile = {
        path,
        oldPath: null,
        status: "unchanged",
        additions: null,
        deletions: null,
        binary: false,
      };
      setState("visited", (visited) => [...visited, unchanged]);
    }
    if (state.selected) backStack.push(state.selected);
    setState("backCount", backStack.length);
    await selectFile(path);
  }
  return editor?.working ?? null;
}

/** Returns to the file go-to-definition was started from. */
export async function goBack() {
  const previous = backStack.pop();
  setState("backCount", backStack.length);
  if (previous) await selectFile(previous);
  editor?.focus();
}

// ------------------------------------------------------------------ watching

let refreshTimer: number | undefined;

export async function startWatching() {
  // The backend watches one checkout at a time: the one being compared.
  await ipc.onWorktreeChanged(() => {
    window.clearTimeout(refreshTimer);
    refreshTimer = window.setTimeout(() => void refresh(), 150);
  });

  await ipc.onLspEvent((event) => {
    if (event.kind === "status") {
      setState("lspStatus", event.text);
      if (event.text) ipc.devTrace(`lsp status: ${event.text}`);
    } else if (event.kind === "exited") {
      setState("lspStatus", "The language server stopped.");
    } else {
      const worktree = state.comparison?.worktreePath;
      const absolute = uriToPath(event.uri);
      const path = worktree && absolute ? relativeTo(worktree, absolute) : null;
      if (path !== null) setState("problems", path, { errors: event.errors, warnings: event.warnings });
      ipc.devTrace(`diagnostics ${path ?? event.uri}: ${event.errors} errors, ${event.warnings} warnings`);
    }
  });
}
