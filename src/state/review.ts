// Application state and the actions that change it. Components read `state`
// and call these functions; the CodeMirror editor is driven from here.

import type { EditorView } from "@codemirror/view";
import { ask, open as openDialog } from "@tauri-apps/plugin-dialog";
import { createSignal } from "solid-js";
import { createStore, reconcile } from "solid-js/store";
import {
  DiffEditor,
  type DiffDocument,
  type DiffEditorCallbacks,
  type DiffMode,
  type Snapshot,
} from "../editor/diffEditor";
import { languageIdFor } from "../editor/languages";
import * as ipc from "../ipc/commands";
import type { ChangedFile, Comparison, FilePair, FileStatus, RepoInfo } from "../ipc/commands";
import * as lsp from "../lsp/clients";
import { joinPath, pathToUri, relativeTo, uriToPath } from "../lsp/uri";
import { comparePaths } from "./pathOrder";

/** One file at a time, or every file of the diff in one scrolling list. */
export type ViewMode = "file" | "all";

interface State {
  repo: RepoInfo | null;
  base: string;
  head: string;
  comparison: Comparison | null;
  /** Files opened from outside the diff (by go-to-definition). */
  visited: ChangedFile[];
  /**
   * Path of the file shown in the editor. In the all-files view: the file
   * highlighted in the dock (clicked, focused or scrolled to).
   */
  selected: string | null;
  view: ViewMode;
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
  /** Files with unsaved edits that also changed on disk. */
  diskConflict: Record<string, boolean>;
  /** Files folded away in the all-files view. */
  collapsed: Record<string, boolean>;
  /**
   * Context menu of the editor showing `path`; `definition` when a language
   * server can say where the right-clicked symbol is defined.
   */
  menu: { x: number; y: number; path: string | null; definition: boolean } | null;
}

export const [state, setState] = createStore<State>({
  repo: null,
  base: "",
  head: "",
  comparison: null,
  visited: [],
  selected: null,
  view: localStorage.getItem("arnold.view") === "all" ? "all" : "file",
  mode: (localStorage.getItem("arnold.mode") as DiffMode | null) ?? "split",
  dirty: {},
  problems: {},
  lspStatus: null,
  backCount: 0,
  busy: false,
  error: null,
  placeholder: "Open a repository to start reviewing.",
  diskConflict: {},
  collapsed: {},
  menu: null,
});

/** What is known about a file that has been opened in this comparison. */
interface Session {
  pair: FilePair;
  /** Working text as last read from or written to disk. */
  savedText: string;
  snapshot: Snapshot | null;
}

const sameItems = (a: readonly string[], b: readonly string[]) =>
  a.length === b.length && a.every((item, index) => item === b[index]);

/**
 * Every file of the branch under review, for the file explorer: its checkout
 * when it has one, otherwise its tip commit. Kept out of the store, which
 * would wrap each of possibly many thousands of paths.
 */
const [allFiles, setAllFiles] = createSignal<readonly string[]>([], { equals: sameItems });
export { allFiles };

const sessions = new Map<string, Session>();
/** Text last handed to the language server for files no editor shows, by path. */
const backgroundSent = new Map<string, string>();
/** More files than this are not checked in the background. */
const MAX_BACKGROUND_FILES = 50;
/** Files left by go-to-definition, most recent last. */
const backStack: string[] = [];
/** The editor of the one-file view. */
let editor: DiffEditor | null = null;
/** All-files view: where each file goes, and the editors of the files loaded so far. */
const sectionHosts = new Map<string, HTMLElement>();
const sectionEditors = new Map<string, DiffEditor>();
/** All-files view: the file whose editor last had keyboard focus. */
let focusedPath: string | null = null;
/** Where the context menu was opened; kept out of the store, which would wrap the view. */
let menuTarget: { view: EditorView; position: number } | null = null;
/** Latest load per editor, so that a slower earlier load cannot win. */
const loadTokens = new Map<string, number>();
/** `loadTokens` key of the one-file editor; section editors use their path. */
const FILE_VIEW = "";

function nextToken(key: string) {
  const token = (loadTokens.get(key) ?? 0) + 1;
  loadTokens.set(key, token);
  return token;
}

const isLatest = (key: string, token: number) => loadTokens.get(key) === token;

const fail = (error: unknown) => setState("error", String(error));

const fileAt = (path: string | null) =>
  state.comparison?.files.find((file) => file.path === path) ??
  state.visited.find((file) => file.path === path);

/** Whether `path` is one of the diff's files (not a file opened from outside it). */
export const isChanged = (path: string | null) =>
  state.comparison?.files.some((file) => file.path === path) ?? false;

const isEditable = (file: ChangedFile, pair: FilePair) =>
  state.comparison?.worktreePath != null && pair.working !== null && file.status !== "deleted";

/**
 * Created and deleted files are shown whole (a deleted one read-only) instead
 * of as a diff with one side empty. Unchanged files have no diff at all.
 */
const WHOLE_FILE: FileStatus[] = ["added", "untracked", "deleted", "unchanged"];

function documentOf(file: ChangedFile, session: Session): DiffDocument {
  const base = session.pair.base ?? "";
  const working = session.pair.working ?? "";
  return {
    path: file.path,
    base,
    working: file.status === "deleted" ? base : working,
    editable: isEditable(file, session.pair),
    plain: WHOLE_FILE.includes(file.status),
  };
}

/** The editor showing `path`, if one does. */
function editorFor(path: string | null): DiffEditor | null {
  if (path === null) return null;
  if (state.view === "all") return sectionEditors.get(path) ?? null;
  return state.selected === path ? editor : null;
}

/** The file keyboard commands act on: the selected one, or the one being typed in. */
function activePath(): string | null {
  if (state.view === "all" && focusedPath !== null && sectionEditors.has(focusedPath)) return focusedPath;
  return state.selected;
}

const activeEditor = () => editorFor(activePath());

function callbacksFor(pathOf: () => string | null): DiffEditorCallbacks {
  return {
    onChange(text) {
      const path = pathOf();
      const session = path ? sessions.get(path) : undefined;
      if (path && session) setState("dirty", path, text !== session.savedText);
    },
    onSave: () => void save(pathOf()),
    onFocus() {
      const path = pathOf();
      if (state.view !== "all" || path === null) return;
      focusedPath = path;
      if (state.selected !== path) setState("selected", path);
    },
    onContextMenu(x, y, view, position) {
      menuTarget = position === null ? null : { view, position };
      setState("menu", { x, y, path: pathOf(), definition: menuTarget !== null && lsp.hasLanguageServer(view) });
    },
  };
}

export function attachEditor(host: HTMLElement) {
  editor = new DiffEditor(host, callbacksFor(() => state.selected));
  lsp.onDisplayFile(displayUri);
  lsp.onStartFailed((reason) => setState("lspStatus", String(reason)));
}

// ---------------------------------------------------------------- repository

export async function chooseRepo() {
  const picked = await openDialog({ directory: true, title: "Open a git repository" });
  if (typeof picked === "string") await openRepoAt(picked);
}

export async function openRepoAt(path: string) {
  if (state.repo && !(await confirmDiscard("Open repository"))) return;
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
  setState("diskConflict", reconcile({}));
  setState("collapsed", reconcile({}));
  // Dropping the list also removes the file sections of the all-files view.
  setState({ comparison: null, visited: [], backCount: 0, lspStatus: null });
  setAllFiles([]);
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
    // Listed, stepped through and shown in the order of the file explorer.
    comparison.files.sort((a, b) => comparePaths(a.path, b.path));
    setState("comparison", reconcile(comparison, { key: "path" }));
    void loadAllFiles(repo, comparison);
    // A visited file that has since been edited is now part of the diff.
    setState("visited", (visited) =>
      visited.filter((file) => !comparison.files.some((changed) => changed.path === file.path)),
    );
    setState("error", null);

    const selected = fileAt(state.selected);
    if (state.view === "all") {
      if (!selected) setState("selected", comparison.files[0]?.path ?? null);
      await Promise.all(
        [...sectionEditors].map(([path, target]) => {
          const file = fileAt(path);
          return file && revalidate(file, target, path);
        }),
      );
    } else if (selected && editor) {
      await revalidate(selected, editor, FILE_VIEW);
    } else {
      const unsavedSelection = state.selected !== null && state.dirty[state.selected];
      if (!unsavedSelection) await selectFile(comparison.files[0]?.path ?? null);
    }
    void checkChangedFiles();
  } catch (error) {
    fail(error);
  }
}

async function loadAllFiles(repo: RepoInfo, comparison: Comparison) {
  try {
    const files = await ipc.listFiles(repo.root, comparison);
    if (repo === state.repo && comparison.head === state.comparison?.head) setAllFiles(files);
  } catch (error) {
    fail(error);
  }
}

const unchangedFile = (path: string): ChangedFile => ({
  path,
  oldPath: null,
  status: "unchanged",
  additions: null,
  deletions: null,
  binary: false,
});

/** Opens any file of the checkout (file explorer): a changed one with its diff, others on their own. */
/** A file picked by hand in the dock: shown on its own, leaving the all-files view. */
export async function openFile(path: string) {
  if (!fileAt(path)) setState("visited", (visited) => [...visited, unchangedFile(path)]);
  if (state.view === "file") {
    if (path !== state.selected) await selectFile(path);
    return;
  }
  focusedPath = null;
  setState({ selected: path, menu: null });
  await setView("file");
}

export async function selectFile(path: string | null) {
  if (state.view === "all") {
    setState({ selected: path, menu: null });
    if (path !== null) revealSection(path);
    return;
  }
  stashOpenFile();
  setState({ selected: path, menu: null });
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
  const snapshot = editor?.snapshot();
  if (session && snapshot) session.snapshot = snapshot;
}

/** The session of a file just read from disk, brought up to date with it. */
function sessionFor(file: ChangedFile, pair: FilePair): Session {
  const diskText = pair.working ?? "";
  let session = sessions.get(file.path);
  if (!session) {
    session = { pair, savedText: diskText, snapshot: null };
    sessions.set(file.path, session);
  } else if (state.dirty[file.path]) {
    // Keep the unsaved text; only the base side is refreshed.
    setState("diskConflict", file.path, diskText !== session.savedText);
    session.pair = { ...session.pair, base: pair.base };
  } else {
    if (diskText !== session.savedText) session.snapshot = null;
    session.pair = pair;
    session.savedText = diskText;
  }
  return session;
}

const unshowable = (pair: FilePair) =>
  pair.binary ? "Binary file." : pair.tooLarge ? "This file is too large to show." : null;

async function showFile(file: ChangedFile) {
  const repo = state.repo;
  const comparison = state.comparison;
  if (!repo || !comparison) return;
  const token = nextToken(FILE_VIEW);
  try {
    const pair = await ipc.readFilePair(repo.root, comparison, file);
    if (!isLatest(FILE_VIEW, token)) return;
    const reason = unshowable(pair);
    if (reason) {
      editor?.close();
      setState("placeholder", reason);
      return;
    }
    const session = sessionFor(file, pair);
    setState("placeholder", null);
    const document = documentOf(file, session);
    editor?.open(document, state.mode, session.snapshot ?? undefined);
    if (editor) void attachLanguageServer(file, document, editor);
  } catch (error) {
    fail(error);
  }
}

/** After a refresh: pick up changes made on disk to a file being shown. */
async function revalidate(file: ChangedFile, target: DiffEditor, key: string) {
  const repo = state.repo;
  const comparison = state.comparison;
  const session = sessions.get(file.path);
  if (!repo || !comparison) return;
  if (!session) {
    if (key === FILE_VIEW) return showFile(file);
    hideSection(file.path);
    await showSection(file.path);
    return;
  }

  const token = nextToken(key);
  const pair = await ipc.readFilePair(repo.root, comparison, file);
  if (!isLatest(key, token) || unshowable(pair)) return;
  const diskText = pair.working ?? "";
  const baseChanged = (pair.base ?? "") !== (session.pair.base ?? "");
  const diskChanged = diskText !== session.savedText;
  if (!baseChanged && !diskChanged) return;

  if (state.dirty[file.path]) {
    if (diskChanged) setState("diskConflict", file.path, true);
    if (baseChanged) {
      session.pair = { ...session.pair, base: pair.base };
      const document = documentOf(file, session);
      target.open(document, state.mode, target.snapshot() ?? undefined);
      void attachLanguageServer(file, document, target);
    }
    return;
  }
  session.pair = pair;
  session.savedText = diskText;
  session.snapshot = null;
  target.reload(documentOf(file, session));
}

/** Resolves a disk conflict on a file with unsaved edits. */
export async function resolveDiskConflict(path: string | null, choice: "reload" | "keep") {
  const file = fileAt(path);
  const session = file && sessions.get(file.path);
  if (!file || !session || !state.repo || !state.comparison) return;
  const pair = await ipc.readFilePair(state.repo.root, state.comparison, file);
  session.savedText = pair.working ?? "";
  if (choice === "reload") {
    session.pair = pair;
    session.snapshot = null;
    setState("dirty", file.path, false);
    editorFor(file.path)?.reload(documentOf(file, session));
  }
  setState("diskConflict", file.path, false);
}

// ----------------------------------------------------------- all-files view

let revealSection: (path: string) => void = () => {};

/** How the all-files view scrolls a file into view. */
export function onRevealSection(handler: (path: string) => void) {
  revealSection = handler;
}

export async function setView(view: ViewMode) {
  if (view === state.view) return;
  localStorage.setItem("arnold.view", view);
  const path = activePath();
  if (view === "all") {
    stashOpenFile();
    editor?.close();
    focusedPath = null;
    // The all-files view lists the diff; a file outside it gives way to the first changed one.
    const selected = isChanged(state.selected) ? state.selected : (state.comparison?.files[0]?.path ?? null);
    setState({ view, selected, menu: null });
  } else {
    // Removing the all-files view hides its sections, keeping their edits.
    setState({ view, menu: null });
    await selectFile(path);
  }
}

/** All-files view: `host` is where the file at `path` is shown once it is near the screen. */
export function registerSection(path: string, host: HTMLElement) {
  sectionHosts.set(path, host);
}

export function unregisterSection(path: string) {
  hideSection(path);
  sectionHosts.delete(path);
}

/** Loads a file into its section of the all-files view, if not done yet. */
export async function showSection(path: string): Promise<DiffEditor | null> {
  const shown = sectionEditors.get(path);
  if (shown) return shown;
  const host = sectionHosts.get(path);
  const file = fileAt(path);
  const repo = state.repo;
  const comparison = state.comparison;
  if (!host || !file || !repo || !comparison || state.view !== "all") return null;
  const token = nextToken(path);
  try {
    const pair = await ipc.readFilePair(repo.root, comparison, file);
    if (!isLatest(path, token) || sectionHosts.get(path) !== host) return sectionEditors.get(path) ?? null;
    const reason = unshowable(pair);
    if (reason) {
      const note = document.createElement("div");
      note.className = "section-note";
      note.textContent = reason;
      host.replaceChildren(note);
      return null;
    }
    const session = sessionFor(file, pair);
    const target = new DiffEditor(host, callbacksFor(() => path));
    sectionEditors.set(path, target);
    const shownDocument = documentOf(file, session);
    target.open(shownDocument, state.mode, session.snapshot ?? undefined);
    void attachLanguageServer(file, shownDocument, target);
    return target;
  } catch (error) {
    fail(error);
    return null;
  }
}

/**
 * Unloads a section of the all-files view to save memory; its edits are kept.
 * `keepFocused` leaves the file being typed in alone. Returns whether an
 * editor was removed.
 */
export function hideSection(path: string, keepFocused = false): boolean {
  if (keepFocused && path === focusedPath) return false;
  nextToken(path); // A load still under way is no longer wanted.
  const target = sectionEditors.get(path);
  const session = sessions.get(path);
  const snapshot = target?.snapshot();
  if (session && snapshot) session.snapshot = snapshot;
  target?.destroy();
  sectionHosts.get(path)?.replaceChildren();
  sectionEditors.delete(path);
  if (focusedPath === path) focusedPath = null;
  // Without an editor the file goes back to background checking.
  if (target) void checkChangedFiles();
  return target !== undefined;
}

export function setCollapsed(path: string, collapsed: boolean) {
  setState("collapsed", path, collapsed);
  if (collapsed) hideSection(path);
}

// ------------------------------------------------------------------- editing

export function setMode(mode: DiffMode) {
  localStorage.setItem("arnold.mode", mode);
  setState("mode", mode);
  editor?.setMode(mode);
  for (const target of sectionEditors.values()) target.setMode(mode);
  activeEditor()?.focus();
}

/** Saves one file (by default the one being edited). */
export async function save(path: string | null = activePath()) {
  if (await writeOut(path)) {
    // Update the file list now rather than waiting for the file watcher.
    void refresh();
  }
}

/** Saves every file with unsaved edits. */
export async function saveAll() {
  let saved = false;
  for (const path of Object.keys(state.dirty)) saved = (await writeOut(path)) || saved;
  if (saved) void refresh();
}

async function writeOut(path: string | null): Promise<boolean> {
  const session = path ? sessions.get(path) : undefined;
  const worktree = state.comparison?.worktreePath;
  // A file of the all-files view that is not loaded keeps its text in the snapshot.
  const text = editorFor(path)?.text() ?? session?.snapshot?.doc.toString();
  if (!path || !session || !worktree || text == null || !state.dirty[path]) return false;
  try {
    await ipc.writeFile(worktree, path, text, session.pair.eol, session.pair.bom);
    session.savedText = text;
    session.pair = { ...session.pair, working: text };
    setState("dirty", path, false);
    setState("diskConflict", path, false);
    return true;
  } catch (error) {
    fail(error);
    return false;
  }
}

export function resetSelectedLines() {
  setState("menu", null);
  const target = activeEditor();
  target?.resetSelectedLines();
  target?.focus();
}

/** "Go to definition" in the context menu: the symbol that was right-clicked. */
export function goToDefinition() {
  setState("menu", null);
  if (menuTarget) lsp.goToDefinitionAt(menuTarget.view, menuTarget.position);
  menuTarget = null;
}

export const nextChange = () => activeEditor()?.nextChange();
export const previousChange = () => activeEditor()?.previousChange();

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
    setState("diskConflict", file.path, false);
    // The all-files view reloads the file in the refresh.
    if (state.view === "file" && state.selected === file.path) editor?.close();
    await refresh();
  } catch (error) {
    fail(error);
  }
}

// ----------------------------------------------------------- language server

/** Connects a file being shown to its language server, once that is running. */
async function attachLanguageServer(file: ChangedFile, document: DiffDocument, target: DiffEditor) {
  const worktree = state.comparison?.worktreePath;
  const language = languageIdFor(file.path);
  if (!worktree || !language || !document.editable) return;
  const client = await lsp.clientFor(worktree, language);
  // The user may have moved on while the server was starting.
  if (!client || editorFor(file.path) !== target) return;
  target.setLanguageSupport(client.plugin(pathToUri(joinPath(worktree, file.path)), language));
  ipc.devTrace(`language server attached to ${file.path}`);
  // The file shown before this one is closed now; it goes back to the background.
  void checkChangedFiles();
}

let checking = false;
let checkAgain = false;

/**
 * Hands every changed F# file that no editor shows to the language server,
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
        if (editorFor(file.path)) {
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
  const from = activePath();
  if (path !== from) {
    if (!fileAt(path)) setState("visited", (visited) => [...visited, unchangedFile(path)]);
    if (from) backStack.push(from);
    setState("backCount", backStack.length);
    if (state.view === "file") await selectFile(path);
  }
  if (state.view === "file") return editor?.working ?? null;
  return (await showFileSection(path))?.working ?? null;
}

/** All-files view: loads a file's section and makes it the one being edited. */
async function showFileSection(path: string) {
  setState({ selected: path, menu: null });
  setState("collapsed", path, false);
  focusedPath = path;
  return showSection(path);
}

/** Returns to the file go-to-definition was started from. */
export async function goBack() {
  const previous = backStack.pop();
  setState("backCount", backStack.length);
  if (state.view === "all") {
    const target = previous ? await showFileSection(previous) : activeEditor();
    target?.revealCursor();
    target?.focus();
    return;
  }
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
