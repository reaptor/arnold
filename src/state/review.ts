// Application state and the actions that change it. Components read `state`
// and call these functions; the CodeMirror editor is driven from here.

import type { EditorView } from "@codemirror/view";
import { open as openDialog } from "@tauri-apps/plugin-dialog";
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
import type { ChangedFile, Comparison, FilePair, FileStatus, RepoInfo, SetupEvent } from "../ipc/commands";
import * as lsp from "../lsp/clients";
import { joinPath, pathToUri, relativeTo, uriToPath } from "../lsp/uri";
import { comparePaths } from "./pathOrder";

/** One file at a time, or every file of the diff in one scrolling list. */
export type ViewMode = "file" | "all";

/**
 * Getting F# language features for the reviewed commit: preparing a copy of
 * it, starting FsAutoComplete, waiting for it to load the projects.
 */
export type SetupPhase = "preparing" | "starting" | "loading" | "ready" | "failed";

export interface SetupStep {
  title: string;
  state: "running" | "done" | "failed" | "skipped";
  note: string | null;
}

export interface LanguageSetup {
  commit: string;
  phase: SetupPhase;
  /** The prepared copy the server runs in, once there is one. */
  path: string | null;
  steps: SetupStep[];
  /** Why features are off or may be incomplete. */
  problems: string[];
}

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
  /** Errors and warnings the language server reported, by file path. */
  problems: Record<string, { errors: number; warnings: number }>;
  /** What the language server is busy with, if anything. */
  lspStatus: string | null;
  /** F# language features for the reviewed commit; null when it has no F# projects. */
  languageSetup: LanguageSetup | null;
  /** Number of files that "back" can return to. */
  backCount: number;
  busy: boolean;
  error: string | null;
  /** Shown in place of the editor (nothing selected, binary file, ...). */
  placeholder: string | null;
  /** Files folded away in the all-files view. */
  collapsed: Record<string, boolean>;
  /**
   * Context menu of an editor; `definition` when a language server can say
   * where the right-clicked symbol is defined.
   */
  menu: { x: number; y: number; definition: boolean } | null;
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
  problems: {},
  lspStatus: null,
  languageSetup: null,
  backCount: 0,
  busy: false,
  error: null,
  placeholder: "Open a repository to start reviewing.",
  collapsed: {},
  menu: null,
});

/** What is known about a file that has been opened in this comparison. */
interface Session {
  pair: FilePair;
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

/** What the preparation steps printed, last lines only. Kept out of the store. */
const [setupOutput, setSetupOutput] = createSignal<readonly string[]>([]);
export { setupOutput };
const MAX_OUTPUT_LINES = 400;

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

/**
 * Created and deleted files are shown whole (a deleted one read-only) instead
 * of as a diff with one side empty. Unchanged files have no diff at all.
 */
const WHOLE_FILE: FileStatus[] = ["added", "deleted", "unchanged"];

function documentOf(file: ChangedFile, session: Session): DiffDocument {
  const base = session.pair.base ?? "";
  const working = session.pair.working ?? "";
  return {
    path: file.path,
    base,
    working: file.status === "deleted" ? base : working,
    plain: WHOLE_FILE.includes(file.status),
  };
}

/** The editor showing `path`, if one does. */
function editorFor(path: string | null): DiffEditor | null {
  if (path === null) return null;
  if (state.view === "all") return sectionEditors.get(path) ?? null;
  return state.selected === path ? editor : null;
}

/** The file keyboard commands act on: the selected one, or the one with keyboard focus. */
function activePath(): string | null {
  if (state.view === "all" && focusedPath !== null && sectionEditors.has(focusedPath)) return focusedPath;
  return state.selected;
}

const activeEditor = () => editorFor(activePath());

function callbacksFor(pathOf: () => string | null): DiffEditorCallbacks {
  return {
    onFocus() {
      const path = pathOf();
      if (state.view !== "all" || path === null) return;
      focusedPath = path;
      if (state.selected !== path) setState("selected", path);
    },
    onContextMenu(x, y, view, position) {
      menuTarget = position === null ? null : { view, position };
      setState("menu", { x, y, definition: menuTarget !== null && lsp.hasLanguageServer(view) });
    },
  };
}

export function attachEditor(host: HTMLElement) {
  editor = new DiffEditor(host, callbacksFor(() => state.selected));
  lsp.onDisplayFile(displayUri);
  lsp.onStartFailed((reason) => addSetupProblem(String(reason)));
  lsp.onServerEvent(onServerEvent);
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
  setState(change);
  localStorage.setItem(
    `arnold.branches.${state.repo.root}`,
    JSON.stringify({ base: state.base, head: state.head }),
  );
  resetSessions();
  setState({ selected: null });
  await refresh();
}

/**
 * Forgets the files of the last comparison. The language server keeps
 * running: it is replaced only when the reviewed commit changes.
 */
function resetSessions() {
  sessions.clear();
  backgroundSent.clear();
  backStack.length = 0;
  editor?.close();
  setState("problems", reconcile({}));
  setState("collapsed", reconcile({}));
  // Dropping the list also removes the file sections of the all-files view.
  setState({ comparison: null, visited: [], backCount: 0 });
  setAllFiles([]);
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
    if (setupKey(repo, comparison) !== preparedKey) void setUpLanguageFeatures(repo, comparison);

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
      await selectFile(comparison.files[0]?.path ?? null);
    }
    void checkChangedFiles();
  } catch (error) {
    fail(error);
  }
}

async function loadAllFiles(repo: RepoInfo, comparison: Comparison) {
  try {
    const files = await ipc.listFiles(repo.root, comparison);
    if (repo === state.repo && comparison.headCommit === state.comparison?.headCommit) setAllFiles(files);
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
  let session = sessions.get(file.path);
  if (!session) {
    session = { pair, snapshot: null };
    sessions.set(file.path, session);
  } else {
    if (pair.working !== session.pair.working) session.snapshot = null;
    session.pair = pair;
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
    editor?.open(documentOf(file, session), state.mode, session.snapshot ?? undefined);
    if (editor) void attachLanguageServer(file, editor);
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
  if (pair.base === session.pair.base && pair.working === session.pair.working) return;
  session.pair = pair;
  session.snapshot = null;
  target.reload(documentOf(file, session));
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
    // Removing the all-files view hides its sections.
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
    target.open(documentOf(file, session), state.mode, session.snapshot ?? undefined);
    void attachLanguageServer(file, target);
    return target;
  } catch (error) {
    fail(error);
    return null;
  }
}

/**
 * Unloads a section of the all-files view to save memory.
 * `keepFocused` leaves the file with keyboard focus alone. Returns whether an
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

// ------------------------------------------------------------------- viewing

export function setMode(mode: DiffMode) {
  localStorage.setItem("arnold.mode", mode);
  setState("mode", mode);
  editor?.setMode(mode);
  for (const target of sectionEditors.values()) target.setMode(mode);
  activeEditor()?.focus();
}

/** "Go to definition" in the context menu: the symbol that was right-clicked. */
export function goToDefinition() {
  setState("menu", null);
  if (menuTarget) lsp.goToDefinitionAt(menuTarget.view, menuTarget.position);
  menuTarget = null;
}

export const nextChange = () => activeEditor()?.nextChange();
export const previousChange = () => activeEditor()?.previousChange();

// ----------------------------------------------------------- language server

/** Commit the language server was last set up for, with its repository and branch. */
let preparedKey: string | null = null;
/** Bumped for each setup, so that a superseded one stops quietly. */
let setupRun = 0;

const setupKey = (repo: RepoInfo, comparison: Comparison) =>
  `${repo.root}\n${comparison.head}\n${comparison.headCommit}`;

/** The prepared copy of the reviewed commit, once a server may run there. */
function serverRoot(): string | null {
  const setup = state.languageSetup;
  return setup && setup.phase !== "preparing" && setup.phase !== "failed" ? setup.path : null;
}

/**
 * Gets F# language features going for the reviewed commit: a copy of it is
 * prepared (restored, built where needed), then FsAutoComplete starts there
 * and loads the projects. The diff can be used all along; editors get
 * language features once the server runs.
 */
async function setUpLanguageFeatures(repo: RepoInfo, comparison: Comparison) {
  preparedKey = setupKey(repo, comparison);
  const run = ++setupRun;
  const current = () => run === setupRun;
  await lsp.stop();
  forEachShownEditor((target) => target.setLanguageSupport([]));
  backgroundSent.clear();
  setState("problems", reconcile({}));
  setSetupOutput([]);
  setState({
    lspStatus: null,
    languageSetup: { commit: comparison.headCommit, phase: "preparing", path: null, steps: [], problems: [] },
  });

  let prepared: { path: string | null };
  try {
    prepared = await ipc.prepareWorkspace(repo.root, comparison.head, comparison.headCommit, (event) => {
      if (current()) onSetupEvent(event);
    });
  } catch (error) {
    if (!current()) return;
    finishStep("failed");
    addSetupProblem(String(error));
    setState("languageSetup", "phase", "failed");
    return;
  }
  if (!current()) return;
  finishStep("done");
  if (prepared.path === null) {
    setState("languageSetup", null);
    return;
  }

  setState("languageSetup", { path: prepared.path, phase: "starting" });
  startStep("Starting FsAutoComplete");
  const client = await lsp.clientFor(prepared.path, "fsharp");
  if (!current()) return;
  if (!client) {
    finishStep("failed");
    setState("languageSetup", "phase", "failed");
    return;
  }
  finishStep("done");
  startStep("Loading projects");
  setState("languageSetup", "phase", "loading");
  ipc.devTrace(`language server running in ${prepared.path}`);
  forEachShownEditor((target, file) => void attachLanguageServer(file, target));
  void checkChangedFiles();
}

function onSetupEvent(event: SetupEvent) {
  if (event.kind !== "output") ipc.devTrace(`setup ${event.kind}: ${"title" in event ? event.title : event.text}`);
  switch (event.kind) {
    case "step":
      finishStep("done");
      startStep(event.title);
      break;
    case "skipped":
      finishStep("done");
      setState("languageSetup", "steps", (steps): SetupStep[] => [
        ...steps,
        { title: event.title, state: "skipped", note: event.reason },
      ]);
      break;
    case "warning":
      finishStep("failed", event.text);
      addSetupProblem(event.text);
      break;
    case "output":
      setSetupOutput((lines) => [...lines.slice(-(MAX_OUTPUT_LINES - 1)), event.line]);
      break;
  }
}

function startStep(title: string) {
  if (!state.languageSetup) return;
  setState("languageSetup", "steps", (steps): SetupStep[] => [...steps, { title, state: "running", note: null }]);
}

/** Ends the running step, if any. */
function finishStep(outcome: "done" | "failed", note?: string) {
  const steps = state.languageSetup?.steps ?? [];
  const index = steps.findIndex((step) => step.state === "running");
  if (index < 0) return;
  setState("languageSetup", "steps", index, { state: outcome, note: note ?? steps[index].note });
}

function addSetupProblem(problem: string) {
  if (state.languageSetup) setState("languageSetup", "problems", (problems) => [...problems, problem]);
}

function onServerEvent(event: ipc.LspEvent) {
  switch (event.kind) {
    case "status":
      setState("lspStatus", event.text);
      if (event.text) ipc.devTrace(`lsp status: ${event.text}`);
      break;
    case "workspaceLoaded":
      if (state.languageSetup?.phase === "loading") {
        finishStep("done");
        setState("languageSetup", "phase", "ready");
        ipc.devTrace("language features ready");
      }
      break;
    case "projectFailed": {
      const name = event.project.split(/[\\/]/).pop() || event.project;
      addSetupProblem(`${name} could not be loaded: ${event.message}`);
      break;
    }
    case "exited":
      finishStep("failed");
      addSetupProblem("The language server stopped.");
      setState({ lspStatus: null });
      if (state.languageSetup) setState("languageSetup", "phase", "failed");
      break;
    case "diagnostics": {
      const root = serverRoot();
      const absolute = uriToPath(event.uri);
      const path = root && absolute ? relativeTo(root, absolute) : null;
      if (path !== null) setState("problems", path, { errors: event.errors, warnings: event.warnings });
      ipc.devTrace(`diagnostics ${path ?? event.uri}: ${event.errors} errors, ${event.warnings} warnings`);
      break;
    }
  }
}

/** Every editor on screen, with its file. */
function forEachShownEditor(action: (target: DiffEditor, file: ChangedFile) => void) {
  if (state.view === "all") {
    for (const [path, target] of sectionEditors) {
      const file = fileAt(path);
      if (file) action(target, file);
    }
  } else {
    const file = fileAt(state.selected);
    if (file && editor) action(editor, file);
  }
}

/** Connects a file being shown to its language server, once that is running. */
async function attachLanguageServer(file: ChangedFile, target: DiffEditor) {
  // A deleted file is not in the prepared copy.
  const root = serverRoot();
  const language = languageIdFor(file.path);
  if (!root || !language || file.status === "deleted") return;
  const client = await lsp.clientFor(root, language);
  // The user may have moved on while the server was starting.
  if (!client || editorFor(file.path) !== target) return;
  target.setLanguageSupport(client.plugin(pathToUri(joinPath(root, file.path)), language));
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
      const root = serverRoot();
      if (!repo || !comparison || !root) return;
      const files = comparison.files
        .filter((file) => languageIdFor(file.path) === "fsharp" && file.status !== "deleted" && !file.binary)
        .slice(0, MAX_BACKGROUND_FILES);
      if (files.length === 0 || !(await lsp.clientFor(root, "fsharp"))) return;

      for (const file of files) {
        if (comparison !== state.comparison) break;
        if (editorFor(file.path)) {
          // The editor owns this file; send it afresh once it is left.
          backgroundSent.delete(file.path);
          continue;
        }
        const text = (await ipc.readFilePair(repo.root, comparison, file)).working;
        if (text == null || backgroundSent.get(file.path) === text) continue;
        backgroundSent.set(file.path, text);
        await lsp.openInBackground(pathToUri(joinPath(root, file.path)), "fsharp", text);
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
  const root = serverRoot();
  const absolute = uriToPath(uri);
  const path = root && absolute ? relativeTo(root, absolute) : null;
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

/** All-files view: loads a file's section and gives it the keyboard. */
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
  // The backend watches the repository's branches: new commits refresh the diff.
  await ipc.onRefsChanged(() => {
    window.clearTimeout(refreshTimer);
    refreshTimer = window.setTimeout(() => void refresh(), 150);
  });
}
