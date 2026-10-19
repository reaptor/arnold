// Application state and the actions that change it. Components read `state`
// and call these functions; the CodeMirror editor is driven from here.

import { EditorSelection } from "@codemirror/state";
import type { EditorView } from "@codemirror/view";
import { open as openDialog } from "@tauri-apps/plugin-dialog";
import { createSignal } from "solid-js";
import { createStore, reconcile } from "solid-js/store";
import {
  type BlockedEdit,
  DiffEditor,
  type DiffDocument,
  type DiffEditorCallbacks,
  type DiffMode,
  type LineAction,
  type Snapshot,
} from "../editor/diffEditor";
import { languageIdFor } from "../editor/languages";
import * as ipc from "../ipc/commands";
import type {
  ChangedFile,
  CommitRange,
  Comparison,
  FilePair,
  FileStatus,
  FormatCheck,
  NewThreadPlace,
  PrChange,
  PrComment,
  PrThread,
  PullRequest,
  RepoInfo,
  SetupEvent,
  Vote,
  WorkingSide,
  WorkingStatus,
  Worktree,
} from "../ipc/commands";
import * as lsp from "../lsp/clients";
import { joinPath, pathToUri, relativeTo, uriToPath } from "../lsp/uri";
import { comparePaths } from "./pathOrder";
import { type Suggestion, suggestionComment, suggestionPlace, suggestionsFor, withoutBlocks } from "./suggestions";
import type { LineBlock } from "../editor/lineDiff";
import { isSuggestion, placeName, setThreadActions } from "../editor/threads";
import { azureHostLinks, setHostLinks } from "../components/Markdown";
import * as pullRequests from "./pullRequests";
import * as commitMode from "./commitMode";
import { confirm } from "@tauri-apps/plugin-dialog";
import { flushSave, isSaving, queueSave, saveCount } from "./autosave";
import { firstReview, holdPlaces, notePlace, type Place, placeIn, placesReady } from "./places";

/**
 * What is shown: the uncommitted changes of a checkout, to commit them (the
 * first step with an agent's work); one branch against another; or a pull
 * request of the repository's code host.
 */
export type Source = "commit" | "branches" | "pullRequest";

/** A pull request picked for review, with the remote it came from. */
export type ReviewedPullRequest = PullRequest & { remote: string };

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

export interface SetupProblem {
  text: string;
  /** The full output it was taken from (a project's build log), if any. */
  details: string | null;
}

/** Where FsAutoComplete is with loading one project. */
export type ProjectLoad = "loading" | "loaded" | "failed";

export interface LanguageSetup {
  commit: string;
  phase: SetupPhase;
  /** The folder the server runs in, once it is ready: the opened checkout or a prepared copy. */
  path: string | null;
  /** The reviewed branch is checked out in the opened folder, so no copy is made. */
  inPlace: boolean;
  steps: SetupStep[];
  /** Why features are off or may be incomplete. */
  problems: SetupProblem[];
  /** The projects FsAutoComplete announced, by path, and how loading them went. */
  projects: Record<string, ProjectLoad>;
  /** The project being loaded now. */
  currentProject: string | null;
}

interface State {
  repo: RepoInfo | null;
  source: Source;
  /** Branches compared when `source` is "branches". */
  base: string;
  head: string;
  /** Reviewed when `source` is "pullRequest"; null until one is picked. */
  pullRequest: ReviewedPullRequest | null;
  /**
   * The worktree of Arnold's own the pull request under review is edited in
   * (`.arnold/worktrees/pr-<id>`), once made; edits there become suggestions.
   */
  prWorktree: (Worktree & { pullRequest: number }) | null;
  /** The pull request being opened (read, its commits fetched), by id. */
  openingPullRequest: number | null;
  /** What is being sent to the host about the pull request under review: a vote or a change. */
  updatingPullRequest: "vote" | PrChange["kind"] | null;
  /** The commits of `head` being shown, or null for all of its changes. */
  range: CommitRange | null;
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
  /** Commit mode: the checkout whose uncommitted changes are listed. */
  checkout: string | null;
  /** Commit mode: its uncommitted changes, once read. */
  work: WorkingStatus | null;
  /**
   * Commit mode: which diff of the selected file is shown: unstaged (index →
   * working tree) or staged (HEAD → index). Null for a file opened from the
   * explorer, shown whole.
   */
  workSide: WorkingSide | null;
  /** Commit mode: a commit or push is under way; what is happening, or null. */
  committing: string | null;
  /**
   * Commit mode: Fantomas would reformat the code base (or could not check
   * it: `failure`), while asking whether to commit anyway.
   */
  unformatted: (FormatCheck & { failure?: string }) | null;
  /** A short message in the status bar (Commit mode: what a key did or why not). */
  notice: string | null;
  /** Files folded away in the all-files view. */
  collapsed: Record<string, boolean>;
  /**
   * Context menu of an editor; `definition` when a language server can say
   * where the right-clicked symbol is defined. `comment`: where a comment
   * would go ("line 14"), when one can be added; null when not, and
   * undefined when comments do not apply (two branches). `file`: the menu
   * of a file of the dock rather than of an editor.
   */
  menu: {
    x: number;
    y: number;
    definition: boolean;
    comment?: string | null;
    file?: string;
    /** Commit mode: what can be done to the selected lines (an editor) or the file (a list of the dock). */
    lines?: readonly LineAction[];
    /** Commit mode: the list of the dock the file was right-clicked in. */
    side?: WorkingSide;
    /** Commit mode: the right-clicked editor can be typed in, so its line actions need Alt. */
    editable?: boolean;
    /**
     * Branches, editing a checked-out branch: undoing uncommitted edits of
     * `path`, in the selected lines (an editor) and in the whole file.
     */
    undo?: { path: string; lines: boolean; file: boolean };
    /**
     * A file or folder of the explorer (or an untracked file of Unstaged):
     * adding it to the root `.gitignore` of `checkout`; null when no
     * checkout shows the files (a branch that is not checked out).
     */
    ignore?: {
      path: string;
      folder: boolean;
      checkout: string | null;
      /** Whether git tracks it (or files in the folder), for "Stop tracking"; undefined while asking. */
      tracked?: boolean;
    };
  } | null;
}

export const [state, setState] = createStore<State>({
  repo: null,
  source: "branches",
  base: "",
  head: "",
  pullRequest: null,
  prWorktree: null,
  openingPullRequest: null,
  updatingPullRequest: null,
  range: null,
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
  checkout: null,
  work: null,
  workSide: null,
  committing: null,
  unformatted: null,
  notice: null,
});

/** What is known about a file that has been opened in this comparison. */
interface Session {
  pair: FilePair;
  snapshot: Snapshot | null;
}

const sameItems = (a: readonly string[], b: readonly string[]) =>
  a.length === b.length && a.every((item, index) => item === b[index]);

/**
 * Every file of the commit shown (the branch tip, or the newest commit picked),
 * for the file explorer. Kept out of the store, which
 * would wrap each of possibly many thousands of paths.
 */
const [allFiles, setAllFiles] = createSignal<readonly string[]>([], { equals: sameItems });
export { allFiles, setAllFiles };

/**
 * The file the explorer marks and follows. In Commit mode each part of the
 * dock marks only what was picked in it, so a file picked in Unstaged or
 * Staged leaves the explorer alone.
 */
export const explorerSelection = (): string | null =>
  state.source === "commit" && state.workSide !== null ? null : state.selected;

/**
 * The files with changes, for marking them in the explorer: those of the
 * diff, or in Commit mode the uncommitted ones (unstaged first).
 */
export function changedFiles(): readonly ChangedFile[] {
  if (state.source !== "commit") return state.comparison?.files ?? [];
  const work = state.work;
  if (!work) return [];
  const unstaged = new Set(work.unstaged.map((file) => file.path));
  return [...work.unstaged, ...work.staged.filter((file) => !unstaged.has(file.path))];
}

/** The discussions of a pull request, and which one (`remote/id`) they belong to. Kept out of the store. */
const [discussion, setDiscussion] = createSignal<{ key: string; threads: readonly PrThread[] }>({ key: "", threads: [] });

const pullRequestKey = (pullRequest: ReviewedPullRequest | null) =>
  pullRequest ? `${pullRequest.remote}/${pullRequest.id}` : "";

/** The discussions of the pull request under review; empty for branches. */
export const threads = (): readonly PrThread[] =>
  state.source === "pullRequest" && discussion().key === pullRequestKey(state.pullRequest) && discussion().key !== ""
    ? discussion().threads
    : [];

/**
 * Threads about `path` (its lines or the whole file), to show in its diff. Their lines are those
 * of the pull request's latest push against the merge-base, so they are left
 * out while only some of the commits are shown.
 */
export const fileThreads = (path: string): readonly PrThread[] =>
  state.range === null ? threads().filter((thread) => thread.path === path) : [];

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
/** The editor of the one-file view, which Commit mode uses too. */
export const fileEditor = () => editor;
/** All-files view: where each file goes, and the editors of the files loaded so far. */
const sectionHosts = new Map<string, HTMLElement>();
const sectionEditors = new Map<string, DiffEditor>();
/** All-files view: the file whose editor last had keyboard focus. */
let focusedPath: string | null = null;
/** Where the context menu was opened; kept out of the store, which would wrap the view. */
let menuTarget: { view: EditorView; position: number } | null = null;
/** Where a comment from the context menu would go. */
let menuComment: NewThreadPlace | null = null;
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

/**
 * How `file` is shown, with `texts` (by default what was read). When the
 * comparison reads the branch's checkout its files can be edited; a file
 * outside the diff is shown alone while its text is the merge-base's, and
 * as a diff once edited.
 */
function documentOf(
  file: ChangedFile,
  session: Session,
  texts: Pick<FilePair, "base" | "working" | "committed"> = session.pair,
): DiffDocument {
  const base = texts.base ?? "";
  const working = texts.working ?? "";
  const deleted = file.status === "deleted";
  const editable = !!state.comparison?.checkout && !deleted;
  return {
    path: file.path,
    base,
    working: deleted ? base : working,
    plain: file.status === "unchanged" ? base === working : WHOLE_FILE.includes(file.status),
    deleted,
    editable,
    // Frames the uncommitted edits; a file the last commit lacks is all edits, framed by nothing.
    committed: editable ? ((texts.committed === undefined ? session.pair.committed : texts.committed) ?? null) : null,
    suggest: editable && state.source === "pullRequest",
    threads: fileThreads(file.path),
    draft: drafts.get(file.path) ?? null,
  };
}

/** The editor showing `path`, if one does. */
function editorFor(path: string | null): DiffEditor | null {
  if (path === null) return null;
  if (state.view === "all" && state.source === "commit") return commitMode.languageEditorFor(path);
  if (state.view === "all") return sectionEditors.get(path) ?? null;
  return state.selected === path ? editor : null;
}

/** The file keyboard commands act on: the selected one, or the one with keyboard focus. */
function activePath(): string | null {
  if (state.view === "all" && focusedPath !== null && sectionEditors.has(focusedPath)) return focusedPath;
  return state.selected;
}

const activeEditor = () => (state.source === "commit" ? commitMode.currentEditor() : editorFor(activePath()));

/**
 * What an editor reports back. `side`: a section of Commit mode's all-files
 * view, which becomes the file acted on once it is focused or clicked.
 */
export function editorCallbacks(pathOf: () => string | null, side?: WorkingSide): DiffEditorCallbacks {
  const activate = () => {
    const path = pathOf();
    if (side && path !== null) commitMode.focusSection(side, path);
  };
  return {
    onFocus() {
      const path = pathOf();
      if (side) return activate();
      if (state.view !== "all" || path === null) return;
      focusedPath = path;
      if (state.selected !== path) setState("selected", path);
    },
    onContextMenu(x, y, view, position, comment) {
      activate();
      menuTarget = position === null ? null : { view, position };
      const path = pathOf();
      menuComment = path !== null && comment && canComment(path) ? { path, lines: comment } : null;
      const undos = path !== null && editsUndoable(path) ? editorFor(path)?.editUndos() : undefined;
      setState("menu", {
        x,
        y,
        definition: menuTarget !== null && lsp.hasLanguageServer(view),
        comment: state.source === "pullRequest" ? (menuComment && placeName(menuComment)) : undefined,
        lines:
          state.source === "commit"
            ? (side && path !== null ? commitMode.sectionEditor(side, path) : editorFor(path))?.lineActions()
            : undefined,
        editable: !view.state.readOnly,
        undo: undos && path !== null ? { path, lines: undos.lines, file: !!fileAt(path)?.uncommitted } : undefined,
      });
    },
    onStepChange(direction) {
      void stepChange(direction);
    },
    onChange(text) {
      const path = pathOf();
      if (path === null) return;
      if (state.source === "commit") commitMode.edited(path, text);
      else edited(path, text);
    },
    onLineAction(action) {
      activate();
      if (state.source === "commit") void commitMode.lineAction(action);
    },
    onMoveChunk(direction, lines) {
      activate();
      if (state.source === "commit") void commitMode.moveChunk(direction, lines);
    },
    onSuggest(block) {
      const path = pathOf();
      if (path !== null) void suggestChange(path, block);
    },
    onPlaceChange() {
      // Sections of the all-files view: the list keeps the place there.
      if (state.view === "file" && pathOf() === state.selected) noteFilePlace();
    },
    onBlockedEdit(edit) {
      if (!editBlocked()) return;
      const path = pathOf();
      if (!editInfoOpen()) {
        // Said once: typing on afterwards is just ignored.
        if (editInfoShown) return;
        editInfoShown = true;
        openEditInfo(edit.rect);
        pendingEdit = path !== null && edit.at ? { path, at: edit.at, text: "" } : null;
      }
      // Kept while the popup is open, to be typed once the diff can be edited.
      if (pendingEdit?.path === path) {
        pendingEdit.text = edit.erase ? pendingEdit.text.slice(0, -1) : pendingEdit.text + edit.text;
      }
    },
  };
}

export function attachEditor(host: HTMLElement) {
  editor = new DiffEditor(host, editorCallbacks(() => state.selected));
  setThreadActions({ reply, createThread, cancelDraft, deleteComment });
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
    // The branch checked out in the folder, which needs no copy for the
    // language server; another only when that one is the base (or detached).
    const current = repo.currentBranch && repo.currentBranch !== base ? repo.currentBranch : null;
    const last = rememberedReview(repo.root);
    const head = current ?? (has(remembered.head) && remembered.head !== base ? remembered.head : defaultHead(repo, base));
    if (state.source === "commit") commitMode.leave();
    resetSessions();
    setState({ repo, base, head, source: "branches", pullRequest: null, prWorktree: null, range: null, comparison: null, selected: null });
    // The store's copy: a later repository replaces it.
    const opened = state.repo;
    await pullRequests.loadRemotes(repo.root);
    if (opened !== state.repo) return;
    // Uncommitted changes come first: they are committed before they are reviewed.
    // The mode the app was closed in. Uncommitted changes open Commit mode
    // instead of a pull request (or nothing remembered), as only Branches
    // shows them too; the pull request opens when Pull request mode is picked.
    if (last.source === "commit" || (last.source !== "branches" && (await commitMode.hasUncommitted(repo.root)))) {
      if (opened === state.repo) await setSource("commit", false);
      return;
    }
    if (opened !== state.repo) return;
    if (last.source === "pullRequest" && rememberedPullRequest(repo.root)) {
      await setSource("pullRequest", false);
      return;
    }
    await setView("all");
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
  if (state.source === "commit") commitMode.leave();
  setState({ ...change, source: "branches", range: null });
  rememberReview(state.repo.root, { source: "branches" });
  localStorage.setItem(
    `arnold.branches.${state.repo.root}`,
    JSON.stringify({ base: state.base, head: state.head }),
  );
  resetSessions();
  setState({ selected: null });
  await setView("all");
  await refresh();
}

// ------------------------------------------------------------- pull requests

// ------------------------------------------------- kept between app starts

/** What a repository was last reviewed as (null: nothing kept yet), and the pull request last opened in it (until closed). */
interface RememberedReview {
  source: Source | null;
  pullRequest: { remote: string; id: number } | null;
}

const reviewKey = (root: string) => `arnold.review.${root}`;

function rememberedReview(root: string): RememberedReview {
  try {
    const saved = JSON.parse(localStorage.getItem(reviewKey(root)) ?? "{}");
    const pullRequest =
      typeof saved.pullRequest?.remote === "string" && typeof saved.pullRequest?.id === "number"
        ? { remote: saved.pullRequest.remote, id: saved.pullRequest.id }
        : null;
    return { source: ["commit", "branches", "pullRequest"].includes(saved.source) ? saved.source : null, pullRequest };
  } catch {
    return { source: null, pullRequest: null };
  }
}

function rememberReview(root: string, change: Partial<RememberedReview>) {
  localStorage.setItem(reviewKey(root), JSON.stringify({ ...rememberedReview(root), ...change }));
}

/** The pull request last opened in the repository, when its remote still has pull requests. */
function rememberedPullRequest(root: string): RememberedReview["pullRequest"] {
  const pullRequest = rememberedReview(root).pullRequest;
  return pullRequest && pullRequests.prs.remotes.some((remote) => remote.remote === pullRequest.remote) ? pullRequest : null;
}

/**
 * Switches between reviewing two branches and reviewing a pull request.
 * Each keeps what was picked in it, so switching back shows that again;
 * Pull request mode with none picked opens the one last opened, from an
 * earlier start. `remember` false: the app's own switch, not kept for the
 * next start.
 */
export async function setSource(source: Source, remember = true) {
  if (!state.repo || source === state.source) return;
  if (remember) rememberReview(state.repo.root, { source });
  const reopen = source === "pullRequest" && !state.pullRequest ? rememberedPullRequest(state.repo.root) : null;
  if (reopen) {
    await openPullRequest(reopen.remote, reopen.id);
    // It could not be opened (gone, or no token): the list, with why.
    if (state.pullRequest) return;
  }
  if (state.source === "commit") commitMode.leave();
  resetSessions();
  setState({ source, range: null, selected: null });
  if (source === "commit") return commitMode.enter();
  await setView("all");
  if (source === "pullRequest" && pullRequests.prs.list === null) void pullRequests.loadPullRequests();
  await refresh();
}

/**
 * Reviews pull request `id` of `remote`: reads it and fetches its commits if
 * the repository lacks them, then compares them as a pull request does.
 */
export async function openPullRequest(remote: string, id: number) {
  const repo = state.repo;
  if (!repo) return;
  setState({ openingPullRequest: id, error: null });
  try {
    const pullRequest = await ipc.openPullRequest(repo.root, remote, id);
    if (repo !== state.repo || state.openingPullRequest !== id) return;
    if (state.source === "commit") commitMode.leave();
    resetSessions();
    drafts.clear();
    setState({ source: "pullRequest", pullRequest: { ...pullRequest, remote }, prWorktree: null, range: null, selected: null });
    rememberReview(repo.root, { source: "pullRequest", pullRequest: { remote, id } });
    setHostLinks(azureHostLinks(pullRequest.url));
    void loadThreads();
    // Checked out from the start, so it can be edited (into suggestions) and
    // the language server runs on its files.
    await loadPrWorktree(true);
    await setView("all");
    await refresh();
  } catch (error) {
    if (repo === state.repo) fail(pullRequests.prErrorText(error));
  } finally {
    if (state.openingPullRequest === id) setState("openingPullRequest", null);
  }
}

/** Leaves the pull request, back to the list of them. */
export function closePullRequest() {
  if (state.repo) rememberReview(state.repo.root, { pullRequest: null });
  resetSessions();
  setState({ pullRequest: null, prWorktree: null, range: null, selected: null });
}

/** The two commits or branches compared, or null when nothing is picked yet. */
function compared(): { base: string; head: string } | null {
  if (state.source === "commit") return null;
  if (state.source === "pullRequest") {
    const pullRequest = state.pullRequest;
    return pullRequest?.sourceCommit && pullRequest.targetCommit
      ? { base: pullRequest.targetCommit, head: pullRequest.sourceCommit }
      : null;
  }
  return state.base && state.head ? { base: state.base, head: state.head } : null;
}

/** The branch reviewed and the one it goes into, by name: the pull request's or the picked ones. */
export function branchNames(): { base: string; head: string } {
  if (state.source === "pullRequest") {
    const pullRequest = state.pullRequest;
    return { base: pullRequest?.targetBranch ?? "", head: pullRequest?.sourceBranch ?? "" };
  }
  return { base: state.base, head: state.head };
}

/** Name of what is reviewed, for the folder of the language server's copy. */
const reviewedName = (comparison: Comparison) =>
  state.source === "pullRequest" && state.pullRequest ? state.pullRequest.sourceBranch : comparison.head;

const sameRange = (a: CommitRange | null, b: CommitRange | null) =>
  a === b || (a !== null && b !== null && a.oldest === b.oldest && a.newest === b.newest);

/** Shows only what a run of the branch's commits changed, or with null all of its changes. */
export async function setRange(range: CommitRange | null) {
  if (!state.repo || sameRange(range, state.range)) return;
  resetSessions();
  setState("range", range);
  await refresh();
}

/**
 * Forgets the files of the last comparison. The language server keeps
 * running: it is replaced only when the reviewed commit changes.
 */
export function resetSessions() {
  // An edit waiting to be saved still goes to the checkout it was made in.
  void flushSave();
  // Another review: where the user was in it is restored by the next refresh.
  holdPlaces();
  placePending = true;
  editInfoShown = false;
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
  if (repo && state.source === "branches" && !(await checkOutHead(repo))) return;
  const revs = compared();
  if (!repo || !revs) return;
  try {
    const range = state.range;
    const comparison = await ipc.compareBranches(repo.root, revs.base, revs.head, range, range ? null : prCheckout());
    const now = compared();
    if (repo !== state.repo || range !== state.range || now?.base !== revs.base || now.head !== revs.head) return;
    // Commits no longer on the branch (rebased away): all of the changes are shown instead.
    if (!sameRange(comparison.range, range)) setState("range", comparison.range);
    // Listed, stepped through and shown in the order of the file explorer.
    comparison.files.sort((a, b) => comparePaths(a.path, b.path));
    // Deleted files start collapsed in the all-files view; once opened or
    // closed by hand they keep that across refreshes.
    for (const file of comparison.files) {
      if (file.status === "deleted" && !(file.path in state.collapsed)) setState("collapsed", file.path, true);
    }
    // The file shown on its own whose edits took it back to the merge-base
    // stays shown, now as a file outside the diff.
    const shown = state.view === "file" && comparison.checkout ? state.selected : null;
    const dropped =
      shown !== null && isChanged(shown) && !comparison.files.some((file) => file.path === shown) ? shown : null;
    setState("comparison", reconcile(comparison, { key: "path" }));
    void loadAllFiles(repo, comparison);
    // A visited file that has since been edited is now part of the diff.
    setState("visited", (visited) => [
      ...visited.filter((file) => !comparison.files.some((changed) => changed.path === file.path)),
      ...(dropped !== null && !visited.some((file) => file.path === dropped) ? [unchangedFile(dropped)] : []),
    ]);
    setState("error", null);
    if (setupKey(repo, comparison) !== preparedKey) void setUpLanguageFeatures(repo, comparison);

    // A review just opened: where the user was in it last time.
    const place = placePending ? takePlace(repo.root) : undefined;
    const listed = (path: string | undefined) => path !== undefined && comparison.files.some((file) => file.path === path);
    // Nothing is shown yet, so the view can just be set.
    // (The file on its own only while that file is still in the diff.)
    if (place?.view === "all" || (place?.view === "file" && listed(place.file?.key))) setState("view", place.view);
    if (state.view === "file" && place?.file && listed(place.file.key)) {
      restoring = { path: place.file.key, snapshot: snapshotOf(place.file) };
      setState("selected", place.file.key);
    } else if (state.view === "all" && place?.list && listed(place.list.key)) {
      setState("selected", place.list.key);
    }

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
    if (place !== undefined) restoreListPlace(place, listed);
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
  if (state.source === "commit") return commitMode.openTreeFile(path);
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
    // A save under way would otherwise be read back half-way.
    if (isSaving(file.path)) await flushSave();
    const pair = await ipc.readFilePair(repo.root, comparison, file);
    if (!isLatest(FILE_VIEW, token)) return;
    const reason = unshowable(pair);
    if (reason) {
      editor?.close();
      setState("placeholder", reason);
      return;
    }
    const session = sessionFor(file, pair);
    if (restoring?.path === file.path) session.snapshot = restoring.snapshot;
    restoring = null;
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
  const saves = saveCount();
  const pair = await ipc.readFilePair(repo.root, comparison, file);
  if (!isLatest(key, token) || unshowable(pair)) return;
  const document = documentOf(file, session, pair);
  if (document.editable) {
    // The editor may hold newer text than was read; the save's own change brings it back.
    if (isSaving(file.path) || saves !== saveCount()) return;
    session.pair = pair;
    if (target.shows(file.path, document)) {
      target.updateTexts(document.base, document.working);
      target.setCommitted(document.committed ?? null);
    } else {
      reopen(target, document);
    }
    return;
  }
  if (pair.base === session.pair.base && pair.working === session.pair.working && target.shows(file.path, document)) return;
  session.pair = pair;
  session.snapshot = null;
  target.reload(document);
}

/** Shows `document` in `target` in place of what it shows, keeping the cursor, scroll, undo history and focus. */
function reopen(target: DiffEditor, document: DiffDocument) {
  const focused = target.working?.hasFocus ?? false;
  target.open(document, state.mode, target.snapshot() ?? undefined);
  if (focused) target.focus();
  const file = fileAt(document.path);
  if (file) void attachLanguageServer(file, target);
}

// ------------------------------------------------------------------ editing

/**
 * Branches: the working text of `path` was edited in the checkout the
 * comparison reads. It is saved once typing pauses; a file outside the diff
 * turns into a diff at its first edit, and back once the edits are undone.
 */
function edited(path: string, text: string) {
  const checkout = state.comparison?.checkout;
  if (state.source === "commit" || !checkout) return;
  queueSave(checkout, path, text, (error) => fail(`Saving ${path}: ${String(error)}`));
  const file = fileAt(path);
  const target = editorFor(path);
  const session = sessions.get(path);
  if (file?.status !== "unchanged" || !target || !session) return;
  // Not from inside the editor's own update.
  queueMicrotask(() => {
    const texts = target.texts();
    if (!texts) return;
    const document = documentOf(file, session, texts);
    if (!target.shows(path, document)) reopen(target, document);
  });
}

/**
 * The Read-only popup, when open. It opens by itself the first time a
 * read-only diff is typed in, then next to the cursor (`anchor`); opened
 * from its status bar pill it shows above the pill.
 */
const [editInfo, setEditInfo] = createSignal<{ anchor: CursorRect | null } | null>(null);
type CursorRect = NonNullable<BlockedEdit["rect"]>;
export const editInfoOpen = () => editInfo() !== null;
export const editInfoAnchor = () => editInfo()?.anchor ?? null;
function openEditInfo(anchor: CursorRect | null) {
  pendingEdit = null;
  setEditInfo({ anchor });
}
export function setEditInfoOpen(open: boolean) {
  if (open) openEditInfo(null);
  else {
    pendingEdit = null;
    setEditInfo(null);
  }
}
let editInfoShown = false;

/** What was typed into the read-only diff that opened the popup: typed again once `makeEditable` made it editable. */
let pendingEdit: { path: string; at: NonNullable<BlockedEdit["at"]>; text: string } | null = null;

/** Types `typed` into its file's editor once that shows the file editable again (it is rebuilt after `makeEditable`). */
async function retype(typed: NonNullable<typeof pendingEdit>) {
  if (typed.text === "") return;
  for (let tries = 0; tries < 100; tries++) {
    if (editorFor(typed.path)?.typeAt(typed.at, typed.text)) return;
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
}

/** Whether `path` may have uncommitted edits to undo: a file of the checked-out branch or pull request being compared. */
const editsUndoable = (path: string) =>
  state.source !== "commit" &&
  !!state.comparison?.checkout &&
  (fileAt(path)?.status !== "deleted" || !!fileAt(path)?.uncommitted);

/** "Undo edits in selected lines": the right-clicked editor's selection goes back to the last commit. */
export function undoLineEdits() {
  const undo = state.menu?.undo;
  setState("menu", null);
  const target = undo && editorFor(undo.path);
  if (target?.undoEdits(false)) target.focus();
}

/**
 * Undoes every uncommitted edit to `path`. In an editor showing it that is
 * an edit like any other, which can be undone; otherwise (or for a file no
 * commit has, which is deleted) the checkout's file is put back, after asking.
 */
export async function undoFileEdits(path: string) {
  setState("menu", null);
  const target = editorFor(path);
  if (target?.undoEdits(true)) return target.focus();
  const checkout = state.comparison?.checkout;
  const branch = state.source === "pullRequest" ? `pull request !${state.pullRequest?.id}` : state.comparison?.head;
  if (!checkout || !branch) return;
  const sure = await confirm(
    `Undo every uncommitted edit to ${path}? It goes back to the last commit of ${branch}; a file no commit has is deleted. This cannot be undone.`,
    { title: "Undo edits", kind: "warning", okLabel: "Undo edits" },
  );
  if (!sure || state.comparison?.checkout !== checkout) return;
  try {
    await flushSave();
    await ipc.undoFileEdits(checkout, [path]);
    await refresh();
  } catch (error) {
    fail(`Undoing the edits to ${path}: ${String(error)}`);
  }
}

/**
 * What makes the diff editable: checking the branch or the pull request out
 * in a worktree of its own (again, after that failed), or throwing away
 * edits made to an earlier push of the pull request.
 */
export type EditAction = { kind: "worktree" } | { kind: "discard" };

/** Why the diff cannot be edited, and what would make it editable; null when it can (or edits do not apply). */
export function editBlocked(): { reason: string; action: EditAction | null } | null {
  const repo = state.repo;
  const comparison = state.comparison;
  if (state.source === "commit" || !repo || !comparison || comparison.checkout) return null;
  if (state.range !== null) {
    return { reason: "Only some commits are shown. Pick All changes in the commit picker to edit.", action: null };
  }
  if (state.source === "pullRequest") {
    const pullRequest = state.pullRequest;
    if (!pullRequest) return null;
    const worktree = prWorktreeOf(pullRequest);
    if (worktree?.edited && worktree.head !== pullRequest.sourceCommit) {
      return {
        reason: `Your edits in ${worktree.path} were made to an earlier push of this pull request (${worktree.head.slice(0, 8)}), so their lines no longer match it. Throw them away to edit the latest push.`,
        action: { kind: "discard" },
      };
    }
    return {
      reason: `Arnold could not check the pull request out in a git worktree of its own (.arnold/worktrees/pr-${pullRequest.id})${problemOf(checkoutKey(repo.root, `!${pullRequest.id}`))}`,
      action: { kind: "worktree" },
    };
  }
  return {
    reason: `Arnold could not check ${comparison.head} out in a git worktree of its own (.arnold/worktrees)${problemOf(checkoutKey(repo.root, comparison.head))}`,
    action: { kind: "worktree" },
  };
}

/** Why the last checkout into a worktree failed; `key` says of what (`checkoutKey`). Not tried again by itself. */
const [checkoutProblem, setCheckoutProblem] = createSignal<{ key: string; error: string } | null>(null);
const checkoutKey = (root: string, what: string) => `${root}\n${what}`;
const problemOf = (key: string) => (checkoutProblem()?.key === key ? `: ${checkoutProblem()!.error}` : ".");

/** The checkout into a worktree under way, which refreshes that come meanwhile wait for. */
let checkingOut: { key: string; done: Promise<boolean> } | null = null;

/**
 * Branches: makes sure the compared branch is checked out somewhere (the
 * opened folder, a linked worktree, or else a new worktree of its own in
 * `.arnold/worktrees`), so its files can be edited and the language server
 * runs on them. A remote branch is checked out as its local branch, which
 * is compared from then on. False when the review changed meanwhile.
 * A failure leaves the comparison read-only, with why (`editBlocked`).
 */
async function checkOutHead(repo: RepoInfo): Promise<boolean> {
  const head = state.head;
  const local = repo.branches.find((branch) => !branch.isRemote && branch.name === head);
  if (!head || head === state.base || local?.worktreePath) return true;
  const key = checkoutKey(repo.root, head);
  if (checkoutProblem()?.key === key) return true;
  if (checkingOut?.key !== key) {
    const done = (async () => {
      try {
        const checkout = await ipc.branchWorktree(repo.root, head);
        const branches = await ipc.listBranches(repo.root);
        if (state.repo !== repo) return false;
        setState("repo", "branches", branches);
        if (checkout.created) commitMode.notify(`Checked ${checkout.branch} out in ${relativeTo(repo.root, checkout.path) ?? checkout.path}`);
        if (state.head !== head) return false;
        if (checkout.branch !== head) {
          setState("head", checkout.branch);
          localStorage.setItem(`arnold.branches.${repo.root}`, JSON.stringify({ base: state.base, head: state.head }));
        }
        return true;
      } catch (error) {
        setCheckoutProblem({ key, error: String(error) });
        return state.repo === repo && state.head === head;
      }
    })();
    checkingOut = { key, done };
    void done.finally(() => {
      if (checkingOut?.done === done) checkingOut = null;
    });
  }
  return checkingOut.done;
}

/** The worktree of `pullRequest`, if it has one. */
const prWorktreeOf = (pullRequest: ReviewedPullRequest) =>
  state.prWorktree?.pullRequest === pullRequest.id ? state.prWorktree : null;

/** The pull request's worktree, when it is at the commit reviewed (so its files can be compared and edited). */
function prCheckout(): string | null {
  const pullRequest = state.pullRequest;
  if (state.source !== "pullRequest" || !pullRequest) return null;
  const worktree = prWorktreeOf(pullRequest);
  return worktree?.head === pullRequest.sourceCommit ? worktree.path : null;
}

/**
 * Reads (with `create`, makes) the worktree of the pull request under
 * review; one without edits moves to the pull request's latest push, and
 * with `discard` one with edits does too, throwing them away.
 */
async function loadPrWorktree(create: boolean, discard = false) {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  if (!repo || !pullRequest?.sourceCommit) return;
  try {
    const worktree = await ipc.pullRequestWorktree(repo.root, pullRequest.id, pullRequest.sourceCommit, create, discard);
    if (state.repo !== repo || state.pullRequest?.id !== pullRequest.id) return;
    setState("prWorktree", worktree && { ...worktree, pullRequest: pullRequest.id });
    if (create) setCheckoutProblem(null);
  } catch (error) {
    setCheckoutProblem({ key: checkoutKey(repo.root, `!${pullRequest.id}`), error: String(error) });
  }
}

/**
 * Makes the diff editable as `editBlocked` says, then types what was typed
 * while the Read-only popup was open (taken by the call, so call this
 * before closing the popup).
 */
export async function makeEditable() {
  const typed = pendingEdit;
  pendingEdit = null;
  await makeEditableAs(editBlocked()?.action);
  if (typed && !editBlocked()) await retype(typed);
}

async function makeEditableAs(action: EditAction | null | undefined) {
  if (!action) return;
  if (action.kind === "worktree" && state.source === "branches") {
    // Tried again on the next refresh.
    setCheckoutProblem(null);
    resetSessions();
    return refresh();
  }
  if (action.kind === "discard") {
    const sure = await confirm(
      "Throw away your edits to the earlier push of this pull request and edit the latest one? This cannot be undone.",
      { title: "Discard edits", kind: "warning", okLabel: "Discard edits" },
    );
    if (!sure) return;
  }
  setState({ busy: true, error: null });
  try {
    await loadPrWorktree(true, action.kind === "discard");
    resetSessions();
    await refresh();
  } finally {
    setState("busy", false);
  }
}

/** Commit mode on the checkout the comparison reads, to commit the edits made there. */
export async function commitEdits() {
  const checkout = state.comparison?.checkout;
  if (!checkout) return;
  await flushSave();
  await setSource("commit");
  await commitMode.setCheckout(checkout);
}

// --------------------------------------------------------------- suggestions

/** A file edited in a pull request's worktree, and its edits as suggestions. */
export interface SuggestionFile {
  path: string;
  /** The file in the pull request's latest push, and as edited. */
  committed: string;
  working: string;
  suggestions: Suggestion[];
}

/** What the Suggest change dialog offers to post. */
export interface SuggestionPlan {
  files: SuggestionFile[];
}

const [suggestionPlan, setSuggestionPlan] = createSignal<SuggestionPlan | null>(null);
export { suggestionPlan, setSuggestionPlan };

/**
 * A framed change's Suggest button: the change, as the suggestion the dialog
 * posts. `block` is the change as the editor frames it (its lines in the
 * working text); the editor's own text is used, edits not saved yet included.
 */
export async function suggestChange(path: string, block: LineBlock) {
  const target = editorFor(path);
  const committed = target?.committedText();
  const working = target?.texts()?.working;
  if (state.source !== "pullRequest" || !state.comparison?.checkout || committed == null || working === undefined) return;
  await flushSave();
  const suggestion = suggestionsFor(path, committed, working).find((candidate) =>
    candidate.block.toB > candidate.block.fromB
      ? block.fromB < candidate.block.toB && Math.max(block.toB, block.fromB + 1) > candidate.block.fromB
      : candidate.block.fromB === block.fromB,
  );
  if (!suggestion) {
    fail(`That change to ${path} cannot be suggested: the file is empty in the pull request.`);
    return;
  }
  setSuggestionPlan({ files: [{ path, committed, working, suggestions: [suggestion] }] });
}

/**
 * Posts `picked` (with their messages) as suggestion comments on the pull
 * request, one each, then puts the posted edits back in the worktree: they
 * now live on the pull request. Edits not posted stay. Resolves to an error
 * text when nothing could be posted.
 */
export async function postSuggestions(picked: { file: SuggestionFile; suggestion: Suggestion; message: string }[]): Promise<string | null> {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  const checkout = state.comparison?.checkout;
  if (!repo || !pullRequest || !checkout) return "No pull request is being edited.";
  if (state.comparison?.tipCommit !== pullRequest.sourceCommit) return "The pull request has a new push. Look at it before suggesting.";
  const posted = new Map<SuggestionFile, Suggestion[]>();
  let failure: string | null = null;
  for (const { file, suggestion, message } of picked) {
    try {
      const place = { path: file.path, lines: suggestionPlace(suggestion) };
      await ipc.createPullRequestThread(repo.root, pullRequest.remote, pullRequest.id, place, suggestionComment(suggestion, message));
      posted.set(file, [...(posted.get(file) ?? []), suggestion]);
    } catch (error) {
      failure = writeErrorText(error, "the suggestion");
      break;
    }
  }
  for (const [file, suggestions] of posted) {
    const text = withoutBlocks(file.committed, file.working, suggestions.map((suggestion) => suggestion.block));
    const target = editorFor(file.path);
    try {
      if (target && target.texts()?.working === file.working) target.replaceWorkingText(text);
      else await ipc.saveWorktreeFile(checkout, file.path, text);
    } catch (error) {
      failure ??= `Posted, but could not undo the edits to ${file.path}: ${String(error)}`;
    }
  }
  await flushSave();
  await loadThreads();
  if (!failure) return null;
  const count = [...posted.values()].reduce((sum, list) => sum + list.length, 0);
  if (count === 0) return failure;
  // Not offered again: what is left shows with its own Suggest buttons.
  setSuggestionPlan(null);
  fail(`${count === 1 ? "1 suggestion" : `${count} suggestions`} posted, then: ${failure}`);
  return null;
}

// ----------------------------------------------------------- all-files view

let revealSection: (path: string, offset?: number) => void = () => {};

/** How the all-files view scrolls a file into view (with `offset`, that far into it). */
export function onRevealSection(handler: (path: string, offset?: number) => void) {
  revealSection = handler;
}

// ----------------------------------------------------------------- places

/** A review was opened (or left): its place is restored once its files are read. */
let placePending = true;
/** The place restored into the file shown on its own, applied when it is read. */
let restoring: { path: string; snapshot: Snapshot } | null = null;

/** The review shown, as places keeps them apart; null while there is none. */
export function placeReview(): string | null {
  if (state.source === "commit") return state.checkout ? `commit:${state.checkout}` : null;
  if (state.source === "pullRequest") return state.pullRequest ? `pr:${state.pullRequest.remote}!${state.pullRequest.id}` : null;
  return state.base && state.head ? `branches:${state.base}...${state.head}` : null;
}

/**
 * The place kept for the review shown, taken once: later refreshes leave the
 * views alone. Its view only for the first review since the app started.
 */
export function takePlace(root: string): Place | null {
  placePending = false;
  const review = placeReview();
  const place = review ? placeIn(root, review) : null;
  if (!firstReview() && place) delete place.view;
  return place;
}

/** The view changed: noted for next time. */
export function noteView() {
  const root = state.repo?.root;
  const review = placeReview();
  if (root && review) notePlace(root, review, { view: state.view });
}

export const snapshotOf = (place: { anchor: number; head: number; scrollTop: number }): Snapshot => ({
  selection: EditorSelection.single(place.anchor, place.head),
  scrollTop: place.scrollTop,
});

/**
 * All-files view: scrolls back to where the list was, once its sections are
 * drawn, then lets moving save again. `listed` tells whether a key is still
 * in the list.
 */
export function restoreListPlace(place: Place | null, listed: (key: string | undefined) => boolean, reveal = revealSection) {
  const list = place?.list;
  if (state.view !== "all" || !list || !listed(list.key)) return placesReady();
  requestAnimationFrame(() => {
    reveal(list.key, list.offset);
    placesReady();
  });
}

/** The file shown on its own moved (or another was shown): noted for next time. */
function noteFilePlace() {
  const root = state.repo?.root;
  const review = placeReview();
  const snapshot = editor?.snapshot();
  const key =
    state.selected === null ? null : state.source === "commit" ? `${state.workSide ?? "file"}:${state.selected}` : state.selected;
  if (!root || !review || !snapshot || key === null) return;
  const { anchor, head } = snapshot.selection.main;
  notePlace(root, review, { view: "file", file: { key, anchor, head, scrollTop: snapshot.scrollTop } });
}

/** The all-files view was scrolled: noted for next time. */
export function noteListPlace(key: string, offset: number) {
  const root = state.repo?.root;
  const review = placeReview();
  if (root && review && state.view === "all") notePlace(root, review, { view: "all", list: { key, offset: Math.round(offset) } });
}

export async function setView(view: ViewMode) {
  if (state.source === "commit") return commitMode.setView(view);
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
  noteView();
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
    if (isSaving(path)) await flushSave();
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
    const target = new DiffEditor(host, editorCallbacks(() => path));
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
  commitMode.setSectionMode(mode);
  activeEditor()?.focus();
}

/** "Go to definition" in the context menu: the symbol that was right-clicked. */
export function goToDefinition() {
  setState("menu", null);
  if (menuTarget) lsp.goToDefinitionAt(menuTarget.view, menuTarget.position);
  menuTarget = null;
}

export const nextChange = () => stepChange(1);
export const previousChange = () => stepChange(-1);

/**
 * F7 / Shift+F7. The one-file view goes round its file; the all-files view
 * goes on to the next (or previous) expanded file once a file's changes run
 * out, and stops at the last one.
 */
async function stepChange(direction: 1 | -1) {
  if (state.view === "all" && state.source === "commit") return commitMode.stepChange(direction);
  if (state.view !== "all") {
    if (direction > 0) editor?.nextChange();
    else editor?.previousChange();
    return;
  }
  const from = activePath();
  const current = from && !state.collapsed[from] ? await showSection(from) : null;
  if (current?.stepChange(direction)) return current.focus();
  const files = state.comparison?.files ?? [];
  let index = files.findIndex((file) => file.path === from);
  if (index < 0) index = direction > 0 ? -1 : files.length;
  for (let next = index + direction; next >= 0 && next < files.length; next += direction) {
    const path = files[next].path;
    if (state.collapsed[path]) continue;
    const target = await showFileSection(path);
    if (!target) continue; // Binary or too large to show.
    revealSection(path);
    target.enterChange(direction);
    target.focus();
    return;
  }
  current?.focus();
}

// ----------------------------------------------------------- language server

/** Commit the language server was last set up for, with its repository and branch. */
let preparedKey: string | null = null;
/** Bumped for each setup, so that a superseded one stops quietly. */
let setupRun = 0;

/**
 * What the language server is set up for. A checkout the server runs in
 * as it is (the opened folder, or Commit mode's checkout) is the same
 * whatever its commit: its files on disk are what the server reads.
 */
const setupKey = (repo: RepoInfo, comparison: Comparison) => {
  const checkout = headCheckout(repo, comparison);
  return checkout
    ? inPlaceKey(checkout, comparison.head)
    : `${repo.root}\n${reviewedName(comparison)}\n${comparison.tipCommit}\ncopy`;
};

const inPlaceKey = (root: string, branch: string) => `${root}\n${branch}\nin place`;


/**
 * Where the reviewed branch is checked out (the opened folder or a linked
 * worktree), if it is: the language server then runs there, without a copy,
 * and sees edits. A pull request or any other branch gets a copy in
 * `.arnold/worktrees`.
 */
function headCheckout(repo: RepoInfo, comparison: Comparison): string | null {
  if (state.source === "pullRequest") {
    // Whatever commits are picked, while the worktree has the reviewed push.
    const worktree = state.pullRequest && prWorktreeOf(state.pullRequest);
    return worktree?.head === comparison.tipCommit ? worktree.path : null;
  }
  if (state.source !== "branches") return null;
  const branch = repo.branches.find((candidate) => candidate.name === comparison.head && !candidate.isRemote);
  return branch?.worktreePath ?? null;
}

/** The folder of the reviewed commit, once a server may run there. */
function serverRoot(): string | null {
  const setup = state.languageSetup;
  return setup && setup.phase !== "preparing" && setup.phase !== "failed" ? setup.path : null;
}

/**
 * Gets F# language features going for the reviewed commit: the opened
 * checkout (when it has the reviewed branch) or a copy of the commit is
 * prepared (restored, built where needed), then FsAutoComplete starts there
 * and loads the projects. The diff can be used all along; editors get
 * language features once the server runs.
 */
function setUpLanguageFeatures(repo: RepoInfo, comparison: Comparison) {
  const checkout = headCheckout(repo, comparison);
  return prepareLanguageFeatures(
    setupKey(repo, comparison),
    checkout ?? repo.root,
    reviewedName(comparison),
    comparison.tipCommit,
    checkout !== null,
  );
}

/**
 * Commit mode: language features for a checkout as it is on disk, edits
 * included. Nothing happens when they are already set up for it.
 */
export function setUpLanguageFeaturesIn(checkout: string, branch: string, commit: string) {
  const key = inPlaceKey(checkout, branch);
  if (key === preparedKey) return;
  void prepareLanguageFeatures(key, checkout, branch, commit, true);
}

async function prepareLanguageFeatures(key: string, root: string, name: string, commit: string, inPlace: boolean) {
  preparedKey = key;
  const run = ++setupRun;
  const current = () => run === setupRun;
  await lsp.stop();
  forEachShownEditor((target) => target.setLanguageSupport([]));
  backgroundSent.clear();
  setState("problems", reconcile({}));
  setSetupOutput([]);
  setState({
    lspStatus: null,
    languageSetup: {
      commit,
      phase: "preparing",
      path: null,
      inPlace,
      steps: [],
      problems: [],
      projects: {},
      currentProject: null,
    },
  });

  let prepared: { path: string | null };
  try {
    prepared = await ipc.prepareWorkspace(root, name, commit, inPlace, (event) => {
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
  // Without a working handshake the server never reports the projects loaded.
  client.initializing.catch((error: unknown) => {
    if (!current() || state.languageSetup?.phase !== "loading") return;
    finishStep("failed");
    addSetupProblem(`The language server could not start: ${error instanceof Error ? error.message : String(error)}`);
    setState("languageSetup", "phase", "failed");
  });
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

function addSetupProblem(text: string, details: string | null = null) {
  if (state.languageSetup) setState("languageSetup", "problems", (problems) => [...problems, { text, details }]);
}

/** A project path as the user knows it: relative to the prepared copy. */
export function projectName(project: string): string {
  const root = state.languageSetup?.path;
  return (root && relativeTo(root, project)) || project.split(/[\\/]/).pop() || project;
}

/** How many of the announced projects are loaded, failed, or still to do. */
export function projectCounts(setup: LanguageSetup) {
  const loads = Object.values(setup.projects);
  const loaded = loads.filter((load) => load === "loaded").length;
  const failed = loads.filter((load) => load === "failed").length;
  return { total: loads.length, loaded, failed, finished: loaded + failed };
}

function setProjectLoad(project: string, load: ProjectLoad) {
  const setup = state.languageSetup;
  if (!setup || !project) return;
  setState("languageSetup", "projects", project, load);
  if (load === "loading") setState("languageSetup", "currentProject", project);
  else if (setup.currentProject === project) setState("languageSetup", "currentProject", null);
  // The "Loading projects" step counts along.
  const index = setup.steps.findIndex((step) => step.state === "running");
  if (setup.phase === "loading" && index >= 0) {
    setState("languageSetup", "steps", index, "note", projectNote(setup));
  }
}

function projectNote(setup: LanguageSetup): string | null {
  const { total, loaded, failed, finished } = projectCounts(setup);
  if (total === 0) return null;
  if (setup.phase === "loading") return `${finished} of ${total}${failed ? `, ${failed} failed` : ""}`;
  return failed ? `${loaded} of ${total} loaded, ${failed} failed` : `${total} ${total === 1 ? "project" : "projects"}`;
}

function onServerEvent(event: ipc.LspEvent) {
  switch (event.kind) {
    case "status":
      setState("lspStatus", event.text);
      if (event.text) ipc.devTrace(`lsp status: ${event.text}`);
      break;
    case "workspaceLoaded":
      if (state.languageSetup?.phase === "loading") {
        setState("languageSetup", { phase: "ready", currentProject: null });
        finishStep("done", projectNote(state.languageSetup) ?? undefined);
        ipc.devTrace("language features ready");
      }
      break;
    case "projectLoading":
      setProjectLoad(event.project, "loading");
      break;
    case "projectLoaded":
      setProjectLoad(event.project, "loaded");
      break;
    case "projectFailed":
      setProjectLoad(event.project, "failed");
      addSetupProblem(`${projectName(event.project)} could not be loaded: ${event.message}`, event.details);
      ipc.devTrace(`project failed: ${projectName(event.project)}: ${event.message}`);
      break;
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
  if (state.view === "all" && state.source === "commit") {
    commitMode.forEachSection(action);
  } else if (state.view === "all") {
    for (const [path, target] of sectionEditors) {
      const file = fileAt(path);
      if (file) action(target, file);
    }
  } else {
    const file = state.source === "commit" ? commitMode.shownFile() : fileAt(state.selected);
    if (file && editor) action(editor, file);
  }
}

/** Connects a file being shown to its language server, once that is running. */
export async function attachLanguageServer(file: ChangedFile, target: DiffEditor) {
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
  if (state.source === "commit") {
    if (path !== state.selected) await commitMode.openTreeFile(path);
    return editor?.working ?? null;
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

// ---------------------------------------------------------------- discussions

/** (Re-)reads the discussions of the pull request under review and shows them in the open files. */
export async function loadThreads() {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  if (!repo || state.source !== "pullRequest" || !pullRequest) return;
  const key = pullRequestKey(pullRequest);
  try {
    const list = await ipc.pullRequestThreads(repo.root, pullRequest.remote, pullRequest.id);
    if (repo !== state.repo || pullRequestKey(state.pullRequest) !== key) return;
    setDiscussion({ key, threads: list });
    showThreadsInEditors();
  } catch (error) {
    // Tried again with the next check of the pull request.
    ipc.devTrace(`reading the discussions of !${pullRequest.id}: ${pullRequests.prErrorText(error)}`);
  }
}

function showThreadsInEditors() {
  if (editor && state.selected && state.view === "file") editor.setThreads(fileThreads(state.selected));
  for (const [path, target] of sectionEditors) target.setThreads(fileThreads(path));
}

/** New comments being written, by file (one per file). */
const drafts = new Map<string, NewThreadPlace>();

/**
 * Whether comments can be added to `path`: a file of the pull request's
 * diff, shown with all of its changes (the lines the host knows).
 */
function canComment(path: string): boolean {
  const pullRequest = state.pullRequest;
  return (
    state.source === "pullRequest" &&
    pullRequest !== null &&
    state.range === null &&
    isChanged(path) &&
    state.comparison?.headCommit === pullRequest.sourceCommit &&
    // Its lines are no longer the pull request's.
    !fileAt(path)?.uncommitted
  );
}

/** Right-click on a changed file of the dock: its menu, with commenting on the whole file. */
export function openFileMenu(path: string, x: number, y: number): boolean {
  if (editsUndoable(path)) {
    menuTarget = null;
    menuComment = null;
    const undo = { path, lines: false, file: !!fileAt(path)?.uncommitted };
    setState("menu", { x, y, definition: false, file: path, undo });
    return true;
  }
  if (state.source !== "pullRequest") return false;
  menuTarget = null;
  menuComment = canComment(path) ? { path, lines: null } : null;
  setState("menu", { x, y, definition: false, comment: menuComment && placeName(menuComment), file: path });
  return true;
}

/** What "Add to .gitignore" would act on for `path`; undefined in Pull request mode, which never writes to the repository's files. */
export function ignoreTarget(path: string, folder: boolean): NonNullable<State["menu"]>["ignore"] {
  if (state.source === "commit") return { path, folder, checkout: state.checkout };
  if (state.source === "branches") return { path, folder, checkout: state.comparison?.checkout ?? null };
  return undefined;
}

/** Right-click on a row of the explorer: a file's menu (see `openFileMenu`) and adding it to `.gitignore`. */
export function openTreeMenu(path: string, folder: boolean, x: number, y: number): boolean {
  const ignore = ignoreTarget(path, folder);
  if (!ignore) return false;
  if (!folder && state.source === "branches" && openFileMenu(path, x, y)) {
    setState("menu", "ignore", ignore);
  } else {
    menuTarget = null;
    menuComment = null;
    setState("menu", { x, y, definition: false, file: path, ignore });
  }
  const checkout = ignore.checkout;
  if (checkout) {
    void ipc.pathTracked(checkout, path).then(
      (tracked) => {
        if (state.menu?.ignore?.path === path && state.menu.ignore.checkout === checkout) setState("menu", "ignore", "tracked", tracked);
      },
      () => {},
    );
  }
  return true;
}

/**
 * "Add to .gitignore" in a context menu: a line ignoring just that file or
 * folder at the end of the checkout's root `.gitignore`. The checkout's
 * watcher then refreshes the lists, where it no longer shows if untracked.
 */
export async function addToGitignore() {
  const ignore = state.menu?.ignore;
  setState("menu", null);
  if (!ignore?.checkout) return;
  const what = ignore.folder ? `${ignore.path}/` : ignore.path;
  try {
    await flushSave();
    const result = await ipc.ignorePath(ignore.checkout, ignore.path, ignore.folder);
    const done = result.added ? `Added ${result.pattern} to .gitignore` : `.gitignore already has ${result.pattern}`;
    commitMode.notify(
      result.tracked
        ? `${done}, but git still tracks ${what}: .gitignore only keeps out untracked files (see Stop tracking)`
        : done,
    );
  } catch (error) {
    fail(`Adding ${what} to .gitignore: ${String(error)}`);
  }
}

/**
 * "Stop tracking…" in the explorer's menu: after asking, the file (or the
 * folder's files) leaves the index, a removal staged for the next commit,
 * and stays on disk.
 */
export async function stopTracking() {
  const ignore = state.menu?.ignore;
  setState("menu", null);
  if (!ignore?.checkout || !ignore.tracked) return;
  const what = ignore.folder ? `${ignore.path}/` : ignore.path;
  const sure = await confirm(
    `Stop tracking ${what}? ${ignore.folder ? "Its files stay" : "It stays"} on disk; the removal from the repository is staged, for the next commit. Add ${ignore.folder ? "the folder" : "it"} to .gitignore as well to keep ${ignore.folder ? "them" : "it"} out of the changes.`,
    { title: "Stop tracking", kind: "warning", okLabel: "Stop tracking" },
  );
  if (!sure) return;
  try {
    await flushSave();
    const count = await ipc.untrackPath(ignore.checkout, ignore.path);
    commitMode.notify(
      ignore.folder
        ? `Stopped tracking ${count} ${count === 1 ? "file" : "files"} in ${what}: the removal is staged`
        : `Stopped tracking ${what}: the removal is staged`,
    );
  } catch (error) {
    fail(`Stopping tracking ${what}: ${String(error)}`);
  }
}

/**
 * "Add comment" in a context menu: opens a card to write it, under the
 * lines (or at the top of the file), showing the file first if need be.
 */
export async function addComment() {
  setState("menu", null);
  const place = menuComment;
  menuComment = null;
  if (!place) return;
  drafts.set(place.path, place);
  // An editor made from now on shows the card by itself.
  if (state.view === "all") {
    const target = await showFileSection(place.path);
    revealSection(place.path);
    target?.setDraft(place);
  } else if (state.selected !== place.path) {
    await selectFile(place.path);
  } else {
    editor?.setDraft(place);
  }
}

/** Text for a failed attempt to write to the pull request; `what` was sent ("the comment"). */
function writeErrorText(error: unknown, what = "the comment"): string {
  if (ipc.isPrError(error) && error.kind === "tokenRejected") {
    return `Azure DevOps did not accept ${what}${error.message ? ` (${error.message})` : ""}. Writing to pull requests needs a token with the Code (Read & write) scope.`;
  }
  return pullRequests.prErrorText(error);
}

async function createThread(place: NewThreadPlace | null, text: string) {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  if (!repo || !pullRequest) throw "No pull request is open.";
  try {
    await ipc.createPullRequestThread(repo.root, pullRequest.remote, pullRequest.id, place, text);
  } catch (error) {
    throw writeErrorText(error);
  }
  if (place && drafts.get(place.path) === place) {
    drafts.delete(place.path);
    editorFor(place.path)?.setDraft(null);
  }
  await loadThreads();
}

async function reply(thread: PrThread, text: string) {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  if (!repo || !pullRequest) throw "No pull request is open.";
  try {
    await ipc.replyToPullRequestThread(repo.root, pullRequest.remote, pullRequest.id, thread.id, thread.comments[0].id, text);
  } catch (error) {
    throw writeErrorText(error);
  }
  await loadThreads();
}

/**
 * Deletes a comment of the token's owner from the pull request, after
 * asking. Deleting the first comment of a suggestion removes the suggestion.
 */
async function deleteComment(thread: PrThread, comment: PrComment) {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  if (!repo || !pullRequest) return;
  const suggestion = comment === thread.comments[0] && isSuggestion(thread);
  const what = suggestion ? "suggestion" : "comment";
  const replies = comment === thread.comments[0] ? thread.comments.length - 1 : 0;
  const sure = await confirm(
    `Delete this ${what} from pull request !${pullRequest.id}?${replies > 0 ? ` The ${replies === 1 ? "reply" : `${replies} replies`} to it stay.` : ""} This cannot be undone.`,
    { title: `Delete ${what}`, kind: "warning", okLabel: "Delete" },
  );
  if (!sure) return;
  try {
    await ipc.deletePullRequestComment(repo.root, pullRequest.remote, pullRequest.id, thread.id, comment.id);
  } catch (error) {
    fail(writeErrorText(error, `the deletion of the ${what}`));
  }
  await loadThreads();
}

function cancelDraft(place: NewThreadPlace) {
  if (drafts.get(place.path) !== place) return;
  drafts.delete(place.path);
  editorFor(place.path)?.setDraft(null);
}

/** Starts a discussion about the pull request as a whole. */
export const createGeneralThread = (text: string) => createThread(null, text);

/** Shows the file a thread is about, scrolled to the thread. */
export async function revealThread(thread: PrThread) {
  const path = thread.path;
  if (!path || !isChanged(path)) return;
  if (state.range !== null) await setRange(null);
  if (state.view === "all") {
    const target = await showFileSection(path);
    revealSection(path);
    // After the section has scrolled in, so the editor's own scrolling wins.
    requestAnimationFrame(() => target?.revealThread(thread.id));
    return;
  }
  if (state.selected !== path) await selectFile(path);
  editor?.revealThread(thread.id);
}

// ------------------------------------------------------ voting and completing

/** How the owner of the token voted on the pull request under review. */
export const yourVote = (): Vote => state.pullRequest?.reviewers.find((reviewer) => reviewer.you)?.vote ?? "none";

/**
 * Sends a vote or a change of the pull request under review to the host and
 * shows the pull request as it is afterwards. Throws the error text.
 */
async function changePullRequest(
  action: NonNullable<State["updatingPullRequest"]>,
  what: string,
  send: (root: string, pullRequest: ReviewedPullRequest) => Promise<PullRequest>,
) {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  if (!repo || !pullRequest || state.updatingPullRequest !== null) return;
  setState({ updatingPullRequest: action, error: null });
  try {
    const fresh = await send(repo.root, pullRequest);
    if (state.repo === repo && pullRequestKey(state.pullRequest) === pullRequestKey(pullRequest)) {
      // A new push has to be fetched first, which checking does.
      const { sourceCommit, targetCommit } = state.pullRequest!;
      if (fresh.sourceCommit !== sourceCommit || fresh.targetCommit !== targetCommit) void checkPullRequest();
      setState("pullRequest", { ...fresh, sourceCommit, targetCommit, remote: pullRequest.remote });
    }
  } catch (error) {
    throw writeErrorText(error, what);
  } finally {
    setState("updatingPullRequest", null);
  }
  // The list's votes and states.
  void pullRequests.loadPullRequests();
}

/** Votes on the pull request under review; `none` resets the vote. */
export function votePullRequest(vote: Vote) {
  return changePullRequest("vote", "the vote", (root, pullRequest) =>
    ipc.voteOnPullRequest(root, pullRequest.remote, pullRequest.id, vote),
  ).catch(fail);
}

const CHANGE_NAMES: Record<PrChange["kind"], string> = {
  complete: "completing the pull request",
  abandon: "abandoning the pull request",
  reactivate: "reactivating the pull request",
  draft: "the change",
};

/** Completes, abandons, reactivates, drafts or publishes the pull request under review. Throws the error text. */
export async function updatePullRequest(change: PrChange) {
  await changePullRequest(change.kind, CHANGE_NAMES[change.kind], (root, pullRequest) =>
    ipc.updatePullRequest(root, pullRequest.remote, pullRequest.id, change),
  );
  // The host merges after answering; its outcome shows with the next check.
  if (change.kind === "complete") window.setTimeout(() => void checkPullRequest(), 3000);
}

// ------------------------------------------------------------------ watching

/** How often the open pull request is checked for new pushes while Arnold is shown. */
const PULL_REQUEST_POLL = 60_000;
/** Coming back to Arnold checks it too, unless it was checked this recently. */
const PULL_REQUEST_ON_FOCUS = 15_000;

let refreshTimer: number | undefined;
let configTimer: number | undefined;
let pullRequestCheckedAt = 0;
let checkingPullRequest = false;

let checkoutTimer: number | undefined;

export async function startWatching() {
  await commitMode.startWatching();
  // The files of the checked-out branch being compared: edits made in
  // Arnold, an editor or an agent show up in the diff.
  await ipc.onCheckoutChanged(() => {
    if (state.source === "commit" || !state.comparison?.checkout) return;
    window.clearTimeout(checkoutTimer);
    checkoutTimer = window.setTimeout(() => void refresh(), 150);
  });
  // The backend watches the repository's branches: a commit, fetch or new
  // branch updates the branch pickers and the diff.
  await ipc.onRefsChanged(() => {
    window.clearTimeout(refreshTimer);
    refreshTimer = window.setTimeout(() => void refsChanged(), 150);
  });
  // A remote added or removed (`git remote add`) changes whether pull
  // requests are offered; a push that sets an upstream changes Commit mode's
  // push target.
  await ipc.onConfigChanged(() => {
    window.clearTimeout(configTimer);
    configTimer = window.setTimeout(() => void configChanged(), 150);
  });
  // Pushes to a pull request are only known to the host, so it is asked.
  window.setInterval(() => {
    if (document.visibilityState === "visible") void checkPullRequest();
  }, PULL_REQUEST_POLL);
  window.addEventListener("focus", () => {
    if (Date.now() - pullRequestCheckedAt > PULL_REQUEST_ON_FOCUS) void checkPullRequest();
  });
}

async function configChanged() {
  const repo = state.repo;
  if (!repo) return;
  await pullRequests.refreshRemotes(repo.root);
  if (state.repo !== repo) return;
  ipc.devTrace(`config changed: pull requests from ${pullRequests.prs.remotes.map((remote) => remote.remote).join(", ") || "no remote"}`);
  if (state.source === "pullRequest" && !state.pullRequest) {
    if (pullRequests.prs.remotes.length === 0) await setSource("branches", false);
    else if (pullRequests.prs.list === null) void pullRequests.loadPullRequests();
  }
  if (state.source === "commit") await commitMode.refreshWork();
}

async function refsChanged() {
  const repo = state.repo;
  if (repo) {
    try {
      const branches = await ipc.listBranches(repo.root);
      if (state.repo === repo) setState("repo", "branches", reconcile(branches, { key: "name" }));
    } catch (error) {
      fail(error);
    }
  }
  // A commit made elsewhere empties the staged list.
  if (state.source === "commit") await commitMode.refreshWork();
  else await refresh();
}

/** Asks the host whether the pull request under review has new pushes, and shows them. */
export async function checkPullRequest() {
  const repo = state.repo;
  const pullRequest = state.pullRequest;
  if (!repo || state.source !== "pullRequest" || !pullRequest || state.openingPullRequest !== null) return;
  if (checkingPullRequest) return;
  checkingPullRequest = true;
  pullRequestCheckedAt = Date.now();
  try {
    const fresh = await ipc.openPullRequest(repo.root, pullRequest.remote, pullRequest.id);
    if (state.repo !== repo || pullRequestKey(state.pullRequest) !== pullRequestKey(pullRequest)) return;
    const moved = fresh.sourceCommit !== pullRequest.sourceCommit || fresh.targetCommit !== pullRequest.targetCommit;
    setState("pullRequest", { ...fresh, remote: pullRequest.remote });
    if (moved) {
      // A worktree without edits follows the new push.
      await loadPrWorktree(false);
      await refresh();
    }
    await loadThreads();
  } catch (error) {
    // Asked again on the next round; opening the list shows a lasting problem.
    ipc.devTrace(`checking pull request !${pullRequest.id}: ${pullRequests.prErrorText(error)}`);
  } finally {
    checkingPullRequest = false;
  }
}

