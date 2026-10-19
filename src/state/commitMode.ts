// Commit mode: the uncommitted changes of a checkout, in two lists like the
// commit dialog of Git Extensions. A file picked in Unstaged shows index →
// working tree, editable; picked in Staged it shows HEAD → index. The
// all-files view (the default) shows every file of both lists at once, each
// as it shows when picked. S stages, U unstages and R resets the selected
// lines (or, in the lists, files), and committing leads on to reviewing the
// branch or creating a pull request.

import { confirm } from "@tauri-apps/plugin-dialog";
import { createSignal } from "solid-js";
import { reconcile } from "solid-js/store";
import type { SectionEntry } from "../components/SectionList";
import { type DiffDocument, type DiffMode, DiffEditor, type LineAction, type MoveDirection, type Snapshot } from "../editor/diffEditor";
import { carryOver, dropSelectedLines, type LineSelection, resetEdits, takeSelectedLines } from "../editor/resetLines";
import * as ipc from "../ipc/commands";
import type { BranchSync, ChangedFile, FilePair, WorkingSide } from "../ipc/commands";
import { dropPendingSave, flushSave, hasPendingSave, queueSave, saved } from "./autosave";
import { comparePaths } from "./pathOrder";
import {
  attachLanguageServer,
  editorCallbacks,
  fileEditor,
  openPullRequest,
  restoreListPlace,
  setAllFiles,
  setBranches,
  setState,
  setUpLanguageFeaturesIn,
  noteView,
  snapshotOf,
  state,
  takePlace,
  type ViewMode,
} from "./review";
import { holdPlaces } from "./places";

/** How long a notice stays in the status bar. */
const NOTICE_TIME = 5000;

export const listOf = (side: WorkingSide): readonly ChangedFile[] =>
  (side === "unstaged" ? state.work?.unstaged : state.work?.staged) ?? [];

const entryOf = (side: WorkingSide, path: string | null) => listOf(side).find((file) => file.path === path);

/** A file of the explorer without uncommitted changes: shown on its own. */
const treeFile = (path: string): ChangedFile => ({
  path,
  oldPath: null,
  status: "unchanged",
  additions: null,
  deletions: null,
  binary: false,
});

/** The file shown in the editor, as listed (or as a file of the explorer). */
export function shownFile(): ChangedFile | null {
  const path = state.selected;
  if (path === null) return null;
  return state.workSide ? (entryOf(state.workSide, path) ?? null) : treeFile(path);
}

let noticeTimer: number | undefined;

/** Shows `text` in the status bar for a while. */
export function notify(text: string) {
  window.clearTimeout(noticeTimer);
  setState("notice", text);
  noticeTimer = window.setTimeout(() => setState("notice", null), NOTICE_TIME);
}

const fail = (what: string) => (error: unknown) => setState("error", `${what}: ${String(error)}`);

// ------------------------------------------------------------ checkouts

const checkoutKey = (root: string) => `arnold.checkout.${root}`;
const samePath = (a: string, b: string) => a.replace(/[\\/]+$/, "") === b.replace(/[\\/]+$/, "");

/** Whether the checkout at `root` has anything to commit. */
export async function hasUncommitted(root: string): Promise<boolean> {
  try {
    const work = await ipc.workingStatus(root);
    if (work.unstaged.length + work.staged.length > 0) return true;
  } catch {
    // Not able to tell: reviewing goes ahead as usual.
  }
  void ipc.stopWatchingCheckout();
  return false;
}

/** The checkout Commit mode opens: the one used last, else the opened folder. */
function commitCheckout(repo: NonNullable<typeof state.repo>): string {
  const remembered = localStorage.getItem(checkoutKey(repo.root));
  const known = (path: string) =>
    samePath(path, repo.root) || repo.branches.some((branch) => branch.worktreePath && samePath(branch.worktreePath, path));
  return remembered && known(remembered) ? remembered : repo.root;
}

/** Switches to Commit mode, in the checkout used last (else the opened folder). */
export async function enter() {
  const repo = state.repo;
  if (!repo) return;
  await openCheckout(commitCheckout(repo));
}

// Outside Commit mode: how many files the checkout it opens has uncommitted
// changes in, for the count on the Commit mode button.
const [countElsewhere, setCountElsewhere] = createSignal<{ root: string; count: number } | null>(null);
let countTicket = 0;

/** Files with uncommitted changes, staged or not, in the checkout Commit mode works in. */
export function uncommittedCount(): number {
  if (state.source === "commit") {
    const work = state.work;
    return work ? new Set([...work.unstaged, ...work.staged].map((file) => file.path)).size : 0;
  }
  const counted = countElsewhere();
  return counted && counted.root === state.repo?.root ? counted.count : 0;
}

/** Counts again outside Commit mode (which has its own lists): after a refresh, a commit, on focus. */
export async function countUncommitted() {
  const repo = state.repo;
  if (!repo || state.source === "commit") return;
  const ticket = ++countTicket;
  try {
    const count = await ipc.uncommittedCount(commitCheckout(repo));
    if (ticket === countTicket) setCountElsewhere({ root: repo.root, count });
  } catch {
    // Not able to tell: no count rather than a wrong one.
    if (ticket === countTicket) setCountElsewhere(null);
  }
}

/** Lists the uncommitted changes of another checkout of the repository. */
export async function setCheckout(path: string) {
  const repo = state.repo;
  if (!repo || (state.checkout !== null && samePath(path, state.checkout))) return;
  localStorage.setItem(checkoutKey(repo.root), path);
  await openCheckout(path);
}

async function openCheckout(path: string) {
  await flushSave();
  // Where the user was in this checkout is restored once its lists are read.
  holdPlaces();
  placePending = true;
  snapshots.clear();
  shownKey = null;
  shownAlone = null;
  shownDiff = null;
  fileEditor()?.close();
  for (const key of [...sectionEditors.keys()]) hideSection(key);
  sectionSnapshotsClear();
  setAllFiles([]);
  setMessage(localStorage.getItem(messageKey(path)) ?? "");
  // One file at a time, without changing the remembered choice for reviewing.
  setState({
    checkout: path,
    work: null,
    workSide: null,
    selected: null,
    view: rememberedView(),
    menu: null,
    placeholder: "Reading the uncommitted changes…",
  });
  await refreshWork();
}

/** Leaves Commit mode: edits are saved, the checkout no longer watched. */
export function leave() {
  void flushSave();
  snapshots.clear();
  shownKey = null;
  shownAlone = null;
  shownDiff = null;
  fileEditor()?.close();
  void ipc.stopWatchingCheckout();
  setState({ work: null, workSide: null, notice: null });
}

// ------------------------------------------------------------ reading

let refreshing: Promise<void> | null = null;
let readAgain = false;

/** Reads the lists again and brings the file shown up to date; calls during a read make it read once more. */
export function refreshWork(): Promise<void> {
  if (refreshing) {
    readAgain = true;
    return refreshing;
  }
  refreshing = (async () => {
    do {
      readAgain = false;
      await readWork();
    } while (readAgain);
  })().finally(() => (refreshing = null));
  return refreshing;
}

async function readWork() {
  const repo = state.repo;
  const checkout = state.checkout;
  if (!repo || !checkout || state.source !== "commit") return;
  try {
    const [work, files] = await Promise.all([ipc.workingStatus(checkout), ipc.worktreeFiles(checkout)]);
    if (state.repo !== repo || state.checkout !== checkout || state.source !== "commit") return;
    work.unstaged.sort((a, b) => comparePaths(a.path, b.path));
    work.staged.sort((a, b) => comparePaths(a.path, b.path));
    // Where the selected file was, before the lists change under it.
    const side = state.workSide;
    const index = side ? listOf(side).findIndex((file) => file.path === state.selected) : -1;
    setState("work", reconcile(work, { key: "path" }));
    setAllFiles(files);
    if (work.head) setUpLanguageFeaturesIn(checkout, work.branch ?? "HEAD", work.head);
    const place = placePending ? (placePending = false, takePlace(repo.root)) : undefined;
    const known = (key: string | undefined) => {
      if (key === undefined) return false;
      const path = pathOfKey(key);
      return key.startsWith("file:") ? files.includes(path) : !!entryOf(sideOfKey(key), path);
    };
    // Only the lists are in the all-files view.
    const listed = (key: string | undefined) => key !== undefined && !key.startsWith("file:") && known(key);
    // Nothing is shown yet, so the view can just be set.
    // (The file on its own only while that file is still there.)
    if (place?.view === "all" || (place?.view === "file" && known(place.file?.key))) setState("view", place.view);
    if (state.view === "file" && place?.file && known(place.file.key)) {
      const key = place.file.key;
      snapshots.set(key, snapshotOf(place.file));
      setState({ selected: pathOfKey(key), workSide: key.startsWith("file:") ? null : sideOfKey(key) });
    } else if (state.view === "all" && place?.list && listed(place.list.key)) {
      setState({ selected: pathOfKey(place.list.key), workSide: sideOfKey(place.list.key) });
    }
    await keepSelection(index);
    if (place !== undefined) restoreListPlace(place, listed, (key, offset) => revealSection?.(key, offset));
  } catch (error) {
    fail("Reading the uncommitted changes")(error);
  }
}

/**
 * After the lists changed: the file shown stays if it is still listed; a
 * file gone from its list (all staged, say) gives way to the one now at its
 * place, as in Git Extensions, else to the same file in the other list.
 * The all-files view brings the files it shows up to date instead.
 */
async function keepSelection(formerIndex: number) {
  const side = state.workSide;
  const path = state.selected;
  const all = state.view === "all";
  const pick = (to: WorkingSide, file: string) =>
    all ? (setState({ selected: file, workSide: to }), refreshSections()) : selectWorkFile(to, file);
  if (path !== null && (side === null || entryOf(side, path))) return all ? refreshSections() : show();
  if (side !== null && path !== null) {
    const list = listOf(side);
    const next = list[Math.min(Math.max(formerIndex, 0), list.length - 1)];
    if (next) return pick(side, next.path);
    const other: WorkingSide = side === "unstaged" ? "staged" : "unstaged";
    if (entryOf(other, path)) return pick(other, path);
  }
  const first: [WorkingSide, ChangedFile | undefined][] = [
    ["unstaged", listOf("unstaged")[0]],
    ["staged", listOf("staged")[0]],
  ];
  const [firstSide, firstFile] = first.find(([, file]) => file) ?? ["unstaged", undefined];
  if (firstFile) return pick(firstSide, firstFile.path);
  setState({ selected: null, workSide: null });
  return all ? refreshSections() : show();
}

/** A file clicked in Unstaged or Staged: shown on its own, leaving the all-files view (not for next time). */
export async function openWorkFile(side: WorkingSide, path: string) {
  if (state.view !== "all") return selectWorkFile(side, path);
  setState({ selected: path, workSide: side, menu: null });
  await setView("file", false);
}

/** Picks a file in Unstaged or Staged (arrow keys, a menu); the all-files view scrolls to it. */
export async function selectWorkFile(side: WorkingSide, path: string) {
  if (state.view === "all") {
    setState({ selected: path, workSide: side, menu: null });
    revealSection?.(keyOf(side, path));
    return;
  }
  if (side === state.workSide && path === state.selected) return;
  stash();
  setState({ selected: path, workSide: side, menu: null });
  await show();
}

/**
 * A file of the explorer, or the target of go-to-definition: the whole file
 * as it is on disk, editable, against the last commit (whatever is staged of
 * it). A file without changes is shown on its own, and switches between that
 * and the diff as edits give it changes or take them all back.
 */
export async function openTreeFile(path: string) {
  // The all-files view lists Unstaged and Staged: a file of the explorer is shown on its own.
  if (state.view === "all") {
    setState({ selected: path, workSide: null, menu: null });
    return setView("file", false);
  }
  if (state.workSide === null && state.selected === path) return;
  stash();
  setState({ selected: path, workSide: null, menu: null });
  await show();
}

/** Where the editor was in each file, by side and path. */
const snapshots = new Map<string, Snapshot>();
/** Side and path of what the editor shows. */
let shownKey: string | null = null;
/** A file of the explorer shown on its own because it has no changes: its first edit brings in the diff. */
let shownAlone: string | null = null;
/** A file of the explorer shown against the last commit: once edits take all its changes back, it is shown on its own. */
let shownDiff: string | null = null;
let showToken = 0;

/** Key of a file in the all-files view and of where the editor was in it. */
export const keyOf = (side: WorkingSide | null, path: string) => `${side ?? "file"}:${path}`;

function stash() {
  const snapshot = fileEditor()?.snapshot();
  if (shownKey && snapshot) snapshots.set(shownKey, snapshot);
}

function documentOf(file: ChangedFile, side: WorkingSide | null, pair: FilePair): DiffDocument {
  const base = pair.base ?? "";
  const working = pair.working ?? "";
  if (side === null) {
    // The explorer: the last commit → the working tree, as Unstaged shows it
    // when nothing is staged, and the whole file. New, deleted and unchanged
    // files show on their own. Lines are staged from Unstaged, which knows
    // the index.
    const added = pair.base === null;
    const deleted = pair.working === null;
    return {
      path: file.path,
      base,
      working: deleted ? base : working,
      plain: added || deleted || base === working,
      deleted,
      editable: !deleted,
      lineActions: [],
      collapseUnchanged: false,
      moves: { toWorking: "Revert this change to the last commit" },
    };
  }
  const deleted = file.status === "deleted";
  return {
    path: file.path,
    base,
    // A deleted file shows what it was; a new one is shown whole.
    working: deleted ? base : working,
    plain: deleted || file.status === "added",
    deleted,
    editable: side === "unstaged" && !deleted,
    lineActions: side === "unstaged" ? ["stage", "reset"] : ["unstage", "reset"],
    // Left is the index (unstaged) or the last commit (staged): a change goes
    // into the index, or the side on the right takes the left one's text.
    moves:
      side === "unstaged"
        ? { toBase: "Stage this change", toWorking: "Reset this change to the index" }
        : { toWorking: "Unstage this change" },
  };
}

/** Shows the selected file, or brings it up to date when the editor already shows it. */
async function show() {
  if (state.view === "all") return refreshSections();
  const editor = fileEditor();
  const checkout = state.checkout;
  const file = shownFile();
  const side = state.workSide;
  if (!editor || !checkout || !file) {
    editor?.close();
    shownKey = null;
    shownAlone = null;
    shownDiff = null;
    setState(
      "placeholder",
      state.work && state.work.unstaged.length + state.work.staged.length === 0
        ? "No uncommitted changes. Review the branch, push it or create its pull request from the toolbar."
        : "Pick a file to see its changes.",
    );
    return;
  }
  const token = ++showToken;
  try {
    // A save under way would otherwise be read back half-way.
    await saved();
    const pair = await ipc.readWorkingPair(checkout, file, side ?? "committed");
    if (token !== showToken) return;
    const reason = pair.binary
      ? "Binary file."
      : pair.tooLarge
        ? "This file is too large to show."
        : side === null && pair.working === null && pair.base === null
          ? "This file has been deleted from the working tree."
          : null;
    if (reason) {
      editor.close();
      shownKey = null;
      shownAlone = null;
      shownDiff = null;
      setState("placeholder", reason);
      return;
    }
    const key = keyOf(side, file.path);
    const same = key === shownKey;
    // Edits not saved yet stay, and decide whether a file of the explorer has changes.
    const unsaved = same && hasPendingSave(file.path) ? editor.texts()?.working : undefined;
    const document = documentOf(file, side, side === null && unsaved !== undefined ? { ...pair, working: unsaved } : pair);
    const unchanged = side === null && !!document.plain && !document.deleted && pair.base !== null;
    const working = unsaved ?? document.working;
    if (same && editor.shows(file.path, document)) {
      // The other side follows the index.
      editor.updateTexts(document.base, working);
      return;
    }
    // Shown differently now (changed on disk): where the editor was stays.
    const focused = same && !!editor.working?.hasFocus;
    if (same) stash();
    shownKey = key;
    shownAlone = document.plain && unchanged ? file.path : null;
    shownDiff = side === null && !document.plain ? file.path : null;
    setState("placeholder", null);
    editor.open({ ...document, working }, state.mode, snapshots.get(key));
    if (focused) editor.focus();
    void attachLanguageServer(file, editor);
  } catch (error) {
    fail(`Reading ${file.path}`)(error);
  }
}

// ------------------------------------------------------------ editing

/** The working text of `path` was edited: it is saved once typing pauses. */
export function edited(path: string, text: string) {
  const checkout = state.checkout;
  if (!checkout || state.workSide === "staged") return;
  // Not from inside the editor's own update.
  if (shownAlone === path) queueMicrotask(() => switchView(path, false));
  else if (shownDiff === path && text === fileEditor()?.texts()?.base) queueMicrotask(() => switchView(path, true));
  queueSave(checkout, path, text, fail(`Saving ${path}`));
}

/**
 * A file of the explorer has its first change (`alone` false: from now on it
 * is shown against the last commit), or edits took all its changes back
 * (`alone`: shown on its own). The cursor, scroll position and undo history stay.
 */
function switchView(path: string, alone: boolean) {
  const editor = fileEditor();
  const texts = editor?.texts();
  if (!editor || !texts || state.workSide !== null || state.selected !== path) return;
  if ((alone ? shownDiff : shownAlone) !== path || (texts.working === texts.base) !== alone) return;
  shownAlone = alone ? path : null;
  shownDiff = alone ? null : path;
  const file = treeFile(path);
  const pair: FilePair = { base: texts.base, working: texts.working, binary: false, tooLarge: false };
  const snapshot = editor.snapshot() ?? undefined;
  editor.open({ ...documentOf(file, null, pair), plain: alone }, state.mode, snapshot);
  editor.focus();
  void attachLanguageServer(file, editor);
}


// ------------------------------------------------------------ staging

/**
 * A change's move button. Unstaged: into the index (stage), or the working
 * tree takes the index's text (reset). Staged: the index takes the last
 * commit's text (unstage). All files: the working tree takes the last
 * commit's text.
 */
export function moveChunk(direction: MoveDirection, lines: LineSelection) {
  const side = state.workSide;
  if (direction === "toBase") return side === "unstaged" ? lineAction("stage", lines) : undefined;
  return lineAction(side === "staged" ? "unstage" : "reset", lines);
}

/**
 * S, U or R on the selected lines of the file shown (the cursor's line when
 * nothing is selected), or on `lines`. A deleted file is staged or reset as
 * a whole. In a file of the explorer only resetting (to the last commit) applies.
 */
export async function lineAction(action: LineAction, lines?: LineSelection) {
  const editor = currentEditor();
  const file = shownFile();
  const side = state.workSide;
  const checkout = state.checkout;
  if (!editor || !file || !checkout || (side === null && action !== "reset")) return;
  if (file.status === "deleted" && side) return fileAction(action, side, file);
  const selection = lines ?? editor.selectedLines();
  const texts = editor.texts();
  if (!selection || !texts) return;
  const none = () => notify(`The selected lines have no ${side === "staged" ? "staged " : ""}changes to ${action}.`);
  try {
    if (side === "unstaged" && action === "stage") {
      await flushSave();
      const index = takeSelectedLines(texts.base, texts.working, selection);
      if (index === null) return none();
      await ipc.setIndexText(checkout, file.path, index);
    } else if (side !== "staged" && action === "reset") {
      // An edit like any other: it can be undone, and is saved.
      const edits = resetEdits(texts.base, texts.working, selection);
      if (edits.length === 0) return none();
      editor.applyWorkingEdits(edits);
      notify("Reset the selected lines. Undo brings them back.");
      return;
    } else if (side === "staged") {
      const index = dropSelectedLines(texts.base, texts.working, selection);
      if (index === null) return none();
      if (action === "reset") {
        // The lines go back in the working tree too, as `git apply --reverse --index` would.
        await flushSave();
        const disk = (await ipc.readWorkingPair(checkout, file, "unstaged")).working ?? "";
        const reset = carryOver(texts.working, index, disk);
        if (reset === null) {
          return notify("Those lines have been changed again since they were staged. Unstage them (U) and reset them in Unstaged.");
        }
        const sure = await confirm(`Reset the selected lines of ${file.path}? They go back to the last commit, in the index and in the working tree.`, {
          title: "Reset lines",
          kind: "warning",
          okLabel: "Reset",
        });
        if (!sure) return;
        await setIndexText(checkout, file, index);
        await ipc.saveWorktreeFile(checkout, file.path, reset);
      } else {
        await setIndexText(checkout, file, index);
      }
    } else {
      return;
    }
  } catch (error) {
    fail(`Could not ${action} the lines`)(error);
  }
  await refreshWork();
}

/** Unstaging every line of a new file takes it out of the index again. */
function setIndexText(checkout: string, file: ChangedFile, text: string) {
  return file.status === "added" && text === ""
    ? ipc.unstageFiles(checkout, [file.path])
    : ipc.setIndexText(checkout, file.path, text);
}

const pathsOf = (file: ChangedFile) => (file.oldPath ? [file.path, file.oldPath] : [file.path]);

/** What can be done to files picked in Unstaged or Staged: a line action on whole files, or deleting them from disk. */
export type FileAction = LineAction | "delete";

/** S, U, R or Del on a file of the lists. */
export function fileAction(action: FileAction, side: WorkingSide, file: ChangedFile) {
  return filesAction(action, side, [file]);
}

/** Files of a list still on disk: the ones Delete can take. */
export const deletable = (files: readonly ChangedFile[]) => files.filter((file) => file.status !== "deleted");

/** S, U, R or Del on files picked in Unstaged or Staged, all at once. Resetting and deleting ask first. */
export async function filesAction(action: FileAction, side: WorkingSide, files: readonly ChangedFile[]) {
  const checkout = state.checkout;
  if (!checkout || files.length === 0) return;
  const what = files.length === 1 ? files[0].path : `${files.length} files`;
  try {
    if (action === "stage" && side === "unstaged") {
      await flushSave();
      await ipc.stageFiles(checkout, files.flatMap(pathsOf));
    } else if (action === "unstage" && side === "staged") {
      await ipc.unstageFiles(checkout, files.flatMap(pathsOf));
    } else if (action === "reset") {
      if (!(await confirmReset(side, files))) return;
      await forget(files);
      if (side === "unstaged") await ipc.discardFiles(checkout, files.map((file) => file.path));
      else await ipc.resetFilesToHead(checkout, files.flatMap(pathsOf));
    } else if (action === "delete") {
      const onDisk = deletable(files);
      if (onDisk.length === 0) return notify(files.length === 1 ? `${files[0].path} is already deleted.` : "Those files are already deleted.");
      if (!(await confirmDelete(side, onDisk))) return;
      await forget(onDisk);
      await ipc.deleteFiles(checkout, onDisk.map((file) => file.path));
    } else {
      return;
    }
  } catch (error) {
    fail(`Could not ${action} ${what}`)(error);
  }
  await refreshWork();
}

/** Files about to be put back or deleted: no save may write them again, nor a snapshot bring back their old text. */
async function forget(files: readonly ChangedFile[]) {
  for (const file of files) dropPendingSave(file.path);
  await saved();
  for (const file of files) snapshots.delete(keyOf("unstaged", file.path));
}

/** The files a question is about: all of them, up to a handful. */
function fileNames(files: readonly ChangedFile[]): string {
  const most = 8;
  const names = files.slice(0, most).map((file) => `  ${file.path}`);
  if (files.length > most) names.push(`  …and ${files.length - most} more`);
  return names.join("\n");
}

function confirmReset(side: WorkingSide, files: readonly ChangedFile[]): Promise<boolean> {
  const untracked = side === "unstaged" ? files.filter((file) => file.status === "added").length : 0;
  if (files.length === 1) {
    const file = files[0];
    const text =
      side === "unstaged"
        ? untracked
          ? `Delete ${file.path}? It is not tracked by git, so it cannot be brought back.`
          : `Reset ${file.path}? Its unstaged changes are lost.`
        : file.status === "added"
          ? `Unstage ${file.path}? It is new, so it stays on disk as an untracked file.`
          : `Reset ${file.path} to the last commit? Its staged and unstaged changes are lost.`;
    return confirm(text, { title: "Reset", kind: "warning", okLabel: untracked ? "Delete" : "Reset" });
  }
  const text =
    side === "unstaged"
      ? `Reset ${files.length} files? Their unstaged changes are lost.` +
        (untracked ? ` ${untracked === files.length ? "They are" : `${untracked} of them are`} not tracked by git, so they are deleted and cannot be brought back.` : "")
      : `Reset ${files.length} files to the last commit? Their staged and unstaged changes are lost; new ones stay on disk as untracked files.`;
  return confirm(`${text}\n\n${fileNames(files)}`, { title: "Reset", kind: "warning", okLabel: `Reset ${files.length} files` });
}

/**
 * Deleting asks first, saying what cannot be brought back: untracked files,
 * and unstaged changes (git keeps only what is staged or committed).
 */
function confirmDelete(side: WorkingSide, files: readonly ChangedFile[]): Promise<boolean> {
  const unstaged = new Map(listOf("unstaged").map((file) => [file.path, file]));
  // An added file of Unstaged is untracked; of Staged, it is in the index.
  const isUntracked = (file: ChangedFile) => side === "unstaged" && file.status === "added";
  const untracked = files.filter(isUntracked).length;
  const edited = files.filter((file) => unstaged.has(file.path) && !isUntracked(file)).length;
  const one = files.length === 1;
  const lost: string[] = [];
  if (untracked) lost.push(one ? "It is not tracked by git, so it cannot be brought back." : `${untracked === files.length ? "They are" : `${untracked} of them are`} not tracked by git, so they cannot be brought back.`);
  if (edited)
    lost.push(one ? "Its unstaged changes are lost." : edited === files.length ? "Their unstaged changes are lost." : `The unstaged changes of ${edited} of them are lost.`);
  if (untracked + edited < files.length)
    lost.push(one ? "Git still has it: Reset brings it back." : `Git still has ${untracked + edited ? "the others" : "them"}: Reset brings them back.`);
  const text = one ? `Delete ${files[0].path} from disk? ${lost.join(" ")}` : `Delete ${files.length} files from disk? ${lost.join(" ")}\n\n${fileNames(files)}`;
  return confirm(text, { title: "Delete", kind: "warning", okLabel: one ? "Delete" : `Delete ${files.length} files` });
}

/** Stages every unstaged file. */
export async function stageAll() {
  const checkout = state.checkout;
  const files = listOf("unstaged");
  if (!checkout || files.length === 0) return;
  await flushSave();
  await ipc.stageFiles(checkout, files.flatMap(pathsOf)).catch(fail("Could not stage the files"));
  await refreshWork();
}

/** Unstages every staged file. */
export async function unstageAll() {
  const checkout = state.checkout;
  const files = listOf("staged");
  if (!checkout || files.length === 0) return;
  await ipc.unstageFiles(checkout, files.flatMap(pathsOf)).catch(fail("Could not unstage the files"));
  await refreshWork();
}

// ------------------------------------------------------------ all-files view

const VIEW_KEY = "arnold.commitView";

/** Commit mode's view: every listed file at once (the default), or one file at a time. */
const rememberedView = (): ViewMode => (localStorage.getItem(VIEW_KEY) === "file" ? "file" : "all");

/** Where each file of the all-files view is shown once near the screen, by side and path. */
const sectionHosts = new Map<string, HTMLElement>();
const sectionEditors = new Map<string, DiffEditor>();
/** Bumped for each load of a section, so that a superseded one stops quietly. */
const sectionLoads = new Map<string, number>();
/** The section with keyboard focus: it is not unloaded while scrolled away. */
let focusedKey: string | null = null;
let revealSection: ((key: string, offset?: number) => void) | null = null;
/** A checkout was opened: where the user was in it is restored once its lists are read. */
let placePending = false;

const sideOfKey = (key: string): WorkingSide => (key.startsWith("unstaged:") ? "unstaged" : "staged");
const pathOfKey = (key: string) => key.slice(key.indexOf(":") + 1);

/** Snapshots of sections belong to one checkout. */
function sectionSnapshotsClear() {
  for (const key of [...snapshots.keys()]) if (key.startsWith("unstaged:") || key.startsWith("staged:")) snapshots.delete(key);
}

/** Switches between one file at a time and every listed file; `remember` keeps the choice for next time. */
export async function setView(view: ViewMode, remember = true) {
  if (view === state.view) return;
  if (remember) localStorage.setItem(VIEW_KEY, view);
  if (view === "all") {
    await flushSave();
    stash();
    fileEditor()?.close();
    shownKey = null;
    shownAlone = null;
    shownDiff = null;
    focusedKey = null;
    // A file of the explorer is not listed: the first listed file is selected instead.
    if (state.workSide === null) {
      const first = sectionEntries()[0];
      setState({ selected: first ? first.file.path : null, workSide: first ? sideOfKey(first.key) : null });
    }
    setState({ view, menu: null });
  } else {
    // The all-files view goes away, and its sections with it.
    setState({ view, menu: null });
    await show();
  }
  noteView();
}

/** The files of the all-files view: Unstaged, then Staged. */
export function sectionEntries(): SectionEntry[] {
  const group = (side: WorkingSide, heading: string) =>
    listOf(side).map((file, index) => ({ key: keyOf(side, file.path), file, group: index === 0 ? heading : undefined }));
  return [...group("unstaged", `Unstaged · ${listOf("unstaged").length}`), ...group("staged", `Staged · ${listOf("staged").length}`)];
}

export const selectedSection = () => (state.workSide && state.selected !== null ? keyOf(state.workSide, state.selected) : null);

export function selectSection(key: string) {
  setState({ selected: pathOfKey(key), workSide: sideOfKey(key) });
}

/** A section's editor got the keyboard (or a click): it is the file acted on. */
export function focusSection(side: WorkingSide, path: string) {
  focusedKey = keyOf(side, path);
  if (state.workSide !== side || state.selected !== path) setState({ selected: path, workSide: side });
}

export function onRevealSection(handler: ((key: string, offset?: number) => void) | null) {
  revealSection = handler;
}

export function registerSection(key: string, host: HTMLElement) {
  sectionHosts.set(key, host);
}

export function unregisterSection(key: string) {
  hideSection(key);
  sectionHosts.delete(key);
}

export function setSectionCollapsed(key: string, collapsed: boolean) {
  setState("collapsed", key, collapsed);
  if (collapsed) hideSection(key);
}

/** Loads a file into its section of the all-files view, if not done yet. */
export async function showSection(key: string): Promise<DiffEditor | null> {
  const shown = sectionEditors.get(key);
  if (shown) return shown;
  const host = sectionHosts.get(key);
  const side = sideOfKey(key);
  const path = pathOfKey(key);
  const file = entryOf(side, path);
  const checkout = state.checkout;
  if (!host || !file || !checkout || state.view !== "all" || state.source !== "commit") return null;
  const load = (sectionLoads.get(key) ?? 0) + 1;
  sectionLoads.set(key, load);
  try {
    // A save under way would otherwise be read back half-way.
    await saved();
    const pair = await ipc.readWorkingPair(checkout, file, side);
    if (sectionLoads.get(key) !== load || sectionHosts.get(key) !== host) return sectionEditors.get(key) ?? null;
    const reason = pair.binary ? "Binary file." : pair.tooLarge ? "This file is too large to show." : null;
    if (reason) {
      const note = document.createElement("div");
      note.className = "section-note";
      note.textContent = reason;
      host.replaceChildren(note);
      return null;
    }
    const target = new DiffEditor(host, editorCallbacks(() => path, side));
    sectionEditors.set(key, target);
    target.open(documentOf(file, side, pair), state.mode, snapshots.get(key));
    void attachLanguageServer(file, target);
    return target;
  } catch (error) {
    fail(`Reading ${path}`)(error);
    return null;
  }
}

/**
 * Unloads a section of the all-files view to save memory. `keepFocused`
 * leaves the one with keyboard focus alone. Returns whether an editor was removed.
 */
export function hideSection(key: string, keepFocused = false): boolean {
  if (keepFocused && key === focusedKey) return false;
  sectionLoads.set(key, (sectionLoads.get(key) ?? 0) + 1); // A load still under way is no longer wanted.
  const target = sectionEditors.get(key);
  const snapshot = target?.snapshot();
  if (snapshot) snapshots.set(key, snapshot);
  if (target && hasPendingSave(pathOfKey(key))) void flushSave();
  target?.destroy();
  sectionHosts.get(key)?.replaceChildren();
  sectionEditors.delete(key);
  if (focusedKey === key) focusedKey = null;
  return target !== undefined;
}

/** After the lists changed: every file shown takes what is on disk and in the index now; edits not saved yet stay. */
async function refreshSections() {
  const checkout = state.checkout;
  if (!checkout) return;
  await saved();
  for (const [key, target] of [...sectionEditors]) {
    const side = sideOfKey(key);
    const path = pathOfKey(key);
    const file = entryOf(side, path);
    // A file gone from its list goes with its section.
    if (!file) continue;
    try {
      const pair = await ipc.readWorkingPair(checkout, file, side);
      if (sectionEditors.get(key) !== target) continue;
      if (pair.binary || pair.tooLarge) {
        hideSection(key);
        void showSection(key);
        continue;
      }
      const document = documentOf(file, side, pair);
      const unsaved = side === "unstaged" && hasPendingSave(path) ? target.texts()?.working : undefined;
      const working = unsaved ?? document.working;
      if (target.shows(path, document)) {
        target.updateTexts(document.base, working);
        continue;
      }
      // Shown differently now (staged as a whole, say): where the editor was stays.
      const focused = !!target.working?.hasFocus;
      const snapshot = target.snapshot();
      if (snapshot) snapshots.set(key, snapshot);
      target.open({ ...document, working }, state.mode, snapshots.get(key));
      if (focused) target.focus();
      void attachLanguageServer(file, target);
    } catch (error) {
      fail(`Reading ${path}`)(error);
    }
  }
}

/** The editor the keyboard commands act on: the file shown, or in the all-files view the selected file's section. */
export function currentEditor(): DiffEditor | null {
  if (state.view !== "all") return fileEditor();
  const key = selectedSection();
  return key ? (sectionEditors.get(key) ?? null) : null;
}

export const sectionEditor = (side: WorkingSide, path: string) => sectionEditors.get(keyOf(side, path)) ?? null;

/** The editor the language server talks to for `path`: its working tree text (Unstaged) where shown, else Staged. */
export const languageEditorFor = (path: string) =>
  sectionEditors.get(keyOf("unstaged", path)) ?? sectionEditors.get(keyOf("staged", path)) ?? null;

/** Every section on screen, with its file. */
export function forEachSection(action: (target: DiffEditor, file: ChangedFile) => void) {
  for (const [key, target] of sectionEditors) {
    const file = entryOf(sideOfKey(key), pathOfKey(key));
    if (file) action(target, file);
  }
}

export function setSectionMode(mode: DiffMode) {
  for (const target of sectionEditors.values()) target.setMode(mode);
}

/** F7 / Shift+F7 in the all-files view: the next change, on into the next file. */
export async function stepChange(direction: 1 | -1) {
  const entries = sectionEntries();
  const from = selectedSection();
  const current = from && !state.collapsed[from] ? await showSection(from) : null;
  if (current?.stepChange(direction)) return current.focus();
  let index = entries.findIndex((entry) => entry.key === from);
  if (index < 0) index = direction > 0 ? -1 : entries.length;
  for (let next = index + direction; next >= 0 && next < entries.length; next += direction) {
    const key = entries[next].key;
    if (state.collapsed[key]) continue;
    const target = await showSection(key);
    if (!target) continue; // Binary or too large to show.
    focusSection(sideOfKey(key), pathOfKey(key));
    revealSection?.(key);
    target.enterChange(direction);
    target.focus();
    return;
  }
  current?.focus();
}

// ------------------------------------------------------------ committing

const messageKey = (checkout: string) => `arnold.commitMessage.${checkout}`;
const [message, setMessage] = createSignal("");
export { message };

/** The commit message as typed; kept per checkout until committed. */
export function setCommitMessage(text: string) {
  setMessage(text);
  const checkout = state.checkout;
  if (!checkout) return;
  if (text) localStorage.setItem(messageKey(checkout), text);
  else localStorage.removeItem(messageKey(checkout));
}

export const canCommit = () =>
  !state.committing && message().trim() !== "" && (state.work?.staged.length ?? 0) > 0;

/** Commits what is staged. */
export async function commit() {
  const checkout = state.checkout;
  if (!checkout || !canCommit()) return;
  await flushSave();
  setState("committing", "Checking the formatting…");
  try {
    const go = await formattingChecked(checkout);
    if (go.answer === "cancel") return;
    if (go.answer === "format") {
      setState("committing", "Formatting with Fantomas…");
      const formatted = await ipc.formatAndStage(checkout, go.files);
      if (formatted.failed.length > 0) {
        setState(
          "error",
          `Fantomas could not format ${formatted.failed.join(", ")} as staged, so nothing was committed. ` +
            "The formatting of the other files is staged.",
        );
        return;
      }
    }
    setState("committing", "Committing…");
    const made = await ipc.commitStaged(checkout, message().trim());
    setCommitMessage("");
    notify(`Committed ${made.sha.slice(0, 8)} ${made.subject} on ${made.branch ?? "a detached HEAD"}.`);
  } catch (error) {
    fail(state.committing === "Committing…" ? "The commit failed" : "Formatting with Fantomas failed")(error);
  } finally {
    setState("committing", null);
  }
  await refreshWork();
}

export type FormatAnswer = "format" | "commit" | "cancel";

let answerFormatting: ((answer: FormatAnswer) => void) | null = null;

/**
 * When the repository uses Fantomas and it would reformat the code base (or
 * cannot check it), asks whether to format first, commit as it is, or stop.
 * `files`: what Fantomas would reformat.
 */
async function formattingChecked(checkout: string): Promise<{ answer: FormatAnswer; files: string[] }> {
  let unformatted: NonNullable<typeof state.unformatted>;
  try {
    const check = await ipc.checkFormatting(checkout);
    if (!check || (check.unformatted.length === 0 && check.errors.length === 0)) return { answer: "commit", files: [] };
    unformatted = check;
  } catch (error) {
    unformatted = { unformatted: [], errors: [], failure: String(error) };
  }
  setState("unformatted", unformatted);
  const answer = await new Promise<FormatAnswer>((resolve) => (answerFormatting = resolve));
  return { answer, files: unformatted.unformatted };
}

/** The answer to the Not formatted dialog. */
export function answerFormatCheck(answer: FormatAnswer) {
  setState("unformatted", null);
  answerFormatting?.(answer);
  answerFormatting = null;
}

// ------------------------------------------------------------ going on

/** The branch the default comparison goes against: the repository's default branch. */
export const reviewBase = () => state.repo?.defaultBranch ?? (state.base || null);

/** Why the checked-out branch cannot be reviewed against the base branch; null when it can. */
export function whyNoReview(): string | null {
  const branch = state.work?.branch;
  const base = reviewBase();
  if (!branch) return "The checkout is not on a branch.";
  if (!base) return "The repository has no default branch to review against.";
  if (branch === base) return `${branch} is the base branch itself.`;
  return null;
}

/** Why the checked-out branch cannot be pushed; null when it can. */
export function whyNoPush(): string | null {
  const work = state.work;
  if (!work?.branch) return "The checkout is not on a branch.";
  return whyNotPushable(work.branch, work.sync);
}

/** Why `branch`, standing against its remote as `sync` says, has nothing a push could send; else null. */
export function whyNotPushable(branch: string, sync: BranchSync | null): string | null {
  if (!sync) return "The repository has no remote to push to.";
  if (sync.behind > 0) {
    return `${sync.remote}/${sync.remoteBranch} has ${sync.behind === 1 ? "a commit" : `${sync.behind} commits`} that ${branch} lacks. Pull first.`;
  }
  if (sync.published && sync.ahead === 0) return `${sync.remote}/${sync.remoteBranch} is up to date.`;
  return null;
}

/** Reviews the checked-out branch against the base branch, in Branches. */
export async function reviewBranch() {
  const branch = state.work?.branch;
  const base = reviewBase();
  if (whyNoReview() || !branch || !base) return;
  await flushSave();
  await setBranches({ head: branch, base });
}

/** Pushes the checked-out branch. Resolves to whether it is on the remote now. */
export async function pushBranch(): Promise<boolean> {
  const checkout = state.checkout;
  const work = state.work;
  if (!checkout || !work?.branch || !work.sync || state.committing) return false;
  if (whyNoPush()) return work.sync.published && work.sync.ahead === 0 && work.sync.behind === 0;
  const { remote, remoteBranch } = work.sync;
  setState("committing", `Pushing ${work.branch} to ${remote}…`);
  try {
    await ipc.pushCheckoutBranch(checkout);
    notify(`Pushed ${work.branch} to ${remote}/${remoteBranch}.`);
    return true;
  } catch (error) {
    fail(`Pushing ${work.branch} failed`)(error);
    return false;
  } finally {
    setState("committing", null);
    await refreshWork();
  }
}

const [createRequested, setCreateRequested] = createSignal(false);
/** Set when the Create pull request dialog should open as soon as it can. */
export { createRequested, setCreateRequested };

/**
 * Pushes the checked-out branch, then reviews it against the base branch
 * with the Create pull request dialog open.
 */
export async function pushAndCreatePullRequest() {
  if (whyNoReview() || !(await pushBranch())) return;
  setCreateRequested(true);
  await reviewBranch();
}

/** Pushes the checked-out branch when it has commits to push, then opens its pull request in Pull request. */
export async function pushAndViewPullRequest(remote: string, id: number) {
  if (!whyNoPush() && !(await pushBranch())) return;
  await flushSave();
  await openPullRequest(remote, id);
}

// ------------------------------------------------------------ watching

let changeTimer: number | undefined;

/** Files and the index of the checkout change under Arnold (an agent, an editor, Arnold's own saves). */
export async function startWatching() {
  await ipc.onCheckoutChanged(() => {
    window.clearTimeout(changeTimer);
    changeTimer = window.setTimeout(() => void refreshWork(), 100);
  });
  // Outside Commit mode only the compared checkout is watched: an agent's
  // edits elsewhere are counted when the window comes back.
  window.addEventListener("focus", () => void countUncommitted());
}
