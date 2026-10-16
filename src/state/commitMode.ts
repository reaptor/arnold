// Commit mode: the uncommitted changes of a checkout, in two lists like the
// commit dialog of Git Extensions. A file picked in Unstaged shows index →
// working tree, editable; picked in Staged it shows HEAD → index. S stages,
// U unstages and R resets the selected lines (or, in the lists, files), and
// committing leads on to reviewing the branch or creating a pull request.

import { confirm } from "@tauri-apps/plugin-dialog";
import { createSignal } from "solid-js";
import { reconcile } from "solid-js/store";
import type { DiffDocument, LineAction, MoveDirection, Snapshot } from "../editor/diffEditor";
import { carryOver, dropSelectedLines, type LineSelection, resetEdits, takeSelectedLines } from "../editor/resetLines";
import * as ipc from "../ipc/commands";
import type { ChangedFile, FilePair, WorkingSide } from "../ipc/commands";
import { comparePaths } from "./pathOrder";
import {
  attachLanguageServer,
  fileEditor,
  openPullRequest,
  setAllFiles,
  setBranches,
  setState,
  setUpLanguageFeaturesIn,
  state,
} from "./review";

/** How long typing pauses before an edit is saved to disk. */
const SAVE_DELAY = 400;
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

function notify(text: string) {
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

/** Switches to Commit mode, in the checkout used last (else the opened folder). */
export async function enter() {
  const repo = state.repo;
  if (!repo) return;
  const remembered = localStorage.getItem(checkoutKey(repo.root));
  const known = (path: string) =>
    samePath(path, repo.root) || repo.branches.some((branch) => branch.worktreePath && samePath(branch.worktreePath, path));
  await openCheckout(remembered && known(remembered) ? remembered : repo.root);
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
  snapshots.clear();
  shownKey = null;
  fileEditor()?.close();
  setAllFiles([]);
  setMessage(localStorage.getItem(messageKey(path)) ?? "");
  // One file at a time, without changing the remembered choice for reviewing.
  setState({
    checkout: path,
    work: null,
    workSide: null,
    selected: null,
    view: "file",
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
    await keepSelection(index);
  } catch (error) {
    fail("Reading the uncommitted changes")(error);
  }
}

/**
 * After the lists changed: the file shown stays if it is still listed; a
 * file gone from its list (all staged, say) gives way to the one now at its
 * place, as in Git Extensions, else to the same file in the other list.
 */
async function keepSelection(formerIndex: number) {
  const side = state.workSide;
  const path = state.selected;
  if (path !== null && (side === null || entryOf(side, path))) return show();
  if (side !== null && path !== null) {
    const list = listOf(side);
    const next = list[Math.min(Math.max(formerIndex, 0), list.length - 1)];
    if (next) return selectWorkFile(side, next.path);
    const other: WorkingSide = side === "unstaged" ? "staged" : "unstaged";
    if (entryOf(other, path)) return selectWorkFile(other, path);
  }
  const first: [WorkingSide, ChangedFile | undefined][] = [
    ["unstaged", listOf("unstaged")[0]],
    ["staged", listOf("staged")[0]],
  ];
  const [firstSide, firstFile] = first.find(([, file]) => file) ?? ["unstaged", undefined];
  if (firstFile) return selectWorkFile(firstSide, firstFile.path);
  setState({ selected: null, workSide: null });
  return show();
}

/** Picks a file in Unstaged or Staged. */
export async function selectWorkFile(side: WorkingSide, path: string) {
  if (side === state.workSide && path === state.selected) return;
  stash();
  setState({ selected: path, workSide: side, menu: null });
  await show();
}

/**
 * A file of the explorer, or the target of go-to-definition: the whole file
 * as it is on disk, editable, against the last commit (whatever is staged of it).
 */
export async function openTreeFile(path: string) {
  if (state.workSide === null && state.selected === path) return;
  stash();
  setState({ selected: path, workSide: null, menu: null });
  await show();
}

/** Where the editor was in each file, by side and path. */
const snapshots = new Map<string, Snapshot>();
/** Side and path of what the editor shows. */
let shownKey: string | null = null;
let showToken = 0;

const keyOf = (side: WorkingSide | null, path: string) => `${side ?? "file"}:${path}`;

function stash() {
  const snapshot = fileEditor()?.snapshot();
  if (shownKey && snapshot) snapshots.set(shownKey, snapshot);
}

function documentOf(file: ChangedFile, side: WorkingSide | null, pair: FilePair): DiffDocument {
  const base = pair.base ?? "";
  const working = pair.working ?? "";
  if (side === null) {
    // The explorer: the last commit → the working tree, as Unstaged shows it
    // when nothing is staged, and the whole file. New and deleted files show
    // whole. Lines are staged from Unstaged, which knows the index.
    const added = pair.base === null;
    const deleted = pair.working === null;
    return {
      path: file.path,
      base,
      working: deleted ? base : working,
      plain: added || deleted,
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
  const editor = fileEditor();
  const checkout = state.checkout;
  const file = shownFile();
  const side = state.workSide;
  if (!editor || !checkout || !file) {
    editor?.close();
    shownKey = null;
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
    await saving;
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
      setState("placeholder", reason);
      return;
    }
    const document = documentOf(file, side, pair);
    const key = keyOf(side, file.path);
    if (key === shownKey && editor.shows(file.path, document)) {
      // Edits not saved yet stay; the other side follows the index.
      const working = pending?.path === file.path ? (editor.texts()?.working ?? document.working) : document.working;
      editor.updateTexts(document.base, working);
      return;
    }
    shownKey = key;
    setState("placeholder", null);
    editor.open(document, state.mode, snapshots.get(key));
    void attachLanguageServer(file, editor);
  } catch (error) {
    fail(`Reading ${file.path}`)(error);
  }
}

// ------------------------------------------------------------ editing

let pending: { checkout: string; path: string; text: string } | null = null;
let saveTimer: number | undefined;
/** The last save; saves run one after the other. */
let saving: Promise<void> = Promise.resolve();

/** The working text of `path` was edited: it is saved once typing pauses. */
export function edited(path: string, text: string) {
  const checkout = state.checkout;
  if (!checkout || state.workSide === "staged") return;
  pending = { checkout, path, text };
  window.clearTimeout(saveTimer);
  saveTimer = window.setTimeout(() => void flushSave(), SAVE_DELAY);
}

/** Saves an edit still waiting for typing to pause. */
export function flushSave(): Promise<void> {
  window.clearTimeout(saveTimer);
  const job = pending;
  pending = null;
  if (job) {
    saving = saving
      .then(() => ipc.saveWorktreeFile(job.checkout, job.path, job.text))
      .catch(fail(`Saving ${job.path}`));
  }
  return saving;
}

function dropPendingSave(path: string) {
  if (pending?.path !== path) return;
  pending = null;
  window.clearTimeout(saveTimer);
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
  const editor = fileEditor();
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

/** S, U or R on a file of the lists. Resetting asks first. */
export async function fileAction(action: LineAction, side: WorkingSide, file: ChangedFile) {
  const checkout = state.checkout;
  if (!checkout) return;
  try {
    if (action === "stage" && side === "unstaged") {
      await flushSave();
      await ipc.stageFiles(checkout, pathsOf(file));
    } else if (action === "unstage" && side === "staged") {
      await ipc.unstageFiles(checkout, pathsOf(file));
    } else if (action === "reset") {
      if (!(await confirmReset(side, file))) return;
      dropPendingSave(file.path);
      await saving;
      if (side === "unstaged") await ipc.discardFiles(checkout, [file.path]);
      else await ipc.resetFilesToHead(checkout, pathsOf(file));
      snapshots.delete(keyOf("unstaged", file.path));
    } else {
      return;
    }
  } catch (error) {
    fail(`Could not ${action} ${file.path}`)(error);
  }
  await refreshWork();
}

function confirmReset(side: WorkingSide, file: ChangedFile): Promise<boolean> {
  const untracked = side === "unstaged" && file.status === "added";
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
  const sync = work.sync;
  if (!sync) return "The repository has no remote to push to.";
  if (sync.behind > 0) {
    return `${sync.remote}/${sync.remoteBranch} has ${sync.behind === 1 ? "a commit" : `${sync.behind} commits`} that ${work.branch} lacks. Pull first.`;
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
}
