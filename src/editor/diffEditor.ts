// The diff: a side-by-side MergeView or a single unified editor over the same
// working text. Reviewing, it is read-only; in Commit mode the working tree's
// text can be edited and lines staged, unstaged or reset. Switching mode or
// file keeps the selection and scroll position (and an edited file's undo
// history).

import { historyField } from "@codemirror/commands";
import {
  type Chunk,
  getChunks,
  getOriginalDoc,
  goToNextChunk,
  goToPreviousChunk,
  MergeView,
  unifiedMergeView,
  updateOriginalDoc,
} from "@codemirror/merge";
import {
  Annotation,
  ChangeSet,
  Compartment,
  EditorSelection,
  EditorState,
  type Extension,
  Prec,
  Text,
  Transaction,
} from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { changeKindColors } from "./changeKinds";
import { deletedFile } from "./deletedFile";
import { riderIslandsDark } from "./riderIslandsTheme";
import { languageFor, languageLoading } from "./languages";
import { editFrames, setCommitted } from "./editFrames";
import { type LineBlock, lineFirstDiffConfig } from "./lineDiff";
import { lineTints } from "./lineTints";
import { tabIndentation } from "./tabIndent";
import { semanticHighlight, semanticRanges, type SemanticRange, setSemanticRanges } from "./semanticHighlight";
import { matchingLine } from "./matchingLine";
import { type LineSelection, resetEdits, type TextEdit } from "./resetLines";
import { draftCardId, showThreadMarks, threadCardId, type ThreadMark, threadMarks } from "./threads";
import { type AiSuggestion, locate, suggestionCardId } from "./aiSuggestions";
import type { LineRange, NewThreadPlace, PrThread } from "../ipc/commands";

export type DiffMode = "split" | "unified";

/** What can be done to the selected lines of an uncommitted change. */
export type LineAction = "stage" | "unstage" | "reset";

/** Which way a change moves between the two sides: into the base (left) or into the working (right) text. */
export type MoveDirection = "toBase" | "toWorking";

/** The keys of the line actions: plain where nothing can be typed, with Alt where it can. */
export const LINE_ACTION_KEYS: Record<LineAction, string> = { stage: "S", unstage: "U", reset: "R" };

export interface DiffDocument {
  path: string;
  /** Text at the merge-base ("" when the file did not exist). */
  base: string;
  /** Text under review ("" when the file was deleted). */
  working: string;
  /** Show the working text alone, without a diff (an added, deleted or unchanged file). */
  plain?: boolean;
  /**
   * A file shown alone that has a base to compare with: once its text
   * differs from `base`, a "Show changes" button offers the diff (through
   * `onShowChanges`). This is the button's tooltip.
   */
  showChanges?: string;
  /** The file is removed by the branch; `working` holds its base text. */
  deleted?: boolean;
  /** Pull request discussions about lines of the file. */
  threads?: readonly PrThread[];
  /** A new comment being written about lines of the file. */
  draft?: NewThreadPlace | null;
  /** Copilot's open suggestions for the file; each shows where its lines are found in the working text. */
  aiSuggestions?: readonly AiSuggestion[];
  /** The working text can be edited: a file of the working tree. */
  editable?: boolean;
  /**
   * The working text at the branch's last commit, when the working side is a
   * checkout's file: the lines that differ from it (uncommitted edits) are
   * framed. Null or absent: no frames.
   */
  committed?: string | null;
  /** The framed changes have a Suggest button (a pull request's files). */
  suggest?: boolean;
  /** What the selected lines can have done to them (Commit mode); none when reviewing. */
  lineActions?: readonly LineAction[];
  /** Fold long runs of unchanged lines away (the default); false shows the whole file. */
  collapseUnchanged?: boolean;
  /**
   * Buttons on each change that move it to the other side, by what they do
   * ("Stage this change"); a direction without a label has no button.
   */
  moves?: Partial<Record<MoveDirection, string>>;
}

/** Where a comment would go: the selection, or the line under the pointer. */
export type CommentPlace = LineRange;

/** Editor state kept while a file is not on screen. */
export interface Snapshot {
  selection: EditorSelection;
  scrollTop: number;
  /** An editable file's text and undo history; used again only while the file has that text. */
  doc?: Text;
  history?: unknown;
}

/** A place in a text: a 1-based line and a column within it. */
export interface TextPoint {
  line: number;
  column: number;
}

/** Typing or pasting into a read-only side. */
export interface BlockedEdit {
  /** What would have been typed or pasted; "" when that cannot be told. */
  text: string;
  /** Backspace or Delete: takes back the last character typed. */
  erase: boolean;
  /** The selection it went to, when in the working text (where it can be typed again once editable). */
  at: { anchor: TextPoint; head: TextPoint } | null;
  /** Where the cursor is on screen. */
  rect: { left: number; top: number; bottom: number } | null;
}

export interface DiffEditorCallbacks {
  onFocus(): void;
  /**
   * `view` is the editor right-clicked; `position` the text position under
   * the pointer; `comment` where a comment would go.
   */
  onContextMenu(x: number, y: number, view: EditorView, position: number | null, comment: CommentPlace | null): void;
  /** F7 / Shift+F7 in the editor. */
  onStepChange(direction: 1 | -1): void;
  /** The working text was edited (by typing, undo or a reset of lines). */
  onChange?(text: string): void;
  /** S, U or R on the selected lines. */
  onLineAction?(action: LineAction): void;
  /** A change's move button: `selection` is all of its lines. */
  onMoveChunk?(direction: MoveDirection, selection: LineSelection): void;
  /** Typing or pasting into a read-only side, which does nothing. */
  onBlockedEdit?(edit: BlockedEdit): void;
  /** A framed change's Suggest button: `block` is the change, in lines of the committed and the working text. */
  onSuggest?(block: LineBlock): void;
  /** A file was shown, or its cursor or scroll moved. */
  onPlaceChange?(): void;
  /** The "Show changes" button of an edited file shown alone (`DiffDocument.showChanges`). */
  onShowChanges?(): void;
}

/** Whether a key pressed in an editor would change its text. */
const isEditKey = (event: KeyboardEvent) =>
  !event.ctrlKey && !event.metaKey && (event.key.length === 1 || ["Backspace", "Delete", "Enter", "Tab"].includes(event.key));

/** Arrows of the move buttons: which way the change goes. */
const MOVE_ARROWS: Record<MoveDirection, string> = { toBase: "←", toWorking: "→" };

/** All of a change's lines: on the working side, or the base side for lines only removed. */
function chunkLines(chunk: Chunk, a: Text, b: Text): LineSelection {
  const lines = (doc: Text, from: number, to: number) => ({
    from: doc.lineAt(Math.min(from, doc.length)).number - 1,
    to: doc.lineAt(Math.min(Math.max(from, to - 1), doc.length)).number - 1,
  });
  return chunk.fromB < chunk.toB
    ? { side: "b", ...lines(b, chunk.fromB, chunk.toB) }
    : { side: "a", ...lines(a, chunk.fromA, chunk.toA) };
}

/** Marks text that changed on disk rather than in the editor. */
const fromOutside = Annotation.define<boolean>();

/** The one replacement turning `from` into `to`: what lies between their common start and end. */
function replacement(from: string, to: string): { from: number; to: number; insert: string } {
  let start = 0;
  const shorter = Math.min(from.length, to.length);
  while (start < shorter && from.charCodeAt(start) === to.charCodeAt(start)) start++;
  let end = 0;
  while (end < shorter - start && from.charCodeAt(from.length - 1 - end) === to.charCodeAt(to.length - 1 - end)) end++;
  return { from: start, to: from.length - end, insert: to.slice(start, to.length - end) };
}

const COLLAPSE = { margin: 3, minSize: 8 };

/** Share of the split view's width given to the base (left) side. */
const SPLIT_KEY = "arnold.splitRatio";
const MIN_SPLIT = 0.1;
const MAX_SPLIT = 0.9;

const clampSplit = (ratio: number) => Math.min(Math.max(ratio, MIN_SPLIT), MAX_SPLIT);

function storedSplit(): number {
  const stored = Number(localStorage.getItem(SPLIT_KEY));
  return stored > 0 && stored < 1 ? clampSplit(stored) : 0.5;
}

/** Shared by every split view, so dragging one divider moves them all. */
let splitRatio = storedSplit();
/** Re-applies `splitRatio`, one per split view on screen. */
const splitLayouts = new Set<() => void>();

const prefersDark = () => window.matchMedia("(prefers-color-scheme: dark)").matches;

export class DiffEditor {
  private merge: MergeView | null = null;
  private unified: EditorView | null = null;
  private document: DiffDocument | null = null;
  private mode: DiffMode = "split";
  /** Language-server support for the working editor; arrives after the file is shown. */
  private support: Extension = [];
  private readonly supportSlot = new Compartment();
  private layoutSplit: (() => void) | null = null;
  /** The side last given the keyboard (or right-clicked): what line actions act on. */
  private lastFocused: EditorView | null = null;
  /** The "Show changes" button, while the document offers one. */
  private changesButton: HTMLElement | null = null;

  constructor(
    private readonly parent: HTMLElement,
    private readonly callbacks: DiffEditorCallbacks,
  ) {}

  /** Shows a file. `snapshot` restores where an earlier visit of the same file was. */
  open(document: DiffDocument, mode: DiffMode, snapshot?: Snapshot) {
    this.close();
    this.document = document;
    this.mode = mode;
    this.build(snapshot);
  }

  /** Replaces the discussions shown in the file. */
  setThreads(threads: readonly PrThread[]) {
    if (!this.document || JSON.stringify(threads) === JSON.stringify(this.document.threads ?? [])) return;
    this.document = { ...this.document, threads };
    this.showThreads();
  }

  /** Replaces Copilot's suggestions shown in the file. */
  setAiSuggestions(suggestions: readonly AiSuggestion[]) {
    if (!this.document || JSON.stringify(suggestions) === JSON.stringify(this.document.aiSuggestions ?? [])) return;
    this.document = { ...this.document, aiSuggestions: suggestions };
    this.showThreads();
  }

  /** Scrolls the card of `suggestion` into view; false when the file does not show it. */
  revealSuggestion(suggestion: AiSuggestion): boolean {
    const view = this.viewOf("head");
    const lines = view && locate(view.state.doc.toString(), suggestion.original, suggestion.line);
    if (!view || !lines) return false;
    const line = view.state.doc.line(lines.to);
    view.dispatch({ selection: { anchor: view.state.doc.line(lines.from).from }, effects: EditorView.scrollIntoView(line.from, { y: "center" }) });
    return true;
  }

  /** Shows (or with null removes) the card for writing a new comment. */
  setDraft(draft: NewThreadPlace | null) {
    if (!this.document || JSON.stringify(draft) === JSON.stringify(this.document.draft ?? null)) return;
    this.document = { ...this.document, draft };
    this.showThreads();
    if (draft) {
      const view = this.viewOf(draft.lines?.side ?? this.fileSide());
      const line = view?.state.doc.line(Math.min(Math.max(draft.lines?.endLine ?? 1, 1), view.state.doc.lines));
      // The start of the line, so long lines do not scroll the card out of sight sideways.
      if (view && line) view.dispatch({ effects: EditorView.scrollIntoView(line.from, { y: "nearest", yMargin: 160 }) });
    }
  }

  /** The editor showing `side`, if this file shows it. */
  private viewOf(side: "base" | "head"): EditorView | null {
    if (this.merge) return side === "base" ? this.merge.a : this.merge.b;
    const shown = this.document?.plain && this.document.deleted ? "base" : "head";
    return side === shown ? this.unified : null;
  }

  /** The side whose top shows comments about the whole file: the base text of a deleted file, else the working text. */
  private fileSide(): "base" | "head" {
    return this.document?.plain && this.document.deleted ? "base" : "head";
  }

  /** Which side `view` shows. In the unified view only the working text can be commented on. */
  private sideOf(view: EditorView): "base" | "head" {
    if (this.merge) return view === this.merge.a ? "base" : "head";
    return this.document?.plain && this.document.deleted ? "base" : "head";
  }

  /**
   * Where a comment goes: the selection if there is one, else the whole line
   * at `position`. Offsets are 1-based; the end is just past the last
   * character, and a selection ending at the start of a line ends on the line before.
   */
  private commentPlace(view: EditorView, position: number | null): CommentPlace | null {
    const doc = view.state.doc;
    const selection = view.state.selection.main;
    const side = this.sideOf(view);
    if (!selection.empty) {
      const start = doc.lineAt(selection.from);
      let end = doc.lineAt(selection.to);
      let endOffset = selection.to - end.from + 1;
      if (selection.to === end.from && end.number > start.number) {
        end = doc.line(end.number - 1);
        endOffset = end.length + 1;
      }
      return { side, startLine: start.number, startOffset: selection.from - start.from + 1, endLine: end.number, endOffset };
    }
    if (position === null) return null;
    const line = doc.lineAt(position);
    return { side, startLine: line.number, startOffset: 1, endLine: line.number, endOffset: line.length + 1 };
  }

  /** Scrolls the card of thread `id` into view; false when the file does not show it. */
  revealThread(id: number): boolean {
    const thread = this.document?.threads?.find((candidate) => candidate.id === id);
    if (!thread) return false;
    // A thread about the whole file is at the top.
    const position = thread.position;
    const view = this.viewOf(position?.side ?? this.fileSide()) ?? this.unified;
    if (!view) return false;
    const line = view.state.doc.line(Math.min(Math.max(position?.startLine ?? 1, 1), view.state.doc.lines));
    view.dispatch({ selection: { anchor: line.from }, effects: EditorView.scrollIntoView(line.from, { y: "center" }) });
    return true;
  }

  /** Attaches language-server support (errors, hover, completion, ...) to the working editor. */
  setLanguageSupport(support: Extension) {
    this.support = support;
    this.working?.dispatch({ effects: this.supportSlot.reconfigure(support) });
  }

  setMode(mode: DiffMode) {
    if (mode === this.mode || !this.document) {
      this.mode = mode;
      return;
    }
    const snapshot = this.snapshot();
    this.destroyViews();
    this.mode = mode;
    this.build(snapshot ?? undefined);
  }

  close() {
    this.destroyViews();
    this.document = null;
    this.support = [];
  }

  /** The file shown, if any. */
  get shownPath(): string | null {
    return this.document?.path ?? null;
  }

  /** The editor of the text under review (the right side in split mode). */
  get working(): EditorView | null {
    return this.merge?.b ?? this.unified;
  }

  snapshot(): Snapshot | null {
    const view = this.working;
    if (!view) return null;
    const snapshot: Snapshot = { selection: view.state.selection, scrollTop: this.scroller()?.scrollTop ?? 0 };
    if (this.document?.editable) {
      snapshot.doc = view.state.doc;
      snapshot.history = view.state.field(historyField, false);
    }
    return snapshot;
  }

  /** The texts shown: the base side, and the working side with any edits. */
  texts(): { base: string; working: string } | null {
    const document = this.document;
    const view = this.working;
    if (!document || !view) return null;
    return { base: document.base, working: view.state.doc.toString() };
  }

  /** Whether the file shown is `path`, shown with these settings. */
  shows(path: string, kind: Pick<DiffDocument, "plain" | "deleted" | "editable">): boolean {
    const document = this.document;
    return (
      document?.path === path &&
      !!document.plain === !!kind.plain &&
      !!document.deleted === !!kind.deleted &&
      !!document.editable === !!kind.editable
    );
  }

  /**
   * Brings the texts up to date with what changed elsewhere (on disk, in the
   * index) without rebuilding the editors, so the cursor, scroll position
   * and undo history stay. The change is not undoable on its own.
   */
  updateTexts(base: string, working: string) {
    const document = this.document;
    const view = this.working;
    if (!document || !view) return;
    this.document = { ...document, base, working };
    const current = view.state.doc.toString();
    this.syncChangesButton(current);
    if (current !== working) {
      view.dispatch({
        changes: replacement(current, working),
        annotations: [fromOutside.of(true), Transaction.addToHistory.of(false)],
      });
    }
    if (this.merge) {
      const original = this.merge.a.state.doc.toString();
      if (original !== base) this.merge.a.dispatch({ changes: replacement(original, base), annotations: fromOutside.of(true) });
    } else if (this.unified && !document.plain) {
      const original = getOriginalDoc(this.unified.state);
      const text = original.toString();
      if (text !== base) {
        const changes = ChangeSet.of([replacement(text, base)], original.length);
        this.unified.dispatch({ effects: updateOriginalDoc.of({ doc: Text.of(base.split("\n")), changes }) });
      }
    }
  }

  /** Replaces the committed text the uncommitted edits are framed against (after a commit, say). */
  setCommitted(committed: string | null) {
    const document = this.document;
    const view = this.working;
    if (!document || !view || (document.committed ?? null) === committed) return;
    this.document = { ...document, committed };
    view.dispatch({ effects: setCommitted.of(committed) });
  }

  /** Makes the working text `text`, as an edit of its own that can be undone. */
  replaceWorkingText(text: string) {
    const view = this.working;
    if (!view) return;
    const current = view.state.doc.toString();
    if (current !== text) this.applyWorkingEdits([replacement(current, text)]);
  }

  /** Changes the working text as an edit of its own, which can be undone. */
  applyWorkingEdits(edits: TextEdit[]) {
    this.working?.dispatch({ changes: edits, userEvent: "input.reset", scrollIntoView: true });
  }

  /**
   * The lines selected (0-based, inclusive) on the side that last had the
   * keyboard; the cursor's line when nothing is selected.
   */
  selectedLines(): LineSelection | null {
    const working = this.working;
    if (!working) return null;
    const view = this.merge && this.lastFocused === this.merge.a ? this.merge.a : working;
    const range = view.state.selection.main;
    const doc = view.state.doc;
    const from = doc.lineAt(range.from).number - 1;
    let to = doc.lineAt(range.to).number - 1;
    // A selection ending at the start of a line does not include that line.
    if (!range.empty && to > from && range.to === doc.line(to + 1).from) to--;
    return { side: view === working ? "b" : "a", from, to };
  }

  /**
   * Edits that put the working text back to `committed` (the branch's last
   * commit): all of it, or the selected lines of the working side.
   */
  private editUndo(all: boolean): TextEdit[] {
    const committed = this.document?.committed;
    const view = this.working;
    if (committed == null || !this.document?.editable || !view) return [];
    const working = view.state.doc.toString();
    if (all) return working === committed ? [] : [replacement(working, committed)];
    const selection = this.selectedLines();
    return selection?.side === "b" ? resetEdits(committed, working, selection) : [];
  }

  /** The text the uncommitted edits are framed against; null when there is none. */
  committedText(): string | null {
    return this.document?.committed ?? null;
  }

  /** Whether uncommitted edits can be undone here: in the selected lines, and in the whole file. */
  editUndos(): { lines: boolean; file: boolean } {
    return { lines: this.editUndo(false).length > 0, file: this.editUndo(true).length > 0 };
  }

  /**
   * Puts the selected lines (or with `all` the whole file) back to the
   * branch's last commit, as an edit that can itself be undone. Returns
   * whether there was anything to undo.
   */
  undoEdits(all: boolean): boolean {
    const edits = this.editUndo(all);
    if (edits.length === 0) return false;
    this.applyWorkingEdits(edits);
    return true;
  }

  /** What the selected lines can have done to them. */
  lineActions(): readonly LineAction[] {
    return this.document?.lineActions ?? [];
  }

  private lineAction(action: LineAction): boolean {
    if (!this.lineActions().includes(action) || !this.callbacks.onLineAction) return false;
    this.callbacks.onLineAction(action);
    return true;
  }

  /** Replaces the text after the file changed on disk, keeping the scroll position. */
  reload(document: DiffDocument) {
    const scrollTop = this.scroller()?.scrollTop ?? 0;
    this.destroyViews();
    this.document = document;
    this.build();
    this.restoreScroll(scrollTop);
  }

  nextChange() {
    const view = this.working;
    if (view) goToNextChunk({ state: view.state, dispatch: view.dispatch });
    view?.focus();
  }

  previousChange() {
    const view = this.working;
    if (view) goToPreviousChunk({ state: view.state, dispatch: view.dispatch });
    view?.focus();
  }

  /**
   * Moves the cursor to the next (1) or previous (-1) change without wrapping
   * around. Returns false when there is none that way; a file shown whole
   * has none.
   */
  stepChange(direction: 1 | -1): boolean {
    const view = this.working;
    const chunks = view && !this.document?.plain ? getChunks(view.state)?.chunks : undefined;
    if (!view || !chunks) return false;
    const head = view.state.selection.main.head;
    const target =
      direction > 0
        ? chunks.find((chunk) => chunk.fromB > head)
        : [...chunks].reverse().find((chunk) => chunk.toB < head);
    if (!target) return false;
    this.moveTo(view, target.fromB, target.toB);
    return true;
  }

  /**
   * Puts the cursor on the first (1) or last (-1) change of the file, or at
   * the top of a file shown whole.
   */
  enterChange(direction: 1 | -1) {
    const view = this.working;
    if (!view) return;
    const chunks = this.document?.plain ? undefined : getChunks(view.state)?.chunks;
    const target = chunks?.length ? chunks[direction > 0 ? 0 : chunks.length - 1] : null;
    this.moveTo(view, target?.fromB ?? 0, target?.toB ?? 0);
  }

  private moveTo(view: EditorView, from: number, to: number) {
    view.dispatch({
      selection: { anchor: from },
      userEvent: "select.byChunk",
      effects: EditorView.scrollIntoView(EditorSelection.range(to, from)),
    });
  }

  focus() {
    this.working?.focus();
  }

  /**
   * Types `text` over the selection `at` of the working text, as typing
   * would, when it can be edited. Points past the end of the text are
   * clamped.
   */
  typeAt(at: { anchor: TextPoint; head: TextPoint }, text: string): boolean {
    const view = this.working;
    if (!view || view.state.readOnly || !this.document?.editable) return false;
    const doc = view.state.doc;
    const offset = (point: TextPoint) => {
      const line = doc.line(Math.min(Math.max(point.line, 1), doc.lines));
      return line.from + Math.min(Math.max(point.column, 0), line.length);
    };
    const [from, to] = [offset(at.anchor), offset(at.head)].sort((a, b) => a - b);
    view.dispatch({
      changes: { from, to, insert: text },
      selection: { anchor: from + text.length },
      // Not "input.type": a burst typed again opens no completions.
      userEvent: "input",
      scrollIntoView: true,
    });
    view.focus();
    return true;
  }

  /** Tells the callbacks about `text` typed into read-only `view`. */
  private blockedEdit(view: EditorView, text: string, erase = false) {
    const point = (pos: number): TextPoint => {
      const line = view.state.doc.lineAt(pos);
      return { line: line.number, column: pos - line.from };
    };
    const selection = view.state.selection.main;
    const at = view === this.working ? { anchor: point(selection.anchor), head: point(selection.head) } : null;
    const coords = view.coordsAtPos(selection.head);
    const rect = coords && { left: coords.left, top: coords.top, bottom: coords.bottom };
    this.callbacks.onBlockedEdit?.({ text, erase, at, rect });
  }

  /** Scrolls the cursor of the working editor into view. */
  revealCursor() {
    const view = this.working;
    view?.dispatch({ effects: EditorView.scrollIntoView(view.state.selection.main.head, { y: "center" }) });
  }

  destroy() {
    this.close();
  }

  private scroller(): HTMLElement | null {
    return this.merge?.dom ?? this.unified?.scrollDOM ?? null;
  }

  private restoreScroll(scrollTop: number) {
    requestAnimationFrame(() => {
      const scroller = this.scroller();
      if (scroller) scroller.scrollTop = scrollTop;
    });
  }

  /**
   * Puts the "Show changes" button over the top right of the editor, shown
   * while the text differs from the base. It sticks under the section header
   * of the all-files view.
   */
  private addChangesButton(title: string) {
    const bar = document.createElement("div");
    bar.className = "show-changes-bar";
    const button = document.createElement("button");
    button.className = "show-changes";
    button.title = title;
    const dot = document.createElement("span");
    dot.className = "edited-dot";
    dot.setAttribute("aria-hidden", "true");
    dot.textContent = "●";
    const action = document.createElement("span");
    action.className = "edit-status-action";
    action.textContent = "Show changes";
    button.append(dot, "Edited", action);
    // Keeps the cursor in the editor until the click.
    button.addEventListener("mousedown", (event) => event.preventDefault());
    button.addEventListener("click", () => this.callbacks.onShowChanges?.());
    bar.append(button);
    this.parent.prepend(bar);
    this.changesButton = bar;
    this.syncChangesButton(this.working?.state.doc.toString() ?? "");
  }

  private syncChangesButton(text: string) {
    if (this.changesButton) this.changesButton.hidden = text === (this.document?.base ?? "");
  }

  private destroyViews() {
    this.changesButton?.remove();
    this.changesButton = null;
    this.lastFocused = null;
    if (this.layoutSplit) splitLayouts.delete(this.layoutSplit);
    this.layoutSplit = null;
    this.merge?.destroy();
    this.unified?.destroy();
    this.merge = null;
    this.unified = null;
    this.parent.replaceChildren();
  }

  private build(snapshot?: Snapshot) {
    const document = this.document!;
    const doc = document.working;
    const selection = snapshot && fits(snapshot.selection, doc.length) ? snapshot.selection : undefined;
    // An edited file's undo history, while the file still has the text it was made for.
    const history =
      document.editable && snapshot?.history && snapshot.doc?.toString() === doc ? snapshot.history : undefined;
    const workingExtensions = [
      // Before basicSetup's own history, so that this initial value wins.
      history ? historyField.init(() => history as never) : [],
      this.sharedExtensions(document.path, !document.editable),
      // Tab indents where text can be typed; elsewhere it moves the focus.
      document.editable ? tabIndentation(Text.of(doc.split("\n"))) : [],
      this.supportSlot.of(this.support),
      editFrames(document.committed ?? null, document.suggest ? (block) => this.callbacks.onSuggest?.(block) : undefined),
      keymap.of([
        {
          key: "F7",
          run: () => (this.callbacks.onStepChange(1), true),
          shift: () => (this.callbacks.onStepChange(-1), true),
        },
      ]),
      EditorView.updateListener.of((update) => {
        if (update.transactions.some((tr) => tr.effects.some((effect) => effect.is(setSemanticRanges)))) {
          this.mirrorSemanticRanges();
        }
        const edited = update.transactions.some((tr) => tr.docChanged && !tr.annotation(fromOutside));
        if (update.docChanged) this.syncChangesButton(update.state.doc.toString());
        if (edited && this.document?.editable) this.callbacks.onChange?.(update.state.doc.toString());
        if (update.selectionSet) this.callbacks.onPlaceChange?.();
      }),
    ];

    if (document.plain) {
      this.unified = new EditorView({
        parent: this.parent,
        doc,
        selection,
        extensions: [workingExtensions, document.deleted ? deletedFile : []],
      });
    } else if (this.mode === "split") {
      this.merge = new MergeView({
        parent: this.parent,
        a: {
          doc: document.base,
          extensions: this.sharedExtensions(document.path, true),
        },
        b: { doc, selection, extensions: workingExtensions },
        highlightChanges: true,
        // changeKindColors draws the change gutter.
        gutter: false,
        collapseUnchanged: document.collapseUnchanged === false ? undefined : COLLAPSE,
        diffConfig: lineFirstDiffConfig,
        // The merge view places a control by each change, between the sides;
        // its own revert is replaced by the move buttons.
        ...(this.hasMoves() ? { revertControls: "b-to-a" as const, renderRevertControl: () => this.splitMoveControl() } : {}),
      });
      const moves = this.document?.moves;
      this.merge.dom.classList.toggle("arnold-two-moves", !!moves?.toBase && !!moves.toWorking);
      this.addSplitter(this.merge);
    } else {
      this.unified = new EditorView({
        parent: this.parent,
        doc,
        selection,
        extensions: [
          workingExtensions,
          unifiedMergeView({
            original: document.base,
            highlightChanges: true,
            gutter: false,
            syntaxHighlightDeletions: true,
            collapseUnchanged: document.collapseUnchanged === false ? undefined : COLLAPSE,
            diffConfig: lineFirstDiffConfig,
            mergeControls: this.hasMoves() ? (type, _action) => this.unifiedMoveButton(type) : false,
          }),
        ],
      });
      // Deletions are drawn with the language there is when they are first
      // shown: once it has loaded, the view is made again with it.
      const unified = this.unified;
      void languageLoading(document.path)?.then(
        () => {
          if (this.unified !== unified) return;
          const focused = unified.hasFocus;
          const again = this.snapshot();
          this.destroyViews();
          this.build(again ?? undefined);
          if (focused) this.unified?.focus();
        },
        () => {},
      );
    }
    if (document.plain && document.showChanges) this.addChangesButton(document.showChanges);
    this.showThreads();
    if (snapshot) this.restoreScroll(snapshot.scrollTop);
    this.scroller()?.addEventListener("scroll", () => this.callbacks.onPlaceChange?.(), { passive: true });
    this.callbacks.onPlaceChange?.();
  }

  /**
   * A card under the last line each thread is about, on its side, and the
   * card for a new comment. The split view gives the other side an empty
   * block as high as each card; the unified view puts base-side threads
   * under the matching working line.
   */
  private showThreads() {
    const document = this.document;
    if (!document) return;
    // Line 0: above the first line, for comments about the whole file.
    type Card = { id: string; side: "base" | "head"; line: number; own: ThreadMark };
    const cards: Card[] = [];
    for (const thread of document.threads ?? []) {
      const position = thread.position;
      const line = position?.endLine ?? 0;
      const lines = position && { from: position.startLine, to: position.endLine };
      cards.push({
        id: threadCardId(thread),
        side: position?.side ?? this.fileSide(),
        line,
        own: { kind: "thread", thread, line, lines },
      });
    }
    // Found in the working text as it is now; a suggestion whose lines changed is not shown.
    const head = document.deleted ? null : this.viewOf("head");
    if (head) {
      const text = head.state.doc.toString();
      for (const suggestion of document.aiSuggestions ?? []) {
        const lines = locate(text, suggestion.original, suggestion.line);
        if (!lines) continue;
        cards.push({
          id: suggestionCardId(suggestion),
          side: "head",
          line: lines.to,
          own: { kind: "ai", suggestion, line: lines.to, lines },
        });
      }
    }
    const draft = document.draft;
    if (draft) {
      const range = draft.lines;
      const line = range?.endLine ?? 0;
      const lines = range && { from: range.startLine, to: range.endLine };
      cards.push({ id: draftCardId(draft), side: range?.side ?? this.fileSide(), line, own: { kind: "draft", draft, line, lines } });
    }

    if (this.merge) {
      const { a, b, chunks } = this.merge;
      const marksA: ThreadMark[] = [];
      const marksB: ThreadMark[] = [];
      for (const card of cards) {
        const onBase = card.side === "base";
        const [from, to] = onBase ? [a, b] : [b, a];
        (onBase ? marksA : marksB).push(card.own);
        const line = card.line === 0 ? 0 : matchingLine(card.line, chunks, from.state.doc, to.state.doc, onBase);
        (onBase ? marksB : marksA).push({ kind: "spacer", id: card.id, line });
      }
      showThreadMarks(a, marksA);
      showThreadMarks(b, marksB);
    } else if (this.unified) {
      const view = this.unified;
      const marks: ThreadMark[] = [];
      if (document.plain) {
        // Only one side is shown: the base text of a deleted file, the working text of the rest.
        const side = document.deleted ? "base" : "head";
        for (const card of cards) if (card.side === side) marks.push(card.own);
      } else {
        const chunks = getChunks(view.state)?.chunks ?? [];
        const original = getOriginalDoc(view.state);
        for (const card of cards) {
          if (card.side === "head") {
            marks.push(card.own);
            continue;
          }
          const line = matchingLine(card.line, chunks, original, view.state.doc, true);
          if (card.own.kind !== "spacer") marks.push({ ...card.own, line, lines: null });
        }
      }
      showThreadMarks(view, marks);
    }
  }

  /**
   * Only the working side talks to the language server. Its symbol colors are
   * copied to the base side wherever the two sides have the same text.
   */
  private mirrorSemanticRanges() {
    const merge = this.merge;
    if (!merge) return;
    const chunks = merge.chunks;
    const mirrored: SemanticRange[] = [];
    let next = 0;
    for (const range of semanticRanges(merge.b)) {
      while (next < chunks.length && chunks[next].toB <= range.from) next++;
      if (next < chunks.length && chunks[next].fromB < range.to) continue; // Inside a change.
      const offset = next > 0 ? chunks[next - 1].toA - chunks[next - 1].toB : 0;
      mirrored.push({ from: range.from + offset, to: range.to + offset, kind: range.kind });
    }
    merge.a.dispatch({ effects: setSemanticRanges.of(mirrored) });
  }

  private hasMoves(): boolean {
    const moves = this.document?.moves;
    return !!moves && (!!moves.toBase || !!moves.toWorking);
  }

  private moveButton(direction: MoveDirection, run: () => void): HTMLButtonElement {
    const label = this.document?.moves?.[direction] ?? "";
    const button = document.createElement("button");
    button.className = `arnold-move arnold-move-${direction}`;
    button.textContent = MOVE_ARROWS[direction];
    button.title = label;
    button.setAttribute("aria-label", label);
    button.onmousedown = (event) => {
      // Before the merge view's own handler, which would revert the change.
      event.preventDefault();
      event.stopPropagation();
      run();
    };
    return button;
  }

  /** Split view: the buttons beside a change, between the sides. */
  private splitMoveControl(): HTMLElement {
    const control = document.createElement("div");
    control.className = "arnold-move-control";
    const moves = this.document?.moves ?? {};
    for (const direction of ["toBase", "toWorking"] as const) {
      if (!moves[direction]) continue;
      control.append(
        this.moveButton(direction, () => {
          // The merge view numbers its controls by change.
          const merge = this.merge;
          const chunk = merge?.chunks[Number(control.dataset.chunk)];
          if (merge && chunk) this.callbacks.onMoveChunk?.(direction, chunkLines(chunk, merge.a.state.doc, merge.b.state.doc));
        }),
      );
    }
    return control;
  }

  /**
   * Unified view: the buttons on a change's removed lines. "accept" takes the
   * working text into the base, "reject" puts the base text back.
   */
  private unifiedMoveButton(type: "accept" | "reject"): HTMLElement {
    const direction: MoveDirection = type === "accept" ? "toBase" : "toWorking";
    if (!this.document?.moves?.[direction]) {
      const none = document.createElement("span");
      none.hidden = true;
      return none;
    }
    const button = this.moveButton(direction, () => {
      const view = this.unified;
      if (!view) return;
      const at = view.posAtDOM(button);
      const chunk = getChunks(view.state)?.chunks.find((candidate) => candidate.fromB <= at && candidate.endB >= at);
      if (chunk) this.callbacks.onMoveChunk?.(direction, chunkLines(chunk, getOriginalDoc(view.state), view.state.doc));
    });
    button.textContent = `${MOVE_ARROWS[direction]} ${this.document.moves[direction]!.replace(/ this change.*$/, "")}`;
    return button;
  }

  /** A draggable divider between the two sides; double-click makes them equal again. */
  private addSplitter(merge: MergeView) {
    const left = merge.a.dom.parentElement!;
    const right = merge.b.dom.parentElement!;
    const apply = () => {
      left.style.flexGrow = String(splitRatio);
      right.style.flexGrow = String(1 - splitRatio);
    };
    const setRatio = (ratio: number) => {
      splitRatio = clampSplit(ratio);
      splitLayouts.forEach((layout) => layout());
    };
    const persist = () => localStorage.setItem(SPLIT_KEY, String(splitRatio));

    const splitter = document.createElement("div");
    splitter.className = "arnold-diff-splitter";
    splitter.title = "Drag to resize · double-click to reset";
    splitter.onpointerdown = (down) => {
      if (down.button !== 0) return;
      down.preventDefault();
      // Only the two editors share the width; the splitter stays fixed.
      const startLeft = left.getBoundingClientRect().width;
      const total = startLeft + right.getBoundingClientRect().width;
      splitter.setPointerCapture(down.pointerId);
      splitter.classList.add("dragging");
      splitter.onpointermove = (move) => setRatio((startLeft + move.clientX - down.clientX) / total);
      splitter.onpointerup = splitter.onpointercancel = () => {
        splitter.onpointermove = splitter.onpointerup = splitter.onpointercancel = null;
        splitter.classList.remove("dragging");
        persist();
      };
    };
    splitter.ondblclick = () => {
      setRatio(0.5);
      persist();
    };
    left.after(splitter);
    this.layoutSplit = apply;
    splitLayouts.add(apply);
    apply();
  }

  private sharedExtensions(path: string, readOnly: boolean): Extension {
    // A key's keydown already told about it: its input event, if any, is not told again.
    let typedKey = false;
    const actions = ["stage", "unstage", "reset"] as const;
    // A plain letter acts where it cannot be typed, and on a selection where
    // it can: typing over selected lines is what S and R are for here. With
    // only a cursor it types.
    const plain = (action: LineAction) => (view: EditorView) =>
      (view.state.readOnly || !view.state.selection.main.empty) && this.lineAction(action);
    const letterKeys = actions.flatMap((action) => {
      const letter = LINE_ACTION_KEYS[action];
      return [
        { key: letter.toLowerCase(), run: plain(action) },
        { key: letter, run: plain(action) },
      ];
    });
    // Alt+S/U/R always act, on the cursor's line too. Matched by the physical
    // key: macOS turns Alt+letter into a typed character (Alt+S is "ß"), for
    // which CodeMirror runs no Alt binding.
    const altKeys = {
      any: (_view: EditorView, event: KeyboardEvent) => {
        if (!event.altKey || event.ctrlKey || event.metaKey) return false;
        const action = actions.find((candidate) => event.code === `Key${LINE_ACTION_KEYS[candidate]}`);
        return action !== undefined && this.lineAction(action);
      },
    };
    return [
      keymap.of([...letterKeys, altKeys]),
      basicSetup,
      // Read-only, but still focusable with a cursor for go-to-definition and selecting.
      EditorState.readOnly.of(readOnly),
      languageFor(path),
      semanticHighlight,
      prefersDark() ? riderIslandsDark : [],
      changeKindColors,
      lineTints,
      threadMarks,
      // Typing into a read-only side, told before any key binding can swallow the key.
      Prec.highest(
        EditorView.domEventHandlers({
          keydown: (event, view) => {
            if (!view.state.readOnly || !isEditKey(event) || event.isComposing) return false;
            typedKey = true;
            if (event.key === "Backspace" || event.key === "Delete") this.blockedEdit(view, "", true);
            else this.blockedEdit(view, event.key === "Enter" ? "\n" : event.key === "Tab" ? "" : event.key);
            return false;
          },
          // Dictation, the emoji picker and composed characters arrive without
          // a key of their own (a key's own input event follows its keydown).
          beforeinput: (event, view) => {
            if (view.state.readOnly && !typedKey) {
              const text = event.inputType.startsWith("insert") ? (event.data ?? "") : "";
              this.blockedEdit(view, event.inputType === "insertParagraph" ? "\n" : text);
            }
            typedKey = false;
            return false;
          },
          keyup: () => {
            typedKey = false;
            return false;
          },
          // CodeMirror swallows pasting into a read-only editor before any input event.
          paste: (event, view) => {
            if (view.state.readOnly) this.blockedEdit(view, event.clipboardData?.getData("text/plain") ?? "");
            return false;
          },
        }),
      ),
      EditorView.domEventHandlers({
        focus: (_event, view) => {
          this.lastFocused = view;
          this.callbacks.onFocus();
          return false;
        },
        contextmenu: (event, view) => {
          event.preventDefault();
          this.lastFocused = view;
          const position = view.posAtCoords({ x: event.clientX, y: event.clientY });
          // Line actions act on the right-clicked line unless it is part of the selection.
          const selection = view.state.selection.main;
          const undoable = view === this.working && this.document?.editable && this.document.committed != null;
          if (
            (this.lineActions().length > 0 || undoable) &&
            position !== null &&
            (selection.empty || position < selection.from || position > selection.to)
          ) {
            view.dispatch({ selection: { anchor: position } });
          }
          const comment = this.commentPlace(view, position);
          this.callbacks.onContextMenu(event.clientX, event.clientY, view, position, comment);
          return true;
        },
      }),
    ];
  }
}

function fits(selection: EditorSelection, length: number) {
  return selection.ranges.every((range) => range.to <= length);
}

