// The read-only diff: a side-by-side MergeView or a single unified editor over
// the same working text. Switching mode or file keeps the selection and scroll
// position.

import { getChunks, getOriginalDoc, goToNextChunk, goToPreviousChunk, MergeView, unifiedMergeView } from "@codemirror/merge";
import { Compartment, EditorSelection, EditorState, type Extension } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { changeKindColors } from "./changeKinds";
import { deletedFile } from "./deletedFile";
import { riderIslandsDark } from "./riderIslandsTheme";
import { languageFor } from "./languages";
import { lineFirstDiffConfig } from "./lineDiff";
import { lineTints } from "./lineTints";
import { semanticHighlight, semanticRanges, type SemanticRange, setSemanticRanges } from "./semanticHighlight";
import { matchingLine } from "./matchingLine";
import { draftCardId, showThreadMarks, threadCardId, type ThreadMark, threadMarks } from "./threads";
import type { NewThreadPlace, PrThread } from "../ipc/commands";

export type DiffMode = "split" | "unified";

export interface DiffDocument {
  path: string;
  /** Text at the merge-base ("" when the file did not exist). */
  base: string;
  /** Text under review ("" when the file was deleted). */
  working: string;
  /** Show the working text alone, without a diff (an added, deleted or unchanged file). */
  plain?: boolean;
  /** The file is removed by the branch; `working` holds its base text. */
  deleted?: boolean;
  /** Pull request discussions about lines of the file. */
  threads?: readonly PrThread[];
  /** A new comment being written about lines of the file. */
  draft?: NewThreadPlace | null;
}

/** Where a comment would go: the selection, or the line under the pointer. Without the path. */
export type CommentPlace = Omit<NewThreadPlace, "path">;

/** Editor state kept while a file is not on screen. */
export interface Snapshot {
  selection: EditorSelection;
  scrollTop: number;
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

  /** Shows (or with null removes) the card for writing a new comment. */
  setDraft(draft: NewThreadPlace | null) {
    if (!this.document || JSON.stringify(draft) === JSON.stringify(this.document.draft ?? null)) return;
    this.document = { ...this.document, draft };
    this.showThreads();
    if (draft) {
      const view = this.viewOf(draft.side);
      const line = view?.state.doc.line(Math.min(Math.max(draft.endLine, 1), view.state.doc.lines));
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
    const position = thread?.position;
    if (!position) return false;
    const view = this.merge ? (position.side === "base" ? this.merge.a : this.merge.b) : this.unified;
    if (!view) return false;
    const line = view.state.doc.line(Math.min(Math.max(position.startLine, 1), view.state.doc.lines));
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

  /** The editor of the text under review (the right side in split mode). */
  get working(): EditorView | null {
    return this.merge?.b ?? this.unified;
  }

  snapshot(): Snapshot | null {
    const view = this.working;
    if (!view) return null;
    return { selection: view.state.selection, scrollTop: this.scroller()?.scrollTop ?? 0 };
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

  private destroyViews() {
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
    const workingExtensions = [
      this.sharedExtensions(document.path),
      this.supportSlot.of(this.support),
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
          extensions: this.sharedExtensions(document.path),
        },
        b: { doc, selection, extensions: workingExtensions },
        highlightChanges: true,
        // changeKindColors draws the change gutter.
        gutter: false,
        collapseUnchanged: COLLAPSE,
        diffConfig: lineFirstDiffConfig,
      });
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
            collapseUnchanged: COLLAPSE,
            diffConfig: lineFirstDiffConfig,
            mergeControls: false,
          }),
        ],
      });
    }
    this.showThreads();
    if (snapshot) this.restoreScroll(snapshot.scrollTop);
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
    type Card = { id: string; side: "base" | "head"; line: number; own: ThreadMark };
    const cards: Card[] = [];
    for (const thread of document.threads ?? []) {
      const position = thread.position;
      if (!position) continue;
      const lines = { from: position.startLine, to: position.endLine };
      cards.push({
        id: threadCardId(thread),
        side: position.side,
        line: position.endLine,
        own: { kind: "thread", thread, line: position.endLine, lines },
      });
    }
    const draft = document.draft;
    if (draft) {
      const lines = { from: draft.startLine, to: draft.endLine };
      cards.push({ id: draftCardId(draft), side: draft.side, line: draft.endLine, own: { kind: "draft", draft, line: draft.endLine, lines } });
    }

    if (this.merge) {
      const { a, b, chunks } = this.merge;
      const marksA: ThreadMark[] = [];
      const marksB: ThreadMark[] = [];
      for (const card of cards) {
        const onBase = card.side === "base";
        const [from, to] = onBase ? [a, b] : [b, a];
        (onBase ? marksA : marksB).push(card.own);
        const line = matchingLine(card.line, chunks, from.state.doc, to.state.doc, onBase);
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

  private sharedExtensions(path: string): Extension {
    return [
      basicSetup,
      // Read-only, but still focusable with a cursor for go-to-definition and selecting.
      EditorState.readOnly.of(true),
      languageFor(path),
      semanticHighlight,
      prefersDark() ? riderIslandsDark : [],
      changeKindColors,
      lineTints,
      threadMarks,
      EditorView.domEventHandlers({
        focus: () => {
          this.callbacks.onFocus();
          return false;
        },
        contextmenu: (event, view) => {
          event.preventDefault();
          const position = view.posAtCoords({ x: event.clientX, y: event.clientY });
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

