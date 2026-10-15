// The read-only diff: a side-by-side MergeView or a single unified editor over
// the same working text. Switching mode or file keeps the selection and scroll
// position.

import { goToNextChunk, goToPreviousChunk, MergeView, unifiedMergeView } from "@codemirror/merge";
import { Compartment, EditorSelection, EditorState, type Extension } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { changeKindColors } from "./changeKinds";
import { riderIslandsDark } from "./riderIslandsTheme";
import { languageFor } from "./languages";
import { lineFirstDiffConfig } from "./lineDiff";
import { semanticHighlight, semanticRanges, type SemanticRange, setSemanticRanges } from "./semanticHighlight";

export type DiffMode = "split" | "unified";

export interface DiffDocument {
  path: string;
  /** Text at the merge-base ("" when the file did not exist). */
  base: string;
  /** Text under review ("" when the file was deleted). */
  working: string;
  /** Show the working text alone, without a diff (an added, deleted or unchanged file). */
  plain?: boolean;
}

/** Editor state kept while a file is not on screen. */
export interface Snapshot {
  selection: EditorSelection;
  scrollTop: number;
}

export interface DiffEditorCallbacks {
  onFocus(): void;
  /** `view` is the editor right-clicked; `position` the text position under the pointer. */
  onContextMenu(x: number, y: number, view: EditorView, position: number | null): void;
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
      keymap.of([{ key: "F7", run: () => (this.nextChange(), true), shift: () => (this.previousChange(), true) }]),
      EditorView.updateListener.of((update) => {
        if (update.transactions.some((tr) => tr.effects.some((effect) => effect.is(setSemanticRanges)))) {
          this.mirrorSemanticRanges();
        }
      }),
    ];

    if (document.plain) {
      this.unified = new EditorView({ parent: this.parent, doc, selection, extensions: workingExtensions });
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
    if (snapshot) this.restoreScroll(snapshot.scrollTop);
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
      EditorView.domEventHandlers({
        focus: () => {
          this.callbacks.onFocus();
          return false;
        },
        contextmenu: (event, view) => {
          event.preventDefault();
          const position = view.posAtCoords({ x: event.clientX, y: event.clientY });
          this.callbacks.onContextMenu(event.clientX, event.clientY, view, position);
          return true;
        },
      }),
    ];
  }
}

function fits(selection: EditorSelection, length: number) {
  return selection.ranges.every((range) => range.to <= length);
}

