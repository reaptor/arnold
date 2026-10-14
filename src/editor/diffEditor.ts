// The editable diff: a side-by-side MergeView or a single unified editor over
// the same working text. Switching mode or file keeps the text, selection,
// scroll position and undo history.

import { historyField } from "@codemirror/commands";
import { goToNextChunk, goToPreviousChunk, MergeView, unifiedMergeView } from "@codemirror/merge";
import { Compartment, EditorSelection, EditorState, type Extension, type Text } from "@codemirror/state";
import { oneDark } from "@codemirror/theme-one-dark";
import { EditorView, keymap } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { languageFor } from "./languages";
import { resetEdits } from "./resetLines";

export type DiffMode = "split" | "unified";

export interface DiffDocument {
  path: string;
  /** Text at the merge-base ("" when the file did not exist). */
  base: string;
  /** Text under review ("" when the file was deleted). */
  working: string;
  editable: boolean;
  /** Show the working text alone, without a diff (a file that is not part of the diff). */
  plain?: boolean;
}

/** Editor state kept while a file is not on screen. */
export interface Snapshot {
  doc: Text;
  selection: EditorSelection;
  history: unknown;
  scrollTop: number;
}

export interface DiffEditorCallbacks {
  onChange(text: string): void;
  onSave(): void;
  onFocus(): void;
  onContextMenu(x: number, y: number): void;
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

  /** Shows a file. `snapshot` restores an earlier editing session of the same file. */
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

  /** The editable (working) editor. */
  get working(): EditorView | null {
    return this.merge?.b ?? this.unified;
  }

  text(): string | null {
    return this.working?.state.doc.toString() ?? null;
  }

  snapshot(): Snapshot | null {
    const view = this.working;
    if (!view) return null;
    return {
      doc: view.state.doc,
      selection: view.state.selection,
      history: view.state.field(historyField, false),
      scrollTop: this.scroller()?.scrollTop ?? 0,
    };
  }

  /** Replaces the working text after the file changed on disk. Not undoable. */
  reload(document: DiffDocument) {
    const scrollTop = this.scroller()?.scrollTop ?? 0;
    this.destroyViews();
    this.document = document;
    this.build();
    this.restoreScroll(scrollTop);
  }

  /**
   * Resets the lines selected in the focused side to the base version.
   * Returns false when the selection touches no change.
   */
  resetSelectedLines(): boolean {
    const working = this.working;
    if (!working || !this.document?.editable) return false;
    const onBaseSide = this.merge !== null && this.merge.a.hasFocus;
    const source = onBaseSide ? this.merge!.a : working;
    const range = source.state.selection.main;
    const doc = source.state.doc;
    const from = doc.lineAt(range.from).number - 1;
    let to = doc.lineAt(range.to).number - 1;
    // A selection ending at the start of a line does not include that line.
    if (!range.empty && to > from && range.to === doc.line(to + 1).from) to--;

    const edits = resetEdits(this.document.base, working.state.doc.toString(), {
      side: onBaseSide ? "a" : "b",
      from,
      to,
    });
    if (edits.length === 0) return false;
    working.dispatch({ changes: edits, userEvent: "input.reset" });
    return true;
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
    const doc = snapshot?.doc ?? document.working;
    const selection = snapshot && fits(snapshot.selection, snapshot.doc.length) ? snapshot.selection : undefined;
    const workingExtensions = [
      // Must come before basicSetup's own history so that this initial value wins.
      snapshot?.history ? historyField.init(() => snapshot.history as never) : [],
      this.sharedExtensions(document.path),
      EditorState.readOnly.of(!document.editable),
      this.supportSlot.of(this.support),
      keymap.of([
        { key: "Mod-s", run: () => (this.callbacks.onSave(), true) },
        { key: "Alt-r", run: () => this.resetSelectedLines() },
        { key: "F7", run: () => (this.nextChange(), true), shift: () => (this.previousChange(), true) },
      ]),
      EditorView.updateListener.of((update) => {
        if (update.docChanged) this.callbacks.onChange(update.state.doc.toString());
      }),
    ];

    if (document.plain) {
      this.unified = new EditorView({ parent: this.parent, doc, selection, extensions: workingExtensions });
    } else if (this.mode === "split") {
      this.merge = new MergeView({
        parent: this.parent,
        a: {
          doc: document.base,
          extensions: [
            this.sharedExtensions(document.path),
            EditorState.readOnly.of(true),
            // The base side cannot be typed in, so plain R is free to mean "reset".
            keymap.of([{ key: "r", run: () => this.resetSelectedLines() }]),
          ],
        },
        b: { doc, selection, extensions: workingExtensions },
        revertControls: document.editable ? "a-to-b" : undefined,
        highlightChanges: true,
        gutter: true,
        collapseUnchanged: COLLAPSE,
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
            gutter: true,
            syntaxHighlightDeletions: true,
            collapseUnchanged: COLLAPSE,
            mergeControls: document.editable ? resetButton : false,
          }),
        ],
      });
    }
    if (snapshot) this.restoreScroll(snapshot.scrollTop);
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
      // Only the two editors share the width; the splitter and revert column stay fixed.
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
      languageFor(path),
      prefersDark() ? oneDark : [],
      EditorView.domEventHandlers({
        focus: () => {
          this.callbacks.onFocus();
          return false;
        },
        contextmenu: (event) => {
          event.preventDefault();
          this.callbacks.onContextMenu(event.clientX, event.clientY);
          return true;
        },
      }),
    ];
  }
}

function fits(selection: EditorSelection, length: number) {
  return selection.ranges.every((range) => range.to <= length);
}

/** Unified mode offers one action per change: put it back to the base version. */
function resetButton(type: "reject" | "accept", action: (event: MouseEvent) => void): HTMLElement {
  const button = document.createElement("button");
  if (type === "accept") {
    // "Accept" would only hide the change from the view; nothing to accept here.
    button.hidden = true;
    return button;
  }
  button.textContent = "Reset";
  button.title = "Reset this change to the base version";
  button.className = "arnold-reset-chunk";
  button.onmousedown = (event) => {
    event.preventDefault();
    action(event);
  };
  return button;
}
