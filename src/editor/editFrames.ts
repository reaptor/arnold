// Frames around the lines that differ from the branch's last commit: edits not
// committed yet, while a checked-out branch is compared with its files on disk.
// The diff itself shows everything the branch changes; the frames tell which
// of those changes are still uncommitted. Lines removed by an edit leave a
// bar where they were.
//
// The frames are a layer above the text, measured from the drawn lines like
// lineTints, so they follow folding, widgets and wrapping. styles.css gives
// them their color (--edited, as the "edited" marks of the dock).
//
// With `onSuggest` (a pull request's files) each framed change also gets a
// Suggest button on a row of its own under it, there from the first keystroke.

import { type EditorState, type Extension, type Range, StateEffect, StateField } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, layer, type LayerMarker, WidgetType } from "@codemirror/view";
import { type LineBlock, lineBlocks } from "./lineDiff";

/** Replaces the committed text the working text is compared with; null removes the frames. */
export const setCommitted = StateEffect.define<string | null>();

interface Edits {
  committed: string | null;
  /** Changed regions against `committed`, in 0-based line numbers. */
  blocks: readonly LineBlock[];
}

const blocksOf = (committed: string | null, doc: string): LineBlock[] =>
  committed === null ? [] : lineBlocks(committed.split("\n"), doc.split("\n"));

function editsField(initial: string | null) {
  return StateField.define<Edits>({
    create: (state) => ({ committed: initial, blocks: blocksOf(initial, state.doc.toString()) }),
    update(value, transaction) {
      let committed = value.committed;
      for (const effect of transaction.effects) if (effect.is(setCommitted)) committed = effect.value;
      if (committed === value.committed && !transaction.docChanged) return value;
      return { committed, blocks: blocksOf(committed, transaction.newDoc.toString()) };
    },
  });
}

class Frame implements LayerMarker {
  constructor(
    readonly left: number,
    readonly top: number,
    readonly width: number,
    readonly height: number,
    /** Lines removed: a bar instead of a frame. */
    readonly removed: boolean,
  ) {}

  draw() {
    const element = document.createElement("div");
    this.adjust(element);
    return element;
  }

  update(element: HTMLElement) {
    this.adjust(element);
    return true;
  }

  eq(other: LayerMarker): boolean {
    return (
      other instanceof Frame &&
      other.left === this.left &&
      other.top === this.top &&
      other.width === this.width &&
      other.height === this.height &&
      other.removed === this.removed
    );
  }

  private adjust(element: HTMLElement) {
    element.className = this.removed ? "cm-editFrame cm-editFrame-removed" : "cm-editFrame";
    element.style.left = `${this.left}px`;
    element.style.top = `${this.top}px`;
    element.style.width = `${this.width}px`;
    element.style.height = `${this.height}px`;
  }
}

/** A Suggest button on a row of its own under a framed change. */
class SuggestWidget extends WidgetType {
  constructor(
    readonly block: LineBlock,
    readonly onSuggest: (block: LineBlock) => void,
  ) {
    super();
  }

  eq(other: SuggestWidget) {
    const [a, b] = [this.block, other.block];
    return a.fromA === b.fromA && a.toA === b.toA && a.fromB === b.fromB && a.toB === b.toB;
  }

  toDOM() {
    const row = document.createElement("div");
    row.className = "cm-suggestRow";
    const button = document.createElement("button");
    button.type = "button";
    button.className = "cm-suggestButton";
    button.textContent = "Suggest";
    button.title = "Suggest this change on the pull request";
    // The editor would take the press as placing the cursor.
    button.addEventListener("mousedown", (event) => event.preventDefault());
    button.addEventListener("click", () => this.onSuggest(this.block));
    row.append(button);
    return row;
  }

  ignoreEvent() {
    return true;
  }
}

/** A Suggest row under the last line of each change (for removed lines, where they were). */
function suggestRows(state: EditorState, field: StateField<Edits>, onSuggest: (block: LineBlock) => void): DecorationSet {
  const doc = state.doc;
  const rows: Range<Decoration>[] = [];
  for (const block of state.field(field).blocks) {
    const widget = new SuggestWidget(block, onSuggest);
    // Under the last changed line, or the line before removed ones (above the first line at the top).
    const after = block.toB > block.fromB ? block.toB : block.fromB;
    if (after === 0) {
      rows.push(Decoration.widget({ widget, block: true, side: -1 }).range(0));
    } else if (after <= doc.lines) {
      rows.push(Decoration.widget({ widget, block: true, side: 1 }).range(doc.line(after).to));
    }
  }
  return Decoration.set(rows, true);
}

/** How tall the bar for removed lines is, centered on the line boundary. */
const REMOVED_BAR = 2;

function frames(view: EditorView, field: StateField<Edits>): Frame[] {
  const { blocks } = view.state.field(field);
  if (blocks.length === 0) return [];
  // Layer positions are relative to the scrolled content, as in lineTints.
  const scroller = view.scrollDOM.getBoundingClientRect();
  const baseLeft = scroller.left - view.scrollDOM.scrollLeft * view.scaleX;
  const baseTop = scroller.top - view.scrollDOM.scrollTop * view.scaleY;
  const content = view.contentDOM.getBoundingClientRect();
  const left = (content.left - baseLeft) / view.scaleX;
  const width = content.width / view.scaleX;
  const y = (screen: number) => (screen - baseTop) / view.scaleY;

  // The drawn lines (the viewport only), by 0-based line number.
  const drawn = new Map<number, DOMRect>();
  for (const element of view.contentDOM.querySelectorAll(":scope > .cm-line")) {
    const line = view.state.doc.lineAt(view.posAtDOM(element)).number - 1;
    drawn.set(line, element.getBoundingClientRect());
  }

  const result: Frame[] = [];
  for (const block of blocks) {
    if (block.fromB < block.toB) {
      let top: number | null = null;
      let bottom: number | null = null;
      for (let line = block.fromB; line < block.toB; line++) {
        const rect = drawn.get(line);
        if (!rect) continue;
        top = top === null ? rect.top : Math.min(top, rect.top);
        bottom = bottom === null ? rect.bottom : Math.max(bottom, rect.bottom);
      }
      if (top !== null && bottom !== null) result.push(new Frame(left, y(top), width, (bottom - top) / view.scaleY, false));
    } else {
      // Only removed: a bar at the top of the line that now follows, or under the last line.
      const after = drawn.get(block.fromB);
      const before = drawn.get(block.fromB - 1);
      const at = after ? after.top : before?.bottom;
      if (at !== undefined) result.push(new Frame(left, y(at) - REMOVED_BAR / 2, width, REMOVED_BAR, true));
    }
  }
  return result;
}

const layerUpdate = (update: { transactions: readonly unknown[]; viewportChanged: boolean; geometryChanged: boolean }) =>
  update.transactions.length > 0 || update.viewportChanged || update.geometryChanged;

/**
 * Frames the lines of the working text that differ from `committed` (the
 * file at the branch's last commit); none when it is null. With `onSuggest`
 * each framed change has a Suggest button, which calls it with the change.
 */
export function editFrames(committed: string | null, onSuggest?: (block: LineBlock) => void): Extension {
  const field = editsField(committed);
  return [
    field,
    layer({ above: true, class: "cm-editFrameLayer", markers: (view) => frames(view, field), update: layerUpdate }),
    onSuggest ? EditorView.decorations.compute([field], (state) => suggestRows(state, field, onSuggest)) : [],
  ];
}
