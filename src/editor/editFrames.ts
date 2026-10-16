// Frames around the lines that differ from the branch's last commit: edits not
// committed yet, while a checked-out branch is compared with its files on disk.
// The diff itself shows everything the branch changes; the frames tell which
// of those changes are still uncommitted. Lines removed by an edit leave a
// bar where they were.
//
// The frames are a layer above the text, measured from the drawn lines like
// lineTints, so they follow folding, widgets and wrapping. styles.css gives
// them their color (--edited, as the "edited" marks of the dock).

import { StateEffect, StateField, type Extension } from "@codemirror/state";
import { layer, type EditorView, type LayerMarker } from "@codemirror/view";
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

/**
 * Frames the lines of the working text that differ from `committed` (the
 * file at the branch's last commit); none when it is null.
 */
export function editFrames(committed: string | null): Extension {
  const field = editsField(committed);
  return [
    field,
    layer({
      above: true,
      class: "cm-editFrameLayer",
      markers: (view) => frames(view, field),
      update: (update) => update.transactions.length > 0 || update.viewportChanged || update.geometryChanged,
    }),
  ];
}
