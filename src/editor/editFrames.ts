// Frames around the lines that differ from the branch's last commit: edits not
// committed yet, while a checked-out branch is compared with its files on disk.
// The diff itself shows everything the branch changes; the frames tell which
// of those changes are still uncommitted. Lines removed by an edit leave a
// bar where they were.
//
// A new line equal to its neighbour (an empty line pressed in next to another)
// could be either of the two. The frame goes on the one just typed (where the
// cursor is, as the merge view's own diff does while typing) and stays there.
// One frame surrounds all of an edit: empty lines inside it that happen to
// match the committed text are taken in, and so is the empty line Enter put
// the cursor on next to it.
//
// The frames are a layer above the text, measured from the drawn lines like
// lineTints, so they follow folding, widgets and wrapping. In the unified
// view the lines an edit removed are shown (red) above the lines it added:
// the frame takes them in, so that all of the edit's text is inside it.
// styles.css gives them their color (--edited, as the "edited" marks of the dock).
//
// With `onSuggest` (a pull request's files) each framed change also gets a
// Suggest button on a row of its own under it, there from the first keystroke.
// Until suggested such a change is a draft that only this computer has: its
// frame is dashed (posted suggestions have a solid one) and its row says so.

import { type ChangeSet, type EditorState, type Extension, type Range, StateEffect, StateField, type Text } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, layer, type LayerMarker, WidgetType } from "@codemirror/view";
import { joinAcrossBlankLines, type LineBlock, lineBlocks, placeBlocks, takeInBlankLines } from "./lineDiff";

/** Replaces the committed text the working text is compared with; null removes the frames. */
export const setCommitted = StateEffect.define<string | null>();

interface Edits {
  committed: string | null;
  /** Changed regions against `committed`, in 0-based line numbers. */
  blocks: readonly LineBlock[];
}

/**
 * What the last edit changed: its line ranges in the new text, the line the
 * cursor ended on (null when the text was not edited), and the earlier
 * blocks moved along with it.
 */
interface Edit {
  lines: { from: number; to: number }[];
  cursor: number | null;
  before: readonly LineBlock[];
}

/** The 0-based line `line` of `doc` (or its end) in the text `changes` make of it. */
function mapLine(line: number, doc: Text, changes: ChangeSet, newDoc: Text): number {
  if (line >= doc.lines) return newDoc.lines;
  return newDoc.lineAt(changes.mapPos(doc.line(line + 1).from, -1)).number - 1;
}

function editOf(before: readonly LineBlock[], doc: Text, changes: ChangeSet, newDoc: Text, head: number): Edit {
  const lines: Edit["lines"] = [];
  changes.iterChangedRanges((_fromA, _toA, fromB, toB) => {
    lines.push({ from: newDoc.lineAt(fromB).number - 1, to: newDoc.lineAt(toB).number - 1 });
  });
  const moved = before.map((block) => {
    const fromB = mapLine(block.fromB, doc, changes, newDoc);
    // An empty block (removed lines) stays empty.
    const toB = block.toB === block.fromB ? fromB : Math.max(fromB, mapLine(block.toB, doc, changes, newDoc));
    return { ...block, fromB, toB };
  });
  return { lines, cursor: newDoc.lineAt(head).number - 1, before: moved };
}

/**
 * Of the places a sliding block could be: one on the lines just edited, and
 * of those the one it was at before (or overlapping it, as when another
 * empty line is added to new ones), else the last (the line the cursor went
 * to). Untouched by the edit, where it was before, else where the diff put it.
 */
function pickPosition(edit: Edit) {
  const same = (p: LineBlock) => edit.before.some((b) => b.fromB === p.fromB && b.toB === p.toB);
  const overlapping = (p: LineBlock) => edit.before.some((b) => b.fromB < p.toB && p.fromB < b.toB);
  return (positions: LineBlock[], diffed: LineBlock): LineBlock => {
    const touched = positions.filter((p) =>
      edit.lines.some((range) =>
        p.fromB === p.toB ? range.from <= p.fromB && p.fromB <= range.to + 1 : range.from < p.toB && p.fromB <= range.to,
      ),
    );
    if (touched.length > 0) return touched.find(same) ?? touched.find(overlapping) ?? touched[touched.length - 1];
    return positions.find(same) ?? diffed;
  };
}

function blocksOf(committed: string | null, doc: string, edit: Edit | null = null): LineBlock[] {
  if (committed === null) return [];
  const aLines = committed.split("\n");
  const bLines = doc.split("\n");
  const blocks = lineBlocks(aLines, bLines);
  if (!edit) return joinAcrossBlankLines(blocks, bLines);
  // The empty line the edit put the cursor on, and those framed before, stay framed.
  const edited = (line: number) => edit.lines.some((range) => range.from <= line && line <= range.to);
  const framedBefore = (line: number) => edit.before.some((block) => block.fromB <= line && line < block.toB);
  const placed = placeBlocks(blocks, aLines, bLines, pickPosition(edit));
  const grown = takeInBlankLines(placed, aLines, bLines, (line) => (line === edit.cursor && edited(line)) || framedBefore(line));
  return joinAcrossBlankLines(grown, bLines);
}

function editsField(initial: string | null) {
  return StateField.define<Edits>({
    create: (state) => ({ committed: initial, blocks: blocksOf(initial, state.doc.toString()) }),
    update(value, transaction) {
      let committed = value.committed;
      for (const effect of transaction.effects) if (effect.is(setCommitted)) committed = effect.value;
      if (committed === value.committed && !transaction.docChanged) return value;
      const edit = transaction.docChanged
        ? editOf(value.blocks, transaction.startState.doc, transaction.changes, transaction.newDoc, transaction.newSelection.main.head)
        : { lines: [], cursor: null, before: value.blocks };
      return { committed, blocks: blocksOf(committed, transaction.newDoc.toString(), edit) };
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
    /** A draft not suggested yet: dashed. */
    readonly pending: boolean,
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
      other.removed === this.removed &&
      other.pending === this.pending
    );
  }

  private adjust(element: HTMLElement) {
    element.className = `cm-editFrame${this.removed ? " cm-editFrame-removed" : ""}${this.pending ? " cm-editFrame-pending" : ""}`;
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
    const note = document.createElement("span");
    note.className = "cm-suggestNote";
    note.textContent = "Draft · not on the pull request yet";
    note.title = "This edit is only in your worktree of the pull request. Nobody else sees it until you press Suggest.";
    row.append(button, note);
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

function frames(view: EditorView, field: StateField<Edits>, pending: boolean): Frame[] {
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

  // The drawn lines (the viewport only), by 0-based line number, and the
  // removed lines the unified view shows above a line (or after the last).
  const drawn = new Map<number, DOMRect>();
  const removedAbove = new Map<number, DOMRect>();
  let removed: DOMRect | null = null;
  for (const element of view.contentDOM.children) {
    if (element.classList.contains("cm-deletedChunk")) {
      removed = element.getBoundingClientRect();
    } else if (element.classList.contains("cm-line")) {
      const line = view.state.doc.lineAt(view.posAtDOM(element)).number - 1;
      drawn.set(line, element.getBoundingClientRect());
      if (removed) removedAbove.set(line, removed);
      removed = null;
    }
  }
  if (removed) removedAbove.set(view.state.doc.lines, removed);

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
      // Lines replaced: the removed ones shown above are in the frame too.
      const replaced = block.toA > block.fromA ? removedAbove.get(block.fromB) : undefined;
      if (replaced && top !== null) top = Math.min(top, replaced.top);
      if (top !== null && bottom !== null) result.push(new Frame(left, y(top), width, (bottom - top) / view.scaleY, false, pending));
    } else if (removedAbove.has(block.fromB)) {
      // Only removed, and shown: framed like added lines.
      const shown = removedAbove.get(block.fromB)!;
      result.push(new Frame(left, y(shown.top), width, shown.height / view.scaleY, false, pending));
    } else {
      // Only removed: a bar at the top of the line that now follows, or under the last line.
      const after = drawn.get(block.fromB);
      const before = drawn.get(block.fromB - 1);
      const at = after ? after.top : before?.bottom;
      if (at !== undefined) result.push(new Frame(left, y(at) - REMOVED_BAR / 2, width, REMOVED_BAR, true, pending));
    }
  }
  return result;
}

const layerUpdate = (update: { transactions: readonly unknown[]; viewportChanged: boolean; geometryChanged: boolean }) =>
  update.transactions.length > 0 || update.viewportChanged || update.geometryChanged;

/**
 * Frames the lines of the working text that differ from `committed` (the
 * file at the branch's last commit); none when it is null. With `onSuggest`
 * each framed change is a draft suggestion (dashed, marked as not posted)
 * with a Suggest button, which calls it with the change.
 */
export function editFrames(committed: string | null, onSuggest?: (block: LineBlock) => void): Extension {
  const field = editsField(committed);
  return [
    field,
    layer({ above: true, class: "cm-editFrameLayer", markers: (view) => frames(view, field, !!onSuggest), update: layerUpdate }),
    onSuggest ? EditorView.decorations.compute([field], (state) => suggestRows(state, field, onSuggest)) : [],
  ];
}
