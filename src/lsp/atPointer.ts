// Popups opened with the mouse show up where the pointer is: a hover card
// (type information, diagnostics) at the pointer, above or below the line it
// rests on, rather than at the start of the symbol or diagnostic it describes;
// the quick fixes menu (codeActions.ts) where it was clicked open. The place
// is kept relative to the line there, so the popup moves with the text when
// it scrolls or lines above fold or unfold, and CodeMirror keeps it within
// the window, flipping it above or below.

import { type Extension } from "@codemirror/state";
import { EditorView, getTooltip, hasHoverTooltips, type Rect, showTooltip, ViewPlugin } from "@codemirror/view";

/** A place in an editor: below the top of the line at `pos`, right of the left of its content. */
export interface DocPoint {
  pos: number;
  x: number;
  y: number;
}

/** The pointer's last place over each editor (popups included), in window coordinates. */
const pointers = new WeakMap<EditorView, { x: number; y: number }>();

// Captured, so a press is known before the button it lands on acts (the
// actions of a diagnostic's hover card run on mousedown).
const trackPointer = ViewPlugin.define((view) => {
  const track = (event: MouseEvent) => pointers.set(view, { x: event.clientX, y: event.clientY });
  view.dom.addEventListener("mousemove", track, true);
  view.dom.addEventListener("mousedown", track, true);
  return {
    destroy() {
      view.dom.removeEventListener("mousemove", track, true);
      view.dom.removeEventListener("mousedown", track, true);
    },
  };
});

/** Where the pointer was last over `view` (or its popups), in window coordinates. */
export function lastPointer(view: EditorView): { x: number; y: number } | undefined {
  return pointers.get(view);
}

/** `client` (window coordinates) as a place in the document of `view`, kept within the part of the editor in sight. */
export function docPoint(view: EditorView, client: { x: number; y: number }): DocPoint {
  const visible = view.scrollDOM.getBoundingClientRect();
  const x = Math.min(Math.max(client.x, visible.left), visible.right);
  const y = Math.min(Math.max(client.y, visible.top), visible.bottom - 1);
  const height = y - view.documentTop;
  // Whole lines, block widgets (deleted lines in the unified diff) included.
  const pos = view.lineBlockAtHeight(height).from;
  return { pos, x: x - view.contentDOM.getBoundingClientRect().left, y: height - view.lineBlockAt(pos).top };
}

/** Where `point` is in the window now: a point, or with `line` the height of the line or widget there. */
export function pointCoords(view: EditorView, point: DocPoint, line = false): Rect {
  const left = view.contentDOM.getBoundingClientRect().left + point.x;
  const height = view.lineBlockAt(point.pos).top + point.y;
  const top = view.documentTop;
  if (!line) return { left, right: left, top: top + height, bottom: top + height };
  const block = view.lineBlockAtHeight(height);
  return { left, right: left, top: top + block.top, bottom: top + block.bottom };
}

/** The place of each editor's open hover card. */
const cards = new WeakMap<EditorView, DocPoint>();

/**
 * Anchors each hover card at the pointer as it opens. All hover tooltips
 * share one card, which CodeMirror places at the start of the first one's
 * range unless the card has `getCoords`; the card keeps its place while it
 * stays open, as hovering over other words of a diagnostic swaps its parts.
 */
const hoverAtPointer = EditorView.updateListener.of((update) => {
  const view = update.view;
  const open = cards.get(view);
  if (!hasHoverTooltips(update.state)) cards.delete(view);
  else if (open && update.docChanged) open.pos = update.changes.mapPos(open.pos);
  if (open || !hasHoverTooltips(update.state)) return;
  const pointer = pointers.get(view);
  if (!pointer) return;
  const point = docPoint(view, pointer);
  for (const tooltip of view.state.facet(showTooltip)) {
    const card = tooltip && getTooltip(view, tooltip);
    // The card's own `getCoords` is a getter that passes on its parts'.
    if (card?.dom.classList.contains("cm-tooltip-hover")) {
      cards.set(view, point);
      Object.defineProperty(card, "getCoords", { value: () => pointCoords(view, point, true) });
    }
  }
});

/** For the language server's editors: hover cards at the pointer, and the pointer for the quick fixes menu. */
export const atPointer: Extension = [trackPointer, hoverAtPointer];
