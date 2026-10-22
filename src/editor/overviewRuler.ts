// Marks along the diff's scrollbar, as in VS Code and Rider: one for each
// change (and error, discussion and find match) where it is in the scrolling content, so
// changes can be seen and gone to while scrolling. Clicking a mark goes there.
// The marks are measured when an editor's text or heights change, or the
// scrolling area is resized, never by polling.

import { type BlockInfo, BlockType, EditorView, type ViewUpdate } from "@codemirror/view";

/** What a mark stands for; changes are colored like the change gutter. */
export type MarkKind = "inserted" | "deleted" | "modified" | "error" | "comment" | "search";

/** Something to mark: a stretch of the scrolling content, in pixels from its top. */
export interface OverviewBand {
  top: number;
  bottom: number;
  kind: MarkKind;
  /** The mark's tooltip. */
  title: string;
  /** Where a file of the all-files view will be, before it was ever loaded: drawn faint. */
  estimated?: boolean;
  /** Puts the cursor there and scrolls it to the middle of the screen; with `scroll` false, leaves the scrolling to the caller. */
  pick(scroll?: boolean): void;
}

/** A mark on the ruler, in pixels from its top; `band` is the index of the (first) band it shows. */
export interface OverviewMark {
  top: number;
  height: number;
  kind: MarkKind;
  estimated: boolean;
  band: number;
}

/** Marks are at least this high, so that one line of a long file still shows. */
export const MIN_MARK_HEIGHT = 3;

/**
 * Places `bands` of content `contentHeight` high on a ruler `trackHeight`
 * high, as the scrollbar maps them. Marks of the same kind that touch are
 * drawn as one, which picks the first of them.
 */
export function layoutMarks(
  bands: readonly OverviewBand[],
  contentHeight: number,
  trackHeight: number,
  minHeight = MIN_MARK_HEIGHT,
): OverviewMark[] {
  if (contentHeight <= 0 || trackHeight <= 0) return [];
  const scale = trackHeight / contentHeight;
  const order = bands.map((band, index) => ({ band, index })).sort((x, y) => x.band.top - y.band.top || x.index - y.index);
  const marks: OverviewMark[] = [];
  const last = new Map<string, OverviewMark>();
  for (const { band, index } of order) {
    const height = Math.min(Math.max((band.bottom - band.top) * scale, minHeight), trackHeight);
    const top = Math.max(0, Math.min(band.top * scale, trackHeight - height));
    const key = `${band.kind}${band.estimated ? "?" : ""}`;
    const previous = last.get(key);
    // Under a pixel apart reads as one mark anyway.
    if (previous && top <= previous.top + previous.height + 1) {
      previous.height = Math.max(previous.height, top + height - previous.top);
      continue;
    }
    const mark = { top, height, kind: band.kind, estimated: !!band.estimated, band: index };
    marks.push(mark);
    last.set(key, mark);
  }
  return marks;
}

const isWidget = (part: BlockInfo) => part.type === BlockType.WidgetBefore || part.type === BlockType.WidgetAfter;

/** The part of a line block that shows the line (or the folded lines it is in), without the block widgets above and below it. */
export function textPart(block: BlockInfo): BlockInfo {
  if (typeof block.type === "number") return block;
  return block.type.find((part) => !isWidget(part)) ?? block;
}

/** The block widget right above a line block's text: the unified view's removed lines, at their chunk. */
export function partAbove(block: BlockInfo): BlockInfo | null {
  if (typeof block.type === "number") return null;
  const parts = block.type;
  const text = parts.findIndex((part) => !isWidget(part));
  const above = text > 0 ? parts[text - 1] : null;
  return above?.type === BlockType.WidgetBefore ? above : null;
}

/** Where the lines from `from` to `to` (positions in them) of `view` are, in pixels from the top of its document. */
export function linesExtent(view: EditorView, from: number, to: number): { top: number; bottom: number } {
  return { top: textPart(view.lineBlockAt(from)).top, bottom: textPart(view.lineBlockAt(to)).bottom };
}

/** Rulers on screen, told when an editor in their scrolling area changes. */
const rulers = new Set<OverviewRuler>();

/** Tells the ruler over an editor that its marks may have moved: its text, chunks, errors or heights changed. */
export const overviewUpdates = EditorView.updateListener.of((update: ViewUpdate) => {
  if (rulers.size === 0) return;
  if (!update.heightChanged && !update.geometryChanged && !update.transactions.some((tr) => tr.effects.length > 0)) return;
  for (const ruler of rulers) if (ruler.covers(update.view)) ruler.schedule(update.view);
});

interface Measured {
  marks: OverviewMark[];
  bands: OverviewBand[];
  top: number;
  right: number;
  height: number;
}

/**
 * A strip over the vertical scrollbar of `scroller`, placed in `container`
 * (positioned, holding the scroller). `read` tells where the marks go; it
 * runs while measuring, so it may read the layout.
 */
export class OverviewRuler {
  readonly dom = document.createElement("div");
  private bands: OverviewBand[] = [];
  private frame = 0;
  private readonly resizer: ResizeObserver;

  constructor(
    private readonly scroller: HTMLElement,
    private readonly container: HTMLElement,
    private readonly read: () => OverviewBand[],
  ) {
    this.dom.className = "overview-ruler";
    this.dom.hidden = true;
    // Keeps the keyboard where it is; the mark's pick moves it.
    this.dom.addEventListener("mousedown", (event) => {
      const index = (event.target as HTMLElement).dataset.band;
      if (event.button !== 0 || index === undefined) return;
      event.preventDefault();
      this.bands[Number(index)]?.pick();
    });
    container.append(this.dom);
    this.resizer = new ResizeObserver(() => this.schedule());
    this.resizer.observe(scroller);
    rulers.add(this);
    this.schedule();
  }

  /** Whether `view` is in this ruler's scrolling area, or is that area (a unified view scrolls itself). */
  covers(view: EditorView): boolean {
    return this.scroller.contains(view.dom) || view.scrollDOM === this.scroller;
  }

  /** Measures again: in `view`'s next measure cycle when an editor asks, else in the next frame. */
  schedule(view?: EditorView) {
    if (view) {
      view.requestMeasure({ key: this, read: () => this.measure(), write: (measured) => this.draw(measured) });
    } else if (!this.frame) {
      this.frame = requestAnimationFrame(() => {
        this.frame = 0;
        this.draw(this.measure());
      });
    }
  }

  destroy() {
    cancelAnimationFrame(this.frame);
    this.resizer.disconnect();
    rulers.delete(this);
    this.dom.remove();
  }

  private measure(): Measured | null {
    const scroller = this.scroller;
    const height = scroller.clientHeight;
    // Nothing to scroll to: every change is on screen already.
    if (!scroller.isConnected || height === 0 || scroller.scrollHeight <= height) return null;
    const box = scroller.getBoundingClientRect();
    const frame = this.container.getBoundingClientRect();
    const bands = this.read();
    return {
      marks: layoutMarks(bands, scroller.scrollHeight, height),
      bands,
      top: box.top - frame.top + scroller.clientTop,
      right: frame.right - box.right,
      height,
    };
  }

  private draw(measured: Measured | null) {
    if (!rulers.has(this)) return;
    this.dom.hidden = measured === null;
    if (!measured) return;
    this.bands = measured.bands;
    Object.assign(this.dom.style, { top: `${measured.top}px`, right: `${measured.right}px`, height: `${measured.height}px` });
    const elements = this.dom.children;
    measured.marks.forEach((mark, index) => {
      const element = (elements[index] as HTMLElement | undefined) ?? this.dom.appendChild(document.createElement("div"));
      element.className = `overview-mark overview-${mark.kind}${mark.estimated ? " overview-estimated" : ""}`;
      element.style.top = `${mark.top}px`;
      element.style.height = `${mark.height}px`;
      element.dataset.band = String(mark.band);
      element.title = measured.bands[mark.band].title;
    });
    while (elements.length > measured.marks.length) elements[elements.length - 1].remove();
  }
}
