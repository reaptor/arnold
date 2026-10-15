// Pull request discussions in the diff: a card under the lines a thread is
// about. In the split view the other side gets an empty block of the same
// height, so the two sides stay lined up.

import { uncollapseUnchanged } from "@codemirror/merge";
import { type EditorState, type Range, StateEffect, StateField } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, WidgetType } from "@codemirror/view";
import type { PrThread } from "../ipc/commands";
import { ago } from "../components/CommitPicker";
import { openClickedLink, renderMarkdown } from "../components/Markdown";

/** Where a card (or the empty block matching one) goes in an editor. */
export interface ThreadMark {
  thread: PrThread;
  /** The card goes under this line (1-based). */
  line: number;
  /** Lines tinted as commented on, if any. */
  lines: { from: number; to: number } | null;
  /** Only an empty block as high as the card on the other side of the split view. */
  spacer: boolean;
}

const STATUS_NAMES: Record<string, string> = {
  active: "Active",
  pending: "Pending",
  fixed: "Resolved",
  wontFix: "Won't fix",
  closed: "Closed",
  byDesign: "By design",
};

/** Resolved, closed and the like: shown folded until opened. */
export const isOpen = (thread: PrThread) => thread.status === "active" || thread.status === "pending" || thread.status === "unknown";

export const statusName = (thread: PrThread) => STATUS_NAMES[thread.status] ?? "";

/** Folded or not, by thread id; outlives the cards, which come and go with scrolling. */
const folded = new Map<number, boolean>();
const isFolded = (thread: PrThread) => folded.get(thread.id) ?? !isOpen(thread);

/** Mentions are stored as `@<id>`; without a lookup of the person, the name is not known. */
const withoutMentionIds = (text: string) => text.replace(/@<[0-9a-f-]{36}>/gi, "@someone");

/**
 * A thread as a card: its status, then each comment. Clicking the head folds
 * it; `onResize` hears about that. Also used outside the editor.
 */
export function threadElement(thread: PrThread, onResize?: () => void): HTMLElement {
  const card = document.createElement("div");
  card.className = "thread";
  card.dataset.status = thread.status;

  const head = document.createElement("div");
  head.className = "thread-head";
  const status = document.createElement("span");
  status.className = "thread-status";
  status.textContent = statusName(thread);
  const first = thread.comments[0];
  const who = document.createElement("span");
  who.className = "thread-who";
  who.textContent = first.author;
  const summary = document.createElement("span");
  summary.className = "thread-summary";
  summary.textContent = withoutMentionIds(first.content).replace(/\s+/g, " ");
  const count = document.createElement("span");
  count.className = "thread-count";
  const replies = thread.comments.length - 1;
  count.textContent = replies > 0 ? `${replies} ${replies === 1 ? "reply" : "replies"}` : "";
  head.append(status, who, summary, count);

  const body = document.createElement("div");
  body.className = "thread-body";
  for (const comment of thread.comments) {
    const item = document.createElement("div");
    item.className = "thread-comment";
    if (comment.parentId !== 0) item.classList.add("reply");
    const meta = document.createElement("div");
    meta.className = "thread-meta";
    const author = document.createElement("strong");
    author.textContent = comment.author;
    const when = document.createElement("span");
    when.textContent = comment.published ? ago(comment.published) : "";
    when.title = comment.published ? new Date(comment.published).toLocaleString() : "";
    meta.append(author, when);
    if (comment.edited) {
      const edited = document.createElement("span");
      edited.textContent = "(edited)";
      meta.append(edited);
    }
    const content = document.createElement("div");
    content.className = "markdown thread-text";
    content.innerHTML = renderMarkdown(withoutMentionIds(comment.content));
    item.append(meta, content);
    body.append(item);
  }

  const show = () => card.classList.toggle("folded", isFolded(thread));
  head.onclick = () => {
    folded.set(thread.id, !isFolded(thread));
    show();
    onResize?.();
  };
  card.addEventListener("click", (event) => void openClickedLink(event));
  show();
  card.append(head, body);
  return card;
}

// ------------------------------------------------------------------ in CodeMirror

/** Card heights by thread id, and the empty blocks that copy them. */
const heights = new Map<number, number>();
const spacers = new Map<number, Set<{ dom: HTMLElement; view: EditorView }>>();

class ThreadWidget extends WidgetType {
  private observer: ResizeObserver | null = null;

  constructor(
    readonly thread: PrThread,
    private readonly key: string,
  ) {
    super();
  }

  eq(other: ThreadWidget) {
    return other.key === this.key;
  }

  toDOM(view: EditorView) {
    const wrapper = document.createElement("div");
    wrapper.className = "cm-thread";
    const card = threadElement(this.thread, () => view.requestMeasure());
    wrapper.append(card);
    // The other side's empty block follows the card's height.
    this.observer = new ResizeObserver(() => {
      const height = wrapper.offsetHeight;
      if (height === heights.get(this.thread.id)) return;
      heights.set(this.thread.id, height);
      for (const spacer of spacers.get(this.thread.id) ?? []) {
        spacer.dom.style.height = `${height}px`;
        spacer.view.requestMeasure();
      }
      view.requestMeasure();
    });
    this.observer.observe(wrapper);
    return wrapper;
  }

  destroy() {
    this.observer?.disconnect();
    this.observer = null;
  }

  // Clicks, selecting text and links in the card are the card's own.
  ignoreEvent() {
    return true;
  }
}

class SpacerWidget extends WidgetType {
  private entry: { dom: HTMLElement; view: EditorView } | null = null;

  constructor(readonly id: number) {
    super();
  }

  eq(other: SpacerWidget) {
    return other.id === this.id;
  }

  toDOM(view: EditorView) {
    const dom = document.createElement("div");
    dom.className = "cm-thread-spacer";
    dom.style.height = `${heights.get(this.id) ?? 0}px`;
    this.entry = { dom, view };
    let set = spacers.get(this.id);
    if (!set) spacers.set(this.id, (set = new Set()));
    set.add(this.entry);
    return dom;
  }

  destroy() {
    if (this.entry) spacers.get(this.id)?.delete(this.entry);
    this.entry = null;
  }
}

export const setThreadMarks = StateEffect.define<ThreadMark[]>();

const commentedLine = Decoration.line({ class: "cm-thread-line" });

function decorate(state: EditorState, marks: ThreadMark[]): DecorationSet {
  const doc = state.doc;
  const clamp = (line: number) => Math.min(Math.max(line, 1), doc.lines);
  const ranges: Range<Decoration>[] = [];
  for (const mark of marks) {
    const widget = mark.spacer
      ? new SpacerWidget(mark.thread.id)
      : new ThreadWidget(mark.thread, JSON.stringify(mark.thread));
    ranges.push(Decoration.widget({ widget, block: true, side: 1 }).range(doc.line(clamp(mark.line)).to));
    if (mark.lines) {
      for (let line = clamp(mark.lines.from); line <= clamp(mark.lines.to); line++) {
        ranges.push(commentedLine.range(doc.line(line).from));
      }
    }
  }
  return Decoration.set(ranges, true);
}

const threadField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(decorations, transaction) {
    for (const effect of transaction.effects) {
      if (effect.is(setThreadMarks)) return decorate(transaction.state, effect.value);
    }
    return decorations.map(transaction.changes);
  },
  provide: (field) => EditorView.decorations.from(field),
});

export const threadMarks = [threadField];

/** Shows the cards: unfolds the stretches of unchanged lines they would be hidden in. */
export function showThreadMarks(view: EditorView, marks: ThreadMark[]) {
  view.dispatch({ effects: setThreadMarks.of(marks) });
  const doc = view.state.doc;
  const positions = marks.map((mark) => doc.line(Math.min(Math.max(mark.line, 1), doc.lines)).from);
  if (positions.length === 0) return;
  const starts = new Set<number>();
  for (const source of view.state.facet(EditorView.decorations)) {
    if (typeof source === "function") continue;
    source.between(0, doc.length, (from, to, decoration) => {
      // The folded unchanged lines of @codemirror/merge.
      if ((decoration.spec.widget as { type?: string } | undefined)?.type !== "collapsed-unchanged-code") return;
      if (positions.some((pos) => pos >= from && pos <= to)) starts.add(from);
    });
  }
  if (starts.size > 0) view.dispatch({ effects: [...starts].map((start) => uncollapseUnchanged.of(start)) });
}
