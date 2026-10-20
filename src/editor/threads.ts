// Pull request discussions in the diff: a card under the lines a thread is
// about, with a field to reply, and a card to write a new comment in. In the
// split view the other side gets an empty block of the same height, so the
// two sides stay lined up.

import { uncollapseUnchanged } from "@codemirror/merge";
import { type EditorState, type Range, StateEffect, StateField } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, WidgetType } from "@codemirror/view";
import type { NewThreadPlace, PrComment, PrThread } from "../ipc/commands";
import { ago } from "../components/CommitPicker";
import { openInBrowser } from "../components/DevOpsSetup";
import { MARKDOWN_GUIDE, openClickedLink, renderMarkdown } from "../components/Markdown";
import { type AiSuggestion, suggestionCardId, suggestionElement } from "./aiSuggestions";

/** What goes under a line of an editor. */
export type ThreadMark =
  | {
      kind: "thread";
      thread: PrThread;
      /** The card goes under this line (1-based); 0 puts it above the first line. */
      line: number;
      /** Lines tinted as commented on, if any. */
      lines: { from: number; to: number } | null;
    }
  | { kind: "draft"; draft: NewThreadPlace; line: number; lines: { from: number; to: number } | null }
  /** Copilot's suggestion to replace `lines`. */
  | { kind: "ai"; suggestion: AiSuggestion; line: number; lines: { from: number; to: number } | null }
  /** An empty block as high as the card `id` on the other side of the split view. */
  | { kind: "spacer"; id: string; line: number };

export const threadCardId = (thread: PrThread) => `thread:${thread.id}`;
export const draftCardId = (draft: NewThreadPlace) => `draft:${draft.path}`;

/** What the cards do with what is written in them; set by the application. */
export interface ThreadActions {
  reply(thread: PrThread, text: string): Promise<void>;
  createThread(place: NewThreadPlace | null, text: string): Promise<void>;
  cancelDraft(place: NewThreadPlace): void;
  /** Deletes a comment of the token's owner (asking first). */
  deleteComment(thread: PrThread, comment: PrComment): Promise<void>;
}

let actions: ThreadActions | null = null;

export function setThreadActions(handlers: ThreadActions) {
  actions = handlers;
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
/** Threads being replied to. */
const replying = new Set<number>();
/** Text being written, by field, so it survives the card being redrawn. */
const drafts = new Map<string, string>();

/** Mentions are stored as `@<id>`; without a lookup of the person, the name is not known. */
const withoutMentionIds = (text: string) => text.replace(/@<[0-9a-f-]{36}>/gi, "@someone");

const isMac = navigator.platform.toLowerCase().includes("mac");

function element<K extends keyof HTMLElementTagNameMap>(tag: K, className: string, text?: string) {
  const node = document.createElement(tag);
  node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

interface ComposerOptions {
  /** Keeps the text while the field is redrawn. */
  key: string;
  placeholder: string;
  submitLabel: string;
  autofocus?: boolean;
  /** Rejects with what to show when it did not work. */
  onSubmit(text: string): Promise<void>;
  onCancel(): void;
  onResize?(): void;
}

/**
 * A field to write Markdown in, with a preview and a link to the Markdown
 * guide. Ctrl+Enter (⌘+Enter) sends, Escape cancels.
 */
export function composerElement(options: ComposerOptions): HTMLElement {
  const root = element("div", "composer");
  const bar = element("div", "composer-bar");
  const write = element("button", "composer-tab on", "Write");
  const preview = element("button", "composer-tab", "Preview");
  const help = element("a", "composer-help", "Markdown supported");
  help.href = MARKDOWN_GUIDE;
  help.title = "Markdown in Azure DevOps";
  help.onclick = (event) => {
    event.preventDefault();
    openInBrowser(MARKDOWN_GUIDE);
  };
  bar.append(write, preview, element("span", "composer-space"), help);

  const text = element("textarea", "composer-text");
  text.placeholder = options.placeholder;
  text.rows = 4;
  text.value = drafts.get(options.key) ?? "";
  const shown = element("div", "markdown composer-preview");
  shown.hidden = true;
  shown.addEventListener("click", (event) => void openClickedLink(event));
  const problem = element("div", "composer-error");
  problem.hidden = true;

  const actionsRow = element("div", "composer-actions");
  const hint = element("span", "composer-hint", `${isMac ? "⌘" : "Ctrl"}+Enter`);
  const cancel = element("button", "", "Cancel");
  const submit = element("button", "primary", options.submitLabel);
  actionsRow.append(hint, element("span", "composer-space"), cancel, submit);

  const resize = () => options.onResize?.();
  const update = () => {
    submit.disabled = text.value.trim() === "";
  };
  const showPreview = (on: boolean) => {
    write.classList.toggle("on", !on);
    preview.classList.toggle("on", on);
    text.hidden = on;
    shown.hidden = !on;
    if (on) shown.innerHTML = renderMarkdown(withoutMentionIds(text.value)) || "<p>Nothing to preview.</p>";
    else text.focus();
    resize();
  };
  const send = async () => {
    const value = text.value.trim();
    if (!value || submit.disabled) return;
    submit.disabled = cancel.disabled = text.disabled = true;
    submit.textContent = "Sending…";
    problem.hidden = true;
    try {
      await options.onSubmit(value);
      drafts.delete(options.key);
    } catch (error) {
      problem.textContent = String(error);
      problem.hidden = false;
      submit.textContent = options.submitLabel;
      cancel.disabled = text.disabled = false;
      update();
      resize();
    }
  };
  const close = () => {
    drafts.delete(options.key);
    options.onCancel();
  };

  write.onclick = () => showPreview(false);
  preview.onclick = () => showPreview(true);
  cancel.onclick = close;
  submit.onclick = () => void send();
  text.oninput = () => {
    drafts.set(options.key, text.value);
    update();
  };
  text.onkeydown = (event) => {
    event.stopPropagation();
    if (event.key === "Enter" && (isMac ? event.metaKey : event.ctrlKey)) {
      event.preventDefault();
      void send();
    } else if (event.key === "Escape") {
      event.preventDefault();
      close();
    }
  };
  update();
  root.append(bar, text, shown, problem, actionsRow);
  if (options.autofocus) requestAnimationFrame(() => text.focus({ preventScroll: true }));
  return root;
}

/**
 * A thread as a card: its status, then each comment, then a way to reply.
 * Clicking the head folds it; `onResize` hears about changes of height.
 * Also used outside the editor.
 */
export function threadElement(thread: PrThread, onResize?: () => void): HTMLElement {
  const card = element("div", "thread");
  card.dataset.status = thread.status;

  const head = element("div", "thread-head");
  const first = thread.comments[0];
  const replies = thread.comments.length - 1;
  head.append(
    element("span", "thread-status", statusName(thread)),
    element("span", "thread-who", first.author),
    element("span", "thread-summary", withoutMentionIds(first.content).replace(/\s+/g, " ")),
    element("span", "thread-count", replies > 0 ? `${replies} ${replies === 1 ? "reply" : "replies"}` : ""),
  );

  const body = element("div", "thread-body");
  for (const comment of thread.comments) {
    const item = element("div", "thread-comment");
    if (comment.parentId !== 0) item.classList.add("reply");
    const meta = element("div", "thread-meta");
    const when = element("span", "", comment.published ? ago(comment.published) : "");
    when.title = comment.published ? new Date(comment.published).toLocaleString() : "";
    meta.append(element("strong", "", comment.author), when);
    if (comment.edited) meta.append(element("span", "", "(edited)"));
    if (comment.you && actions) {
      const suggestion = comment === first && isSuggestion(thread);
      const remove = element("button", "thread-delete", suggestion ? "Delete suggestion" : "Delete");
      remove.title = suggestion ? "Delete this suggestion from the pull request" : "Delete this comment from the pull request";
      remove.onclick = (event) => {
        event.stopPropagation();
        void actions!.deleteComment(thread, comment);
      };
      meta.append(remove);
    }
    const content = element("div", "markdown thread-text");
    content.innerHTML = renderMarkdown(withoutMentionIds(comment.content));
    item.append(meta, content);
    body.append(item);
  }

  const footer = element("div", "thread-footer");
  const showFooter = () => {
    if (!actions) return footer.replaceChildren();
    if (!replying.has(thread.id)) {
      const reply = element("button", "thread-reply", "Reply");
      reply.onclick = () => {
        replying.add(thread.id);
        showFooter();
        onResize?.();
      };
      return footer.replaceChildren(reply);
    }
    footer.replaceChildren(
      composerElement({
        key: `reply:${thread.id}`,
        placeholder: "Reply…",
        submitLabel: "Reply",
        autofocus: true,
        onSubmit: async (text) => {
          // Before the reply comes back with the thread, so its new card shows no field.
          replying.delete(thread.id);
          try {
            await actions!.reply(thread, text);
          } catch (error) {
            replying.add(thread.id);
            throw error;
          }
        },
        onCancel: () => {
          replying.delete(thread.id);
          showFooter();
          onResize?.();
        },
        onResize,
      }),
    );
  };
  showFooter();
  body.append(footer);

  const show = () => card.classList.toggle("folded", isFolded(thread));
  head.onclick = () => {
    folded.set(thread.id, !isFolded(thread));
    show();
    onResize?.();
  };
  body.addEventListener("click", (event) => void openClickedLink(event));
  show();
  card.append(head, body);
  return card;
}

/** Where a new comment goes, in words: "line 14", "lines 14–16". */
export function placeName(place: NewThreadPlace): string {
  const range = place.lines;
  if (!range) return "the whole file";
  const lines = range.startLine === range.endLine ? `line ${range.startLine}` : `lines ${range.startLine}–${range.endLine}`;
  return range.side === "base" ? `${lines} before the change` : lines;
}

/** A card to write a new comment about lines of a file in. */
function draftElement(draft: NewThreadPlace, onResize: () => void): HTMLElement {
  const card = element("div", "thread draft");
  const head = element("div", "thread-head");
  head.append(element("span", "thread-status", "New comment"), element("span", "thread-summary", placeName(draft)));
  card.append(
    head,
    composerElement({
      key: draftCardId(draft),
      placeholder: "Add a comment…",
      submitLabel: "Comment",
      autofocus: true,
      onSubmit: (text) => actions?.createThread(draft, text) ?? Promise.reject("Comments cannot be sent here."),
      onCancel: () => actions?.cancelDraft(draft),
      onResize,
    }),
  );
  return card;
}

// ------------------------------------------------------------------ in CodeMirror

/** Card heights by card id, and the empty blocks that copy them. */
const heights = new Map<string, number>();
const spacers = new Map<string, Set<{ dom: HTMLElement; view: EditorView }>>();

class CardWidget extends WidgetType {
  private observer: ResizeObserver | null = null;

  constructor(
    private readonly id: string,
    /** Tells cards apart that look different. */
    private readonly key: string,
    private readonly build: (onResize: () => void) => HTMLElement,
  ) {
    super();
  }

  eq(other: CardWidget) {
    return other.key === this.key;
  }

  toDOM(view: EditorView) {
    const wrapper = element("div", "cm-thread");
    wrapper.append(this.build(() => view.requestMeasure()));
    // The other side's empty block follows the card's height.
    this.observer = new ResizeObserver(() => {
      const height = wrapper.offsetHeight;
      if (height === heights.get(this.id)) return;
      heights.set(this.id, height);
      for (const spacer of spacers.get(this.id) ?? []) {
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

  // Clicks, typing, selecting text and links in the card are the card's own.
  ignoreEvent() {
    return true;
  }
}

class SpacerWidget extends WidgetType {
  private entry: { dom: HTMLElement; view: EditorView } | null = null;

  constructor(readonly id: string) {
    super();
  }

  eq(other: SpacerWidget) {
    return other.id === this.id;
  }

  toDOM(view: EditorView) {
    const dom = element("div", "cm-thread-spacer");
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

/** The lines a Copilot suggestion would replace: framed in the accent color. */
const aiLine = (first: boolean, last: boolean) =>
  Decoration.line({ class: `cm-ai-line${first ? " cm-ai-first" : ""}${last ? " cm-ai-last" : ""}` });

/** Whether `thread` suggests a change: its first comment holds a ```suggestion block. */
export const isSuggestion = (thread: PrThread) => /^\s*(`{3,}|~{3,})\s*suggestion\b/m.test(thread.comments[0]?.content ?? "");

/**
 * The lines an open suggestion would replace: framed in amber like edits,
 * the card under them showing the suggested code in the same frame.
 */
const suggestedLine = (first: boolean, last: boolean) =>
  Decoration.line({
    class: `cm-suggested-line${first ? " cm-suggested-first" : ""}${last ? " cm-suggested-last" : ""}`,
  });

function widgetOf(mark: ThreadMark): WidgetType {
  switch (mark.kind) {
    case "spacer":
      return new SpacerWidget(mark.id);
    case "draft":
      return new CardWidget(draftCardId(mark.draft), JSON.stringify(mark.draft), (onResize) =>
        draftElement(mark.draft, onResize),
      );
    case "thread":
      return new CardWidget(threadCardId(mark.thread), JSON.stringify(mark.thread), (onResize) =>
        threadElement(mark.thread, onResize),
      );
    case "ai":
      return new CardWidget(suggestionCardId(mark.suggestion), JSON.stringify(mark.suggestion), () => suggestionElement(mark.suggestion));
  }
}

function decorate(state: EditorState, marks: ThreadMark[]): DecorationSet {
  const doc = state.doc;
  const clamp = (line: number) => Math.min(Math.max(line, 1), doc.lines);
  const ranges: Range<Decoration>[] = [];
  for (const mark of marks) {
    const widget = widgetOf(mark);
    ranges.push(
      mark.line === 0
        ? Decoration.widget({ widget, block: true, side: -1 }).range(0)
        : Decoration.widget({ widget, block: true, side: 1 }).range(doc.line(clamp(mark.line)).to),
    );
    if (mark.kind !== "spacer" && mark.lines) {
      const [from, to] = [clamp(mark.lines.from), clamp(mark.lines.to)];
      const suggested = mark.kind === "thread" && isSuggestion(mark.thread) && isOpen(mark.thread);
      for (let line = from; line <= to; line++) {
        const decoration =
          mark.kind === "ai" ? aiLine(line === from, line === to) : suggested ? suggestedLine(line === from, line === to) : commentedLine;
        ranges.push(decoration.range(doc.line(line).from));
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
  // Cards above the first line are never folded away.
  const positions = marks.filter((mark) => mark.line > 0).map((mark) => doc.line(Math.min(mark.line, doc.lines)).from);
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
