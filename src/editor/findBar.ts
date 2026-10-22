// Find in a file as in VS Code and Rider: Ctrl/Cmd+F opens a find box at the
// editor's top right, over the code, so the sides of the split view stay
// lined up. Every match is highlighted and the current one stands out; the
// box tells which match of how many it is. Typing finds as it goes, from where
// the cursor was; Enter / F3 / Ctrl/Cmd+G step to the next match and with
// Shift to the previous one, wrapping around. Matches in folded unchanged
// lines unfold when stepped to. Escape closes the box and leaves the match
// selected. Where the text can be edited, the box also replaces.
//
// It is @codemirror/search's panel, made by this module: the query, the match
// highlighting and replacing stay the library's.

import {
  closeSearchPanel,
  getSearchQuery,
  openSearchPanel,
  replaceAll,
  replaceNext,
  search,
  SearchQuery,
  searchPanelOpen,
  setSearchQuery,
} from "@codemirror/search";
import { EditorSelection, type EditorState, type Extension, Prec } from "@codemirror/state";
import { EditorView, keymap, type Panel, runScopeHandlers, type ViewUpdate } from "@codemirror/view";
import { revealUnchangedOnBothSides } from "./unchangedLines";

/** A match, from one position to another of the text. */
export interface Match {
  from: number;
  to: number;
}

/** Matches past this many are not counted. */
export const MOST_COUNTED = 9999;

/** The query's matches in `state`, at most `most` of them. A regular expression's empty matches are left out: there is nothing to show. */
export function matchesOf(query: SearchQuery, state: EditorState, most = MOST_COUNTED + 1): Match[] {
  const matches: Match[] = [];
  if (!query.valid) return matches;
  const cursor = query.getCursor(state);
  for (let next = cursor.next(); !next.done && matches.length < most; next = cursor.next()) {
    if (next.value.to > next.value.from) matches.push({ from: next.value.from, to: next.value.to });
  }
  return matches;
}

/** The first match starting at or after `pos`, else the first one (wrapping around). */
export function nextMatch(query: SearchQuery, state: EditorState, pos: number): Match | null {
  if (!query.valid) return null;
  let first: Match | null = null;
  const cursor = query.getCursor(state);
  for (let next = cursor.next(); !next.done; next = cursor.next()) {
    const { from, to } = next.value;
    if (to === from) continue;
    if (from >= pos) return { from, to };
    first ??= { from, to };
  }
  return first;
}

/** The last match starting before `pos`, else the last one (wrapping around). */
export function previousMatch(query: SearchQuery, state: EditorState, pos: number): Match | null {
  if (!query.valid) return null;
  let before: Match | null = null;
  let last: Match | null = null;
  const cursor = query.getCursor(state);
  for (let next = cursor.next(); !next.done; next = cursor.next()) {
    const { from, to } = next.value;
    if (to === from) continue;
    last = { from, to };
    if (from < pos) before = last;
  }
  return before ?? last;
}

/** What the box says about the matches: "3 of 12" on a match, else how many there are. */
export function countText(matches: readonly Match[], selection: Match, searched: boolean): string {
  if (!searched) return "";
  if (matches.length === 0) return "No results";
  const more = matches.length > MOST_COUNTED;
  const total = more ? `${MOST_COUNTED}+` : String(matches.length);
  const at = matches.findIndex((match) => match.from === selection.from && match.to === selection.to);
  if (at >= 0 && at < MOST_COUNTED) return `${at + 1} of ${total}`;
  return `${total} ${matches.length === 1 ? "result" : "results"}`;
}

/** The last query, so that a file opened next (or the other side) finds it too. */
let lastQuery = new SearchQuery({ search: "", literal: true });

/** A match selected while typing (or opening the box): typing goes on finding from where it started. */
const TYPED = "select.search.typed";

const scrollToMatch = (range: Match) => EditorView.scrollIntoView(EditorSelection.range(range.from, range.to), { y: "center" });

/** Selects `match`, unfolding the unchanged lines it is in first, and scrolls it to the middle (unless `scroll` is false). */
export function selectMatch(view: EditorView, match: Match, scroll = true, userEvent = "select.search") {
  revealUnchangedOnBothSides(view, match.from);
  view.dispatch({
    selection: EditorSelection.single(match.from, match.to),
    effects: scroll ? scrollToMatch(match) : [],
    userEvent,
  });
}

/** The matches highlighted in `view` (none while its find box is closed), at most `most` of them. */
export function shownMatches(view: EditorView, most: number): Match[] {
  return searchPanelOpen(view.state) ? matchesOf(getSearchQuery(view.state), view.state, most) : [];
}

/** Steps to the next match (or the previous one); opens the box when there is nothing to find yet. */
function step(forward: boolean) {
  return (view: EditorView) => {
    const query = getSearchQuery(view.state);
    if (!searchPanelOpen(view.state) || !query.valid) return openFind(view);
    const { from, to } = view.state.selection.main;
    const match = forward ? nextMatch(query, view.state, to) : previousMatch(query, view.state, from);
    if (match) selectMatch(view, match);
    panelOf(view)?.selectInput();
    return true;
  };
}

/** Opens the box (or goes to it) with the selected text, else the last text searched for. */
function openFind(view: EditorView): boolean {
  const selection = view.state.selection.main;
  const selected = view.state.sliceDoc(selection.from, selection.to);
  const fromSelection = !selection.empty && selected.length <= 200 && !selected.includes("\n");
  const current = getSearchQuery(view.state);
  const base = current.search ? current : lastQuery;
  const query = new SearchQuery({ ...specOf(base), search: fromSelection ? selected : base.search });
  if (!query.eq(current)) view.dispatch({ effects: setSearchQuery.of(query) });
  const panel = panelOf(view);
  if (panel) panel.focusInput();
  else openSearchPanel(view);
  // As VS Code does: the nearest match from the cursor is the current one.
  const match = nextMatch(query, view.state, selection.from);
  if (match) selectMatch(view, match, true, TYPED);
  const opened = panelOf(view);
  if (opened) opened.origin = selection.from;
  return true;
}

function specOf(query: SearchQuery) {
  const { search, caseSensitive, literal, regexp, wholeWord, replace } = query;
  return { search, caseSensitive, literal, regexp, wholeWord, replace };
}

const panels = new WeakMap<EditorView, FindPanel>();
const panelOf = (view: EditorView) => (searchPanelOpen(view.state) ? (panels.get(view) ?? null) : null);

const SVG = "http://www.w3.org/2000/svg";

/** A 16×16 line icon, as the app's own icons.tsx draws them. */
function icon(path: string): SVGSVGElement {
  const svg = document.createElementNS(SVG, "svg");
  svg.setAttribute("viewBox", "0 0 16 16");
  svg.setAttribute("class", "icon");
  svg.setAttribute("aria-hidden", "true");
  const shape = document.createElementNS(SVG, "path");
  shape.setAttribute("d", path);
  svg.append(shape);
  return svg;
}

function button(className: string, title: string, content: Node | string, onClick: () => void): HTMLButtonElement {
  const element = document.createElement("button");
  element.type = "button";
  element.className = className;
  element.title = title;
  element.setAttribute("aria-label", title);
  element.append(content);
  // Keeps the keyboard in the find field.
  element.addEventListener("mousedown", (event) => event.preventDefault());
  element.addEventListener("click", onClick);
  return element;
}

type Option = "caseSensitive" | "wholeWord" | "regexp";

const isMac = () => navigator.platform.toLowerCase().includes("mac");

class FindPanel implements Panel {
  readonly dom = document.createElement("div");
  readonly top = true;
  /** Where typing finds from: the cursor when the box opened, or the match last stepped to. */
  origin: number;
  private readonly bar = document.createElement("div");
  private readonly field = document.createElement("input");
  private readonly replaceField = document.createElement("input");
  private readonly replaceRow = document.createElement("div");
  private readonly expand: HTMLButtonElement;
  private readonly count = document.createElement("span");
  private readonly options: Record<Option, HTMLButtonElement>;
  private readonly stepButtons: HTMLButtonElement[];
  private matches: Match[] = [];
  private query: SearchQuery;
  private replacing = false;

  constructor(private readonly view: EditorView) {
    panels.set(view, this);
    this.query = getSearchQuery(view.state);
    this.origin = view.state.selection.main.from;
    const mod = isMac() ? "⌘" : "Ctrl+";
    const alt = isMac() ? "⌥" : "Alt+";

    this.dom.className = "cm-findPanel";
    this.bar.className = "cm-findBar";
    this.bar.addEventListener("keydown", (event) => this.keydown(event));

    this.field.className = "cm-findField";
    this.field.placeholder = "Find";
    this.field.setAttribute("aria-label", "Find");
    this.field.setAttribute("main-field", "true");
    this.field.spellcheck = false;
    this.field.value = this.query.search;
    this.field.addEventListener("input", () => this.typed());

    const toggle = (option: Option, label: string, title: string, key: string) =>
      button(`cm-findOption cm-findOption-${option}`, `${title} (${alt}${key})`, label, () => this.toggle(option));
    this.options = {
      caseSensitive: toggle("caseSensitive", "Aa", "Match case", "C"),
      wholeWord: toggle("wholeWord", "ab", "Words", "W"),
      regexp: toggle("regexp", ".*", "Regex", "R"),
    };
    const field = document.createElement("div");
    field.className = "cm-findInput";
    field.append(this.field, this.options.caseSensitive, this.options.wholeWord, this.options.regexp);

    this.count.className = "cm-findCount";
    this.count.setAttribute("aria-live", "polite");
    this.stepButtons = [
      button("cm-findButton", `Previous match (Shift+Enter, Shift+F3)`, icon("M8 13V3.5M3.5 7.5 8 3l4.5 4.5"), () =>
        step(false)(view),
      ),
      button("cm-findButton", `Next match (Enter, F3)`, icon("M8 3v9.5M3.5 8.5 8 13l4.5-4.5"), () => step(true)(view)),
    ];
    const close = button("cm-findButton", "Close (Escape)", icon("M4 4l8 8M12 4l-8 8"), () => closeSearchPanel(view));
    this.expand = button("cm-findButton cm-findExpand", `Replace (${mod}${isMac() ? "⌥F" : "H"})`, icon("M6 4l4 4-4 4"), () =>
      this.showReplace(!this.replacing),
    );

    const findRow = document.createElement("div");
    findRow.className = "cm-findRow";
    findRow.append(field, this.count, ...this.stepButtons, close);

    this.replaceField.className = "cm-findField cm-replaceField";
    this.replaceField.placeholder = "Replace";
    this.replaceField.setAttribute("aria-label", "Replace");
    this.replaceField.spellcheck = false;
    this.replaceField.value = this.query.replace;
    this.replaceField.addEventListener("input", () => this.commit({ replace: this.replaceField.value }));
    const replaceInput = document.createElement("div");
    replaceInput.className = "cm-findInput";
    replaceInput.append(this.replaceField);
    this.replaceRow.className = "cm-findRow cm-replaceRow";
    this.replaceRow.append(
      replaceInput,
      button("cm-findTextButton", "Replace (Enter)", "Replace", () => this.replaceOne()),
      button("cm-findTextButton", `Replace all (${mod}${isMac() ? "Enter" : "Alt+Enter"})`, "All", () => this.replaceEvery()),
    );

    const rows = document.createElement("div");
    rows.className = "cm-findRows";
    rows.append(findRow, this.replaceRow);
    this.bar.append(this.expand, rows);
    this.dom.append(this.bar);
    this.refresh(true);
  }

  mount() {
    this.focusInput();
  }

  destroy() {
    if (panels.get(this.view) === this) panels.delete(this.view);
  }

  update(update: ViewUpdate) {
    const query = getSearchQuery(update.state);
    const queryChanged = !query.eq(this.query);
    if (queryChanged) {
      this.query = query;
      if (this.field.value !== query.search) this.field.value = query.search;
      if (this.replaceField.value !== query.replace) this.replaceField.value = query.replace;
    }
    // Stepping or clicking elsewhere moves where typing finds from.
    if (update.selectionSet && !update.transactions.some((tr) => tr.isUserEvent(TYPED))) {
      this.origin = update.state.selection.main.from;
    }
    if (queryChanged || update.docChanged || update.selectionSet || update.startState.readOnly !== update.state.readOnly) {
      this.refresh(queryChanged || update.docChanged);
    }
  }

  focusInput() {
    this.field.focus();
    this.field.select();
  }

  selectInput() {
    if (this.field.ownerDocument.activeElement === this.field) this.field.select();
  }

  /** Shows the matches and options as they are now; counts the matches again when `recount`. */
  private refresh(recount: boolean) {
    const state = this.view.state;
    if (recount) this.matches = matchesOf(this.query, state);
    const searched = this.query.search !== "";
    this.count.textContent = countText(this.matches, state.selection.main, searched);
    this.bar.classList.toggle("cm-findBar-none", searched && this.matches.length === 0);
    this.bar.classList.toggle("cm-findBar-invalid", searched && !this.query.valid);
    for (const button of this.stepButtons) button.disabled = this.matches.length === 0;
    for (const option of Object.keys(this.options) as Option[]) {
      this.options[option].setAttribute("aria-pressed", String(this.query[option]));
    }
    const editable = !state.readOnly;
    this.expand.hidden = !editable;
    this.replaceRow.hidden = !editable || !this.replacing;
    this.bar.classList.toggle("cm-findBar-replacing", editable && this.replacing);
  }

  private commit(change: Partial<ReturnType<typeof specOf>>): SearchQuery {
    const query = new SearchQuery({ ...specOf(this.query), ...change });
    lastQuery = query;
    if (!query.eq(this.query)) this.view.dispatch({ effects: setSearchQuery.of(query) });
    return query;
  }

  /** Finds as the text is typed: the first match from where the cursor was. */
  private typed() {
    const query = this.commit({ search: this.field.value });
    const match = nextMatch(query, this.view.state, this.origin);
    if (match) selectMatch(this.view, match, true, TYPED);
  }

  private toggle(option: Option) {
    this.commit({ [option]: !this.query[option] });
    // As typing would: the matches may have moved.
    if (this.query.valid) this.typed();
  }

  showReplace(show: boolean) {
    this.replacing = show && !this.view.state.readOnly;
    this.refresh(false);
    if (this.replacing) this.replaceField.focus();
    else this.field.focus();
  }

  private replaceOne() {
    replaceNext(this.view);
    const match = this.view.state.selection.main;
    if (!match.empty) revealUnchangedOnBothSides(this.view, match.from);
  }

  private replaceEvery() {
    replaceAll(this.view);
  }

  private keydown(event: KeyboardEvent) {
    const mod = isMac() ? event.metaKey : event.ctrlKey;
    if (event.key === "Enter" && !event.isComposing) {
      event.preventDefault();
      if (event.target === this.replaceField) {
        if (mod || event.altKey) this.replaceEvery();
        else this.replaceOne();
      } else {
        step(!event.shiftKey)(this.view);
      }
    } else if (event.key === "Escape") {
      event.preventDefault();
      closeSearchPanel(this.view);
    } else if (event.altKey && !mod && !event.shiftKey && (event.code === "KeyC" || event.code === "KeyW" || event.code === "KeyR")) {
      // By the physical key: macOS types a character for Alt+letter.
      event.preventDefault();
      this.toggle(event.code === "KeyC" ? "caseSensitive" : event.code === "KeyW" ? "wholeWord" : "regexp");
    } else if (isReplaceShortcut(event)) {
      event.preventDefault();
      this.showReplace(true);
    } else if (runScopeHandlers(this.view, event, "search-panel")) {
      event.preventDefault();
    }
  }
}

/** Ctrl+H (Windows, Linux) or Cmd+Alt+F (macOS): find and replace, as in VS Code. */
function isReplaceShortcut(event: KeyboardEvent) {
  if (isMac()) return event.metaKey && event.altKey && !event.ctrlKey && event.code === "KeyF";
  return event.ctrlKey && !event.altKey && !event.metaKey && event.code === "KeyH";
}

/** Find in the file, with its keys; in place of @codemirror/search's own panel. */
export const findBar: Extension = [
  search({ top: true, literal: true, createPanel: (view) => new FindPanel(view), scrollToMatch }),
  Prec.high(
    keymap.of([
      { key: "Mod-f", run: openFind, scope: "editor search-panel", preventDefault: true },
      { key: "F3", run: step(true), shift: step(false), scope: "editor search-panel", preventDefault: true },
      { key: "Mod-g", run: step(true), shift: step(false), scope: "editor search-panel", preventDefault: true },
      {
        any: (view, event) => {
          if (!isReplaceShortcut(event)) return false;
          openFind(view);
          panelOf(view)?.showReplace(true);
          return true;
        },
      },
    ]),
  ),
];
