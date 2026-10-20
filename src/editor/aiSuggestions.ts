// Copilot's suggestions in the diff: a card under the lines a suggestion
// would replace, saying why, showing the new code, with Accept and Discard.
// Where a suggestion goes is found by its text (`locate`), so edits above it
// move it along and edits to its lines make it go away.

import { openClickedLink, renderMarkdown } from "../components/Markdown";

/** Why a suggestion is made: the code departs from the repository's own convention, or a well-established reason. */
export type SuggestionBasis = "pattern" | "proven";

export interface AiSuggestion {
  id: string;
  path: string;
  /** Where `original` started in the text reviewed (1-based); the nearest match wins. */
  line: number;
  /** The lines to replace, as they were reviewed. */
  original: string;
  /** What to put in their place; "" removes them. */
  replacement: string;
  title: string;
  /** Why, in Markdown. */
  why: string;
  basis: SuggestionBasis;
  status: "open" | "accepted" | "discarded";
}

export const BASIS_NAMES: Record<SuggestionBasis, string> = {
  pattern: "Repository pattern",
  proven: "Proven practice",
};

export const BASIS_TITLES: Record<SuggestionBasis, string> = {
  pattern: "The change departs from how the existing code of this repository does it",
  proven: "A well-established reason: a bug, an unhandled case, security, performance or a documented best practice",
};

/** Lines of `text`, without line ends. */
const linesOf = (text: string) => text.replace(/\r\n?/g, "\n").replace(/\n$/, "").split("\n");

/**
 * The lines (1-based, inclusive) of `text` that hold `original`, the match
 * nearest to `near`; null when it is not there. Trailing spaces are ignored,
 * and, when nothing matches that way, indentation too.
 */
export function locate(text: string, original: string, near: number): { from: number; to: number } | null {
  const wanted = linesOf(original);
  if (wanted.length === 0 || wanted.every((line) => !line.trim())) return null;
  const lines = linesOf(text);
  for (const normalize of [(line: string) => line.trimEnd(), (line: string) => line.trim()]) {
    const target = wanted.map(normalize);
    let best: number | null = null;
    for (let start = 0; start + target.length <= lines.length; start++) {
      let matches = true;
      for (let offset = 0; offset < target.length && matches; offset++) {
        matches = normalize(lines[start + offset]) === target[offset];
      }
      if (matches && (best === null || Math.abs(start + 1 - near) < Math.abs(best + 1 - near))) best = start;
    }
    if (best !== null) return { from: best + 1, to: best + target.length };
  }
  return null;
}

/** `text` with lines `from`–`to` (1-based) replaced by `replacement`, keeping the text's line ends. */
export function replaceLines(text: string, from: number, to: number, replacement: string): string {
  const eol = text.includes("\r\n") ? "\r\n" : "\n";
  const lines = text.split(/\r\n|\n/);
  const inserted = replacement === "" ? [] : linesOf(replacement);
  lines.splice(from - 1, to - from + 1, ...inserted);
  return lines.join(eol);
}

/** What the cards do; set by the application. */
export interface SuggestionActions {
  accept(suggestion: AiSuggestion): Promise<void>;
  discard(suggestion: AiSuggestion): void;
  /** Why the suggestion cannot be accepted here; null when it can. */
  whyNotAccept(suggestion: AiSuggestion): string | null;
}

let actions: SuggestionActions | null = null;

export function setSuggestionActions(handlers: SuggestionActions) {
  actions = handlers;
}

export const suggestionCardId = (suggestion: AiSuggestion) => `ai:${suggestion.id}`;

function element<K extends keyof HTMLElementTagNameMap>(tag: K, className: string, text?: string) {
  const node = document.createElement(tag);
  node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

/** The lines removed and added, as a small diff. */
function changeElement(suggestion: AiSuggestion): HTMLElement {
  const pre = element("pre", "ai-suggestion-code");
  for (const line of linesOf(suggestion.original)) pre.append(element("div", "removed", line || " "));
  if (suggestion.replacement !== "") for (const line of linesOf(suggestion.replacement)) pre.append(element("div", "added", line || " "));
  return pre;
}

/** A suggestion as a card. Also used outside the editor. */
export function suggestionElement(suggestion: AiSuggestion): HTMLElement {
  const card = element("div", "thread ai-suggestion");
  const head = element("div", "thread-head");
  const basis = element("span", `ai-basis ${suggestion.basis}`, BASIS_NAMES[suggestion.basis]);
  basis.title = BASIS_TITLES[suggestion.basis];
  head.append(element("span", "thread-status", "✦ Copilot suggests"), element("span", "ai-suggestion-title", suggestion.title), basis);

  const why = element("div", "markdown thread-text ai-suggestion-why");
  why.innerHTML = renderMarkdown(suggestion.why);
  why.addEventListener("click", (event) => void openClickedLink(event));

  const footer = element("div", "ai-suggestion-actions");
  const accept = element("button", "primary", "Accept");
  const discard = element("button", "", "Discard");
  const problem = element("span", "ai-suggestion-problem");
  const blocked = actions ? actions.whyNotAccept(suggestion) : "Suggestions cannot be accepted here.";
  accept.disabled = blocked !== null;
  accept.title = blocked ?? "Replace the lines with the suggested code, like typing would";
  discard.title = "Hide this suggestion";
  accept.onclick = async () => {
    accept.disabled = discard.disabled = true;
    problem.textContent = "";
    try {
      await actions?.accept(suggestion);
    } catch (error) {
      problem.textContent = String(error);
      accept.disabled = discard.disabled = false;
    }
  };
  discard.onclick = () => actions?.discard(suggestion);
  footer.append(accept, discard, problem);

  card.append(head, why, changeElement(suggestion), footer);
  return card;
}
