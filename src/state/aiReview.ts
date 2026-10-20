// Suggesting improvements with AI: Copilot reviews all the changes under
// review against the repository's own patterns and well-established
// practice, and answers with suggestions to replace lines. Each shows in the
// diff with Accept and Discard (editor/aiSuggestions.ts); the AI panel lists
// them with a summary.

import { type AiSuggestion, locate, replaceLines, setSuggestionActions, type SuggestionBasis } from "../editor/aiSuggestions";
import * as ipc from "../ipc/commands";
import type { ContextItem } from "../ipc/commands";
import { ai, ask, changesContext, failQuestion, type Message, patchSource, reviewedNote, setOpen, setReviewNote, showCode } from "./ai";
import { replaceSuggestions, setStatus, suggestionById } from "./aiSuggestions";
import { branchNames, editableViewFor, editBlocked, editOnDisk, revealInEditor, state, writableCheckout } from "./review";

/** The most suggestions taken from one answer. */
const MAX_SUGGESTIONS = 12;

/** `text` with its line numbers, as the review instructions describe ("12| code"). */
export function numbered(text: string): string {
  const lines = text.replace(/\r\n?/g, "\n").replace(/\n$/, "").split("\n");
  const width = String(lines.length).length;
  return lines.map((line, index) => `${String(index + 1).padStart(width)}| ${line}`).join("\n");
}

interface RawSuggestion {
  path: string;
  line: number;
  original: string;
  replacement: string;
  title: string;
  why: string;
  basis: SuggestionBasis;
}

/**
 * The summary and suggestions of a review answer: a JSON object, perhaps in
 * a Markdown fence or with words around it. Suggestions missing what they
 * need are left out. Throws when there is no such object.
 */
export function parseReview(text: string): { summary: string; suggestions: RawSuggestion[] } {
  const start = text.indexOf("{");
  const end = text.lastIndexOf("}");
  if (start === -1 || end < start) throw new Error("The answer has no suggestions in it.");
  let value: unknown;
  try {
    value = JSON.parse(text.slice(start, end + 1));
  } catch (error) {
    throw new Error(`The answer could not be read (${(error as Error).message}).`);
  }
  const object = (value ?? {}) as Record<string, unknown>;
  const string = (field: unknown) => (typeof field === "string" ? field : "");
  const list = Array.isArray(object.suggestions) ? (object.suggestions as Record<string, unknown>[]) : [];
  const suggestions: RawSuggestion[] = [];
  for (const item of list) {
    if (!item || typeof item !== "object") continue;
    const path = string(item.path).replace(/^\.?\//, "").replace(/^[ab]\//, "");
    const original = string(item.original);
    const replacement = string(item.replacement);
    if (!path || !original.trim() || original === replacement) continue;
    suggestions.push({
      path,
      line: Number(item.line) || 1,
      original,
      replacement,
      title: string(item.title).trim() || "Suggested change",
      why: string(item.why).trim(),
      basis: item.basis === "pattern" ? "pattern" : "proven",
    });
  }
  return { summary: string(object.summary).trim(), suggestions };
}

/** Whether there are changes to suggest improvements to. */
export const canSuggest = () =>
  state.source === "commit" ? !!state.checkout && !!state.work : !!state.comparison && !!state.repo;

let reviewCount = 0;

/** Asks Copilot for improvements to all the changes under review. */
export async function suggestImprovements() {
  try {
    if (!ai.open) setOpen(true);
    const source = patchSource(null);
    if (!source) return;
    const changes = await changesContext([], null, "All changes");
    const files = changes.files.length ? await ipc.reviewContext(source.cwd, source.of, changes.files) : [];
    const note = reviewedNote();
    const context: ContextItem[] = [...(note ? [note] : []), ...changes.items];
    for (const file of files) {
      context.push({
        label: file.changed ? `${file.path} (new version, whole file)` : `${file.path} (unchanged, beside the changed files)`,
        kind: "file",
        text: numbered(file.text),
      });
    }
    const count = changes.files.length;
    const chip = { label: `Suggest improvements · ${count} file${count === 1 ? "" : "s"}` };
    let question: string;
    if (state.source === "commit") question = "Suggest improvements to my uncommitted changes.";
    else if (state.source === "pullRequest" && state.pullRequest) question = `Suggest improvements to pull request !${state.pullRequest.id}.`;
    else question = `Suggest improvements to the changes on ${branchNames().head || "this branch"}.`;
    if (count === 0) {
      failQuestion("There are no text changes to suggest improvements to.");
      return;
    }
    const texts = new Map(files.filter((file) => file.changed).map((file) => [file.path, file.text]));
    const review = ++reviewCount;
    await ask(question, context, [chip], { purpose: "review", onAnswer: (text) => finish(text, texts, changes.files, review) });
  } catch (error) {
    failQuestion(String(error));
  }
}

/** Reads a review answer into suggestions; keeps those whose lines are in the code reviewed. */
function finish(text: string, texts: Map<string, string>, changed: string[], review: number): Partial<Message> {
  let parsed: ReturnType<typeof parseReview>;
  try {
    parsed = parseReview(text);
  } catch (error) {
    return { error: `${(error as Error).message} Ask again, or pick another model.` };
  }
  const kept: AiSuggestion[] = [];
  let dropped = 0;
  for (const raw of parsed.suggestions) {
    const path = changed.find((file) => file === raw.path) ?? changed.find((file) => file.endsWith(`/${raw.path}`));
    const fileText = path === undefined ? undefined : texts.get(path);
    const lines = fileText === undefined ? null : locate(fileText, raw.original, raw.line);
    if (!path || !lines || kept.length >= MAX_SUGGESTIONS) {
      dropped++;
      continue;
    }
    kept.push({ ...raw, path, line: lines.from, id: `${review}-${kept.length + 1}`, status: "open" });
  }
  replaceSuggestions(kept);
  setReviewNote(
    kept.length
      ? {
          label: "Improvements suggested earlier in this panel (a separate review)",
          kind: "note",
          text: kept.map((suggestion, index) => `${index + 1}. ${suggestion.path}:${suggestion.line} ${suggestion.title}: ${suggestion.why}`).join("\n"),
        }
      : null,
  );
  return { review: { summary: parsed.summary, ids: kept.map((suggestion) => suggestion.id), dropped } };
}

// ------------------------------------------------------------------ acting on one

/** Why `suggestion` cannot be accepted (its file cannot be edited here); null when it can. */
export function whyNotAccept(suggestion: AiSuggestion): string | null {
  if (editableViewFor(suggestion.path) || writableCheckout()) return null;
  if (state.source === "commit") return "Open a checkout to accept suggestions.";
  return editBlocked()?.reason ?? "The files of this review cannot be edited.";
}

/** Replaces the suggestion's lines with its code: in an editor that shows the file, like typing; else on disk. */
export async function accept(suggestion: AiSuggestion) {
  const view = editableViewFor(suggestion.path);
  const moved = "Its lines have changed since the review, so the suggestion no longer fits.";
  if (view) {
    const doc = view.state.doc;
    const lines = locate(doc.toString(), suggestion.original, suggestion.line);
    if (!lines) throw new Error(moved);
    let from = doc.line(lines.from).from;
    let to = doc.line(lines.to).to;
    const insert = suggestion.replacement.replace(/\r\n?/g, "\n").replace(/\n$/, "");
    if (suggestion.replacement === "") {
      // Removes the lines with their line end.
      if (lines.to < doc.lines) to = doc.line(lines.to + 1).from;
      else if (lines.from > 1) from = doc.line(lines.from - 1).to;
    }
    view.dispatch({ changes: { from, to, insert }, userEvent: "input.copilot" });
  } else {
    const checkout = writableCheckout();
    if (!checkout) throw new Error(whyNotAccept(suggestion) ?? "The file cannot be edited here.");
    await editOnDisk(checkout, suggestion.path, (text) => {
      const lines = locate(text, suggestion.original, suggestion.line);
      if (!lines) throw new Error(moved);
      return replaceLines(text, lines.from, lines.to, suggestion.replacement);
    });
  }
  setStatus(suggestion.id, "accepted");
}

export function discard(suggestion: AiSuggestion) {
  setStatus(suggestion.id, "discarded");
}

/** Shows the code of a suggestion, its card in sight when it is still open. */
export function revealSuggestion(id: string) {
  const suggestion = suggestionById(id);
  if (!suggestion) return;
  showCode(suggestion.path, suggestion.line);
  if (suggestion.status !== "open") return;
  // Once the file is on screen.
  let tries = 0;
  const reveal = () => {
    if (revealInEditor(suggestion.path, (target) => target.revealSuggestion(suggestion)) || ++tries > 20) return;
    setTimeout(reveal, 50);
  };
  setTimeout(reveal, 50);
}

export const openCount = (ids: string[]) => ids.filter((id) => suggestionById(id)?.status === "open").length;


setSuggestionActions({
  accept: (suggestion) => accept(suggestion),
  discard,
  whyNotAccept,
});
