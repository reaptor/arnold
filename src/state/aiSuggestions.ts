// Copilot's suggestions from the last "Suggest improvements": kept here so
// that the editors of every mode can show the open ones (review.ts and
// commitMode.ts read `openFor`, and push changes to the editors on screen).

import { createStore, produce } from "solid-js/store";
import type { AiSuggestion } from "../editor/aiSuggestions";

const [suggestions, setSuggestions] = createStore<{ list: AiSuggestion[] }>({ list: [] });

export { suggestions };

const listeners = new Set<() => void>();

/** `handler` runs after the suggestions changed. */
export function onSuggestionsChange(handler: () => void) {
  listeners.add(handler);
}

const changed = () => listeners.forEach((handler) => handler());

/** The open suggestions for `path`, as plain objects for the editor. */
export const openFor = (path: string): AiSuggestion[] =>
  suggestions.list.filter((suggestion) => suggestion.path === path && suggestion.status === "open").map((suggestion) => ({ ...suggestion }));

export const suggestionById = (id: string) => suggestions.list.find((suggestion) => suggestion.id === id) ?? null;

/** Replaces all suggestions with those of a new review. */
export function replaceSuggestions(list: AiSuggestion[]) {
  setSuggestions("list", list);
  changed();
}

export function setStatus(id: string, status: AiSuggestion["status"]) {
  setSuggestions(
    "list",
    produce((list) => {
      const suggestion = list.find((candidate) => candidate.id === id);
      if (suggestion) suggestion.status = status;
    }),
  );
  changed();
}

export function clearSuggestions() {
  if (suggestions.list.length === 0) return;
  replaceSuggestions([]);
}
