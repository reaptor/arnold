import { createSignal, For, onMount, Show } from "solid-js";
import { createStore } from "solid-js/store";
import { postSuggestions, setSuggestionPlan, type SuggestionFile, type SuggestionPlan, state } from "../state/review";
import type { Suggestion } from "../state/suggestions";

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

/** "lines 12–14" / "line 12" of the pull request. */
const lineName = (suggestion: Suggestion) =>
  suggestion.startLine === suggestion.endLine ? `line ${suggestion.startLine}` : `lines ${suggestion.startLine}–${suggestion.endLine}`;

/**
 * Suggest changes: the edits made in a pull request's worktree, one
 * suggestion per changed block, each with an optional message, posted as
 * Azure DevOps suggestion comments. Posted edits are put back in the
 * worktree, as they now live on the pull request.
 */
export function SuggestDialog(props: { plan: SuggestionPlan }) {
  type Item = { file: SuggestionFile; suggestion: Suggestion; picked: boolean; message: string };
  const [items, setItems] = createStore<Item[]>(
    props.plan.files.flatMap((file) => file.suggestions.map((suggestion) => ({ file, suggestion, picked: true, message: "" }))),
  );
  const [busy, setBusy] = createSignal(false);
  const [error, setError] = createSignal<string | null>(null);
  const picked = () => items.filter((item) => item.picked);
  let first: HTMLButtonElement | undefined;
  onMount(() => first?.focus());

  const close = () => {
    if (!busy()) setSuggestionPlan(null);
  };
  const post = async () => {
    setBusy(true);
    setError(null);
    const failure = await postSuggestions(
      picked().map((item) => ({ file: item.file, suggestion: item.suggestion, message: item.message })),
    );
    setBusy(false);
    if (failure) setError(failure);
    else setSuggestionPlan(null);
  };

  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && close()}
      onKeyDown={(event) => event.key === "Escape" && close()}
    >
      <form
        class="modal complete-dialog suggest-dialog"
        role="dialog"
        aria-label="Suggest changes"
        onSubmit={(event) => {
          event.preventDefault();
          void post();
        }}
      >
        <h2>{items.length === 1 ? "Suggest change" : "Suggest changes"}</h2>
        <p>
          {items.length === 1 ? "It becomes a suggestion comment" : "Each change becomes a suggestion comment"} on pull
          request !{state.pullRequest?.id}, which its author can apply with a click.
        </p>
        <ol class="suggestions">
          <For each={items}>
            {(item, index) => (
              <li class="suggestion" classList={{ skipped: !item.picked }}>
                <label class="suggestion-head">
                  <Show when={items.length > 1}>
                    <input
                      type="checkbox"
                      checked={item.picked}
                      disabled={busy()}
                      onChange={(event) => setItems(index(), "picked", event.currentTarget.checked)}
                    />
                  </Show>
                  <span class="suggestion-path">{item.file.path}</span>
                  <span class="suggestion-lines">{lineName(item.suggestion)}</span>
                </label>
                <pre class="suggestion-diff">
                  <For each={item.suggestion.old}>{(line) => <span class="suggestion-old">{line || " "}</span>}</For>
                  <For each={item.suggestion.new}>{(line) => <span class="suggestion-new">{line || " "}</span>}</For>
                </pre>
                <Show when={item.picked}>
                  <textarea
                    class="suggestion-message"
                    rows={1}
                    placeholder="Message (optional, Markdown)"
                    value={item.message}
                    disabled={busy()}
                    onInput={(event) => setItems(index(), "message", event.currentTarget.value)}
                  />
                </Show>
              </li>
            )}
          </For>
        </ol>
        <Show when={error()}>{(text) => <p class="connect-problem">{text()}</p>}</Show>
        <div class="connect-actions">
          <button type="button" onClick={close} disabled={busy()}>
            Cancel
          </button>
          <button ref={first} type="submit" class="primary" disabled={busy() || picked().length === 0}>
            {busy() ? "Posting…" : `Post ${plural(picked().length, "suggestion")}`}
          </button>
        </div>
      </form>
    </div>
  );
}
