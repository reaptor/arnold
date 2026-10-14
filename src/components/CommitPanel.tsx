import { createEffect, createSignal, For, onCleanup, Show } from "solid-js";
import type { Uncommitted } from "../ipc/commands";
import { commitChanges, pushChanges, saveAll, state } from "../state/review";

const STATUS_CLASS: Record<Uncommitted["status"], string> = {
  A: "status-added",
  M: "status-modified",
  D: "status-deleted",
  R: "status-renamed",
  U: "status-modified",
};

const STATUS_NAME: Record<Uncommitted["status"], string> = {
  A: "Added",
  M: "Modified",
  D: "Deleted",
  R: "Renamed",
  U: "Unmerged",
};

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

/** Toolbar button with a popup to commit the checkout under review and push it. */
export function CommitPanel() {
  const [open, setOpen] = createSignal(false);
  const [message, setMessage] = createSignal("");
  // Unticked rather than ticked paths, so files that show up later start ticked.
  const [unticked, setUnticked] = createSignal(new Set<string>());
  const [working, setWorking] = createSignal<"commit" | "push" | null>(null);
  const [outcome, setOutcome] = createSignal<{ error: boolean; text: string } | null>(null);
  let root!: HTMLDivElement;
  let textarea!: HTMLTextAreaElement;

  const changes = () => state.checkout?.changes ?? [];
  const chosen = () => changes().filter((change) => !unticked().has(change.path));
  const unsaved = () => Object.values(state.dirty).filter(Boolean).length;
  const target = () => state.checkout?.push ?? null;
  const ahead = () => target()?.ahead ?? 0;

  const canCommit = () => !working() && message().trim() !== "" && chosen().length > 0 && unsaved() === 0;
  const canPush = () => {
    const push = target();
    return !working() && push !== null && push.behind === 0 && (push.ahead > 0 || push.unpublished);
  };

  const toggle = () => {
    setOpen(!open());
    setOutcome(null);
    if (open()) queueMicrotask(() => textarea.focus());
  };
  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) setOpen(false);
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  const tick = (path: string, on: boolean) => {
    const next = new Set(unticked());
    if (on) next.delete(path);
    else next.add(path);
    setUnticked(next);
  };
  const tickAll = (on: boolean) => setUnticked(on ? new Set<string>() : new Set(changes().map((c) => c.path)));

  const commit = async (andPush: boolean) => {
    if (!canCommit()) return;
    const files = chosen();
    // Everything ticked: commit all, which also takes in files that appeared meanwhile.
    const paths =
      files.length === changes().length ? null : files.flatMap((c) => (c.oldPath ? [c.path, c.oldPath] : [c.path]));
    setWorking("commit");
    setOutcome(null);
    const { committed, error } = await commitChanges(message(), paths, andPush);
    setWorking(null);
    if (!committed) return void setOutcome({ error: true, text: error ?? "Nothing was committed." });
    setMessage("");
    setUnticked(new Set<string>());
    const what = `Committed ${plural(files.length, "file")}`;
    setOutcome(error ? { error: true, text: error } : { error: false, text: andPush ? `${what} and pushed.` : `${what}.` });
  };

  const push = async () => {
    if (!canPush()) return;
    setWorking("push");
    setOutcome(null);
    const error = await pushChanges();
    setWorking(null);
    setOutcome(error ? { error: true, text: error } : { error: false, text: "Pushed." });
  };

  const onKeyDown = (event: KeyboardEvent) => {
    if (event.key === "Escape") {
      setOpen(false);
      event.preventDefault();
    } else if (event.key === "Enter" && (event.metaKey || event.ctrlKey)) {
      event.preventDefault();
      void commit(event.shiftKey);
    }
  };

  const pushSummary = () => {
    const push = target();
    if (!push) return "No remote to push to.";
    const where = `${push.remote}/${push.branch}`;
    if (push.unpublished) return `${where} does not exist yet; Push creates it.`;
    if (push.behind > 0)
      return `${where} has ${plural(push.behind, "commit")} this branch does not. Pull before pushing.`;
    if (push.ahead > 0) return `${plural(push.ahead, "commit")} not pushed to ${where}.`;
    return `Up to date with ${where}.`;
  };

  return (
    <div class="picker" ref={root}>
      <button
        onClick={toggle}
        disabled={!state.checkout}
        title="Commit the changes of the branch under review and push them"
      >
        Commit
        <Show when={changes().length > 0}>
          <span class="count-badge">{changes().length}</span>
        </Show>
        <Show when={ahead() > 0}>
          <span class="ahead" title={`${plural(ahead(), "commit")} not pushed`}>
            ↑{ahead()}
          </span>
        </Show>
      </button>
      <Show when={open() && state.checkout}>
        {(checkout) => (
          <div class="picker-popup commit-popup" onKeyDown={onKeyDown}>
            <div class="commit-title">
              <Show
                when={checkout().branch}
                fallback={
                  <>
                    Commit on <b>{state.head}</b> <span class="tag">detached</span>
                  </>
                }
              >
                {(branch) => (
                  <>
                    Commit to <b>{branch()}</b>
                  </>
                )}
              </Show>
            </div>
            <Show
              when={changes().length > 0}
              fallback={<div class="picker-empty commit-empty">No uncommitted changes.</div>}
            >
              <label class="commit-all">
                <input
                  type="checkbox"
                  checked={chosen().length === changes().length}
                  ref={(box) =>
                    createEffect(() => (box.indeterminate = chosen().length > 0 && chosen().length < changes().length))
                  }
                  onChange={(event) => tickAll(event.currentTarget.checked)}
                />
                {chosen().length === changes().length
                  ? plural(changes().length, "file")
                  : `${chosen().length} of ${changes().length} files`}
              </label>
              <ul class="picker-list commit-files">
                <For each={changes()}>
                  {(change) => (
                    <li title={change.oldPath ? `${change.oldPath} → ${change.path}` : change.path}>
                      <label>
                        <input
                          type="checkbox"
                          checked={!unticked().has(change.path)}
                          onChange={(event) => tick(change.path, event.currentTarget.checked)}
                        />
                        <span class={`status ${STATUS_CLASS[change.status]}`} title={STATUS_NAME[change.status]}>
                          {change.status}
                        </span>
                        <span class="picker-name">{change.path}</span>
                      </label>
                    </li>
                  )}
                </For>
              </ul>
            </Show>
            <Show when={unsaved() > 0}>
              <div class="commit-note">
                {plural(unsaved(), "file")} with unsaved edits.
                <button onClick={() => saveAll()}>Save all</button>
              </div>
            </Show>
            <textarea
              ref={textarea}
              class="commit-message"
              placeholder="Commit message"
              rows={4}
              value={message()}
              onInput={(event) => setMessage(event.currentTarget.value)}
              spellcheck={false}
            />
            <div class="commit-buttons">
              <button class="primary" disabled={!canCommit()} onClick={() => commit(false)} title="Ctrl/Cmd+Enter">
                {working() === "commit" ? "Committing…" : "Commit"}
              </button>
              <button
                disabled={!canCommit() || !target() || (target()?.behind ?? 0) > 0}
                onClick={() => commit(true)}
                title="Ctrl/Cmd+Shift+Enter"
              >
                Commit & Push
              </button>
            </div>
            <div class="commit-push">
              <span>{pushSummary()}</span>
              <button disabled={!canPush()} onClick={push}>
                {working() === "push" ? "Pushing…" : "Push"}
              </button>
            </div>
            <Show when={outcome()}>
              {(result) => (
                <div class="commit-outcome" classList={{ error: result().error }}>
                  {result().text}
                </div>
              )}
            </Show>
          </div>
        )}
      </Show>
    </div>
  );
}
