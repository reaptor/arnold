import { createMemo, createSignal, For, onCleanup, Show } from "solid-js";
import { composerElement, isOpen, threadElement } from "../editor/threads";
import type { PrThread } from "../ipc/commands";
import { createGeneralThread, isChanged, loadThreads, revealThread, state, threads } from "../state/review";
import { openInBrowser } from "./DevOpsSetup";
import { splitPath } from "./FileDock";
import { ChevronDownIcon, CommentIcon, ExternalIcon, RefreshIcon } from "./icons";

/** A thread card, the same as in the diff. */
function ThreadCard(props: { thread: PrThread }) {
  return <div ref={(host) => host.append(threadElement(props.thread))} />;
}

const where = (thread: PrThread) =>
  thread.position ? `line ${thread.position.startLine}${thread.position.side === "base" ? " (before)" : ""}` : "whole file";

/**
 * Whether a click on a card's comments means "show it in the diff": not one
 * on the head (which folds the card), a button, a link or the reply field,
 * nor one that ends a text selection.
 */
const isJumpClick = (event: MouseEvent) =>
  !(event.target as Element).closest(".thread-head, .thread-footer, a, button, input, textarea") &&
  !window.getSelection()?.toString();

/**
 * Toolbar button with the number of open discussions of the pull request;
 * opens all of them: those about the pull request as a whole, then those
 * about files, which jump to their place in the diff from their line or a
 * click on their comments.
 */
export function DiscussionPicker() {
  const [open, setOpen] = createSignal(false);
  const [reloading, setReloading] = createSignal(false);
  const [writing, setWriting] = createSignal(false);
  let root!: HTMLDivElement;
  let popup!: HTMLDivElement;

  const general = createMemo(() => threads().filter((thread) => thread.path === null));
  const byFile = createMemo(() => {
    const files = new Map<string, PrThread[]>();
    for (const thread of threads()) {
      if (thread.path === null) continue;
      files.set(thread.path, [...(files.get(thread.path) ?? []), thread]);
    }
    for (const list of files.values()) list.sort((a, b) => (a.position?.startLine ?? 0) - (b.position?.startLine ?? 0));
    return [...files].sort(([a], [b]) => a.localeCompare(b));
  });
  const active = () => threads().filter(isOpen).length;

  const toggle = () => {
    setOpen(!open());
    if (!open()) return;
    queueMicrotask(() => {
      const overflow = popup.getBoundingClientRect().right - (document.documentElement.clientWidth - 8);
      if (overflow > 0) popup.style.left = `${-overflow}px`;
    });
  };
  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) setOpen(false);
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  const reload = async () => {
    setReloading(true);
    await loadThreads();
    setReloading(false);
  };
  const jump = (thread: PrThread) => {
    setOpen(false);
    void revealThread(thread);
  };

  return (
    <div class="picker discussion-picker" ref={root}>
      <button
        class="picker-button"
        onClick={toggle}
        title={`Discussions: ${active()} active, ${threads().length - active()} resolved or closed`}
      >
        <CommentIcon />
        <span class="picker-value">{active()}</span>
        <ChevronDownIcon />
      </button>
      <Show when={open()}>
        <div ref={popup} class="picker-popup discussion-popup" data-tauri-drag-region="false">
          <div class="discussion-list">
            <Show
              when={writing()}
              fallback={
                <button class="discussion-new" onClick={() => setWriting(true)}>
                  <CommentIcon />
                  Comment on the pull request…
                </button>
              }
            >
              <div
                class="thread draft"
                ref={(host) =>
                  host.append(
                    composerElement({
                      key: "general",
                      placeholder: "Comment on the pull request as a whole…",
                      submitLabel: "Comment",
                      autofocus: true,
                      onSubmit: async (text) => {
                        await createGeneralThread(text);
                        setWriting(false);
                      },
                      onCancel: () => setWriting(false),
                    }),
                  )
                }
              />
            </Show>
            <Show when={threads().length === 0}>
              <p class="picker-empty">No one has commented on this pull request yet.</p>
            </Show>
            <Show when={state.range !== null && threads().some((thread) => thread.path)}>
              <p class="discussion-note">Comments on files show in the diff when all changes are shown.</p>
            </Show>
            <Show when={general().length > 0}>
              <h3>Pull request</h3>
              <For each={general()}>{(thread) => <ThreadCard thread={thread} />}</For>
            </Show>
            <For each={byFile()}>
              {([path, list]) => (
                <>
                  <h3 title={path}>
                    <span class="discussion-file">{splitPath(path).name}</span>
                    <span class="discussion-folder">{splitPath(path).directory}</span>
                  </h3>
                  <For each={list}>
                    {(thread) => (
                      <div
                        classList={{ "discussion-item": true, jumps: isChanged(path) }}
                        onClick={(event) => isChanged(path) && isJumpClick(event) && jump(thread)}
                      >
                        <button
                          class="discussion-jump"
                          disabled={!isChanged(path)}
                          onClick={() => jump(thread)}
                          title={isChanged(path) ? "Show it in the diff" : "Not in the diff"}
                        >
                          {where(thread)}
                        </button>
                        <ThreadCard thread={thread} />
                      </div>
                    )}
                  </For>
                </>
              )}
            </For>
          </div>
          <div class="pr-popup-footer">
            <Show when={state.pullRequest}>
              {(pullRequest) => (
                <button onClick={() => openInBrowser(pullRequest().url)} title={pullRequest().url}>
                  Open in Azure DevOps
                  <ExternalIcon />
                </button>
              )}
            </Show>
            <span class="toolbar-space" />
            <button
              classList={{ "pr-reload": true, busy: reloading() }}
              onClick={() => void reload()}
              disabled={reloading()}
              title="Reload the discussions"
            >
              <RefreshIcon />
            </button>
          </div>
        </div>
      </Show>
    </div>
  );
}
