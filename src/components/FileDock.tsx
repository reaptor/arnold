import { createEffect, createMemo, createSignal, For, Show } from "solid-js";
import type { ChangedFile, FileStatus } from "../ipc/commands";
import { resetFileAt, selectFile, state } from "../state/review";

export const STATUS_LETTER: Record<FileStatus, string> = {
  added: "A",
  modified: "M",
  deleted: "D",
  renamed: "R",
  untracked: "U",
  unchanged: "·",
};

export const STATUS_NAME: Record<FileStatus, string> = {
  added: "Added",
  modified: "Modified",
  deleted: "Deleted",
  renamed: "Renamed",
  untracked: "Untracked (new, not committed)",
  unchanged: "Not changed (opened by go-to-definition)",
};

export function splitPath(path: string) {
  const slash = path.lastIndexOf("/");
  return { directory: slash < 0 ? "" : path.slice(0, slash + 1), name: path.slice(slash + 1) };
}

/** Left dock: every file in the diff. */
export function FileDock(props: { width: number }) {
  const [filter, setFilter] = createSignal("");
  const files = createMemo(() => {
    const needle = filter().toLowerCase();
    // Changed files first, then files opened from outside the diff.
    return [...(state.comparison?.files ?? []), ...state.visited].filter((file) =>
      file.path.toLowerCase().includes(needle),
    );
  });
  const canReset = () => state.comparison?.worktreePath != null;

  const move = (delta: number) => {
    const list = files();
    const index = list.findIndex((file) => file.path === state.selected);
    const next = list[Math.min(Math.max(index + delta, 0), list.length - 1)];
    if (next && next.path !== state.selected) void selectFile(next.path);
  };

  const onKeyDown = (event: KeyboardEvent) => {
    if (event.target instanceof HTMLInputElement) return;
    if (event.key === "ArrowDown") move(1);
    else if (event.key === "ArrowUp") move(-1);
    else if (event.key.toLowerCase() === "r" && !event.metaKey && !event.ctrlKey && !event.altKey && canReset())
      void resetFileAt(state.selected);
    else return;
    event.preventDefault();
  };

  const title = (file: ChangedFile) =>
    `${STATUS_NAME[file.status]}: ${file.oldPath ? `${file.oldPath} → ` : ""}${file.path}`;

  return (
    <aside class="dock" style={{ width: `${props.width}px` }} onKeyDown={onKeyDown}>
      <div class="dock-header">
        <input
          class="dock-filter"
          placeholder="Filter files"
          value={filter()}
          onInput={(event) => setFilter(event.currentTarget.value)}
        />
        <span class="dock-count" title="Changed files">
          {state.comparison?.files.length ?? 0}
        </span>
      </div>
      <ul class="file-list" tabindex="0" aria-label="Changed files">
        <For
          each={files()}
          fallback={<li class="file-empty">{state.comparison ? "No changed files" : ""}</li>}
        >
          {(file) => {
            const parts = splitPath(file.path);
            let row!: HTMLLIElement;
            // The selection also follows scrolling in the all-files view.
            createEffect(() => {
              if (file.path === state.selected) row.scrollIntoView({ block: "nearest" });
            });
            return (
              <li
                ref={row}
                classList={{ file: true, selected: file.path === state.selected }}
                title={title(file)}
                onClick={() => void selectFile(file.path)}
              >
                <span class={`status status-${file.status}`}>{STATUS_LETTER[file.status]}</span>
                <span class="file-path">
                  <span class="file-name">{parts.name}</span>
                  <span class="file-directory">{parts.directory}</span>
                </span>
                <Show when={state.dirty[file.path]}>
                  <span class="unsaved" title="Unsaved edits">
                    ●
                  </span>
                </Show>
                <Show when={state.problems[file.path]?.errors}>
                  <span class="problems" title="Errors reported by the language server">
                    {state.problems[file.path].errors}
                  </span>
                </Show>
                <span class="counts">
                  <Show when={file.binary} fallback={<Counts file={file} />}>
                    <span class="binary">bin</span>
                  </Show>
                </span>
                <Show when={canReset() && file.status !== "unchanged"}>
                  <button
                    class="row-action"
                    title="Reset file to the base version (R)"
                    onClick={(event) => {
                      event.stopPropagation();
                      void resetFileAt(file.path);
                    }}
                  >
                    ↺
                  </button>
                </Show>
              </li>
            );
          }}
        </For>
      </ul>
    </aside>
  );
}

export function Counts(props: { file: ChangedFile }) {
  return (
    <>
      <Show when={props.file.additions}>
        <span class="added">+{props.file.additions}</span>
      </Show>
      <Show when={props.file.deletions}>
        <span class="removed">−{props.file.deletions}</span>
      </Show>
    </>
  );
}
