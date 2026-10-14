import { createEffect, createMemo, createSignal, For, Show } from "solid-js";
import type { ChangedFile, FileStatus } from "../ipc/commands";
import { allFiles, resetFileAt, selectFile, state } from "../state/review";
import { FileTree } from "./FileTree";

export const STATUS_LETTER: Record<FileStatus, string> = {
  added: "A",
  modified: "M",
  deleted: "D",
  renamed: "R",
  untracked: "A",
  unchanged: "·",
};

export const STATUS_NAME: Record<FileStatus, string> = {
  added: "Added",
  modified: "Modified",
  deleted: "Deleted",
  renamed: "Renamed",
  untracked: "Added (not committed yet)",
  unchanged: "Not changed (opened by go-to-definition)",
};

export function splitPath(path: string) {
  const slash = path.lastIndexOf("/");
  return { directory: slash < 0 ? "" : path.slice(0, slash + 1), name: path.slice(slash + 1) };
}

const MIN_CHANGES_HEIGHT = 44;
/** Room the file explorer keeps when the changes are made taller. */
const MIN_EXPLORER_HEIGHT = 120;

const remembered = (key: string) => localStorage.getItem(`arnold.${key}`);
const remember = (key: string, value: unknown) => localStorage.setItem(`arnold.${key}`, String(value));

/**
 * Left dock, in two parts: the files changed by the branch on top (like the
 * open editors of VS Code), and below them every file of the checkout.
 */
export function FileDock(props: { width: number }) {
  let dock!: HTMLElement;
  const [filter, setFilter] = createSignal("");
  const [changesOpen, setChangesOpen] = createSignal(remembered("changesOpen") !== "false");
  const [explorerOpen, setExplorerOpen] = createSignal(remembered("explorerOpen") !== "false");
  const [changesHeight, setChangesHeight] = createSignal(Number(remembered("changesHeight")) || 240);

  const files = createMemo(() => {
    const needle = filter().toLowerCase();
    return (state.comparison?.files ?? []).filter((file) => file.path.toLowerCase().includes(needle));
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

  const toggleChanges = () => {
    setChangesOpen(!changesOpen());
    remember("changesOpen", changesOpen());
  };
  const toggleExplorer = () => {
    setExplorerOpen(!explorerOpen());
    remember("explorerOpen", explorerOpen());
  };

  const startResize = (down: PointerEvent) => {
    const startY = down.clientY;
    const startHeight = changesHeight();
    const onMove = (move: PointerEvent) => {
      const most = dock.clientHeight - MIN_EXPLORER_HEIGHT;
      setChangesHeight(Math.max(Math.min(startHeight + move.clientY - startY, most), MIN_CHANGES_HEIGHT));
    };
    const onUp = () => {
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", onUp);
      remember("changesHeight", changesHeight());
    };
    window.addEventListener("pointermove", onMove);
    window.addEventListener("pointerup", onUp);
    down.preventDefault();
  };

  const checkout = () => {
    const worktree = state.comparison?.worktreePath;
    return worktree ? `the checkout at ${worktree}` : `the last commit of ${state.head}`;
  };

  return (
    <aside class="dock" style={{ width: `${props.width}px` }} ref={dock}>
      <div class="dock-header">
        <input
          class="dock-filter"
          placeholder="Filter files"
          value={filter()}
          onInput={(event) => setFilter(event.currentTarget.value)}
        />
      </div>
      <section class="dock-section" classList={{ grow: !explorerOpen() }}>
        <DockHeading
          open={changesOpen()}
          onToggle={toggleChanges}
          title="Changes"
          detail={state.base ? `vs ${state.base}` : ""}
          count={state.comparison?.files.length ?? 0}
          explanation={`Files that ${state.head || "the branch"} changes compared with ${state.base || "the base branch"}: the diff under review. Arrow keys move through them; R resets the selected file.`}
        />
        <Show when={changesOpen()}>
          <ul
            class="file-list"
            tabindex="0"
            aria-label="Changed files"
            onKeyDown={onKeyDown}
            style={{ "max-height": explorerOpen() ? `${changesHeight()}px` : undefined }}
          >
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
        </Show>
      </section>
      <Show when={changesOpen() && explorerOpen()}>
        <div class="dock-sash" onPointerDown={startResize} />
      </Show>
      <section class="dock-section" classList={{ grow: explorerOpen() }}>
        <DockHeading
          open={explorerOpen()}
          onToggle={toggleExplorer}
          title="All files"
          detail={state.head ? `in ${state.head}` : ""}
          count={allFiles().length}
          explanation={`File explorer: every file in ${checkout()}, changed or not. Changed files are marked with their status letter; folders holding changes with a dot. Click a file to open it.`}
        />
        <Show when={explorerOpen()}>
          <FileTree filter={filter()} />
        </Show>
      </section>
    </aside>
  );
}

/** Title bar of a part of the dock; clicking it folds the part away. */
function DockHeading(props: {
  open: boolean;
  onToggle(): void;
  title: string;
  detail: string;
  count: number;
  explanation: string;
}) {
  return (
    <header class="dock-heading" title={props.explanation} onClick={() => props.onToggle()}>
      <span class="dock-caret">{props.open ? "▾" : "▸"}</span>
      <span class="dock-title">{props.title}</span>
      <span class="dock-detail">{props.detail}</span>
      <span class="dock-count">{props.count}</span>
    </header>
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
