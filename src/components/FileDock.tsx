import { createEffect, createMemo, createSignal, For, type JSX, Show } from "solid-js";
import type { ChangedFile, FileStatus } from "../ipc/commands";
import {
  allFiles,
  branchNames,
  editsToCommit,
  fileThreads,
  openFile,
  openFileMenu,
  selectFile,
  showOverview,
  state,
} from "../state/review";
import { isOpen } from "../editor/threads";
import { pickedCount } from "../state/commitRange";
import { Chevron } from "./Chevron";
import { FileTree, type TreeControls, TreeButtons } from "./FileTree";
import { CommentIcon } from "./icons";

export const STATUS_LETTER: Record<FileStatus, string> = {
  added: "A",
  modified: "M",
  deleted: "D",
  renamed: "R",
  conflicted: "!",
  unchanged: "·",
};

export const STATUS_NAME: Record<FileStatus, string> = {
  added: "Added",
  modified: "Modified",
  deleted: "Deleted",
  renamed: "Renamed",
  conflicted: "Merge conflicts",
  unchanged: "Not changed (opened by go-to-definition)",
};

/**
 * Whether `file` is marked as having uncommitted edits: in Branches, and in
 * your own pull request edited on its branch. Others' pull requests' edits
 * are only on their way to becoming suggestions, which their Suggest
 * buttons and frames show.
 */
export const showsEdited = (file: ChangedFile) => !!file.uncommitted && editsToCommit();

/** Marks a file with uncommitted edits in the checkout a comparison reads. */
export function EditedMark() {
  return (
    <span class="edited-mark" title={`Uncommitted edits: not in a commit of ${branchNames().head || "the branch"} yet`}>
      ● edited
    </span>
  );
}

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
export function FileDock(props: { width: number; hidden?: boolean }) {
  let changesList!: HTMLUListElement;
  let explorer!: HTMLElement;
  const [filter, setFilter] = createSignal("");
  const [treeControls, setTreeControls] = createSignal<TreeControls>();
  /** "3 commits" when only some of the branch's commits are shown. */
  const picked = () => {
    if (!state.range) return null;
    const count = pickedCount(state.comparison?.commits ?? [], state.range);
    return `${count} commit${count === 1 ? "" : "s"}`;
  };
  const [changesOpen, setChangesOpen] = createSignal(remembered("changesOpen") !== "false");
  const [explorerOpen, setExplorerOpen] = createSignal(remembered("explorerOpen") !== "false");
  const [changesHeight, setChangesHeight] = createSignal(Number(remembered("changesHeight")) || 240);

  const needle = () => filter().toLowerCase();
  const files = createMemo(() =>
    (state.comparison?.files ?? []).filter((file) => file.path.toLowerCase().includes(needle())),
  );
  const matchingFiles = createMemo(() =>
    needle() ? allFiles().filter((path) => path.toLowerCase().includes(needle())).length : allFiles().length,
  );
  const bothOpen = () => changesOpen() && explorerOpen();

  const move = (delta: number) => {
    const list = files();
    const index = list.findIndex((file) => file.path === state.selected);
    const next = list[Math.min(Math.max(index + delta, 0), list.length - 1)];
    if (!next) return;
    showOverview(false);
    if (next.path !== state.selected) void selectFile(next.path);
  };

  const onKeyDown = (event: KeyboardEvent) => {
    if (event.target instanceof HTMLInputElement) return;
    if (event.key === "ArrowDown") move(1);
    else if (event.key === "ArrowUp") move(-1);
    else return;
    event.preventDefault();
  };

  const title = (file: ChangedFile) =>
    `${STATUS_NAME[file.status]}${showsEdited(file) ? ", with uncommitted edits" : ""}: ${file.oldPath ? `${file.oldPath} → ` : ""}${file.path}`;

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
    // Start from what is shown: a short window may have squeezed the list.
    const startHeight = changesList.clientHeight;
    const most = startHeight + explorer.clientHeight - MIN_EXPLORER_HEIGHT;
    const onMove = (move: PointerEvent) => {
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

  return (
    <aside class="dock" classList={{ hidden: props.hidden }} style={{ width: `${props.width}px` }}>
      {/* Outside both islands, as it narrows both lists. */}
      <div class="dock-header">
        <input
          class="dock-filter"
          placeholder="Filter Changes & All files"
          title="Shows only the files whose path contains this text, in both Changes and All files."
          value={filter()}
          onInput={(event) => setFilter(event.currentTarget.value)}
          onKeyDown={(event) => event.key === "Escape" && setFilter("")}
        />
        <Show when={filter()}>
          <button class="dock-filter-clear" title="Clear the filter (Esc)" onClick={() => setFilter("")}>
            ×
          </button>
        </Show>
      </div>
      <div class="dock-island" classList={{ grow: !explorerOpen() }}>
        <section class="dock-section" classList={{ grow: !explorerOpen() }}>
          <DockHeading
            open={changesOpen()}
            onToggle={toggleChanges}
            title="Changes"
            detail={picked() ? `in ${picked()}` : branchNames().base ? `vs ${branchNames().base}` : ""}
            count={state.comparison?.files.length ?? 0}
            matching={filter() ? files().length : undefined}
            explanation={
              picked()
                ? `Files that the picked ${picked()} of ${branchNames().head} change, compared with the commit before them. Arrow keys move through them.`
                : `Files that ${branchNames().head || "the branch"} changes compared with ${branchNames().base || "the base branch"}: the diff under review. Arrow keys move through them.`
            }
          />
          <Show when={changesOpen()}>
            <ul
              class="file-list"
              ref={changesList}
              classList={{ sized: explorerOpen() }}
              tabindex="0"
              aria-label="Changed files"
              onKeyDown={onKeyDown}
              style={{ height: explorerOpen() ? `${changesHeight()}px` : undefined }}
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
                      onClick={() => void openFile(file.path)}
                      onContextMenu={(event) => {
                        if (openFileMenu(file.path, event.clientX, event.clientY)) event.preventDefault();
                      }}
                    >
                      <span class={`status status-${file.status}`}>{STATUS_LETTER[file.status]}</span>
                      <span class="file-path">
                        <span class="file-name">{parts.name}</span>
                        <span class="file-directory">{parts.directory}</span>
                      </span>
                      <Show when={showsEdited(file)}>
                        <EditedMark />
                      </Show>
                      <Show when={fileThreads(file.path).length > 0}>
                        <span
                          class="file-threads"
                          classList={{ open: fileThreads(file.path).some(isOpen) }}
                          title={`${fileThreads(file.path).filter(isOpen).length} active of ${fileThreads(file.path).length} discussions`}
                        >
                          <CommentIcon />
                          {fileThreads(file.path).length}
                        </span>
                      </Show>
                      <span class="counts">
                        <Show when={file.binary} fallback={<Counts file={file} />}>
                          <span class="binary">bin</span>
                        </Show>
                      </span>
                    </li>
                  );
                }}
              </For>
            </ul>
          </Show>
        </section>
      </div>
      {/* Like the splitter beside the dock, the gap between the islands is the handle. */}
      <div
        class="dock-sash"
        classList={{ fixed: !bothOpen() }}
        onPointerDown={(down) => bothOpen() && startResize(down)}
      />
      <section class="dock-section dock-island" classList={{ grow: explorerOpen() }} ref={explorer}>
        <DockHeading
          open={explorerOpen()}
          onToggle={toggleExplorer}
          title="All files"
          detail={state.range ? `in ${state.range.newest.slice(0, 7)}` : branchNames().head ? `in ${branchNames().head}` : ""}
          count={allFiles().length}
          matching={filter() ? matchingFiles() : undefined}
          explanation={`File explorer: every file in ${state.range ? `commit ${state.range.newest.slice(0, 7)}, the newest one picked` : `the last commit of ${branchNames().head}`}, changed or not. Changed files are marked with their status letter; folders holding changes with a dot. With a solution or projects, the Solution view (the grid button) shows the projects in solution order and F# files in compile order. Click a file to open it.`}
        >
          <Show when={explorerOpen()}>
            <TreeButtons controls={treeControls} filtering={!!filter()} />
          </Show>
        </DockHeading>
        <Show when={explorerOpen()}>
          <FileTree filter={filter()} controls={setTreeControls} />
        </Show>
      </section>
    </aside>
  );
}

/** Title bar of a part of the dock; clicking it folds the part away. */
export function DockHeading(props: {
  open: boolean;
  onToggle(): void;
  title: string;
  detail: string;
  count: number;
  /** How many of them the filter leaves, while filtering. */
  matching?: number;
  explanation: string;
  /** Buttons at the end of the heading (Stage all, Collapse all). */
  children?: JSX.Element;
}) {
  return (
    <header class="dock-heading" title={props.explanation} onClick={() => props.onToggle()}>
      <Chevron class="dock-caret" open={props.open} />
      <span class="dock-title">{props.title}</span>
      <span class="dock-detail">{props.detail}</span>
      {props.children}
      <span class="dock-count" classList={{ filtered: props.matching !== undefined }}>
        {props.matching !== undefined ? `${props.matching} of ${props.count}` : props.count}
      </span>
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
