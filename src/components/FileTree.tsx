import { createEffect, createMemo, createSignal, Index, on, onCleanup, onMount, Show } from "solid-js";
import { createStore } from "solid-js/store";
import type { FileStatus } from "../ipc/commands";
import { allFiles, openFile, state } from "../state/review";
import { splitPath, STATUS_LETTER, STATUS_NAME } from "./FileDock";

/** Height of one row; rows are virtualized, so it is fixed. Matches `.tree-row`. */
const ROW_HEIGHT = 22;
/** Rows drawn above and below the visible ones. */
const OVERSCAN = 12;

interface Folder {
  path: string;
  folders: Map<string, Folder>;
  /** Full paths of the files directly in this folder. */
  files: string[];
  sorted: boolean;
}

interface Row {
  kind: "folder" | "file";
  path: string;
  label: string;
  /** Folder of a file found by the filter. */
  detail: string;
  depth: number;
  open: boolean;
}

const collator = new Intl.Collator(undefined, { numeric: true, sensitivity: "base" });

function buildTree(paths: readonly string[]): Folder {
  const root: Folder = { path: "", folders: new Map(), files: [], sorted: false };
  for (const path of paths) {
    const parts = path.split("/");
    let folder = root;
    for (let index = 0; index < parts.length - 1; index++) {
      let child = folder.folders.get(parts[index]);
      if (!child) {
        child = { path: parts.slice(0, index + 1).join("/"), folders: new Map(), files: [], sorted: false };
        folder.folders.set(parts[index], child);
      }
      folder = child;
    }
    folder.files.push(path);
  }
  return root;
}

/** Folders are sorted the first time they are opened, not all up front. */
function sortOnce(folder: Folder) {
  if (folder.sorted) return;
  folder.folders = new Map([...folder.folders].sort(([a], [b]) => collator.compare(a, b)));
  folder.files.sort((a, b) => collator.compare(splitPath(a).name, splitPath(b).name));
  folder.sorted = true;
}

/** The rows of the open part of the tree, folders before files as in VS Code. */
function flatten(folder: Folder, expanded: Record<string, boolean>, depth: number, rows: Row[]) {
  sortOnce(folder);
  for (const [name, child] of folder.folders) {
    // A chain of folders that each hold just one folder is one row: "src/Api".
    let shown = child;
    let label = name;
    while (shown.files.length === 0 && shown.folders.size === 1) {
      const [[innerName, inner]] = shown.folders;
      label += `/${innerName}`;
      shown = inner;
    }
    const open = !!expanded[shown.path];
    rows.push({ kind: "folder", path: shown.path, label, detail: "", depth, open });
    if (open) flatten(shown, expanded, depth + 1, rows);
  }
  for (const path of folder.files) {
    rows.push({ kind: "file", path, label: splitPath(path).name, detail: "", depth, open: false });
  }
}

const ancestors = (path: string) => {
  const parts = path.split("/");
  return parts.slice(0, -1).map((_, index) => parts.slice(0, index + 1).join("/"));
};

/**
 * File explorer: every file of the branch under review as a folder tree.
 * Changed files are marked with their status, folders holding changes with
 * a dot. With a filter, matching files are listed flat instead.
 */
export function FileTree(props: { filter: string }) {
  let scroller!: HTMLDivElement;
  const [expanded, setExpanded] = createStore<Record<string, boolean>>({});
  const [cursor, setCursor] = createSignal<string | null>(null);
  const [scrollTop, setScrollTop] = createSignal(0);
  const [viewHeight, setViewHeight] = createSignal(0);

  const tree = createMemo(() => buildTree(allFiles()));
  const fileSet = createMemo(() => new Set(allFiles()));
  const changed = createMemo(
    () => new Map<string, FileStatus>((state.comparison?.files ?? []).map((file) => [file.path, file.status])),
  );
  const changedFolders = createMemo(() => new Set([...changed().keys()].flatMap(ancestors)));

  const rows = createMemo<Row[]>(() => {
    const needle = props.filter.toLowerCase();
    if (needle) {
      return allFiles()
        .filter((path) => path.toLowerCase().includes(needle))
        .map((path) => {
          const parts = splitPath(path);
          return { kind: "file", path, label: parts.name, detail: parts.directory, depth: 0, open: false };
        });
    }
    const result: Row[] = [];
    flatten(tree(), expanded, 0, result);
    return result;
  });

  const first = () => Math.max(0, Math.floor(scrollTop() / ROW_HEIGHT) - OVERSCAN);
  const visible = createMemo(() =>
    rows().slice(first(), first() + Math.ceil(viewHeight() / ROW_HEIGHT) + 2 * OVERSCAN),
  );

  onMount(() => {
    const observer = new ResizeObserver(() => setViewHeight(scroller.clientHeight));
    observer.observe(scroller);
    onCleanup(() => observer.disconnect());
  });

  const scrollToRow = (index: number) => {
    if (index < 0) return;
    const top = index * ROW_HEIGHT;
    if (top < scroller.scrollTop) scroller.scrollTop = top;
    else if (top + ROW_HEIGHT > scroller.scrollTop + scroller.clientHeight)
      scroller.scrollTop = top + ROW_HEIGHT - scroller.clientHeight;
  };
  const indexOf = (path: string | null) => rows().findIndex((row) => row.path === path);

  // Follow the file shown in the editor: open its folders and scroll to it.
  createEffect(
    on(
      () => state.selected,
      (path) => {
        if (!path || !fileSet().has(path)) return;
        for (const folder of ancestors(path)) if (!expanded[folder]) setExpanded(folder, true);
        setCursor(path);
        scrollToRow(indexOf(path));
      },
    ),
  );

  const toggle = (path: string) => setExpanded(path, !expanded[path]);

  const activate = (row: Row) => {
    setCursor(row.path);
    if (row.kind === "folder") toggle(row.path);
    else if (row.path !== state.selected) void openFile(row.path);
  };

  const onKeyDown = (event: KeyboardEvent) => {
    const list = rows();
    if (list.length === 0) return;
    const index = Math.max(indexOf(cursor()), 0);
    const row = list[index];
    const moveTo = (next: number) => {
      const target = Math.min(Math.max(next, 0), list.length - 1);
      setCursor(list[target].path);
      scrollToRow(target);
    };
    if (event.key === "ArrowDown") moveTo(indexOf(cursor()) < 0 ? 0 : index + 1);
    else if (event.key === "ArrowUp") moveTo(index - 1);
    else if (event.key === "Home") moveTo(0);
    else if (event.key === "End") moveTo(list.length - 1);
    else if (event.key === "Enter" || event.key === " ") activate(row);
    else if (event.key === "ArrowRight" && row.kind === "folder") {
      if (row.open) moveTo(index + 1);
      else setExpanded(row.path, true);
    } else if (event.key === "ArrowLeft") {
      if (row.kind === "folder" && row.open) setExpanded(row.path, false);
      else {
        // To the folder row holding this one.
        let parent = index - 1;
        while (parent >= 0 && list[parent].depth >= row.depth) parent--;
        if (parent >= 0) moveTo(parent);
      }
    } else return;
    event.preventDefault();
    event.stopPropagation();
  };

  const statusOf = (path: string) => changed().get(path);

  return (
    <div
      class="tree"
      ref={scroller}
      tabindex="0"
      role="tree"
      aria-label="All files"
      onScroll={() => setScrollTop(scroller.scrollTop)}
      onKeyDown={onKeyDown}
    >
      <Show
        when={rows().length > 0}
        fallback={
          <div class="file-empty">
            {props.filter ? "No files match." : state.comparison ? "Loading files…" : ""}
          </div>
        }
      >
        <div class="tree-spacer" style={{ height: `${rows().length * ROW_HEIGHT}px` }}>
          <div style={{ transform: `translateY(${first() * ROW_HEIGHT}px)` }}>
            <Index each={visible()}>
              {(row) => {
                const status = () => (row().kind === "file" ? statusOf(row().path) : undefined);
                const title = () => {
                  const known = status();
                  return known ? `${STATUS_NAME[known]}: ${row().path}` : row().path;
                };
                return (
                  <div
                    class="tree-row"
                    classList={{
                      selected: row().path === state.selected,
                      cursor: row().path === cursor(),
                      [`changed-${status()}`]: status() !== undefined,
                    }}
                    role="treeitem"
                    aria-expanded={row().kind === "folder" ? row().open : undefined}
                    style={{ "padding-left": `${8 + row().depth * 12}px` }}
                    title={title()}
                    onClick={() => activate(row())}
                  >
                    <span class="tree-caret">{row().kind === "folder" ? (row().open ? "▾" : "▸") : ""}</span>
                    <span class="tree-label" classList={{ "tree-folder": row().kind === "folder" }}>
                      {row().label}
                      <Show when={row().detail}>
                        <span class="file-directory">{row().detail}</span>
                      </Show>
                    </span>
                    <Show when={row().kind === "file" && state.dirty[row().path]}>
                      <span class="unsaved" title="Unsaved edits">
                        ●
                      </span>
                    </Show>
                    <Show when={row().kind === "file" && state.problems[row().path]?.errors}>
                      <span class="problems" title="Errors reported by the language server">
                        {state.problems[row().path].errors}
                      </span>
                    </Show>
                    <Show
                      when={status()}
                      fallback={
                        <Show when={row().kind === "folder" && changedFolders().has(row().path)}>
                          <span class="tree-dot" title="Holds changed files">
                            ●
                          </span>
                        </Show>
                      }
                    >
                      {(known) => <span class={`status status-${known()}`}>{STATUS_LETTER[known()]}</span>}
                    </Show>
                  </div>
                );
              }}
            </Index>
          </div>
        </div>
      </Show>
    </div>
  );
}
