import { createEffect, createMemo, createSignal, Index, on, onCleanup, onMount, Show, untrack } from "solid-js";
import { createStore, produce, reconcile } from "solid-js/store";
import type { FileStatus } from "../ipc/commands";
import { allFiles, changedFiles, openFile, openTreeMenu, state } from "../state/review";
import { explorerView, fileOrder, setExplorerView, showsSolution, solutionTree } from "../state/solutionView";
import { Chevron } from "./Chevron";
import { CollapseAllIcon, ExpandIcon, SolutionViewIcon } from "./icons";
import { FileTypeIcon, FolderIcon } from "./fileIcons";
import { folderModel, type Row, solutionModel, type TreeModel } from "./treeModel";
import { showsEdited, splitPath, STATUS_LETTER, STATUS_NAME } from "./FileDock";

/** Height of one row; rows are virtualized, so it is fixed. Matches `.tree-row`. */
const ROW_HEIGHT = 22;
/** Rows drawn above and below the visible ones. */
const OVERSCAN = 12;

/** What the buttons in the explorer's heading do to the tree. */
export interface TreeControls {
  /** What Expand selected opens: the folder or file picked, or null for the whole tree. */
  selected(): string | null;
  expandSelected(): void;
  collapseAll(): void;
}

/**
 * File explorer: every file of the branch under review, as the solution's
 * projects (F# files in compile order) or as a folder tree. Changed files
 * are marked with their status, folders holding changes with a dot. With a
 * filter, matching files are listed flat instead.
 */
export function FileTree(props: { filter: string; controls?: (controls: TreeControls) => void }) {
  let scroller!: HTMLDivElement;
  const [expanded, setExpanded] = createStore<Record<string, boolean>>({});
  const [cursor, setCursor] = createSignal<string | null>(null);
  const [scrollTop, setScrollTop] = createSignal(0);
  const [viewHeight, setViewHeight] = createSignal(0);

  const model = createMemo<TreeModel>(() => {
    const solution = showsSolution() ? solutionTree() : null;
    return solution ? solutionModel(solution, allFiles()) : folderModel(allFiles());
  });
  const fileSet = createMemo(() => new Set(allFiles()));
  const changed = createMemo(
    () => new Map<string, FileStatus>(changedFiles().map((file) => [file.path, file.status])),
  );
  /** Rows holding changed files. */
  const changedGroups = createMemo(() => new Set([...changed().keys()].flatMap((path) => model().holders(path))));
  /** Files with uncommitted edits in the checkout a comparison reads. */
  const edited = createMemo(() => new Set(changedFiles().flatMap((file) => (showsEdited(file) ? [file.path] : []))));

  const rows = createMemo<Row[]>(() => {
    const needle = props.filter.toLowerCase();
    if (needle) {
      const order = fileOrder();
      return allFiles()
        .filter((path) => path.toLowerCase().includes(needle))
        .sort(order)
        .map((path) => {
          const parts = splitPath(path);
          return { kind: "file", key: path, path, projectFile: "", label: parts.name, detail: parts.directory, depth: 0, open: false, icon: null, note: "", unlisted: "" };
        });
    }
    return model().rows(expanded);
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

  /**
   * Scrolls the row `key` into view once the rows are drawn: folders opened
   * just now make the list longer, and until then the scroll would stop at
   * its old end, short of the row.
   */
  const scrollToRow = (key: string) =>
    queueMicrotask(() => {
      const index = indexOf(key);
      if (index < 0) return;
      const top = index * ROW_HEIGHT;
      if (top < scroller.scrollTop) scroller.scrollTop = top;
      else if (top + ROW_HEIGHT > scroller.scrollTop + scroller.clientHeight)
        scroller.scrollTop = top + ROW_HEIGHT - scroller.clientHeight;
    });
  const indexOf = (key: string | null) => rows().findIndex((row) => row.key === key);
  const openAll = (keys: string[]) =>
    setExpanded(
      produce((open) => {
        for (const key of keys) open[key] = true;
      }),
    );

  /** Opens the folders down to `path` and scrolls to it. */
  const reveal = (path: string) => {
    const closed = model().holders(path).filter((key) => !rowOpen(key));
    if (closed.length) openAll(closed);
    setCursor(path);
    scrollToRow(path);
  };

  // Follow the file shown in the editor, wherever it was picked (Changes, the
  // all-files view's scrolling, F7, a discussion, go-to-definition, in Commit
  // mode also Unstaged or Staged). A file picked before the tree had it (still
  // loading) is revealed when it comes; a refresh of the files leaves the
  // scrolling alone.
  let revealed: string | null = null;
  createEffect(() => {
    const path = state.selected;
    const known = !!path && fileSet().has(path);
    untrack(() => {
      if (!known) revealed = null;
      else if (path !== revealed) {
        revealed = path;
        reveal(path);
      }
    });
  });
  // The other view, or the tree back from a filter's flat list, puts the
  // rows elsewhere: find the file shown again.
  createEffect(
    on(
      [showsSolution, () => props.filter === ""],
      () => {
        const path = state.selected;
        if (!props.filter && path && fileSet().has(path)) reveal(path);
      },
      { defer: true },
    ),
  );

  /** Whether the group row `key` is open (some start open). */
  const rowOpen = (key: string) => rows().find((row) => row.key === key)?.open ?? !!expanded[key];
  const toggle = (row: Row) => setExpanded(row.key, !row.open);

  const inTree = (key: string | null): key is string => !!key && (fileSet().has(key) || model().isGroup(key));
  /** The row under the cursor, or else the file shown, if it is in the tree. */
  const selected = () => [cursor(), state.selected].find(inTree) ?? null;

  /**
   * Opens the folder under the cursor and every folder in it. With a file
   * there (or nothing picked, the file shown), opens the folders down to it.
   * With nothing selected, opens every folder.
   */
  const expandSelected = () => {
    const key = selected();
    const tree = model();
    openAll([...(key ? tree.holders(key) : []), ...(key !== null && fileSet().has(key) ? [] : tree.groupsIn(key))]);
    if (key) scrollToRow(key);
  };

  /** Closes every folder; the cursor stays, so Expand selected finds its way back. */
  const collapseAll = () => setExpanded(reconcile(model().collapsed()));

  props.controls?.({ selected, expandSelected, collapseAll });

  const activate = (row: Row) => {
    setCursor(row.key);
    if (row.kind === "folder") toggle(row);
    else void openFile(row.path);
  };

  const onKeyDown = (event: KeyboardEvent) => {
    const list = rows();
    if (list.length === 0) return;
    const index = Math.max(indexOf(cursor()), 0);
    const row = list[index];
    const moveTo = (next: number) => {
      const target = Math.min(Math.max(next, 0), list.length - 1);
      setCursor(list[target].key);
      scrollToRow(list[target].key);
    };
    if (event.key === "ArrowDown") moveTo(indexOf(cursor()) < 0 ? 0 : index + 1);
    else if (event.key === "ArrowUp") moveTo(index - 1);
    else if (event.key === "Home") moveTo(0);
    else if (event.key === "End") moveTo(list.length - 1);
    else if (event.key === "Enter" || event.key === " ") activate(row);
    else if (event.key === "*") expandSelected();
    else if (event.key === "ArrowRight" && row.kind === "folder") {
      if (row.open) moveTo(index + 1);
      else setExpanded(row.key, true);
    } else if (event.key === "ArrowLeft") {
      if (row.kind === "folder" && row.open) setExpanded(row.key, false);
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
            {props.filter ? "No files match." : state.comparison || state.work ? "Loading files…" : ""}
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
                  const edits = edited().has(row().path) ? ", with uncommitted edits" : "";
                  const name = row().kind === "file" ? row().path : row().note || row().path || row().label;
                  const note = row().kind === "file" && row().note ? ` (${row().note})` : "";
                  return known ? `${STATUS_NAME[known]}${edits}: ${name}${note}` : `${name}${note}`;
                };
                return (
                  <div
                    class="tree-row"
                    classList={{
                      selected: row().kind === "file" && row().path === state.selected,
                      cursor: row().key === cursor(),
                      unlisted: !!row().unlisted,
                      [`changed-${status()}`]: status() !== undefined,
                    }}
                    role="treeitem"
                    aria-expanded={row().kind === "folder" ? row().open : undefined}
                    style={{ "padding-left": `${8 + row().depth * 12}px` }}
                    title={title()}
                    onClick={() => activate(row())}
                    onContextMenu={(event) => {
                      // Groups of the Solution view without a real folder or a file have no menu.
                      if (!row().path && !row().projectFile) return;
                      if (openTreeMenu(row().path, row().kind === "folder", event.clientX, event.clientY, row().projectFile)) {
                        event.preventDefault();
                      }
                    }}
                  >
                    <span class="tree-caret">
                      <Show when={row().kind === "folder"}>
                        <Chevron open={row().open} />
                      </Show>
                    </span>
                    <Show
                      when={row().kind === "folder"}
                      fallback={<FileTypeIcon name={row().label} />}
                    >
                      <GroupIconOf icon={row().icon} />
                    </Show>
                    <span class="tree-label" classList={{ "tree-folder": row().kind === "folder" }}>
                      {row().label}
                      <Show when={row().detail}>
                        <span class="file-directory">{row().detail}</span>
                      </Show>
                      <Show when={row().unlisted}>
                        <span class="tree-unlisted">not in {row().unlisted}</span>
                      </Show>
                    </span>
                    <Show
                      when={status()}
                      fallback={
                        <Show when={row().kind === "folder" && changedGroups().has(row().key)}>
                          <span class="tree-dot" title="Holds changed files">
                            ●
                          </span>
                        </Show>
                      }
                    >
                      {(known) => (
                        <>
                          <Show when={edited().has(row().path)}>
                            <span class="edited-dot" aria-label="uncommitted edits">
                              ●
                            </span>
                          </Show>
                          <span class={`status status-${known()}`}>{STATUS_LETTER[known()]}</span>
                        </>
                      )}
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

function GroupIconOf(props: { icon: Row["icon"] }) {
  return (
    <Show when={props.icon !== "solution"} fallback={<FileTypeIcon name="solution.sln" />}>
      <FolderIcon
        project={props.icon === "fsharpProject" || props.icon === "csharpProject" ? props.icon : undefined}
      />
    </Show>
  );
}

/** "Expand selected" (or "Expand all", with nothing selected) and "Collapse all" at the end of the All files heading. */
export function TreeButtons(props: { controls: () => TreeControls | undefined; filtering: boolean }) {
  const run = (event: MouseEvent, action: (controls: TreeControls) => void) => {
    event.stopPropagation();
    const controls = props.controls();
    if (controls) action(controls);
  };
  const off = () => (props.filtering ? " (not while filtering: the files are listed flat)" : "");
  const all = () => props.controls()?.selected() === null;
  const solution = () => explorerView() === "solution";
  return (
    <>
      <Show when={solutionTree()}>
        <button
          class="dock-action dock-icon-action"
          aria-pressed={solution()}
          title={
            solution()
              ? "Solution view: the projects in the order of the solution, and the files of an F# project in its compile order. The change lists follow this order. Click to show the folders instead."
              : "Folders view: the files in their folders, by name. Click to show the solution's projects, with F# files in compile order."
          }
          aria-label="Solution view"
          onClick={(event) => {
            event.stopPropagation();
            setExplorerView(solution() ? "folders" : "solution");
          }}
        >
          <SolutionViewIcon />
        </button>
      </Show>
      <button
        class="dock-action dock-icon-action"
        disabled={props.filtering}
        title={
          all()
            ? `Expand all: open every folder (*)${off()}`
            : `Expand selected: open the selected folder and every folder in it, or the folders down to the selected file (*)${off()}`
        }
        aria-label={all() ? "Expand all" : "Expand selected"}
        onClick={(event) => run(event, (controls) => controls.expandSelected())}
      >
        <ExpandIcon />
      </button>
      <button
        class="dock-action dock-icon-action"
        disabled={props.filtering}
        title={`Collapse all folders${off()}`}
        aria-label="Collapse all"
        onClick={(event) => run(event, (controls) => controls.collapseAll())}
      >
        <CollapseAllIcon />
      </button>
    </>
  );
}
