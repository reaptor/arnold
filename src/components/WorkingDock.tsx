import { createEffect, createMemo, createSignal, For, Show, untrack } from "solid-js";
import type { ChangedFile, WorkingSide } from "../ipc/commands";
import { type FileAction, filesAction, listOf, openWorkFile, selectWorkFile, stageAll, unstageAll } from "../state/commitMode";
import { isMac } from "../platform";
import { allFiles, ignoreTarget, noteDeparture, setState, state } from "../state/review";
import { Counts, DockHeading, splitPath, STATUS_LETTER, STATUS_NAME } from "./FileDock";
import { FileTree } from "./FileTree";

const MIN_LIST_HEIGHT = 44;
/** Room the file explorer keeps when the lists above it are made taller. */
const MIN_EXPLORER_HEIGHT = 120;

const remembered = (key: string) => localStorage.getItem(`arnold.${key}`);
const remember = (key: string, value: unknown) => localStorage.setItem(`arnold.${key}`, String(value));

const LIST_NAMES: Record<WorkingSide, string> = { unstaged: "Unstaged changes", staged: "Staged changes" };

/** A file of a list as Git Extensions describes it. */
function fileTitle(side: WorkingSide, file: ChangedFile): string {
  const status =
    side === "unstaged" && file.status === "added" ? "Untracked (new, not added to git yet)" : STATUS_NAME[file.status];
  return `${status}: ${file.oldPath ? `${file.oldPath} → ` : ""}${file.path}`;
}

/**
 * Left dock in Commit mode, in three parts: the unstaged changes, the staged
 * ones (what a commit takes) and every file of the checkout.
 */
export function WorkingDock(props: { width: number; hidden?: boolean }) {
  const [filter, setFilter] = createSignal("");
  const [open, setOpen] = createSignal({
    unstaged: remembered("unstagedOpen") !== "false",
    staged: remembered("stagedOpen") !== "false",
    explorer: remembered("workExplorerOpen") !== "false",
  });
  const [heights, setHeights] = createSignal({
    unstaged: Number(remembered("unstagedHeight")) || 200,
    staged: Number(remembered("stagedHeight")) || 160,
  });
  const lists: Partial<Record<WorkingSide, HTMLUListElement>> = {};
  let explorer!: HTMLElement;

  const needle = () => filter().toLowerCase();
  const matching = (side: WorkingSide) => listOf(side).filter((file) => file.path.toLowerCase().includes(needle()));
  const unstaged = createMemo(() => matching("unstaged"));
  const staged = createMemo(() => matching("staged"));
  const shown = (side: WorkingSide) => (side === "unstaged" ? unstaged() : staged());
  const matchingFiles = createMemo(() =>
    needle() ? allFiles().filter((path) => path.toLowerCase().includes(needle())).length : allFiles().length,
  );

  const toggle = (part: "unstaged" | "staged" | "explorer") => {
    setOpen({ ...open(), [part]: !open()[part] });
    remember(part === "explorer" ? "workExplorerOpen" : `${part}Open`, open()[part]);
  };

  /** A list keeps the height dragged to while something below it is open. */
  const sized = (side: WorkingSide) => (side === "unstaged" ? open().staged || open().explorer : open().explorer);

  const startResize = (side: WorkingSide, down: PointerEvent) => {
    const list = lists[side];
    if (!list || !open()[side]) return;
    const startY = down.clientY;
    const startHeight = list.clientHeight;
    // What the parts below can give up.
    const below =
      side === "unstaged"
        ? (open().staged ? (lists.staged?.clientHeight ?? 0) - MIN_LIST_HEIGHT : 0) + explorer.clientHeight - MIN_EXPLORER_HEIGHT
        : explorer.clientHeight - MIN_EXPLORER_HEIGHT;
    const most = startHeight + Math.max(below, 0);
    const onMove = (move: PointerEvent) => {
      const height = Math.max(Math.min(startHeight + move.clientY - startY, most), MIN_LIST_HEIGHT);
      setHeights({ ...heights(), [side]: height });
    };
    const onUp = () => {
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", onUp);
      remember(`${side}Height`, heights()[side]);
    };
    window.addEventListener("pointermove", onMove);
    window.addEventListener("pointerup", onUp);
    down.preventDefault();
  };

  const selectedIn = (side: WorkingSide, path: string) => state.workSide === side && state.selected === path;

  /**
   * The files picked in one of the lists, for the keys and the menu: the
   * shown file alone, or more with Shift and ⌘/Ctrl. `anchor` is where a
   * Shift range starts, `cursor` the row the arrow keys move from.
   */
  const [picked, setPicked] = createSignal<{ side: WorkingSide | null; paths: ReadonlySet<string>; anchor: string | null; cursor: string | null }>({
    side: null,
    paths: new Set(),
    anchor: null,
    cursor: null,
  });
  const pickOnly = (side: WorkingSide | null, path: string | null) =>
    setPicked({ side, paths: new Set(path === null ? [] : [path]), anchor: path, cursor: path });
  const isPicked = (side: WorkingSide, path: string) => picked().side === side && picked().paths.has(path);
  const pickedFiles = (side: WorkingSide) => shown(side).filter((file) => isPicked(side, file.path));
  const several = (side: WorkingSide) => picked().side === side && picked().paths.size > 1;

  // A file shown some other way (the explorer, the all-files view's scrolling)
  // is picked alone; files picked together stay so while it is one of the lists'.
  createEffect(() => {
    const side = state.workSide;
    const path = state.selected;
    const current = untrack(picked);
    if (side !== null && path !== null && current.side === side && current.paths.has(path)) return;
    if (side !== null && current.side === side && current.paths.size > 1) return;
    pickOnly(side, side === null ? null : path);
  });

  /** The rows from `from` to `to` of a list, both included. */
  const range = (side: WorkingSide, from: string | null, to: string) => {
    const paths = shown(side).map((file) => file.path);
    const end = paths.indexOf(to);
    const start = from === null ? -1 : paths.indexOf(from);
    if (start < 0) return [to];
    return paths.slice(Math.min(start, end), Math.max(start, end) + 1);
  };

  /** A click: alone (shown), ⌘/Ctrl adds or takes away one, Shift picks the rows from the last one clicked. */
  const clickRow = (side: WorkingSide, path: string, event: MouseEvent) => {
    const current = picked();
    const same = current.side === side;
    if (event.shiftKey) {
      const anchor = same ? current.anchor : null;
      setPicked({ side, paths: new Set(range(side, anchor, path)), anchor: anchor ?? path, cursor: path });
    } else if (isMac ? event.metaKey : event.ctrlKey) {
      const paths = new Set(same ? current.paths : []);
      if (paths.has(path)) paths.delete(path);
      else paths.add(path);
      setPicked({ side, paths, anchor: path, cursor: path });
    } else {
      pickOnly(side, path);
      if (!selectedIn(side, path)) noteDeparture();
      void openWorkFile(side, path);
    }
  };

  const move = (side: WorkingSide, delta: number, extend: boolean) => {
    const list = shown(side);
    const current = picked();
    const from = current.side === side && current.cursor !== null ? current.cursor : state.workSide === side ? state.selected : null;
    const index = list.findIndex((file) => file.path === from);
    const next = list[Math.min(Math.max(index + delta, 0), list.length - 1)];
    if (!next) return;
    if (extend) {
      const anchor = current.side === side ? current.anchor : null;
      setPicked({ side, paths: new Set(range(side, anchor, next.path)), anchor: anchor ?? next.path, cursor: next.path });
      return;
    }
    pickOnly(side, next.path);
    if (!selectedIn(side, next.path)) void selectWorkFile(side, next.path);
  };

  const act = (action: FileAction, side: WorkingSide) => {
    const files = pickedFiles(side);
    if (files.length > 0) void filesAction(action, side, files);
  };

  /** S, U, R and Del as in Git Extensions, on every picked file; arrow keys move through the list, Shift picks more. */
  const onKeyDown = (side: WorkingSide, event: KeyboardEvent) => {
    if (event.target instanceof HTMLInputElement || event.altKey) return;
    const key = event.key.toLowerCase();
    const mod = isMac ? event.metaKey : event.ctrlKey;
    // macOS keyboards' delete key is Backspace.
    const deleting = event.key === "Delete" || (isMac && event.key === "Backspace");
    if (mod && key === "a") {
      const paths = shown(side).map((file) => file.path);
      const current = picked();
      setPicked({ side, paths: new Set(paths), anchor: paths[0] ?? null, cursor: current.side === side ? current.cursor : (paths[0] ?? null) });
    } else if (deleting && !event.ctrlKey && !(event.metaKey && !isMac)) {
      act("delete", side);
    } else if (event.metaKey || event.ctrlKey) {
      return;
    } else if (event.key === "ArrowDown") move(side, 1, event.shiftKey);
    else if (event.key === "ArrowUp") move(side, -1, event.shiftKey);
    else if (event.key === "Escape" && several(side)) pickOnly(side, state.workSide === side ? state.selected : null);
    else if (key === "s" && side === "unstaged") act("stage", side);
    else if (key === "u" && side === "staged") act("unstage", side);
    else if (key === "r") act("reset", side);
    else return;
    event.preventDefault();
  };

  const List = (listProps: { side: WorkingSide }) => {
    const side = listProps.side;
    return (
      <ul
        class="file-list"
        ref={(element) => (lists[side] = element)}
        classList={{ sized: sized(side) }}
        tabindex="0"
        aria-label={LIST_NAMES[side]}
        onKeyDown={(event) => onKeyDown(side, event)}
        style={{ height: sized(side) ? `${heights()[side]}px` : undefined }}
      >
        <For
          each={shown(side)}
          fallback={
            <li class="file-empty">
              {state.work ? (side === "unstaged" ? "No unstaged changes" : "Nothing staged yet: S stages a file or lines") : ""}
            </li>
          }
        >
          {(file) => {
            const parts = splitPath(file.path);
            let row!: HTMLLIElement;
            createEffect(() => {
              if (selectedIn(side, file.path) || (picked().side === side && picked().cursor === file.path)) row.scrollIntoView({ block: "nearest" });
            });
            return (
              <li
                ref={row}
                classList={{
                  file: true,
                  selected: isPicked(side, file.path),
                  cursor: several(side) && picked().cursor === file.path,
                }}
                title={fileTitle(side, file)}
                // Shift+click picks rows, it does not select their text.
                onMouseDown={(event) => event.shiftKey && event.preventDefault()}
                onClick={(event) => clickRow(side, file.path, event)}
                onContextMenu={(event) => {
                  event.preventDefault();
                  // On a file picked with others: the menu is for all of them.
                  const together = several(side) && isPicked(side, file.path);
                  if (!together) {
                    pickOnly(side, file.path);
                    void selectWorkFile(side, file.path);
                  }
                  setState("menu", {
                    x: event.clientX,
                    y: event.clientY,
                    definition: false,
                    file: file.path,
                    files: together ? pickedFiles(side).map((picked) => picked.path) : [file.path],
                    side,
                    lines: side === "unstaged" ? ["stage", "reset"] : ["unstage", "reset"],
                    // Untracked, so ignoring takes it off the list.
                    ignore: !together && side === "unstaged" && file.status === "added" ? ignoreTarget(file.path, false) : undefined,
                  });
                }}
              >
                <span class={`status status-${file.status}`}>{STATUS_LETTER[file.status]}</span>
                <span class="file-path">
                  <span class="file-name">{parts.name}</span>
                  <span class="file-directory">{parts.directory}</span>
                </span>
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
    );
  };

  const where = () => state.work?.branch ?? (state.work ? "detached HEAD" : "");

  return (
    <aside class="dock" classList={{ hidden: props.hidden }} style={{ width: `${props.width}px` }}>
      <div class="dock-header">
        <input
          class="dock-filter"
          placeholder="Filter the changes & All files"
          title="Shows only the files whose path contains this text, in all three lists."
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
      {/* Without the explorer, the last list open takes the room. */}
      <div class="dock-island" classList={{ grow: !open().explorer && !open().staged }}>
        <section class="dock-section" classList={{ grow: !open().explorer && !open().staged }}>
          <DockHeading
            open={open().unstaged}
            onToggle={() => toggle("unstaged")}
            title="Unstaged"
            detail=""
            count={listOf("unstaged").length}
            matching={filter() ? unstaged().length : undefined}
            explanation="Changes in the working tree that are not staged, untracked files included. A file picked here shows index → working tree and can be edited. S stages the file (or in the diff, the selected lines), R resets it, Del deletes it from disk. Shift or Ctrl/⌘+click picks several files for the keys and the right-click menu."
          >
            <Show when={listOf("unstaged").length > 0}>
              <button
                class="dock-action"
                title="Stage every unstaged file"
                onClick={(event) => {
                  event.stopPropagation();
                  void stageAll();
                }}
              >
                Stage all
              </button>
            </Show>
          </DockHeading>
          <Show when={open().unstaged}>
            <List side="unstaged" />
          </Show>
        </section>
      </div>
      <div class="dock-sash" classList={{ fixed: !open().unstaged || !sized("unstaged") }} onPointerDown={(down) => startResize("unstaged", down)} />
      <div class="dock-island" classList={{ grow: !open().explorer }}>
        <section class="dock-section" classList={{ grow: !open().explorer }}>
          <DockHeading
            open={open().staged}
            onToggle={() => toggle("staged")}
            title="Staged"
            detail={where() ? `on ${where()}` : ""}
            count={listOf("staged").length}
            matching={filter() ? staged().length : undefined}
            explanation="What the next commit takes: the index against the last commit. A file picked here shows HEAD → index. U unstages the file (or in the diff, the selected lines), R resets it, Del deletes it from disk. Shift or Ctrl/⌘+click picks several files for the keys and the right-click menu."
          >
            <Show when={listOf("staged").length > 0}>
              <button
                class="dock-action"
                title="Unstage every staged file"
                onClick={(event) => {
                  event.stopPropagation();
                  void unstageAll();
                }}
              >
                Unstage all
              </button>
            </Show>
          </DockHeading>
          <Show when={open().staged}>
            <List side="staged" />
          </Show>
        </section>
      </div>
      <div class="dock-sash" classList={{ fixed: !open().staged || !sized("staged") }} onPointerDown={(down) => startResize("staged", down)} />
      <section class="dock-section dock-island" classList={{ grow: open().explorer }} ref={explorer}>
        <DockHeading
          open={open().explorer}
          onToggle={() => toggle("explorer")}
          title="All files"
          detail="in the working tree"
          count={allFiles().length}
          matching={filter() ? matchingFiles() : undefined}
          explanation="File explorer: every file of the checkout as it is on disk, tracked or not (ignored files left out). Files with uncommitted changes are marked with their status letter. Click a file to open it; it can be edited."
        />
        <Show when={open().explorer}>
          <FileTree filter={filter()} />
        </Show>
      </section>
    </aside>
  );
}
