import { createMemo, For, onCleanup, onMount, Show } from "solid-js";
import type { ChangedFile } from "../ipc/commands";
import {
  hideSection,
  onRevealSection,
  registerSection,
  resetFileAt,
  resolveDiskConflict,
  save,
  setCollapsed,
  setState,
  showSection,
  state,
  unregisterSection,
} from "../state/review";
import { Chevron } from "./Chevron";
import { Counts, splitPath, STATUS_LETTER, STATUS_NAME } from "./FileDock";

/** Files this close to the screen are loaded into an editor... */
const LOAD_MARGIN = "1500px 0px";
/** ...and unloaded again once they are this far away. */
const UNLOAD_MARGIN = "5000px 0px";
/** Editor line height: 13px font at line-height 1.5. */
const LINE_HEIGHT = 19.5;
/** Gap kept above a file scrolled to. */
const REVEAL_GAP = 8;
/**
 * Where the eye rests while reading, as a share of the view's height: the
 * file under this line is the one selected in the dock.
 */
const READING_LINE = 0.3;

/** Height of a file's diff before it is loaded: about one row per changed line. */
function estimatedHeight(file: ChangedFile) {
  const rows = (file.additions ?? 0) + (file.deletions ?? 0) + 8;
  return Math.min(rows, 300) * LINE_HEIGHT;
}

const hostOf = (section: HTMLElement) => section.querySelector<HTMLElement>(".section-host");

/**
 * Every file of the diff in one scrolling list, like the "all files" view of
 * a pull request. Each file is a full editable diff; editors are created as
 * files come near the screen and dropped again when far away.
 */
export function AllFilesView() {
  let scroller!: HTMLDivElement;
  const files = createMemo(() => [...(state.comparison?.files ?? []), ...state.visited]);
  const sections = new Map<string, HTMLElement>();

  const load = (section: HTMLElement) => {
    const path = section.dataset.path!;
    if (state.collapsed[path]) return;
    void showSection(path).then(() => {
      const host = hostOf(section);
      // The real height replaces the estimate once something is shown.
      if (host?.firstChild) host.style.minHeight = "";
    });
  };

  /** Drops a file's editor, keeping its height so nothing below moves. */
  const unload = (section: HTMLElement, keepFocused: boolean) => {
    const host = hostOf(section);
    const height = host?.offsetHeight ?? 0;
    if (hideSection(section.dataset.path!, keepFocused) && host) host.style.minHeight = `${height}px`;
  };

  let loader: IntersectionObserver;
  let unloader: IntersectionObserver;
  let resizer: ResizeObserver;
  const heights = new WeakMap<Element, number>();

  // Runs before the sections' own onMount, so the observers exist when they register.
  onMount(() => {
    loader = new IntersectionObserver(
      (entries) => entries.forEach((entry) => entry.isIntersecting && load(entry.target as HTMLElement)),
      { root: scroller, rootMargin: LOAD_MARGIN },
    );
    unloader = new IntersectionObserver(
      (entries) => entries.forEach((entry) => !entry.isIntersecting && unload(entry.target as HTMLElement, true)),
      { root: scroller, rootMargin: UNLOAD_MARGIN },
    );
    // Files above the screen change height as they load. Keep what is on
    // screen still by scrolling along (WebKit has no scroll anchoring).
    resizer = new ResizeObserver((entries) => {
      const changed = entries
        .map((entry) => entry.target as HTMLElement)
        .sort((a, b) => a.offsetTop - b.offsetTop);
      let moved = 0;
      let shift = 0;
      for (const section of changed) {
        const height = section.offsetHeight;
        const before = heights.get(section);
        heights.set(section, height);
        if (before === undefined || before === height) continue;
        const top = section.offsetTop - moved;
        if (top + before <= scroller.scrollTop) shift += height - before;
        moved += height - before;
      }
      if (shift !== 0) scroller.scrollTop += shift;
      // Files growing or shrinking is not scrolling: the selection stays.
      spied = fileAtReadingLine();
    });
    onRevealSection(reveal);
    // The sections register in their own onMount, after this one.
    requestAnimationFrame(() => state.selected && reveal(state.selected));
  });

  onCleanup(() => {
    cancelAnimationFrame(spyFrame);
    loader.disconnect();
    unloader.disconnect();
    resizer.disconnect();
    onRevealSection(() => {});
  });

  function reveal(path: string) {
    const section = sections.get(path);
    if (!section) return;
    setCollapsed(path, false);
    scroller.scrollTop = section.offsetTop - REVEAL_GAP;
    // A short file may sit above the reading line; it stays selected until scrolling moves on.
    spied = fileAtReadingLine();
    load(section);
  }

  const observe = (path: string, section: HTMLElement) => {
    sections.set(path, section);
    loader.observe(section);
    unloader.observe(section);
    resizer.observe(section);
  };

  const unobserve = (path: string, section: HTMLElement) => {
    sections.delete(path);
    loader.unobserve(section);
    unloader.unobserve(section);
    resizer.unobserve(section);
  };

  const toggle = (path: string) => {
    const section = sections.get(path);
    if (!section) return;
    setState("selected", path);
    if (state.collapsed[path]) {
      setCollapsed(path, false);
      load(section);
    } else {
      unload(section, false);
      setCollapsed(path, true);
    }
  };

  /**
   * The file under the reading line. Near the ends the line slides to the
   * edge, so the first file is selected at the top and the last at the bottom
   * even when they are short.
   */
  const fileAtReadingLine = (): string | null => {
    const height = scroller.clientHeight;
    const line = READING_LINE * height;
    const fromTop = scroller.scrollTop;
    const fromBottom = scroller.scrollHeight - height - fromTop;
    let offset = Math.min(line, fromTop);
    if (fromBottom < line) offset += ((line - fromBottom) / line) * (height - offset);
    const probe = fromTop + offset;
    let found: string | null = null;
    for (const file of files()) {
      const section = sections.get(file.path);
      if (!section) continue;
      if (section.offsetTop > probe) break;
      found = file.path;
    }
    return found ?? files()[0]?.path ?? null;
  };

  // Scroll spy: the dock follows the file being read. It acts only when the
  // file under the reading line changes, so a file picked or clicked into
  // stays selected until scrolling actually moves on to another one.
  let spied: string | null = null;
  let spyFrame = 0;
  const onScroll = () => {
    if (spyFrame) return;
    spyFrame = requestAnimationFrame(() => {
      spyFrame = 0;
      const path = fileAtReadingLine();
      if (path === spied) return;
      spied = path;
      if (path && path !== state.selected) setState("selected", path);
    });
  };

  return (
    <div
      class="all-files"
      ref={scroller}
      onScroll={onScroll}
    >
      <For
        each={files()}
        fallback={
          <div class="placeholder">
            {state.comparison ? "No differences." : state.repo ? "" : "Open a repository to start reviewing."}
          </div>
        }
      >
        {(file) => (
          <FileSection
            file={file}
            observe={observe}
            unobserve={unobserve}
            toggle={toggle}
          />
        )}
      </For>
    </div>
  );
}

function FileSection(props: {
  file: ChangedFile;
  observe(path: string, section: HTMLElement): void;
  unobserve(path: string, section: HTMLElement): void;
  toggle(path: string): void;
}) {
  // The list is reconciled by path, so a section always shows the same file.
  const path = props.file.path;
  const parts = splitPath(path);
  let section!: HTMLElement;
  let host!: HTMLDivElement;

  onMount(() => {
    host.style.minHeight = `${estimatedHeight(props.file)}px`;
    registerSection(path, host);
    props.observe(path, section);
  });
  onCleanup(() => {
    props.unobserve(path, section);
    unregisterSection(path);
  });

  const canEdit = () => state.comparison?.worktreePath != null;
  const collapsed = () => !!state.collapsed[path];
  const title = () =>
    `${STATUS_NAME[props.file.status]}: ${props.file.oldPath ? `${props.file.oldPath} → ` : ""}${path}`;
  // Buttons in the header do their own thing rather than folding the file.
  const action = (run: () => void) => (event: MouseEvent) => {
    event.stopPropagation();
    run();
  };

  return (
    <section
      class="file-section"
      classList={{ selected: state.selected === path }}
      data-path={path}
      ref={section}
    >
      <header class="section-header" title={title()} onClick={() => props.toggle(path)}>
        <Chevron class="section-caret" open={!collapsed()} />
        <span class={`status status-${props.file.status}`}>{STATUS_LETTER[props.file.status]}</span>
        <span class="section-path">
          <Show when={props.file.oldPath}>
            <span class="section-directory">{props.file.oldPath} → </span>
          </Show>
          <span class="section-directory">{parts.directory}</span>
          <span class="section-name">{parts.name}</span>
        </span>
        <Show when={state.dirty[path]}>
          <span class="unsaved" title="Unsaved edits">
            ●
          </span>
        </Show>
        <Show when={state.problems[path]?.errors}>
          <span class="problems" title="Errors reported by the language server">
            {state.problems[path].errors}
          </span>
        </Show>
        <span class="counts">
          <Show when={props.file.binary} fallback={<Counts file={props.file} />}>
            <span class="binary">bin</span>
          </Show>
        </span>
        <Show when={state.dirty[path]}>
          <button class="section-action" title="Save this file" onClick={action(() => void save(path))}>
            Save
          </button>
        </Show>
        <Show when={canEdit() && props.file.status !== "unchanged"}>
          <button
            class="section-action"
            title="Reset file to the base version"
            onClick={action(() => void resetFileAt(path))}
          >
            ↺ Reset
          </button>
        </Show>
      </header>
      <Show when={state.diskConflict[path]}>
        <div class="banner">
          <span>This file changed on disk while you have unsaved edits.</span>
          <button onClick={() => resolveDiskConflict(path, "reload")}>Reload from disk</button>
          <button onClick={() => resolveDiskConflict(path, "keep")}>Keep my edits</button>
        </div>
      </Show>
      <div class="diff-host section-host" classList={{ hidden: collapsed() }} ref={host} />
    </section>
  );
}
