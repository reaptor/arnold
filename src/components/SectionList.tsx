import { createMemo, For, type JSX, onCleanup, onMount, Show } from "solid-js";
import type { ChangedFile } from "../ipc/commands";
import { state } from "../state/review";
import { Chevron } from "./Chevron";
import { Counts, EditedMark, showsEdited, splitPath, STATUS_LETTER, STATUS_NAME } from "./FileDock";

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

/** A file of the list; `key` tells it apart when the same file is listed twice. */
export interface SectionEntry {
  key: string;
  file: ChangedFile;
  /** A heading shown above this file, starting a group of files. */
  group?: string;
}

/** Where the files come from and what happens to them: the review's diff, or Commit mode's lists. */
export interface SectionSource {
  entries(): readonly SectionEntry[];
  /** The key of the file selected in the dock. */
  selected(): string | null;
  select(key: string): void;
  collapsed(key: string): boolean;
  setCollapsed(key: string, collapsed: boolean): void;
  /** `host` is where the file is shown once it is near the screen. */
  register(key: string, host: HTMLElement): void;
  unregister(key: string): void;
  /** Loads the file into its host, if not done yet. */
  show(key: string): Promise<unknown>;
  /** Unloads the file's editor; `keepFocused` leaves the one with keyboard focus. Returns whether one was removed. */
  hide(key: string, keepFocused: boolean): boolean;
  /** How the list is asked to scroll a file into view; null when the list goes away. */
  onReveal(handler: ((key: string) => void) | null): void;
  /** Shown when there are no files. */
  placeholder(): string;
  /** Buttons at the end of a file's header. */
  actions?(entry: SectionEntry): JSX.Element;
}

/**
 * Files in one scrolling list, each a full diff, like the "all files" view
 * of a pull request. Editors are created as files come near the screen and
 * dropped again when far away.
 */
export function SectionList(props: { source: SectionSource }) {
  const source = props.source;
  let scroller!: HTMLDivElement;
  const entries = createMemo(() => source.entries());
  const keys = createMemo(() => entries().map((entry) => entry.key));
  const entryOf = (key: string) => entries().find((entry) => entry.key === key);
  const sections = new Map<string, HTMLElement>();

  const load = (section: HTMLElement) => {
    const key = section.dataset.key!;
    if (source.collapsed(key)) return;
    void source.show(key).then(() => {
      const host = hostOf(section);
      // The real height replaces the estimate once something is shown.
      if (host?.firstChild) host.style.minHeight = "";
    });
  };

  /** Drops a file's editor, keeping its height so nothing below moves. */
  const unload = (section: HTMLElement, keepFocused: boolean) => {
    const host = hostOf(section);
    const height = host?.offsetHeight ?? 0;
    if (source.hide(section.dataset.key!, keepFocused) && host) host.style.minHeight = `${height}px`;
  };

  let loader: IntersectionObserver;
  let unloader: IntersectionObserver;
  let resizer: ResizeObserver;
  const heights = new WeakMap<Element, number>();

  // Runs before the sections' own onMount, so the observers exist when they register.
  onMount(() => {
    loader = new IntersectionObserver(
      (records) => records.forEach((record) => record.isIntersecting && load(record.target as HTMLElement)),
      { root: scroller, rootMargin: LOAD_MARGIN },
    );
    unloader = new IntersectionObserver(
      (records) => records.forEach((record) => !record.isIntersecting && unload(record.target as HTMLElement, true)),
      { root: scroller, rootMargin: UNLOAD_MARGIN },
    );
    // Files above the screen change height as they load. Keep what is on
    // screen still by scrolling along (WebKit has no scroll anchoring).
    resizer = new ResizeObserver((records) => {
      const changed = records
        .map((record) => record.target as HTMLElement)
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
    source.onReveal(reveal);
    // The sections register in their own onMount, after this one.
    requestAnimationFrame(() => {
      const selected = source.selected();
      if (selected) reveal(selected);
    });
  });

  onCleanup(() => {
    cancelAnimationFrame(spyFrame);
    loader.disconnect();
    unloader.disconnect();
    resizer.disconnect();
    source.onReveal(null);
  });

  function reveal(key: string) {
    const section = sections.get(key);
    if (!section) return;
    // Scrolls to the header only: a collapsed file (a deleted one starts so) stays collapsed.
    // The first file of a group brings its heading along.
    const heading = section.previousElementSibling;
    const top = heading instanceof HTMLHeadingElement ? heading : section;
    scroller.scrollTop = top.offsetTop - REVEAL_GAP;
    // A short file may sit above the reading line; it stays selected until scrolling moves on.
    spied = fileAtReadingLine();
    load(section);
  }

  const observe = (key: string, section: HTMLElement) => {
    sections.set(key, section);
    loader.observe(section);
    unloader.observe(section);
    resizer.observe(section);
  };

  const unobserve = (key: string, section: HTMLElement) => {
    sections.delete(key);
    loader.unobserve(section);
    unloader.unobserve(section);
    resizer.unobserve(section);
  };

  const toggle = (key: string) => {
    const section = sections.get(key);
    if (!section) return;
    source.select(key);
    if (source.collapsed(key)) {
      source.setCollapsed(key, false);
      load(section);
    } else {
      unload(section, false);
      source.setCollapsed(key, true);
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
    for (const key of keys()) {
      const section = sections.get(key);
      if (!section) continue;
      if (section.offsetTop > probe) break;
      found = key;
    }
    return found ?? keys()[0] ?? null;
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
      const key = fileAtReadingLine();
      if (key === spied) return;
      spied = key;
      if (key && key !== source.selected()) source.select(key);
    });
  };

  return (
    <div class="all-files" ref={scroller} onScroll={onScroll}>
      <For each={keys()} fallback={<div class="placeholder">{source.placeholder()}</div>}>
        {(key) => (
          <Show when={entryOf(key)}>
            {(entry) => (
              <>
                <Show when={entry().group}>{(group) => <h3 class="section-group">{group()}</h3>}</Show>
                <FileSection
                  entry={entry()}
                  source={source}
                  observe={observe}
                  unobserve={unobserve}
                  toggle={toggle}
                />
              </>
            )}
          </Show>
        )}
      </For>
    </div>
  );
}

function FileSection(props: {
  entry: SectionEntry;
  source: SectionSource;
  observe(key: string, section: HTMLElement): void;
  unobserve(key: string, section: HTMLElement): void;
  toggle(key: string): void;
}) {
  // The list is keyed, so a section always shows the same file.
  const key = props.entry.key;
  const file = () => props.entry.file;
  const path = file().path;
  const parts = splitPath(path);
  let section!: HTMLElement;
  let host!: HTMLDivElement;

  onMount(() => {
    host.style.minHeight = `${estimatedHeight(file())}px`;
    props.source.register(key, host);
    props.observe(key, section);
  });
  onCleanup(() => {
    props.unobserve(key, section);
    props.source.unregister(key);
  });

  const collapsed = () => props.source.collapsed(key);
  const title = () =>
    `${STATUS_NAME[file().status]}${showsEdited(file()) ? ", with uncommitted edits" : ""}: ${file().oldPath ? `${file().oldPath} → ` : ""}${path}`;

  return (
    <section
      class="file-section"
      classList={{ selected: props.source.selected() === key }}
      data-key={key}
      data-path={path}
      ref={section}
    >
      <header class="section-header" title={title()} onClick={() => props.toggle(key)}>
        <Chevron class="section-caret" open={!collapsed()} />
        <span class={`status status-${file().status}`}>{STATUS_LETTER[file().status]}</span>
        <span class="section-path">
          <Show when={file().oldPath}>
            <span class="section-directory">{file().oldPath} → </span>
          </Show>
          <span class="section-directory">{parts.directory}</span>
          <span class="section-name">{parts.name}</span>
        </span>
        <Show when={showsEdited(file())}>
          <EditedMark />
        </Show>
        <Show when={state.problems[path]?.errors}>
          <span class="problems" title="Errors reported by the language server">
            {state.problems[path].errors}
          </span>
        </Show>
        <span class="counts">
          <Show when={file().binary} fallback={<Counts file={file()} />}>
            <span class="binary">bin</span>
          </Show>
        </span>
        <Show when={props.source.actions}>
          {(actions) => (
            <span class="section-actions" onClick={(event) => event.stopPropagation()}>
              {actions()(props.entry)}
            </span>
          )}
        </Show>
      </header>
      <div class="diff-host section-host" classList={{ hidden: collapsed() }} ref={host} />
    </section>
  );
}
