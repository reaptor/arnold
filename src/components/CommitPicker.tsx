import { createEffect, createMemo, createSignal, For, onCleanup, Show } from "solid-js";
import type { Commit, CommitRange } from "../ipc/commands";
import { isPicked, onlyCommit, pickedCount, toggleCommit } from "../state/commitRange";
import { CommitIcon, ChevronDownIcon } from "./icons";

interface Props {
  commits: Commit[];
  /** The commits shown, or null for all of the changes. */
  value: CommitRange | null;
  onChange(range: CommitRange | null): void;
}

const relativeTime = new Intl.RelativeTimeFormat(undefined, { numeric: "auto" });
const UNITS: [Intl.RelativeTimeFormatUnit, number][] = [
  ["year", 365 * 24 * 3600],
  ["month", 30 * 24 * 3600],
  ["week", 7 * 24 * 3600],
  ["day", 24 * 3600],
  ["hour", 3600],
  ["minute", 60],
];

/** "3 hours ago", "yesterday", ... */
export function ago(iso: string, now = Date.now()): string {
  const seconds = (Date.parse(iso) - now) / 1000;
  for (const [unit, size] of UNITS) {
    if (Math.abs(seconds) >= size) return relativeTime.format(Math.round(seconds / size), unit);
  }
  return relativeTime.format(0, "minute");
}

const plural = (count: number) => `${count} commit${count === 1 ? "" : "s"}`;

/**
 * Picks what the diff shows, as in an Azure DevOps pull request: all of the
 * branch's changes, or what some of its commits changed. Ticking commits
 * picks a run of them (see `toggleCommit`); clicking a row picks only that one.
 */
export function CommitPicker(props: Props) {
  const [open, setOpen] = createSignal(false);
  const [query, setQuery] = createSignal("");
  const [active, setActive] = createSignal(0);
  let root!: HTMLDivElement;
  let input!: HTMLInputElement;
  let list!: HTMLUListElement;
  let popup!: HTMLDivElement;

  const count = createMemo(() => pickedCount(props.commits, props.value));
  const label = createMemo(() => {
    if (!props.value) return "All changes";
    if (count() === 1) return props.commits.find((commit) => commit.sha === props.value!.newest)?.subject ?? "1 commit";
    return plural(count());
  });

  /** The rows of the list, as positions in `commits`; -1 stands for "All changes". */
  const rows = createMemo((): number[] => {
    const needle = query().toLowerCase();
    const indexes = props.commits.flatMap((commit, index) => (commit.subject.toLowerCase().includes(needle) ? [index] : []));
    return needle ? indexes : [-1, ...indexes];
  });

  const close = () => setOpen(false);
  const toggle = () => {
    setOpen(!open());
    setQuery("");
    setActive(0);
    if (open()) {
      queueMicrotask(() => {
        // In a narrow window the list moves left rather than off the edge.
        const overflow = popup.getBoundingClientRect().right - (document.documentElement.clientWidth - 8);
        if (overflow > 0) popup.style.left = `${-overflow}px`;
        input.focus();
      });
    }
  };
  /** Clicking a row: only that commit, or all changes. */
  const choose = (position: number) => {
    const index = rows()[position];
    if (index === undefined) return;
    close();
    props.onChange(index === -1 ? null : onlyCommit(props.commits, index));
  };
  /** Ticking a box: the list stays open to tick more. */
  const tick = (position: number) => {
    const index = rows()[position];
    if (index === undefined) return;
    setActive(position);
    props.onChange(index === -1 ? null : toggleCommit(props.commits, props.value, index));
  };

  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) close();
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  const move = (position: number) => {
    setActive(position);
    list.children[position]?.scrollIntoView({ block: "nearest" });
  };
  const onKeyDown = (event: KeyboardEvent) => {
    if (event.key === "Escape") close();
    else if (event.key === "Enter") choose(active());
    // Space ticks, except while typing words into the search.
    else if (event.key === " " && !query()) tick(active());
    else if (event.key === "ArrowDown") move(Math.min(active() + 1, rows().length - 1));
    else if (event.key === "ArrowUp") move(Math.max(active() - 1, 0));
    else return;
    event.preventDefault();
  };

  return (
    <div class="picker commit-picker" ref={root}>
      <button class="picker-button" onClick={toggle} title={`Showing ${props.value ? plural(count()) : "all changes"}: ${label()}`}>
        <CommitIcon />
        <span class="picker-value">{label()}</span>
        <ChevronDownIcon />
      </button>
      <Show when={open()}>
        <div ref={popup} class="picker-popup commit-popup" data-tauri-drag-region="false">
          <input
            ref={input}
            class="picker-search"
            placeholder="Find commit"
            value={query()}
            onInput={(event) => {
              setQuery(event.currentTarget.value);
              setActive(0);
            }}
            onKeyDown={onKeyDown}
          />
          <ul class="picker-list" ref={list}>
            <For each={rows()} fallback={<li class="picker-empty">No matching commit</li>}>
              {(index, position) => {
                const commit = () => props.commits[index];
                const checked = () => (index === -1 ? !props.value : isPicked(props.commits, props.value, index));
                return (
                  <li
                    classList={{ active: position() === active(), "commit-all": index === -1 }}
                    onPointerEnter={() => setActive(position())}
                    onClick={() => choose(position())}
                  >
                    <input
                      type="checkbox"
                      class="commit-check"
                      tabIndex={-1}
                      checked={checked()}
                      ref={(box) => createEffect(() => (box.indeterminate = index === -1 && !!props.value))}
                      onClick={(event) => event.stopPropagation()}
                      onChange={(event) => {
                        tick(position());
                        // The box follows the range, which may keep it as it was.
                        event.currentTarget.checked = checked();
                      }}
                    />
                    <Show
                      when={index !== -1}
                      fallback={
                        <>
                          <span class="picker-name">All changes</span>
                          <span class="commit-meta">{plural(props.commits.length)}</span>
                        </>
                      }
                    >
                      <span class="picker-name" title={commit().subject}>
                        {commit().subject || "(no message)"}
                      </span>
                      <span class="commit-meta" title={new Date(commit().date).toLocaleString()}>
                        {ago(commit().date)}
                      </span>
                    </Show>
                  </li>
                );
              }}
            </For>
          </ul>
        </div>
      </Show>
    </div>
  );
}
