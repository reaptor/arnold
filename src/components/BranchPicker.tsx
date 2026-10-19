import { createMemo, createSignal, For, onCleanup, Show } from "solid-js";
import type { Branch } from "../ipc/commands";
import { ArrowUpIcon, BranchIcon, ChevronDownIcon } from "./icons";

interface Props {
  label: string;
  value: string;
  branches: Branch[];
  onChange(name: string): void;
  /** A pill after the chosen branch on the button: what it has that is not pushed. */
  mark?: PickerMark | null;
}

export interface PickerMark {
  text: string;
  title: string;
}

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

/** What a local branch has that its remote lacks, as a short mark and its explanation. */
export function unpushed(branch: Branch | undefined): { label: string; count: number; title: string } | null {
  const sync = branch?.sync;
  if (!sync) return null;
  const behind = sync.behind > 0 ? `, and ${sync.remote}/${sync.remoteBranch} has ${plural(sync.behind, "commit")} it lacks` : "";
  if (!sync.published) {
    const only = sync.ahead > 0 ? `: ${plural(sync.ahead, "commit")} only on this computer` : "";
    return { label: "not pushed", count: sync.ahead, title: `${branch!.name} is not on ${sync.remote}${only}` };
  }
  if (sync.ahead === 0) return null;
  return {
    label: `${sync.ahead} unpushed`,
    count: sync.ahead,
    title: `${plural(sync.ahead, "commit")} of ${branch!.name} not pushed to ${sync.remote}/${sync.remoteBranch}${behind}`,
  };
}

/** The pill for local branch `name` of `branches` when it has commits to push. */
export function unpushedMark(branches: Branch[], name: string): PickerMark | null {
  const mark = unpushed(branches.find((branch) => !branch.isRemote && branch.name === name));
  return mark && { text: mark.count > 0 ? String(mark.count) : mark.label, title: mark.title };
}

/** Button that opens a searchable list of branches. */
export function BranchPicker(props: Props) {
  const [open, setOpen] = createSignal(false);
  const [query, setQuery] = createSignal("");
  const [active, setActive] = createSignal(0);
  let root!: HTMLDivElement;
  let input!: HTMLInputElement;

  const matches = createMemo(() => {
    const needle = query().toLowerCase();
    return props.branches.filter((branch) => branch.name.toLowerCase().includes(needle));
  });

  const close = () => setOpen(false);
  const toggle = () => {
    setOpen(!open());
    setQuery("");
    setActive(0);
    if (open()) queueMicrotask(() => input.focus());
  };
  const choose = (branch: Branch | undefined) => {
    if (!branch) return;
    close();
    if (branch.name !== props.value) props.onChange(branch.name);
  };

  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) close();
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  const onKeyDown = (event: KeyboardEvent) => {
    if (event.key === "Escape") close();
    else if (event.key === "Enter") choose(matches()[active()]);
    else if (event.key === "ArrowDown") setActive(Math.min(active() + 1, matches().length - 1));
    else if (event.key === "ArrowUp") setActive(Math.max(active() - 1, 0));
    else return;
    event.preventDefault();
  };

  return (
    <div class="picker branch-picker" ref={root}>
      <button
        class="picker-button"
        onClick={toggle}
        title={`${props.label}: ${props.value}${props.mark ? `\n${props.mark.title}` : ""}`}
      >
        <BranchIcon />
        <span class="picker-value">{props.value || "—"}</span>
        <Show when={props.mark}>
          {(mark) => (
            <span class="unpushed-mark">
              <ArrowUpIcon />
              {mark().text}
            </span>
          )}
        </Show>
        <ChevronDownIcon />
      </button>
      <Show when={open()}>
        <div class="picker-popup" data-tauri-drag-region="false">
          <input
            ref={input}
            class="picker-search"
            placeholder="Find branch"
            value={query()}
            onInput={(event) => {
              setQuery(event.currentTarget.value);
              setActive(0);
            }}
            onKeyDown={onKeyDown}
          />
          <ul class="picker-list">
            <For each={matches()} fallback={<li class="picker-empty">No matching branch</li>}>
              {(branch, index) => (
                <li
                  classList={{ active: index() === active(), current: branch.name === props.value }}
                  onPointerEnter={() => setActive(index())}
                  onClick={() => choose(branch)}
                >
                  <span class="picker-name">{branch.name}</span>
                  <Show when={branch.worktreePath}>
                    <span class="tag" title={`Checked out at ${branch.worktreePath}`}>
                      checked out
                    </span>
                  </Show>
                  <Show when={branch.isRemote}>
                    <span class="tag">remote</span>
                  </Show>
                  <Show when={unpushed(branch)}>
                    {(mark) => (
                      <span class="tag tag-unpushed" title={mark().title}>
                        {mark().label}
                      </span>
                    )}
                  </Show>
                </li>
              )}
            </For>
          </ul>
        </div>
      </Show>
    </div>
  );
}
