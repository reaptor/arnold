import { createMemo, createSignal, For, onCleanup, Show } from "solid-js";
import type { Branch } from "../ipc/commands";
import { BranchIcon, ChevronDownIcon } from "./icons";

interface Props {
  label: string;
  value: string;
  branches: Branch[];
  onChange(name: string): void;
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
    <div class="picker" ref={root}>
      <button class="picker-button" onClick={toggle} title={`${props.label}: ${props.value}`}>
        <BranchIcon />
        <span class="picker-value">{props.value || "—"}</span>
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
                </li>
              )}
            </For>
          </ul>
        </div>
      </Show>
    </div>
  );
}
