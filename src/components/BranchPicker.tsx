import { createMemo, createSignal, For, onCleanup, Show } from "solid-js";
import type { Branch } from "../ipc/commands";
import { branchOrder, creatorOf, loadCreators, setBranchOrder, sortBranches } from "../state/branchInfo";
import { fetching, fetchRemotes, state } from "../state/review";
import { ago } from "./CommitPicker";
import { ArrowUpIcon, BranchIcon, ChevronDownIcon, RefreshIcon } from "./icons";

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

/** Who to show with `branch`: its creator when Azure DevOps says, else who committed last. */
function who(branch: Branch): { name: string; created: boolean } | null {
  const creator = state.repo ? creatorOf(state.repo.root, branch) : null;
  if (creator) return { name: creator, created: true };
  return branch.lastAuthor ? { name: branch.lastAuthor, created: false } : null;
}

/** The tooltip of `branch`'s line: who created it, and its last commit. */
function aboutBranch(branch: Branch): string {
  const person = who(branch);
  const lines = [branch.name];
  if (person?.created) lines.push(`Created by ${person.name}`);
  if (branch.updatedAt > 0) {
    const when = new Date(branch.updatedAt * 1000);
    lines.push(`Last commit ${ago(when.toISOString())} by ${branch.lastAuthor || "someone"} (${when.toLocaleString()})`);
  }
  if (branch.worktreePath) lines.push(`Checked out at ${branch.worktreePath}`);
  return lines.join("\n");
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
    const found = props.branches.filter(
      (branch) => branch.name.toLowerCase().includes(needle) || (who(branch)?.name.toLowerCase().includes(needle) ?? false),
    );
    return sortBranches(found, branchOrder());
  });
  /** Whether the repository has a remote to fetch from. */
  const hasRemote = () => state.repo?.branches.some((branch) => branch.isRemote || branch.sync) ?? false;

  const close = () => setOpen(false);
  const toggle = () => {
    setOpen(!open());
    setQuery("");
    setActive(0);
    if (open()) {
      queueMicrotask(() => input.focus());
      if (state.repo) void loadCreators(state.repo.root);
    }
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
        <div class="picker-popup branch-popup" data-tauri-drag-region="false">
          <input
            ref={input}
            class="picker-search"
            placeholder="Find branch or person"
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
                  title={aboutBranch(branch)}
                >
                  <span class="picker-name">{branch.name}</span>
                  <Show when={branch.worktreePath}>
                    <span class="tag">checked out</span>
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
                  <span class="branch-meta">
                    <Show when={who(branch)}>{(person) => <span class="branch-person">{person().name}</span>}</Show>
                    <Show when={branch.updatedAt > 0}>
                      <span class="branch-when">{ago(new Date(branch.updatedAt * 1000).toISOString())}</span>
                    </Show>
                  </span>
                </li>
              )}
            </For>
          </ul>
          <div class="pr-popup-footer branch-popup-footer">
            <div class="segmented" role="group" aria-label="Order of the branches">
              <button
                classList={{ on: branchOrder() === "recent" }}
                onClick={() => setBranchOrder("recent")}
                title="Branches with the latest commits first"
              >
                Recent
              </button>
              <button
                classList={{ on: branchOrder() === "name" }}
                onClick={() => setBranchOrder("name")}
                title="Branches by name"
              >
                A–Z
              </button>
            </div>
            <span class="toolbar-space" />
            <Show when={hasRemote()}>
              <button
                classList={{ "pr-reload": true, busy: fetching() }}
                disabled={fetching()}
                onClick={() => void fetchRemotes()}
                title="Fetch every branch of every remote, so new branches can be picked and checked out; local branches checked out nowhere that are only behind are brought up to date"
              >
                <RefreshIcon />
                {fetching() ? "Fetching…" : "Fetch"}
              </button>
            </Show>
          </div>
        </div>
      </Show>
    </div>
  );
}
