import { createMemo, createSignal, For, onCleanup, Show } from "solid-js";
import { favorites, folderName, isFavorite, sortFavorites, toggleFavorite } from "../state/favorites";
import { chooseRepo, openRepoAt, state } from "../state/review";
import { ChevronDownIcon } from "./icons";

const parentPath = (path: string) => path.replace(/[\\/]+$/, "").replace(/[\\/][^\\/]*$/, "");

/** The repository button: starred repositories, starring the open one, and "Open repository…". */
export function RepoPicker() {
  const [open, setOpen] = createSignal(false);
  const [active, setActive] = createSignal(0);
  let root!: HTMLDivElement;
  let popup!: HTMLDivElement;

  const starred = createMemo(() => sortFavorites(favorites()));
  // Keyboard-reachable rows: each favorite, then "Open repository…".
  const rowCount = () => starred().length + 1;

  const close = () => setOpen(false);
  const toggle = () => {
    // With nothing to pick from, the menu would only hold "Open repository…".
    if (!state.repo && starred().length === 0) return void chooseRepo();
    setOpen(!open());
    setActive(Math.max(starred().indexOf(state.repo?.root ?? ""), 0));
    if (open()) queueMicrotask(() => popup.focus());
  };
  const choose = (index: number) => {
    close();
    const path = starred()[index];
    if (path === undefined) void chooseRepo();
    else if (path !== state.repo?.root) void openRepoAt(path);
  };

  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) close();
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  const onKeyDown = (event: KeyboardEvent) => {
    if (event.key === "Escape") close();
    else if (event.key === "Enter") choose(active());
    else if (event.key === "ArrowDown") setActive(Math.min(active() + 1, rowCount() - 1));
    else if (event.key === "ArrowUp") setActive(Math.max(active() - 1, 0));
    else return;
    event.preventDefault();
  };

  return (
    <div class="picker" ref={root}>
      <button class="picker-button" onClick={toggle} title={state.repo?.root ?? "Open a git repository"}>
        <Show when={state.repo} fallback={<span class="picker-value">Open repository…</span>}>
          {(repo) => (
            <>
              <span class="picker-value">{folderName(repo().root)}</span>
              <Show when={isFavorite(repo().root)}>
                <span class="star on" title="In favorites">★</span>
              </Show>
            </>
          )}
        </Show>
        <ChevronDownIcon />
      </button>
      <Show when={open()}>
        <div class="picker-popup repo-popup" ref={popup} data-tauri-drag-region="false" tabIndex={-1} onKeyDown={onKeyDown}>
          <Show when={state.repo}>
            {(repo) => (
              <button class="repo-star-current" onClick={() => toggleFavorite(repo().root)}>
                <span class="star" classList={{ on: isFavorite(repo().root) }}>
                  {isFavorite(repo().root) ? "★" : "☆"}
                </span>
                {isFavorite(repo().root) ? "Remove from favorites" : "Add to favorites"}
              </button>
            )}
          </Show>
          <div class="picker-heading">Favorites</div>
          <ul class="picker-list">
            <For each={starred()} fallback={<li class="picker-empty">Star a repository to keep it here.</li>}>
              {(path, index) => (
                <li
                  classList={{ active: index() === active(), current: path === state.repo?.root }}
                  title={path}
                  onPointerEnter={() => setActive(index())}
                  onClick={() => choose(index())}
                >
                  <span class="picker-name">
                    {folderName(path)}
                    <span class="repo-path">{parentPath(path)}</span>
                  </span>
                  <button
                    class="star on"
                    title="Remove from favorites"
                    onClick={(event) => {
                      event.stopPropagation();
                      toggleFavorite(path);
                      setActive(Math.min(active(), rowCount() - 1));
                    }}
                  >
                    ★
                  </button>
                </li>
              )}
            </For>
          </ul>
          <ul class="picker-list repo-actions">
            <li
              classList={{ active: active() === starred().length }}
              onPointerEnter={() => setActive(starred().length)}
              onClick={() => choose(starred().length)}
            >
              <span class="picker-name">Open repository…</span>
            </li>
          </ul>
        </div>
      </Show>
    </div>
  );
}
