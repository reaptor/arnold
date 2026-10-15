import { createSignal, onCleanup, onMount, Show } from "solid-js";
import { AllFilesView } from "./components/AllFilesView";
import { FileDock } from "./components/FileDock";
import { Toolbar } from "./components/Toolbar";
import { startupPath } from "./ipc/commands";
import {
  attachEditor,
  goBack,
  goToDefinition,
  openRepoAt,
  setState,
  startWatching,
  state,
} from "./state/review";

const isMac = navigator.platform.toLowerCase().includes("mac");

/** "Go back" as editors have it: Ctrl+- on macOS, Alt+Left elsewhere. */
const isBackShortcut = (event: KeyboardEvent) =>
  isMac
    ? event.ctrlKey && !event.metaKey && event.key === "-"
    : event.altKey && !event.ctrlKey && event.key === "ArrowLeft";

const MIN_DOCK = 160;
const MAX_DOCK = 640;

export function App() {
  const [dockWidth, setDockWidth] = createSignal(Number(localStorage.getItem("arnold.dockWidth")) || 280);
  let editorHost!: HTMLDivElement;

  onMount(() => {
    attachEditor(editorHost);
    void startWatching();
    void startupPath().then((fromCommandLine) => {
      const path = fromCommandLine ?? localStorage.getItem("arnold.lastRepo");
      if (path) void openRepoAt(path);
    });
  });

  const onKeyDown = (event: KeyboardEvent) => {
    if (event.key === "Escape") {
      setState("menu", null);
    } else if (isBackShortcut(event) && state.backCount > 0) {
      event.preventDefault();
      event.stopPropagation();
      void goBack();
    }
  };
  const closeMenu = () => setState("menu", null);
  // Capture phase: the editor would otherwise use the back shortcut itself.
  window.addEventListener("keydown", onKeyDown, true);
  window.addEventListener("pointerdown", closeMenu);
  onCleanup(() => {
    window.removeEventListener("keydown", onKeyDown, true);
    window.removeEventListener("pointerdown", closeMenu);
  });

  const startResize = (down: PointerEvent) => {
    const startX = down.clientX;
    const startWidth = dockWidth();
    const onMove = (move: PointerEvent) =>
      setDockWidth(Math.min(Math.max(startWidth + move.clientX - startX, MIN_DOCK), MAX_DOCK));
    const onUp = () => {
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", onUp);
      localStorage.setItem("arnold.dockWidth", String(dockWidth()));
    };
    window.addEventListener("pointermove", onMove);
    window.addEventListener("pointerup", onUp);
    down.preventDefault();
  };


  return (
    <div class="app">
      <Toolbar />
      <Show when={state.error}>
        <div class="banner error">
          <span>{state.error}</span>
          <button onClick={() => setState("error", null)}>Dismiss</button>
        </div>
      </Show>
      <div class="main">
        <FileDock width={dockWidth()} />
        <div class="splitter" onPointerDown={startResize} />
        <section class="content">
          <div
            class="editor-host diff-host"
            classList={{ hidden: state.view === "all" || state.placeholder !== null }}
            ref={editorHost}
          />
          <Show when={state.view === "file" && state.placeholder}>
            <div class="placeholder">{state.placeholder}</div>
          </Show>
          <Show when={state.view === "all"}>
            <AllFilesView />
          </Show>
        </section>
      </div>
      <StatusBar />
      <Show when={state.menu}>
        {(menu) => (
          <ul
            class="menu"
            style={{ left: `${menu().x}px`, top: `${menu().y}px` }}
            onPointerDown={(event) => event.stopPropagation()}
          >
            <li classList={{ disabled: !menu().definition }} onClick={() => menu().definition && goToDefinition()}>
              <span>Go to definition</span>
              <kbd>F12</kbd>
            </li>
          </ul>
        )}
      </Show>
    </div>
  );
}

function StatusBar() {
  const totals = () => {
    let added = 0;
    let removed = 0;
    for (const file of state.comparison?.files ?? []) {
      added += file.additions ?? 0;
      removed += file.deletions ?? 0;
    }
    return { added, removed };
  };
  return (
    <footer class="statusbar">
      <Show when={state.comparison} fallback={<span>{state.busy ? "Loading…" : ""}</span>}>
        {(comparison) => (
          <>
            <span>
              {comparison().files.length} files <span class="added">+{totals().added}</span>{" "}
              <span class="removed">−{totals().removed}</span>
            </span>
            <span title={comparison().mergeBase}>from {comparison().mergeBase.slice(0, 8)}</span>
            <Show
              when={comparison().worktreePath}
              fallback={
                <span class="warning">
                  {comparison().head} is not checked out anywhere: showing its last commit, without language
                  features
                </span>
              }
            >
              <span class="status-path" title="Checkout being reviewed">
                {comparison().worktreePath}
              </span>
            </Show>
            <span class="toolbar-space" />
            <Show when={state.lspStatus} fallback={<span class="hint">F12 go to definition · F7 next change</span>}>
              <span class="status-path" title={state.lspStatus ?? ""}>
                {state.lspStatus}
              </span>
            </Show>
          </>
        )}
      </Show>
    </footer>
  );
}
