import { createSignal, onCleanup, onMount, Show } from "solid-js";
import { AllFilesView } from "./components/AllFilesView";
import { FileDock } from "./components/FileDock";
import { Toolbar } from "./components/Toolbar";
import { startupPath } from "./ipc/commands";
import {
  attachEditor,
  goBack,
  openRepoAt,
  resetFileAt,
  resetSelectedLines,
  resolveDiskConflict,
  save,
  saveAll,
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

  // Ctrl/Cmd+S also saves when focus is outside the editors; inside one, the
  // editor saves its own file.
  const onKeyDown = (event: KeyboardEvent) => {
    if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === "s") {
      event.preventDefault();
      const inEditor = event.target instanceof Element && event.target.closest(".cm-editor") !== null;
      if (!inEditor) void (state.view === "all" ? saveAll() : save());
    } else if (event.key === "Escape") {
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

  const canEdit = () => state.comparison?.worktreePath != null;

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
          <Show when={state.view === "file" && state.selected && state.diskConflict[state.selected]}>
            <div class="banner">
              <span>This file changed on disk while you have unsaved edits.</span>
              <button onClick={() => resolveDiskConflict(state.selected, "reload")}>Reload from disk</button>
              <button onClick={() => resolveDiskConflict(state.selected, "keep")}>Keep my edits</button>
            </div>
          </Show>
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
            <li classList={{ disabled: !canEdit() }} onClick={() => canEdit() && resetSelectedLines()}>
              <span>Reset selected lines</span>
              <kbd>Alt+R</kbd>
            </li>
            <li classList={{ disabled: !canEdit() }} onClick={() => canEdit() && resetFileAt(menu().path)}>
              <span>Reset file…</span>
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
                <span class="warning">Read-only: {comparison().head} is not checked out anywhere</span>
              }
            >
              <span class="status-path" title="Checkout being edited">
                {comparison().worktreePath}
              </span>
            </Show>
            <span class="toolbar-space" />
            <Show when={state.lspStatus} fallback={<span class="hint">Alt+R reset lines · F7 next change</span>}>
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
