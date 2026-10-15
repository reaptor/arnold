import { createSignal, onCleanup, onMount, Show } from "solid-js";
import { AllFilesView } from "./components/AllFilesView";
import { FileDock } from "./components/FileDock";
import { LanguageStatus } from "./components/LanguageStatus";
import { PullRequestPanel, TokenDialog } from "./components/PullRequestPanel";
import { Toolbar } from "./components/Toolbar";
import { startupPath } from "./ipc/commands";
import { isMac } from "./platform";
import { prs } from "./state/pullRequests";
import {
  addComment,
  attachEditor,
  goBack,
  goToDefinition,
  openRepoAt,
  setState,
  startWatching,
  state,
} from "./state/review";

/** "Go back" as editors have it: Ctrl+- on macOS, Alt+Left elsewhere. */
const isBackShortcut = (event: KeyboardEvent) =>
  isMac
    ? event.ctrlKey && !event.metaKey && event.key === "-"
    : event.altKey && !event.ctrlKey && event.key === "ArrowLeft";

const MIN_DOCK = 160;
const MAX_DOCK = 640;

/** Reviewing pull requests, and none is picked yet: the main area lists them instead. */
const choosingPullRequest = () => state.repo !== null && state.source === "pullRequest" && state.pullRequest === null;

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
        <Show when={choosingPullRequest()}>
          <PullRequestPanel />
        </Show>
        <Show when={!choosingPullRequest()}>
          <FileDock width={dockWidth()} />
          <div class="splitter" onPointerDown={startResize} />
        </Show>
        <section class="content" classList={{ hidden: choosingPullRequest() }}>
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
      <Show when={prs.tokenDialog}>
        <TokenDialog />
      </Show>
      <Show when={state.menu}>
        {(menu) => (
          <ul
            class="menu"
            style={{ left: `${menu().x}px`, top: `${menu().y}px` }}
            ref={(list) =>
              // Inside the window, also when opened near its right or bottom edge.
              queueMicrotask(() => {
                const box = list.getBoundingClientRect();
                if (box.right > window.innerWidth - 4) list.style.left = `${Math.max(4, window.innerWidth - box.width - 4)}px`;
                if (box.bottom > window.innerHeight - 4) list.style.top = `${Math.max(4, window.innerHeight - box.height - 4)}px`;
              })
            }
            onPointerDown={(event) => event.stopPropagation()}
          >
            <li classList={{ disabled: !menu().definition }} onClick={() => menu().definition && goToDefinition()}>
              <span>Go to definition</span>
              <kbd>F12</kbd>
            </li>
            <Show when={menu().comment !== undefined}>
              <li
                classList={{ disabled: !menu().comment }}
                onClick={() => menu().comment && addComment()}
                title={menu().comment ? undefined : "Comments go on the pull request's files, with all of its changes shown"}
              >
                <span>Add comment</span>
                <kbd>{menu().comment ?? ""}</kbd>
              </li>
            </Show>
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
            <Show when={state.source === "pullRequest" && state.pullRequest}>
              {(pullRequest) => (
                <span title={pullRequest().url}>
                  Pull request !{pullRequest().id} by {pullRequest().author}
                </span>
              )}
            </Show>
            <span title={`Commits only: ${comparison().mergeBase} to ${comparison().headCommit}`}>
              from {comparison().mergeBase.slice(0, 8)} to {comparison().headCommit.slice(0, 8)}
            </span>
            <span class="toolbar-space" />
            <span class="hint">F12 go to definition · F7 next change</span>
            <LanguageStatus />
          </>
        )}
      </Show>
    </footer>
  );
}
