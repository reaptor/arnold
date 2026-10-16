import { createSignal, For, onCleanup, onMount, Show } from "solid-js";
import { AllFilesView } from "./components/AllFilesView";
import { WorkingFilesView } from "./components/WorkingFilesView";
import { CommitBox, UnformattedDialog } from "./components/CommitPanel";
import { EditStatus } from "./components/EditStatus";
import { FileDock } from "./components/FileDock";
import { LanguageStatus } from "./components/LanguageStatus";
import { PullRequestPanel, TokenDialog } from "./components/PullRequestPanel";
import { Toolbar } from "./components/Toolbar";
import { WorkingDock } from "./components/WorkingDock";
import { LINE_ACTION_KEYS, type LineAction } from "./editor/diffEditor";
import { startupPath } from "./ipc/commands";
import { isMac } from "./platform";
import { prs } from "./state/pullRequests";
import { fileAction, lineAction, listOf } from "./state/commitMode";
import {
  addComment,
  attachEditor,
  goBack,
  goToDefinition,
  openRepoAt,
  setState,
  startWatching,
  state,
  undoFileEdits,
  undoLineEdits,
} from "./state/review";

/** "Go back" as editors have it: Ctrl+- on macOS, Alt+Left elsewhere. */
const isBackShortcut = (event: KeyboardEvent) =>
  isMac
    ? event.ctrlKey && !event.metaKey && event.key === "-"
    : event.altKey && !event.ctrlKey && event.key === "ArrowLeft";

/** Hides or shows the files dock: Ctrl+B, and on macOS Cmd+B too, as in VS Code. */
const isDockShortcut = (event: KeyboardEvent) =>
  (event.ctrlKey || (isMac && event.metaKey)) &&
  !(event.ctrlKey && event.metaKey) &&
  !event.altKey &&
  !event.shiftKey &&
  event.key.toLowerCase() === "b";

/**
 * The webview's own context menu (Reload, Inspect, Back...) is not the app's.
 * Text fields keep it, for cut, copy and paste.
 */
function suppressBrowserMenu(event: MouseEvent) {
  const target = event.target;
  const textField =
    (target instanceof HTMLInputElement && !["checkbox", "radio", "button", "submit"].includes(target.type)) ||
    target instanceof HTMLTextAreaElement;
  if (!textField) event.preventDefault();
}

const MIN_DOCK = 160;
const MAX_DOCK = 640;
/** The commit message pane: room for a line or two and the Commit button. */
const MIN_COMMIT_HEIGHT = 96;
/** Room the diff keeps when the commit message pane is made taller. */
const MIN_DIFF_HEIGHT = 120;

/** Reviewing pull requests, and none is picked yet: the main area lists them instead. */
const choosingPullRequest = () => state.repo !== null && state.source === "pullRequest" && state.pullRequest === null;

export function App() {
  const [dockWidth, setDockWidth] = createSignal(Number(localStorage.getItem("arnold.dockWidth")) || 280);
  const [dockHidden, setDockHidden] = createSignal(localStorage.getItem("arnold.dockHidden") === "true");
  const toggleDock = () => {
    setDockHidden(!dockHidden());
    localStorage.setItem("arnold.dockHidden", String(dockHidden()));
  };
  let editorHost!: HTMLDivElement;
  let contentColumn!: HTMLDivElement;
  const [commitHeight, setCommitHeight] = createSignal(Number(localStorage.getItem("arnold.commitHeight")) || 130);

  const startCommitResize = (down: PointerEvent) => {
    const startY = down.clientY;
    const startHeight = commitHeight();
    // The diff keeps some room.
    const most = Math.max(contentColumn.clientHeight - MIN_DIFF_HEIGHT, MIN_COMMIT_HEIGHT);
    const onMove = (move: PointerEvent) =>
      setCommitHeight(Math.min(Math.max(startHeight - (move.clientY - startY), MIN_COMMIT_HEIGHT), most));
    const onUp = () => {
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", onUp);
      localStorage.setItem("arnold.commitHeight", String(commitHeight()));
    };
    window.addEventListener("pointermove", onMove);
    window.addEventListener("pointerup", onUp);
    down.preventDefault();
  };

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
    } else if (isDockShortcut(event)) {
      event.preventDefault();
      event.stopPropagation();
      toggleDock();
    }
  };
  const closeMenu = () => setState("menu", null);
  // Capture phase: the editor would otherwise use these shortcuts itself (Ctrl+B moves the cursor on macOS).
  window.addEventListener("keydown", onKeyDown, true);
  window.addEventListener("pointerdown", closeMenu);
  window.addEventListener("contextmenu", suppressBrowserMenu);
  onCleanup(() => {
    window.removeEventListener("contextmenu", suppressBrowserMenu);
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
          {/* Hidden rather than removed, so the explorer keeps its open folders and scroll position. */}
          <Show when={state.source === "commit"} fallback={<FileDock width={dockWidth()} hidden={dockHidden()} />}>
            <WorkingDock width={dockWidth()} hidden={dockHidden()} />
          </Show>
          <div class="splitter" classList={{ hidden: dockHidden() }} onPointerDown={startResize} />
        </Show>
        <div class="content-column" classList={{ hidden: choosingPullRequest() }} ref={contentColumn}>
        <section class="content">
          <div
            class="editor-host diff-host"
            classList={{ hidden: state.view === "all" || state.placeholder !== null }}
            ref={editorHost}
          />
          <Show when={state.view === "file" && state.placeholder}>
            <div class="placeholder">{state.placeholder}</div>
          </Show>
          <Show when={state.view === "all" && state.source !== "commit"}>
            <AllFilesView />
          </Show>
          <Show when={state.view === "all" && state.source === "commit"}>
            <WorkingFilesView />
          </Show>
        </section>
        {/* Commit mode: the message under the diff, in an island of its own; the gap between them resizes it. */}
        <Show when={state.source === "commit" && state.checkout}>
          <div class="content-sash" onPointerDown={startCommitResize} />
          <section class="commit-island" style={{ height: `${commitHeight()}px` }}>
            <CommitBox />
          </section>
        </Show>
        </div>
      </div>
      <StatusBar />
      <Show when={prs.tokenDialog}>
        <TokenDialog />
      </Show>
      <Show when={state.unformatted}>
        <UnformattedDialog />
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
            <Show when={!menu().file}>
              <li classList={{ disabled: !menu().definition }} onClick={() => menu().definition && goToDefinition()}>
                <span>Go to definition</span>
                <kbd>F12</kbd>
              </li>
            </Show>
            <Show when={menu().undo}>
              {(undo) => (
                <>
                  <Show when={!menu().file}>
                    <li class="menu-separator" />
                    <li
                      classList={{ disabled: !undo().lines }}
                      title={undo().lines ? undefined : "The selected lines have no uncommitted edits"}
                      onClick={() => undo().lines && undoLineEdits()}
                    >
                      <span>Undo edits in selected lines</span>
                    </li>
                  </Show>
                  <li
                    classList={{ disabled: !undo().file }}
                    title={undo().file ? undefined : "The file has no uncommitted edits"}
                    onClick={() => undo().file && void undoFileEdits(undo().path)}
                  >
                    <span>{menu().file ? "Undo edits to file…" : "Undo all edits to file"}</span>
                  </li>
                </>
              )}
            </Show>
            <Show when={menu().lines?.length}>
              <Show when={!menu().file}>
                <li class="menu-separator" />
              </Show>
              <For each={menu().lines}>
                {(action) => (
                  <li onClick={() => runMenuAction(action)}>
                    <span>{actionLabel(action, !!menu().file)}</span>
                    <kbd>{menu().editable ? `${isMac ? "⌥" : "Alt+"}${LINE_ACTION_KEYS[action]}` : LINE_ACTION_KEYS[action]}</kbd>
                  </li>
                )}
              </For>
            </Show>
            <Show when={menu().comment !== undefined}>
              <li
                classList={{ disabled: !menu().comment }}
                onClick={() => menu().comment && addComment()}
                title={menu().comment ? undefined : "Comments go on the pull request's files, with all of its changes shown"}
              >
                <span>{menu().file ? "Add comment on file" : "Add comment"}</span>
                <Show when={!menu().file}>
                  <kbd>{menu().comment ?? ""}</kbd>
                </Show>
              </li>
            </Show>
          </ul>
        )}
      </Show>
    </div>
  );
}

const ACTION_LABELS: Record<LineAction, string> = { stage: "Stage", unstage: "Unstage", reset: "Reset" };

/**
 * "Stage selected lines" in an editor's menu, "Stage file" in a list's. A
 * reset asks first ("…"), except of unstaged lines: that is an edit, undone like any other.
 */
const actionLabel = (action: LineAction, file: boolean) =>
  `${ACTION_LABELS[action]} ${file ? "file" : "selected lines"}${action === "reset" && (file || state.workSide === "staged") ? "…" : ""}`;

/** A line or file action of a context menu. */
function runMenuAction(action: LineAction) {
  const menu = state.menu;
  setState("menu", null);
  if (!menu) return;
  if (menu.file && menu.side) {
    const file = listOf(menu.side).find((candidate) => candidate.path === menu.file);
    if (file) void fileAction(action, menu.side, file);
  } else {
    void lineAction(action);
  }
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
      <Show when={state.source !== "commit"} fallback={<CommitStatus />}>
      <Show when={state.comparison} fallback={<span>{state.busy ? "Loading…" : ""}</span>}>
        {(comparison) => (
          <>
            <span>
              {comparison().files.length} files <span class="added">+{totals().added}</span>{" "}
              <span class="removed">−{totals().removed}</span>
            </span>
            <Show when={state.source === "branches"}>
              <EditStatus />
            </Show>
            <Show when={state.source === "pullRequest" && state.pullRequest}>
              {(pullRequest) => (
                <span title={pullRequest().url}>
                  Pull request !{pullRequest().id} by {pullRequest().author}
                </span>
              )}
            </Show>
            <Show
              when={comparison().checkout}
              fallback={
                <span title={`Commits only: ${comparison().mergeBase} to ${comparison().headCommit}`}>
                  from {comparison().mergeBase.slice(0, 8)} to {comparison().headCommit.slice(0, 8)}
                </span>
              }
            >
              {(checkout) => (
                <span class="status-path" title={`From ${comparison().mergeBase} to the files of ${checkout()}, uncommitted edits included`}>
                  from {comparison().mergeBase.slice(0, 8)} to {checkout()}
                </span>
              )}
            </Show>
            <span class="toolbar-space" />
            <span class="hint">F12 go to definition · F7 next change · Ctrl+B files</span>
            <LanguageStatus />
          </>
        )}
      </Show>
      </Show>
    </footer>
  );
}

/** The status bar in Commit mode: the checkout, how much is uncommitted, what the keys do. */
function CommitStatus() {
  const hint = () => {
    if (state.workSide === null) return "Against the last commit · stage and reset in Unstaged and Staged · F7 next change";
    if (state.workSide === "staged") return "U unstage · R reset lines · S/U/R in the lists · F7 next change";
    return `S stage · R reset selected lines (${isMac ? "⌥" : "Alt+"}S/R: the cursor's line) · F7 next change`;
  };
  return (
    <Show when={state.work} fallback={<span>{state.checkout ? "Reading the uncommitted changes…" : ""}</span>}>
      {(work) => (
        <>
          <span>
            {work().unstaged.length} unstaged · {work().staged.length} staged
          </span>
          <span class="status-path" title="The checkout whose changes are listed">
            {state.checkout}
          </span>
          <span class="toolbar-space" />
          <Show
            when={state.notice}
            fallback={<span class="hint">{hint()}</span>}
          >
            <span class="notice">{state.notice}</span>
          </Show>
          <LanguageStatus />
        </>
      )}
    </Show>
  );
}
