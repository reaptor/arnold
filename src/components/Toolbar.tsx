import { isTauri } from "@tauri-apps/api/core";
import { getCurrentWindow } from "@tauri-apps/api/window";
import { createSignal, onCleanup, Show } from "solid-js";
import {
  goBack,
  nextChange,
  previousChange,
  refresh,
  setBranches,
  setMode,
  setView,
  state,
} from "../state/review";
import { isMac, platform } from "../platform";
import { BranchPicker } from "./BranchPicker";
import {
  ArrowDownIcon,
  ArrowUpIcon,
  BackIcon,
  FileIcon,
  FilesIcon,
  RefreshIcon,
  SplitIcon,
  UnifiedIcon,
} from "./icons";
import { RepoPicker } from "./RepoPicker";
import { WindowControls } from "./WindowControls";

/** The main toolbar, which is also the window's title bar as in Rider: macOS
    draws its window buttons over the left end, on Windows the app draws its
    own at the right end. Dragging empty space moves the window, double-click
    maximizes. */
export function Toolbar() {
  // The browser preview has no window buttons to make room for, and in full
  // screen macOS hides them.
  const trafficLights = isMac && isTauri();
  const [fullscreen, setFullscreen] = createSignal(false);
  if (trafficLights) {
    const window = getCurrentWindow();
    const update = () => void window.isFullscreen().then(setFullscreen);
    update();
    const stop = window.onResized(update);
    onCleanup(() => void stop.then((unlisten) => unlisten()));
  }

  return (
    <header
      class="toolbar"
      classList={{ "traffic-lights": trafficLights && !fullscreen(), "window-buttons": platform === "windows" }}
      data-tauri-drag-region="deep"
    >
      <RepoPicker />
      <Show when={state.repo}>
        {(repo) => (
          <>
            <BranchPicker
              label="Branch to review"
              value={state.head}
              branches={repo().branches}
              onChange={(head) => setBranches({ head })}
            />
            <span class="toolbar-word">against</span>
            <BranchPicker
              label="Base branch"
              value={state.base}
              branches={repo().branches}
              onChange={(base) => setBranches({ base })}
            />
          </>
        )}
      </Show>
      <span class="toolbar-space" />
      <Show when={state.repo}>
        <Show when={state.backCount > 0}>
          <button class="icon-button" onClick={() => goBack()} title="Back to where you jumped from">
            <BackIcon />
          </button>
        </Show>
        <div class="segmented" role="group" aria-label="Files shown">
          <button
            classList={{ on: state.view === "file" }}
            onClick={() => setView("file")}
            title="One file: show the file selected in the dock"
          >
            <FileIcon />
            <span class="segmented-label">One file</span>
          </button>
          <button
            classList={{ on: state.view === "all" }}
            onClick={() => setView("all")}
            title="All files: show every file of the diff in one list"
          >
            <FilesIcon />
            <span class="segmented-label">All files</span>
          </button>
        </div>
        <div class="segmented" role="group" aria-label="Diff layout">
          <button
            classList={{ on: state.mode === "split" }}
            onClick={() => setMode("split")}
            title="Side-by-side diff"
          >
            <SplitIcon />
          </button>
          <button
            classList={{ on: state.mode === "unified" }}
            onClick={() => setMode("unified")}
            title="Unified diff"
          >
            <UnifiedIcon />
          </button>
        </div>
        <span class="toolbar-separator" />
        <button class="icon-button" onClick={previousChange} title="Previous change (Shift+F7)">
          <ArrowUpIcon />
        </button>
        <button class="icon-button" onClick={nextChange} title="Next change (F7)">
          <ArrowDownIcon />
        </button>
        <button class="icon-button" onClick={() => refresh()} title="Refresh the comparison">
          <RefreshIcon />
        </button>
      </Show>
      <Show when={platform === "windows"}>
        <WindowControls />
      </Show>
    </header>
  );
}
