import { Show } from "solid-js";
import {
  nextChange,
  overviewShown,
  previousChange,
  setIgnoreWhitespace,
  setMode,
  setView,
  showOverview,
  state,
} from "../state/review";
import {
  ArrowDownIcon,
  ArrowUpIcon,
  FileIcon,
  FilesIcon,
  OverviewIcon,
  SplitIcon,
  UnifiedIcon,
  WhitespaceIcon,
} from "./icons";

/** How the diff is shown, on the frame over it (as the dock's filter is over the dock):
    which files, which layout, whitespace, and stepping from change to change. The
    toolbar above keeps what is reviewed and what to do with it. */
export function ViewBar() {
  return (
    <div class="view-bar" role="toolbar" aria-label="Diff view">
      <div class="segmented" role="group" aria-label="Files shown">
        <Show when={state.source === "pullRequest"}>
          <button
            classList={{ on: overviewShown() }}
            onClick={() => showOverview(true)}
            title="Overview: the pull request's description, status and every discussion"
            aria-label="Overview"
          >
            <OverviewIcon />
            <span class="view-label">Overview</span>
          </button>
        </Show>
        <button
          classList={{ on: state.view === "file" && !overviewShown() }}
          onClick={() => {
            showOverview(false);
            void setView("file");
          }}
          title="One file: show the file selected in the dock"
          aria-label="One file"
        >
          <FileIcon />
          <span class="view-label">One file</span>
        </button>
        <button
          classList={{ on: state.view === "all" && !overviewShown() }}
          onClick={() => {
            showOverview(false);
            void setView("all");
          }}
          title={
            state.source === "commit"
              ? "All files: every uncommitted file in one list, Unstaged then Staged"
              : "All files: show every file of the diff in one list"
          }
          aria-label="All files"
        >
          <FilesIcon />
          <span class="view-label">All files</span>
        </button>
      </div>
      <span class="toolbar-space" />
      <div class="segmented" role="group" aria-label="Diff layout">
        <button
          classList={{ on: state.mode === "split" }}
          onClick={() => setMode("split")}
          title="Side-by-side diff"
          aria-label="Side by side"
        >
          <SplitIcon />
          <span class="view-label">Side by side</span>
        </button>
        <button
          classList={{ on: state.mode === "unified" }}
          onClick={() => setMode("unified")}
          title="Unified diff"
          aria-label="Unified"
        >
          <UnifiedIcon />
          <span class="view-label">Unified</span>
        </button>
      </div>
      <button
        class="whitespace-toggle"
        classList={{ on: !state.ignoreWhitespace }}
        onClick={() => setIgnoreWhitespace(!state.ignoreWhitespace)}
        title={
          state.ignoreWhitespace
            ? "Whitespace: changes in whitespace are left out of the diff (click to show them)"
            : "Whitespace: changes in whitespace are shown (click to leave them out)"
        }
        aria-label="Show whitespace changes"
        aria-pressed={!state.ignoreWhitespace}
      >
        <WhitespaceIcon />
        <span class="view-label">Whitespace</span>
      </button>
      <span class="toolbar-separator" />
      <div class="change-steps" role="group" aria-label="Changes">
        <span class="view-label">Changes</span>
        <button onClick={previousChange} title="Previous change (Shift+F7)" aria-label="Previous change">
          <ArrowUpIcon />
        </button>
        <button onClick={nextChange} title="Next change (F7)" aria-label="Next change">
          <ArrowDownIcon />
        </button>
      </div>
    </div>
  );
}
