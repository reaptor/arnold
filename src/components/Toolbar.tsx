import { isTauri } from "@tauri-apps/api/core";
import { getCurrentWindow } from "@tauri-apps/api/window";
import { createSignal, onCleanup, Show } from "solid-js";
import {
  goBack,
  headBranch,
  nextChange,
  previousChange,
  setBranches,
  setRange,
  setMode,
  setSource,
  setView,
  state,
} from "../state/review";
import { prs } from "../state/pullRequests";
import { setCheckout, uncommittedCount } from "../state/commitMode";
import { isMac, platform } from "../platform";
import { BranchActions, PushHeadButton } from "./BranchActions";
import { BranchPicker, unpushedMark } from "./BranchPicker";
import { CommitPicker } from "./CommitPicker";
import { CreatePullRequestButton } from "./CreatePullRequest";
import { DiscussionPicker } from "./DiscussionPicker";
import { PullRequestActions } from "./PullRequestActions";
import { PullRequestPicker } from "./PullRequestPicker";
import {
  ArrowDownIcon,
  BranchIcon,
  ArrowUpIcon,
  BackIcon,
  CommitIcon,
  FileIcon,
  FilesIcon,
  PullRequestIcon,
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
            <div class="segmented source-switch" role="group" aria-label="What to review">
              <button
                classList={{ on: state.source === "commit" }}
                onClick={() => void setSource("commit")}
                title={`Commit: stage, edit and commit the uncommitted changes of a checkout, the first step with an agent's work; then push the branch, review it or create its pull request${
                  uncommittedCount() > 0 ? `\n${uncommittedCount()} file${uncommittedCount() === 1 ? "" : "s"} with uncommitted changes` : ""
                }`}
              >
                <CommitIcon />
                <span>Commit</span>
                <Show when={uncommittedCount() > 0}>
                  <span class="mode-count" aria-label={`${uncommittedCount()} uncommitted`}>
                    {uncommittedCount()}
                  </span>
                </Show>
              </button>
              <button
                classList={{ on: state.source === "branches" }}
                onClick={() => void setSource("branches")}
                title="Compare a branch against a base branch, then create its pull request"
              >
                <BranchIcon />
                <span>Branches</span>
              </button>
              <Show when={prs.remotes.length > 0}>
                <button
                  classList={{ on: state.source === "pullRequest" }}
                  onClick={() => void setSource("pullRequest")}
                  title="Review an active pull request from Azure DevOps: discussions, comments, votes and completing"
                >
                  <PullRequestIcon />
                  <span>Pull request</span>
                </button>
              </Show>
            </div>
            <Show when={state.source === "commit"}>
              <BranchPicker
                label="Checkout to commit in"
                value={state.work?.branch ?? (state.work ? "detached HEAD" : "")}
                branches={repo().branches.filter((branch) => !branch.isRemote && branch.worktreePath)}
                onChange={(name) => {
                  const path = repo().branches.find((branch) => branch.name === name && !branch.isRemote)?.worktreePath;
                  if (path) void setCheckout(path);
                }}
              />
            </Show>
            <Show when={state.source === "pullRequest"}>
              <PullRequestPicker />
            </Show>
            <Show when={state.source === "branches"}>
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
                mark={unpushedMark(repo().branches, state.base)}
              />
            </Show>
            <Show when={state.source === "branches" || (state.source === "pullRequest" && state.pullRequest)}>
              <span class="toolbar-separator" />
              <CommitPicker
                commits={state.comparison?.commits ?? []}
                value={state.range}
                edits={state.source === "branches" ? (state.comparison?.uncommitted ?? 0) : 0}
                onChange={(range) => void setRange(range)}
              />
            </Show>
            <Show when={state.source === "pullRequest" && state.pullRequest}>
              <DiscussionPicker />
            </Show>
          </>
        )}
      </Show>
      <span class="toolbar-space" />
      <Show when={state.repo && state.source === "pullRequest" && state.pullRequest}>
        <PullRequestActions />
        <span class="toolbar-separator" />
      </Show>
      <Show when={state.repo && state.source === "commit" && state.work}>
        <BranchActions />
        <span class="toolbar-separator" />
      </Show>
      <Show when={state.repo && state.source === "branches" && headBranch()?.sync}>
        <PushHeadButton />
        <Show when={prs.remotes.length === 0}>
          <span class="toolbar-separator" />
        </Show>
      </Show>
      <Show when={state.repo && state.source === "branches" && prs.remotes.length > 0}>
        <CreatePullRequestButton />
        <span class="toolbar-separator" />
      </Show>
      <Show when={state.repo && (state.source !== "pullRequest" || state.pullRequest)}>
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
            aria-label="One file"
          >
            <FileIcon />
          </button>
          <button
            classList={{ on: state.view === "all" }}
            onClick={() => setView("all")}
            title={
              state.source === "commit"
                ? "All files: every uncommitted file in one list, Unstaged then Staged"
                : "All files: show every file of the diff in one list"
            }
            aria-label="All files"
          >
            <FilesIcon />
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
      </Show>
      <Show when={platform === "windows"}>
        <WindowControls />
      </Show>
    </header>
  );
}
