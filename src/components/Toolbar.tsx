import { isTauri } from "@tauri-apps/api/core";
import { getCurrentWindow } from "@tauri-apps/api/window";
import { createSignal, onCleanup, Show } from "solid-js";
import { editsToCommit, goBack, goForward, headBranch, setBranches, setRange, setSource, state } from "../state/review";
import { prs } from "../state/pullRequests";
import { attentionCount, dashboard, showDashboard } from "../state/dashboard";
import { favorites } from "../state/favorites";
import { commitOnBranch, uncommittedCount } from "../state/commitMode";
import { checkoutChoices } from "../state/branchInfo";
import { isMac, platform } from "../platform";
import { BranchActions, PushHeadButton } from "./BranchActions";
import { BranchPicker, unpushedMark } from "./BranchPicker";
import { CommitPicker } from "./CommitPicker";
import { CreatePullRequestButton } from "./CreatePullRequest";
import { DiscussionPicker } from "./DiscussionPicker";
import { PullRequestActions } from "./PullRequestActions";
import { PullRequestPicker } from "./PullRequestPicker";
import { BranchIcon, BackIcon, ForwardIcon, CommitIcon, DashboardIcon, PullRequestIcon, SparkleIcon } from "./icons";
import { ai, toggleOpen as toggleAi } from "../state/ai";
import { RepoPicker } from "./RepoPicker";
import { WindowControls } from "./WindowControls";

/** The open repository, unless the dashboard covers it: what the repository's widgets are for. */
const reviewed = () => (dashboard.open ? null : state.repo);

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
      <Show when={favorites().length > 0 || dashboard.open}>
        <button
          class="dashboard-toggle"
          classList={{ on: dashboard.open }}
          aria-pressed={dashboard.open}
          onClick={() => showDashboard(!dashboard.open)}
          aria-label="Favorites' pull requests"
          title={`Favorites' pull requests: the active pull requests of every favorite (★) repository, in one list${
            attentionCount() > 0 ? `\n${attentionCount()} waiting on you` : ""
          }`}
        >
          <DashboardIcon />
          <span class="dashboard-label">Favorites' PRs</span>
          <Show when={attentionCount() > 0}>
            <span class="mode-count" aria-label={`${attentionCount()} waiting on you`}>
              {attentionCount()}
            </span>
          </Show>
        </button>
      </Show>
      <Show when={reviewed() && (state.source !== "pullRequest" || state.pullRequest)}>
        <div class="nav-buttons" role="group" aria-label="Navigation">
          <button
            class="icon-button"
            disabled={state.backCount === 0}
            onClick={() => goBack()}
            title={`Back to where you were (${isMac ? "⌃-" : "Alt+←"}, mouse back button)`}
            aria-label="Back"
          >
            <BackIcon />
          </button>
          <button
            class="icon-button"
            disabled={state.forwardCount === 0}
            onClick={() => goForward()}
            title={`Forward again (${isMac ? "⌃⇧-" : "Alt+→"}, mouse forward button)`}
            aria-label="Forward"
          >
            <ForwardIcon />
          </button>
        </div>
      </Show>
      <Show when={reviewed()}>
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
                branches={checkoutChoices(repo().branches)}
                onChange={(name) => void commitOnBranch(name)}
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
                edits={editsToCommit() ? (state.comparison?.uncommitted ?? 0) : 0}
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
      <Show when={reviewed() && state.source === "pullRequest" && state.pullRequest}>
        <PullRequestActions />
        <span class="toolbar-separator" />
      </Show>
      <Show when={reviewed() && state.source === "commit" && state.work}>
        <BranchActions />
        <span class="toolbar-separator" />
      </Show>
      <Show when={reviewed() && state.source === "branches" && headBranch()?.sync}>
        <PushHeadButton />
        <Show when={prs.remotes.length === 0}>
          <span class="toolbar-separator" />
        </Show>
      </Show>
      <Show when={reviewed() && state.source === "branches" && prs.remotes.length > 0}>
        <CreatePullRequestButton />
        <span class="toolbar-separator" />
      </Show>
      <Show when={reviewed()}>
        <button
          class="icon-button ai-toggle"
          classList={{ on: ai.open }}
          onClick={toggleAi}
          title={`AI: explain code with GitHub Copilot (${isMac ? "⌘⇧A" : "Ctrl+Shift+A"})`}
          aria-label="AI"
          aria-pressed={ai.open}
        >
          <SparkleIcon />
        </button>
      </Show>
      <Show when={platform === "windows"}>
        <WindowControls />
      </Show>
    </header>
  );
}
