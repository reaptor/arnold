import { For, Match, Show, Switch } from "solid-js";
import {
  closeTokenDialog,
  currentRemote,
  loadPullRequests,
  needsToken,
  openTokenDialog,
  prErrorText,
  prs,
  selectRemote,
} from "../state/pullRequests";
import { openPullRequest, setSource, state } from "../state/review";
import { DevOpsSetup, openInBrowser } from "./DevOpsSetup";
import { ExternalIcon, KeyIcon, PullRequestIcon } from "./icons";
import { PullRequestList } from "./PullRequestList";

const useBranches = () => void setSource("branches");

/** Picks which of the repository's remotes the pull requests come from, when there are several. */
export function RemoteSelect() {
  return (
    <Show when={prs.remotes.length > 1}>
      <select
        class="pr-remote"
        value={prs.remote ?? ""}
        onChange={(event) => void selectRemote(event.currentTarget.value)}
        title="Remote the pull requests come from"
      >
        <For each={prs.remotes}>
          {(remote) => (
            <option value={remote.remote}>
              {remote.remote}: {remote.name}
            </option>
          )}
        </For>
      </select>
    </Show>
  );
}

/**
 * The main area while reviewing pull requests and none is picked yet: the
 * active ones to pick from, or how to connect to Azure DevOps first.
 */
export function PullRequestPanel() {
  return (
    <div class="pr-panel">
      <div class="pr-card" data-tauri-drag-region="false">
        <Switch>
          <Match when={!currentRemote()}>
            <p class="pr-message">Looking for Azure DevOps remotes…</p>
          </Match>
          <Match when={needsToken()}>
            <DevOpsSetup remote={currentRemote()!} onUseBranches={useBranches} />
          </Match>
          <Match when={state.openingPullRequest !== null && prs.list === null}>
            <p class="pr-message">Opening pull request !{state.openingPullRequest}…</p>
          </Match>
          <Match when={prs.error}>
            {(error) => (
              <>
                <h2>
                  <PullRequestIcon />
                  Could not read the pull requests
                </h2>
                <RemoteSelect />
                <p class="connect-problem">{prErrorText(error())}</p>
                <div class="connect-actions">
                  <button class="primary" onClick={() => void loadPullRequests()} disabled={prs.loading}>
                    {prs.loading ? "Trying…" : "Try again"}
                  </button>
                  <button onClick={openTokenDialog}>
                    <KeyIcon />
                    Change token
                  </button>
                  <span class="toolbar-space" />
                  <button onClick={useBranches}>Compare branches instead</button>
                </div>
              </>
            )}
          </Match>
          <Match when={prs.list === null}>
            <p class="pr-message">Reading pull requests of {currentRemote()!.name}…</p>
          </Match>
          <Match when={prs.list!.length === 0}>
            <h2>
              <PullRequestIcon />
              No active pull requests
            </h2>
            <RemoteSelect />
            <p>
              <strong>{currentRemote()!.name}</strong> has no active pull requests right now.
            </p>
            <div class="connect-actions">
              <button onClick={() => void loadPullRequests()} disabled={prs.loading}>
                {prs.loading ? "Reading…" : "Check again"}
              </button>
              <button onClick={() => openInBrowser(`${currentRemote()!.webUrl}/pullrequests`)}>
                Open in Azure DevOps
                <ExternalIcon />
              </button>
              <span class="toolbar-space" />
              <button onClick={useBranches}>Compare branches instead</button>
            </div>
          </Match>
          <Match when={prs.list}>
            {(list) => (
              <>
                <h2>
                  <PullRequestIcon />
                  Pick a pull request to review
                </h2>
                <p class="pr-subtitle">
                  {list().length} active in <strong>{currentRemote()!.name}</strong>. Arnold shows what the pull request
                  changes, as Azure DevOps does.
                </p>
                <RemoteSelect />
                <PullRequestList
                  pullRequests={list()}
                  current={state.pullRequest?.id ?? null}
                  opening={state.openingPullRequest}
                  onPick={(pullRequest) => void openPullRequest(prs.remote!, pullRequest.id)}
                  roomy
                />
                <div class="connect-actions">
                  <button onClick={openTokenDialog}>
                    <KeyIcon />
                    Token
                  </button>
                  <button onClick={() => void loadPullRequests()} disabled={prs.loading}>
                    {prs.loading ? "Reading…" : "Reload"}
                  </button>
                  <span class="toolbar-space" />
                  <button onClick={useBranches}>Compare branches instead</button>
                </div>
              </>
            )}
          </Match>
        </Switch>
      </div>
    </div>
  );
}

/** Changing or removing the token, over whatever is shown. */
export function TokenDialog() {
  return (
    <Show when={currentRemote()}>
      {(remote) => (
        <div
          class="modal-backdrop"
          data-tauri-drag-region="false"
          onPointerDown={(event) => event.target === event.currentTarget && closeTokenDialog()}
          onKeyDown={(event) => event.key === "Escape" && closeTokenDialog()}
        >
          <div class="modal" role="dialog" aria-label="Azure DevOps token">
            <DevOpsSetup remote={remote()} changing={remote().token !== null} onCancel={closeTokenDialog} />
          </div>
        </div>
      )}
    </Show>
  );
}
