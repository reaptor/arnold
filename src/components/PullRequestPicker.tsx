import { createSignal, Match, onCleanup, Show, Switch } from "solid-js";
import {
  currentRemote,
  loadPullRequests,
  needsToken,
  openTokenDialog,
  prErrorText,
  prs,
} from "../state/pullRequests";
import { closePullRequest, openPullRequest, state } from "../state/review";
import { openInBrowser } from "./DevOpsSetup";
import { ChevronDownIcon, ExternalIcon, KeyIcon, PullRequestIcon } from "./icons";
import { RemoteSelect } from "./PullRequestPanel";
import { PullRequestList } from "./PullRequestList";

/** Toolbar button showing the pull request under review; opens the list of active ones. */
export function PullRequestPicker() {
  const [open, setOpen] = createSignal(false);
  let root!: HTMLDivElement;
  let popup!: HTMLDivElement;

  const close = () => setOpen(false);
  const toggle = () => {
    setOpen(!open());
    if (!open()) return;
    // Shows what there is at once and brings it up to date meanwhile.
    void loadPullRequests();
    queueMicrotask(() => {
      const overflow = popup.getBoundingClientRect().right - (document.documentElement.clientWidth - 8);
      if (overflow > 0) popup.style.left = `${-overflow}px`;
    });
  };

  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) close();
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  const label = () => {
    const pullRequest = state.pullRequest;
    if (state.openingPullRequest !== null) return `Opening !${state.openingPullRequest}…`;
    return pullRequest ? pullRequest.title : "Choose a pull request";
  };
  const title = () => {
    const pullRequest = state.pullRequest;
    if (!pullRequest) return "Pick an active pull request to review";
    return `Pull request !${pullRequest.id}: ${pullRequest.title}\n${pullRequest.sourceBranch} into ${pullRequest.targetBranch}, by ${pullRequest.author}`;
  };

  return (
    <div class="picker pr-picker" ref={root}>
      <button class="picker-button" onClick={toggle} title={title()}>
        <PullRequestIcon />
        <Show when={state.pullRequest && state.openingPullRequest === null}>
          <span class="pr-id">!{state.pullRequest!.id}</span>
        </Show>
        <span class="picker-value">{label()}</span>
        <ChevronDownIcon />
      </button>
      <Show when={state.openingPullRequest === null && state.pullRequest}>
        {(pullRequest) => (
          <span class="toolbar-word pr-into" title={`${pullRequest().sourceBranch} into ${pullRequest().targetBranch}`}>
            {pullRequest().sourceBranch} → {pullRequest().targetBranch}
          </span>
        )}
      </Show>
      <Show when={open()}>
        <div ref={popup} class="picker-popup pr-popup" data-tauri-drag-region="false">
          <RemoteSelect />
          <Switch>
            <Match when={needsToken()}>
              <p class="pr-message">Arnold needs an Azure DevOps personal access token to list pull requests.</p>
              <button
                class="primary"
                onClick={() => {
                  close();
                  openTokenDialog();
                }}
              >
                Set up a token…
              </button>
            </Match>
            <Match when={prs.error && !prs.list}>
              <p class="connect-problem">{prErrorText(prs.error)}</p>
              <button onClick={() => void loadPullRequests()} disabled={prs.loading}>
                {prs.loading ? "Trying…" : "Try again"}
              </button>
            </Match>
            <Match when={prs.list === null}>
              <p class="pr-message">Reading pull requests…</p>
            </Match>
            <Match when={prs.list}>
              {(list) => (
                <PullRequestList
                  pullRequests={list()}
                  current={state.pullRequest?.id ?? null}
                  opening={state.openingPullRequest}
                  onEscape={close}
                  onPick={(pullRequest) => {
                    close();
                    if (pullRequest.id !== state.pullRequest?.id || state.pullRequest.remote !== prs.remote) {
                      void openPullRequest(prs.remote!, pullRequest.id);
                    }
                  }}
                />
              )}
            </Match>
          </Switch>
          <div class="pr-popup-footer">
            <Show when={state.pullRequest}>
              {(pullRequest) => (
                <button onClick={() => openInBrowser(pullRequest().url)} title={pullRequest().url}>
                  Open !{pullRequest().id} in Azure DevOps
                  <ExternalIcon />
                </button>
              )}
            </Show>
            <span class="toolbar-space" />
            <Show when={state.pullRequest}>
              <button
                onClick={() => {
                  close();
                  closePullRequest();
                }}
                title="Back to the list of pull requests"
              >
                All pull requests
              </button>
            </Show>
            <Show when={currentRemote()?.token}>
              <button
                onClick={() => {
                  close();
                  openTokenDialog();
                }}
                title="Change or remove the Azure DevOps token"
              >
                <KeyIcon />
              </button>
            </Show>
          </div>
        </div>
      </Show>
    </div>
  );
}
