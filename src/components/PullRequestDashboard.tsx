import { createMemo, createSignal, For, Match, onCleanup, Show, Switch } from "solid-js";
import type { PullRequest } from "../ipc/commands";
import {
  attentionCount,
  dashboard,
  loadDashboard,
  openFromDashboard,
  openingKey,
  type RepoPullRequests,
  setAttentionOnly,
  setHiddenCreators,
  setHideDrafts,
  showDashboard,
  shownOf,
} from "../state/dashboard";
import { favorites, folderName } from "../state/favorites";
import { attention, attentionFirst, type Creator, creatorsOf, prStatus } from "../state/prStatus";
import { prErrorText } from "../state/pullRequests";
import { openRepoAt, setSource, state } from "../state/review";
import { ago } from "./CommitPicker";
import { openInBrowser } from "./DevOpsSetup";
import { ChevronDownIcon, ExternalIcon, PullRequestIcon, RefreshIcon } from "./icons";
import { votes } from "./PullRequestList";

/** Opens a favorite in Pull request mode, where its token is set up. */
async function setUpToken(root: string) {
  showDashboard(false);
  if (state.repo?.root !== root) await openRepoAt(root);
  if (state.repo?.root === root) await setSource("pullRequest");
}

/**
 * The main area in place of the open repository: the active pull requests of
 * every favorite repository, where each stands and which wait on the user.
 * A click reviews one, as picking it in its repository's list does. Only the
 * repositories with pull requests to show (or a problem) get a section; the
 * others are named at the bottom.
 */
export function PullRequestDashboard() {
  const all = () => dashboard.repos.flatMap((repo) => repo.list ?? []);
  const active = () => dashboard.repos.filter((repo) => (repo.list?.length ?? 0) > 0);
  const listed = () => dashboard.repos.filter((repo) => repo.error !== null || shownOf(repo).length > 0);
  const withoutRemote = () => dashboard.repos.filter((repo) => repo.remote === null && repo.error === null);
  const noneActive = () => dashboard.repos.filter((repo) => repo.remote !== null && repo.list?.length === 0);
  const filteredOut = () =>
    dashboard.repos.filter((repo) => repo.error === null && (repo.list?.length ?? 0) > 0 && shownOf(repo).length === 0);
  const names = (repos: RepoPullRequests[]) => repos.map((repo) => folderName(repo.root)).join(", ");

  return (
    <div class="pr-panel dashboard" data-tauri-drag-region="false">
      <div class="pr-card dashboard-card">
        <h2>
          <PullRequestIcon />
          Pull requests in your favorites
        </h2>
        <Switch>
          <Match when={favorites().length === 0}>
            <p class="pr-message">
              Star repositories with the ☆ in the repository menu: the dashboard lists the active pull requests of all
              of them.
            </p>
          </Match>
          <Match when={dashboard.repos.length === 0}>
            <p class="pr-message">
              Reading the pull requests of {favorites().length} {favorites().length === 1 ? "repository" : "repositories"}…
            </p>
          </Match>
          <Match when={true}>
            <p class="pr-subtitle">
              {all().length} active in {active().length} {active().length === 1 ? "repository" : "repositories"}
              <Show when={attentionCount() > 0}>
                {" · "}
                <span class="dashboard-attention-count">{attentionCount()} waiting on you</span>
              </Show>
            </p>
            <div class="dashboard-filters">
              <CreatorFilter />
              <button
                class="dashboard-filter"
                classList={{ on: dashboard.attentionOnly }}
                aria-pressed={dashboard.attentionOnly}
                onClick={() => setAttentionOnly(!dashboard.attentionOnly)}
                title="Only the pull requests waiting on you: your review, or your own that need changes, have conflicts or can be completed"
              >
                <span class="attention-dot" />
                Waiting on you
              </button>
              <button
                class="dashboard-filter"
                classList={{ on: dashboard.hideDrafts }}
                aria-pressed={dashboard.hideDrafts}
                onClick={() => setHideDrafts(!dashboard.hideDrafts)}
                title="Leave out the draft pull requests"
              >
                Hide drafts
              </button>
              <span class="toolbar-space" />
              <button
                class="dashboard-filter"
                classList={{ "pr-reload": true, busy: dashboard.loading }}
                onClick={() => void loadDashboard()}
                disabled={dashboard.loading}
                title="Read the pull requests again"
              >
                <RefreshIcon />
                {dashboard.loading ? "Reading…" : "Reload"}
              </button>
            </div>
            <For each={listed()}>{(repo) => <RepoSection repo={repo} />}</For>
            <Show when={listed().length === 0}>
              <p class="dashboard-empty">No pull requests to show.</p>
            </Show>
            <Show when={noneActive().length > 0}>
              <p class="connect-note">No active pull requests: {names(noneActive())}.</p>
            </Show>
            <Show when={filteredOut().length > 0}>
              <p class="connect-note">None the filters leave: {names(filteredOut())}.</p>
            </Show>
            <Show when={withoutRemote().length > 0}>
              <p class="connect-note">Without an Azure DevOps remote: {names(withoutRemote())}.</p>
            </Show>
          </Match>
        </Switch>
      </div>
    </div>
  );
}

function RepoSection(props: { repo: RepoPullRequests }) {
  const shown = createMemo(() => attentionFirst(shownOf(props.repo)));
  const filtered = () => (props.repo.list?.length ?? 0) - shown().length;
  return (
    <section class="dashboard-repo">
      <header class="dashboard-repo-header" title={props.repo.root}>
        <span class="dashboard-repo-name">{folderName(props.repo.root)}</span>
        <Show when={props.repo.remote}>{(remote) => <span class="repo-path">{remote().name}</span>}</Show>
        <span class="toolbar-space" />
        <Show when={props.repo.list}>
          {(list) => (
            <span class="commit-meta">
              {filtered() > 0 ? `${shown().length} of ${list().length}` : list().length} active
            </span>
          )}
        </Show>
        <Show when={props.repo.remote}>
          {(remote) => (
            <button
              class="icon-button"
              onClick={() => openInBrowser(`${remote().webUrl}/pullrequests`)}
              title={`Open the pull requests of ${remote().name} in Azure DevOps`}
            >
              <ExternalIcon />
            </button>
          )}
        </Show>
      </header>
      <Switch>
        <Match when={props.repo.error?.kind === "noToken"}>
          <div class="dashboard-note">
            <span>No Azure DevOps token is set up for {props.repo.remote?.organization ?? "this repository"}.</span>
            <button onClick={() => void setUpToken(props.repo.root)}>Set up a token…</button>
          </div>
        </Match>
        <Match when={props.repo.error}>
          {(error) => <p class="connect-problem">{prErrorText(error())}</p>}
        </Match>
        <Match when={true}>
          <ul class="dashboard-list">
            <For each={shown()}>{(pullRequest) => <PullRequestRow repo={props.repo} pullRequest={pullRequest} />}</For>
          </ul>
        </Match>
      </Switch>
    </section>
  );
}

function PullRequestRow(props: { repo: RepoPullRequests; pullRequest: PullRequest }) {
  const status = () => prStatus(props.pullRequest);
  const needs = () => attention(props.pullRequest);
  const remote = () => props.repo.remote!.remote;
  const opening = () => dashboard.opening === openingKey(props.repo.root, remote(), props.pullRequest.id);
  const current = () =>
    state.repo?.root === props.repo.root &&
    state.source === "pullRequest" &&
    state.pullRequest?.remote === remote() &&
    state.pullRequest.id === props.pullRequest.id;
  const open = () => void openFromDashboard(props.repo.root, remote(), props.pullRequest.id);
  return (
    <li
      class="dashboard-pr"
      classList={{ attention: needs() !== null, current: current(), busy: dashboard.opening !== null }}
      tabIndex={0}
      onClick={open}
      onKeyDown={(event) => event.key === "Enter" && open()}
      title={`!${props.pullRequest.id} ${props.pullRequest.title}\n${props.pullRequest.sourceBranch} into ${props.pullRequest.targetBranch}, by ${props.pullRequest.author}`}
    >
      <span class={`pr-status status-${status().kind}`}>{status().label}</span>
      <div class="pr-row">
        <div class="pr-line">
          <span class="picker-name">{props.pullRequest.title}</span>
          <Show when={needs()}>{(what) => <span class="attention-tag">{what()}</span>}</Show>
          <Show
            when={opening()}
            fallback={
              <span class="commit-meta" title={new Date(props.pullRequest.created).toLocaleString()}>
                {ago(props.pullRequest.created)}
              </span>
            }
          >
            <span class="commit-meta pr-opening">Opening…</span>
          </Show>
        </div>
        <div class="pr-line pr-details">
          <span class="pr-id">!{props.pullRequest.id}</span>
          <span class="pr-branches">
            {props.pullRequest.sourceBranch} → {props.pullRequest.targetBranch}
          </span>
          <span class="pr-author">{props.pullRequest.yours ? `${props.pullRequest.author} (you)` : props.pullRequest.author}</span>
          <Show when={votes(props.pullRequest)}>
            {(summary) => (
              <span class="pr-votes" title={summary().title}>
                <For each={summary().parts}>{(part) => <span class={`vote-${part.kind}`}>{part.text}</span>}</For>
              </span>
            )}
          </Show>
        </div>
      </div>
    </li>
  );
}

const sameCreator = (a: Creator, b: Creator) =>
  a.id && b.id ? a.id.toLowerCase() === b.id.toLowerCase() : a.name === b.name;

/**
 * Narrows the dashboard to the pull requests of some people. Who is unticked is
 * kept between starts, so someone new is ticked from their first pull request.
 */
function CreatorFilter() {
  const [open, setOpen] = createSignal(false);
  let root!: HTMLDivElement;

  const onOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) setOpen(false);
  };
  document.addEventListener("pointerdown", onOutside);
  onCleanup(() => document.removeEventListener("pointerdown", onOutside));

  // Everyone with an active pull request, and whoever was unticked earlier even without one now.
  const people = createMemo(() => {
    const found = creatorsOf(dashboard.repos.flatMap((repo) => repo.list ?? []));
    const gone = dashboard.hiddenCreators.filter((creator) => !found.some((each) => sameCreator(each, creator)));
    return [...found, ...gone.map((creator) => ({ ...creator, count: 0, you: false }))];
  });
  const hidden = (creator: Creator) => dashboard.hiddenCreators.some((each) => sameCreator(each, creator));
  const everyone = () => dashboard.hiddenCreators.length === 0;
  const toggle = (creator: Creator) =>
    setHiddenCreators(
      hidden(creator)
        ? dashboard.hiddenCreators.filter((each) => !sameCreator(each, creator))
        : [...dashboard.hiddenCreators, { id: creator.id, name: creator.name }],
    );
  // Unticking *Everyone* unticks all listed, to then tick the few wanted.
  const toggleEveryone = () =>
    setHiddenCreators(everyone() ? people().map((person) => ({ id: person.id, name: person.name })) : []);
  const label = () => {
    if (everyone()) return "Everyone";
    const shown = people().filter((person) => !hidden(person));
    if (shown.length === 0) return "Nobody";
    if (shown.length === 1) return shown[0].you ? "You" : shown[0].name;
    return `${shown.length} people`;
  };

  return (
    <div class="picker creator-filter" ref={root}>
      <button
        class="dashboard-filter"
        classList={{ on: !everyone() }}
        onClick={() => setOpen(!open())}
        title="Only the pull requests these people created"
      >
        Created by: <strong>{label()}</strong>
        <ChevronDownIcon />
      </button>
      <Show when={open()}>
        <div class="picker-popup creator-popup" onKeyDown={(event) => event.key === "Escape" && setOpen(false)}>
          <ul class="picker-list">
            <li onClick={toggleEveryone} classList={{ current: everyone() }}>
              <input class="commit-check" type="checkbox" checked={everyone()} tabIndex={-1} />
              <span class="picker-name">Everyone</span>
            </li>
            <For each={people()}>
              {(person) => (
                <li onClick={() => toggle(person)} classList={{ current: !hidden(person) }}>
                  <input class="commit-check" type="checkbox" checked={!hidden(person)} tabIndex={-1} />
                  <span class="picker-name">
                    {person.name}
                    <Show when={person.you}>
                      <span class="repo-path">you</span>
                    </Show>
                  </span>
                  <span class="commit-meta">{person.count}</span>
                </li>
              )}
            </For>
          </ul>
        </div>
      </Show>
    </div>
  );
}
