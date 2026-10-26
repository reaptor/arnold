// The dashboard: the active pull requests of every favorite repository, open
// or not, in place of the open repository's files. Read when it is opened;
// the creators left out of it, who has been seen, and whether drafts are
// left out, are kept between starts.

import { createStore } from "solid-js/store";
import * as ipc from "../ipc/commands";
import type { PrError, PrRemote, PullRequest } from "../ipc/commands";
import { favorites, sortFavorites } from "./favorites";
import { attention, byCreators, type Creator, creatorsOf, newCreators } from "./prStatus";
import { preferredRemote } from "./pullRequests";
import { openPullRequestIn } from "./review";

const SETTING = "arnold.dashboard";

/** A favorite repository and its active pull requests. */
export interface RepoPullRequests {
  root: string;
  /** The remote they come from; null when the repository has no Azure DevOps remote. */
  remote: PrRemote | null;
  /** Null when they could not be read. */
  list: PullRequest[] | null;
  /** Why not: no token, the host turned it down, the folder is gone... */
  error: PrError | null;
  /** How many discussions each pull request (by id) has still to resolve; missing until read, or when that failed. */
  openThreads: Record<number, number | undefined>;
}

interface DashboardState {
  open: boolean;
  /** Each favorite, sorted as the repository button lists them; empty until first read. */
  repos: RepoPullRequests[];
  loading: boolean;
  /**
   * The pull requests these people opened are not shown. Who is left out is
   * kept rather than who is picked, so newcomers are shown from their first.
   */
  hiddenCreators: Creator[];
  /**
   * Everyone the dashboard has listed, or the user has picked or left out.
   * Someone else, while not everyone is shown, makes it ask whether to show
   * them. Null until the dashboard is first read.
   */
  knownCreators: Creator[] | null;
  /** Draft pull requests are not shown. */
  hideDrafts: boolean;
  /** The pull request being opened from the dashboard, `root\nremote\nid`. */
  opening: string | null;
}

const creatorList = (saved: unknown): Creator[] | null =>
  Array.isArray(saved)
    ? saved.filter((creator: Creator) => typeof creator?.id === "string" && typeof creator?.name === "string")
    : null;

function readSetting(): Pick<DashboardState, "hiddenCreators" | "knownCreators" | "hideDrafts"> {
  try {
    const saved = JSON.parse(localStorage.getItem(SETTING) ?? "{}");
    return {
      hiddenCreators: creatorList(saved.hiddenCreators) ?? [],
      knownCreators: creatorList(saved.knownCreators),
      hideDrafts: saved.hideDrafts === true,
    };
  } catch {
    return { hiddenCreators: [], knownCreators: null, hideDrafts: false };
  }
}

export const [dashboard, setDashboard] = createStore<DashboardState>({
  open: false,
  repos: [],
  loading: false,
  opening: null,
  ...readSetting(),
});

const saveSetting = () =>
  localStorage.setItem(
    SETTING,
    JSON.stringify({
      hiddenCreators: dashboard.hiddenCreators,
      knownCreators: dashboard.knownCreators,
      hideDrafts: dashboard.hideDrafts,
    }),
  );

export const openingKey = (root: string, remote: string, id: number) => `${root}\n${remote}\n${id}`;

const asPrError = (error: unknown): PrError => (ipc.isPrError(error) ? error : { kind: "other", message: String(error) });

async function readRepo(root: string): Promise<RepoPullRequests> {
  let remote: PrRemote | null;
  try {
    remote = preferredRemote(root, await ipc.favoritePullRequestRemotes(root)) ?? null;
  } catch (error) {
    return { root, remote: null, list: null, error: asPrError(error), openThreads: {} };
  }
  if (!remote) return { root, remote: null, list: [], error: null, openThreads: {} };
  if (remote.token === null) return { root, remote, list: null, error: { kind: "noToken" }, openThreads: {} };
  // The counts read before stay until the new ones are in.
  const openThreads = { ...dashboard.repos.find((repo) => repo.root === root)?.openThreads };
  try {
    return { root, remote, list: await ipc.listFavoritePullRequests(root, remote.remote), error: null, openThreads };
  } catch (error) {
    return { root, remote, list: null, error: asPrError(error), openThreads: {} };
  }
}

/** Reads how many discussions each of the pull requests of `repos[index]` has open, one request each. */
async function readOpenThreads(index: number, run: number) {
  const repo = dashboard.repos[index];
  if (!repo.remote || !repo.list) return;
  const root = repo.root;
  const remote = repo.remote.remote;
  await Promise.all(
    repo.list.map(async ({ id }) => {
      // Left out rather than shown wrong when it cannot be read.
      const count = await ipc.favoritePullRequestOpenThreads(root, remote, id).catch(() => undefined);
      if (run === reading) setDashboard("repos", index, "openThreads", id, count);
    }),
  );
}

/** Everyone who opened one of the pull requests read. */
const found = () => creatorsOf(dashboard.repos.flatMap((repo) => repo.list ?? []));

/** Adds `creators` to those known, keeping who was known. */
function know(creators: readonly Creator[]) {
  const known = dashboard.knownCreators ?? [];
  const added = newCreators(creators, known, []).map(({ id, name }) => ({ id, name }));
  if (dashboard.knownCreators !== null && added.length === 0) return;
  setDashboard("knownCreators", [...known, ...added]);
  saveSetting();
}

/**
 * People who opened pull requests the dashboard has not listed before, while
 * not everyone is shown: the dashboard asks whether to show them (they are
 * until the user says).
 */
export const unknownCreators = () =>
  dashboard.hiddenCreators.length === 0 || dashboard.knownCreators === null
    ? []
    : newCreators(found(), dashboard.knownCreators, dashboard.hiddenCreators);

/** Answers whether to show the pull requests of `unknownCreators`. */
export function includeUnknownCreators(include: boolean) {
  const unknown = unknownCreators();
  if (!include) setHiddenCreators([...dashboard.hiddenCreators, ...unknown.map(({ id, name }) => ({ id, name }))]);
  know(unknown);
}

let reading = 0;

/** (Re-)reads the pull requests of every favorite, all at once; what was read before stays shown meanwhile. */
export async function loadDashboard() {
  const run = ++reading;
  setDashboard("loading", true);
  const repos = await Promise.all(sortFavorites(favorites()).map(readRepo));
  if (run !== reading) return;
  setDashboard({ repos, loading: false });
  // Everyone is shown, or the first time: nobody to ask about.
  if (dashboard.hiddenCreators.length === 0 || dashboard.knownCreators === null) know(found());
  repos.forEach((_, index) => void readOpenThreads(index, run));
}

export function showDashboard(open: boolean) {
  setDashboard("open", open);
  if (open) void loadDashboard();
}

/** The pull requests of `repo` the filters leave, those waiting on the user first. */
export function shownOf(repo: RepoPullRequests): PullRequest[] {
  return (repo.list ?? []).filter(
    (pullRequest) =>
      byCreators(pullRequest, dashboard.hiddenCreators) &&
      (!dashboard.hideDrafts || !pullRequest.draft),
  );
}

/** How many pull requests from the creators not left out (and not drafts left out) wait on the user, once read. */
export const attentionCount = () =>
  dashboard.repos
    .flatMap((repo) => repo.list ?? [])
    .filter(
      (pullRequest) =>
        byCreators(pullRequest, dashboard.hiddenCreators) &&
        (!dashboard.hideDrafts || !pullRequest.draft) &&
        attention(pullRequest) !== null,
    ).length;

/** Leaves out the pull requests of `hidden`; whoever is picked or left out here is known from then on. */
export function setHiddenCreators(hidden: Creator[]) {
  const before = dashboard.hiddenCreators;
  const changed = [...newCreators(hidden, before, []), ...newCreators(before, hidden, [])];
  setDashboard("hiddenCreators", hidden);
  saveSetting();
  know(hidden.length === 0 ? found() : changed);
}

export function setHideDrafts(on: boolean) {
  setDashboard("hideDrafts", on);
  saveSetting();
}

/**
 * Reviews a pull request picked on the dashboard, as if it were picked in the
 * list of its repository (opening that first): it is checked out if needed,
 * then the dashboard gives way to it.
 */
export async function openFromDashboard(root: string, remote: string, id: number) {
  if (dashboard.opening !== null) return;
  setDashboard("opening", openingKey(root, remote, id));
  try {
    await openPullRequestIn(root, remote, id);
  } finally {
    setDashboard({ opening: null, open: false });
  }
}
