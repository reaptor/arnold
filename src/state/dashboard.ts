// The dashboard: the active pull requests of every favorite repository, open
// or not, in place of the open repository's files. Read when it is opened;
// the creators it is narrowed to are kept between starts.

import { createStore } from "solid-js/store";
import * as ipc from "../ipc/commands";
import type { PrError, PrRemote, PullRequest } from "../ipc/commands";
import { favorites, sortFavorites } from "./favorites";
import { attention, byCreators, type Creator } from "./prStatus";
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
}

interface DashboardState {
  open: boolean;
  /** Each favorite, sorted as the repository button lists them; empty until first read. */
  repos: RepoPullRequests[];
  loading: boolean;
  /** Only pull requests these people opened are shown; everyone's when empty. */
  creators: Creator[];
  /** Only the pull requests that wait on the user are shown. */
  attentionOnly: boolean;
  /** The pull request being opened from the dashboard, `root\nremote\nid`. */
  opening: string | null;
}

function readSetting(): Pick<DashboardState, "creators" | "attentionOnly"> {
  try {
    const saved = JSON.parse(localStorage.getItem(SETTING) ?? "{}");
    const creators = Array.isArray(saved.creators)
      ? saved.creators.filter((creator: Creator) => typeof creator?.id === "string" && typeof creator?.name === "string")
      : [];
    return { creators, attentionOnly: saved.attentionOnly === true };
  } catch {
    return { creators: [], attentionOnly: false };
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
  localStorage.setItem(SETTING, JSON.stringify({ creators: dashboard.creators, attentionOnly: dashboard.attentionOnly }));

export const openingKey = (root: string, remote: string, id: number) => `${root}\n${remote}\n${id}`;

const asPrError = (error: unknown): PrError => (ipc.isPrError(error) ? error : { kind: "other", message: String(error) });

async function readRepo(root: string): Promise<RepoPullRequests> {
  let remote: PrRemote | null;
  try {
    remote = preferredRemote(root, await ipc.favoritePullRequestRemotes(root)) ?? null;
  } catch (error) {
    return { root, remote: null, list: null, error: asPrError(error) };
  }
  if (!remote) return { root, remote: null, list: [], error: null };
  if (remote.token === null) return { root, remote, list: null, error: { kind: "noToken" } };
  try {
    return { root, remote, list: await ipc.listFavoritePullRequests(root, remote.remote), error: null };
  } catch (error) {
    return { root, remote, list: null, error: asPrError(error) };
  }
}

let reading = 0;

/** (Re-)reads the pull requests of every favorite, all at once; what was read before stays shown meanwhile. */
export async function loadDashboard() {
  const run = ++reading;
  setDashboard("loading", true);
  const repos = await Promise.all(sortFavorites(favorites()).map(readRepo));
  if (run !== reading) return;
  setDashboard({ repos, loading: false });
}

export function showDashboard(open: boolean) {
  setDashboard("open", open);
  if (open) void loadDashboard();
}

/** The pull requests of `repo` the filters leave, those waiting on the user first. */
export function shownOf(repo: RepoPullRequests): PullRequest[] {
  return (repo.list ?? []).filter(
    (pullRequest) =>
      byCreators(pullRequest, dashboard.creators) && (!dashboard.attentionOnly || attention(pullRequest) !== null),
  );
}

/** How many pull requests from the chosen creators wait on the user, once read. */
export const attentionCount = () =>
  dashboard.repos
    .flatMap((repo) => repo.list ?? [])
    .filter((pullRequest) => byCreators(pullRequest, dashboard.creators) && attention(pullRequest) !== null).length;

export function setCreators(creators: Creator[]) {
  setDashboard("creators", creators);
  saveSetting();
}

export function setAttentionOnly(on: boolean) {
  setDashboard("attentionOnly", on);
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
