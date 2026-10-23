// Notifications of the operating system about the pull requests the user
// follows: new comments, votes, pushes and completion. Off until the user
// turns them on for a pull request, or for all of them in the repository
// (which asking when turning on one offers). While any are followed, the
// active pull requests of the open repository are checked every few minutes,
// also with Arnold in the background; a click on a notification opens the
// pull request there, at the comment.

import { createEffect, createRoot, createSignal, on } from "solid-js";
import * as ipc from "../ipc/commands";
import type { PullRequest } from "../ipc/commands";
import { addedNews, commentKeys, type News, newsOf, type Seen } from "./prNews";
import { currentRemote, prErrorText, prs, selectRemote } from "./pullRequests";
import {
  checkPullRequest,
  isChanged,
  loadThreads,
  openDiscussions,
  openPullRequest,
  openRepoAt,
  revealThread,
  state,
  threads,
} from "./review";

const SETTING = "arnold.prNotifications";
/** How often the pull requests are checked. */
const CHECK_EVERY = 2 * 60_000;

/** Where a click on a notification goes. */
interface Target {
  root: string;
  remote: string;
  id: number;
  /** The discussion of the comment it is about. */
  thread: number | null;
}

/** The pull requests followed in a repository: all of them (new ones too), or those in `ids`. */
interface Followed {
  all: boolean;
  ids: number[];
}

/** By repository and remote, `root\nremote`. */
const [followed, setFollowed] = createSignal<Record<string, Followed>>(readSetting());

function readSetting(): Record<string, Followed> {
  try {
    const read = JSON.parse(localStorage.getItem(SETTING) ?? "{}");
    return read && typeof read === "object" && !Array.isArray(read) ? read : {};
  } catch {
    return {};
  }
}

const NONE: Followed = { all: false, ids: [] };
const keyOf = (root: string, remote: string) => `${root}\n${remote}`;
const followedIn = (root: string | null, remote: string | null): Followed =>
  (root && remote && followed()[keyOf(root, remote)]) || NONE;
/** What is followed in the open repository, at its current remote. */
const here = () => followedIn(prs.root, prs.remote);

/** Whether the user gets notified about every pull request of the open repository. */
export const allNotified = () => here().all;
/** Whether the user gets notified about pull request `id` of the open repository. */
export const isNotified = (id: number) => here().all || here().ids.includes(id);
/** The pull requests of the open repository notified about one by one. */
export const notifiedIds = () => here().ids;

function save(root: string, remote: string, now: Followed) {
  const turnedOn = !Object.values(followed()).some((each) => each.all || each.ids.length > 0) && (now.all || now.ids.length > 0);
  const next = { ...followed() };
  if (now.all || now.ids.length > 0) next[keyOf(root, remote)] = now;
  else delete next[keyOf(root, remote)];
  try {
    localStorage.setItem(SETTING, JSON.stringify(next));
  } catch {
    // Kept for this run only.
  }
  setFollowed(next);
  if (turnedOn) {
    // Shows that they work (and has macOS ask whether Arnold may send them).
    void ipc
      .notify(
        "Notifications are on",
        now.all
          ? "Arnold tells you about new comments, votes and pushes on the pull requests of this repository."
          : "Arnold tells you about new comments, votes and pushes on this pull request.",
        "",
      )
      .catch((error) => ipc.devTrace(`notifying: ${error}`));
  }
  void checkForNews();
}

/** Turns notifications about every pull request of the open repository, new ones too, on or off. */
export function setAllNotified(on: boolean) {
  const { root, remote } = prs;
  if (!root || !remote) return;
  save(root, remote, on ? { ...here(), all: true } : NONE);
}

/**
 * Turns notifications about pull request `id` of the open repository on or
 * off. Off while all are notified about: the others stay on.
 */
export function setNotified(id: number, on: boolean) {
  const { root, remote } = prs;
  if (!root || !remote) return;
  const now = here();
  const others = (now.all ? (prs.list ?? []).map((pullRequest) => pullRequest.id) : now.ids).filter((each) => each !== id);
  save(root, remote, { all: on && now.all, ids: on ? [...others, id] : others });
}

/** The question asked when turning on notifications about one pull request. */
export interface NotifyQuestion {
  id: number;
  title: string;
  /** The repository's name. */
  repo: string;
}

const [notifyQuestion, setNotifyQuestion] = createSignal<NotifyQuestion | null>(null);
/** Set while the Notifications dialog is open. */
export { notifyQuestion };

/** Turns on notifications about `pullRequest`, asking whether about all pull requests of the repository instead. */
export function askToNotify(pullRequest: { id: number; title: string }) {
  if (!prs.root || !prs.remote) return;
  const repo = prs.root.split(/[\\/]/).filter(Boolean).pop() ?? prs.root;
  setNotifyQuestion({ id: pullRequest.id, title: pullRequest.title, repo });
}

/** The answer to the Notifications dialog: this pull request, all of the repository's, or null to cancel. */
export function answerNotify(answer: "one" | "all" | null) {
  const asked = notifyQuestion();
  setNotifyQuestion(null);
  if (!asked || !answer) return;
  if (answer === "all") setAllNotified(true);
  else setNotified(asked.id, true);
}

/** The followed pull requests at the last check, of `seenOf`; null until the first check, which notifies nothing. */
let seen: Map<number, Seen> | null = null;
/** The ids of every active pull request at the last check. */
let known = new Set<number>();
let seenOf = "";
let checking = false;
let started = false;

/** Starts checking, and going where a clicked notification points. */
export function startNotifications() {
  if (started) return;
  started = true;
  void ipc.onNotificationClicked((key) => void goTo(key));
  window.setInterval(() => void checkForNews(), CHECK_EVERY);
  // Another repository, remote or token: what it has now is the starting point.
  createRoot(() =>
    createEffect(
      on(
        () => [prs.root, prs.remote, currentRemote()?.token],
        () => void checkForNews(),
      ),
    ),
  );
}

/** Reads the followed pull requests and notifies about what changed since the last check. */
export async function checkForNews() {
  const { root, remote } = prs;
  if (checking || !root || !remote || !currentRemote()?.token) return;
  const isFollowed = (id: number) => followedIn(root, remote).all || followedIn(root, remote).ids.includes(id);
  if (!followedIn(root, remote).all && followedIn(root, remote).ids.length === 0) {
    // Turned on again later, that is the starting point.
    seen = null;
    return;
  }
  const about = keyOf(root, remote);
  if (about !== seenOf) {
    seen = null;
    seenOf = about;
  }
  checking = true;
  try {
    const list = await ipc.listPullRequests(root, remote);
    const before = seen;
    const now = new Map<number, Seen>();
    for (const pullRequest of list.filter((each) => isFollowed(each.id))) {
      const threads = await ipc.pullRequestThreads(root, remote, pullRequest.id).catch(() => null);
      now.set(pullRequest.id, { pullRequest, comments: threads && commentKeys(threads) });
      if (!before) continue;
      const earlier = before.get(pullRequest.id);
      // Just turned on, it is the starting point; new in the repository, it is news.
      if (earlier) send(root, remote, pullRequest, newsOf(earlier, pullRequest, threads));
      else if (!known.has(pullRequest.id)) send(root, remote, pullRequest, addedNews(pullRequest));
    }
    // Followed ones that are no longer active: completed or abandoned.
    for (const [id, earlier] of before ?? []) {
      if (!isFollowed(id) || list.some((pullRequest) => pullRequest.id === id)) continue;
      const pullRequest = await ipc.readPullRequest(root, remote, id).catch(() => null);
      if (pullRequest) send(root, remote, pullRequest, newsOf(earlier, pullRequest, null));
    }
    // Picked ones that are gone: nothing more to follow.
    const left = followedIn(root, remote);
    const ids = left.ids.filter((id) => list.some((pullRequest) => pullRequest.id === id));
    if (ids.length < left.ids.length) save(root, remote, { ...left, ids });
    if (seenOf === about) {
      seen = now;
      known = new Set(list.map((pullRequest) => pullRequest.id));
    }
  } catch (error) {
    ipc.devTrace(`checking pull requests for notifications: ${prErrorText(error)}`);
  } finally {
    checking = false;
  }
}

function send(root: string, remote: string, pullRequest: PullRequest, news: News) {
  if (news.lines.length === 0) return;
  // Shown in place already.
  const inView =
    document.hasFocus() &&
    state.repo?.root === root &&
    state.source === "pullRequest" &&
    state.pullRequest?.remote === remote &&
    state.pullRequest.id === pullRequest.id;
  if (inView) return;
  const target: Target = { root, remote, id: pullRequest.id, thread: news.thread };
  void ipc
    .notify(`!${pullRequest.id} ${pullRequest.title}`, news.lines.join("\n"), JSON.stringify(target))
    .catch((error) => ipc.devTrace(`notifying: ${error}`));
}

/** Opens the pull request a clicked notification is about (checking it out), at its comment. */
async function goTo(key: string) {
  let target: Target;
  try {
    target = JSON.parse(key);
  } catch {
    return;
  }
  if (typeof target?.id !== "number") return;
  if (state.repo?.root !== target.root) {
    await openRepoAt(target.root);
    if (state.repo?.root !== target.root) return;
  }
  if (prs.remote !== target.remote) await selectRemote(target.remote);
  const open = state.source === "pullRequest" && state.pullRequest?.remote === target.remote && state.pullRequest.id === target.id;
  if (open) await checkPullRequest();
  else {
    await openPullRequest(target.remote, target.id);
    if (state.pullRequest?.id !== target.id) return;
    await loadThreads();
  }
  const thread = threads().find((candidate) => candidate.id === target.thread);
  if (!thread) return;
  if (thread.path && isChanged(thread.path)) await revealThread(thread);
  else openDiscussions();
}
