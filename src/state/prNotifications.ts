// Notifications of the operating system about the pull requests the user
// follows (opened, or is a reviewer of): new comments, votes, pushes and
// completion. Off until the user turns them on. While on, the active pull
// requests of the open repository are checked every few minutes, also with
// Arnold in the background; a click on a notification opens the pull request
// there, at the comment.

import { createEffect, createRoot, createSignal, on } from "solid-js";
import * as ipc from "../ipc/commands";
import type { PullRequest } from "../ipc/commands";
import { addedNews, commentKeys, isFollowed, type News, newsOf, type Seen } from "./prNews";
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

const [enabled, setEnabled] = createSignal(readSetting());
/** Whether the user turned notifications on. */
export const notificationsOn = enabled;

function readSetting(): boolean {
  try {
    return localStorage.getItem(SETTING) === "on";
  } catch {
    return false;
  }
}

/** The followed pull requests at the last check, of `seenOf`; null until the first check, which notifies nothing. */
let seen: Map<number, Seen> | null = null;
let seenOf = "";
let checking = false;
let started = false;

export function setNotificationsOn(value: boolean) {
  localStorage.setItem(SETTING, value ? "on" : "off");
  setEnabled(value);
  seen = null;
  if (!value) return;
  // Shows that they work (and has macOS ask whether Arnold may send them).
  void ipc
    .notify(
      "Notifications are on",
      "Arnold tells you about new comments, votes and pushes on the pull requests you opened or review.",
      "",
    )
    .catch((error) => ipc.devTrace(`notifying: ${error}`));
  void checkForNews();
}

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
  if (!notificationsOn() || checking || !root || !remote || !currentRemote()?.token) return;
  const about = `${root}\n${remote}`;
  if (about !== seenOf) {
    seen = null;
    seenOf = about;
  }
  checking = true;
  try {
    const list = await ipc.listPullRequests(root, remote);
    const before = seen;
    const now = new Map<number, Seen>();
    for (const pullRequest of list.filter(isFollowed)) {
      const threads = await ipc.pullRequestThreads(root, remote, pullRequest.id).catch(() => null);
      now.set(pullRequest.id, { pullRequest, comments: threads && commentKeys(threads) });
      if (!before) continue;
      const earlier = before.get(pullRequest.id);
      send(root, remote, pullRequest, earlier ? newsOf(earlier, pullRequest, threads) : addedNews(pullRequest));
    }
    // Followed ones that are no longer active: completed or abandoned.
    for (const [id, earlier] of before ?? []) {
      if (list.some((pullRequest) => pullRequest.id === id)) continue;
      const pullRequest = await ipc.readPullRequest(root, remote, id).catch(() => null);
      if (pullRequest) send(root, remote, pullRequest, newsOf(earlier, pullRequest, null));
    }
    if (seenOf === about && notificationsOn()) seen = now;
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
