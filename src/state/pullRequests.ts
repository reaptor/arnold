// The pull requests of the repository's code host (Azure DevOps): which
// remotes have them, the active ones, and the personal access token they
// need. Picking one to review is in review.ts.

import { createStore } from "solid-js/store";
import * as ipc from "../ipc/commands";
import type { PrError, PrRemote, PullRequest } from "../ipc/commands";

interface PrState {
  root: string | null;
  /** The repository's remotes that have pull requests; empty when none do. */
  remotes: PrRemote[];
  /** The remote whose pull requests are listed. */
  remote: string | null;
  /** Its active pull requests, newest first; null until they have been read. */
  list: PullRequest[] | null;
  loading: boolean;
  /** Why the list could not be read. */
  error: PrError | null;
  /** A token is being checked with the host. */
  checkingToken: boolean;
  /** Why the token just entered was not saved. */
  tokenError: string | null;
  /** The token dialog is open (to change or remove the token). */
  tokenDialog: boolean;
}

export const [prs, setPrs] = createStore<PrState>({
  root: null,
  remotes: [],
  remote: null,
  list: null,
  loading: false,
  error: null,
  checkingToken: false,
  tokenError: null,
  tokenDialog: false,
});

/** The remote whose pull requests are listed. */
export const currentRemote = (): PrRemote | null => prs.remotes.find((remote) => remote.remote === prs.remote) ?? null;

/** No token, or the one there is was turned down: the user has to set one up. */
export const needsToken = () => {
  const remote = currentRemote();
  return remote !== null && (remote.token === null || prs.error?.kind === "tokenRejected" || prs.error?.kind === "noToken");
};

/** Text for an error of a pull request command (or anything else). */
export function prErrorText(error: unknown): string {
  if (!ipc.isPrError(error)) return String(error);
  switch (error.kind) {
    case "noToken":
      return "No personal access token is set up for Azure DevOps.";
    case "tokenRejected":
      return `Azure DevOps did not accept the personal access token${error.message ? ` (${error.message})` : ""}.`;
    default:
      return error.message;
  }
}

/** Finds the repository's remotes that have pull requests. Never fails: without them there is just no choice. */
export async function loadRemotes(root: string) {
  setPrs({ root, remotes: [], remote: null, list: null, error: null, tokenError: null, tokenDialog: false });
  let remotes: PrRemote[];
  try {
    remotes = await ipc.pullRequestRemotes(root);
  } catch (error) {
    ipc.devTrace(`pull request remotes: ${prErrorText(error)}`);
    remotes = [];
  }
  if (prs.root !== root) return;
  const remembered = localStorage.getItem(`arnold.prRemote.${root}`);
  const remote =
    remotes.find((candidate) => candidate.remote === remembered) ??
    remotes.find((candidate) => candidate.remote === "origin") ??
    remotes[0];
  setPrs({ remotes, remote: remote?.remote ?? null });
}

/** (Re-)reads the active pull requests of the current remote. */
export async function loadPullRequests() {
  const { root, remote } = prs;
  const current = currentRemote();
  if (!root || !remote || !current) return;
  if (current.token === null) {
    setPrs({ list: null, error: { kind: "noToken" } });
    return;
  }
  setPrs({ loading: true });
  try {
    const list = await ipc.listPullRequests(root, remote);
    if (prs.root === root && prs.remote === remote) setPrs({ list, error: null });
  } catch (error) {
    if (prs.root === root && prs.remote === remote) {
      setPrs({ error: ipc.isPrError(error) ? error : { kind: "other", message: String(error) } });
    }
  } finally {
    setPrs("loading", false);
  }
}

export async function selectRemote(remote: string) {
  if (remote === prs.remote || !prs.root) return;
  localStorage.setItem(`arnold.prRemote.${prs.root}`, remote);
  setPrs({ remote, list: null, error: null, tokenError: null });
  await loadPullRequests();
}

const setRemoteToken = (remote: string, token: PrRemote["token"]) =>
  setPrs("remotes", (candidate) => candidate.remote === remote, "token", token);

/** Checks `token` with the host; saves it and lists the pull requests if it works. */
export async function saveToken(token: string): Promise<boolean> {
  const { root, remote } = prs;
  if (!root || !remote) return false;
  setPrs({ checkingToken: true, tokenError: null });
  try {
    const list = await ipc.setPullRequestToken(root, remote, token);
    if (prs.root !== root) return false;
    // A token works for every repository of the organization.
    const organization = currentRemote()?.organization;
    for (const other of prs.remotes) {
      if (other.organization === organization) setRemoteToken(other.remote, "keychain");
    }
    setPrs({ list, error: null, tokenDialog: false });
    return true;
  } catch (error) {
    setPrs("tokenError", tokenErrorText(error));
    return false;
  } finally {
    setPrs("checkingToken", false);
  }
}

function tokenErrorText(error: unknown): string {
  if (!ipc.isPrError(error)) return String(error);
  const organization = currentRemote()?.organization ?? "the organization";
  switch (error.kind) {
    case "noToken":
      return "Paste a token first.";
    case "tokenRejected":
      return `Azure DevOps did not accept this token. Check that it was created for ${organization}, has the Code (Read) scope and has not expired.`;
    case "notFound":
      return `The token works, but it cannot see this repository: ${error.message}`;
    default:
      return error.message;
  }
}

/** Forgets the saved token of the current remote's organization. */
export async function removeToken() {
  const { root, remote } = prs;
  if (!root || !remote) return;
  try {
    const left = await ipc.removePullRequestToken(root, remote);
    const organization = currentRemote()?.organization;
    for (const other of prs.remotes) {
      if (other.organization === organization) setRemoteToken(other.remote, left);
    }
    setPrs({ tokenDialog: false, list: null, error: left ? null : { kind: "noToken" } });
    if (left) await loadPullRequests();
  } catch (error) {
    setPrs("tokenError", prErrorText(error));
  }
}

export const openTokenDialog = () => setPrs({ tokenDialog: true, tokenError: null });
export const closeTokenDialog = () => setPrs({ tokenDialog: false, tokenError: null });
