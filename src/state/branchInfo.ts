import { createSignal } from "solid-js";
import * as ipc from "../ipc/commands";
import type { Branch } from "../ipc/commands";

// What the branch pickers show besides the names: who created each branch
// (Azure DevOps knows; git does not), and the order they are listed in.

const [creators, setCreators] = createSignal<{ root: string; byName: Record<string, string> } | null>(null);
let creatorsAsked: { root: string; at: number } | null = null;
const CREATORS_AGAIN = 60_000;

/** Asks Azure DevOps who created the branches of `root`, unless asked in the last minute (`force`: anyway). */
export async function loadCreators(root: string, force = false) {
  if (!force && creatorsAsked?.root === root && Date.now() - creatorsAsked.at < CREATORS_AGAIN) return;
  creatorsAsked = { root, at: Date.now() };
  try {
    const byName = await ipc.branchCreators(root);
    setCreators({ root, byName });
  } catch (error) {
    // The picker shows who committed last instead.
    ipc.devTrace(`branch creators: ${String(error)}`);
  }
}

/** Who created `branch` on its remote, if the remote says: a local branch by the branch it is pushed to. */
export function creatorOf(root: string, branch: Branch): string | null {
  const known = creators();
  if (known?.root !== root) return null;
  if (branch.isRemote) return known.byName[branch.name] ?? null;
  const pushedTo = branch.sync ? `${branch.sync.remote}/${branch.sync.remoteBranch}` : `origin/${branch.name}`;
  return known.byName[pushedTo] ?? null;
}

export type BranchOrder = "recent" | "name";

const ORDER_KEY = "arnold.branchOrder";
function storedOrder(): BranchOrder {
  try {
    return localStorage.getItem(ORDER_KEY) === "name" ? "name" : "recent";
  } catch {
    return "recent";
  }
}
const [branchOrder, setOrder] = createSignal<BranchOrder>(storedOrder());
export { branchOrder };

/** Lists branches by their last commit, newest first, or by name; kept between app starts. */
export function setBranchOrder(order: BranchOrder) {
  setOrder(order);
  localStorage.setItem(ORDER_KEY, order);
}

/** `branches` in `order`: by last commit (newest first, then by name), or by name. */
export function sortBranches(branches: Branch[], order: BranchOrder): Branch[] {
  const byName = (a: Branch, b: Branch) => a.name.localeCompare(b.name, undefined, { sensitivity: "base", numeric: true });
  return [...branches].sort(order === "recent" ? (a, b) => b.updatedAt - a.updatedAt || byName(a, b) : byName);
}

/**
 * The branches to pick a checkout from in Commit mode: every local branch,
 * and each remote branch no local branch is pushed to (picking one checks it
 * out as a local branch, in a worktree of its own).
 */
export function checkoutChoices(branches: Branch[]): Branch[] {
  const local = branches.filter((branch) => !branch.isRemote);
  const tracked = new Set(local.map((branch) => (branch.sync ? `${branch.sync.remote}/${branch.sync.remoteBranch}` : null)));
  const names = new Set(local.map((branch) => branch.name));
  const remoteOnly = branches.filter(
    (branch) => branch.isRemote && !tracked.has(branch.name) && !names.has(branch.name.slice(branch.name.indexOf("/") + 1)),
  );
  return [...local, ...remoteOnly];
}
