// Which view the explorer shows (Solution or Folders), and the solution
// layout the Solution view and the order of the change lists come from.

import { createMemo, createRoot, createSignal } from "solid-js";
import * as ipc from "../ipc/commands";
import { comparePaths } from "./pathOrder";
import { buildSolutionTree, hasSolution, mainSolution, parseSolution, projectsOf, type SolutionTree, solutionOrder } from "./solutionLayout";

export type ExplorerView = "solution" | "folders";

const STORAGE_KEY = "arnold.explorerView";

function rememberedView(): ExplorerView {
  try {
    return localStorage.getItem(STORAGE_KEY) === "folders" ? "folders" : "solution";
  } catch {
    return "solution";
  }
}

/** The view picked: Solution unless the user switched to Folders. */
const [explorerView, setView] = createSignal<ExplorerView>(rememberedView());

export function setExplorerView(view: ExplorerView) {
  setView(view);
  try {
    localStorage.setItem(STORAGE_KEY, view);
  } catch {
    // Not remembered, then.
  }
}

/** The layout of the files listed last, once read; null without solution or project files. */
const [solutionTree, setSolutionTree] = createSignal<SolutionTree | null>(null);
export { solutionTree };

/** Whether the Solution view is shown: picked, and the files have a solution or projects. */
export const showsSolution = () => explorerView() === "solution" && solutionTree() !== null;
export { explorerView };

/** How the change lists (Changes, Unstaged, Staged) are ordered: as the explorer shows the files. */
export const fileOrder = createRoot(() =>
  createMemo(() => {
    const tree = solutionTree();
    return explorerView() === "solution" && tree ? solutionOrder(tree) : comparePaths;
  }),
);

let loading = 0;
/** What the last layout was read from, so an unchanged one is not built again. */
let lastSignature = "";

/**
 * Reads the solution and project files among `files` (from `checkout` when
 * given, else from `commit`) and lays them out for the Solution view.
 */
export async function loadSolution(root: string, commit: string, checkout: string | null, files: readonly string[]) {
  const id = ++loading;
  if (!hasSolution(files)) {
    lastSignature = "";
    setSolutionTree(null);
    return;
  }
  try {
    const solution = mainSolution(files);
    const read = async (paths: string[]) => {
      const texts = paths.length ? await ipc.readTexts(root, commit, checkout, paths) : [];
      return paths.map((path, index) => [path, texts[index] ?? null] as const);
    };
    const [solutionText] = solution ? await read([solution]) : [];
    if (id !== loading) return;
    const entries = solution && solutionText?.[1] ? parseSolution(solution, solutionText[1]) : null;
    const projects = projectsOf(files, entries).filter((path) => /\.fsproj$/i.test(path));
    const texts = new Map([...(solutionText ? [solutionText] : []), ...(await read(projects))]);
    if (id !== loading) return;
    const signature = JSON.stringify([files, solution, [...texts]]);
    if (signature === lastSignature && solutionTree()) return;
    lastSignature = signature;
    setSolutionTree(buildSolutionTree(files, solution, texts));
  } catch {
    // Without its layout the explorer shows the folders.
    if (id === loading) setSolutionTree(null);
  }
}

/** Forgets the layout, as the files are being read again from elsewhere. */
export function clearSolution() {
  loading++;
  lastSignature = "";
  setSolutionTree(null);
}
