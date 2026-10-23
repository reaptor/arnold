// The rows of the All files explorer, in its Folders view or Solution view.

import { collator } from "../state/pathOrder";
import type { GroupIcon, SolutionTree, TreeNode } from "../state/solutionLayout";
import { PROJECT_EXTENSIONS } from "./fileIcons";

interface Folder {
  path: string;
  folders: Map<string, Folder>;
  /** Full paths of the files directly in this folder. */
  files: string[];
  sorted: boolean;
}

export interface Row {
  kind: "folder" | "file";
  /** Identifies the row: a file's path, a folder's path, or a key of the Solution view. */
  key: string;
  /** The file, or the real folder a group stands for ("" when none). */
  path: string;
  /** The solution or project file a group stands for ("" when none). */
  projectFile: string;
  label: string;
  /** Folder shown after the name: of a file found by the filter, or one shown away from its folder. */
  detail: string;
  depth: number;
  open: boolean;
  icon: GroupIcon | null;
  /** Explains the row, after its path. */
  note: string;
  /** An F# file in a project's folder that the project file does not list. */
  unlisted: string;
}

export interface TreeModel {
  rows(expanded: Record<string, boolean>): Row[];
  /** Keys of the groups holding the row `key` (a file's path or a group's key), outermost first. */
  holders(key: string): string[];
  isGroup(key: string): boolean;
  /** Keys of the group and every group in it; with null, of every group. */
  groupsIn(key: string | null): string[];
  /** What Collapse all leaves open or closed. */
  collapsed(): Record<string, boolean>;
}

const nameOf = (path: string) => path.slice(path.lastIndexOf("/") + 1);

export const ancestors = (path: string) => {
  const parts = path.split("/");
  return parts.slice(0, -1).map((_, index) => parts.slice(0, index + 1).join("/"));
};

function buildFolders(paths: readonly string[]): Folder {
  const root: Folder = { path: "", folders: new Map(), files: [], sorted: false };
  for (const path of paths) {
    const parts = path.split("/");
    let folder = root;
    for (let index = 0; index < parts.length - 1; index++) {
      let child = folder.folders.get(parts[index]);
      if (!child) {
        child = { path: parts.slice(0, index + 1).join("/"), folders: new Map(), files: [], sorted: false };
        folder.folders.set(parts[index], child);
      }
      folder = child;
    }
    folder.files.push(path);
  }
  return root;
}

/** Folders are sorted the first time they are opened, not all up front. */
function sortOnce(folder: Folder) {
  if (folder.sorted) return;
  folder.folders = new Map([...folder.folders].sort(([a], [b]) => collator.compare(a, b)));
  folder.files.sort((a, b) => collator.compare(nameOf(a), nameOf(b)));
  folder.sorted = true;
}

/** The folder at `path` ("" for `root` itself), if there is one. */
function folderAt(root: Folder, path: string): Folder | undefined {
  if (path === "") return root;
  let folder: Folder | undefined = root;
  for (const part of path.split("/")) folder = folder?.folders.get(part);
  return folder;
}

function eachFolder(folder: Folder, visit: (folder: Folder) => void) {
  visit(folder);
  for (const child of folder.folders.values()) eachFolder(child, visit);
}

const fileRow = (path: string, depth: number, extra: Partial<Row> = {}): Row => ({
  kind: "file",
  key: path,
  path,
  projectFile: "",
  label: nameOf(path),
  detail: "",
  depth,
  open: false,
  icon: null,
  note: "",
  unlisted: "",
  ...extra,
});

/**
 * The rows of the open part of a folder tree, folders before files as in VS
 * Code. Folder keys are their paths after `prefix`.
 */
function flattenFolder(
  folder: Folder,
  expanded: Record<string, boolean>,
  depth: number,
  rows: Row[],
  prefix: string,
  iconOf: (path: string) => GroupIcon,
) {
  sortOnce(folder);
  for (const [name, child] of folder.folders) {
    // A chain of folders that each hold just one folder is one row: "src/Api".
    let shown = child;
    let label = name;
    while (shown.files.length === 0 && shown.folders.size === 1) {
      const [[innerName, inner]] = shown.folders;
      label += `/${innerName}`;
      shown = inner;
    }
    const key = prefix + shown.path;
    const open = !!expanded[key];
    rows.push({ kind: "folder", key, path: shown.path, projectFile: "", label, detail: "", depth, open, icon: iconOf(shown.path), note: "", unlisted: "" });
    if (open) flattenFolder(shown, expanded, depth + 1, rows, prefix, iconOf);
  }
  for (const path of folder.files) rows.push(fileRow(path, depth));
}

/** Folders holding a project file show that project's icon. */
function projectIcons(paths: readonly string[]): (path: string) => GroupIcon {
  const projects = new Map<string, GroupIcon>();
  for (const path of paths) {
    const kind = PROJECT_EXTENSIONS[path.slice(path.lastIndexOf(".") + 1).toLowerCase()];
    const slash = path.lastIndexOf("/");
    if (kind && slash > 0) projects.set(path.slice(0, slash), kind);
  }
  return (path) => projects.get(path) ?? "folder";
}

/** Every file in its folder, as on disk. */
export function folderModel(paths: readonly string[]): TreeModel {
  const root = buildFolders(paths);
  const iconOf = projectIcons(paths);
  return {
    rows(expanded) {
      const rows: Row[] = [];
      flattenFolder(root, expanded, 0, rows, "", iconOf);
      return rows;
    },
    holders: ancestors,
    isGroup: (key) => key !== "" && !!folderAt(root, key),
    groupsIn(key) {
      const folder = folderAt(root, key ?? "");
      const keys: string[] = [];
      // The root (path "") has no row to open.
      if (folder) eachFolder(folder, (inner) => inner.path && keys.push(inner.path));
      return keys;
    },
    collapsed: () => ({}),
  };
}

/** A part of the Solution view shown as folders: its folder tree, and where its keys start. */
interface FolderPart {
  prefix: string;
  root: string;
  tree: Folder;
  holders: string[];
}

/** Projects in solution order, F# files in compile order (see solutionLayout.ts). */
export function solutionModel(solution: SolutionTree, paths: readonly string[]): TreeModel {
  const iconOf = projectIcons(paths);
  const groupHolders = new Map<string, string[]>();
  const groups = new Map<string, Extract<TreeNode, { kind: "group" }>>();
  const fileHolders = new Map<string, string[]>();
  const parts = new Map<string, FolderPart>();
  const partOfFile = new Map<string, FolderPart>();
  /** Folder parts inside each group. */
  const partsIn = new Map<string, FolderPart[]>();

  const index = (nodes: TreeNode[], holders: string[]) => {
    for (const node of nodes) {
      if (node.kind === "file") fileHolders.set(node.key, holders);
      else if (node.kind === "group") {
        groups.set(node.key, node);
        groupHolders.set(node.key, holders);
        index(node.children, [...holders, node.key]);
      } else {
        const part: FolderPart = { prefix: `${node.key}|`, root: node.root, tree: buildFolders(node.files), holders };
        parts.set(node.key, part);
        for (const path of node.files) partOfFile.set(path, part);
        for (const holder of holders) partsIn.set(holder, [...(partsIn.get(holder) ?? []), part]);
      }
    }
  };
  index(solution.nodes, []);

  /** The folder part a key of one of its folders belongs to, with the folder's path. */
  const inPart = (key: string) => {
    const bar = key.indexOf("|");
    const part = bar < 0 ? undefined : parts.get(key.slice(0, bar));
    return part && { part, path: key.slice(bar + 1) };
  };
  const below = (path: string, root: string) => ancestors(path).filter((dir) => root === "" || dir.startsWith(`${root}/`));
  const partFolders = (part: FolderPart, from = part.root) => {
    const keys: string[] = [];
    const folder = folderAt(part.tree, from);
    if (folder) eachFolder(folder, (inner) => inner.path !== part.root && inner.path && keys.push(part.prefix + inner.path));
    return keys;
  };

  const flatten = (nodes: TreeNode[], expanded: Record<string, boolean>, depth: number, rows: Row[]) => {
    for (const node of nodes) {
      if (node.kind === "file") {
        rows.push(
          fileRow(node.key, depth, {
            label: node.label,
            detail: node.detail ?? "",
            unlisted: node.unlisted ?? "",
            note: node.unlisted ? `Not listed in ${node.unlisted}, so not compiled` : "",
          }),
        );
      } else if (node.kind === "group") {
        const open = expanded[node.key] ?? !!node.open;
        rows.push({
          kind: "folder",
          key: node.key,
          path: node.dir ?? "",
          projectFile: node.projectFile ?? "",
          label: node.label,
          detail: "",
          depth,
          open,
          icon: node.icon,
          note: node.note ?? "",
          unlisted: "",
        });
        if (open) flatten(node.children, expanded, depth + 1, rows);
      } else {
        const part = parts.get(node.key)!;
        const folder = folderAt(part.tree, part.root);
        if (folder) flattenFolder(folder, expanded, depth, rows, part.prefix, iconOf);
      }
    }
  };

  return {
    rows(expanded) {
      const rows: Row[] = [];
      flatten(solution.nodes, expanded, 0, rows);
      return rows;
    },
    holders(key) {
      const known = fileHolders.get(key) ?? groupHolders.get(key);
      if (known) return known;
      const part = partOfFile.get(key);
      if (part) return [...part.holders, ...below(key, part.root).map((dir) => part.prefix + dir)];
      const folder = inPart(key);
      if (folder) return [...folder.part.holders, ...below(folder.path, folder.part.root).map((dir) => folder.part.prefix + dir)];
      return [];
    },
    isGroup(key) {
      if (groups.has(key)) return true;
      const folder = inPart(key);
      return !!folder && folder.path !== folder.part.root && !!folderAt(folder.part.tree, folder.path);
    },
    groupsIn(key) {
      if (key === null) return [...groups.keys(), ...[...parts.values()].flatMap((part) => partFolders(part))];
      const folder = inPart(key);
      if (folder) return partFolders(folder.part, folder.path);
      const inner = [...groups.keys()].filter((group) => group === key || groupHolders.get(group)!.includes(key));
      return [...inner, ...(partsIn.get(key) ?? []).flatMap((part) => partFolders(part))];
    },
    collapsed: () => Object.fromEntries([...groups.values()].filter((group) => group.open).map((group) => [group.key, false])),
  };
}
