// The explorer's Solution view, as Rider shows a .NET solution: its projects
// in the order of the solution file, and the files of an F# project in the
// order of the project file (its compile order).

import { comparePaths } from "./pathOrder";

const SOLUTION = /\.slnx?$/i;
const PROJECT = /\.(fs|cs|vb)proj$/i;
/** Projects that list their files, in an order that matters. */
const ORDERED = /\.fsproj$/i;
/** F# files a project would have to list to compile them. */
const FSHARP_SOURCE = /\.(fs|fsi|fsx)$/i;
/** Build output, not part of a globbing project. */
const BUILD_OUTPUT = /(^|\/)(bin|obj)\//i;
/** Item types that name other things than files of the project. */
const NOT_FILES = new Set(["ProjectReference", "PackageReference", "PackageVersion", "Reference", "FrameworkReference", "Analyzer", "Using", "InternalsVisibleTo"]);
/** Project type of a solution folder in a .sln file. */
const SOLUTION_FOLDER_TYPE = "2150E333-8FDC-42A3-9474-1A3956D46DE8";

export type SolutionEntry =
  | { kind: "folder"; name: string; entries: SolutionEntry[]; files: string[] }
  | { kind: "project"; path: string };

/** A file a project lists, with where it shows in the project. */
export interface ProjectItem {
  path: string;
  /** Path inside the project: its `Link`, or its path from the project's folder. */
  shownAs: string;
}

export type GroupIcon = "solution" | "solutionFolder" | "fsharpProject" | "csharpProject" | "project" | "folder";

/** A part of the Solution view. */
export type TreeNode =
  | {
      kind: "group";
      /** Unique among the rows, never a file's path. */
      key: string;
      label: string;
      icon: GroupIcon;
      /** The real folder it stands for, if any (for its context menu). */
      dir: string | null;
      children: TreeNode[];
      /** Open until closed by hand. */
      open?: boolean;
      note?: string;
    }
  | {
      kind: "file";
      /** The file's path: each file shows once. */
      key: string;
      label: string;
      /** Folder shown after the name, for a file outside its folder's group. */
      detail?: string;
      /** Not listed in the project it sits in. */
      unlisted?: string;
    }
  /** Files shown as the folders they are in, from `root` down, as in the Folders view. */
  | { kind: "folders"; key: string; root: string; files: string[] };

export interface SolutionTree {
  nodes: TreeNode[];
  /** Position of each file in the view; files of one `folders` part share theirs. */
  rank: ReadonlyMap<string, number>;
  /** Where files outside the view go: after the rest. */
  otherRank: number;
}

const directoryOf = (path: string) => path.slice(0, Math.max(path.lastIndexOf("/"), 0));
const nameOf = (path: string) => path.slice(path.lastIndexOf("/") + 1);
const withoutExtension = (name: string) => name.replace(/\.[^.]*$/, "");
const isUnder = (path: string, dir: string) => dir === "" || path.startsWith(`${dir}/`);

/** `relative` (backslashes allowed) from the folder `dir`, as a path from the repository's top; null above it. */
export function resolvePath(dir: string, relative: string): string | null {
  const parts = dir ? dir.split("/") : [];
  for (const part of relative.replace(/\\/g, "/").split("/")) {
    if (part === "" || part === ".") continue;
    if (part === "..") {
      if (parts.length === 0) return null;
      parts.pop();
    } else parts.push(part);
  }
  return parts.join("/");
}

/** The solution the view shows: the one nearest the top of the repository. */
export function mainSolution(files: readonly string[]): string | null {
  const depth = (path: string) => path.split("/").length;
  const solutions = files.filter((path) => SOLUTION.test(path));
  solutions.sort((a, b) => depth(a) - depth(b) || comparePaths(a, b));
  return solutions[0] ?? null;
}

export const isProjectFile = (path: string) => PROJECT.test(path);

/** Whether the files hold something for the Solution view to show. */
export const hasSolution = (files: readonly string[]) => files.some((path) => SOLUTION.test(path) || PROJECT.test(path));

const attribute = (attributes: string, name: string) =>
  new RegExp(`\\b${name}\\s*=\\s*"([^"]*)"`, "i").exec(attributes)?.[1] ?? null;

const withoutComments = (text: string) => text.replace(/<!--[\s\S]*?-->/g, "");

/** The folders and projects of a solution file (.sln or .slnx), in its order. */
export function parseSolution(path: string, text: string): SolutionEntry[] {
  return /\.slnx$/i.test(path) ? parseSlnx(directoryOf(path), text) : parseSln(directoryOf(path), text);
}

function parseSln(dir: string, text: string): SolutionEntry[] {
  const byId = new Map<string, SolutionEntry>();
  const order: string[] = [];
  const parents = new Map<string, string>();
  let current: Extract<SolutionEntry, { kind: "folder" }> | null = null;
  let section: "items" | "nested" | null = null;
  for (const raw of text.split(/\r?\n/)) {
    const line = raw.trim();
    const project = /^Project\("\{([^}]+)\}"\)\s*=\s*"([^"]*)"\s*,\s*"([^"]*)"\s*,\s*"\{([^}]+)\}"/.exec(line);
    if (project) {
      const [, type, name, relative, id] = project;
      let entry: SolutionEntry | null = null;
      if (type.toUpperCase() === SOLUTION_FOLDER_TYPE) entry = current = { kind: "folder", name, entries: [], files: [] };
      else {
        current = null;
        const resolved = resolvePath(dir, relative);
        if (resolved !== null) entry = { kind: "project", path: resolved };
      }
      if (entry) {
        byId.set(id.toUpperCase(), entry);
        order.push(id.toUpperCase());
      }
    } else if (line === "EndProject") current = null;
    else if (/^ProjectSection\(SolutionItems\)/.test(line)) section = "items";
    else if (/^GlobalSection\(NestedProjects\)/.test(line)) section = "nested";
    else if (/^End(Project|Global)Section$/.test(line)) section = null;
    else if (section === "items" && current) {
      const item = /^(.+?)\s*=/.exec(line)?.[1];
      const resolved = item === undefined ? null : resolvePath(dir, item);
      if (resolved !== null) current.files.push(resolved);
    } else if (section === "nested") {
      const nested = /^\{([^}]+)\}\s*=\s*\{([^}]+)\}/.exec(line);
      if (nested) parents.set(nested[1].toUpperCase(), nested[2].toUpperCase());
    }
  }
  const top: SolutionEntry[] = [];
  for (const id of order) {
    const entry = byId.get(id)!;
    const parent = byId.get(parents.get(id) ?? "");
    if (parent?.kind === "folder") parent.entries.push(entry);
    else top.push(entry);
  }
  return top;
}

function parseSlnx(dir: string, text: string): SolutionEntry[] {
  const top: SolutionEntry[] = [];
  const folders = new Map<string, Extract<SolutionEntry, { kind: "folder" }>>();
  /** The folder named "/a/b/", made along with the folders above it. */
  const folderNamed = (name: string) => {
    const parts = name.split("/").filter(Boolean);
    let entries = top;
    let folder: Extract<SolutionEntry, { kind: "folder" }> | null = null;
    for (let index = 0; index < parts.length; index++) {
      const key = parts.slice(0, index + 1).join("/");
      let found = folders.get(key);
      if (!found) {
        found = { kind: "folder", name: parts[index], entries: [], files: [] };
        folders.set(key, found);
        entries.push(found);
      }
      folder = found;
      entries = found.entries;
    }
    return folder;
  };
  let current: Extract<SolutionEntry, { kind: "folder" }> | null = null;
  for (const tag of withoutComments(text).matchAll(/<(\/?)(Folder|Project|File)\b([^>]*?)\/?>/gi)) {
    const [, closing, element, attributes] = tag;
    const kind = element.toLowerCase();
    if (kind === "folder") {
      current = closing ? null : folderNamed(attribute(attributes, "Name") ?? "");
      continue;
    }
    if (closing) continue;
    const path = attribute(attributes, "Path");
    const resolved = path === null ? null : resolvePath(dir, path);
    if (resolved === null) continue;
    if (kind === "project") (current?.entries ?? top).push({ kind: "project", path: resolved });
    else current?.files.push(resolved);
  }
  return top;
}

/** Glob of an item (`*`, `**`, `?`) as a pattern over paths from the repository's top. */
function globPattern(pattern: string): RegExp {
  let source = "";
  for (let index = 0; index < pattern.length; index++) {
    const char = pattern[index];
    if (char === "*" && pattern[index + 1] === "*") {
      index++;
      if (pattern[index + 1] === "/") index++;
      source += "(?:.*/)?";
      if (pattern[index + 1] === undefined) source += "[^/]*";
    } else if (char === "*") source += "[^/]*";
    else if (char === "?") source += "[^/]";
    else source += char.replace(/[.+^${}()|[\]\\]/g, "\\$&");
  }
  return new RegExp(`^${source}$`, "i");
}

/**
 * The files a project file lists, in its order, kept to those in `files`
 * (each once). Items that use properties are skipped.
 */
export function parseProject(path: string, text: string, files: readonly string[]): ProjectItem[] {
  const dir = directoryOf(path);
  const known = new Set(files);
  const items: ProjectItem[] = [];
  const seen = new Set<string>();
  const add = (file: string, link: string | null) => {
    if (seen.has(file) || file === path) return;
    seen.add(file);
    const shownAs = link !== null ? link.replace(/\\/g, "/") : isUnder(file, dir) ? file.slice(dir ? dir.length + 1 : 0) : nameOf(file);
    items.push({ path: file, shownAs });
  };
  const body = withoutComments(text);
  const tags = /<([A-Za-z_][\w.-]*)\s([^>]*?)(\/?)>/g;
  for (let tag = tags.exec(body); tag; tag = tags.exec(body)) {
    const [whole, element, attributes, selfClosing] = tag;
    const include = attribute(attributes, "Include");
    if (include === null || NOT_FILES.has(element)) continue;
    let link = attribute(attributes, "Link");
    if (!selfClosing) {
      const end = body.indexOf(`</${element}>`, tag.index + whole.length);
      const inner = end < 0 ? "" : body.slice(tag.index + whole.length, end);
      link ??= /<Link>\s*([^<]*?)\s*<\/Link>/i.exec(inner)?.[1] ?? null;
    }
    for (const value of include.split(";")) {
      const relative = value.trim().replace(/^\$\(MSBuildThisFileDirectory\)/i, "");
      if (!relative || relative.includes("$(") || relative.includes("@(")) continue;
      if (/[*?]/.test(relative)) {
        const pattern = resolvePath(dir, relative);
        if (pattern === null) continue;
        const matcher = globPattern(pattern);
        for (const file of files.filter((file) => matcher.test(file)).sort(comparePaths)) add(file, null);
      } else {
        const file = resolvePath(dir, relative);
        if (file !== null && known.has(file)) add(file, link);
      }
    }
  }
  return items;
}

/** The project files the view needs to read: those of the solution, or every one without a solution. */
export function projectsOf(files: readonly string[], solution: SolutionEntry[] | null): string[] {
  if (!solution) return files.filter((path) => PROJECT.test(path)).sort(comparePaths);
  const known = new Set(files);
  const projects: string[] = [];
  const visit = (entries: SolutionEntry[]) => {
    for (const entry of entries) {
      if (entry.kind === "folder") visit(entry.entries);
      else if (known.has(entry.path) && PROJECT.test(entry.path)) projects.push(entry.path);
    }
  };
  visit(solution);
  return projects;
}

const iconOf = (project: string): GroupIcon =>
  /\.fsproj$/i.test(project) ? "fsharpProject" : /\.csproj$/i.test(project) ? "csharpProject" : "project";

/**
 * The Solution view of `files`: the solution (or, without one, every
 * project), then the files outside its projects. `texts` holds the solution's
 * and the projects' texts.
 */
export function buildSolutionTree(
  files: readonly string[],
  solutionPath: string | null,
  texts: ReadonlyMap<string, string | null>,
): SolutionTree {
  const solutionText = solutionPath === null ? null : (texts.get(solutionPath) ?? null);
  const solution = solutionPath !== null && solutionText !== null ? parseSolution(solutionPath, solutionText) : null;
  const projects = projectsOf(files, solution);
  const listed = new Map(
    projects.filter((project) => ORDERED.test(project)).map((project) => [project, parseProject(project, texts.get(project) ?? "", files)]),
  );

  // Files listed by name are claimed first, in the order they show.
  const claimed = new Set<string>(solutionPath ? [solutionPath] : []);
  const claim = (path: string) => !claimed.has(path) && (claimed.add(path), true);
  const projectFiles = new Map<string, ProjectItem[]>();
  const solutionItems = new Map<Extract<SolutionEntry, { kind: "folder" }>, string[]>();
  const known = new Set(files);
  const claimEntries = (entries: SolutionEntry[]) => {
    for (const entry of entries) {
      if (entry.kind === "folder") {
        claimEntries(entry.entries);
        solutionItems.set(entry, entry.files.filter((file) => known.has(file) && claim(file)));
      } else if (known.has(entry.path)) claimProject(entry.path);
    }
  };
  const claimProject = (project: string) => {
    if (!claim(project)) return;
    projectFiles.set(project, (listed.get(project) ?? []).filter((item) => claim(item.path)));
  };
  if (solution) claimEntries(solution);
  else projects.forEach(claimProject);

  // The rest belong to the project in the deepest folder holding them.
  const projectDirs = [...projectFiles.keys()].map((project) => ({ project, dir: directoryOf(project) }));
  projectDirs.sort((a, b) => b.dir.length - a.dir.length);
  const unlisted = new Map<string, string[]>();
  const globbed = new Map<string, string[]>();
  const other: string[] = [];
  for (const path of files) {
    if (claimed.has(path)) continue;
    const home = projectDirs.find(({ dir }) => isUnder(path, dir))?.project;
    if (home && ORDERED.test(home) && FSHARP_SOURCE.test(path)) unlisted.set(home, [...(unlisted.get(home) ?? []), path]);
    else if (home && !ORDERED.test(home) && !BUILD_OUTPUT.test(path.slice(directoryOf(home).length))) {
      globbed.set(home, [...(globbed.get(home) ?? []), path]);
    } else other.push(path);
  }

  let groups = 0;
  const groupKey = (kind: string) => `\u0000${kind}:${groups++}`;
  const fileNode = (path: string, extra: Partial<Extract<TreeNode, { kind: "file" }>> = {}): TreeNode => ({
    kind: "file",
    key: path,
    label: nameOf(path),
    ...extra,
  });

  const projectNode = (project: string): TreeNode => {
    const dir = directoryOf(project);
    const children: TreeNode[] = [fileNode(project)];
    const projectName = nameOf(project);
    // Folders show where their files are listed: a folder whose files are
    // listed apart shows more than once, as in Rider.
    for (const item of projectFiles.get(project) ?? []) {
      const parts = item.shownAs.split("/").filter(Boolean);
      let container = children;
      for (let index = 0; index < parts.length - 1; index++) {
        const last = container[container.length - 1];
        if (last?.kind === "group" && last.label === parts[index]) {
          container = last.children;
          continue;
        }
        const real = resolvePath(dir, parts.slice(0, index + 1).join("/"));
        const folder: TreeNode = {
          kind: "group",
          key: groupKey("folder"),
          label: parts[index],
          icon: "folder",
          dir: real !== null && isUnder(item.path, real) ? real : null,
          children: [],
        };
        container.push(folder);
        container = folder.children;
      }
      const shownIn = directoryOf(item.shownAs);
      const actualIn = isUnder(item.path, dir) ? directoryOf(item.path.slice(dir ? dir.length + 1 : 0)) : null;
      container.push(fileNode(item.path, shownIn === actualIn ? {} : { detail: directoryOf(item.path) + "/" }));
    }
    for (const path of (unlisted.get(project) ?? []).sort(comparePaths)) {
      const inside = directoryOf(path.slice(dir ? dir.length + 1 : 0));
      children.push(fileNode(path, { unlisted: projectName, detail: inside ? `${inside}/` : undefined }));
    }
    const rest = globbed.get(project);
    if (rest) children.push({ kind: "folders", key: groupKey("files"), root: dir, files: rest });
    return {
      kind: "group",
      key: groupKey("project"),
      label: withoutExtension(projectName),
      icon: iconOf(project),
      dir: dir || null,
      children,
      note: project,
    };
  };

  const entryNodes = (entries: SolutionEntry[]): TreeNode[] =>
    entries.flatMap((entry): TreeNode[] => {
      if (entry.kind === "project") return projectFiles.has(entry.path) ? [projectNode(entry.path)] : [];
      return [
        {
          kind: "group",
          key: groupKey("solutionFolder"),
          label: entry.name,
          icon: "solutionFolder",
          dir: null,
          children: [...entryNodes(entry.entries), ...(solutionItems.get(entry) ?? []).map((path) => fileNode(path))],
        },
      ];
    });

  const nodes: TreeNode[] = [];
  if (solution && solutionPath) {
    nodes.push({
      kind: "group",
      key: groupKey("solution"),
      label: withoutExtension(nameOf(solutionPath)),
      icon: "solution",
      dir: null,
      children: [fileNode(solutionPath), ...entryNodes(solution)],
      open: true,
      note: solutionPath,
    });
  } else {
    nodes.push(...[...projectFiles.keys()].map(projectNode));
  }
  if (other.length > 0) {
    nodes.push({
      kind: "group",
      key: groupKey("other"),
      label: "Files outside projects",
      icon: "folder",
      dir: null,
      children: [{ kind: "folders", key: groupKey("files"), root: "", files: other }],
    });
  }

  const rank = new Map<string, number>();
  let next = 0;
  const visit = (node: TreeNode) => {
    if (node.kind === "file") rank.set(node.key, next++);
    else if (node.kind === "group") node.children.forEach(visit);
    else {
      const shared = next++;
      for (const path of node.files) rank.set(path, shared);
    }
  };
  nodes.forEach(visit);
  return { nodes, rank, otherRank: other.length > 0 ? rank.get(other[0])! : next };
}

/** Orders paths as the Solution view shows them; paths it lacks go with the files outside projects. */
export function solutionOrder(tree: SolutionTree): (a: string, b: string) => number {
  const rankOf = (path: string) => tree.rank.get(path) ?? tree.otherRank;
  return (a, b) => rankOf(a) - rankOf(b) || comparePaths(a, b);
}
