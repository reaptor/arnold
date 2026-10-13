// Conversions between file-system paths and the `file://` URIs used by LSP.

const DRIVE = /^[A-Za-z]:$/;

const slashes = (path: string) => path.replace(/\\/g, "/");

/** `/work/my repo/A.fs` → `file:///work/my%20repo/A.fs`; `C:\x\A.fs` → `file:///C:/x/A.fs`. */
export function pathToUri(path: string): string {
  let normal = slashes(path);
  if (/^[A-Za-z]:/.test(normal)) normal = "/" + normal[0].toUpperCase() + normal.slice(1);
  const encoded = normal
    .split("/")
    .map((segment) => (DRIVE.test(segment) ? segment : encodeURIComponent(segment)))
    .join("/");
  return "file://" + encoded;
}

/** The path a `file://` URI points at (forward slashes), or null for other URIs. */
export function uriToPath(uri: string): string | null {
  if (!uri.startsWith("file://")) return null;
  let path: string;
  try {
    path = decodeURIComponent(uri.slice("file://".length));
  } catch {
    return null;
  }
  // `/c:/x` → `C:/x`
  if (/^\/[A-Za-z]:/.test(path)) path = path[1].toUpperCase() + path.slice(2);
  return path;
}

/**
 * Servers spell URIs in their own way (`file:///c%3A/x` and `file:///C:/x` are
 * the same file). This gives every spelling of a file URI the form `pathToUri`
 * produces, so that URIs can be compared as strings.
 */
export function canonicalUri(uri: string): string {
  const path = uriToPath(uri);
  return path === null ? uri : pathToUri(path);
}

/** `path` relative to the folder `root`, or null if it is not inside it. */
export function relativeTo(root: string, path: string): string | null {
  const folder = slashes(root).replace(/\/+$/, "") + "/";
  const target = slashes(path);
  // Windows paths compare without regard to case.
  const windows = /^[A-Za-z]:/.test(folder);
  const inside = windows
    ? target.toLowerCase().startsWith(folder.toLowerCase())
    : target.startsWith(folder);
  return inside ? target.slice(folder.length) : null;
}

/** Joins a checkout folder and a repository-relative path. */
export const joinPath = (root: string, relative: string) => slashes(root).replace(/\/+$/, "") + "/" + relative;
