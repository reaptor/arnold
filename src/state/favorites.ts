import { createSignal } from "solid-js";

// Starred repositories, kept as the root paths `open_repo` reported.

const KEY = "arnold.favorites";

function load(): string[] {
  try {
    const stored = JSON.parse(localStorage.getItem(KEY) ?? "[]");
    return Array.isArray(stored) ? stored.filter((path) => typeof path === "string") : [];
  } catch {
    return [];
  }
}

const [favorites, setFavorites] = createSignal(load());

export { favorites };

export const folderName = (path: string) => path.split(/[\\/]/).filter(Boolean).pop() ?? path;

/** The favorites sorted by folder name, then by full path for same-named folders. */
export const sortFavorites = (paths: readonly string[]) =>
  [...paths].sort(
    (a, b) =>
      folderName(a).localeCompare(folderName(b), undefined, { sensitivity: "base", numeric: true }) ||
      a.localeCompare(b),
  );

export const isFavorite = (path: string) => favorites().includes(path);

export function toggleFavorite(path: string) {
  const next = isFavorite(path) ? favorites().filter((other) => other !== path) : [...favorites(), path];
  setFavorites(next);
  localStorage.setItem(KEY, JSON.stringify(next));
}
