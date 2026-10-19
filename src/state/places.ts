// Where the user was in each review, kept between app starts: the file shown
// on its own with its cursor and scroll, and how far the all-files view was
// scrolled. A review is the repository's Commit mode in one checkout, a pair
// of branches, or a pull request.
//
// Saving waits while a review is being opened (`hold` until `ready`): until
// its place is restored the views move by themselves, and that is not where
// the user was.

/** The file shown on its own: its key in the dock, the cursor (text offsets) and the editor's scroll. */
export interface FilePlace {
  key: string;
  anchor: number;
  head: number;
  scrollTop: number;
}

/** The all-files view: the file at the top of the list, and how far into it the list is scrolled. */
export interface ListPlace {
  key: string;
  offset: number;
}

export interface Place {
  /** The view the user was in: the file on its own, or the all-files list. */
  view?: "file" | "all";
  file?: FilePlace;
  list?: ListPlace;
}

/** Reviews kept per repository, the most recent ones. */
const KEPT = 50;
/** Moving on saves this long after it stops. */
const SAVE_DELAY = 400;

const storageKey = (root: string) => `arnold.places.${root}`;

type Saved = Record<string, Place & { at: number }>;

function read(root: string): Saved {
  try {
    const saved = JSON.parse(localStorage.getItem(storageKey(root)) ?? "{}");
    return saved && typeof saved === "object" ? saved : {};
  } catch {
    return {};
  }
}

/** Where the user was in `review` of the repository at `root`. */
export function placeIn(root: string, review: string): Place | null {
  const place = read(root)[review];
  return place && typeof place === "object" ? place : null;
}

function write(root: string, review: string, change: Place) {
  const saved = read(root);
  saved[review] = { ...saved[review], ...change, at: Date.now() };
  const kept = Object.entries(saved)
    .sort(([, a], [, b]) => (b.at ?? 0) - (a.at ?? 0))
    .slice(0, KEPT);
  try {
    localStorage.setItem(storageKey(root), JSON.stringify(Object.fromEntries(kept)));
  } catch {
    // Storage full or unavailable: the place is just not kept.
  }
}

let ready = false;
let started = false;

/**
 * Whether this is the first review opened since the app started (asked once):
 * it opens in the view the app was closed in. Opening a review later shows
 * all files, as it always has.
 */
export function firstReview(): boolean {
  const first = !started;
  started = true;
  return first;
}

let unsaved: { root: string; review: string; change: Place } | null = null;
let timer = 0;

/** Notes where the user is now; saved once moving stops. */
export function notePlace(root: string, review: string, change: Place) {
  if (!ready) return;
  if (unsaved && (unsaved.root !== root || unsaved.review !== review)) flushPlaces();
  unsaved = { root, review, change: { ...unsaved?.change, ...change } };
  clearTimeout(timer);
  timer = window.setTimeout(flushPlaces, SAVE_DELAY);
}

/** Saves a noted place now. */
export function flushPlaces() {
  clearTimeout(timer);
  if (unsaved) write(unsaved.root, unsaved.review, unsaved.change);
  unsaved = null;
}

/** A review is being opened: what the views do meanwhile is not saved. */
export function holdPlaces() {
  flushPlaces();
  ready = false;
}

/** The review's place is restored (or it had none): moving saves again. */
export function placesReady() {
  ready = true;
}

window.addEventListener("pagehide", flushPlaces);
window.addEventListener("beforeunload", flushPlaces);
