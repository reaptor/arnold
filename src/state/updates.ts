// Arnold's own updates: checks GitHub now and then while the app runs,
// downloads a newer version in the background and installs it when the user
// chooses to restart. The backend does the work (src-tauri/src/updates.rs).

import { createStore } from "solid-js/store";
import {
  type AvailableUpdate,
  checkForUpdate,
  downloadUpdate,
  installUpdate,
  onCheckForUpdates,
  onShowLicenses,
  updaterInfo,
} from "../ipc/commands";
import { flushSave } from "./autosave";
import { setLicensesOpen } from "./licenses";

export type UpdatePhase =
  | { kind: "idle" }
  | { kind: "checking" }
  | { kind: "upToDate" }
  /** Found, but not downloaded: a development build, or the download failed. */
  | { kind: "available"; update: AvailableUpdate }
  | { kind: "downloading"; update: AvailableUpdate; percent: number | null }
  | { kind: "ready"; update: AvailableUpdate }
  | { kind: "installing"; update: AvailableUpdate };

interface UpdatesState {
  /** The running version, once known. */
  version: string | null;
  canInstall: boolean;
  phase: UpdatePhase;
  /** When the last check finished (ms). */
  checkedAt: number | null;
  /** Why the last check, download or install failed. */
  error: string | null;
  popupOpen: boolean;
}

const [updates, setUpdates] = createStore<UpdatesState>({
  version: null,
  canInstall: false,
  phase: { kind: "idle" },
  checkedAt: null,
  error: null,
  popupOpen: false,
});

export { updates };

export const setUpdatePopupOpen = (open: boolean) => setUpdates("popupOpen", open);

/** First check this long after the start, so it does not compete with opening the repository. */
const FIRST_CHECK = 10_000;
/** How often to check again. */
const CHECK_EVERY = 60 * 60_000;
/** How often to see whether a check is due; a timer that slept through a laptop's sleep catches up. */
const TICK = 5 * 60_000;

let started = false;

/** Starts checking for updates in the background. */
export function startUpdateChecks() {
  if (started) return;
  started = true;
  void updaterInfo()
    .then((info) => setUpdates({ version: info.version, canInstall: info.canInstall }))
    .catch((error) => console.warn("updater unavailable", error));
  void onCheckForUpdates(() => {
    setUpdatePopupOpen(true);
    void checkNow();
  });
  void onShowLicenses(() => setLicensesOpen(true));
  // Development builds are not checked in the background; the popup still can.
  if (import.meta.env.DEV) return;
  window.setTimeout(() => void checkNow({ quiet: true }), FIRST_CHECK);
  window.setInterval(() => {
    if (updates.checkedAt === null || Date.now() - updates.checkedAt >= CHECK_EVERY) void checkNow({ quiet: true });
  }, TICK);
}

const busy = () => ["checking", "downloading", "installing"].includes(updates.phase.kind);

/**
 * Looks for a newer version and, when the build can install it, downloads it.
 * A failure (offline, say) leaves the status bar as it was; only the popup
 * tells. `quiet`: a background check.
 */
export async function checkNow({ quiet = false } = {}) {
  if (busy()) return;
  // Once one is downloaded, the background checks stop; asking still looks for a newer one.
  if (quiet && updates.phase.kind === "ready") return;
  const before = updates.phase;
  setUpdates({ phase: { kind: "checking" }, error: null });
  let update: AvailableUpdate | null;
  try {
    update = await checkForUpdate();
  } catch (error) {
    setUpdates({ phase: before, error: `Could not check for updates: ${error}` });
    return;
  } finally {
    setUpdates("checkedAt", Date.now());
  }
  if (!update) {
    setUpdates("phase", { kind: "upToDate" });
    return;
  }
  if (before.kind === "ready" && before.update.version === update.version) {
    setUpdates("phase", before);
    return;
  }
  if (!updates.canInstall) {
    setUpdates("phase", { kind: "available", update });
    return;
  }
  await download(update);
}

async function download(update: AvailableUpdate) {
  setUpdates({ phase: { kind: "downloading", update, percent: null }, error: null });
  try {
    await downloadUpdate((progress) => {
      const percent = progress.total ? Math.floor((progress.downloaded / progress.total) * 100) : null;
      setUpdates("phase", { kind: "downloading", update, percent });
    });
    setUpdates("phase", { kind: "ready", update });
  } catch (error) {
    setUpdates({ phase: { kind: "available", update }, error: `The download failed: ${error}` });
  }
}

/** Downloads the update a check found but could not download. */
export function retryDownload() {
  if (updates.phase.kind === "available" && updates.canInstall) void download(updates.phase.update);
}

/** Saves pending edits, then installs the downloaded update and restarts into it. */
export async function restartToUpdate() {
  if (updates.phase.kind !== "ready") return;
  const update = updates.phase.update;
  setUpdates({ phase: { kind: "installing", update }, error: null });
  try {
    await flushSave();
    await installUpdate();
  } catch (error) {
    setUpdates({ phase: { kind: "ready", update }, error: `Could not install the update: ${error}` });
  }
}
