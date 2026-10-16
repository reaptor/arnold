// Saving edits to a checkout's files once typing pauses: Commit mode's
// editable diffs and those of a checked-out branch alike. Saves run one
// after the other, and a file being saved is not re-read from disk under
// the editor, which may already hold newer text.

import * as ipc from "../ipc/commands";

/** How long typing pauses before an edit is saved to disk. */
const SAVE_DELAY = 400;

interface Save {
  checkout: string;
  path: string;
  text: string;
  failed: (error: unknown) => void;
}

let pending: Save | null = null;
let saveTimer: number | undefined;
/** The last save; saves run one after the other. */
let saving: Promise<void> = Promise.resolve();
/** Files whose save has started but not finished, by path. */
const writing = new Map<string, number>();
/** Saves started so far. */
let started = 0;

/** Counts the saves started: a read from disk that a save may have overtaken is not trusted over the editor. */
export const saveCount = () => started;

/** `text` is the new content of `path` in `checkout`: it is saved once typing pauses. */
export function queueSave(checkout: string, path: string, text: string, failed: (error: unknown) => void) {
  if (pending && (pending.path !== path || pending.checkout !== checkout)) void flushSave();
  pending = { checkout, path, text, failed };
  window.clearTimeout(saveTimer);
  saveTimer = window.setTimeout(() => void flushSave(), SAVE_DELAY);
}

/** Saves an edit still waiting for typing to pause. Resolves once every save so far is done. */
export function flushSave(): Promise<void> {
  window.clearTimeout(saveTimer);
  const job = pending;
  pending = null;
  if (job) {
    started += 1;
    writing.set(job.path, (writing.get(job.path) ?? 0) + 1);
    saving = saving
      .then(() => ipc.saveWorktreeFile(job.checkout, job.path, job.text))
      .catch(job.failed)
      .finally(() => {
        const left = (writing.get(job.path) ?? 1) - 1;
        if (left > 0) writing.set(job.path, left);
        else writing.delete(job.path);
      });
  }
  return saving;
}

/** Resolves once the saves started so far are done, leaving an edit still waiting for typing to pause. */
export const saved = (): Promise<void> => saving;

/** Forgets an edit of `path` that has not been saved yet (the file is being put back). */
export function dropPendingSave(path: string) {
  if (pending?.path !== path) return;
  pending = null;
  window.clearTimeout(saveTimer);
}

/** Whether `path` has an edit waiting to be saved. */
export const hasPendingSave = (path: string) => pending?.path === path;

/**
 * Whether what is on disk for `path` may be older than its editor: an edit
 * waits to be saved, or is being saved.
 */
export const isSaving = (path: string) => pending?.path === path || writing.has(path);
