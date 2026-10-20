// What the app is busy with: slow backend calls (fetching, checking out,
// talking to Azure DevOps…) show in the status bar and as a bar under the
// toolbar while they run, so a click that takes a while is seen to work.
// Calls that end quickly never show, so fast ones do not flicker.

import { createSignal } from "solid-js";

/** How long a call runs before it shows. */
const SHOW_AFTER_MS = 250;

interface Activity {
  id: number;
  label: string;
}

const [running, setRunning] = createSignal<readonly Activity[]>([]);
let nextId = 1;

/** Shows `label` (e.g. "Fetching") while `work` runs, once it has run for a moment. */
export async function track<T>(label: string, work: Promise<T>): Promise<T> {
  const activity = { id: nextId++, label };
  const shown = setTimeout(() => setRunning((list) => [...list, activity]), SHOW_AFTER_MS);
  try {
    return await work;
  } finally {
    clearTimeout(shown);
    setRunning((list) => list.filter((item) => item !== activity));
  }
}

/** What the app is busy with, the latest first; empty when idle. */
export const activities = () => [...running()].reverse();

export const isBusy = () => running().length > 0;
