// What the app is busy with: slow backend calls (fetching, checking out,
// talking to Azure DevOps…) show in the status bar while they run, so a
// click that takes a while is seen to work. The button or row that was
// clicked shows a spinner too, and ignores more clicks until its work is
// done. Calls that end quickly never show, and once shown the label stays a
// moment after the last call ends, so calls made one after another do not
// flicker.

import { createSignal } from "solid-js";

/** How long a call runs before it shows. */
const SHOW_AFTER_MS = 250;

/** How long the label stays after the last call ends, in case another starts. */
const HIDE_AFTER_MS = 300;

/** How long the label stays at least, once shown. */
const MIN_SHOWN_MS = 600;

/** How soon after a click a call counts as started by it. */
const CLICK_STARTS_MS = 500;

/** How long a control stays busy after its last call ends, so calls made one after another do not flicker. */
const SETTLE_MS = 80;

/** What a click can start: the spinner goes on the nearest of these. */
const CONTROL = 'button, li, [role="treeitem"]';

interface Activity {
  id: number;
  label: string;
}

const [running, setRunning] = createSignal<readonly Activity[]>([]);
/** What was running last, kept while the label lingers after it ended. */
const [lastShown, setLastShown] = createSignal<readonly Activity[]>([]);
const [shown, setShown] = createSignal(false);
let shownAt = 0;
let hideTimer: ReturnType<typeof setTimeout> | undefined;
let nextId = 1;

function setActivities(list: readonly Activity[]) {
  setRunning(list);
  if (list.length > 0) {
    clearTimeout(hideTimer);
    hideTimer = undefined;
    setLastShown(list);
    if (!shown()) {
      shownAt = performance.now();
      setShown(true);
    }
  } else if (shown() && hideTimer === undefined) {
    const wait = Math.max(HIDE_AFTER_MS, shownAt + MIN_SHOWN_MS - performance.now());
    hideTimer = setTimeout(() => {
      hideTimer = undefined;
      setShown(false);
    }, wait);
  }
}

let lastClick: { control: HTMLElement; at: number } | null = null;

/** Calls running for each control they were started from. */
const held = new WeakMap<HTMLElement, number>();

/**
 * Notes which control each click lands on, so a call it starts can show on
 * it. Listens in the capture phase, before the control's own handler runs.
 */
export function followClicks(): () => void {
  const onClick = (event: MouseEvent) => {
    const control = event.target instanceof Element ? event.target.closest<HTMLElement>(CONTROL) : null;
    lastClick = control ? { control, at: performance.now() } : null;
  };
  window.addEventListener("click", onClick, true);
  return () => window.removeEventListener("click", onClick, true);
}

/** The control a call starting now was started from: one just clicked, or one still busy with an earlier call. */
function startedFrom(): HTMLElement | null {
  if (!lastClick?.control.isConnected) return null;
  const { control, at } = lastClick;
  return performance.now() - at < CLICK_STARTS_MS || (held.get(control) ?? 0) > 0 ? control : null;
}

function showWorking(control: HTMLElement) {
  if (control.dataset.working !== undefined || !control.isConnected) return;
  const style = getComputedStyle(control);
  // The spinner takes the control's own color, which is hidden under it.
  control.style.setProperty("--working-color", style.color);
  control.dataset.working = style.position === "static" ? "anchor" : "";
  control.setAttribute("aria-busy", "true");
}

function release(control: HTMLElement) {
  const count = (held.get(control) ?? 1) - 1;
  held.set(control, count);
  if (count > 0) return;
  setTimeout(() => {
    if ((held.get(control) ?? 0) > 0) return;
    held.delete(control);
    delete control.dataset.working;
    control.style.removeProperty("--working-color");
    control.removeAttribute("aria-busy");
  }, SETTLE_MS);
}

/**
 * Shows `label` (e.g. "Fetching") while `work` runs, once it has run for a
 * moment, or at once while the label is still up from an earlier call.
 */
export async function track<T>(label: string, work: Promise<T>): Promise<T> {
  const activity = { id: nextId++, label };
  const control = startedFrom();
  if (control) held.set(control, (held.get(control) ?? 0) + 1);
  const reveal = setTimeout(
    () => {
      setActivities([...running(), activity]);
      if (control) showWorking(control);
    },
    shown() ? 0 : SHOW_AFTER_MS,
  );
  try {
    return await work;
  } finally {
    clearTimeout(reveal);
    setActivities(running().filter((item) => item !== activity));
    if (control) release(control);
  }
}

/** What the app is busy with, the latest first; empty when idle. */
export const activities = () => (shown() ? [...(running().length > 0 ? running() : lastShown())].reverse() : []);

export const isBusy = shown;
