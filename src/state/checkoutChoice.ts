/**
 * Checking a branch or pull request out in the opened folder, the default,
 * and asking what to do when that folder has uncommitted changes: check out
 * in a worktree of its own instead (the changes stay untouched), stash the
 * changes, or throw them away and check out in the folder.
 */

import { createSignal } from "solid-js";
import type { CheckedOut, Placement } from "../ipc/commands";
import { flushSave } from "./autosave";

/** What the Uncommitted changes dialog asks about. */
export interface CheckoutQuestion {
  /** What is being checked out: `feature/x`, `pull request !12`. */
  what: string;
  /** The opened folder. */
  folder: string;
  /** Uncommitted changes there, `untracked` of them new files. */
  files: number;
  untracked: number;
  /** What Cancel leaves: "Nothing is checked out". */
  cancelNote: string;
}

const [checkoutQuestion, setCheckoutQuestion] = createSignal<CheckoutQuestion | null>(null);
/** Set while the Uncommitted changes dialog is open. */
export { checkoutQuestion };

let answer: ((placement: Placement | null) => void) | null = null;

/** The answer to the Uncommitted changes dialog: where to check out, or null to cancel. */
export function answerCheckout(placement: Placement | null) {
  const resolve = answer;
  answer = null;
  setCheckoutQuestion(null);
  resolve?.(placement);
}

/**
 * Checks `question.what` out with `attempt`: in the opened folder, or, when
 * that has uncommitted changes, where the user picks (`stashed` says the
 * changes were stashed). Null when they cancel.
 */
export async function checkOutAsking<T extends object>(
  question: Omit<CheckoutQuestion, "files" | "untracked">,
  attempt: (placement: Placement) => Promise<CheckedOut<T>>,
): Promise<(T & { stashed: boolean }) | null> {
  // Edits on their way to disk count as uncommitted changes.
  await flushSave();
  let placement: Placement = "here";
  for (;;) {
    const result = await attempt(placement);
    if (result.kind === "done") return { ...result, stashed: placement === "stash" };
    // Only `here` is refused; asked again if the folder changed meanwhile.
    answerCheckout(null);
    const picked = await new Promise<Placement | null>((resolve) => {
      answer = resolve;
      setCheckoutQuestion({ ...question, files: result.files, untracked: result.untracked });
    });
    if (!picked) return null;
    placement = picked;
  }
}
