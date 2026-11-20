// Suggesting a commit message: GitHub Copilot reads the staged changes and
// writes a short message into the commit message box, through the same
// sign-in and connection as explaining code (ai.ts).

import { createSignal } from "solid-js";
import * as ipc from "../ipc/commands";
import type { AiEvent, ContextItem } from "../ipc/commands";
import { ai, changesContext, ensureReady, refresh, setAi, setOpen } from "./ai";
import { message, setCommitMessage } from "./commitMode";
import { cleanMessage } from "./suggestedMessage";
import { state } from "./review";

/** The conversation of the suggestion being written, if any. */
const [writing, setWriting] = createSignal<string | null>(null);
/** Why the last suggestion failed; cleared by the next one. */
const [suggestError, setSuggestError] = createSignal<string | null>(null);

export { suggestError };
export const clearSuggestError = () => setSuggestError(null);
export const suggesting = () => writing() !== null;
/** Stop was clicked before Copilot had started. */
let stopped = false;

/** Whether there is something staged to suggest a message for. */
export const canSuggestMessage = () => !state.committing && !!state.checkout && (state.work?.staged.length ?? 0) > 0;

/** Suggest was clicked: suggests a message for the staged changes, or stops the one being written. */
export async function toggleSuggestion() {
  const conversation = writing();
  if (conversation) {
    stopped = true;
    await ipc.aiCancel(conversation).catch(() => {});
    return;
  }
  setSuggestError(null);
  if (!ai.status) await refresh();
  if (!ai.status?.signedIn) {
    // The AI panel signs in and starts Copilot; suggested once it is ready.
    setAi("pending", () => () => suggestCommitMessage());
    if (!ai.open) setOpen(true);
    return;
  }
  await suggestCommitMessage();
}

async function suggestCommitMessage() {
  const checkout = state.checkout;
  if (!checkout || writing() || !canSuggestMessage()) return;
  const conversation = `commit-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;
  setWriting(conversation);
  stopped = false;
  const before = message();
  let full = "";
  let frame = 0;
  // The message box shows the suggestion as it is written, once per frame.
  const show = () => {
    frame = 0;
    if (state.checkout === checkout) setCommitMessage(cleanMessage(full));
  };
  try {
    // Signed in, but Copilot may not have started yet: started without the AI panel.
    await ensureReady();
    if (stopped) return;
    const changes = await changesContext([], "staged", "Staged changes");
    if (changes.items.length === 0) throw new Error("There are no staged text changes to describe.");
    const branch = state.work?.branch;
    const context: ContextItem[] = [
      { label: "What is committed", kind: "note", text: `The staged changes${branch ? ` on branch ${branch}` : ""}.` },
      ...changes.items,
    ];
    let failed: string | null = null;
    await ipc.aiAsk(
      {
        conversation,
        model: ai.model,
        question: "Write the commit message for these staged changes.",
        context,
        cwd: checkout,
        purpose: "commitMessage",
      },
      (event: AiEvent) => {
        if (event.kind === "delta") {
          full += event.text;
          if (!frame) frame = requestAnimationFrame(show);
        } else if (event.kind === "error") failed = event.message;
      },
    );
    if (failed) throw new Error(failed);
    if (frame) cancelAnimationFrame(frame);
    frame = 0;
    const suggestion = cleanMessage(full);
    // Stopped before anything was written: the message stays as it was.
    if (state.checkout === checkout) setCommitMessage(suggestion || before);
  } catch (error) {
    if (frame) cancelAnimationFrame(frame);
    frame = 0;
    if (state.checkout === checkout) {
      setCommitMessage(before);
      setSuggestError(`Could not suggest a message: ${error instanceof Error ? error.message : String(error)}`);
    }
  } finally {
    setWriting(null);
    void ipc.aiEnd(conversation).catch(() => {});
  }
}
