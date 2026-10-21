import { createEffect, createSignal, on, onMount, Show } from "solid-js";
import { answerCheckout, checkoutQuestion } from "../state/checkoutChoice";
import { BackIcon, CloseIcon, StashIcon, TrashIcon, WorktreeIcon } from "./icons";

const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;
const folderName = (path: string) => path.split(/[\\/]/).filter(Boolean).pop() ?? path;

/**
 * Checking out in the opened folder while it has uncommitted changes: in a
 * worktree instead, leaving them as they are, stashing them and checking
 * out in the folder, or throwing them away and checking out in the folder,
 * which asks once more, with the dangerous button in red.
 */
export function CheckoutDialog() {
  const asked = () => checkoutQuestion()!;
  const cancel = () => answerCheckout(null);
  // Discarding asks once more, in the dialog; Escape and Back return to the choices.
  const [confirming, setConfirming] = createSignal(false);
  const back = () => (confirming() ? setConfirming(false) : cancel());
  let first: HTMLButtonElement | undefined;
  let keep: HTMLButtonElement | undefined;
  onMount(() => first?.focus());
  createEffect(on(confirming, (now) => (now ? keep : first)?.focus(), { defer: true }));
  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && back()}
      onKeyDown={(event) => event.key === "Escape" && back()}
    >
      <Show
        when={!confirming()}
        fallback={
          <div class="modal complete-dialog checkout-dialog" role="alertdialog" aria-label="Discard all changes">
            <h2>
              <TrashIcon class="danger-icon" />
              Discard {plural(asked().files, "uncommitted change")}?
            </h2>
            <p>
              Every uncommitted change in <span class="checkout-folder">{asked().folder}</span>
              <Show when={asked().untracked > 0}>, {plural(asked().untracked, "new file")} included,</Show> is thrown away
              for good before {asked().what} is checked out there. Git keeps no copy: this cannot be undone.
            </p>
            <div class="committed-choices">
              <button ref={keep} class="committed-choice" onClick={() => setConfirming(false)}>
                <BackIcon />
                <span>
                  Back
                  <small>Keep the changes and pick another way</small>
                </span>
              </button>
              <button class="committed-choice danger" onClick={() => answerCheckout("discard")}>
                <TrashIcon />
                <span>
                  Discard changes and check out {asked().what}
                  <small>The changes are gone for good</small>
                </span>
              </button>
            </div>
          </div>
        }
      >
        <div class="modal complete-dialog checkout-dialog" role="dialog" aria-label="Uncommitted changes">
          <h2>{folderName(asked().folder)} has uncommitted changes</h2>
          <p>
            Checking out {asked().what} there would mix it with {plural(asked().files, "uncommitted change")}
            <Show when={asked().untracked > 0}> ({plural(asked().untracked, "new file")})</Show> in{" "}
            <span class="checkout-folder">{asked().folder}</span>.
          </p>
          <div class="committed-choices">
            <button ref={first} class="committed-choice primary" onClick={() => answerCheckout("worktree")}>
              <WorktreeIcon />
              <span>
                Check out in a worktree
                <small>In .arnold/worktrees, a folder of its own. Your changes stay where they are, untouched.</small>
              </span>
            </button>
            <button class="committed-choice" onClick={() => answerCheckout("stash")}>
              <StashIcon />
              <span>
                Stash changes and check out here
                <small>
                  Puts {plural(asked().files, "uncommitted change")}
                  <Show when={asked().untracked > 0}>, new files included,</Show> on the git stash first.{" "}
                  <code class="checkout-code">git stash pop</code> brings them back.
                </small>
              </span>
            </button>
            <button class="committed-choice" onClick={() => setConfirming(true)}>
              <TrashIcon />
              <span>
                Discard all changes and check out here
                <small>
                  Throws away {plural(asked().files, "uncommitted change")}
                  <Show when={asked().untracked > 0}>, new files included,</Show> for good, once you confirm. This cannot be
                  undone.
                </small>
              </span>
            </button>
            <button class="committed-choice" onClick={cancel}>
              <CloseIcon />
              <span>
                Cancel
                <small>{asked().cancelNote}</small>
              </span>
            </button>
          </div>
        </div>
      </Show>
    </div>
  );
}
