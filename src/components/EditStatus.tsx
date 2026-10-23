import { createEffect, createSignal, onCleanup, Show } from "solid-js";
import {
  branchNames,
  commitEdits,
  type EditAction,
  editBlocked,
  editInfoAnchor,
  editInfoOpen,
  editsToCommit,
  makeEditable,
  setEditInfoOpen,
  state,
} from "../state/review";

/** Closes a status bar popup on a click outside `root` or Escape. */
function closeOnOutside(root: () => HTMLElement, close: () => void) {
  const outside = (event: PointerEvent) => {
    if (!root().contains(event.target as Node)) close();
  };
  const escape = (event: KeyboardEvent) => {
    if (event.key === "Escape") close();
  };
  window.addEventListener("pointerdown", outside);
  window.addEventListener("keydown", escape);
  onCleanup(() => {
    window.removeEventListener("pointerdown", outside);
    window.removeEventListener("keydown", escape);
  });
}

/**
 * Branches and pull requests: whether the diff can be edited. With the
 * branch checked out, how many files have uncommitted edits and a way to
 * commit them (others' pull requests' edits are suggested from the Suggest
 * button on each change instead; your own pull request on its branch is as
 * Branches). Without (a picked run of commits, a checkout that failed), a
 * "Read-only" pill saying why, and offering what would make the diff
 * editable.
 */
export function EditStatus() {
  return (
    <Show when={state.comparison?.checkout} fallback={<ReadOnlyPill />}>
      <Show when={editsToCommit() && state.comparison!.uncommitted > 0}>
        <button
          class="edit-status edited"
          title={`Files of ${state.comparison!.checkout} that differ from the last commit of ${branchNames().head || state.comparison!.head}. Commit them in Commit mode.`}
          disabled={state.busy}
          onClick={() => void commitEdits()}
        >
          <span class="edited-dot" aria-hidden="true">
            ●
          </span>
          {state.comparison!.uncommitted} uncommitted {state.comparison!.uncommitted === 1 ? "edit" : "edits"}
          <span class="edit-status-action">Commit…</span>
        </button>
      </Show>
    </Show>
  );
}

function ReadOnlyPill() {
  let root!: HTMLDivElement;
  closeOnOutside(() => root, () => setEditInfoOpen(false));
  const act = () => {
    // Takes what was typed before the popup closes (closing drops it).
    void makeEditable();
    setEditInfoOpen(false);
  };
  // Opened by typing: next to the cursor, below its line (above when there
  // is no room), kept inside the window.
  let popup: HTMLDivElement | undefined;
  const [place, setPlace] = createSignal<{ left: number; top: number } | null>(null);
  createEffect(() => {
    const anchor = editInfoAnchor();
    setPlace(null);
    if (!anchor || !popup) return;
    const margin = 8;
    const { width, height } = popup.getBoundingClientRect();
    const below = anchor.bottom + 6;
    const top =
      below + height + margin <= window.innerHeight ? below : Math.max(margin, anchor.top - 6 - height);
    const left = Math.min(Math.max(margin, anchor.left - 24), window.innerWidth - width - margin);
    setPlace({ left, top });
  });

  return (
    <Show when={editBlocked()}>
      {(blocked) => (
        <div class="edit-status-root" ref={root}>
          <button
            class="edit-status read-only"
            title="This diff cannot be edited: click for why"
            onClick={() => setEditInfoOpen(!editInfoOpen())}
          >
            <LockIcon />
            Read-only
          </button>
          <Show when={editInfoOpen()}>
            <div
              class="language-popup edit-popup"
              classList={{ "at-cursor": editInfoAnchor() !== null, placed: place() !== null }}
              style={place() ? { left: `${place()!.left}px`, top: `${place()!.top}px` } : undefined}
              ref={popup}
              role="dialog"
              aria-label="Editing"
            >
              <h3>Read-only</h3>
              <p>{blocked().reason}</p>
              <Show when={checkOutLabel(blocked().action)}>
                {(label) => (
                  <div class="edit-popup-actions">
                    <button class="primary" disabled={state.busy} onClick={act}>
                      {label()}
                    </button>
                  </div>
                )}
              </Show>
              <Show when={blocked().action?.kind === "discard"}>
                <div class="edit-popup-actions">
                  <button disabled={state.busy} onClick={act}>
                    Discard edits and edit the latest push…
                  </button>
                </div>
              </Show>
            </div>
          </Show>
        </div>
      )}
    </Show>
  );
}

const checkOutLabel = (action: EditAction | null) => (action?.kind === "checkOut" ? action.label : null);

function LockIcon() {
  return (
    <svg class="lock-icon" width="12" height="12" viewBox="0 0 16 16" aria-hidden="true">
      <rect x="3" y="7" width="10" height="7" rx="1.5" fill="none" stroke="currentColor" stroke-width="1.3" />
      <path d="M5.5 7V5a2.5 2.5 0 0 1 5 0v2" fill="none" stroke="currentColor" stroke-width="1.3" />
    </svg>
  );
}
