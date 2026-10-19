import { createSignal, Match, onCleanup, Show, Switch } from "solid-js";
import { platform } from "../platform";
import { checkNow, restartToUpdate, retryDownload, setUpdatePopupOpen, updates } from "../state/updates";
import { ago } from "./CommitPicker";
import { openInBrowser } from "./DevOpsSetup";
import { Markdown } from "./Markdown";

const RELEASES_URL = "https://github.com/reaptor/arnold/releases";

/**
 * Status bar item for Arnold's own updates: the version, quietly, until a newer
 * one is downloading or ready; click for "Check for updates" and "Restart to
 * update".
 */
export function UpdateStatus() {
  let root!: HTMLDivElement;
  // Re-render "checked … ago" while the popup is open.
  const [now, setNow] = createSignal(Date.now());
  const clock = window.setInterval(() => setNow(Date.now()), 30_000);

  const closeOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) setUpdatePopupOpen(false);
  };
  const closeOnEscape = (event: KeyboardEvent) => {
    if (event.key === "Escape") setUpdatePopupOpen(false);
  };
  window.addEventListener("pointerdown", closeOutside);
  window.addEventListener("keydown", closeOnEscape);
  onCleanup(() => {
    window.clearInterval(clock);
    window.removeEventListener("pointerdown", closeOutside);
    window.removeEventListener("keydown", closeOnEscape);
  });

  const phase = () => updates.phase;
  const found = () => {
    const current = phase();
    return "update" in current ? current.update : null;
  };

  const label = () => {
    const current = phase();
    switch (current.kind) {
      case "downloading":
        return `Downloading Arnold ${current.update.version}${current.percent === null ? "…" : ` · ${current.percent}%`}`;
      case "ready":
        return `Restart to update to ${current.update.version}`;
      case "installing":
        return `Installing Arnold ${current.update.version}…`;
      case "available":
        return `Arnold ${current.update.version} is available`;
      default:
        return `v${updates.version}`;
    }
  };

  const checked = () => (updates.checkedAt === null ? null : ago(new Date(updates.checkedAt).toISOString(), now()));

  const toggle = () => {
    setNow(Date.now());
    setUpdatePopupOpen(!updates.popupOpen);
  };

  return (
    <Show when={updates.version}>
      <div class="language-status update-status" ref={root}>
        <button
          class="language-button update-button"
          classList={{
            working: phase().kind === "downloading" || phase().kind === "installing",
            ready: phase().kind === "ready" || phase().kind === "available",
          }}
          title="Arnold updates: click to check for updates"
          onClick={toggle}
        >
          <Show when={phase().kind === "downloading" || phase().kind === "installing" || phase().kind === "checking"}>
            <span class="spinner" aria-hidden="true" />
          </Show>
          <span class="language-label">{label()}</span>
        </button>
        <Show when={updates.popupOpen}>
          <div class="language-popup update-popup" role="dialog" aria-label="Arnold updates">
            <h3>Arnold {updates.version}</h3>
            <p>
              <Switch>
                <Match when={phase().kind === "checking"}>Checking for updates…</Match>
                <Match when={phase().kind === "upToDate"}>This is the newest version.</Match>
                <Match when={phase().kind === "downloading"}>
                  Downloading Arnold {found()!.version} in the background. Keep working; restart once it is ready.
                </Match>
                <Match when={phase().kind === "ready"}>
                  Arnold {found()!.version} is downloaded.{" "}
                  {platform === "windows"
                    ? "Restarting runs its installer, which closes Arnold, updates it and starts it again."
                    : "Restarting installs it and starts it again."}{" "}
                  The repository and your place in it are kept; edits are saved first.
                </Match>
                <Match when={phase().kind === "installing"}>Installing Arnold {found()!.version}…</Match>
                <Match when={phase().kind === "available"}>
                  Arnold {found()!.version} is available.
                  <Show when={!updates.canInstall}> Development builds do not update themselves.</Show>
                </Match>
                <Match when={phase().kind === "idle"}>
                  Arnold looks for a newer version once an hour and downloads it in the background.
                </Match>
              </Switch>
              <Show when={checked()}> Last checked {checked()}.</Show>
            </p>
            <Show when={updates.error}>
              <p class="update-error">{updates.error}</p>
            </Show>
            <Show when={found()?.notes}>
              {(notes) => (
                <details class="update-notes" open>
                  <summary>What's new in {found()!.version}</summary>
                  <Markdown text={notes()} />
                </details>
              )}
            </Show>
            <div class="edit-popup-actions">
              <Show when={phase().kind === "ready" || phase().kind === "installing"}>
                <button class="primary" disabled={phase().kind === "installing"} onClick={() => void restartToUpdate()}>
                  Restart to update
                </button>
              </Show>
              <Show when={phase().kind === "available" && updates.canInstall}>
                <button class="primary" onClick={retryDownload}>
                  Download {found()!.version}
                </button>
              </Show>
              <button
                disabled={["checking", "downloading", "installing"].includes(phase().kind)}
                onClick={() => void checkNow()}
              >
                Check for updates
              </button>
              <button onClick={() => openInBrowser(RELEASES_URL)}>
                All releases
              </button>
            </div>
          </div>
        </Show>
      </div>
    </Show>
  );
}
