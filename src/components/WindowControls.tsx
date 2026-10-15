import { getCurrentWindow } from "@tauri-apps/api/window";
import { createSignal, onCleanup } from "solid-js";

/** Minimize, maximize/restore and close, drawn by the app on Windows where the
    toolbar replaces the system title bar (as in Rider). */
export function WindowControls() {
  const window = getCurrentWindow();
  const [maximized, setMaximized] = createSignal(false);
  const update = () => void window.isMaximized().then(setMaximized);
  update();
  const stop = window.onResized(update);
  onCleanup(() => void stop.then((unlisten) => unlisten()));

  return (
    <div class="window-controls" data-tauri-drag-region="false">
      <button title="Minimize" onClick={() => void window.minimize()}>
        <svg viewBox="0 0 10 10" aria-hidden="true">
          <path d="M0 5.5h10" />
        </svg>
      </button>
      <button title={maximized() ? "Restore down" : "Maximize"} onClick={() => void window.toggleMaximize()}>
        <svg viewBox="0 0 10 10" aria-hidden="true">
          {maximized() ? (
            <path d="M2.5 2.5V.5h7v7h-2M.5 2.5h7v7h-7z" />
          ) : (
            <path d="M.5.5h9v9h-9z" />
          )}
        </svg>
      </button>
      <button class="close" title="Close" onClick={() => void window.close()}>
        <svg viewBox="0 0 10 10" aria-hidden="true">
          <path d="m.5.5 9 9m0-9-9 9" />
        </svg>
      </button>
    </div>
  );
}
