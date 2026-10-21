import { createMemo, createResource, createSignal, For, onMount, Show } from "solid-js";
import { setLicensesOpen } from "../state/licenses";
import { updates } from "../state/updates";
import { openInBrowser } from "./DevOpsSetup";

type Package = { name: string; version: string; license: string; url: string };
/** `texts` are indexes into `Notices.texts`: each license file is stored once. */
type Notice = { license: string; texts: number[]; packages: Package[] };
type Notices = { arnold: { license: string; text: string }; packageCount: number; texts: string[]; notices: Notice[] };

// Written by scripts/third-party-notices.mjs during `npm run build`; loaded only when shown.
// Null when it is missing (a dev build that never ran the script): reading a failed resource
// throws, which would leave the dialog empty instead of saying so.
const loadNotices = async (): Promise<Notices | null> => {
  try {
    const response = await fetch("/third-party-notices.json");
    return response.ok ? await response.json() : null;
  } catch {
    return null;
  }
};

/**
 * "Open source licenses": Arnold's own license and the license texts of every package built
 * into it, as MIT, Apache-2.0, BSD and the fonts' OFL ask.
 */
export function LicensesDialog() {
  const [notices] = createResource(loadNotices);
  const [filter, setFilter] = createSignal("");
  let search: HTMLInputElement | undefined;
  onMount(() => search?.focus());
  const close = () => setLicensesOpen(false);

  const term = () => filter().trim().toLowerCase();
  const matches = (name: string, license: string) =>
    name.toLowerCase().includes(term()) || license.toLowerCase().includes(term());
  const shown = createMemo(() =>
    (notices()?.notices ?? []).filter((notice) => notice.packages.some((p) => matches(p.name, notice.license))),
  );
  const arnold = () => {
    const own = notices()?.arnold;
    return own && matches("Arnold", own.license) ? own : undefined;
  };

  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && close()}
      onKeyDown={(event) => event.key === "Escape" && close()}
    >
      <div class="modal complete-dialog licenses-dialog" role="dialog" aria-label="Open source licenses">
        <h2>Open source licenses</h2>
        <p>
          Arnold is free software under the{" "}
          <button class="licenses-link" onClick={() => openInBrowser("https://github.com/reaptor/arnold/blob/main/LICENSE")}>
            MIT License
          </button>
          . It is built on the open source packages below
          <Show when={notices()}>{(loaded) => <> ({loaded().packageCount})</>}</Show>, under their own licenses.
          FsAutoComplete, which Arnold downloads when it is needed, is under the Apache License 2.0.
        </p>
        <p class="licenses-trademarks">
          JetBrains and Rider are trademarks of JetBrains s.r.o. Arnold is not affiliated with or endorsed by
          JetBrains.
        </p>
        <input
          ref={search}
          class="picker-search"
          type="search"
          placeholder="Filter by package or license"
          value={filter()}
          onInput={(event) => setFilter(event.currentTarget.value)}
        />
        <div class="licenses-list">
          <Show when={notices() !== null} fallback={<p class="licenses-missing">The license list is not part of this build.</p>}>
            <Show when={arnold()}>
              {(own) => (
                <details class="licenses-entry">
                  <summary>
                    <span class="licenses-names">
                      Arnold <span class="licenses-version">{updates.version}</span>
                    </span>
                    <span class="licenses-id">{own().license}</span>
                  </summary>
                  <pre class="licenses-text">{own().text}</pre>
                </details>
              )}
            </Show>
            <For each={shown()}>
              {(notice) => (
                <details class="licenses-entry">
                  <summary>
                    <span class="licenses-names">{notice.packages.map((p) => p.name).join(", ")}</span>
                    <span class="licenses-id">{notice.license}</span>
                  </summary>
                  <ul class="licenses-packages">
                    <For each={notice.packages}>
                      {(p) => (
                        <li>
                          <button class="licenses-link" onClick={() => openInBrowser(p.url)}>
                            {p.name}
                          </button>{" "}
                          {p.version}
                        </li>
                      )}
                    </For>
                  </ul>
                  <pre class="licenses-text">
                    {notice.texts.length
                      ? notice.texts.map((id) => notices()!.texts[id]).join("\n\n-----\n\n")
                      : `${notice.license}. The package ships no license file; see its repository.`}
                  </pre>
                </details>
              )}
            </For>
          </Show>
        </div>
        <div class="edit-popup-actions licenses-actions">
          <button class="primary" onClick={close}>
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
