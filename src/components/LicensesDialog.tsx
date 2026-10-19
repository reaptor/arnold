import { createMemo, createResource, createSignal, For, onMount, Show } from "solid-js";
import { setLicensesOpen } from "../state/licenses";
import { openInBrowser } from "./DevOpsSetup";

type Package = { name: string; version: string; license: string; url: string };
type Notice = { license: string; text: string | null; packages: Package[] };
type Notices = { packageCount: number; notices: Notice[] };

// Written by scripts/third-party-notices.mjs during `npm run build`; loaded only when shown.
const loadNotices = async (): Promise<Notices> => {
  const response = await fetch("/third-party-notices.json");
  if (!response.ok) throw new Error(`${response.status}`);
  return response.json();
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

  const shown = createMemo(() => {
    const term = filter().trim().toLowerCase();
    const all = notices()?.notices ?? [];
    if (!term) return all;
    return all.filter(
      (notice) =>
        notice.license.toLowerCase().includes(term) ||
        notice.packages.some((p) => p.name.toLowerCase().includes(term)),
    );
  });

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
          <Show when={!notices.error} fallback={<p class="licenses-missing">The license list is not part of this build.</p>}>
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
                    {notice.text ?? `${notice.license}. The package ships no license file; see its repository.`}
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
