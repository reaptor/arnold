import { createSignal, For, onCleanup, Show } from "solid-js";
import { isDark, setThemeChoice, type ThemeChoice, themeChoice } from "../state/theme";
import { CheckIcon, MoonIcon, SunIcon, SystemThemeIcon } from "./icons";

const CHOICES: { choice: ThemeChoice; label: () => string }[] = [
  { choice: "system", label: () => `System (${isDark() ? "dark" : "light"})` },
  { choice: "light", label: () => "Light" },
  { choice: "dark", label: () => "Dark" },
];

/** Status bar item for the theme: light, dark, or as the system is set. */
export function ThemeStatus() {
  let root!: HTMLDivElement;
  const [open, setOpen] = createSignal(false);

  const closeOutside = (event: PointerEvent) => {
    if (!root.contains(event.target as Node)) setOpen(false);
  };
  const closeOnEscape = (event: KeyboardEvent) => {
    if (event.key === "Escape") setOpen(false);
  };
  window.addEventListener("pointerdown", closeOutside);
  window.addEventListener("keydown", closeOnEscape);
  onCleanup(() => {
    window.removeEventListener("pointerdown", closeOutside);
    window.removeEventListener("keydown", closeOnEscape);
  });

  const title = () => {
    const choice = themeChoice();
    const shown = isDark() ? "dark" : "light";
    return choice === "system" ? `Theme: as the system is set (${shown}); click to pick one` : `Theme: ${shown}; click to change`;
  };

  return (
    <div class="language-status theme-status" ref={root}>
      <button
        class="language-button theme-button"
        title={title()}
        aria-label="Theme"
        aria-haspopup="menu"
        aria-expanded={open()}
        onClick={() => setOpen(!open())}
      >
        <Show when={themeChoice() !== "system"} fallback={<SystemThemeIcon />}>
          <Show when={isDark()} fallback={<SunIcon />}>
            <MoonIcon />
          </Show>
        </Show>
      </button>
      <Show when={open()}>
        <ul class="menu theme-menu" role="menu" aria-label="Theme">
          <For each={CHOICES}>
            {(item) => (
              <li
                role="menuitemradio"
                aria-checked={themeChoice() === item.choice}
                onClick={() => {
                  setThemeChoice(item.choice);
                  setOpen(false);
                }}
              >
                <span class="menu-check">{themeChoice() === item.choice ? <CheckIcon /> : null}</span>
                <span class="menu-label">{item.label()}</span>
              </li>
            )}
          </For>
        </ul>
      </Show>
    </div>
  );
}
