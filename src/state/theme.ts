// Light or dark: as the operating system is set (the default), or picked.
// The choice is remembered; the resolved theme is `data-theme` on the root
// element, which styles.css keys its dark colors on, and the editors and code
// blocks follow it (riderIslandsTheme.ts).

import { isTauri } from "@tauri-apps/api/core";
import { getCurrentWindow } from "@tauri-apps/api/window";
import { createEffect, createRoot, createSignal } from "solid-js";

export type ThemeChoice = "system" | "light" | "dark";

const STORAGE_KEY = "arnold.theme";

function rememberedChoice(): ThemeChoice {
  try {
    const saved = localStorage.getItem(STORAGE_KEY);
    return saved === "light" || saved === "dark" ? saved : "system";
  } catch {
    return "system";
  }
}

const [themeChoice, setChoice] = createSignal<ThemeChoice>(rememberedChoice());
export { themeChoice };

const systemQuery = window.matchMedia("(prefers-color-scheme: dark)");
const [systemDark, setSystemDark] = createSignal(systemQuery.matches);
systemQuery.addEventListener("change", (event) => setSystemDark(event.matches));

/** Whether the dark theme is shown: picked, or the system's while following it. */
export const isDark = () => (themeChoice() === "system" ? systemDark() : themeChoice() === "dark");

export function setThemeChoice(choice: ThemeChoice) {
  setChoice(choice);
  try {
    if (choice === "system") localStorage.removeItem(STORAGE_KEY);
    else localStorage.setItem(STORAGE_KEY, choice);
  } catch {
    // Not remembered, then.
  }
}

createRoot(() => {
  createEffect(() => {
    const dark = isDark();
    document.documentElement.dataset.theme = dark ? "dark" : "light";
  });
  // The window's own parts (macOS's window buttons, Windows' frame) follow a
  // picked theme too; null hands them back to the system.
  if (isTauri()) {
    createEffect(() => {
      const choice = themeChoice();
      void getCurrentWindow()
        .setTheme(choice === "system" ? null : choice)
        .catch(() => {});
    });
  }
});
