/** The desktop the app runs on; the title bar differs per platform. */
export type Platform = "mac" | "windows" | "other";

function detect(): Platform {
  // The browser preview can show another platform's title bar: ?platform=windows.
  if (import.meta.env.DEV) {
    const forced = new URLSearchParams(location.search).get("platform");
    if (forced === "mac" || forced === "windows" || forced === "other") return forced;
  }
  const name = navigator.platform.toLowerCase();
  if (name.includes("mac")) return "mac";
  if (name.includes("win")) return "windows";
  return "other";
}

export const platform: Platform = detect();
export const isMac = platform === "mac";

/** The system's file manager, as its menus name it. */
export const fileManager = isMac ? "Finder" : platform === "windows" ? "File Explorer" : "the file manager";
export const revealLabel = `Reveal in ${isMac ? "Finder" : platform === "windows" ? "File Explorer" : "file manager"}`;
