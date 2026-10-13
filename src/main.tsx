import { render } from "solid-js/web";
import { App } from "./App";
import "./styles.css";

// In a plain browser (UI development without Tauri) use the in-memory backend.
if (import.meta.env.DEV && !("__TAURI_INTERNALS__" in window)) {
  (await import("./ipc/devMock")).install();
}

render(() => <App />, document.getElementById("root")!);
