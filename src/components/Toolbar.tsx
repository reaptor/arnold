import { Show } from "solid-js";
import {
  goBack,
  nextChange,
  previousChange,
  refresh,
  save,
  saveAll,
  setBranches,
  setMode,
  setView,
  state,
} from "../state/review";
import { BranchPicker } from "./BranchPicker";
import { RepoPicker } from "./RepoPicker";

export function Toolbar() {
  return (
    <header class="toolbar">
      <RepoPicker />
      <Show when={state.repo}>
        {(repo) => (
          <>
            <BranchPicker
              label="Review"
              value={state.head}
              branches={repo().branches}
              onChange={(head) => setBranches({ head })}
            />
            <span class="toolbar-word">against</span>
            <BranchPicker
              label="Base"
              value={state.base}
              branches={repo().branches}
              onChange={(base) => setBranches({ base })}
            />
            <span class="toolbar-space" />
            <Show when={state.backCount > 0}>
              <button onClick={() => goBack()} title="Back to where you jumped from">
                ← Back
              </button>
            </Show>
            <div class="segmented" role="group" aria-label="Files shown">
              <button
                classList={{ on: state.view === "file" }}
                onClick={() => setView("file")}
                title="Show the file selected in the dock"
              >
                One file
              </button>
              <button
                classList={{ on: state.view === "all" }}
                onClick={() => setView("all")}
                title="Show every file of the diff in one list"
              >
                All files
              </button>
            </div>
            <div class="segmented" role="group" aria-label="Diff layout">
              <button classList={{ on: state.mode === "split" }} onClick={() => setMode("split")}>
                Split
              </button>
              <button classList={{ on: state.mode === "unified" }} onClick={() => setMode("unified")}>
                Unified
              </button>
            </div>
            <button onClick={previousChange} title="Previous change (Shift+F7)">
              ↑
            </button>
            <button onClick={nextChange} title="Next change (F7)">
              ↓
            </button>
            <button onClick={() => refresh()} title="Refresh the comparison">
              Refresh
            </button>
            <Show
              when={state.view === "all"}
              fallback={
                <button
                  class="primary"
                  disabled={!state.selected || !state.dirty[state.selected]}
                  onClick={() => save()}
                  title="Save the open file (Ctrl/Cmd+S)"
                >
                  Save
                </button>
              }
            >
              <button
                class="primary"
                disabled={!Object.values(state.dirty).some(Boolean)}
                onClick={() => saveAll()}
                title="Save every file with unsaved edits"
              >
                Save all
              </button>
            </Show>
          </>
        )}
      </Show>
    </header>
  );
}
