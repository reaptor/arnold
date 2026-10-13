import { Show } from "solid-js";
import {
  chooseRepo,
  goBack,
  nextChange,
  previousChange,
  refresh,
  save,
  setBranches,
  setMode,
  state,
} from "../state/review";
import { BranchPicker } from "./BranchPicker";

const folderName = (path: string) => path.split(/[\\/]/).filter(Boolean).pop() ?? path;

export function Toolbar() {
  return (
    <header class="toolbar">
      <button onClick={chooseRepo} title={state.repo?.root ?? "Open a git repository"}>
        {state.repo ? folderName(state.repo.root) : "Open repository…"}
      </button>
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
            <button
              class="primary"
              disabled={!state.selected || !state.dirty[state.selected]}
              onClick={() => save()}
              title="Save the open file (Ctrl/Cmd+S)"
            >
              Save
            </button>
          </>
        )}
      </Show>
    </header>
  );
}
