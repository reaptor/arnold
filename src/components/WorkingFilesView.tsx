import type { WorkingSide } from "../ipc/commands";
import {
  fileAction,
  hideSection,
  onRevealSection,
  registerSection,
  sectionEntries,
  selectedSection,
  selectSection,
  setSectionCollapsed,
  showSection,
  unregisterSection,
} from "../state/commitMode";
import { noteListPlace, state } from "../state/review";
import { SectionList, type SectionSource } from "./SectionList";

const sideOf = (key: string): WorkingSide => (key.startsWith("unstaged:") ? "unstaged" : "staged");

/** Commit mode's files: Unstaged, then Staged, each as it shows when picked in its list. */
const workingSource: SectionSource = {
  entries: sectionEntries,
  selected: selectedSection,
  select: selectSection,
  collapsed: (key) => !!state.collapsed[key],
  setCollapsed: setSectionCollapsed,
  register: registerSection,
  unregister: unregisterSection,
  show: showSection,
  hide: hideSection,
  onReveal: onRevealSection,
  onPlace: noteListPlace,
  placeholder: () =>
    state.work ? "No uncommitted changes. Review the branch, push it or create its pull request from the toolbar." : "Reading the uncommitted changes…",
  actions: (entry) => {
    const side = sideOf(entry.key);
    return side === "unstaged" ? (
      <button class="section-action" title="Stage the file (S in the list)" onClick={() => void fileAction("stage", side, entry.file)}>
        Stage
      </button>
    ) : (
      <button class="section-action" title="Unstage the file (U in the list)" onClick={() => void fileAction("unstage", side, entry.file)}>
        Unstage
      </button>
    );
  },
};

/** Every uncommitted file in one scrolling list: Unstaged first, then Staged. */
export function WorkingFilesView() {
  return <SectionList source={workingSource} />;
}
