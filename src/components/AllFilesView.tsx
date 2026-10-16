import {
  hideSection,
  onRevealSection,
  registerSection,
  setCollapsed,
  setState,
  showSection,
  state,
  unregisterSection,
} from "../state/review";
import { SectionList, type SectionSource } from "./SectionList";

/** The review's files: every file of the diff, and files opened from outside it. */
const reviewSource: SectionSource = {
  entries: () => [...(state.comparison?.files ?? []), ...state.visited].map((file) => ({ key: file.path, file })),
  selected: () => state.selected,
  select: (path) => setState("selected", path),
  collapsed: (path) => !!state.collapsed[path],
  setCollapsed,
  register: registerSection,
  unregister: unregisterSection,
  show: showSection,
  hide: hideSection,
  onReveal: (handler) => onRevealSection(handler ?? (() => {})),
  placeholder: () => (state.comparison ? "No differences." : state.repo ? "" : "Open a repository to start reviewing."),
};

/** Every file of the diff in one scrolling list, like the "all files" view of a pull request. */
export function AllFilesView() {
  return <SectionList source={reviewSource} />;
}
