import {
  hideSection,
  noteListPlace,
  onRevealSection,
  registerSection,
  setCollapsed,
  setState,
  showSection,
  state,
  unregisterSection,
} from "../state/review";
import { SectionList, type SectionSource } from "./SectionList";

/**
 * The review's files: every file of the diff, and files opened from outside
 * it. A visited file just edited into the diff is listed once, though for a
 * moment of a refresh it is in both (its two sections would share one host).
 */
const reviewSource: SectionSource = {
  entries: () => {
    const files = state.comparison?.files ?? [];
    const visited = state.visited.filter((file) => !files.some((changed) => changed.path === file.path));
    return [...files, ...visited].map((file) => ({ key: file.path, file }));
  },
  selected: () => state.selected,
  select: (path) => setState("selected", path),
  collapsed: (path) => !!state.collapsed[path],
  setCollapsed,
  register: registerSection,
  unregister: unregisterSection,
  show: showSection,
  hide: hideSection,
  onReveal: (handler) => onRevealSection(handler ?? (() => {})),
  onPlace: noteListPlace,
  placeholder: () => (state.comparison ? "No differences." : state.repo ? "" : "Open a repository to start reviewing."),
};

/** Every file of the diff in one scrolling list, like the "all files" view of a pull request. */
export function AllFilesView() {
  return <SectionList source={reviewSource} />;
}
