// Tab and Shift+Tab in an editable editor: indentation, as in an IDE, rather
// than moving the keyboard focus. Escape and then Tab still leaves the editor.

import { indentLess, indentMore } from "@codemirror/commands";
import { getIndentUnit, indentUnit } from "@codemirror/language";
import { EditorSelection, type Extension, Prec, type StateCommand, type Text } from "@codemirror/state";
import { keymap } from "@codemirror/view";

/** Used when the file does not show how it indents. F# allows no tabs. */
const DEFAULT_UNIT = "    ";
/** Lines looked at to see how a file indents. */
const SAMPLE_LINES = 2000;

/**
 * How `doc` indents: a tab when most indented lines start with one, else
 * the step most often seen between the indentation of one line and the
 * next (2 or 4 spaces, say).
 */
export function detectIndentUnit(doc: Text): string {
  let tabs = 0;
  let spaces = 0;
  const steps = new Map<number, number>();
  let previous = 0;
  const last = Math.min(doc.lines, SAMPLE_LINES);
  for (let number = 1; number <= last; number++) {
    const text = doc.line(number).text;
    if (text.trim() === "") continue;
    if (text.startsWith("\t")) {
      tabs++;
      continue;
    }
    const width = text.length - text.trimStart().length;
    if (width > 0) spaces++;
    const step = width - previous;
    if (step > 0 && step <= 8) steps.set(step, (steps.get(step) ?? 0) + 1);
    previous = width;
  }
  if (tabs > spaces) return "\t";
  let best = 0;
  let bestCount = 0;
  for (const [step, count] of steps) {
    if (count > bestCount || (count === bestCount && step > best)) [best, bestCount] = [step, count];
  }
  return best >= 2 ? " ".repeat(best) : DEFAULT_UNIT;
}

/**
 * Tab: with only cursors, indentation at each cursor up to the next indent
 * stop; with a selection, the selected lines are indented.
 */
export const insertIndent: StateCommand = (target) => {
  const { state, dispatch } = target;
  if (state.readOnly) return false;
  if (state.selection.ranges.some((range) => !range.empty)) return indentMore(target);
  const unit = getIndentUnit(state);
  const tab = state.facet(indentUnit) === "\t";
  dispatch(
    state.update(state.changeByRange((range) => {
      const line = state.doc.lineAt(range.head);
      const column = range.head - line.from;
      const insert = tab ? "\t" : " ".repeat(unit - (column % unit));
      return { changes: { from: range.head, insert }, range: EditorSelection.cursor(range.head + insert.length) };
    }), { userEvent: "input.indent", scrollIntoView: true }),
  );
  return true;
};

/** Shift+Tab: the cursor's or the selected lines one step less indented. */
export const outdent: StateCommand = (target) => !target.state.readOnly && indentLess(target);

/** Indentation for an editable editor, in the way `doc` already indents. */
export function tabIndentation(doc: Text): Extension {
  return [
    indentUnit.of(detectIndentUnit(doc)),
    // Ahead of completion and the rest, which leave Tab to the browser.
    Prec.high(keymap.of([{ key: "Tab", run: insertIndent, shift: outdent }])),
  ];
}
