// Merge conflicts in the working text of a file git could not merge: the
// lines between git's markers (<<<<<<<, an optional ||||||| with the common
// ancestor, =======, >>>>>>>) are tinted by side, and buttons on the first
// marker line take the current side, the incoming side or both, as an edit
// that can be undone like any other.

import { type EditorState, type Extension, RangeSetBuilder, StateField } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, WidgetType } from "@codemirror/view";

/** One conflict: line numbers are 1-based, offsets into the text. */
export interface Conflict {
  /** Start of the `<<<<<<<` line. */
  from: number;
  /** End of the `>>>>>>>` line, past its line break when it has one. */
  to: number;
  /** Lines of the markers; `base` is the `|||||||` line, when git wrote the common ancestor. */
  start: number;
  base: number | null;
  middle: number;
  end: number;
  /** What git wrote after the markers: the current side (HEAD) and the incoming one (the branch merged in). */
  current: string;
  incoming: string;
  /** The lines of each side, without line breaks. */
  ours: string[];
  theirs: string[];
}

/** Which side of a conflict to keep. */
export type Resolution = "current" | "incoming" | "both";

const marker = (line: string, mark: string) => line.startsWith(mark) && (line.length === 7 || line[7] === " ");
const label = (line: string) => line.slice(8).trim();

/** The conflicts in `text`, in order. Markers that do not make a whole conflict are left alone. */
export function findConflicts(text: string): Conflict[] {
  const lines = text.split("\n");
  const conflicts: Conflict[] = [];
  let offset = 0;
  let open: { from: number; start: number; current: string; base: number | null; middle: number | null } | null = null;
  for (let index = 0; index < lines.length; index++) {
    const line = lines[index];
    const number = index + 1;
    const from = offset;
    offset += line.length + 1;
    if (marker(line, "<<<<<<<")) {
      open = { from, start: number, current: label(line), base: null, middle: null };
    } else if (!open) {
      continue;
    } else if (open.middle === null && open.base === null && marker(line, "|||||||")) {
      open.base = number;
    } else if (open.middle === null && line === "=======") {
      open.middle = number;
    } else if (open.middle !== null && marker(line, ">>>>>>>")) {
      const { start, base, middle } = open;
      conflicts.push({
        from: open.from,
        to: Math.min(offset, text.length),
        start,
        base,
        middle,
        end: number,
        current: open.current,
        incoming: label(line),
        ours: lines.slice(start, (base ?? middle) - 1),
        theirs: lines.slice(middle, number - 1),
      });
      open = null;
    }
  }
  return conflicts;
}

/** The text that takes the place of `conflict` (from `from` to `to`) to resolve it as `choice` says. */
export function resolvedText(conflict: Conflict, choice: Resolution, text: string): string {
  const kept = choice === "current" ? conflict.ours : choice === "incoming" ? conflict.theirs : [...conflict.ours, ...conflict.theirs];
  const resolved = kept.map((line) => `${line}\n`).join("");
  // The last line of the file keeps having no line break.
  const lastLine = conflict.to === text.length && !text.endsWith("\n");
  return lastLine ? resolved.replace(/\n$/, "") : resolved;
}

const BUTTONS: { choice: Resolution; text: string; title: (conflict: Conflict) => string }[] = [
  { choice: "current", text: "Accept current", title: (c) => `Keep the lines of ${c.current || "the current side"}` },
  { choice: "incoming", text: "Accept incoming", title: (c) => `Keep the lines of ${c.incoming || "the incoming side"}` },
  { choice: "both", text: "Accept both", title: (c) => `Keep the lines of both, ${c.current || "current"} first` },
];

class ResolveButtons extends WidgetType {
  constructor(private readonly conflict: Conflict) {
    super();
  }

  eq(other: ResolveButtons) {
    const [a, b] = [this.conflict, other.conflict];
    return a.from === b.from && a.current === b.current && a.incoming === b.incoming;
  }

  toDOM(view: EditorView) {
    const bar = document.createElement("span");
    bar.className = "cm-conflict-buttons";
    for (const button of BUTTONS) {
      const element = document.createElement("button");
      element.className = "cm-conflict-button";
      element.textContent = button.text;
      element.title = button.title(this.conflict);
      // Keeps the cursor where it is until the click.
      element.onmousedown = (event) => event.preventDefault();
      element.onclick = () => resolveAt(view, view.posAtDOM(bar), button.choice);
      bar.append(element);
    }
    return bar;
  }

  ignoreEvent() {
    return true;
  }
}

/** Resolves the conflict whose first marker line holds `pos`. */
function resolveAt(view: EditorView, pos: number, choice: Resolution) {
  const text = view.state.doc.toString();
  const line = view.state.doc.lineAt(pos).number;
  const conflict = view.state.field(conflictField).conflicts.find((candidate) => candidate.start === line);
  if (!conflict || view.state.readOnly) return;
  const insert = resolvedText(conflict, choice, text);
  view.dispatch({
    changes: { from: conflict.from, to: conflict.to, insert },
    selection: { anchor: conflict.from },
    userEvent: "input.resolve",
    scrollIntoView: true,
  });
  view.focus();
}

function decorate(state: EditorState, conflicts: readonly Conflict[]): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>();
  const doc = state.doc;
  const line = (number: number, kind: string) => {
    const from = doc.line(number).from;
    builder.add(from, from, Decoration.line({ class: `cm-conflict cm-conflict-${kind}` }));
  };
  for (const conflict of conflicts) {
    for (let number = conflict.start; number <= conflict.end; number++) {
      if (number === conflict.start || number === conflict.base || number === conflict.middle || number === conflict.end) {
        line(number, "marker");
        if (number === conflict.start && !state.readOnly) {
          const end = doc.line(number).to;
          builder.add(end, end, Decoration.widget({ widget: new ResolveButtons(conflict), side: 1 }));
        }
      } else {
        line(number, number < (conflict.base ?? conflict.middle) ? "ours" : number < conflict.middle ? "ancestor" : "theirs");
      }
    }
  }
  return builder.finish();
}

const conflictField = StateField.define<{ conflicts: Conflict[]; decorations: DecorationSet }>({
  create(state) {
    const conflicts = findConflicts(state.doc.toString());
    return { conflicts, decorations: decorate(state, conflicts) };
  },
  update(value, tr) {
    if (!tr.docChanged) return value;
    const conflicts = findConflicts(tr.state.doc.toString());
    return { conflicts, decorations: decorate(tr.state, conflicts) };
  },
  provide: (field) => EditorView.decorations.from(field, (value) => value.decorations),
});

/** Tints the conflicts of the working text and puts buttons on them that resolve them. */
export const mergeConflicts: Extension = conflictField;
