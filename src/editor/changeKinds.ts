// Diff colors: a change is green when it only adds lines, red when it only
// removes lines, and red on the old side and green on the new side when it
// replaces lines (styles.css has the colors). CodeMirror's
// merge view marks every change the same way, so this classifies its chunks
// and draws the change gutter itself (the merge view's own gutter is off).

import { type Chunk, getChunks } from "@codemirror/merge";
import { RangeSet, RangeSetBuilder } from "@codemirror/state";
import {
  BlockType,
  Decoration,
  type DecorationSet,
  type EditorView,
  gutter,
  GutterMarker,
  type ViewUpdate,
  ViewPlugin,
} from "@codemirror/view";

type Kind = "inserted" | "deleted" | "modified";

function kindOf(chunk: Chunk): Kind {
  if (chunk.fromA === chunk.toA) return "inserted";
  if (chunk.fromB === chunk.toB) return "deleted";
  return "modified";
}

const lineDeco = {
  inserted: Decoration.line({ class: "cm-kind-inserted" }),
  deleted: Decoration.line({ class: "cm-kind-deleted" }),
  modified: Decoration.line({ class: "cm-kind-modified" }),
};

class KindMarker extends GutterMarker {
  constructor(readonly elementClass: string) {
    super();
  }
}

const gutterMarker = {
  inserted: new KindMarker("cm-changeMark cm-kind-inserted"),
  deleted: new KindMarker("cm-changeMark cm-kind-deleted"),
  modified: new KindMarker("cm-changeMark cm-kind-modified"),
};

/** The old lines of a replacement, shown above the new ones in the unified view. */
const replacedMarker = new KindMarker("cm-changeMark cm-kind-modified cm-kind-old");

/** The lines a chunk covers on this editor's side, as [from, to] document positions. */
function span(chunk: Chunk, side: "a" | "b") {
  return side === "a" ? [chunk.fromA, chunk.endA] : [chunk.fromB, chunk.endB];
}

function build(view: EditorView) {
  const lines = new RangeSetBuilder<Decoration>();
  const marks = new RangeSetBuilder<GutterMarker>();
  const found = getChunks(view.state);
  if (found) {
    // In the unified view (side null) the editor holds the working text.
    const side = found.side ?? "b";
    const { doc } = view.state;
    for (const chunk of found.chunks) {
      const [from, to] = span(chunk, side);
      if (from === (side === "a" ? chunk.toA : chunk.toB)) continue;
      const kind = kindOf(chunk);
      for (let line = doc.lineAt(from); ; line = doc.line(line.number + 1)) {
        lines.add(line.from, line.from, lineDeco[kind]);
        marks.add(line.from, line.from, gutterMarker[kind]);
        if (line.to >= to || line.number === doc.lines) break;
      }
    }
  }
  return { decorations: lines.finish(), marks: marks.finish() };
}

const changeKinds = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;
    marks: RangeSet<GutterMarker>;

    constructor(view: EditorView) {
      ({ decorations: this.decorations, marks: this.marks } = build(view));
    }

    update(update: ViewUpdate) {
      const before = getChunks(update.startState)?.chunks;
      if (update.docChanged || getChunks(update.state)?.chunks !== before) {
        ({ decorations: this.decorations, marks: this.marks } = build(update.view));
      }
    }
  },
  { decorations: (plugin) => plugin.decorations },
);

/** The unified view shows removed lines as a block widget above the chunk's new lines. */
function deletionMarker(view: EditorView, block: { from: number; type: BlockType | readonly unknown[] }) {
  if (block.type !== BlockType.WidgetBefore) return null;
  const chunk = getChunks(view.state)?.chunks.find((chunk) => chunk.fromB === block.from && chunk.fromA !== chunk.toA);
  if (!chunk) return null;
  const kind = kindOf(chunk);
  return kind === "modified" ? replacedMarker : gutterMarker[kind];
}

export const changeKindColors = [
  changeKinds,
  gutter({
    class: "cm-changeGutter",
    markers: (view) => view.plugin(changeKinds)?.marks ?? RangeSet.empty,
    widgetMarker: (view, _widget, block) => deletionMarker(view, block),
  }),
];
