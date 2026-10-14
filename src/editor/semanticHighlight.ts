// Symbol-kind colors from a language server (types, functions, fields, ...),
// drawn on top of the parser's highlighting. The theme styles the
// `cm-semantic-<kind>` classes.

import { type ChangeDesc, type Extension, RangeSetBuilder, StateEffect, StateField } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView } from "@codemirror/view";

export type SemanticKind =
  | "type"
  | "valueType"
  | "function"
  | "field"
  | "constant"
  | "event"
  | "keyword"
  | "formatItem"
  | "plain";

export interface SemanticRange {
  from: number;
  to: number;
  kind: SemanticKind;
}

const marks = new Map<SemanticKind, Decoration>();
function mark(kind: SemanticKind): Decoration {
  let decoration = marks.get(kind);
  if (!decoration) marks.set(kind, (decoration = Decoration.mark({ class: `cm-semantic-${kind}` })));
  return decoration;
}

/** Replaces the symbol colors; ranges must be sorted and refer to the current document. */
export const setSemanticRanges = StateEffect.define<readonly SemanticRange[]>();

const semanticField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(decorations, transaction) {
    let next = decorations.map(transaction.changes);
    for (const effect of transaction.effects) {
      if (effect.is(setSemanticRanges)) next = build(effect.value, transaction.state.doc.length);
    }
    return next;
  },
  provide: (field) => EditorView.decorations.from(field),
});

function build(ranges: readonly SemanticRange[], length: number): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>();
  let last = 0;
  for (const { from, to, kind } of ranges) {
    if (from < last || to <= from || to > length) continue;
    builder.add(from, to, mark(kind));
    last = to;
  }
  return builder.finish();
}

export const semanticHighlight: Extension = semanticField;

/** The symbol colors currently shown in `view`, in document order. */
export function semanticRanges(view: EditorView): SemanticRange[] {
  const ranges: SemanticRange[] = [];
  const decorations = view.state.field(semanticField, false);
  decorations?.between(0, view.state.doc.length, (from, to, value) => {
    const kind = (value.spec.class as string).slice("cm-semantic-".length) as SemanticKind;
    ranges.push({ from, to, kind });
  });
  return ranges;
}

/** Maps ranges through `changes`, dropping the ones that were edited away. */
export function mapRanges(ranges: readonly SemanticRange[], changes: ChangeDesc): SemanticRange[] {
  const mapped: SemanticRange[] = [];
  for (const range of ranges) {
    if (changes.touchesRange(range.from, range.to)) continue;
    mapped.push({ from: changes.mapPos(range.from), to: changes.mapPos(range.to), kind: range.kind });
  }
  return mapped;
}
