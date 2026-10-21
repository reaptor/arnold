// Long runs of unchanged lines folded into a "42 unchanged lines" bar, as
// @codemirror/merge's collapseUnchanged does, but both ways: a run unfolded by
// clicking its bar gets a slim "Hide 42 unchanged lines" bar at its top that
// folds it again, and the whole file can be folded back as it was opened.
//
// The merge library's own folding only ever unfolds (its state field can only
// be given back all its folds, by being configured again), so this replaces
// it: the same runs, the same bar (and its class), and the library's
// own uncollapseUnchanged effect unfolds here too. What it relies on is
// exported: getChunks, mergeViewSiblings and uncollapseUnchanged.
//
// The runs are worked out from the changes as they are now. Lines edited in
// an unfolded run are a change of their own, so they split the run in two
// (each with its own bar, if still long enough); nothing is ever folded
// without being asked, so text never folds away under the cursor.
//
// In the split view both sides fold and unfold together, so they stay lined up.

import { type Chunk, getChunks, mergeViewSiblings, uncollapseUnchanged } from "@codemirror/merge";
import { type EditorState, type Extension, Facet, type Range, StateEffect, StateField } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, WidgetType } from "@codemirror/view";

/** Unchanged lines within `margin` of a change stay shown; runs shorter than `minSize` are not folded. */
export interface FoldConfig {
  margin: number;
  minSize: number;
}

/** Lines `from` to `to`, 1-based and inclusive. */
export interface LineSpan {
  from: number;
  to: number;
}

/**
 * The runs of unchanged lines that fold, in a text of `lineCount` lines with
 * `changes` (each from the line it starts on to the line it ends on, as
 * @codemirror/merge counts them: the line after its last one).
 */
export function unchangedRuns(lineCount: number, changes: readonly LineSpan[], { margin, minSize }: FoldConfig): LineSpan[] {
  const runs: LineSpan[] = [];
  let after = 1;
  for (let index = 0; index <= changes.length; index++) {
    const change = index < changes.length ? changes[index] : null;
    const from = index > 0 ? after + margin : 1;
    const to = change ? change.from - 1 - margin : lineCount;
    if (to - from + 1 >= minSize) runs.push({ from, to });
    if (change) after = change.to;
  }
  return runs;
}

/** Where `pos` of one side of a split view is on the other: unchanged text is offset by the changes before it. */
export function otherSidePos(pos: number, chunks: readonly Pick<Chunk, "fromA" | "toA" | "fromB" | "toB">[], fromA: boolean): number {
  let ours = 0;
  let theirs = 0;
  for (const chunk of chunks) {
    if ((fromA ? chunk.fromA : chunk.fromB) >= pos) break;
    [ours, theirs] = fromA ? [chunk.toA, chunk.toB] : [chunk.toB, chunk.toA];
  }
  return theirs + (pos - ours);
}

const foldConfig = Facet.define<FoldConfig, FoldConfig>({ combine: (values) => values[0] ?? { margin: 3, minSize: 4 } });

/** Folds the unchanged run at a position (one shown with its "Hide" bar). */
export const collapseUnchangedAt = StateEffect.define<number>({ map: (pos, changes) => changes.mapPos(pos) });

/** Folds every unchanged run again, except those holding one of the positions given. */
export const collapseAllUnchanged = StateEffect.define<readonly number[]>({
  map: (positions, changes) => positions.map((pos) => changes.mapPos(pos)),
});

interface Run {
  from: number;
  to: number;
  lines: number;
}

/** The text's runs of unchanged lines that fold, as positions; none until the changes are known. */
function runsOf(state: EditorState): Run[] {
  const found = getChunks(state);
  if (!found) return [];
  const onA = found.side === "a";
  const doc = state.doc;
  const changes = found.chunks.map((chunk) => ({
    from: doc.lineAt(onA ? chunk.fromA : chunk.fromB).number,
    to: doc.lineAt(Math.min(doc.length, onA ? chunk.toA : chunk.toB)).number,
  }));
  return unchangedRuns(doc.lines, changes, state.facet(foldConfig)).map((run) => ({
    from: doc.line(run.from).from,
    to: doc.line(run.to).to,
    lines: run.to - run.from + 1,
  }));
}

const isFolded = (folds: DecorationSet, run: Run) => {
  let folded = false;
  folds.between(run.from, run.to, (from, to) => {
    if (from > run.to || to < run.from) return;
    folded = true;
    return false;
  });
  return folded;
};

/** Dispatches `effect` (made for `pos` of the view's own text) to `view` and, in the split view, to the other side. */
function onBothSides(view: EditorView, pos: number, effect: (pos: number) => StateEffect<unknown>) {
  view.dispatch({ effects: effect(pos) });
  const siblings = mergeViewSiblings(view);
  const chunks = getChunks(view.state);
  if (!siblings || !chunks) return;
  const fromA = siblings.a === view;
  (fromA ? siblings.b : siblings.a).dispatch({ effects: effect(otherSidePos(pos, chunks.chunks, fromA)) });
}

class FoldedWidget extends WidgetType {
  constructor(readonly lines: number) {
    super();
  }

  eq(other: FoldedWidget) {
    return other.lines === this.lines;
  }

  toDOM(view: EditorView) {
    const bar = document.createElement("div");
    bar.className = "cm-collapsedLines";
    bar.textContent = view.state.phrase("$ unchanged lines", this.lines);
    bar.addEventListener("click", (event) =>
      onBothSides(view, view.posAtDOM(event.target as HTMLElement), (pos) => uncollapseUnchanged.of(pos)),
    );
    return bar;
  }

  ignoreEvent(event: Event) {
    return event instanceof MouseEvent;
  }

  get estimatedHeight() {
    return 27;
  }
}

/** The bar at the top of an unfolded run that folds it again. */
class HideWidget extends WidgetType {
  constructor(readonly lines: number) {
    super();
  }

  eq(other: HideWidget) {
    return other.lines === this.lines;
  }

  toDOM(view: EditorView) {
    const bar = document.createElement("div");
    bar.className = "cm-hideUnchanged";
    bar.textContent = `Hide ${this.lines} unchanged ${this.lines === 1 ? "line" : "lines"}`;
    bar.addEventListener("click", (event) =>
      onBothSides(view, view.posAtDOM(event.target as HTMLElement), (pos) => collapseUnchangedAt.of(pos)),
    );
    return bar;
  }

  ignoreEvent(event: Event) {
    return event instanceof MouseEvent;
  }

  get estimatedHeight() {
    return 19;
  }
}

const fold = (run: Run) => Decoration.replace({ widget: new FoldedWidget(run.lines), block: true }).range(run.from, run.to);

interface Folding {
  /** The folded runs. */
  folds: DecorationSet;
  /** A "Hide" bar on each run shown. */
  bars: DecorationSet;
}

function withBars(state: EditorState, folds: DecorationSet): Folding {
  const bars: Range<Decoration>[] = runsOf(state)
    .filter((run) => !isFolded(folds, run))
    .map((run) => Decoration.widget({ widget: new HideWidget(run.lines), block: true, side: -1 }).range(run.from));
  return { folds, bars: Decoration.set(bars) };
}

const foldAll = (state: EditorState, keep: readonly number[] = []) =>
  Decoration.set(runsOf(state).filter((run) => !keep.some((pos) => pos >= run.from && pos <= run.to)).map(fold));

const foldingField = StateField.define<Folding>({
  create: (state) => withBars(state, foldAll(state)),
  update(folding, tr) {
    const chunks = getChunks(tr.state)?.chunks;
    const before = getChunks(tr.startState)?.chunks;
    // The split view knows its changes only once both sides exist: then they fold.
    if (chunks && !before) return withBars(tr.state, foldAll(tr.state));
    let folds = folding.folds.map(tr.changes);
    for (const effect of tr.effects) {
      if (effect.is(uncollapseUnchanged)) {
        folds = folds.update({ filter: (from) => from !== effect.value });
      } else if (effect.is(collapseAllUnchanged)) {
        folds = foldAll(tr.state, effect.value);
      } else if (effect.is(collapseUnchangedAt)) {
        const run = runsOf(tr.state).find((candidate) => effect.value >= candidate.from && effect.value <= candidate.to);
        if (run && !isFolded(folds, run)) folds = folds.update({ add: [fold(run)] });
      }
    }
    // A change made inside folded lines unfolds them, as the merge library's folding does.
    if (folds.size > 0 && chunks && chunks !== before) {
      const onA = getChunks(tr.state)?.side === "a";
      const touched = new Set<number>();
      for (const chunk of chunks) {
        folds.between(onA ? chunk.fromA : chunk.fromB, onA ? chunk.toA : chunk.toB, (from) => void touched.add(from));
      }
      if (touched.size > 0) folds = folds.update({ filter: (from) => !touched.has(from) });
    }
    if (folds === folding.folds && chunks === before && !tr.docChanged) return folding;
    return withBars(tr.state, folds);
  },
  provide: (field) => [
    EditorView.decorations.from(field, (folding) => folding.folds),
    EditorView.decorations.from(field, (folding) => folding.bars),
  ],
});

/** Folds long runs of unchanged lines of a merge view's editor (each side of the split view has it). */
export function unchangedLines(config: FoldConfig): Extension {
  return [foldConfig.of(config), foldingField];
}

/** Whether the text folds its unchanged lines. */
export const foldsUnchanged = (state: EditorState) => state.field(foldingField, false) !== undefined;

/** Whether folding every run again, but those holding `keep`, would fold any. */
export function canCollapseUnchanged(state: EditorState, keep: readonly number[]): boolean {
  const folds = state.field(foldingField, false)?.folds;
  return !!folds && runsOf(state).some((run) => !isFolded(folds, run) && !keep.some((pos) => pos >= run.from && pos <= run.to));
}

/** Unfolds the runs holding any of `positions`. */
export function revealUnchanged(view: EditorView, positions: readonly number[]) {
  const starts = new Set<number>();
  view.state.field(foldingField, false)?.folds.between(0, view.state.doc.length, (from, to) => {
    if (positions.some((pos) => pos >= from && pos <= to)) starts.add(from);
  });
  if (starts.size > 0) view.dispatch({ effects: [...starts].map((start) => uncollapseUnchanged.of(start)) });
}
