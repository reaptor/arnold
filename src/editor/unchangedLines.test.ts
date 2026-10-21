import { getChunks, uncollapseUnchanged, unifiedMergeView } from "@codemirror/merge";
import { EditorState, type StateEffect } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { describe, expect, it } from "vitest";
import {
  canCollapseUnchanged,
  collapseAllUnchanged,
  collapseUnchangedAt,
  otherSidePos,
  unchangedLines,
  unchangedRuns,
} from "./unchangedLines";

const config = { margin: 3, minSize: 8 };

describe("unchangedRuns", () => {
  it("folds what lies beyond the margin around each change", () => {
    // Changes on line 20 and on lines 50 to 51 (ending on line 52) of 100 lines.
    expect(
      unchangedRuns(
        100,
        [
          { from: 20, to: 21 },
          { from: 50, to: 52 },
        ],
        config,
      ),
    ).toEqual([
      { from: 1, to: 16 },
      { from: 24, to: 46 },
      { from: 55, to: 100 },
    ]);
  });

  it("leaves runs shorter than the minimum shown", () => {
    expect(unchangedRuns(20, [{ from: 10, to: 11 }], config)).toEqual([]);
    expect(unchangedRuns(30, [{ from: 12, to: 13 }], config)).toEqual([
      { from: 1, to: 8 },
      { from: 16, to: 30 },
    ]);
  });

  it("folds all of a file without changes", () => {
    expect(unchangedRuns(10, [], config)).toEqual([{ from: 1, to: 10 }]);
  });
});

describe("otherSidePos", () => {
  // Side A has 10 characters removed at 20; side B has 5 added at 40 (A's 30).
  const chunks = [
    { fromA: 20, toA: 30, fromB: 20, toB: 20 },
    { fromA: 40, toA: 40, fromB: 30, toB: 35 },
  ];

  it("offsets unchanged text by the changes before it", () => {
    expect(otherSidePos(10, chunks, true)).toBe(10);
    expect(otherSidePos(35, chunks, true)).toBe(25);
    expect(otherSidePos(50, chunks, true)).toBe(45);
    expect(otherSidePos(25, chunks, false)).toBe(35);
    expect(otherSidePos(45, chunks, false)).toBe(50);
  });
});

const lines = (count: number, change?: (line: number) => string | null) =>
  Array.from({ length: count }, (_, index) => change?.(index + 1) ?? `line ${index + 1}`).join("\n");

/** A unified view's state: line 30 of 60 changed. */
function unified() {
  const base = lines(60);
  const working = lines(60, (line) => (line === 30 ? "changed" : null));
  return EditorState.create({ doc: working, extensions: [unchangedLines(config), unifiedMergeView({ original: base })] });
}

/** The folded runs and the runs with a "Hide" bar, as line ranges. */
function shown(state: EditorState) {
  const folded: string[] = [];
  const bars: string[] = [];
  for (const source of state.facet(EditorView.decorations)) {
    if (typeof source === "function") continue;
    source.between(0, state.doc.length, (from, to, decoration) => {
      const widget = decoration.spec.widget?.constructor.name;
      const lineOf = (pos: number) => state.doc.lineAt(pos).number;
      if (widget === "FoldedWidget") folded.push(`${lineOf(from)}-${lineOf(to)}`);
      if (widget === "HideWidget") bars.push(`${lineOf(from)}`);
    });
  }
  return { folded, bars };
}

const run = (state: EditorState, ...effects: StateEffect<unknown>[]) => state.update({ effects }).state;

describe("unchangedLines", () => {
  it("folds the unchanged runs and folds an unfolded one again", () => {
    let state = unified();
    expect(getChunks(state)?.chunks).toHaveLength(1);
    expect(shown(state)).toEqual({ folded: ["1-26", "34-60"], bars: [] });

    state = run(state, uncollapseUnchanged.of(state.doc.line(34).from));
    expect(shown(state)).toEqual({ folded: ["1-26"], bars: ["34"] });
    expect(canCollapseUnchanged(state, [])).toBe(true);

    state = run(state, collapseUnchangedAt.of(state.doc.line(34).from));
    expect(shown(state)).toEqual({ folded: ["1-26", "34-60"], bars: [] });
    expect(canCollapseUnchanged(state, [])).toBe(false);
  });

  it("folds everything again but the runs kept", () => {
    let state = unified();
    state = run(state, uncollapseUnchanged.of(0), uncollapseUnchanged.of(state.doc.line(34).from));
    expect(shown(state).bars).toEqual(["1", "34"]);
    const kept = state.doc.line(40).to;
    expect(canCollapseUnchanged(state, [kept])).toBe(true);
    state = run(state, collapseAllUnchanged.of([kept]));
    expect(shown(state)).toEqual({ folded: ["1-26"], bars: ["34"] });
    expect(canCollapseUnchanged(state, [kept])).toBe(false);
  });

  it("splits an unfolded run where it is edited, offering to hide only unchanged lines", () => {
    let state = unified();
    state = run(state, uncollapseUnchanged.of(state.doc.line(34).from));
    const line = state.doc.line(47);
    state = state.update({ changes: { from: line.from, to: line.to, insert: "edited" } }).state;
    expect(getChunks(state)?.chunks).toHaveLength(2);
    // 34-43 above the edit; 51-60 below it.
    expect(shown(state)).toEqual({ folded: ["1-26"], bars: ["34", "51"] });
    state = run(state, collapseUnchangedAt.of(state.doc.line(55).from));
    expect(shown(state)).toEqual({ folded: ["1-26", "51-60"], bars: ["34"] });
  });

  it("unfolds folded lines that are edited, without folding anything new", () => {
    let state = unified();
    const line = state.doc.line(10);
    state = state.update({ changes: { from: line.from, to: line.to, insert: "edited" } }).state;
    // Lines 1-6 are too few to fold; 14-26 were folded with the edited line.
    expect(shown(state)).toEqual({ folded: ["34-60"], bars: ["14"] });
  });
});
