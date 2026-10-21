import { type BlockInfo, BlockType } from "@codemirror/view";
import { describe, expect, it } from "vitest";
import { layoutMarks, type MarkKind, type OverviewBand, partAbove, textPart } from "./overviewRuler";

const band = (top: number, bottom: number, kind: MarkKind = "inserted", estimated = false): OverviewBand => ({
  top,
  bottom,
  kind,
  title: "",
  estimated,
  pick: () => {},
});

describe("layoutMarks", () => {
  it("places bands in proportion to the scrolling content", () => {
    // 2000px of content on a 500px scrollbar: a quarter.
    expect(layoutMarks([band(400, 480), band(1000, 1200, "deleted")], 2000, 500)).toEqual([
      { top: 100, height: 20, kind: "inserted", estimated: false, band: 0 },
      { top: 250, height: 50, kind: "deleted", estimated: false, band: 1 },
    ]);
  });

  it("gives one-line changes of long files a visible height", () => {
    const [mark] = layoutMarks([band(10_000, 10_020)], 100_000, 800);
    expect(mark.top).toBe(80);
    expect(mark.height).toBe(3);
  });

  it("keeps marks at the end inside the ruler", () => {
    const [mark] = layoutMarks([band(99_990, 100_000)], 100_000, 800);
    expect(mark.top + mark.height).toBe(800);
  });

  it("draws touching marks of one kind as one, which picks the first", () => {
    const marks = layoutMarks([band(1000, 1020), band(0, 20), band(1030, 1050), band(1040, 1060, "error")], 10_000, 1000);
    expect(marks).toEqual([
      { top: 0, height: 3, kind: "inserted", estimated: false, band: 1 },
      { top: 100, height: 6, kind: "inserted", estimated: false, band: 0 },
      { top: 104, height: 3, kind: "error", estimated: false, band: 3 },
    ]);
  });

  it("keeps estimated marks apart from measured ones", () => {
    const marks = layoutMarks([band(0, 100), band(100, 200, "inserted", true)], 1000, 1000);
    expect(marks.map((mark) => mark.estimated)).toEqual([false, true]);
  });

  it("has nothing to place without a scrollbar", () => {
    expect(layoutMarks([band(0, 10)], 0, 500)).toEqual([]);
    expect(layoutMarks([band(0, 10)], 500, 0)).toEqual([]);
  });
});

const block = (top: number, height: number, type: BlockType | BlockInfo[]) =>
  ({ top, height, bottom: top + height, type }) as unknown as BlockInfo;

describe("line block parts", () => {
  // Removed lines above the line, a discussion card below it.
  const removed = block(100, 40, BlockType.WidgetBefore);
  const text = block(140, 20, BlockType.Text);
  const card = block(160, 80, BlockType.WidgetAfter);
  const line = block(100, 140, [removed, text, card]);

  it("finds the line without the blocks around it", () => {
    expect(textPart(line)).toBe(text);
    expect(textPart(text)).toBe(text);
  });

  it("finds the block right above the line", () => {
    expect(partAbove(line)).toBe(removed);
    expect(partAbove(block(140, 100, [text, card]))).toBeNull();
    expect(partAbove(text)).toBeNull();
  });

  it("takes folded lines as the line", () => {
    const folded = block(0, 30, BlockType.WidgetRange);
    expect(textPart(block(0, 60, [folded, block(30, 30, BlockType.WidgetAfter)]))).toBe(folded);
  });
});
