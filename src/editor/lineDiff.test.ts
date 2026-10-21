import { type Change, Chunk, type DiffConfig } from "@codemirror/merge";
import { Text } from "@codemirror/state";
import { describe, expect, it, vi } from "vitest";
import {
  ignoringWhitespaceDiffConfig,
  joinAcrossBlankLines,
  type LineBlock,
  lineBlocks,
  lineFirstDiff,
  lineFirstDiffConfig,
  placeBlocks,
  takeInBlankLines,
  withDifferingLines,
} from "./lineDiff";

/** Applies the changes to `a`, which must give `b`. */
function apply(a: string, b: string, changes: readonly Change[]) {
  let out = "";
  let pos = 0;
  for (const change of changes) {
    out += a.slice(pos, change.fromA) + b.slice(change.fromB, change.toB);
    pos = change.toA;
  }
  return out + a.slice(pos);
}

function chunks(a: string, b: string, config: DiffConfig = lineFirstDiffConfig) {
  return Chunk.build(Text.of(a.split("\n")), Text.of(b.split("\n")), config);
}

/** Changed lines on the working side, 1-based, one entry per chunk. */
function changedLines(a: string, b: string, config?: DiffConfig) {
  const doc = Text.of(b.split("\n"));
  return chunks(a, b, config).map((chunk) => [doc.lineAt(chunk.fromB).number, doc.lineAt(chunk.endB).number]);
}

// Deterministic pseudo-random numbers.
function random(seed: number) {
  return (n: number) => ((seed = (seed * 16807) % 2147483647), seed % n);
}

/** A large source file with varied lines, of the kind that broke the character diff. */
function sourceFile(lineCount: number) {
  const rand = random(42);
  const word = () => ["value", "count", "items", "result", "name", "index", "total", "state"][rand(8)] + rand(100);
  const lines: string[] = [];
  for (let i = 0; i < lineCount; i++) {
    const indent = "    ".repeat(rand(4));
    const kind = rand(5);
    if (kind === 0) lines.push("");
    else if (kind === 1) lines.push(`${indent}let ${word()} = ${word()} + ${rand(1000)}`);
    else if (kind === 2) lines.push(`${indent}if ${word()} > ${word()} then`);
    else lines.push(`${indent}printfn "%d %s" ${word()} ${word()}`);
  }
  return lines.join("\n");
}

const bigFile = sourceFile(12000);

describe("lineFirstDiff", () => {
  it("describes exactly the difference", () => {
    const cases: [string, string][] = [
      ["", ""],
      ["", "a"],
      ["a", ""],
      ["a", "a\n"],
      ["a\n", "a"],
      ["a\nb", "a\nc"],
      ["a\nb", "a\nb\nc"],
      ["a\nb\nc", "c"],
      ["x\ny\n", "y\nx\n"],
      ["let a = 1\nlet b = 2\n", "let a = 10\nlet b = 2\n"],
    ];
    for (const [a, b] of cases) expect(apply(a, b, lineFirstDiff(a, b)), JSON.stringify([a, b])).toBe(b);
  });

  it("describes exactly the difference after random edits", () => {
    const rand = random(7);
    const base = bigFile.split("\n").slice(0, 400);
    for (let round = 0; round < 50; round++) {
      const lines = [...base];
      for (let i = 0; i < 1 + rand(8); i++) {
        const at = rand(lines.length);
        const kind = rand(3);
        if (kind === 0) lines.splice(at, 1);
        else if (kind === 1) lines.splice(at, 0, "inserted " + round);
        else lines[at] = lines[at].slice(0, rand(lines[at].length + 1)) + "edit";
      }
      const a = base.join("\n");
      const b = lines.join("\n");
      expect(apply(a, b, lineFirstDiff(a, b))).toBe(b);
    }
  });

  it("marks only the edited lines when small edits are spread over a large file", () => {
    const lines = bigFile.split("\n");
    const edited = [100, 2000, 4000, 6000, 8000, 10000, 11900].filter((at) => lines[at].length > 5);
    for (const at of edited) lines[at] = lines[at].slice(0, 3) + "renamed" + lines[at].slice(6);
    const working = lines.join("\n");
    expect(changedLines(bigFile, working)).toEqual(edited.map((at) => [at + 1, at + 1]));
    // CodeMirror's own character diff marks thousands of unchanged lines here.
    expect(changedLines(bigFile, working, { scanLimit: 500 }).length).toBeLessThan(edited.length);
  });

  it("highlights only the changed characters within a line", () => {
    const a = "let total = price * quantity\nprintfn \"%d\" total\n";
    const b = "let total = price * amount\nprintfn \"%d\" total\n";
    const [chunk] = chunks(a, b);
    expect(chunk.changes.map((change) => b.slice(chunk.fromB + change.fromB, chunk.fromB + change.toB))).toEqual([
      "amount",
    ]);
  });

  it("stays fast when every line of a large file changed", () => {
    const rewritten = bigFile
      .split("\n")
      .map((line: string, i: number) => `${i}:${line}`)
      .join("\n");
    const started = performance.now();
    const changes = lineFirstDiff(bigFile, rewritten);
    expect(performance.now() - started).toBeLessThan(3000);
    expect(apply(bigFile, rewritten, changes)).toBe(rewritten);
  });
});

describe("placeBlocks", () => {
  const blocks = (a: string[], b: string[], pick: (positions: LineBlock[], diffed: LineBlock) => LineBlock) =>
    placeBlocks(lineBlocks(a, b), a, b, pick);

  it("offers every place a new empty line next to empty lines could be", () => {
    const a = ["run", "", "from"];
    const b = ["run", "", "", "from"];
    const offered: number[][] = [];
    blocks(a, b, (positions, diffed) => {
      offered.push(positions.map((p) => p.fromB));
      return diffed;
    });
    expect(offered).toEqual([[1, 2]]);
  });

  it("puts the block where it is picked, both sides moving together", () => {
    expect(blocks(["run", "", "from"], ["run", "", "", "from"], (positions) => positions[0])).toEqual([
      { fromA: 1, toA: 1, fromB: 1, toB: 2 },
    ]);
  });

  it("slides removed lines too", () => {
    const offered: number[][] = [];
    blocks(["a", "", "", "", "b"], ["a", "", "b"], (positions, diffed) => {
      offered.push(positions.map((p) => p.fromA));
      return diffed;
    });
    // Two of three empty lines: starting at the first or the second.
    expect(offered).toEqual([[1, 2]]);
  });

  it("does not slide into a neighbouring change", () => {
    const offered: number[][] = [];
    // The first line changed; the empty line added after it cannot move above it.
    blocks(["x", "", "y"], ["z", "", "", "y"], (positions, diffed) => {
      offered.push(positions.map((p) => p.fromB));
      return diffed;
    });
    expect(offered.every((starts) => starts.every((start) => start >= 1))).toBe(true);
  });

  it("leaves blocks that cannot slide alone", () => {
    const pick = vi.fn((_positions: LineBlock[], diffed: LineBlock) => diffed);
    expect(blocks(["a", "b"], ["a", "new", "b"], pick)).toEqual([{ fromA: 1, toA: 1, fromB: 1, toB: 2 }]);
    expect(pick).not.toHaveBeenCalled();
  });
});

describe("joinAcrossBlankLines", () => {
  it("joins changes that only empty lines separate", () => {
    // The empty line after "delegate" matches the file's last (empty) line.
    const a = ["// Program.fs", ""];
    const b = ["// Program.fs", "delegate", "", "", "rffr", "", "jk"];
    expect(joinAcrossBlankLines(lineBlocks(a, b), b)).toEqual([{ fromA: 1, toA: 2, fromB: 1, toB: 7 }]);
  });

  it("keeps changes apart that text separates", () => {
    const a = ["a", "b", "c"];
    const b = ["x", "b", "y"];
    expect(joinAcrossBlankLines(lineBlocks(a, b), b)).toHaveLength(2);
  });
});

describe("takeInBlankLines", () => {
  it("grows a block over the empty lines it takes, on both sides", () => {
    const a = ["a", "", "b"];
    const b = ["a", "new", "", "b"];
    expect(takeInBlankLines(lineBlocks(a, b), a, b, (line) => line === 2)).toEqual([
      { fromA: 1, toA: 2, fromB: 1, toB: 3 },
    ]);
  });

  it("takes no line with text, nor lines it is not given", () => {
    const a = ["a", "", "b"];
    const b = ["a", "new", "", "b"];
    expect(takeInBlankLines(lineBlocks(a, b), a, b, () => false)).toEqual(lineBlocks(a, b));
    expect(takeInBlankLines(lineBlocks(a, b), a, b, (line) => line === 0)).toEqual(lineBlocks(a, b));
  });
});

describe("ignoring whitespace", () => {
  const a = "let f x =\n    x + 1\n\nlet g = f 2\n";
  // Wrapped in a module: indented, with a line added before.
  const b = "module M =\n  let f x =\n      x + 1\n\n  let g = f 2\n";

  it("leaves lines that differ only in whitespace unchanged", () => {
    expect(changedLines(a, b)).toEqual([[1, 5]]);
    expect(changedLines(a, b, ignoringWhitespaceDiffConfig)).toEqual([[1, 1]]);
  });

  it("shows no changed spaces within a changed line", () => {
    const [chunk] = chunks("let x = a+b\n", "let  x = a + c\n", ignoringWhitespaceDiffConfig);
    const text = "let  x = a + c\n";
    expect(chunk.changes.map((change) => text.slice(chunk.fromB + change.fromB, chunk.fromB + change.toB))).toEqual(["c"]);
  });

  it("still shows added and removed blank lines", () => {
    expect(changedLines("a\nb\n", "a\n\nb\n", ignoringWhitespaceDiffConfig)).toEqual([[2, 2]]);
    expect(changedLines("a\n\nb\n", "a\nb\n", ignoringWhitespaceDiffConfig)).toHaveLength(1);
  });

  it("diffs the whole texts again after an edit", () => {
    // Long enough that CodeMirror's update would diff only around the edit.
    const indented = bigFile.split("\n").slice(0, 2000);
    const docA = Text.of(indented);
    const docB = Text.of(indented.map((line) => "  " + line));
    const built = Chunk.build(docA, docB, ignoringWhitespaceDiffConfig);
    expect(built).toEqual([]);
    const at = docB.line(1500).to;
    const edited = docB.replace(at, at, Text.of([" // note"]));
    const changes = { iterChangedRanges: (f: (fromA: number, toA: number, fromB: number, toB: number) => void) => f(at, at, at, at + 8) };
    const updated = Chunk.updateB(built, docA, edited, changes as never, ignoringWhitespaceDiffConfig);
    expect(updated.map((chunk) => [edited.lineAt(chunk.fromB).number, edited.lineAt(chunk.endB).number])).toEqual([
      [1500, 1500],
    ]);
  });

  it("marks where unchanged lines that differ in whitespace start and end, for lining the sides up", () => {
    const docA = Text.of(a.split("\n"));
    const docB = Text.of(b.split("\n"));
    const points = withDifferingLines(Chunk.build(docA, docB, ignoringWhitespaceDiffConfig), docA, docB).map(
      (chunk) => [docA.lineAt(chunk.fromA).number, docB.lineAt(chunk.fromB).number, chunk.toB > chunk.fromB],
    );
    // The added line (which ends where "let f" starts), then around "let f", "x + 1" (the blank
    // line after it is the same on both sides) and "let g".
    expect(points).toEqual([
      [1, 1, true],
      [2, 3, false],
      [3, 4, false],
      [4, 5, false],
      [5, 6, false],
    ]);
  });
});
