import { EditorState, type StateCommand, Text, type Transaction } from "@codemirror/state";
import { describe, expect, it } from "vitest";
import { detectIndentUnit, insertIndent, outdent, tabIndentation } from "./tabIndent";

const doc = (...lines: string[]) => Text.of(lines);

describe("detectIndentUnit", () => {
  it("finds the step between indentation levels", () => {
    expect(detectIndentUnit(doc("let f x =", "    let y = x", "    y"))).toBe("    ");
    expect(detectIndentUnit(doc("{", "  a: 1,", "  b: {", "    c: 2", "  }", "}"))).toBe("  ");
  });

  it("uses tabs when the file does", () => {
    expect(detectIndentUnit(doc("all:", "\tmake a", "\tmake b"))).toBe("\t");
  });

  it("falls back to four spaces", () => {
    expect(detectIndentUnit(doc("module A", "let x = 1"))).toBe("    ");
  });
});

/** An editor without a view: commands run on its state. */
function editor(text: string, anchor: number, head = anchor, extensions = tabIndentation(Text.of(text.split("\n")))) {
  const target = {
    state: EditorState.create({ doc: text, selection: { anchor, head }, extensions }),
    dispatch: (transaction: Transaction) => void (target.state = transaction.state),
    run: (command: StateCommand) => command(target),
  };
  return target;
}

describe("Tab", () => {
  it("indents to the next stop at the cursor", () => {
    const view = editor("let f x =\nx", 10);
    view.run(insertIndent);
    expect(view.state.doc.toString()).toBe("let f x =\n    x");
    // Mid-line, from column 2 to column 4.
    const mid = editor("ab", 2);
    mid.run(insertIndent);
    expect(mid.state.doc.toString()).toBe("ab  ");
  });

  it("inserts a tab in a file indented with tabs", () => {
    const view = editor("all:\n\tmake\n", 5);
    view.run(insertIndent);
    expect(view.state.doc.toString()).toBe("all:\n\t\tmake\n");
  });

  it("indents the selected lines", () => {
    const view = editor("a\nb\n    c", 0, 3);
    view.run(insertIndent);
    expect(view.state.doc.toString()).toBe("    a\n    b\n    c");
    view.run(outdent);
    expect(view.state.doc.toString()).toBe("a\nb\n    c");
  });

  it("leaves a read-only editor alone", () => {
    const view = editor("a", 0, 0, [EditorState.readOnly.of(true)]);
    expect(view.run(insertIndent)).toBe(false);
    expect(view.run(outdent)).toBe(false);
  });
});
