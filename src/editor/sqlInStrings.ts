// SQL inside string literals of other languages (F#, C#, ...), highlighted
// as T-SQL the way Rider injects it: a string whose text starts with an SQL
// statement (`SELECT ... FROM`, `INSERT INTO`, `UPDATE ... SET`, ...) gets
// the SQL parser's colors on top of the string color. Interpolation holes
// (`{id}` in `$"..."`, `${id}` in a template literal) keep their own colors.

import { MSSQL } from "@codemirror/lang-sql";
import { highlightingFor, syntaxTree } from "@codemirror/language";
import { type EditorState, type Extension, Prec, type Range } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, ViewPlugin, type ViewUpdate } from "@codemirror/view";
import { highlightTree, type Tag } from "@lezer/highlight";

/** A string literal's text, with where the text between its quotes is. */
export interface StringLiteral {
  from: number;
  to: number;
  /** Positions in the document. */
  contentFrom: number;
  contentTo: number;
}

/** Statements an SQL string starts with; the keyword in one case (`SELECT`, `select`), not `Select` as in a sentence. */
const STATEMENT =
  /^(?:select\b[\s\S]*\bfrom\b|insert\s+into\b|update\s+[\w[\]."@#]+\s+set\b|delete\s+(?:top\s*\(\s*\d+\s*\)\s+)?from\b|merge\s+(?:top\s*\(\s*\d+\s*\)\s+)?(?:into\s+)?[\w[\]."@#]+\s+using\b|with\s+[\w[\]"]+(?:\s*\([^)]*\))?\s+as\s*\(|(?:create|alter|create\s+or\s+alter)\s+(?:table|view|proc|procedure|function|trigger|index|unique|clustered|nonclustered|schema|type)\b|drop\s+(?:table|view|proc|procedure|function|trigger|index|schema|type)\b|truncate\s+table\b|exec(?:ute)?\s+[\w[\]."@#]+|declare\s+@|if\s+(?:not\s+)?exists\s*\(\s*select\b|set\s+(?:nocount|xact_abort|ansi_nulls|quoted_identifier)\b)/i;

/** Line and block comments before the statement. */
const LEADING = /^(?:\s+|--[^\n]*|\/\*[\s\S]*?\*\/)*/;

/** Whether `text` (between the quotes) is an SQL statement. */
export function looksLikeSql(text: string): boolean {
  const start = text.slice(LEADING.exec(text)![0].length);
  if (!STATEMENT.test(start)) return false;
  const keyword = /^\w+/.exec(start)![0];
  return keyword === keyword.toUpperCase() || keyword === keyword.toLowerCase();
}

/** Opening prefix and quotes: `"`, `@"`, `$"`, `$@"`, `"""`, `$"""`, `'`, `` ` ``, `f"`, `r"`, ... */
const OPENING = /^([A-Za-z$@]{0,3})("""|'''|"|'|`)/;

/**
 * The interpolation holes of a string, as ranges of `text`: `{...}` when
 * the prefix has `$` or `f` (F#, C#, Python), `${...}` in a template literal.
 */
export function holesOf(text: string, prefix: string, quote: string): [number, number][] {
  const template = quote === "`";
  if (!template && !/[$fF]/.test(prefix)) return [];
  const holes: [number, number][] = [];
  for (let i = 0; i < text.length; i++) {
    if (template ? !text.startsWith("${", i) : text[i] !== "{") continue;
    // `{{` is a brace, not a hole.
    if (!template && text[i + 1] === "{") {
      i++;
      continue;
    }
    let depth = 0;
    let end = i;
    for (; end < text.length; end++) {
      if (text[end] === "{") depth++;
      else if (text[end] === "}" && --depth === 0) break;
    }
    holes.push([i, Math.min(end + 1, text.length)]);
    i = end;
  }
  return holes;
}

/** Where the text of a literal starts and ends, relative to it; null when it does not start with a quote. */
function quotesOf(text: string): { open: RegExpExecArray; contentTo: number; closed: boolean } | null {
  const open = OPENING.exec(text);
  if (!open) return null;
  const quote = open[2];
  const closed = text.length >= open[0].length + quote.length && text.endsWith(quote);
  return { open, contentTo: closed ? text.length - quote.length : text.length, closed };
}

/**
 * The string literals in `from`..`to`. A string over several lines can come
 * as one token per line (F#'s parser): an unclosed one takes in the tokens
 * on the lines after it.
 */
export function stringLiterals(state: EditorState, from: number, to: number): StringLiteral[] {
  const pieces: { from: number; to: number }[] = [];
  const unclosed = (piece: { from: number; to: number }) => !quotesOf(state.sliceDoc(piece.from, piece.to))?.closed;
  syntaxTree(state).iterate({
    from,
    to,
    enter(node) {
      if (!/string/i.test(node.name)) return;
      // F#'s parser leaves the `$` and `@` before the quote out of the string.
      let start = node.from;
      while (start > 0 && /[$@]/.test(state.sliceDoc(start - 1, start))) start--;
      const last = pieces.at(-1);
      if (last && /^[^\S\n]*\n\s*$/.test(state.sliceDoc(last.to, node.from)) && unclosed(last)) last.to = node.to;
      else pieces.push({ from: start, to: node.to });
      return false;
    },
  });
  const literals: StringLiteral[] = [];
  for (const piece of pieces) {
    const quotes = quotesOf(state.sliceDoc(piece.from, piece.to));
    if (!quotes) continue;
    literals.push({
      from: piece.from,
      to: piece.to,
      contentFrom: piece.from + quotes.open[0].length,
      contentTo: piece.from + quotes.contentTo,
    });
  }
  return literals;
}

const sqlParser = MSSQL.language.parser;
/** SQL text the parser gives no color: the editor's text color rather than the string's. */
const sqlText = Decoration.mark({ class: "cm-sql-text" });
const marks = new Map<string, Decoration>();
const markFor = (classes: string) => {
  let mark = marks.get(classes);
  if (!mark) marks.set(classes, (mark = Decoration.mark({ class: classes })));
  return mark;
};

/**
 * The SQL colors of the visible strings. The marks do not overlap: each
 * one alone decides the color inside the string's.
 */
function injectedSql(view: EditorView): DecorationSet {
  const state = view.state;
  const ranges: Range<Decoration>[] = [];
  const highlighter = { style: (tags: readonly Tag[]) => highlightingFor(state, tags) };
  let done = -1;
  for (const visible of view.visibleRanges) {
    for (const literal of stringLiterals(state, visible.from, visible.to)) {
      // A string reaching over two visible ranges is seen twice.
      if (literal.from < done) continue;
      done = literal.to;
      const content = state.sliceDoc(literal.contentFrom, literal.contentTo);
      if (!looksLikeSql(content)) continue;
      const opening = OPENING.exec(state.sliceDoc(literal.from, literal.contentFrom))!;
      const holes = holesOf(content, opening[1], opening[2]);
      // The parser sees a name in place of each hole.
      let masked = content;
      for (const [from, to] of holes) masked = masked.slice(0, from) + "_".repeat(to - from) + masked.slice(to);
      const outside = (from: number, to: number) => holes.every(([start, end]) => to <= start || from >= end);
      const covered: [number, number][] = [...holes];
      highlightTree(sqlParser.parse(masked), highlighter, (from, to, classes) => {
        if (!outside(from, to)) return;
        covered.push([from, to]);
        ranges.push(markFor(classes).range(literal.contentFrom + from, literal.contentFrom + to));
      });
      covered.sort((a, b) => a[0] - b[0]);
      let at = 0;
      for (const [from, to] of [...covered, [content.length, content.length]]) {
        if (from > at) ranges.push(sqlText.range(literal.contentFrom + at, literal.contentFrom + from));
        at = Math.max(at, to);
      }
    }
  }
  return Decoration.set(ranges, true);
}

const injection = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;
    constructor(view: EditorView) {
      this.decorations = injectedSql(view);
    }
    update(update: ViewUpdate) {
      if (update.docChanged || update.viewportChanged || syntaxTree(update.state) !== syntaxTree(update.startState)) {
        this.decorations = injectedSql(update.view);
      }
    }
  },
);

/**
 * SQL highlighting in the string literals of any language. The highest
 * precedence draws its marks inside the string's own (the language's
 * highlighting is `Prec.high`), so their colors win.
 */
export const sqlInStrings: Extension = [
  injection,
  Prec.highest(EditorView.decorations.of((view) => view.plugin(injection)?.decorations ?? Decoration.none)),
  EditorView.baseTheme({
    "&light .cm-sql-text": { color: "#080808" },
    "&dark .cm-sql-text": { color: "#bdbdbd" },
  }),
];
