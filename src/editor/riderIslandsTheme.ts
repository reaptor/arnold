// JetBrains Rider's "Rider Islands Dark" editor scheme for CodeMirror: the
// scheme Rider picks with the Islands Dark UI theme. It is the Rider Dark
// palette (blue keywords, purple types, teal methods) on the Islands
// background, unlike IntelliJ's orange "Islands Dark". Colors come from
// JetBrains/rider-theme-pack: src/main/resources/colorSchemes/RiderIslandsDark.xml
// (F# has no entries of its own there and falls back to the ReSharper/DEFAULT_
// ones used below). Diff line colors live in styles.css, next to the light ones.

import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import type { Extension } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { tags as t } from "@lezer/highlight";

const color = {
  background: "#191a1c",
  text: "#d0d0d0",
  identifier: "#bdbdbd",
  caret: "#f0f0f0",
  caretRow: "#202424",
  lineNumber: "#808080",
  selection: "#08335e",
  // Not in the scheme; a dimmer take on the selection color.
  selectionInactive: "#2b3a4d",
  identifierUnderCaret: "#232e46",
  matchedBrace: "#422240",
  searchResult: "#484012",
  searchResultSelected: "#144238",
  folded: "#303030",
  foldedText: "#787878",
  // Popups follow the Islands UI theme, not the editor scheme.
  popup: "#26282c",
  popupBorder: "#33353b",
  popupSelection: "#2a4371",

  keyword: "#6c95eb",
  string: "#c9a26d",
  escape: "#d688d4",
  number: "#ed94c0",
  comment: "#85c46c",
  docTag: "#487d34",
  type: "#c191ff",
  valueType: "#e1bfff",
  method: "#39cc9b",
  field: "#66c3cc",
  event: "#ed94c0",
  inactive: "#787878",
  link: "#6c95eb",
  invalid: "#ff5647",
};

const theme = EditorView.theme(
  {
    "&": {
      color: color.text,
      backgroundColor: color.background,
    },
    ".cm-content": {
      caretColor: color.caret,
    },
    ".cm-cursor, .cm-dropCursor": {
      borderLeftColor: color.caret,
    },
    "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection":
      { backgroundColor: color.selection },
    ".cm-selectionBackground": {
      backgroundColor: color.selectionInactive,
    },
    ".cm-activeLine": {
      backgroundColor: color.caretRow,
    },
    ".cm-gutters": {
      backgroundColor: color.background,
      color: color.lineNumber,
      border: "none",
    },
    ".cm-activeLineGutter": {
      backgroundColor: color.caretRow,
      color: color.lineNumber,
    },
    ".cm-foldGutter .cm-gutterElement": {
      color: color.lineNumber,
    },
    ".cm-selectionMatch": {
      backgroundColor: color.identifierUnderCaret,
    },
    "&.cm-focused .cm-matchingBracket": {
      backgroundColor: color.matchedBrace,
      outline: "none",
    },
    "&.cm-focused .cm-nonmatchingBracket": {
      color: color.invalid,
    },
    ".cm-searchMatch": {
      backgroundColor: color.searchResult,
    },
    ".cm-searchMatch.cm-searchMatch-selected": {
      backgroundColor: color.searchResultSelected,
    },
    ".cm-foldPlaceholder": {
      backgroundColor: color.folded,
      border: "none",
      color: color.foldedText,
      borderRadius: "4px",
      padding: "0 4px",
    },
    ".cm-panels": {
      backgroundColor: color.popup,
      color: color.text,
    },
    ".cm-panels.cm-panels-top": {
      borderBottom: `1px solid ${color.popupBorder}`,
    },
    ".cm-panels.cm-panels-bottom": {
      borderTop: `1px solid ${color.popupBorder}`,
    },
    ".cm-tooltip": {
      backgroundColor: color.popup,
      border: `1px solid ${color.popupBorder}`,
      borderRadius: "8px",
      boxShadow: "0 6px 20px rgb(0 0 0 / 0.35)",
      overflow: "hidden",
    },
    ".cm-tooltip .cm-tooltip-arrow:before": {
      borderTopColor: "transparent",
      borderBottomColor: "transparent",
    },
    ".cm-tooltip .cm-tooltip-arrow:after": {
      borderTopColor: color.popup,
      borderBottomColor: color.popup,
    },
    ".cm-tooltip-autocomplete > ul > li[aria-selected]": {
      backgroundColor: color.popupSelection,
      color: color.text,
    },
    ".cm-completionMatchedText": {
      textDecoration: "none",
      color: color.keyword,
    },

    // Symbol kinds from the language server (see lsp/semanticTokens.ts). The
    // second selector wins over the syntax color when the parser's span ends
    // up inside the server's one.
    ...semanticRule("type", { color: color.type }),
    ...semanticRule("valueType", { color: color.valueType }),
    ...semanticRule("function", { color: color.method }),
    ...semanticRule("field", { color: color.field }),
    ...semanticRule("constant", { color: color.field, fontWeight: "bold" }),
    ...semanticRule("event", { color: color.event }),
    ...semanticRule("keyword", { color: color.keyword }),
    ...semanticRule("formatItem", { color: color.type }),
    ...semanticRule("plain", { color: color.identifier }),
  },
  { dark: true },
);

function semanticRule(kind: string, style: Record<string, string>) {
  return { [`.cm-semantic-${kind}, .cm-semantic-${kind} span`]: style };
}

const highlight = HighlightStyle.define([
  {
    tag: [
      t.keyword,
      t.modifier,
      t.controlKeyword,
      t.operatorKeyword,
      t.definitionKeyword,
      t.moduleKeyword,
      t.self,
      t.bool,
      t.null,
      t.atom,
    ],
    color: color.keyword,
  },
  { tag: [t.string, t.character, t.attributeValue], color: color.string },
  { tag: [t.escape, t.regexp], color: color.escape },
  { tag: [t.number, t.integer, t.float], color: color.number },
  { tag: [t.comment, t.lineComment, t.blockComment, t.docComment], color: color.comment, fontStyle: "italic" },
  // special(string): SQL's [quoted] and "quoted" names, which are identifiers, not text.
  { tag: [t.variableName, t.special(t.string), t.punctuation, t.operator, t.bracket, t.content], color: color.identifier },
  { tag: [t.typeName, t.className, t.namespace, t.annotation, t.meta], color: color.type },
  { tag: [t.standard(t.variableName), t.standard(t.name)], color: color.method },
  { tag: [t.function(t.variableName), t.function(t.propertyName), t.function(t.definition(t.variableName))], color: color.method },
  { tag: t.propertyName, color: color.field },
  { tag: [t.constant(t.variableName), t.constant(t.name)], color: color.field, fontWeight: "bold" },
  { tag: t.tagName, color: color.type },
  { tag: t.attributeName, color: color.comment },
  { tag: t.processingInstruction, color: color.type },
  { tag: t.heading, color: color.keyword, fontWeight: "bold" },
  { tag: t.monospace, color: color.comment },
  { tag: [t.link, t.url], color: color.link, textDecoration: "underline" },
  { tag: t.emphasis, fontStyle: "italic" },
  { tag: t.strong, fontWeight: "bold" },
  { tag: t.strikethrough, textDecoration: "line-through" },
  { tag: t.invalid, color: color.invalid },
]);

export const riderIslandsDark: Extension = [theme, syntaxHighlighting(highlight)];
