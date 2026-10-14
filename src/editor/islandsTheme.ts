// JetBrains "Islands Dark" (the default theme of the 2025.3+ IDEs, Rider
// included) for CodeMirror. Colors come from the IDE's own files in
// JetBrains/intellij-community: themes/islands/IslandSchemeDark.xml for the
// editor and its parent Darcula scheme for selection and diff colors. Diff
// line colors live in styles.css, next to the light ones.

import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import type { Extension } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { tags as t } from "@lezer/highlight";

const color = {
  background: "#191a1c",
  text: "#bcbec4",
  caret: "#ced0d6",
  caretRow: "#1f2024",
  lineNumber: "#4b5059",
  lineNumberOnCaretRow: "#a1a3ab",
  selection: "#214283",
  selectionInactive: "#4c4f56",
  identifierUnderCaret: "#373b39",
  matchedBrace: "#43454a",
  searchResult: "#114957",
  searchResultSelected: "#2d543f",
  folded: "#2b2d30",
  popup: "#26282c",
  popupBorder: "#33353b",
  popupSelection: "#2a4371",
  muted: "#9fa2a8",

  keyword: "#cf8e6d",
  string: "#6aab73",
  number: "#2aacb8",
  comment: "#7a7e85",
  docComment: "#5f826b",
  functionDeclaration: "#56a8f5",
  functionCall: "#57aaf7",
  field: "#c77dbb",
  metadata: "#b3ae60",
  tag: "#d5b778",
  link: "#5c92ff",
  invalid: "#f75464",
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
      color: color.lineNumberOnCaretRow,
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
      color: color.muted,
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
      color: color.functionCall,
    },
  },
  { dark: true },
);

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
      t.escape,
    ],
    color: color.keyword,
  },
  { tag: [t.string, t.special(t.string), t.character, t.regexp, t.attributeValue], color: color.string },
  { tag: [t.number, t.integer, t.float], color: color.number },
  { tag: [t.comment, t.lineComment, t.blockComment], color: color.comment },
  { tag: t.docComment, color: color.docComment, fontStyle: "italic" },
  { tag: [t.function(t.definition(t.variableName)), t.function(t.definition(t.propertyName))], color: color.functionDeclaration },
  { tag: [t.function(t.variableName), t.function(t.propertyName)], color: color.functionCall },
  { tag: t.propertyName, color: color.field },
  { tag: [t.constant(t.variableName), t.constant(t.name), t.standard(t.name)], color: color.field, fontStyle: "italic" },
  { tag: [t.meta, t.annotation, t.processingInstruction], color: color.metadata },
  { tag: t.tagName, color: color.tag },
  { tag: t.heading, color: color.keyword, fontWeight: "bold" },
  { tag: [t.link, t.url], color: color.link, textDecoration: "underline" },
  { tag: t.emphasis, fontStyle: "italic" },
  { tag: t.strong, fontWeight: "bold" },
  { tag: t.strikethrough, textDecoration: "line-through" },
  { tag: t.invalid, color: color.invalid },
]);

export const islandsDark: Extension = [theme, syntaxHighlighting(highlight)];
