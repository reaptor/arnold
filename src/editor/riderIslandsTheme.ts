// JetBrains Rider's "Rider Islands Dark" editor scheme for CodeMirror: the
// scheme Rider picks with the Islands Dark UI theme. It is the Rider Dark
// palette (blue keywords, purple types, teal methods) on the Islands
// background, unlike IntelliJ's orange "Islands Dark". Colors come from
// JetBrains/rider-theme-pack: src/main/resources/colorSchemes/RiderIslandsDark.xml
// (F# has no entries of its own there and falls back to the ReSharper/DEFAULT_
// ones used below). The light scheme puts the same kinds in the same hues on
// the Islands Light background. Diff line colors live in styles.css.

import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { Compartment, type Extension } from "@codemirror/state";
import { EditorView, ViewPlugin } from "@codemirror/view";
import { tags as t } from "@lezer/highlight";
import { createEffect, createRoot, on } from "solid-js";
import { isDark } from "../state/theme";

type Palette = Record<keyof typeof dark, string>;

const dark = {
  background: "#191a1c",
  text: "#d0d0d0",
  identifier: "#bdbdbd",
  caret: "#f0f0f0",
  // Also --caret-row in styles.css, which paints the row itself (lineTints.ts).
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
  shadow: "rgb(0 0 0 / 0.35)",

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

const light: Palette = {
  background: "#ffffff",
  text: "#080808",
  identifier: "#080808",
  caret: "#000000",
  // As --caret-row in styles.css.
  caretRow: "#cceeff44",
  lineNumber: "#aeb3c2",
  selection: "#a6d2ff",
  selectionInactive: "#d5e3f5",
  identifierUnderCaret: "#edebfc",
  matchedBrace: "#93d9d9",
  searchResult: "#f5e7a3",
  searchResultSelected: "#a8dbc8",
  folded: "#ebecf0",
  foldedText: "#6c707e",
  popup: "#ffffff",
  popupBorder: "#dfe1e5",
  popupSelection: "#d4e2ff",
  shadow: "rgb(0 0 0 / 0.18)",

  keyword: "#0033b3",
  string: "#8a5a00",
  escape: "#9c27b0",
  number: "#1750eb",
  comment: "#3c7a2b",
  docTag: "#2a5a1d",
  type: "#7a3fc4",
  valueType: "#9b4fc9",
  method: "#00785c",
  field: "#00727f",
  event: "#b0306f",
  inactive: "#8c8c8c",
  link: "#0033b3",
  invalid: "#d1242f",
};

function editorTheme(color: Palette, isDarkTheme: boolean) {
  return EditorView.theme(
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
        boxShadow: `0 6px 20px ${color.shadow}`,
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
      // The completion's documentation is a tooltip inside the list's, beside
      // it: not clipped. The list keeps the rounded corners itself.
      ".cm-tooltip.cm-tooltip-autocomplete": {
        overflow: "visible",
      },
      ".cm-tooltip-autocomplete > ul": {
        borderRadius: "8px",
      },
      ".cm-completionInfo": {
        padding: "6px 10px",
      },
      ".cm-completionInfo pre": {
        margin: "0 0 6px",
        whiteSpace: "pre-wrap",
      },
      ".cm-completionInfo p": {
        margin: "0",
      },
      ".cm-tooltip-autocomplete > ul > li[aria-selected]": {
        backgroundColor: color.popupSelection,
        color: color.text,
      },
      // The quick fixes menu (lsp/codeActions.ts), like the completion list.
      ".cm-fixMenu": {
        listStyle: "none",
        margin: "0",
        padding: "4px",
        minWidth: "200px",
        maxWidth: "480px",
      },
      ".cm-fixMenu > li": {
        padding: "3px 8px",
        borderRadius: "4px",
        cursor: "default",
        whiteSpace: "nowrap",
        overflow: "hidden",
        textOverflow: "ellipsis",
      },
      ".cm-fixMenu > li.cm-fixMenu-active": {
        backgroundColor: color.popupSelection,
      },
      ".cm-fixMenu > li.cm-fixMenu-empty": {
        color: color.inactive,
      },
      // The rename box (lsp/rename.ts), above the symbol.
      ".cm-renameBox": {
        display: "flex",
        alignItems: "center",
        gap: "8px",
        padding: "4px",
      },
      ".cm-renameBox input": {
        font: "inherit",
        color: color.text,
        backgroundColor: color.background,
        border: `1px solid ${color.keyword}`,
        borderRadius: "4px",
        padding: "2px 6px",
        minWidth: "180px",
        outline: "none",
      },
      ".cm-renameBox-hint": {
        color: color.inactive,
        fontSize: "11px",
        paddingRight: "4px",
        whiteSpace: "nowrap",
      },
      ".cm-renameBox.cm-renameBox-problem": {
        padding: "4px 8px",
        color: color.inactive,
      },
      // Code the server tags as unnecessary, such as unused opens (see
      // lsp/diagnostics.ts): faded like Rider's unused code, no underline.
      ".cm-lintRange.cm-unnecessary, .cm-lintRange.cm-unnecessary *": {
        color: color.inactive,
      },
      ".cm-lintRange.cm-unnecessary, .cm-lintRange.cm-unnecessaryParens": {
        backgroundImage: "none",
      },
      // Removable parentheses: only the parentheses fade, the code between
      // them keeps its colors.
      ".cm-unnecessaryParen, .cm-unnecessaryParen *": {
        color: color.inactive,
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
    { dark: isDarkTheme },
  );
}

function semanticRule(kind: string, style: Record<string, string>) {
  return { [`.cm-semantic-${kind}, .cm-semantic-${kind} span`]: style };
}

function tokenColors(color: Palette) {
  return HighlightStyle.define([
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
}

const darkHighlight = tokenColors(dark);
const lightHighlight = tokenColors(light);

/** Token colors of the theme shown; also used for code blocks outside editors (`highlightCode.ts`). */
export const codeHighlight = () => (isDark() ? darkHighlight : lightHighlight);

export const riderIslandsDark: Extension = [editorTheme(dark, true), syntaxHighlighting(darkHighlight)];
export const riderIslandsLight: Extension = [editorTheme(light, false), syntaxHighlighting(lightHighlight)];

const shownTheme = () => (isDark() ? riderIslandsDark : riderIslandsLight);
const themeSlot = new Compartment();
const themeViews = new Set<EditorView>();

createRoot(() =>
  createEffect(
    on(
      isDark,
      () => {
        for (const view of themeViews) view.dispatch({ effects: themeSlot.reconfigure(shownTheme()) });
      },
      { defer: true },
    ),
  ),
);

const followTheme = ViewPlugin.define((view) => {
  themeViews.add(view);
  return { destroy: () => themeViews.delete(view) };
});

/** The editor scheme of the theme shown, switched along with it. */
export const editorColors = (): Extension => [themeSlot.of(shownTheme()), followTheme];
