// Server diagnostics for the editor: @codemirror/lsp-client's
// `serverDiagnostics`, plus the "unnecessary" tag, which FsAutoComplete puts
// on unused opens. Tagged code is faded the way Rider shows it instead of
// getting the hint underline; for removable parentheses only the parentheses
// are, so the code inside keeps its colors. Read-only editors never have unsynced changes,
// so positions map straight from the synced document. Each diagnostic keeps
// the server's original, which code actions are asked with (codeActions.ts);
// in an editor that can be typed in its hover card offers the quick fixes.

import { type Diagnostic, forEachDiagnostic, setDiagnostics, setDiagnosticsEffect } from "@codemirror/lint";
import { type LSPClient, type LSPClientExtension, LSPPlugin } from "@codemirror/lsp-client";
import type { Range, Text } from "@codemirror/state";
import { Decoration, type DecorationSet, type EditorView, ViewPlugin, type ViewUpdate } from "@codemirror/view";
import { type FromServer, type ServerDiagnostic, showFixes } from "./codeActions";

const enum Tag {
  Unnecessary = 1,
}

const SEVERITY = ["error", "error", "warning", "info", "hint"] as const;

/** FsAutoComplete's code for an unused `open`; its range covers only the module name. */
const UNUSED_OPEN = "FSAC0001";

/** FsAutoComplete's code for "Parentheses can be removed"; its range covers the parentheses too. */
const REMOVABLE_PARENS = "FSAC0004";

/** "Quick fixes" in a diagnostic's hover card: the fixes menu for where it starts. */
const quickFixes = {
  name: "Quick fixes",
  apply(view: EditorView, from: number) {
    view.dispatch({ selection: { anchor: from } });
    view.focus();
    showFixes(view);
  },
};

/**
 * Converts one server diagnostic, given its range as document offsets.
 * `fixable`: the editor can be typed in, so its hover card offers fixes.
 */
export function toDiagnostic(item: ServerDiagnostic, from: number, to: number, doc: Text, fixable = false): Diagnostic & FromServer {
  const unnecessary = item.tags?.includes(Tag.Unnecessary) ?? false;
  const parens = unnecessary && item.code === REMOVABLE_PARENS && doc.sliceString(from, from + 1) === "(" && doc.sliceString(to - 1, to) === ")";
  const start = from;
  if (item.code === UNUSED_OPEN) {
    // Fade the whole statement, `open` included, as Rider does.
    const line = doc.lineAt(from);
    from = line.from + (/^\s*/.exec(line.text)?.[0].length ?? 0);
  }
  return {
    from,
    to,
    severity: SEVERITY[item.severity ?? 1] ?? "hint",
    message: item.message,
    ...(unnecessary && { markClass: parens ? "cm-unnecessaryParens" : "cm-unnecessary" }),
    ...(fixable && { actions: [{ ...quickFixes, apply: (view: EditorView) => quickFixes.apply(view, start) }] }),
    lsp: { item, shift: start - from },
  };
}

const parenMark = Decoration.mark({ class: "cm-unnecessaryParen" });

function parenMarks(view: EditorView): DecorationSet {
  const marks: Range<Decoration>[] = [];
  forEachDiagnostic(view.state, (diagnostic, from, to) => {
    if (diagnostic.markClass === "cm-unnecessaryParens") marks.push(parenMark.range(from, from + 1), parenMark.range(to - 1, to));
  });
  // Nested parentheses come out of order.
  return Decoration.set(marks, true);
}

/** Fades just the parentheses of a "Parentheses can be removed" diagnostic. */
const removableParens = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;
    constructor(view: EditorView) {
      this.decorations = parenMarks(view);
    }
    update(update: ViewUpdate) {
      if (update.transactions.some((tr) => tr.effects.some((effect) => effect.is(setDiagnosticsEffect)))) {
        this.decorations = parenMarks(update.view);
      } else if (update.docChanged) {
        this.decorations = this.decorations.map(update.changes);
      }
    }
  },
  { decorations: (plugin) => plugin.decorations },
);

export function serverDiagnostics(): LSPClientExtension {
  return {
    editorExtension: removableParens,
    clientCapabilities: {
      textDocument: { publishDiagnostics: { versionSupport: true, tagSupport: { valueSet: [Tag.Unnecessary] } } },
    },
    notificationHandlers: {
      "textDocument/publishDiagnostics": (
        client: LSPClient,
        params: { uri: string; version?: number; diagnostics: ServerDiagnostic[] },
      ) => {
        const file = client.workspace.getFile(params.uri);
        if (!file || (params.version != null && params.version !== file.version)) return false;
        const view = file.getView();
        const plugin = view && LSPPlugin.get(view);
        if (!view || !plugin) return false;
        const doc = plugin.syncedDoc;
        const diagnostics = params.diagnostics.map((item) =>
          toDiagnostic(
            item,
            plugin.fromPosition(item.range.start, doc),
            plugin.fromPosition(item.range.end, doc),
            doc,
            !view.state.readOnly,
          ),
        );
        view.dispatch(setDiagnostics(view.state, diagnostics));
        return true;
      },
    },
  };
}
