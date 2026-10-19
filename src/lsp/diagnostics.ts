// Server diagnostics for the editor: @codemirror/lsp-client's
// `serverDiagnostics`, plus the "unnecessary" tag, which FsAutoComplete puts
// on unused opens. Tagged code is faded the way Rider shows it instead of
// getting the hint underline. Read-only editors never have unsynced changes,
// so positions map straight from the synced document. Each diagnostic keeps
// the server's original, which code actions are asked with (codeActions.ts);
// in an editor that can be typed in its hover card offers the quick fixes.

import { type Diagnostic, setDiagnostics } from "@codemirror/lint";
import { type LSPClient, type LSPClientExtension, LSPPlugin } from "@codemirror/lsp-client";
import type { Text } from "@codemirror/state";
import type { EditorView } from "@codemirror/view";
import { type FromServer, type ServerDiagnostic, showFixes } from "./codeActions";

const enum Tag {
  Unnecessary = 1,
}

const SEVERITY = ["error", "error", "warning", "info", "hint"] as const;

/** FsAutoComplete's code for an unused `open`; its range covers only the module name. */
const UNUSED_OPEN = "FSAC0001";

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
    ...(unnecessary && { markClass: "cm-unnecessary" }),
    ...(fixable && { actions: [{ ...quickFixes, apply: (view: EditorView) => quickFixes.apply(view, start) }] }),
    lsp: { item, shift: start - from },
  };
}

export function serverDiagnostics(): LSPClientExtension {
  return {
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
