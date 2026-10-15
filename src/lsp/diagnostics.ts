// Server diagnostics for the editor: @codemirror/lsp-client's
// `serverDiagnostics`, plus the "unnecessary" tag, which FsAutoComplete puts
// on unused opens. Tagged code is faded the way Rider shows it instead of
// getting the hint underline. Read-only editors never have unsynced changes,
// so positions map straight from the synced document.

import { type Diagnostic, setDiagnostics } from "@codemirror/lint";
import { type LSPClient, type LSPClientExtension, LSPPlugin } from "@codemirror/lsp-client";
import type { Text } from "@codemirror/state";

const enum Tag {
  Unnecessary = 1,
}

interface ServerDiagnostic {
  range: { start: { line: number; character: number }; end: { line: number; character: number } };
  severity?: number;
  code?: number | string;
  message: string;
  tags?: number[];
}

const SEVERITY = ["error", "error", "warning", "info", "hint"] as const;

/** FsAutoComplete's code for an unused `open`; its range covers only the module name. */
const UNUSED_OPEN = "FSAC0001";

/** Converts one server diagnostic, given its range as document offsets. */
export function toDiagnostic(item: ServerDiagnostic, from: number, to: number, doc: Text): Diagnostic {
  const unnecessary = item.tags?.includes(Tag.Unnecessary) ?? false;
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
          toDiagnostic(item, plugin.fromPosition(item.range.start, doc), plugin.fromPosition(item.range.end, doc), doc),
        );
        view.dispatch(setDiagnostics(view.state, diagnostics));
        return true;
      },
    },
  };
}
