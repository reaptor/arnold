// Semantic tokens (textDocument/semanticTokens/full): the server's symbol
// kinds, so that types, functions and fields get Rider's colors although the
// F# parser cannot tell them apart.

import { type LSPClient, type LSPClientExtension, LSPPlugin } from "@codemirror/lsp-client";
import type { ChangeSet, Text } from "@codemirror/state";
import { type EditorView, ViewPlugin, type ViewUpdate } from "@codemirror/view";
import { mapRanges, type SemanticKind, type SemanticRange, setSemanticRanges } from "../editor/semanticHighlight";
import { canonicalUri } from "./uri";

/** Wait after an edit before asking again. */
const EDIT_DELAY = 400;

/**
 * Token types to Rider's colors. The names are FsAutoComplete's
 * (LspHelpers.fs); the colors follow Rider's F# highlighters
 * (FSharpHighlightingAttributeIds.fs) through their fallbacks: modules and
 * union cases as classes, unions and structs as value types, active patterns
 * and functions as methods, computation expressions as keywords.
 */
const KINDS: Record<string, SemanticKind> = {
  namespace: "type",
  module: "type",
  class: "type",
  type: "type",
  interface: "type",
  typeParameter: "type",
  enumMember: "type",
  struct: "valueType",
  enum: "valueType",
  function: "function",
  method: "function",
  property: "field",
  member: "field",
  event: "event",
  cexpr: "keyword",
  regexp: "formatItem",
  parameter: "plain",
  variable: "plain",
  operator: "plain",
};

/** Views showing a file, so that a finished check can refresh them. */
const plugins = new Set<SemanticTokensPlugin>();

class SemanticTokensPlugin {
  private timer: number | undefined;
  /** Bumped per request, so an older answer never replaces a newer one. */
  private generation = 0;
  /** Edits made while a request is out, to map its answer onto the current text. */
  private pending: ChangeSet | null = null;

  constructor(private readonly view: EditorView) {
    plugins.add(this);
    this.schedule(0);
  }

  update(update: ViewUpdate) {
    if (!update.docChanged) return;
    if (this.pending) this.pending = this.pending.compose(update.changes);
    this.schedule(EDIT_DELAY);
  }

  destroy() {
    plugins.delete(this);
    window.clearTimeout(this.timer);
    this.generation++;
  }

  get uri(): string | null {
    return LSPPlugin.get(this.view)?.uri ?? null;
  }

  schedule(delay: number) {
    window.clearTimeout(this.timer);
    this.timer = window.setTimeout(() => void this.request(), delay);
  }

  private async request() {
    const plugin = LSPPlugin.get(this.view);
    if (!plugin) return;
    const client = plugin.client;
    await client.initializing;
    const legend = client.serverCapabilities?.semanticTokensProvider?.legend;
    if (!legend) return;

    const generation = ++this.generation;
    client.sync();
    const doc = plugin.syncedDoc;
    this.pending = plugin.unsyncedChanges;
    let answer: { data: number[] } | null;
    try {
      answer = await client.request("textDocument/semanticTokens/full", { textDocument: { uri: plugin.uri } });
    } catch {
      return; // Not checked yet or busy; the next edit or check asks again.
    }
    if (generation !== this.generation || !answer) return;
    let ranges = decode(answer.data, legend, doc);
    if (this.pending) ranges = mapRanges(ranges, this.pending);
    this.pending = null;
    this.view.dispatch({ effects: setSemanticRanges.of(ranges) });
  }
}

const WORD = /[\w\u00a1-\uffff]/;

/**
 * Decodes the relative five-number groups of the LSP encoding. FsAutoComplete
 * includes the dot in member tokens (`.Length`) and splits some names in two
 * (`[<Literal>]` as `L` + `iteral`); the dot is left out and the rest of a
 * split name takes the kind of its start.
 */
export function decode(
  data: readonly number[],
  legend: { tokenTypes: readonly string[]; tokenModifiers: readonly string[] },
  doc: Text,
): SemanticRange[] {
  const ranges: SemanticRange[] = [];
  let line = 0;
  let character = 0;
  for (let i = 0; i + 4 < data.length; i += 5) {
    const deltaLine = data[i];
    line += deltaLine;
    character = deltaLine === 0 ? character + data[i + 1] : data[i + 1];
    if (line >= doc.lines) break;
    const textLine = doc.line(line + 1);
    let from = textLine.from + character;
    const to = Math.min(from + data[i + 2], textLine.to);
    while (from < to && doc.sliceString(from, from + 1) === ".") from++;
    if (from >= to) continue;

    const previous = ranges[ranges.length - 1];
    if (previous?.to === from && WORD.test(doc.sliceString(from - 1, from)) && WORD.test(doc.sliceString(from, from + 1))) {
      previous.to = to;
      continue;
    }
    const kind = kindOf(legend.tokenTypes[data[i + 3]], modifierNames(data[i + 4], legend.tokenModifiers));
    if (kind) ranges.push({ from, to, kind });
  }
  return ranges;
}

function modifierNames(bits: number, names: readonly string[]): Set<string> {
  const set = new Set<string>();
  for (let bit = 0; bit < names.length; bit++) if (bits & (1 << bit)) set.add(names[bit]);
  return set;
}

function kindOf(type: string | undefined, modifiers: Set<string>): SemanticKind | undefined {
  // FsAutoComplete marks [<Literal>] values this way.
  if (type === "variable" && modifiers.has("readonly") && modifiers.has("defaultLibrary")) return "constant";
  return type === undefined ? undefined : KINDS[type];
}

export function semanticTokens(): LSPClientExtension {
  return {
    clientCapabilities: {
      textDocument: {
        semanticTokens: {
          requests: { full: true },
          tokenTypes: Object.keys(KINDS),
          tokenModifiers: ["readonly", "defaultLibrary"],
          formats: ["relative"],
        },
      },
    },
    notificationHandlers: {
      // The server publishes diagnostics when it has checked a file; its
      // symbol kinds are complete from then on.
      "textDocument/publishDiagnostics": (_client: LSPClient, params: { uri: string }) => {
        const uri = canonicalUri(params.uri);
        plugins.forEach((plugin) => plugin.uri === uri && plugin.schedule(0));
        return false;
      },
    },
    editorExtension: ViewPlugin.fromClass(SemanticTokensPlugin),
  };
}
