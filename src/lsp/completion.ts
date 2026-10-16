// Completion from the language server, in place of `serverCompletion` from
// @codemirror/lsp-client, which does not suit FsAutoComplete:
// - FsAutoComplete answers with every name in scope (or every member after a
//   dot) and leaves the filtering to the editor, but asked in the middle of a
//   word after a dot (`List.ma`) it fails ("Should not have empty
//   completions"). So it is always asked at the start of the word, and the
//   list is filtered here as the word grows, without asking again.
// - Its items carry only names; the signature and documentation come from
//   `completionItem/resolve`, asked for the item that is highlighted.
// - A failed request means no completions rather than an error.

import { autocompletion, type Completion, type CompletionContext, type CompletionResult, snippet } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { LSPPlugin } from "@codemirror/lsp-client";
import type { Extension } from "@codemirror/state";

/** The part of an LSP completion item used here. */
interface Item {
  label: string;
  kind?: number;
  detail?: string;
  documentation?: string | { kind: "markdown" | "plaintext"; value: string };
  sortText?: string;
  filterText?: string;
  insertText?: string;
  insertTextFormat?: number;
  textEdit?: { newText: string };
}

/** The word being typed: an F# identifier, which may hold `'` after its first character. */
const WORD = /[A-Za-z_][\w']*$/;
/** Typed characters after which the server is asked although no word has started. */
const TRIGGERS = ["."];

/** Whether `pos` is in a comment or string, where names are not completed while typing. */
function inCommentOrString(context: CompletionContext): boolean {
  const node = syntaxTree(context.state).resolveInner(context.pos, -1);
  return /comment|string/i.test(node.name);
}

export async function completionSource(context: CompletionContext): Promise<CompletionResult | null> {
  const plugin = context.view && LSPPlugin.get(context.view);
  if (!plugin) return null;
  // Unknown until the server has started; requests wait for it.
  const capabilities = plugin.client.serverCapabilities;
  if (capabilities && !capabilities.completionProvider) return null;
  const word = context.matchBefore(WORD);
  const from = word?.from ?? context.pos;
  const before = context.state.sliceDoc(from - 1, from);
  const afterTrigger = TRIGGERS.includes(before);
  // While typing: once a word starts or right after a dot; Ctrl+Space anywhere.
  if (!context.explicit && ((!word && !afterTrigger) || inCommentOrString(context))) return null;

  plugin.client.sync();
  const params = {
    textDocument: { uri: plugin.uri },
    position: plugin.toPosition(from),
    context: afterTrigger && !word ? { triggerKind: 2, triggerCharacter: before } : { triggerKind: 1 },
  };
  context.addEventListener("abort", () => plugin.client.cancelRequest(params));
  let result: Item[] | { items: Item[] } | null;
  try {
    result = await plugin.client.request<typeof params, Item[] | { items: Item[] } | null>("textDocument/completion", params);
  } catch {
    return null;
  }
  const items = Array.isArray(result) ? result : (result?.items ?? []);
  if (items.length === 0) return null;
  return {
    from,
    options: items.map((item) => toCompletion(plugin, item)),
    // Filtered here while the word grows; asked again once it is left.
    validFor: /^[\w']*$/,
  };
}

function toCompletion(plugin: LSPPlugin, item: Item): Completion {
  const text = item.textEdit?.newText ?? item.insertText ?? item.label;
  const label = item.filterText ?? item.label;
  const option: Completion = {
    label,
    displayLabel: item.label === label ? undefined : item.label,
    sortText: item.sortText,
    detail: item.detail,
    info: () => describe(plugin, item),
  };
  if (item.insertTextFormat === 2) {
    // Snippet placeholders as `${1:name}`, which CodeMirror's snippets take as they are.
    option.apply = (view, completion, from, to) => snippet(text.replace(/\$(\d+)/g, "${$1}"))(view, completion, from, to);
  } else if (text !== label) {
    option.apply = text;
  }
  return option;
}

/** The highlighted item's signature and documentation, asked of the server if it left them out. */
async function describe(plugin: LSPPlugin, item: Item): Promise<Node | null> {
  let full = item;
  if ((!item.detail || !item.documentation) && plugin.client.serverCapabilities?.completionProvider?.resolveProvider) {
    try {
      full = await plugin.client.request<Item, Item>("completionItem/resolve", item);
    } catch {
      // Shown without them.
    }
  }
  const doc = typeof full.documentation === "string" ? full.documentation : full.documentation?.value;
  const markdown = [full.detail && "```fsharp\n" + full.detail + "\n```", doc].filter(Boolean).join("\n\n");
  if (!markdown) return null;
  const dom = document.createElement("div");
  dom.className = "cm-lsp-documentation cm-lsp-completion-documentation";
  dom.innerHTML = plugin.docToHTML({ kind: "markdown", value: markdown });
  return dom;
}

/**
 * Completion from the language server, for the editors it serves: the only
 * source there. The syntax mode's keyword list would add keywords after a
 * dot, and FsAutoComplete lists the keywords that fit already.
 */
export const serverCompletion: Extension = autocompletion({ override: [completionSource] });
