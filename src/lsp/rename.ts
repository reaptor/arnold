// Renaming a symbol through the language server, in place of
// @codemirror/lsp-client's `renameSymbol`, which reads only the `changes`
// form of the answer (FsAutoComplete sends `documentChanges`) and leaves
// out files no editor has open, which would break the code that uses the
// symbol there. Here F2 or Ctrl+R Ctrl+R (as in Visual Studio) opens a name
// box on the symbol; the rename then edits the files editors show like
// typing would, and the others on disk through `onEditFile`.

import { LSPPlugin } from "@codemirror/lsp-client";
import { type Extension, StateEffect, StateField } from "@codemirror/state";
import { EditorView, keymap, showTooltip, type Tooltip } from "@codemirror/view";
import { type TextEdit, editsOf, type WorkspaceEdit } from "./codeActions";
import { track } from "../state/activity";

/** Edits a file no editor has open: its text with `edits` applied (`applyTextEdits`), saved to disk. */
type EditFile = (uri: string, edits: TextEdit[]) => Promise<void>;

let editFile: EditFile = async (uri) => {
  throw new Error(`cannot edit ${uri}`);
};
let reportError: (message: string) => void = () => {};

export function onEditFile(handler: EditFile) {
  editFile = handler;
}

/** Called with what went wrong when a rename could not be done. */
export function onRenameError(handler: (message: string) => void) {
  reportError = handler;
}

/** `text` with `edits` (LSP ranges in it) applied. */
export function applyTextEdits(text: string, edits: readonly TextEdit[]): string {
  const starts = [0];
  for (let at = text.indexOf("\n"); at !== -1; at = text.indexOf("\n", at + 1)) starts.push(at + 1);
  const offset = (position: TextEdit["range"]["start"]) =>
    Math.min(text.length, (starts[position.line] ?? text.length) + position.character);
  const sorted = edits
    .map((edit) => ({ from: offset(edit.range.start), to: offset(edit.range.end), text: edit.newText }))
    .sort((a, b) => b.from - a.from);
  let result = text;
  for (const edit of sorted) result = result.slice(0, edit.from) + edit.text + result.slice(edit.to);
  return result;
}

// ------------------------------------------------------------- the name box

interface Box {
  from: number;
  to: number;
  name: string;
  /** Why there is nothing to rename, in place of the box. */
  problem?: string;
}

const setBox = StateEffect.define<Box | null>();

const boxField = StateField.define<Box | null>({
  create: () => null,
  update(box, tr) {
    for (const effect of tr.effects) if (effect.is(setBox)) return effect.value;
    // Typing in the editor itself closes it (the box has its own input).
    if (box && tr.docChanged) return null;
    return box;
  },
  provide: (field) =>
    showTooltip.from(field, (box): Tooltip | null => box && { pos: box.from, above: true, create: (view) => boxView(view, box) }),
});

function close(view: EditorView) {
  if (view.state.field(boxField, false)) view.dispatch({ effects: setBox.of(null) });
}

function boxView(view: EditorView, box: Box) {
  const dom = document.createElement("div");
  dom.className = "cm-renameBox";
  if (box.problem) {
    dom.classList.add("cm-renameBox-problem");
    dom.textContent = box.problem;
    // Said once, then gone with the next key or click.
    const dismiss = () => close(view);
    window.addEventListener("keydown", dismiss, { once: true, capture: true });
    window.addEventListener("pointerdown", dismiss, { once: true, capture: true });
    return {
      dom,
      destroy() {
        window.removeEventListener("keydown", dismiss, true);
        window.removeEventListener("pointerdown", dismiss, true);
      },
    };
  }
  const input = dom.appendChild(document.createElement("input"));
  input.value = box.name;
  input.spellcheck = false;
  input.setAttribute("aria-label", `Rename ${box.name}`);
  const hint = dom.appendChild(document.createElement("span"));
  hint.className = "cm-renameBox-hint";
  hint.textContent = "Enter to rename · Esc to cancel";
  let done = false;
  const finish = (rename: boolean) => {
    if (done) return;
    done = true;
    const name = input.value.trim();
    close(view);
    view.focus();
    if (rename && name && name !== box.name) void track(`Renaming to ${name}`, renameTo(view, box.from, name));
  };
  input.addEventListener("keydown", (event) => {
    event.stopPropagation();
    if (event.key === "Enter") {
      event.preventDefault();
      finish(true);
    } else if (event.key === "Escape") {
      event.preventDefault();
      finish(false);
    }
  });
  input.addEventListener("blur", () => finish(false));
  return {
    dom,
    mount() {
      input.focus();
      input.select();
    },
  };
}

interface Range {
  start: { line: number; character: number };
  end: { line: number; character: number };
}

/**
 * Opens the name box on the symbol at the cursor, after asking the server
 * whether it can be renamed. False (nothing done) where there is no server
 * or the editor is read-only.
 */
export function startRename(view: EditorView): boolean {
  const plugin = LSPPlugin.get(view);
  if (!plugin || view.state.readOnly) return false;
  const head = view.state.selection.main.head;
  const word = view.state.wordAt(head);
  void (async () => {
    plugin.client.sync();
    const doc = view.state.doc;
    let from = word?.from ?? head;
    let to = word?.to ?? head;
    const provider = plugin.client.serverCapabilities?.renameProvider;
    if (provider && typeof provider === "object" && provider.prepareProvider) {
      try {
        const prepared = await plugin.client.request<object, Range | { range: Range } | { defaultBehavior: boolean } | null>(
          "textDocument/prepareRename",
          { textDocument: { uri: plugin.uri }, position: plugin.toPosition(head) },
        );
        const range = prepared && ("start" in prepared ? prepared : "range" in prepared ? prepared.range : null);
        if (range) {
          from = plugin.fromPosition(range.start, doc);
          to = plugin.fromPosition(range.end, doc);
        } else if (!prepared) {
          from = to = head;
        }
      } catch (error) {
        const message = (error as { message?: string })?.message ?? String(error);
        if (view.state.doc === doc) view.dispatch({ effects: setBox.of({ from: head, to: head, name: "", problem: message }) });
        return;
      }
    }
    if (view.state.doc !== doc) return;
    if (from === to) {
      view.dispatch({ effects: setBox.of({ from: head, to: head, name: "", problem: "Nothing to rename" }) });
      return;
    }
    view.dispatch({ effects: setBox.of({ from, to, name: doc.sliceString(from, to) }) });
  })();
  return true;
}

/** Renames the symbol at `pos` to `name`, in every file the server says uses it. */
async function renameTo(view: EditorView, pos: number, name: string) {
  const plugin = LSPPlugin.get(view);
  if (!plugin) return;
  plugin.client.sync();
  let result: WorkspaceEdit | null;
  try {
    result = await plugin.client.request<object, WorkspaceEdit | null>("textDocument/rename", {
      textDocument: { uri: plugin.uri },
      position: plugin.toPosition(pos),
      newName: name,
    });
  } catch (error) {
    reportError(`Renaming to ${name}: ${(error as { message?: string })?.message ?? String(error)}`);
    return;
  }
  const edits = result && editsOf(result);
  if (!edits) {
    if (result) reportError(`Renaming to ${name}: the server wants to create, rename or delete files, which Arnold does not do.`);
    return;
  }
  const workspace = plugin.client.workspace;
  const failed: string[] = [];
  for (const [uri, fileEdits] of edits) {
    const file = workspace.getFile(uri);
    if (file?.getView()) {
      workspace.updateFile(uri, {
        changes: fileEdits.map((edit) => ({
          from: plugin.fromPosition(edit.range.start, file.doc),
          to: plugin.fromPosition(edit.range.end, file.doc),
          insert: edit.newText,
        })),
        userEvent: "input.rename",
      });
    } else {
      try {
        await editFile(uri, fileEdits);
      } catch (error) {
        failed.push(`${decodeURIComponent(uri.split("/").pop() ?? uri)}: ${String(error)}`);
      }
    }
  }
  if (failed.length) reportError(`Renaming to ${name}, these files could not be changed: ${failed.join("; ")}`);
}

/** For the language server's editors: the name box and its keys. */
export const rename: Extension = [
  boxField,
  keymap.of([
    { key: "F2", run: startRename, preventDefault: true },
    { key: "Ctrl-r Ctrl-r", run: startRename, preventDefault: true },
  ]),
];
