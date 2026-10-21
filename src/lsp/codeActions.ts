// Quick fixes and refactorings from the language server (LSP code actions),
// such as FsAutoComplete's "Remove unused open". @codemirror/lsp-client has
// none, so they are asked for here: `Alt+Enter` (as in Rider) or `Mod-.`
// (as in VS Code) lists the actions for the selection in a menu at the
// cursor, and the editor's context menu and the hover card of a diagnostic
// open the same menu where they are clicked (atPointer.ts). Only actions that come with their edits are offered
// (the client answers no server requests, so commands could not edit), and
// only in editors that can be typed in.

import { forEachDiagnostic } from "@codemirror/lint";
import { LSPPlugin } from "@codemirror/lsp-client";
import { type Extension, Prec, StateEffect, StateField } from "@codemirror/state";
import { closeHoverTooltips, EditorView, keymap, showTooltip, type Tooltip } from "@codemirror/view";
import { type DocPoint, docPoint, pointCoords } from "./atPointer";
import { canonicalUri } from "./uri";

interface Position {
  line: number;
  character: number;
}
interface Range {
  start: Position;
  end: Position;
}
export interface TextEdit {
  range: Range;
  newText: string;
}
/** The part of a server diagnostic sent back with the request: fixes are found from them. */
export interface ServerDiagnostic {
  range: Range;
  severity?: number;
  code?: number | string;
  source?: string;
  message: string;
  tags?: number[];
  data?: unknown;
}
export interface WorkspaceEdit {
  changes?: Record<string, TextEdit[]>;
  documentChanges?: ({ textDocument: { uri: string }; edits: TextEdit[] } | { kind: string })[];
}
interface CodeAction {
  title: string;
  kind?: string;
  isPreferred?: boolean;
  disabled?: { reason: string };
  edit?: WorkspaceEdit;
}

/** An action that can be applied: its edits, file by file. */
export interface Fix {
  title: string;
  preferred: boolean;
  edits: Map<string, TextEdit[]>;
}

/**
 * What a CodeMirror diagnostic made from a server diagnostic keeps of it
 * (see diagnostics.ts): the original, and how far its start lies after the
 * diagnostic's (which may be widened, as unused opens are).
 */
export interface FromServer {
  lsp?: { item: ServerDiagnostic; shift: number };
}

export const codeActionCapabilities = {
  textDocument: {
    codeAction: {
      codeActionLiteralSupport: {
        codeActionKind: {
          valueSet: ["", "quickfix", "refactor", "refactor.extract", "refactor.inline", "refactor.rewrite", "source", "source.organizeImports"],
        },
      },
      isPreferredSupport: true,
      disabledSupport: true,
    },
  },
  workspace: { workspaceEdit: { documentChanges: true } },
};

/** The edits of `edit`, by canonical file URI; null when it does more than edit text (creates, renames or deletes files). */
export function editsOf(edit: WorkspaceEdit): Map<string, TextEdit[]> | null {
  const edits = new Map<string, TextEdit[]>();
  const add = (uri: string, more: TextEdit[]) => {
    const key = canonicalUri(uri);
    edits.set(key, [...(edits.get(key) ?? []), ...more]);
  };
  if (edit.documentChanges) {
    for (const change of edit.documentChanges) {
      if (!("textDocument" in change)) return null;
      add(change.textDocument.uri, change.edits);
    }
  } else {
    for (const [uri, more] of Object.entries(edit.changes ?? {})) add(uri, more);
  }
  return edits;
}

/** The server's diagnostics touching `from`–`to`, with their ranges where they are now. */
function diagnosticsAt(view: EditorView, plugin: LSPPlugin, from: number, to: number): ServerDiagnostic[] {
  const found: ServerDiagnostic[] = [];
  forEachDiagnostic(view.state, (diagnostic, start, end) => {
    const lsp = (diagnostic as FromServer).lsp;
    if (!lsp || end < from || start > to) return;
    const begin = Math.min(start + lsp.shift, end);
    found.push({ ...lsp.item, range: { start: plugin.toPosition(begin), end: plugin.toPosition(end) } });
  });
  return found;
}

/**
 * The fixes the server has for the selection in `view` (the cursor's line
 * when nothing is selected, so a fix for something on the line is found
 * wherever the cursor is on it). Null when no server serves the editor or
 * it has no code actions.
 */
export async function fixesAt(view: EditorView): Promise<Fix[] | null> {
  const plugin = LSPPlugin.get(view);
  if (!plugin || plugin.client.serverCapabilities?.codeActionProvider === false) return null;
  const selection = view.state.selection.main;
  const line = view.state.doc.lineAt(selection.head);
  const [from, to] = selection.empty ? [line.from, line.to] : [selection.from, selection.to];
  plugin.client.sync();
  const params = {
    textDocument: { uri: plugin.uri },
    range: { start: plugin.toPosition(selection.empty ? selection.head : from), end: plugin.toPosition(selection.empty ? selection.head : to) },
    context: { diagnostics: diagnosticsAt(view, plugin, from, to), triggerKind: 1 },
  };
  let result: (CodeAction | { command: string })[] | null;
  try {
    result = await plugin.client.request<typeof params, typeof result>("textDocument/codeAction", params);
  } catch {
    return [];
  }
  const fixes: Fix[] = [];
  for (const action of result ?? []) {
    if (!("title" in action) || !("edit" in action) || !action.edit || action.disabled) continue;
    const edits = editsOf(action.edit);
    // Every file it edits must be open, to be edited where it can be undone.
    if (!edits || edits.size === 0 || [...edits.keys()].some((uri) => !plugin.client.workspace.getFile(uri))) continue;
    if (fixes.some((fix) => fix.title === action.title)) continue;
    fixes.push({ title: action.title, preferred: !!action.isPreferred, edits });
  }
  // Preferred fixes first, otherwise in the server's order.
  return fixes.sort((a, b) => Number(b.preferred) - Number(a.preferred));
}

/**
 * Applies `fix`. Its positions are those of the files as the server last
 * had them; the menu closes on typing, so nothing was typed since.
 */
export function applyFix(view: EditorView, fix: Fix) {
  const plugin = LSPPlugin.get(view);
  if (!plugin) return;
  const workspace = plugin.client.workspace;
  for (const [uri, edits] of fix.edits) {
    const file = workspace.getFile(uri);
    if (!file) continue;
    workspace.updateFile(uri, {
      changes: edits.map((edit) => ({
        from: plugin.fromPosition(edit.range.start, file.doc),
        to: plugin.fromPosition(edit.range.end, file.doc),
        insert: edit.newText,
      })),
      userEvent: "input.fix",
    });
  }
  view.focus();
}

// ------------------------------------------------------- unused opens

/** FsAutoComplete's code for an unused `open`. */
const UNUSED_OPEN = "FSAC0001";

/** The unused opens the server reported for the file in `view`, where they are now. */
function unusedOpens(view: EditorView, plugin: LSPPlugin): ServerDiagnostic[] {
  return diagnosticsAt(view, plugin, 0, view.state.doc.length).filter((diagnostic) => diagnostic.code === UNUSED_OPEN);
}

/** How many unused opens the file in `view` has; 0 without a server. */
export function unusedOpenCount(view: EditorView): number {
  const plugin = LSPPlugin.get(view);
  return plugin ? unusedOpens(view, plugin).length : 0;
}

/**
 * Removes every unused open of the file in `view` in one edit: the server's
 * own fix for each, asked for together. Resolves to how many were removed.
 */
export async function removeUnusedOpens(view: EditorView): Promise<number> {
  const plugin = LSPPlugin.get(view);
  if (!plugin || view.state.readOnly) return 0;
  plugin.client.sync();
  const doc = view.state.doc;
  const fixes = await Promise.all(
    unusedOpens(view, plugin).map(async (diagnostic) => {
      const params = { textDocument: { uri: plugin.uri }, range: diagnostic.range, context: { diagnostics: [diagnostic], triggerKind: 1 } };
      try {
        const actions = await plugin.client.request<typeof params, (CodeAction | { command: string })[] | null>("textDocument/codeAction", params);
        const fix = (actions ?? []).find(
          (action): action is CodeAction => "title" in action && !!action.edit && /unused open/i.test(action.title),
        );
        return fix?.edit ? editsOf(fix.edit)?.get(canonicalUri(plugin.uri)) ?? [] : [];
      } catch {
        return [];
      }
    }),
  );
  // Typed meanwhile: the positions no longer fit.
  if (view.state.doc !== doc) return 0;
  const changes = fixes
    .flat()
    .map((edit) => ({ from: plugin.fromPosition(edit.range.start, doc), to: plugin.fromPosition(edit.range.end, doc), insert: edit.newText }))
    .sort((a, b) => a.from - b.from)
    // Neighbouring fixes may meet; ones that overlap are left for another go.
    .filter((change, index, all) => index === 0 || change.from >= all[index - 1].to);
  if (changes.length === 0) return 0;
  view.dispatch({ changes, userEvent: "input.fix" });
  view.focus();
  return fixes.filter((edits) => edits.length > 0).length;
}

// ---------------------------------------------------------------- the menu

interface Menu {
  pos: number;
  /** Where the pointer opened it; at `pos` when the keyboard did. */
  at?: DocPoint;
  /** null while the server is asked. */
  fixes: Fix[] | null;
}

const setMenu = StateEffect.define<Menu | null>();

const menuField = StateField.define<Menu | null>({
  create: () => null,
  update(menu, tr) {
    for (const effect of tr.effects) if (effect.is(setMenu)) return effect.value;
    // Typing or moving away closes it.
    if (menu && (tr.docChanged || tr.selection)) return null;
    return menu;
  },
  provide: (field) => showTooltip.from(field, (menu): Tooltip | null => menu && { pos: menu.pos, above: false, create: (view) => menuView(view, menu) }),
});

/** Index of the highlighted row, per open menu. */
const highlighted = new WeakMap<Menu, number>();

function menuView(view: EditorView, menu: Menu) {
  const dom = document.createElement("ul");
  dom.className = "cm-fixMenu";
  dom.setAttribute("role", "menu");
  const at = menu.at;
  const getCoords = at && (() => pointCoords(view, at));
  const fixes = menu.fixes;
  if (fixes === null || fixes.length === 0) {
    const row = dom.appendChild(document.createElement("li"));
    row.className = "cm-fixMenu-empty";
    row.textContent = fixes === null ? "Looking for fixes…" : "No quick fixes here";
    return { dom, getCoords };
  }
  const rows = fixes.map((fix, index) => {
    const row = dom.appendChild(document.createElement("li"));
    row.setAttribute("role", "menuitem");
    row.textContent = fix.title;
    row.addEventListener("mousedown", (event) => {
      // Picked without the editor losing focus first.
      event.preventDefault();
      pick(view, index);
    });
    row.addEventListener("mousemove", () => highlight(index));
    return row;
  });
  const highlight = (index: number) => {
    highlighted.set(menu, index);
    rows.forEach((row, at) => row.classList.toggle("cm-fixMenu-active", at === index));
  };
  highlight(highlighted.get(menu) ?? 0);
  return {
    dom,
    getCoords,
    update() {
      highlight(highlighted.get(menu) ?? 0);
    },
  };
}

function pick(view: EditorView, index: number) {
  const fix = view.state.field(menuField)?.fixes?.[index];
  view.dispatch({ effects: setMenu.of(null) });
  if (fix) applyFix(view, fix);
}

/** Moves the highlight in the open menu; false when none is open. */
function move(view: EditorView, by: number): boolean {
  const menu = view.state.field(menuField, false);
  if (!menu?.fixes?.length) return !!menu;
  const count = menu.fixes.length;
  highlighted.set(menu, ((highlighted.get(menu) ?? 0) + by + count) % count);
  // Redraws the menu without opening another.
  view.dispatch({ effects: setMenu.of(menu) });
  return true;
}

/**
 * Shows the fixes for the selection in a menu at the cursor, or at `pointer`
 * (window coordinates) when the mouse opened it; a hover card gives way to
 * it. False (nothing done) where there is no server or the editor is read-only.
 */
export function showFixes(view: EditorView, pointer?: { x: number; y: number }): boolean {
  if (!LSPPlugin.get(view) || view.state.readOnly) return false;
  const pos = view.state.selection.main.head;
  const asking: Menu = { pos, at: pointer && docPoint(view, pointer), fixes: null };
  view.dispatch({ effects: [setMenu.of(asking), closeHoverTooltips] });
  void fixesAt(view).then((fixes) => {
    // Still open, and for the same place.
    if (view.state.field(menuField, false) !== asking) return;
    view.dispatch({ effects: setMenu.of({ ...asking, fixes: fixes ?? [] }) });
  });
  return true;
}

const menuKeys = Prec.highest(
  keymap.of([
    { key: "ArrowDown", run: (view) => move(view, 1) },
    { key: "ArrowUp", run: (view) => move(view, -1) },
    {
      key: "Enter",
      run(view) {
        const menu = view.state.field(menuField, false);
        if (!menu) return false;
        pick(view, highlighted.get(menu) ?? 0);
        return true;
      },
    },
    {
      key: "Escape",
      run(view) {
        if (!view.state.field(menuField, false)) return false;
        view.dispatch({ effects: setMenu.of(null) });
        return true;
      },
    },
  ]),
);

/** For the language server's editors: the menu and its keys. */
export const codeActions: Extension = [
  menuField,
  menuKeys,
  keymap.of([
    { key: "Alt-Enter", run: (view) => showFixes(view), preventDefault: true },
    { key: "Mod-.", run: (view) => showFixes(view), preventDefault: true },
  ]),
  EditorView.focusChangeEffect.of((state, focusing) => (!focusing && state.field(menuField, false) ? setMenu.of(null) : null)),
];
