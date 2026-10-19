// Language-server clients for the editor. The server process lives in the
// backend; messages travel over a Tauri channel.

import {
  findReferencesKeymap,
  hoverTooltips,
  jumpToDefinition,
  jumpToDefinitionKeymap,
  LSPClient,
  LSPPlugin,
  signatureHelp,
  type Transport,
} from "@codemirror/lsp-client";
import { type Extension, Prec } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import * as ipc from "../ipc/commands";
import { codeActionCapabilities, codeActions } from "./codeActions";
import { serverCompletion } from "./completion";
import { rename } from "./rename";
import { serverDiagnostics } from "./diagnostics";
import { semanticTokens } from "./semanticTokens";
import { canonicalUri, pathToUri } from "./uri";

export type LanguageId = "fsharp" | "csharp";

/** Languages a server exists for so far. */
const SUPPORTED: ReadonlySet<string> = new Set<LanguageId>(["fsharp"]);

/** Called when the client needs another file shown, e.g. for go-to-definition. */
type DisplayFile = (uri: string) => Promise<EditorView | null>;

interface Running {
  key: string;
  id: Promise<number>;
  client: Promise<LSPClient | null>;
}

let running: Running | null = null;
let displayFile: DisplayFile = async () => null;
let startFailed: (reason: unknown) => void = () => {};
let serverEvent: (event: ipc.LspEvent) => void = () => {};
let jumped: (from?: number) => void = () => {};

export function onDisplayFile(handler: DisplayFile) {
  displayFile = handler;
}

/**
 * Called when go-to-definition or a reference is about to leave the place the
 * cursor is at, for "back"; with the position left when the cursor has
 * already moved.
 */
export function onJump(handler: (from?: number) => void) {
  jumped = handler;
}

/** Called with the reason when a server could not be started. */
export function onStartFailed(handler: (reason: unknown) => void) {
  startFailed = handler;
}

/** Called with what the running server reports besides LSP messages; never for a stopped one. */
export function onServerEvent(handler: (event: ipc.LspEvent) => void) {
  serverEvent = handler;
}

const isMac = navigator.platform.toLowerCase().includes("mac");

/** Whether a language server serves the file in `view`. */
export const hasLanguageServer = (view: EditorView) => LSPPlugin.get(view) !== null;

/** Jumps to the definition of the symbol at `position` (F12, Ctrl+click, the context menu). */
export function goToDefinitionAt(view: EditorView, position: number) {
  view.dispatch({ selection: { anchor: position } });
  view.focus();
  jumpToDefinition(view);
}

/**
 * The client puts the cursor in another file's editor right after it is
 * shown, but leaves the keyboard where the jump started, and a newly opened
 * editor restores its old scroll position a frame later. So on the next frame
 * that editor gets the keyboard and scrolls back to its cursor.
 */
function focusAfterJump(view: EditorView | null) {
  if (view) {
    arriving = view;
    requestAnimationFrame(() => {
      // Landed by now; an editor without the listener yet would keep it.
      if (arriving === view) arriving = null;
      if (!view.dom.isConnected) return;
      view.focus();
      view.dispatch({ effects: EditorView.scrollIntoView(view.state.selection.main.head) });
    });
  }
  return view;
}

/** The editor another file's jump lands in next; its landing is not a jump within it. */
let arriving: EditorView | null = null;

/** A jump within one file: the place it left is noted once it lands. */
const noteJumpWithinFile: Extension = EditorView.updateListener.of((update) => {
  if (!update.transactions.some((transaction) => transaction.isUserEvent("select.definition"))) return;
  if (update.view === arriving) {
    arriving = null;
    return;
  }
  const from = update.startState.selection.main.head;
  if (from !== update.state.selection.main.head) jumped(from);
});

// macOS turns Ctrl+click into a right-click, so the context menu that follows
// a Ctrl+click that went to a definition is swallowed.
let swallowContextMenu = false;

/**
 * Ctrl+click jumps to the definition of the clicked symbol; Cmd+click too on
 * macOS. Highest precedence so it runs before the editor's context menu.
 */
const clickToDefinition: Extension = Prec.highest(
  EditorView.domEventHandlers({
    mousedown(event, view) {
      const modifier = event.ctrlKey || (isMac && event.metaKey);
      // Some macOS browsers report a Ctrl+click as the secondary button.
      const button = event.button === 0 || (isMac && event.ctrlKey && event.button === 2);
      if (!modifier || !button) return false;
      const position = view.posAtCoords({ x: event.clientX, y: event.clientY });
      if (position === null) return false;
      goToDefinitionAt(view, position);
      swallowContextMenu = isMac && event.ctrlKey;
      return true;
    },
    contextmenu() {
      const swallow = swallowContextMenu;
      swallowContextMenu = false;
      return swallow;
    },
  }),
);

/**
 * The client for `language` in the checkout `root`, starting the server if
 * needed. Only one server runs at a time: asking for another checkout or
 * language stops the previous one. Resolves to null when no server is
 * available; the reason is reported through the `lsp-event` status.
 */
export function clientFor(root: string, language: LanguageId | null): Promise<LSPClient | null> {
  if (!language || !SUPPORTED.has(language)) return Promise.resolve(null);
  const key = `${language}\n${root}`;
  if (running?.key === key) return running.client;
  void stop();

  const handlers = new Set<(message: string) => void>();
  const id = ipc.lspStart(
    language,
    root,
    (message) => handlers.forEach((handler) => handler(message)),
    (event) => {
      if (running === entry) serverEvent(event);
    },
  );
  const client = id.then(
    (serverId) => {
      const transport: Transport = {
        send: (message) => void ipc.lspSend(serverId, message),
        subscribe: (handler) => void handlers.add(handler),
        unsubscribe: (handler) => void handlers.delete(handler),
      };
      const created = new LSPClient({
        rootUri: pathToUri(root),
        // Loading a solution can keep a server busy for a while.
        timeout: 20_000,
        // Completion and signature help only come up while typing, in the
        // editors that allow it; so do quick fixes and rename (rename.ts
        // writes the files no editor shows). Formatting is left out.
        extensions: [
          serverCompletion,
          signatureHelp(),
          hoverTooltips(),
          keymap.of([...jumpToDefinitionKeymap, ...findReferencesKeymap]),
          // Before diagnostics: its publishDiagnostics hook only refreshes
          // and lets the notification through.
          semanticTokens(),
          serverDiagnostics(),
          clickToDefinition,
          noteJumpWithinFile,
          { clientCapabilities: codeActionCapabilities, editorExtension: codeActions },
          { clientCapabilities: { textDocument: { rename: { prepareSupport: true } } }, editorExtension: rename },
        ],
        notificationHandlers: {
          // Match the server's spelling of file URIs to ours; the built-in
          // handler then finds the open file.
          "textDocument/publishDiagnostics": (_client, params: { uri: string }) => {
            params.uri = canonicalUri(params.uri);
            return false;
          },
        },
        unhandledNotification: () => {},
      });
      created.workspace.displayFile = (uri) => {
        // Another file: the cursor is still where the jump started.
        jumped();
        return displayFile(canonicalUri(uri)).then(focusAfterJump);
      };
      created.connect(transport);
      return created;
    },
    (reason) => {
      startFailed(reason);
      return null;
    },
  );
  const entry: Running = { key, id, client };
  running = entry;
  return client;
}

/**
 * Keeps a file open with the running server although no editor shows it, so
 * that its errors are reported. Ignored while an editor has the file open.
 */
export async function openInBackground(uri: string, language: LanguageId, text: string) {
  const current = running;
  const client = await current?.client;
  if (!current || !client) return;
  // The server ignores documents sent before the initialize handshake is done.
  await client.initializing;
  if (running === current) await ipc.lspOpenBackground(await current.id, uri, language, text);
}

/** Stops the running server, if any. */
export async function stop() {
  const stopping = running;
  running = null;
  if (!stopping) return;
  try {
    (await stopping.client)?.disconnect();
    await ipc.lspStop(await stopping.id);
  } catch {
    // The server never started; nothing to stop.
  }
}
