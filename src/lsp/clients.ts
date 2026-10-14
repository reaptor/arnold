// Language-server clients for the editor. The server process lives in the
// backend; messages travel over a Tauri channel.

import {
  jumpToDefinition,
  languageServerExtensions,
  LSPClient,
  LSPPlugin,
  type Transport,
} from "@codemirror/lsp-client";
import { type Extension, Prec } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import * as ipc from "../ipc/commands";
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

export function onDisplayFile(handler: DisplayFile) {
  displayFile = handler;
}

/** Called with the reason when a server could not be started. */
export function onStartFailed(handler: (reason: unknown) => void) {
  startFailed = handler;
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
  const id = ipc.lspStart(language, root, (message) => handlers.forEach((handler) => handler(message)));
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
        extensions: [...languageServerExtensions(), semanticTokens(), clickToDefinition],
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
      created.workspace.displayFile = (uri) => displayFile(canonicalUri(uri));
      created.connect(transport);
      return created;
    },
    (reason) => {
      startFailed(reason);
      return null;
    },
  );
  running = { key, id, client };
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
