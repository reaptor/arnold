// Explaining code with AI: the AI panel's conversation, signing in to GitHub
// for Copilot, and the code each question is about (a selection, files, or
// all the changes under review).

import { openUrl } from "@tauri-apps/plugin-opener";
import { createStore, produce } from "solid-js/store";
import * as ipc from "../ipc/commands";
import type { AiEvent, AiStatus, ContextItem, DeviceCode, ModelInfo, PatchOf, WorkingSide } from "../ipc/commands";
import * as commitMode from "./commitMode";
import { branchNames, type ExplainTarget, goToLine, state, takeExplainTarget } from "./review";

/** Code a question was about, shown with it; a click shows the code. */
export interface Chip {
  label: string;
  title?: string;
  path?: string;
  line?: number;
  side?: WorkingSide | null;
}

export interface Message {
  id: number;
  role: "user" | "assistant";
  /** The question, or the answer so far (Markdown). */
  text: string;
  chips?: Chip[];
  /** The answer is still being written. */
  streaming?: boolean;
  /** What Copilot is doing before the answer starts (downloading, starting). */
  status?: string | null;
  /** The model's reasoning summary, when it shares one. */
  reasoning?: string;
  /** The model that answered. */
  model?: string | null;
  error?: string;
  /** Stopped before it was complete. */
  stopped?: boolean;
}

interface AiState {
  open: boolean;
  width: number;
  status: AiStatus | null;
  /** Reading the status. */
  checking: boolean;
  /** Signing in: the code to enter (null until GitHub gave one). */
  signIn: { code: DeviceCode | null } | null;
  signInError: string | null;
  /** Downloading or starting Copilot; what it is doing. */
  preparing: string | null;
  prepareError: string | null;
  ready: boolean;
  models: ModelInfo[];
  /** The model picked; null lets Copilot choose. */
  model: string | null;
  conversation: string;
  messages: Message[];
  /** An answer is being written. */
  busy: boolean;
  /** Asked for while signed out: asked once signed in. */
  pending: (() => Promise<void>) | null;
}

const WIDTH_KEY = "arnold.ai.width";
const OPEN_KEY = "arnold.ai.open";
const MODEL_KEY = "arnold.ai.model";
export const MIN_WIDTH = 280;
export const MAX_WIDTH = 900;

const newConversation = () => `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;

const stored = (key: string) => {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
};
const store = (key: string, value: string | null) => {
  try {
    if (value === null) localStorage.removeItem(key);
    else localStorage.setItem(key, value);
  } catch {
    // Only a convenience.
  }
};

export const [ai, setAi] = createStore<AiState>({
  open: stored(OPEN_KEY) === "true",
  width: Number(stored(WIDTH_KEY)) || 400,
  status: null,
  checking: false,
  signIn: null,
  signInError: null,
  preparing: null,
  prepareError: null,
  ready: false,
  models: [],
  model: stored(MODEL_KEY),
  conversation: newConversation(),
  messages: [],
  busy: false,
  pending: null,
});

let nextId = 1;

export function setWidth(width: number) {
  setAi("width", Math.round(Math.min(Math.max(width, MIN_WIDTH), MAX_WIDTH)));
}

export const saveWidth = () => store(WIDTH_KEY, String(ai.width));

export function setOpen(open: boolean) {
  setAi("open", open);
  store(OPEN_KEY, String(open));
  if (open) void refresh();
}

export const toggleOpen = () => setOpen(!ai.open);

export function setModel(model: string | null) {
  setAi("model", model);
  store(MODEL_KEY, model);
}

/** Reads whether the user is signed in, and gets Copilot ready if so. */
export async function refresh() {
  if (ai.checking) return;
  setAi("checking", true);
  try {
    const status = await ipc.aiStatus();
    setAi("status", status);
    if (status.signedIn && !ai.ready && ai.preparing === null) void prepare();
  } catch (error) {
    setAi("prepareError", String(error));
  } finally {
    setAi("checking", false);
  }
}

/** Downloads (the first time) and starts Copilot, so the first question is answered quickly. */
async function prepare() {
  setAi({ preparing: "Starting GitHub Copilot…", prepareError: null });
  try {
    await ipc.aiPrepare((event) => {
      if (event.kind === "status") setAi("preparing", event.text);
    });
    setAi({ ready: true, status: ai.status && { ...ai.status, installed: true } });
    void loadModels();
    const pending = ai.pending;
    setAi("pending", null);
    if (pending) await pending();
  } catch (error) {
    setAi("prepareError", String(error));
  } finally {
    setAi("preparing", null);
  }
}

async function loadModels() {
  try {
    const models = await ipc.aiModels();
    setAi("models", models);
    if (ai.model && !models.some((model) => model.id === ai.model)) setModel(null);
  } catch {
    // The picker stays hidden; Copilot chooses.
  }
}

export async function signIn() {
  setAi({ signIn: { code: null }, signInError: null });
  try {
    const login = await ipc.aiSignIn((code) => {
      setAi("signIn", { code });
      // The page to enter it on opens by itself.
      void openUrl(code.verificationUri).catch(() => {});
    });
    setAi({ signIn: null, ready: false });
    setAi("status", (status) => ({
      signedIn: true,
      login,
      source: { kind: "keychain" },
      expired: false,
      installed: status?.installed ?? false,
    }));
    await prepare();
  } catch (error) {
    const text = String(error);
    setAi({ signIn: null, signInError: text === "cancelled" ? null : text });
  }
}

export async function cancelSignIn() {
  await ipc.aiCancelSignIn();
  setAi("signIn", null);
}

export async function signOut() {
  await stop();
  await ipc.aiSignOut();
  setAi({ ready: false, models: [], pending: null });
  await newChat();
  setAi("checking", false);
  await refresh();
}

/** Starts over: the next question does not see the earlier ones. */
export async function newChat() {
  if (ai.busy) await stop();
  const old = ai.conversation;
  setAi({ conversation: newConversation(), messages: [] });
  void ipc.aiEnd(old).catch(() => {});
}

/** The answer being written, if any, settles once stopped. */
let answering: Promise<void> | null = null;

export async function stop() {
  if (!ai.busy) return;
  await ipc.aiCancel(ai.conversation).catch(() => {});
  await answering;
}

const lastAnswer = () => {
  for (let index = ai.messages.length - 1; index >= 0; index--) if (ai.messages[index].role === "assistant") return index;
  return -1;
};

/**
 * Asks `question` about `context`; the question shows in the panel with
 * `chips`, then the answer streams in under it.
 */
export async function ask(question: string, context: ContextItem[] = [], chips: Chip[] = []) {
  if (!question.trim()) return;
  if (ai.busy) await stop();
  if (!ai.open) setOpen(true);
  if (!ai.status?.signedIn || !ai.ready) {
    // Asked once Copilot is ready.
    setAi("pending", () => () => ask(question, context, chips));
    if (!ai.status) await refresh();
    return;
  }
  const answerId = nextId + 1;
  setAi(
    produce((draft) => {
      draft.messages.push({ id: nextId++, role: "user", text: question, chips });
      draft.messages.push({ id: nextId++, role: "assistant", text: "", streaming: true, status: null });
      draft.busy = true;
    }),
  );
  const update = (change: (message: Message) => void) =>
    setAi(
      "messages",
      (message) => message.id === answerId,
      produce(change),
    );
  // Deltas arrive many per frame: appended once per frame.
  let buffered = "";
  let frame = 0;
  const flush = () => {
    frame = 0;
    if (!buffered) return;
    const text = buffered;
    buffered = "";
    update((message) => {
      message.text += text;
      message.status = null;
    });
  };
  const onEvent = (event: AiEvent) => {
    switch (event.kind) {
      case "delta":
        buffered += event.text;
        if (!frame) frame = requestAnimationFrame(flush);
        break;
      case "reasoning":
        update((message) => {
          message.reasoning = (message.reasoning ?? "") + event.text;
        });
        break;
      case "status":
        update((message) => {
          message.status = event.text;
        });
        break;
      case "done":
        flush();
        update((message) => {
          message.model = event.model;
        });
        break;
      case "error":
        update((message) => {
          message.error = event.message;
        });
        break;
    }
  };
  const request = {
    conversation: ai.conversation,
    model: ai.model,
    question,
    context,
    cwd: state.source === "commit" ? state.checkout : (state.comparison?.checkout ?? state.repo?.root ?? null),
  };
  const conversation = ai.conversation;
  answering = (async () => {
    try {
      await ipc.aiAsk(request, onEvent);
    } catch (error) {
      if (ai.conversation === conversation) update((message) => void (message.error = String(error)));
    } finally {
      if (frame) cancelAnimationFrame(frame);
      flush();
      if (ai.conversation === conversation) {
        update((message) => {
          message.streaming = false;
          message.status = null;
          if (!message.text && !message.error) message.stopped = true;
        });
      }
      setAi("busy", false);
      if (ai.models.length === 0) void loadModels();
    }
  })();
  await answering;
}

/** Marks the answer being written as stopped by the user. */
export async function stopAnswer() {
  const index = lastAnswer();
  if (index >= 0 && ai.messages[index].streaming) setAi("messages", index, "stopped", true);
  await stop();
}

// ---------------------------------------------------------------- what to explain

/** The diff the review shows, as `review_patch` takes it. Commit mode: of list `side`, else against the last commit. */
function patchSource(side: WorkingSide | null): { cwd: string; of: PatchOf } | null {
  if (state.source === "commit") {
    if (!state.checkout) return null;
    const of: PatchOf =
      side === "unstaged" ? { kind: "unstaged" } : side === "staged" ? { kind: "staged" } : { kind: "onDisk", from: "HEAD" };
    return { cwd: state.checkout, of };
  }
  const comparison = state.comparison;
  if (!comparison || !state.repo) return null;
  return comparison.checkout
    ? { cwd: comparison.checkout, of: { kind: "onDisk", from: comparison.mergeBase } }
    : { cwd: state.repo.root, of: { kind: "commits", from: comparison.mergeBase, to: comparison.headCommit } };
}

const OMIT_REASONS = { binary: "binary", lockFile: "lock file", tooLarge: "too large to include" } as const;

/** The patch of `paths` (all changes when empty) as context, and a note of the files left out. */
async function changesContext(paths: string[], side: WorkingSide | null, label: string): Promise<{ items: ContextItem[]; files: string[] }> {
  const source = patchSource(side);
  if (!source) return { items: [], files: [] };
  const patch = await ipc.reviewPatch(source.cwd, source.of, paths);
  const items: ContextItem[] = [];
  if (patch.text) items.push({ label, kind: "diff", text: patch.text });
  if (patch.omitted.length) {
    items.push({
      label: "Changed files not included above",
      kind: "note",
      text: patch.omitted.map((file) => `${file.path} (${OMIT_REASONS[file.reason]})`).join("\n"),
    });
  }
  return { items, files: patch.files };
}

/** What the review compares, for the model: branches, commits, the pull request. */
function reviewNote(): ContextItem | null {
  if (state.source === "commit") {
    const branch = state.work?.branch;
    return { label: "What is reviewed", kind: "note", text: `Uncommitted changes${branch ? ` on branch ${branch}` : ""}.` };
  }
  const comparison = state.comparison;
  if (!comparison) return null;
  const lines: string[] = [];
  const pullRequest = state.pullRequest;
  if (state.source === "pullRequest" && pullRequest) {
    lines.push(`Pull request !${pullRequest.id} by ${pullRequest.author}: ${pullRequest.title}`);
    lines.push(`From ${pullRequest.sourceBranch} into ${pullRequest.targetBranch}.`);
    if (pullRequest.description.trim()) lines.push("", "Description:", pullRequest.description.trim());
  } else {
    const names = branchNames();
    lines.push(`Branch ${names.head} compared with ${names.base}${comparison.checkout ? ", uncommitted edits included" : ""}.`);
  }
  if (comparison.commits.length) {
    lines.push("", "Commits (newest first):", ...comparison.commits.slice(0, 50).map((commit) => `- ${commit.subject}`));
  }
  return { label: "What is reviewed", kind: "note", text: lines.join("\n") };
}

/** The text of a file without changes, as the review shows it. */
async function wholeFile(path: string): Promise<string | null> {
  if (state.source === "commit") return state.checkout ? ipc.readWorktreeText(state.checkout, path) : null;
  const comparison = state.comparison;
  if (!comparison || !state.repo) return null;
  if (comparison.checkout) return ipc.readWorktreeText(comparison.checkout, path);
  const pair = await ipc.readFilePair(state.repo.root, comparison, {
    path,
    oldPath: null,
    status: "unchanged",
    additions: null,
    deletions: null,
    binary: false,
  });
  return pair.working;
}

const MAX_WHOLE_FILE = 60_000;

/** Explains a selection or line of an editor. */
async function explainCode(target: Extract<ExplainTarget, { kind: "code" }>) {
  const lines = target.startLine === target.endLine ? `line ${target.startLine}` : `lines ${target.startLine}–${target.endLine}`;
  const version = target.side === "base" ? "old version" : "new version";
  const context: ContextItem[] = [{ label: `${target.path}, ${lines} (${version})`, kind: "selection", text: target.text }];
  const changes = await changesContext([target.path], target.workSide, `Changes to ${target.path}`).catch(() => ({ items: [], files: [] }));
  context.push(...changes.items);
  if (target.fileText.length <= MAX_WHOLE_FILE) {
    context.push({ label: `${target.path} (${version}, whole file)`, kind: "file", text: target.fileText });
  }
  const question = target.selection
    ? `Explain the selected code${target.side === "base" ? " (from the old version of the file)" : ""}.`
    : "Explain this line.";
  const chip: Chip = {
    label: `${baseName(target.path)} · ${lines}${target.side === "base" ? " (old)" : ""}`,
    title: target.path,
    path: target.path,
    line: target.startLine,
    side: target.workSide,
  };
  await ask(question, context, [chip]);
}

/** Explains the changes to files or a folder; a file without changes is explained whole. */
async function explainFiles(target: Extract<ExplainTarget, { kind: "files" }>) {
  const label = target.folder ? `Changes in ${target.paths[0]}/` : target.paths.length === 1 ? `Changes to ${target.paths[0]}` : "Changes to the files";
  const changes = await changesContext(target.paths, target.workSide, label);
  const chips: Chip[] = target.paths.slice(0, 6).map((path) => ({
    label: target.folder ? `${baseName(path)}/` : baseName(path),
    title: path,
    path: target.folder ? undefined : path,
    line: 1,
    side: target.workSide,
  }));
  if (target.paths.length > 6) chips.push({ label: `+${target.paths.length - 6} more` });
  if (changes.files.length === 0 && !target.folder && target.paths.length === 1) {
    const path = target.paths[0];
    const text = await wholeFile(path);
    if (text === null) throw new Error(`Could not read ${path}.`);
    const context: ContextItem[] = [{ label: path, kind: "file", text: text.slice(0, 150_000) }];
    return ask("Explain this file.", context, chips);
  }
  if (changes.files.length === 0) {
    return ask("Explain these changes.", [{ label, kind: "note", text: "There are no text changes here." }], chips);
  }
  const note = reviewNote();
  const question = target.folder
    ? `Explain the changes in ${target.paths[0]}/.`
    : target.paths.length === 1
      ? `Explain the changes to ${target.paths[0]}.`
      : `Explain the changes to these ${target.paths.length} files.`;
  await ask(question, [...(note ? [note] : []), ...changes.items], chips);
}

const baseName = (path: string) => path.slice(path.lastIndexOf("/") + 1);

/** An "Explain…" item of the context menu was clicked. */
export function explainFromMenu(which: "code" | "files") {
  const target = takeExplainTarget(which);
  if (!target) return;
  if (!ai.open) setOpen(true);
  const run = target.kind === "code" ? explainCode(target) : explainFiles(target);
  run.catch((error) => failQuestion(String(error)));
}

/** Explains everything under review: the branch, pull request or uncommitted changes. */
export async function explainAll() {
  try {
    if (!ai.open) setOpen(true);
    const changes = await changesContext([], null, "All changes");
    const note = reviewNote();
    const count = changes.files.length;
    const chip: Chip = { label: `All changes · ${count} file${count === 1 ? "" : "s"}` };
    let question: string;
    if (state.source === "commit") question = "Explain my uncommitted changes.";
    else if (state.source === "pullRequest" && state.pullRequest) question = `Explain pull request !${state.pullRequest.id}.`;
    else question = `Explain the changes on ${branchNames().head || "this branch"}.`;
    if (count === 0) {
      return ask(question, [...(note ? [note] : []), { label: "All changes", kind: "note", text: "There are no text changes." }], [chip]);
    }
    await ask(question, [...(note ? [note] : []), ...changes.items], [chip]);
  } catch (error) {
    failQuestion(String(error));
  }
}

/** Whether there is something to explain as "all changes". */
export const canExplainAll = () =>
  state.source === "commit" ? !!state.checkout && !!state.work : !!state.comparison && !!state.repo;

function failQuestion(text: string) {
  setAi(
    produce((draft) => {
      draft.messages.push({ id: nextId++, role: "assistant", text: "", error: text });
    }),
  );
}

// ---------------------------------------------------------------- links in answers

/** Files a `path:line` in an answer may name: those of the review, with or without folders. */
function knownFiles(): string[] {
  if (state.source === "commit") {
    const paths = new Set([...commitMode.listOf("unstaged"), ...commitMode.listOf("staged")].map((file) => file.path));
    return [...paths];
  }
  return state.comparison?.files.map((file) => file.path) ?? [];
}

/**
 * The file and line `text` (an inline code span of an answer, such as
 * `src/Orders.fs:12` or `Orders.fs:12-20`) points at, if it is a file of the review.
 */
export function codeLink(text: string): { path: string; line: number } | null {
  const match = /^([\w./\\@+-]+?):(\d+)(?:[-–:]\d+)?$/.exec(text.trim());
  if (!match) return null;
  const wanted = match[1].replace(/\\/g, "/").replace(/^\.\//, "");
  const files = knownFiles();
  const path = files.find((file) => file === wanted) ?? files.find((file) => file.endsWith(`/${wanted}`));
  return path ? { path, line: Number(match[2]) } : null;
}

/** Shows the code a chip or link points at. Commit mode: in the diff it was asked about. */
export function showCode(path: string, line: number, side: WorkingSide | null = null) {
  if (state.source === "commit" && side === null) {
    const listed = (list: WorkingSide) => commitMode.listOf(list).some((file) => file.path === path);
    side = listed("unstaged") ? "unstaged" : listed("staged") ? "staged" : null;
  }
  void goToLine(path, line, side);
}
