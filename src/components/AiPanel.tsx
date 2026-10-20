import { createEffect, createSignal, For, on, onCleanup, onMount, Show } from "solid-js";
import { highlightCode } from "../editor/highlightCode";
import type { AiStatus } from "../ipc/commands";
import {
  ai,
  ask,
  canExplainAll,
  cancelSignIn,
  type Chip,
  codeLink,
  explainAll,
  type Message,
  newChat,
  refresh,
  setModel,
  setOpen,
  showCode,
  signIn,
  signOut,
  stopAnswer,
} from "../state/ai";
import { state } from "../state/review";
import { isMac } from "../platform";
import { openInBrowser } from "./DevOpsSetup";
import { openClickedLink, renderMarkdown } from "./Markdown";
import { CloseIcon, CopyIcon, ExternalIcon, NewChatIcon, SendIcon, SparkleIcon, StopIcon } from "./icons";

const PLANS = "https://github.com/features/copilot/plans";

/** The AI panel on the right: signing in, then a conversation about the code under review. */
export function AiPanel() {
  onMount(() => void refresh());
  return (
    <aside class="ai-panel" aria-label="AI">
      <header class="ai-header">
        <SparkleIcon />
        <span class="ai-title">Explain with Copilot</span>
        <span class="toolbar-space" />
        <Show when={ai.ready && ai.models.length > 0}>
          <select
            class="ai-model"
            value={ai.model ?? ""}
            onChange={(event) => setModel(event.currentTarget.value || null)}
            title="The model that answers"
          >
            <option value="">Auto</option>
            <For each={ai.models}>{(model) => <option value={model.id}>{model.name}</option>}</For>
          </select>
        </Show>
        <Show when={ai.status?.signedIn}>
          <button class="icon-button" onClick={() => void newChat()} disabled={ai.messages.length === 0} title="New chat: forget the questions so far">
            <NewChatIcon />
          </button>
        </Show>
        <button class="icon-button" onClick={() => setOpen(false)} title={`Close (${isMac ? "⌘⇧A" : "Ctrl+Shift+A"})`}>
          <CloseIcon />
        </button>
      </header>
      <Show when={ai.status} fallback={<div class="ai-empty">{ai.prepareError ?? "Checking the GitHub sign-in…"}</div>}>
        {(status) => (
          <Show when={status().signedIn} fallback={<SignIn />}>
            <Conversation />
          </Show>
        )}
      </Show>
    </aside>
  );
}

function SignIn() {
  const expired = () => ai.status?.expired;
  return (
    <div class="ai-signin">
      <h2>Explain code with your GitHub Copilot</h2>
      <p>
        Select code, right-click a file, or explain all the changes, and Copilot explains what they do. Arnold uses your
        own Copilot subscription; the free plan works too.
      </p>
      <Show when={expired()}>
        <p class="connect-problem">GitHub no longer accepts the saved sign-in. Sign in again.</p>
      </Show>
      <Show
        when={ai.signIn}
        fallback={
          <button class="primary" onClick={() => void signIn()}>
            Sign in with GitHub
          </button>
        }
      >
        {(signingIn) => (
          <div class="ai-device">
            <Show when={signingIn().code} fallback={<p>Asking GitHub for a code…</p>}>
              {(code) => (
                  <>
                    <p>Enter this code on GitHub and approve Arnold:</p>
                    <div class="ai-code">
                      <code>{code().userCode}</code>
                      <button class="icon-button" onClick={() => void navigator.clipboard.writeText(code().userCode)} title="Copy the code">
                        <CopyIcon />
                      </button>
                    </div>
                    <button class="connect-link" onClick={() => openInBrowser(code().verificationUri)}>
                      Open {code().verificationUri.replace(/^https:\/\//, "")}
                      <ExternalIcon />
                    </button>
                    <p class="hint">Waiting for GitHub…</p>
                  </>
              )}
            </Show>
            <button onClick={() => void cancelSignIn()}>Cancel</button>
          </div>
        )}
      </Show>
      <Show when={ai.signInError}>
        <p class="connect-problem">{ai.signInError}</p>
      </Show>
      <p class="hint">
        The code you ask about is sent to GitHub Copilot with your question. The first time, Arnold downloads the
        GitHub Copilot CLI (about 115 MB). No Copilot plan yet?{" "}
        <button class="link-button" onClick={() => openInBrowser(PLANS)}>
          See the plans
        </button>
      </p>
    </div>
  );
}

function Conversation() {
  let list!: HTMLDivElement;
  let input!: HTMLTextAreaElement;
  const [question, setQuestion] = createSignal("");
  /** Follows the answer while it is written, unless the user scrolled up. */
  let following = true;

  const send = () => {
    const text = question().trim();
    if (!text || !ai.ready) return;
    setQuestion("");
    void ask(text);
  };

  createEffect(
    on(
      () => {
        const last = ai.messages.at(-1);
        return [ai.messages.length, last?.text.length, last?.status, last?.error];
      },
      () => {
        if (following) queueMicrotask(() => (list.scrollTop = list.scrollHeight));
      },
    ),
  );
  createEffect(
    on(
      () => ai.messages.length,
      () => (following = true),
    ),
  );

  return (
    <>
      <div
        class="ai-messages"
        ref={list}
        onScroll={() => (following = list.scrollHeight - list.scrollTop - list.clientHeight < 40)}
      >
        <Show when={ai.preparing}>
          <div class="ai-status">
            <span class="spinner" />
            {ai.preparing}
          </div>
        </Show>
        <Show when={ai.prepareError}>
          <div class="ai-error">
            <p>{ai.prepareError}</p>
            <button onClick={() => void refresh()}>Try again</button>
          </div>
        </Show>
        <Show when={ai.messages.length === 0 && !ai.preparing && !ai.prepareError}>
          <div class="ai-empty">
            <p>
              Ask about the code under review. Right-click a selection, a line or a file and choose <strong>Explain</strong>, or:
            </p>
            <button disabled={!canExplainAll()} onClick={() => void explainAll()}>
              <SparkleIcon />
              {state.source === "commit"
                ? "Explain my uncommitted changes"
                : state.source === "pullRequest"
                  ? "Explain this pull request"
                  : "Explain all changes"}
            </button>
          </div>
        </Show>
        <For each={ai.messages}>{(message) => (message.role === "user" ? <Question message={message} /> : <Answer message={message} />)}</For>
      </div>
      <form
        class="ai-input"
        onSubmit={(event) => {
          event.preventDefault();
          send();
        }}
      >
        <textarea
          ref={input}
          rows={2}
          value={question()}
          placeholder={ai.ready ? (ai.messages.length ? "Ask a follow-up question" : "Ask about the code") : "Getting Copilot ready…"}
          disabled={!ai.ready}
          onInput={(event) => setQuestion(event.currentTarget.value)}
          onKeyDown={(event) => {
            if (event.key === "Enter" && !event.shiftKey && !event.isComposing) {
              event.preventDefault();
              send();
            }
          }}
        />
        <Show
          when={ai.busy}
          fallback={
            <button type="submit" class="icon-button primary" disabled={!ai.ready || !question().trim()} title="Ask (Enter)">
              <SendIcon />
            </button>
          }
        >
          <button type="button" class="icon-button" onClick={() => void stopAnswer()} title="Stop the answer">
            <StopIcon />
          </button>
        </Show>
      </form>
      <footer class="ai-footer">
        <Show when={ai.status}>
          {(status) => (
            <span class="hint" title={sourceTitle(status().source)}>
              GitHub Copilot{status().login ? ` · ${status().login}` : ""}
            </span>
          )}
        </Show>
        <span class="toolbar-space" />
        <Show when={canExplainAll() && ai.messages.length > 0}>
          <button class="link-button" onClick={() => void explainAll()}>
            Explain all changes
          </button>
        </Show>
        <Show when={ai.status?.source?.kind === "keychain"}>
          <button class="link-button" onClick={() => void signOut()}>
            Sign out
          </button>
        </Show>
      </footer>
    </>
  );
}

const sourceTitle = (source: AiStatus["source"]) =>
  source?.kind === "environment" ? `Signed in with the token in the ${source.name} environment variable` : undefined;

function ChipButton(props: { chip: Chip }) {
  const target = () => props.chip.path;
  return (
    <button
      class="ai-chip"
      disabled={!target()}
      title={props.chip.title ?? props.chip.label}
      onClick={() => props.chip.path && showCode(props.chip.path, props.chip.line ?? 1, props.chip.side ?? null)}
    >
      {props.chip.label}
    </button>
  );
}

function Question(props: { message: Message }) {
  return (
    <div class="ai-question">
      <Show when={props.message.chips?.length}>
        <div class="ai-chips">
          <For each={props.message.chips}>{(chip) => <ChipButton chip={chip} />}</For>
        </div>
      </Show>
      <div class="ai-question-text">{props.message.text}</div>
    </div>
  );
}

function Answer(props: { message: Message }) {
  let body!: HTMLDivElement;
  const [copied, setCopied] = createSignal(false);
  let copiedTimer = 0;
  onCleanup(() => clearTimeout(copiedTimer));

  // Rendered at most once a frame (the text grows a frame at a time); code blocks
  // are colored once the answer is complete.
  createEffect(() => {
    const text = props.message.text;
    const streaming = props.message.streaming;
    body.innerHTML = renderMarkdown(text);
    for (const code of body.querySelectorAll<HTMLElement>(":not(pre) > code")) {
      const link = codeLink(code.textContent ?? "");
      if (!link) continue;
      code.classList.add("code-link");
      code.dataset.path = link.path;
      code.dataset.line = String(link.line);
      code.title = `Show ${link.path}, line ${link.line}`;
    }
    if (!streaming) {
      for (const code of body.querySelectorAll<HTMLElement>("pre > code[data-lang]")) void highlightCode(code, code.dataset.lang!);
    }
  });

  const onClick = (event: MouseEvent) => {
    const code = (event.target as Element).closest<HTMLElement>("code.code-link");
    if (code?.dataset.path) {
      event.preventDefault();
      showCode(code.dataset.path, Number(code.dataset.line) || 1);
      return;
    }
    openClickedLink(event);
  };

  const copy = () => {
    void navigator.clipboard.writeText(props.message.text);
    setCopied(true);
    clearTimeout(copiedTimer);
    copiedTimer = window.setTimeout(() => setCopied(false), 1500);
  };

  return (
    <div class="ai-answer">
      <Show when={props.message.reasoning && !props.message.text}>
        <details class="ai-reasoning">
          <summary>Thinking…</summary>
          <div>{props.message.reasoning}</div>
        </details>
      </Show>
      <Show when={props.message.streaming && !props.message.text}>
        <div class="ai-status">
          <span class="spinner" />
          {props.message.status ?? "Thinking…"}
        </div>
      </Show>
      <div class="markdown ai-markdown" ref={body} onClick={onClick} />
      <Show when={props.message.error}>
        <div class="ai-error">
          <p>{props.message.error}</p>
        </div>
      </Show>
      <Show when={!props.message.streaming && (props.message.text || props.message.stopped)}>
        <div class="ai-answer-footer">
          <Show when={props.message.stopped}>
            <span class="hint">Stopped</span>
          </Show>
          <Show when={props.message.model}>
            <span class="hint">{props.message.model}</span>
          </Show>
          <span class="toolbar-space" />
          <Show when={props.message.text}>
            <button class="icon-button" onClick={copy} title="Copy the answer (Markdown)">
              {copied() ? <span class="hint">Copied</span> : <CopyIcon />}
            </button>
          </Show>
        </div>
      </Show>
    </div>
  );
}
