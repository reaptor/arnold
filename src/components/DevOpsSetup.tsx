import { openUrl } from "@tauri-apps/plugin-opener";
import { createSignal, onMount, Show } from "solid-js";
import type { PrRemote } from "../ipc/commands";
import { platform } from "../platform";
import { prs, removeToken, saveToken } from "../state/pullRequests";
import { ExternalIcon, KeyIcon } from "./icons";

interface Props {
  remote: PrRemote;
  /** Changing a token that works: offers removing it and cancelling. */
  changing?: boolean;
  onCancel?(): void;
  /** "Compare branches instead". */
  onUseBranches?(): void;
}

const STORE_NAME =
  platform === "mac" ? "the macOS Keychain" : platform === "windows" ? "Windows Credential Manager" : "the system's credential store";

const ENVIRONMENT_VARIABLE = "AZURE_DEVOPS_EXT_PAT";

/** Opens `url` in the browser. */
export const openInBrowser = (url: string) => void openUrl(url).catch((error) => console.warn("could not open", url, error));

/**
 * How to set up a personal access token: what to do in Azure DevOps, then
 * a field to paste it into. The token is checked before it is saved.
 */
export function DevOpsSetup(props: Props) {
  const [token, setToken] = createSignal("");
  let input!: HTMLInputElement;

  // In the dialog the field is what the user came for; Escape then closes it.
  onMount(() => props.onCancel && input.focus());

  const rejected = () => !props.changing && prs.error?.kind === "tokenRejected";
  const fromEnvironment = () => props.remote.token === "environment";

  const submit = async (event: Event) => {
    event.preventDefault();
    if (await saveToken(token())) setToken("");
    else input.focus();
  };

  return (
    <div class="connect">
      <h2>
        <KeyIcon />
        {props.changing ? "Azure DevOps token" : "Connect Arnold to Azure DevOps"}
      </h2>
      <Show
        when={!props.changing}
        fallback={
          <p>
            Pull requests of <strong>{props.remote.name}</strong> are read with a personal access token
            {fromEnvironment() ? <> from the {ENVIRONMENT_VARIABLE} environment variable</> : <> saved in {STORE_NAME}</>}.
            Paste a new one below to replace it.
          </p>
        }
      >
        <p>
          The remote <code>{props.remote.remote}</code> is the Azure DevOps repository{" "}
          <strong>{props.remote.name}</strong>. To list its pull requests, Arnold needs a personal access token
          that can read code.
        </p>
      </Show>
      <Show when={rejected()}>
        <p class="connect-problem">
          Azure DevOps did not accept the token
          {fromEnvironment() ? <> in the {ENVIRONMENT_VARIABLE} environment variable</> : " Arnold has saved"}. It may
          have expired or been revoked. Create a new one as below.
        </p>
      </Show>

      <ol class="connect-steps">
        <li>
          <h3>Create a token in Azure DevOps</h3>
          <button class="connect-link" onClick={() => openInBrowser(props.remote.tokenSettingsUrl)}>
            Open Personal access tokens for {props.remote.organization}
            <ExternalIcon />
          </button>
          <ul>
            <li>
              Select <strong>+ New Token</strong>.
            </li>
            <li>
              Name it, for example <em>Arnold</em>, keep organization <strong>{props.remote.organization}</strong>,
              and pick when it expires.
            </li>
            <li>
              Under <strong>Scopes</strong>, choose <strong>Custom defined</strong>, then tick{" "}
              <strong>Code → Read &amp; write</strong> (<strong>Read</strong> is enough if you will not comment, vote or complete). Nothing
              else is needed.
            </li>
            <li>
              Select <strong>Create</strong> and copy the token. Azure DevOps shows it only once.
            </li>
          </ul>
        </li>
        <li>
          <h3>Paste it here</h3>
          <form class="connect-token" onSubmit={submit}>
            <input
              ref={input}
              type="password"
              placeholder="Personal access token"
              autocomplete="off"
              spellcheck={false}
              value={token()}
              onInput={(event) => setToken(event.currentTarget.value)}
              disabled={prs.checkingToken}
            />
            <button class="primary" type="submit" disabled={prs.checkingToken || token().trim() === ""}>
              {prs.checkingToken ? "Checking…" : "Connect"}
            </button>
          </form>
          <Show when={prs.tokenError}>
            <p class="connect-problem">{prs.tokenError}</p>
          </Show>
          <p class="connect-note">
            Arnold checks the token with Azure DevOps first, then keeps it in {STORE_NAME}. It is sent only to{" "}
            {new URL(props.remote.tokenSettingsUrl).host}, to list pull requests and fetch their commits. Arnold changes
            things there only when you ask it to: comments, votes, and creating or completing a pull request (creating
            one pushes the branch you reviewed if Azure DevOps does not have it yet).
          </p>
          <p class="connect-note">
            Already using the Azure CLI? Arnold also reads a token from the <code>{ENVIRONMENT_VARIABLE}</code>{" "}
            environment variable when none is saved.
          </p>
        </li>
      </ol>

      <div class="connect-actions">
        <Show when={props.changing && props.remote.token === "keychain"}>
          <button onClick={() => void removeToken()}>Remove saved token</button>
        </Show>
        <span class="toolbar-space" />
        <Show when={props.onUseBranches}>
          <button onClick={() => props.onUseBranches?.()}>Compare branches instead</button>
        </Show>
        <Show when={props.onCancel}>
          <button onClick={() => props.onCancel?.()}>Cancel</button>
        </Show>
      </div>
    </div>
  );
}
