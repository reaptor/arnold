import { createResource, createSignal, Match, onMount, Show, Switch } from "solid-js";
import * as ipc from "../ipc/commands";
import type { NewPullRequestPlan, PrRemote } from "../ipc/commands";
import { currentRemote, loadPullRequests, openTokenDialog, prErrorText } from "../state/pullRequests";
import {
  defaultDescription,
  defaultTitle,
  DESCRIPTION_LIMIT,
  rememberDraft,
  rememberedDraft,
} from "../state/newPullRequest";
import { openPullRequest, state } from "../state/review";
import { DevOpsSetup, openInBrowser } from "./DevOpsSetup";
import { ExternalIcon, PullRequestIcon } from "./icons";
import { Markdown, MARKDOWN_GUIDE } from "./Markdown";

/**
 * "Create pull request", for the toolbar while two branches are compared:
 * once the branch is reviewed, colleagues review it on Azure DevOps.
 */
export function CreatePullRequestButton() {
  const [open, setOpen] = createSignal(false);
  const ready = () => (state.comparison?.commits.length ?? 0) > 0;
  return (
    <Show when={currentRemote()}>
      {(remote) => (
        <>
          <button
            class="create-pr-button"
            data-tauri-drag-region="false"
            onClick={() => setOpen(true)}
            disabled={!ready()}
            title={
              ready()
                ? `Open a pull request from ${state.head} into ${state.base} on Azure DevOps`
                : `${state.head} has no commits that ${state.base} lacks`
            }
          >
            <PullRequestIcon />
            <span class="action-label">Create pull request</span>
          </button>
          <Show when={open() && state.comparison}>
            {(comparison) => (
              <CreateDialog
                remote={remote()}
                head={state.head}
                base={state.base}
                commit={comparison().tipCommit}
                onClose={() => setOpen(false)}
              />
            )}
          </Show>
        </>
      )}
    </Show>
  );
}

const short = (sha: string | null | undefined) => sha?.slice(0, 7) ?? "";
const plural = (count: number, word: string) => `${count} ${word}${count === 1 ? "" : "s"}`;

interface DialogProps {
  remote: PrRemote;
  head: string;
  base: string;
  /** The tip of `head` as reviewed: what is pushed. */
  commit: string;
  onClose(): void;
}

function CreateDialog(props: DialogProps) {
  const root = () => state.repo?.root ?? "";
  const commits = () => state.comparison?.commits ?? [];
  const [plan, { refetch }] = createResource(
    () =>
      props.remote.token && {
        root: root(),
        remote: props.remote.remote,
        head: props.head,
        base: props.base,
        commit: props.commit,
      },
    (key) => ipc.planPullRequest(key.root, key.remote, key.head, key.base, key.commit),
  );
  const [title, setTitle] = createSignal(defaultTitle(props.head, commits()));
  const [description, setDescription] = createSignal(defaultDescription(commits()));
  const [preview, setPreview] = createSignal(false);
  const [draft, setDraft] = createSignal(rememberedDraft());
  const [busy, setBusy] = createSignal(false);
  const [error, setError] = createSignal<string | null>(null);
  let titleInput: HTMLInputElement | undefined;

  onMount(() => titleInput?.select());

  const close = () => !busy() && props.onClose();
  // Reading a resource that failed throws; this does not.
  const known = (): NewPullRequestPlan | undefined => (plan.state === "ready" ? plan() : undefined);
  const usable = (): NewPullRequestPlan | null => {
    const current = known();
    return current && !current.existing && current.push !== "conflict" && current.commits > 0 ? current : null;
  };
  const pushes = () => usable()?.push === "create" || usable()?.push === "update";
  const tooLong = () => description().length > DESCRIPTION_LIMIT;

  const submit = async (event: Event) => {
    event.preventDefault();
    if (!usable() || !title().trim() || tooLong() || busy()) return;
    rememberDraft(draft());
    setBusy(true);
    setError(null);
    try {
      const created = await ipc.createPullRequest(root(), props.remote.remote, {
        head: props.head,
        base: props.base,
        commit: props.commit,
        title: title().trim(),
        description: description(),
        draft: draft(),
      });
      setBusy(false);
      props.onClose();
      void loadPullRequests();
      await openPullRequest(props.remote.remote, created.id);
    } catch (failure) {
      setBusy(false);
      setError(prErrorText(failure));
      // A failed push may still have pushed; the plan says where things are now.
      void refetch();
    }
  };

  const reviewExisting = (id: number) => {
    props.onClose();
    void openPullRequest(props.remote.remote, id);
  };

  return (
    <div
      class="modal-backdrop"
      data-tauri-drag-region="false"
      onPointerDown={(event) => event.target === event.currentTarget && close()}
      onKeyDown={(event) => event.key === "Escape" && close()}
    >
      <Show
        when={props.remote.token}
        fallback={
          <div class="modal" role="dialog" aria-label="Azure DevOps token">
            <DevOpsSetup remote={props.remote} onCancel={close} />
          </div>
        }
      >
        <form
          class="modal complete-dialog create-dialog"
          role="dialog"
          aria-label="Create pull request"
          onSubmit={submit}
        >
          <h2>
            <PullRequestIcon />
            Create pull request
          </h2>
          <p class="pr-subtitle">
            From <strong>{known()?.sourceBranch ?? props.head}</strong> into{" "}
            <strong>{known()?.targetBranch ?? props.base}</strong> in {props.remote.name}
          </p>

          <Switch>
            <Match when={plan.loading}>
              <p class="create-status">Checking the branches on Azure DevOps…</p>
            </Match>
            <Match when={plan.error}>
              <p class="connect-problem">{prErrorText(plan.error)}</p>
              <div class="create-status-actions">
                <Show when={ipc.isPrError(plan.error) && plan.error.kind === "tokenRejected"}>
                  <button type="button" onClick={openTokenDialog}>
                    Change token…
                  </button>
                </Show>
                <button type="button" onClick={() => void refetch()}>
                  Try again
                </button>
              </div>
            </Match>
            <Match when={known()?.existing}>
              {(existing) => (
                <div class="complete-warning">
                  <p>
                    Pull request <strong>!{existing().id}</strong> from {existing().sourceBranch} into{" "}
                    {existing().targetBranch} is open already: {existing().title}
                  </p>
                  <div class="create-status-actions">
                    <button type="button" class="primary" onClick={() => reviewExisting(existing().id)}>
                      Review it in Arnold
                    </button>
                    <button type="button" onClick={() => openInBrowser(existing().url)}>
                      <ExternalIcon />
                      Open in Azure DevOps
                    </button>
                  </div>
                </div>
              )}
            </Match>
            <Match when={known()?.push === "conflict" && known()}>
              {(current) => (
                <p class="connect-problem">
                  {current().sourceBranch} on Azure DevOps is at <code>{short(current().serverCommit)}</code>, which has
                  commits that are not in what you reviewed (up to <code>{short(props.commit)}</code>). Arnold never
                  pushes over them: fetch, review the branch as it is there, then create the pull request.
                </p>
              )}
            </Match>
            <Match when={known()?.commits === 0 && known()}>
              {(current) => (
                <p class="connect-problem">
                  {props.head} has nothing that {current().targetBranch} on Azure DevOps lacks.
                </p>
              )}
            </Match>
            <Match when={known()}>
              {(current) => (
                <p class="create-status" classList={{ "create-push": pushes() }}>
                  <Switch>
                    <Match when={current().push === "create"}>
                      {current().sourceBranch} is not on Azure DevOps yet. Arnold first pushes the commit you reviewed,{" "}
                      <code>{short(props.commit)}</code>, as a new branch with {plural(current().pushCommits, "commit")}
                      .
                    </Match>
                    <Match when={current().push === "update"}>
                      Arnold first pushes {plural(current().pushCommits, "new commit")} to {current().sourceBranch} on
                      Azure DevOps, up to <code>{short(props.commit)}</code>, the commit you reviewed.
                    </Match>
                    <Match when={current().push === "none"}>
                      {current().sourceBranch} on Azure DevOps is at <code>{short(props.commit)}</code>, the commit you
                      reviewed; nothing is pushed.
                    </Match>
                  </Switch>{" "}
                  The pull request has {plural(current().commits, "commit")}.
                </p>
              )}
            </Match>
          </Switch>

          <Show when={!known()?.existing}>
            <label class="complete-message">
              <span>Title</span>
              <input
                ref={titleInput}
                type="text"
                value={title()}
                onInput={(event) => setTitle(event.currentTarget.value)}
                maxLength={400}
                required
              />
            </label>

            <div class="complete-message">
              <div class="composer-bar">
                <span>Description</span>
                <button
                  type="button"
                  class="composer-tab"
                  classList={{ on: !preview() }}
                  onClick={() => setPreview(false)}
                >
                  Write
                </button>
                <button
                  type="button"
                  class="composer-tab"
                  classList={{ on: preview() }}
                  onClick={() => setPreview(true)}
                >
                  Preview
                </button>
                <span class="composer-space" />
                <a
                  class="composer-help"
                  href={MARKDOWN_GUIDE}
                  onClick={(event) => {
                    event.preventDefault();
                    openInBrowser(MARKDOWN_GUIDE);
                  }}
                >
                  Markdown supported
                </a>
              </div>
              <Show
                when={preview()}
                fallback={
                  <textarea
                    rows={7}
                    value={description()}
                    placeholder="What changed and why, for the reviewers"
                    onInput={(event) => setDescription(event.currentTarget.value)}
                  />
                }
              >
                <Show when={description().trim()} fallback={<p class="create-empty">Nothing to preview.</p>}>
                  <Markdown class="create-preview" text={description()} />
                </Show>
              </Show>
              <Show when={tooLong()}>
                <small class="create-limit">
                  {description().length} of {DESCRIPTION_LIMIT} characters: Azure DevOps takes no more.
                </small>
              </Show>
            </div>

            <label class="complete-option">
              <input type="checkbox" checked={draft()} onChange={(event) => setDraft(event.currentTarget.checked)} />
              <span>
                Create as a draft
                <small>Reviewers are not notified until you publish it.</small>
              </span>
            </label>

            <Show when={state.range}>
              <p class="create-status">
                Only some commits are shown right now; the pull request has all of {props.head}'s commits.
              </p>
            </Show>
          </Show>

          <Show when={error()}>{(text) => <p class="connect-problem">{text()}</p>}</Show>

          <div class="connect-actions">
            <span class="toolbar-space" />
            <button type="button" onClick={close} disabled={busy()}>
              {known()?.existing ? "Close" : "Cancel"}
            </button>
            <Show when={!known()?.existing}>
              <button type="submit" class="primary" disabled={busy() || !usable() || !title().trim() || tooLong()}>
                {busy()
                  ? pushes()
                    ? "Pushing and creating…"
                    : "Creating…"
                  : `${pushes() ? "Push and create" : "Create"}${draft() ? " draft" : ""}`}
              </button>
            </Show>
          </div>
        </form>
      </Show>
    </div>
  );
}
