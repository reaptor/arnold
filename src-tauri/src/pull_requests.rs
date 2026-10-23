//! Pull requests of the repository's Azure DevOps remotes, as an alternative to
//! picking two branches.
//!
//! The webview names a remote, never a URL: where requests go (and the token
//! with them) is always worked out here from the repository's own remotes.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use arnold_pr::azure::{self, AzureRepo};
use arnold_pr::token::{self, TokenSource};
use arnold_pr::{
    suggestion, CompletionPolicies, NewPullRequest, NewThreadPlace, PrChange, PrError, PullRequest, Side, Thread, Vote, WorkItemLookup,
};
use serde::{Deserialize, Serialize};
use tauri::State;

use crate::commands::AppState;

#[derive(Default)]
pub struct PrState {
    /// Tokens read so far, by `AzureRepo::token_key`, so the credential store
    /// (which may ask the user for access) is read once per run.
    tokens: Mutex<HashMap<String, Option<(String, TokenSource)>>>,
    /// Who each of those tokens belongs to (the host's id), once asked.
    users: Mutex<HashMap<String, String>>,
}

/// A remote whose pull requests Arnold can list.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PrRemote {
    /// The git remote's name (`origin`).
    remote: String,
    /// Only `azureDevOps` so far.
    provider: &'static str,
    /// `contoso / Shop / shop-api`.
    name: String,
    organization: String,
    web_url: String,
    /// Where a personal access token is created.
    token_settings_url: String,
    /// Where the token comes from, or `None` when there is none yet.
    token: Option<TokenSource>,
}

fn other(error: impl std::fmt::Display) -> PrError {
    PrError::Other(error.to_string())
}

async fn blocking<T: Send + 'static>(work: impl FnOnce() -> T + Send + 'static) -> Result<T, PrError> {
    tokio::task::spawn_blocking(work).await.map_err(other)
}

async fn azure_remotes(root: &Path) -> Result<Vec<(String, AzureRepo)>, PrError> {
    let remotes = arnold_git::remotes(root).await.map_err(other)?;
    let mut found: Vec<(String, AzureRepo)> = Vec::new();
    for remote in remotes {
        let Some(repo) = azure::parse_remote_url(&remote.url) else { continue };
        // `origin` and a second remote for the same repository: list it once.
        if !found.iter().any(|(_, known)| *known == repo) {
            found.push((remote.name, repo));
        }
    }
    Ok(found)
}

async fn resolve(app: &AppState, root: &str, remote: &str) -> Result<(PathBuf, AzureRepo), PrError> {
    let root = app.checked_root(root).map_err(PrError::Other)?;
    let repo = azure_remotes(&root)
        .await?
        .into_iter()
        .find(|(name, _)| name == remote)
        .map(|(_, repo)| repo)
        .ok_or_else(|| PrError::NotFound(format!("{remote} is not an Azure DevOps remote of this repository")))?;
    Ok((root, repo))
}

impl PrState {
    async fn token(&self, repo: &AzureRepo) -> Result<Option<(String, TokenSource)>, PrError> {
        let key = repo.token_key();
        if let Some(known) = self.tokens.lock().unwrap().get(&key) {
            return Ok(known.clone());
        }
        let loaded = {
            let key = key.clone();
            blocking(move || token::load(&key)).await?.map_err(PrError::Other)?
        };
        self.tokens.lock().unwrap().insert(key, loaded.clone());
        Ok(loaded)
    }

    async fn required_token(&self, repo: &AzureRepo) -> Result<String, PrError> {
        self.token(repo).await?.map(|(token, _)| token).ok_or(PrError::NoToken)
    }

    /// The host's id of whoever the token belongs to.
    async fn user(&self, repo: &AzureRepo, token: &str) -> Result<String, PrError> {
        let key = repo.token_key();
        if let Some(known) = self.users.lock().unwrap().get(&key) {
            return Ok(known.clone());
        }
        let user = {
            let (repo, token) = (repo.clone(), token.to_string());
            blocking(move || azure::current_user(&repo, &token)).await??
        };
        self.users.lock().unwrap().insert(key, user.clone());
        Ok(user)
    }

    /// Marks the token's owner among the reviewers, and as the author, when
    /// the host can say who that is.
    async fn mark_you(&self, repo: &AzureRepo, token: &str, mut pull_request: PullRequest) -> PullRequest {
        if let Ok(user) = self.user(repo, token).await {
            pull_request.yours = !pull_request.author_id.is_empty() && pull_request.author_id.eq_ignore_ascii_case(&user);
            for reviewer in &mut pull_request.reviewers {
                reviewer.you = reviewer.id.eq_ignore_ascii_case(&user);
            }
        }
        pull_request
    }

    /// The credentials header for pushing to `remote` when it is an Azure
    /// DevOps remote with a token set up; else git's own credentials do.
    pub(crate) async fn push_header(&self, root: &Path, remote: &str) -> Option<String> {
        let (_, repo) = azure_remotes(root).await.ok()?.into_iter().find(|(name, _)| name == remote)?;
        let (token, _) = self.token(&repo).await.ok()??;
        Some(format!("Authorization: {}", azure::authorization(&token)))
    }

    fn forget(&self, key: &str) {
        self.tokens.lock().unwrap().remove(key);
        self.users.lock().unwrap().remove(key);
    }
}

/// The repository's Azure DevOps remotes, and whether a token is set up for each.
#[tauri::command]
pub async fn pull_request_remotes(
    root: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<Vec<PrRemote>, PrError> {
    let root = app.checked_root(&root).map_err(PrError::Other)?;
    let mut result = Vec::new();
    for (remote, repo) in azure_remotes(&root).await? {
        // A credential store that cannot be read is the same as no token here;
        // listing pull requests reports the actual problem.
        let token = prs.token(&repo).await.ok().flatten().map(|(_, source)| source);
        result.push(PrRemote {
            remote,
            provider: "azureDevOps",
            name: repo.display_name(),
            organization: repo.organization.clone(),
            web_url: repo.web_url(),
            token_settings_url: repo.token_settings_url(),
            token,
        });
    }
    Ok(result)
}

/// The active pull requests of `remote`, newest first.
#[tauri::command]
pub async fn list_pull_requests(
    root: String,
    remote: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<Vec<PullRequest>, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let list = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || azure::active_pull_requests(&repo, &token)).await??
    };
    // Which are the user's to follow (notifications); the user is asked once per token.
    let mut marked = Vec::with_capacity(list.len());
    for pull_request in list {
        marked.push(prs.mark_you(&repo, &token, pull_request).await);
    }
    Ok(marked)
}

/// Checks `token` against `remote`, then saves it for the remote's
/// organization. Resolves to the active pull requests; a token the host
/// turns down is not saved.
#[tauri::command]
pub async fn set_pull_request_token(
    root: String,
    remote: String,
    token: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<Vec<PullRequest>, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = token.trim().to_string();
    if token.is_empty() {
        return Err(PrError::NoToken);
    }
    let pull_requests = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || azure::active_pull_requests(&repo, &token)).await??
    };
    let key = repo.token_key();
    {
        let (key, token) = (key.clone(), token.clone());
        blocking(move || token::save(&key, &token)).await?.map_err(PrError::Other)?;
    }
    prs.forget(&key);
    prs.tokens.lock().unwrap().insert(key, Some((token, TokenSource::Keychain)));
    Ok(pull_requests)
}

/// Forgets the saved token of `remote`'s organization. Resolves to where a
/// token still comes from (the environment), if anywhere.
#[tauri::command]
pub async fn remove_pull_request_token(
    root: String,
    remote: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<Option<TokenSource>, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let key = repo.token_key();
    {
        let key = key.clone();
        blocking(move || token::delete(&key)).await?.map_err(PrError::Other)?;
    }
    prs.forget(&key);
    Ok(prs.token(&repo).await?.map(|(_, source)| source))
}

/// Reads pull request `id` and makes sure its commits are in the repository,
/// fetching them from Azure DevOps if not. Only objects are fetched: no
/// branch or remote-tracking branch moves. Resolves to the pull request,
/// whose `targetCommit` → `sourceCommit` is then compared like two branches.
#[tauri::command]
pub async fn open_pull_request(
    root: String,
    remote: String,
    id: u64,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<PullRequest, PrError> {
    let (root, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let pull_request = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || azure::pull_request(&repo, &token, id)).await??
    };
    let (Some(source), Some(target)) = (&pull_request.source_commit, &pull_request.target_commit) else {
        return Err(PrError::Other(
            "Azure DevOps is still processing the latest push to this pull request. Try again in a moment.".into(),
        ));
    };
    if !arnold_git::has_commits(&root, &[source, target]).await {
        let refs = [
            format!("refs/heads/{}", pull_request.source_branch),
            format!("refs/heads/{}", pull_request.target_branch),
        ];
        let refs: Vec<&str> = refs.iter().map(String::as_str).collect();
        arnold_git::fetch_objects(&root, &repo.web_url(), &refs, Some(&format!("Authorization: {}", azure::authorization(&token))))
            .await
            .map_err(|e| PrError::Other(format!("Could not fetch the pull request's commits: {e}")))?;
        if !arnold_git::has_commits(&root, &[source, target]).await {
            return Err(PrError::Other(
                "The pull request's branches moved on since Azure DevOps last updated it. Try again in a moment.".into(),
            ));
        }
    }
    Ok(prs.mark_you(&repo, &token, pull_request).await)
}

/// Pull request `id` of `remote` as the host has it now, whatever its state
/// (completed, abandoned). Unlike `open_pull_request`, nothing is fetched.
#[tauri::command]
pub async fn read_pull_request(
    root: String,
    remote: String,
    id: u64,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<PullRequest, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let pull_request = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || azure::pull_request(&repo, &token, id)).await??
    };
    Ok(prs.mark_you(&repo, &token, pull_request).await)
}

/// Votes on pull request `id` as the token's owner; `none` resets the vote.
/// Resolves to the pull request with the votes as they are now.
#[tauri::command]
pub async fn vote_on_pull_request(
    root: String,
    remote: String,
    id: u64,
    vote: Vote,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<PullRequest, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let user = prs.user(&repo, &token).await?;
    let pull_request = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || {
            azure::vote(&repo, &token, id, &user, vote)?;
            azure::pull_request(&repo, &token, id)
        })
        .await??
    };
    Ok(prs.mark_you(&repo, &token, pull_request).await)
}

/// Completes, abandons, reactivates, drafts or publishes pull request `id`.
#[tauri::command]
pub async fn update_pull_request(
    root: String,
    remote: String,
    id: u64,
    change: PrChange,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<PullRequest, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let pull_request = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || azure::update(&repo, &token, id, &change)).await??
    };
    Ok(prs.mark_you(&repo, &token, pull_request).await)
}

/// What the branch policies of pull request `id`'s target branch allow when
/// completing it, for the Complete dialog.
#[tauri::command]
pub async fn completion_policies(
    root: String,
    remote: String,
    id: u64,
    target_branch: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<CompletionPolicies, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    blocking(move || azure::completion_policies(&repo, &token, id, &target_branch)).await?
}

/// The discussions of pull request `id`, with positions as of its latest push.
#[tauri::command]
pub async fn pull_request_threads(
    root: String,
    remote: String,
    id: u64,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<Vec<Thread>, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let mut threads = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || azure::threads(&repo, &token, id)).await??
    };
    // Who may edit and delete what; without knowing, nobody's comments are marked.
    if let Ok(user) = prs.user(&repo, &token).await {
        for comment in threads.iter_mut().flat_map(|thread| thread.comments.iter_mut()) {
            comment.you = !comment.author_id.is_empty() && comment.author_id.eq_ignore_ascii_case(&user);
        }
    }
    Ok(threads)
}

/// Deletes comment `comment` of thread `thread` of pull request `id`: one the
/// token's owner wrote (the host refuses others).
#[tauri::command]
#[allow(clippy::too_many_arguments)] // Tauri passes each argument and state separately.
pub async fn delete_pull_request_comment(
    root: String,
    remote: String,
    id: u64,
    thread: u64,
    comment: u64,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<(), PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    blocking(move || azure::delete_comment(&repo, &token, id, thread, comment)).await?
}

/// Changes the text of comment `comment` of thread `thread` of pull request
/// `id`: one the token's owner wrote (the host refuses others).
#[tauri::command]
#[allow(clippy::too_many_arguments)] // Tauri passes each argument and state separately.
pub async fn edit_pull_request_comment(
    root: String,
    remote: String,
    id: u64,
    thread: u64,
    comment: u64,
    content: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<(), PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    blocking(move || azure::edit_comment(&repo, &token, id, thread, comment, &content)).await?
}

/// Starts a discussion on pull request `id`, about `place` in a file or, without
/// one, about the pull request as a whole.
#[tauri::command]
pub async fn create_pull_request_thread(
    root: String,
    remote: String,
    id: u64,
    place: Option<NewThreadPlace>,
    content: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<Thread, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    blocking(move || azure::create_thread(&repo, &token, id, place.as_ref(), &content)).await?
}

/// Answers comment `parent` of thread `thread` of pull request `id`.
#[tauri::command]
#[allow(clippy::too_many_arguments)] // Tauri passes each argument and state separately.
pub async fn reply_to_pull_request_thread(
    root: String,
    remote: String,
    id: u64,
    thread: u64,
    parent: u64,
    content: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<(), PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    blocking(move || azure::reply(&repo, &token, id, thread, parent, &content)).await?
}

/// A suggestion accepted: the commit that applied it, and whether its
/// thread could be marked resolved too.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AcceptedSuggestion {
    commit: String,
    resolved: bool,
}

/// Accepts the suggestion of thread `thread` of pull request `id`, as the
/// web page's "Apply change" does: commits the suggested lines to the source
/// branch on top of `commit` (its latest push, which the suggestion's lines
/// are of; the host refuses if the branch has moved on), then marks the
/// thread resolved.
#[tauri::command]
#[allow(clippy::too_many_arguments)] // Tauri passes each argument and state separately.
pub async fn accept_pull_request_suggestion(
    root: String,
    remote: String,
    id: u64,
    thread: u64,
    commit: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<AcceptedSuggestion, PrError> {
    let (root, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let (pull_request, threads) = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || Ok::<_, PrError>((azure::pull_request(&repo, &token, id)?, azure::threads(&repo, &token, id)?))).await??
    };
    if pull_request.source_commit.as_deref() != Some(commit.as_str()) {
        return Err(PrError::Other(
            "The pull request has a new push since its suggestions were read. Look at it first, then accept the suggestion again.".into(),
        ));
    }
    let found = threads
        .into_iter()
        .find(|candidate| candidate.id == thread)
        .ok_or_else(|| PrError::NotFound("The suggestion is no longer on the pull request.".into()))?;
    let suggested = found.comments.first().and_then(|first| suggestion::suggested_lines(&first.content));
    let (Some(path), Some(position), Some(lines)) = (found.path.as_deref(), found.position, suggested) else {
        return Err(PrError::Other("This discussion holds no suggestion that can be accepted.".into()));
    };
    if position.side != Side::Head {
        return Err(PrError::Other("The suggestion is about lines the pull request removes, so it cannot be applied.".into()));
    }
    let original = arnold_git::read_blob(&root, &commit, path)
        .await
        .map_err(other)?
        .ok_or_else(|| PrError::NotFound(format!("{path} is not in the pull request's latest push.")))?;
    let content = suggestion::replace_lines(&original, position.start_line, position.end_line, &lines).map_err(PrError::Other)?;
    let author = &found.comments[0].author;
    let message = format!("Apply suggestion from {author} to {path}");
    let branch = pull_request.source_branch.clone();
    let path = path.to_string();
    blocking(move || {
        let commit = azure::push_file(&repo, &token, &branch, &commit, &path, &content, &message)?;
        // The change is in; a thread left open is no reason to report a failure.
        let resolved = azure::resolve_thread(&repo, &token, id, thread).is_ok();
        Ok(AcceptedSuggestion { commit, resolved })
    })
    .await?
}

// ------------------------------------------------------- creating one

/// What creating a pull request from a reviewed branch would do, worked
/// out before anything is sent.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NewPullRequestPlan {
    /// The branches on the host, without `refs/heads/`.
    source_branch: String,
    target_branch: String,
    /// Where the host's source branch is now; `None` when it has none.
    server_commit: Option<String>,
    push: PushPlan,
    /// Commits of the reviewed commit that the target branch lacks: what
    /// the pull request will have.
    commits: u32,
    /// Commits the push sends.
    push_commits: u32,
    /// An active pull request between the same branches, which the host
    /// allows only one of.
    existing: Option<PullRequest>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
enum PushPlan {
    /// The host's branch is at the reviewed commit already.
    None,
    /// The host has no such branch: it is pushed as a new one.
    Create,
    /// The host's branch is behind the reviewed commit: fast-forwarded.
    Update,
    /// The host's branch has commits that were not reviewed; nothing is
    /// pushed over them.
    Conflict,
}

/// The branch a reviewed branch is on the host: its own name for a local
/// branch, without the remote for a remote-tracking one (`origin/feature/x`).
async fn host_branch(root: &Path, name: &str) -> Result<String, PrError> {
    let branches = arnold_git::list_branches(root).await.map_err(other)?;
    let branch = branches
        .iter()
        .find(|branch| branch.name == name)
        .ok_or_else(|| PrError::NotFound(format!("There is no branch {name} in this repository.")))?;
    if !branch.is_remote {
        return Ok(name.to_string());
    }
    let remotes = arnold_git::remotes(root).await.map_err(other)?;
    remotes
        .iter()
        .filter_map(|remote| name.strip_prefix(&format!("{}/", remote.name)))
        // The longest remote name that fits: `team/x` before `team`.
        .min_by_key(|rest| rest.len())
        .map(str::to_string)
        .ok_or_else(|| PrError::Other(format!("Could not tell which remote {name} belongs to.")))
}

async fn plan_new(
    root: &Path,
    repo: &AzureRepo,
    token: &str,
    head: &str,
    base: &str,
    commit: &str,
) -> Result<NewPullRequestPlan, PrError> {
    if !arnold_git::has_commits(root, &[commit]).await {
        return Err(PrError::Other(format!("Commit {commit} is not in this repository.")));
    }
    let source_branch = host_branch(root, head).await?;
    let target_branch = host_branch(root, base).await?;
    if source_branch == target_branch {
        return Err(PrError::Other(format!("{head} and {base} are the same branch on Azure DevOps.")));
    }
    let (server_commit, target_commit, existing) = {
        let (repo, token) = (repo.clone(), token.to_string());
        let (source, target) = (source_branch.clone(), target_branch.clone());
        blocking(move || {
            let server = azure::branch_commit(&repo, &token, &source)?;
            let target_commit = azure::branch_commit(&repo, &token, &target)?;
            let existing = azure::active_pull_requests_between(&repo, &token, &source, &target)?;
            Ok::<_, PrError>((server, target_commit, existing.into_iter().next()))
        })
        .await??
    };
    let target_commit = target_commit
        .ok_or_else(|| PrError::NotFound(format!("Azure DevOps has no branch {target_branch} to merge into.")))?;
    // What the pull request will have: compared with the host's target when
    // that commit is here, else with the local base branch.
    let compared = if arnold_git::has_commits(root, &[&target_commit]).await { target_commit.as_str() } else { base };
    let commits = arnold_git::count_commits(root, Some(compared), commit).await.map_err(other)?;

    let (push, push_commits) = match &server_commit {
        None => (PushPlan::Create, commits),
        Some(server) if server == commit => (PushPlan::None, 0),
        Some(server) if arnold_git::is_ancestor(root, server, commit).await => {
            (PushPlan::Update, arnold_git::count_commits(root, Some(server), commit).await.map_err(other)?)
        }
        Some(_) => (PushPlan::Conflict, 0),
    };
    Ok(NewPullRequestPlan {
        source_branch,
        target_branch,
        server_commit,
        push,
        commits,
        push_commits,
        existing,
    })
}

/// What creating a pull request from `head` (as of `commit`, the one
/// reviewed) into `base` would do: whether the branch has to be pushed
/// first, and whether one is open already.
#[tauri::command]
pub async fn plan_pull_request(
    root: String,
    remote: String,
    head: String,
    base: String,
    commit: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<NewPullRequestPlan, PrError> {
    let (root, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let plan = plan_new(&root, &repo, &token, &head, &base, &commit).await?;
    Ok(NewPullRequestPlan {
        existing: match plan.existing {
            Some(existing) => Some(prs.mark_you(&repo, &token, existing).await),
            None => None,
        },
        ..plan
    })
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateRequest {
    head: String,
    base: String,
    /// The commit reviewed: what is pushed, whatever `head` points at by now.
    commit: String,
    title: String,
    description: String,
    draft: bool,
    /// A work item to link the pull request to.
    #[serde(default)]
    work_item: Option<u64>,
}

/// Opens a pull request from `head` into `base` on `remote`'s Azure DevOps,
/// pushing the reviewed commit to the host's branch first when it is not
/// there yet. Never forced: a host branch with commits that were not
/// reviewed stops it.
#[tauri::command]
pub async fn create_pull_request(
    root: String,
    remote: String,
    request: CreateRequest,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<PullRequest, PrError> {
    let (root, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    let title = request.title.trim().to_string();
    if title.is_empty() {
        return Err(PrError::Other("The pull request needs a title.".into()));
    }
    let plan = plan_new(&root, &repo, &token, &request.head, &request.base, &request.commit).await?;
    if let Some(existing) = &plan.existing {
        return Err(PrError::Other(format!(
            "Pull request !{} from {} into {} is open already.",
            existing.id, plan.source_branch, plan.target_branch
        )));
    }
    if plan.commits == 0 {
        return Err(PrError::Other(format!("{} has nothing that {} lacks.", request.head, plan.target_branch)));
    }
    match plan.push {
        PushPlan::None => {}
        PushPlan::Conflict => return Err(PrError::Other(conflict_text(&plan))),
        PushPlan::Create | PushPlan::Update => {
            let header = format!("Authorization: {}", azure::authorization(&token));
            arnold_git::push_commit(&root, &repo.web_url(), &request.commit, &plan.source_branch, Some(&header))
                .await
                .map_err(|e| match e {
                    arnold_git::GitError::Failed { stderr, .. } if stderr.contains("rejected") => PrError::Other(conflict_text(&plan)),
                    e => PrError::Other(format!("Could not push {} to Azure DevOps: {e}", plan.source_branch)),
                })?;
            // As `git push origin` would; the push itself does not depend on it.
            if let Err(e) = arnold_git::set_remote_tracking(&root, &remote, &plan.source_branch, &request.commit).await {
                crate::commands::dev_trace(format!("updating {remote}/{}: {e}", plan.source_branch));
            }
        }
    }
    let new = NewPullRequest {
        source_branch: plan.source_branch,
        target_branch: plan.target_branch,
        title,
        description: request.description,
        draft: request.draft,
        work_items: request.work_item.into_iter().collect(),
    };
    let pull_request = {
        let (repo, token) = (repo.clone(), token.clone());
        blocking(move || azure::create_pull_request(&repo, &token, &new)).await??
    };
    Ok(prs.mark_you(&repo, &token, pull_request).await)
}

/// Work item `id` of `remote`'s organization, to show what a new pull
/// request will be linked to.
#[tauri::command]
pub async fn work_item(
    root: String,
    remote: String,
    id: u64,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<WorkItemLookup, PrError> {
    let (_, repo) = resolve(&app, &root, &remote).await?;
    let token = prs.required_token(&repo).await?;
    blocking(move || azure::work_item(&repo, &token, id)).await?
}

/// The required branch policies of `branch` on `remote` (names), checked
/// before committing on it: a branch with any takes changes only through
/// pull requests. Empty when `remote` is not on Azure DevOps or no token is
/// set up, as nothing can be told then.
#[tauri::command]
pub async fn required_branch_policies(
    checkout: String,
    remote: String,
    branch: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<Vec<String>, PrError> {
    let checkout = app.checked_root(&checkout).map_err(PrError::Other)?;
    // Any remote by its name, also a second one for a repository `azure_remotes` lists once.
    let remotes = arnold_git::remotes(&checkout).await.map_err(other)?;
    let Some(repo) = remotes
        .iter()
        .find(|known| known.name == remote)
        .and_then(|known| azure::parse_remote_url(&known.url))
    else {
        return Ok(Vec::new());
    };
    let Some((token, _)) = prs.token(&repo).await? else {
        return Ok(Vec::new());
    };
    blocking(move || azure::required_branch_policies(&repo, &token, &branch)).await?
}

/// Who created each branch on the Azure DevOps remotes that have a token
/// set up, by remote-tracking name (`origin/feature/x`). Remotes that cannot
/// be asked are left out: the branch list shows who committed last instead.
#[tauri::command]
pub async fn branch_creators(
    root: String,
    app: State<'_, AppState>,
    prs: State<'_, PrState>,
) -> Result<HashMap<String, String>, PrError> {
    let root = app.checked_root(&root).map_err(PrError::Other)?;
    let mut creators = HashMap::new();
    for (remote, repo) in azure_remotes(&root).await? {
        let Ok(Some((token, _))) = prs.token(&repo).await else { continue };
        match blocking(move || azure::branch_creators(&repo, &token)).await? {
            Ok(list) => creators.extend(list.into_iter().map(|(branch, creator)| (format!("{remote}/{branch}"), creator))),
            Err(error) => eprintln!("[arnold] branch creators of {remote}: {error}"),
        }
    }
    Ok(creators)
}

fn conflict_text(plan: &NewPullRequestPlan) -> String {
    format!(
        "{} on Azure DevOps has commits that are not in what you reviewed. Fetch, review the branch as it is there, then create the pull request.",
        plan.source_branch
    )
}
