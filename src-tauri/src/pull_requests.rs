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
use arnold_pr::{NewThreadPlace, PrError, PullRequest, Thread};
use serde::Serialize;
use tauri::State;

use crate::commands::AppState;

#[derive(Default)]
pub struct PrState {
    /// Tokens read so far, by `AzureRepo::token_key`, so the credential store
    /// (which may ask the user for access) is read once per run.
    tokens: Mutex<HashMap<String, Option<(String, TokenSource)>>>,
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
    blocking(move || azure::active_pull_requests(&repo, &token)).await?
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
    prs.tokens.lock().unwrap().remove(&key);
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
    Ok(pull_request)
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
    blocking(move || azure::threads(&repo, &token, id)).await?
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
