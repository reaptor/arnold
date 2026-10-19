//! Committing the uncommitted changes of a checkout: listing them, staging
//! and unstaging (whole files or some lines), putting files back, saving
//! edits and committing. Every command acts on one checkout of the opened
//! repository, never anything outside it.

use std::path::PathBuf;

use arnold_git::{Ignored, NewCommit, WorkingStatus};
use serde::Deserialize;
use tauri::{AppHandle, State};

use crate::commands::{file_pair, text_error, AppState, FilePair};
use crate::pull_requests::PrState;
use crate::watcher;

/// Which diff of a file with uncommitted changes.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum WorkingSide {
    /// The working tree against the index.
    Unstaged,
    /// The index against HEAD.
    Staged,
    /// The working tree against HEAD, whatever is staged: a file of the explorer.
    Committed,
}

/// What `checkout` has not committed, staged and not. Also makes sure its
/// files and index are watched, so a `checkout-changed` event follows any change.
#[tauri::command]
pub async fn working_status(checkout: String, app: AppHandle, state: State<'_, AppState>) -> Result<WorkingStatus, String> {
    let checkout = state.checked_root(&checkout)?;
    let status = arnold_git::working_status(&checkout).await.map_err(text_error)?;
    watch_checkout(app, &state, checkout).await?;
    Ok(status)
}

/// How many files `checkout` has uncommitted changes in, staged or not
/// (a file in both lists counts once). Unlike `working_status` it leaves
/// the watched checkout as it is: for showing the count outside Commit mode.
#[tauri::command]
pub async fn uncommitted_count(checkout: String, state: State<'_, AppState>) -> Result<usize, String> {
    let checkout = state.checked_root(&checkout)?;
    let status = arnold_git::working_status(&checkout).await.map_err(text_error)?;
    let paths: std::collections::HashSet<&str> =
        status.unstaged.iter().chain(&status.staged).map(|file| file.path.as_str()).collect();
    Ok(paths.len())
}

/// Makes sure `checkout`'s files and index are watched (`checkout-changed`),
/// in place of any other checkout. `checkout` has been through `checked_root`.
pub(crate) async fn watch_checkout(app: AppHandle, state: &AppState, checkout: PathBuf) -> Result<(), String> {
    let watched = matches!(&*state.checkout_watch.lock().unwrap(), Some((path, _)) if *path == checkout);
    if !watched {
        let git_dir = arnold_git::git_dir(&checkout).await.map_err(text_error)?;
        // Events arrive with resolved paths (`/private/var` for `/var` on macOS).
        let git_dir = git_dir.canonicalize().unwrap_or(git_dir);
        match watcher::watch_checkout(app, checkout.clone(), git_dir) {
            Ok(handle) => *state.checkout_watch.lock().unwrap() = Some((checkout, handle)),
            Err(error) => eprintln!("[arnold] cannot watch {}: {error}", checkout.display()),
        }
    }
    Ok(())
}

/// Stops watching the checkout's files: its changes are no longer listed.
#[tauri::command]
pub fn stop_watching_checkout(state: State<'_, AppState>) {
    *state.checkout_watch.lock().unwrap() = None;
}

/// One side of the uncommitted changes of `path`: unstaged is index →
/// working tree, staged is HEAD → index, committed is HEAD → working tree.
/// `old_path` is its path in HEAD when a staged rename.
#[tauri::command]
pub async fn read_working_pair(
    checkout: String,
    path: String,
    old_path: Option<String>,
    side: WorkingSide,
    state: State<'_, AppState>,
) -> Result<FilePair, String> {
    let checkout = state.checked_root(&checkout)?;
    let (base, working) = match side {
        WorkingSide::Unstaged => tokio::try_join!(
            arnold_git::read_index(&checkout, &path),
            arnold_git::read_worktree(&checkout, &path),
        ),
        WorkingSide::Staged => tokio::try_join!(
            arnold_git::read_blob(&checkout, "HEAD", old_path.as_deref().unwrap_or(&path)),
            arnold_git::read_index(&checkout, &path),
        ),
        WorkingSide::Committed => tokio::try_join!(
            arnold_git::read_blob(&checkout, "HEAD", old_path.as_deref().unwrap_or(&path)),
            arnold_git::read_worktree(&checkout, &path),
        ),
    }
    .map_err(text_error)?;
    Ok(file_pair(base, working))
}

/// Every file of the working tree that git sees, for the file explorer.
#[tauri::command]
pub async fn worktree_files(checkout: String, state: State<'_, AppState>) -> Result<Vec<String>, String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::worktree_files(&checkout).await.map_err(text_error)
}

#[tauri::command]
pub async fn stage_files(checkout: String, paths: Vec<String>, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::stage(&checkout, &paths).await.map_err(text_error)
}

#[tauri::command]
pub async fn unstage_files(checkout: String, paths: Vec<String>, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::unstage(&checkout, &paths).await.map_err(text_error)
}

/// Throws away unstaged changes: tracked files back to the index, untracked ones deleted.
#[tauri::command]
pub async fn discard_files(checkout: String, paths: Vec<String>, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::discard(&checkout, &paths).await.map_err(text_error)
}

/// Deletes files from the working tree, leaving the index as it is.
#[tauri::command]
pub async fn delete_files(checkout: String, paths: Vec<String>, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::delete_files(&checkout, &paths).await.map_err(text_error)
}

/// Puts files back to HEAD in the index and the working tree.
#[tauri::command]
pub async fn reset_files_to_head(checkout: String, paths: Vec<String>, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::reset_to_head(&checkout, &paths).await.map_err(text_error)
}

/// Undoes the uncommitted changes to `paths` (Branches, editing a checked-out
/// branch): back to the last commit, and a file no commit has is deleted.
#[tauri::command]
pub async fn undo_file_edits(checkout: String, paths: Vec<String>, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::undo_edits(&checkout, &paths).await.map_err(text_error)
}

/// Ignores `path` (a file, or a folder with `folder`) in the checkout's root `.gitignore`.
#[tauri::command]
pub async fn ignore_path(checkout: String, path: String, folder: bool, state: State<'_, AppState>) -> Result<Ignored, String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::ignore(&checkout, &path, folder).await.map_err(text_error)
}

/// Whether git tracks `path` (a file, or any file in a folder) in the checkout.
#[tauri::command]
pub async fn path_tracked(checkout: String, path: String, state: State<'_, AppState>) -> Result<bool, String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::is_tracked(&checkout, &path).await.map_err(text_error)
}

/// Stops tracking `path` (a file, or a folder's files), keeping it on disk; how many files that was.
#[tauri::command]
pub async fn untrack_path(checkout: String, path: String, state: State<'_, AppState>) -> Result<usize, String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::untrack(&checkout, &path).await.map_err(text_error)
}

/// Gives `path` this text in the index: some of its lines staged or unstaged.
#[tauri::command]
pub async fn set_index_text(checkout: String, path: String, text: String, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::set_index_text(&checkout, &path, &text).await.map_err(text_error)
}

/// The text of `path` in the working tree, for editing a file no editor has
/// open (such as the other files a rename changes). `None` when it is not
/// there; an error when it is not text.
#[tauri::command]
pub async fn read_worktree_text(checkout: String, path: String, state: State<'_, AppState>) -> Result<Option<String>, String> {
    let checkout = state.checked_root(&checkout)?;
    let Some(bytes) = arnold_git::read_worktree(&checkout, &path).await.map_err(text_error)? else {
        return Ok(None);
    };
    match crate::fsio::decode(&bytes) {
        crate::fsio::Decoded::Text(text) => Ok(Some(text)),
        _ => Err(format!("{path} is not a text file")),
    }
}

/// Saves an edit of `path` in the working tree, keeping its line endings.
#[tauri::command]
pub async fn save_worktree_file(checkout: String, path: String, text: String, state: State<'_, AppState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::write_worktree(&checkout, &path, &text).await.map_err(text_error)
}

/// Commits what is staged.
#[tauri::command]
pub async fn commit_staged(checkout: String, message: String, state: State<'_, AppState>) -> Result<NewCommit, String> {
    let checkout = state.checked_root(&checkout)?;
    arnold_git::commit(&checkout, &message).await.map_err(text_error)
}


/// Pushes the branch checked out in `checkout` to its upstream, or to
/// `origin` as a new branch that becomes its upstream. Never forced.
#[tauri::command]
pub async fn push_checkout_branch(checkout: String, state: State<'_, AppState>, prs: State<'_, PrState>) -> Result<(), String> {
    let checkout = state.checked_root(&checkout)?;
    let status = arnold_git::working_status(&checkout).await.map_err(text_error)?;
    let branch = status.branch.ok_or("The checkout is not on a branch, so there is no branch to push.")?;
    let sync = status.sync.ok_or("The repository has no remote to push to.")?;
    let header = prs.push_header(&checkout, &sync.remote).await;
    arnold_git::push_branch(&checkout, &branch, &sync, header.as_deref()).await.map_err(text_error)
}
