//! Everything the webview can ask the backend to do.

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use arnold_git::{CheckoutStatus, Comparison, RepoInfo};
use serde::Serialize;
use tauri::{AppHandle, Manager, State};

use crate::fsio::{self, Decoded, Eol};
use crate::watcher::{self, WatchHandle};

#[derive(Default)]
pub struct AppState {
    /// Checkouts of the opened repository. File commands refuse any other root.
    roots: Mutex<HashSet<PathBuf>>,
    watch: Mutex<Option<WatchHandle>>,
}

impl AppState {
    pub(crate) fn checked_root(&self, root: &str) -> Result<PathBuf, String> {
        let canonical = Path::new(root)
            .canonicalize()
            .map_err(|e| format!("{root}: {e}"))?;
        if self.roots.lock().unwrap().contains(&canonical) {
            Ok(canonical)
        } else {
            Err(format!("{root} is not part of the opened repository"))
        }
    }
}

fn text_error(error: impl std::fmt::Display) -> String {
    error.to_string()
}

/// Development aid: the webview reports each backend call and its outcome,
/// so a `tauri dev` run shows in the terminal what happened.
#[tauri::command]
pub fn dev_trace(message: String) {
    if cfg!(debug_assertions) {
        eprintln!("[arnold] {message}");
    }
}

/// Folder given on the command line (`arnold path/to/repo`), if any. The `arnold` command the
/// installers set up passes `.` when it is run without a folder.
#[tauri::command]
pub fn startup_path() -> Option<String> {
    let argument = std::env::args().nth(1)?;
    // PowerShell completes a folder with spaces as `'.\my repo\'`; on its way through
    // `arnold.cmd` the backslash turns the closing quote into part of the argument.
    let argument = argument.trim_end_matches('"');
    let path = Path::new(argument).canonicalize().ok()?;
    path.is_dir().then(|| path.to_string_lossy().into_owned())
}

#[tauri::command]
pub async fn open_repo(path: String, state: State<'_, AppState>) -> Result<RepoInfo, String> {
    let info = arnold_git::open(Path::new(&path)).await.map_err(text_error)?;
    let mut roots = HashSet::new();
    for checkout in std::iter::once(&info.root).chain(&info.worktrees) {
        if let Ok(canonical) = checkout.canonicalize() {
            roots.insert(canonical);
        }
    }
    *state.roots.lock().unwrap() = roots;
    *state.watch.lock().unwrap() = None;
    Ok(info)
}

/// Compares `head` against its merge-base with `base` and starts watching the
/// checkout that holds `head`. A branch not checked out anywhere gets a linked
/// worktree in the app's data folder first, so it can be edited without
/// switching the branch of the user's own checkouts.
#[tauri::command]
pub async fn compare_branches(
    root: String,
    base: String,
    head: String,
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<Comparison, String> {
    let root = state.checked_root(&root)?;
    let store = app
        .path()
        .app_data_dir()
        .map_err(text_error)?
        .join("worktrees");
    let worktree = arnold_git::ensure_checkout(&root, &head, &store)
        .await
        .map_err(text_error)?;
    let comparison = arnold_git::compare_in(&root, &base, &head, Some(worktree))
        .await
        .map_err(text_error)?;

    // A worktree created after the repository was opened is still part of it.
    if let Some(worktree) = &comparison.worktree_path {
        if let Ok(canonical) = worktree.canonicalize() {
            state.roots.lock().unwrap().insert(canonical);
        }
    }
    let watched = comparison.worktree_path.clone().unwrap_or(root);
    let handle = watcher::watch(app, watched).ok();
    *state.watch.lock().unwrap() = handle;
    Ok(comparison)
}

/// Every file of `head` (in its checkout when it has one), for the file explorer.
#[tauri::command]
pub async fn list_files(
    root: String,
    head: String,
    worktree: Option<String>,
    state: State<'_, AppState>,
) -> Result<Vec<String>, String> {
    let root = state.checked_root(&root)?;
    let worktree = worktree.map(|worktree| state.checked_root(&worktree)).transpose()?;
    arnold_git::list_files(&root, &head, worktree.as_deref())
        .await
        .map_err(text_error)
}

/// Uncommitted changes of the checkout under review and where a push goes.
#[tauri::command]
pub async fn checkout_status(
    worktree: String,
    head: String,
    state: State<'_, AppState>,
) -> Result<CheckoutStatus, String> {
    let worktree = state.checked_root(&worktree)?;
    arnold_git::checkout_status(&worktree, &head)
        .await
        .map_err(text_error)
}

/// Commits `paths` of the checkout under review (all changes when `None`).
#[tauri::command]
pub async fn commit_changes(
    worktree: String,
    message: String,
    paths: Option<Vec<String>>,
    state: State<'_, AppState>,
) -> Result<String, String> {
    let worktree = state.checked_root(&worktree)?;
    arnold_git::commit(&worktree, &message, paths.as_deref())
        .await
        .map_err(text_error)
}

/// Pushes the checkout under review to its remote branch.
#[tauri::command]
pub async fn push_changes(
    worktree: String,
    head: String,
    state: State<'_, AppState>,
) -> Result<(), String> {
    let worktree = state.checked_root(&worktree)?;
    arnold_git::push(&worktree, &head).await.map_err(text_error)
}

#[derive(Debug, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct FilePair {
    /// Text at the merge-base; `None` when the file did not exist there.
    base: Option<String>,
    /// Text being reviewed; `None` when the file was deleted.
    working: Option<String>,
    eol: Eol,
    bom: bool,
    binary: bool,
    too_large: bool,
}

#[tauri::command]
pub async fn read_file_pair(
    root: String,
    merge_base: String,
    head: String,
    worktree: Option<String>,
    path: String,
    old_path: Option<String>,
    state: State<'_, AppState>,
) -> Result<FilePair, String> {
    let root = state.checked_root(&root)?;
    let base_path = old_path.as_deref().unwrap_or(&path);
    let base_bytes = arnold_git::read_blob(&root, &merge_base, base_path)
        .await
        .map_err(text_error)?;

    let working_bytes = match &worktree {
        Some(worktree) => {
            let worktree = state.checked_root(worktree)?;
            match tokio::fs::read(fsio::resolve(&worktree, &path)?).await {
                Ok(bytes) => Some(bytes),
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
                Err(error) => return Err(format!("{path}: {error}")),
            }
        }
        None => arnold_git::read_blob(&root, &head, &path)
            .await
            .map_err(text_error)?,
    };

    let mut pair = FilePair::default();
    let mut format_known = false;
    // The working file decides line endings and BOM; the base only when the file is gone.
    for (bytes, is_working) in [(working_bytes, true), (base_bytes, false)] {
        let Some(bytes) = bytes else { continue };
        match fsio::decode(&bytes) {
            Decoded::Text { text, eol, bom } => {
                if !format_known {
                    (pair.eol, pair.bom, format_known) = (eol, bom, true);
                }
                if is_working {
                    pair.working = Some(text);
                } else {
                    pair.base = Some(text);
                }
            }
            Decoded::Binary => pair.binary = true,
            Decoded::TooLarge => pair.too_large = true,
        }
    }
    if pair.binary || pair.too_large {
        pair.base = None;
        pair.working = None;
    }
    Ok(pair)
}

#[tauri::command]
pub async fn write_file(
    worktree: String,
    path: String,
    content: String,
    eol: Eol,
    bom: bool,
    state: State<'_, AppState>,
) -> Result<(), String> {
    let worktree = state.checked_root(&worktree)?;
    fsio::write_text(&worktree, &path, &content, eol, bom).await
}

/// Puts one file back to how it is at the merge-base: restores its content,
/// or removes it if it did not exist there. Only the working tree is touched.
#[tauri::command]
pub async fn reset_file(
    root: String,
    merge_base: String,
    worktree: String,
    path: String,
    old_path: Option<String>,
    state: State<'_, AppState>,
) -> Result<(), String> {
    let root = state.checked_root(&root)?;
    let worktree = state.checked_root(&worktree)?;
    let base_path = old_path.as_deref().unwrap_or(&path);
    let base = arnold_git::read_blob(&root, &merge_base, base_path)
        .await
        .map_err(text_error)?;

    fsio::reset_to_base(&worktree, &path, base_path, base).await
}
