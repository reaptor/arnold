//! Everything the webview can ask the backend to do.

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use arnold_git::{Comparison, RepoInfo};
use serde::Serialize;
use tauri::{AppHandle, State};

use crate::fsio::{self, Decoded};
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

    /// Lets file and language-server commands use `path` (a prepared copy of
    /// the reviewed commit).
    pub(crate) fn allow_root(&self, path: &Path) {
        if let Ok(canonical) = path.canonicalize() {
            self.roots.lock().unwrap().insert(canonical);
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
    // Without the `\\?\` prefix `std` canonicalize adds on Windows.
    let path = dunce::canonicalize(argument).ok()?;
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
/// repository's branches, so new commits show up.
#[tauri::command]
pub async fn compare_branches(
    root: String,
    base: String,
    head: String,
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<Comparison, String> {
    let root = state.checked_root(&root)?;
    let comparison = arnold_git::compare(&root, &base, &head)
        .await
        .map_err(text_error)?;

    let git_dir = arnold_git::common_dir(&root).await.map_err(text_error)?;
    let handle = watcher::watch(app, git_dir).ok();
    *state.watch.lock().unwrap() = handle;
    Ok(comparison)
}

/// Every file of `commit`, for the file explorer.
#[tauri::command]
pub async fn list_files(
    root: String,
    commit: String,
    state: State<'_, AppState>,
) -> Result<Vec<String>, String> {
    let root = state.checked_root(&root)?;
    arnold_git::list_files(&root, &commit)
        .await
        .map_err(text_error)
}

#[derive(Debug, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct FilePair {
    /// Text at the merge-base; `None` when the file did not exist there.
    base: Option<String>,
    /// Text being reviewed; `None` when the file was deleted.
    working: Option<String>,
    binary: bool,
    too_large: bool,
}

#[tauri::command]
pub async fn read_file_pair(
    root: String,
    merge_base: String,
    commit: String,
    path: String,
    old_path: Option<String>,
    state: State<'_, AppState>,
) -> Result<FilePair, String> {
    let root = state.checked_root(&root)?;
    let base_path = old_path.as_deref().unwrap_or(&path);
    let base_bytes = arnold_git::read_blob(&root, &merge_base, base_path)
        .await
        .map_err(text_error)?;

    let working_bytes = arnold_git::read_blob(&root, &commit, &path)
        .await
        .map_err(text_error)?;

    let mut pair = FilePair::default();
    for (bytes, is_working) in [(working_bytes, true), (base_bytes, false)] {
        let Some(bytes) = bytes else { continue };
        match fsio::decode(&bytes) {
            Decoded::Text(text) if is_working => pair.working = Some(text),
            Decoded::Text(text) => pair.base = Some(text),
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
