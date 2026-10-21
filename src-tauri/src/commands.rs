//! Everything the webview can ask the backend to do.

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use arnold_git::{Branch, BranchCheckout, CheckedOut, CommitRange, Comparison, Placement, RepoInfo, Worktree};
use serde::Serialize;
use tauri::{AppHandle, State};

use crate::fsio::{self, Decoded};
use crate::watcher::{self, WatchHandle};

#[derive(Default)]
pub struct AppState {
    /// Checkouts of the opened repository. File commands refuse any other root.
    roots: Mutex<HashSet<PathBuf>>,
    watch: Mutex<Option<WatchHandle>>,
    /// The checkout whose uncommitted changes are listed, with its watch.
    pub(crate) checkout_watch: Mutex<Option<(PathBuf, WatchHandle)>>,
}

impl AppState {
    /// Lets file commands use the checkouts the branches are checked out in,
    /// which may have been added since the repository was opened.
    pub(crate) fn allow_checkouts(&self, branches: &[Branch]) {
        for path in branches.iter().filter_map(|branch| branch.worktree_path.as_deref()) {
            self.allow_root(path);
        }
    }

    pub(crate) fn checked_root(&self, root: &str) -> Result<PathBuf, String> {
        let canonical = canonical(Path::new(root)).map_err(|e| format!("{root}: {e}"))?;
        if self.roots.lock().unwrap().contains(&canonical) {
            Ok(canonical)
        } else {
            Err(format!("{root} is not part of the opened repository"))
        }
    }

    /// Lets file and language-server commands use `path` (a prepared copy of
    /// the reviewed commit).
    pub(crate) fn allow_root(&self, path: &Path) {
        if let Ok(canonical) = canonical(path) {
            self.roots.lock().unwrap().insert(canonical);
        }
    }
}

/// Resolved path of a checkout. Not `std` canonicalize: on Windows that gives
/// `\\?\C:\…`, and every command works from the checked root, so git and
/// dotnet would get such paths too (git cannot create lock files there).
fn canonical(path: &Path) -> std::io::Result<PathBuf> {
    dunce::canonicalize(path)
}

pub(crate) fn text_error(error: impl std::fmt::Display) -> String {
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
pub async fn open_repo(path: String, app: AppHandle, state: State<'_, AppState>) -> Result<RepoInfo, String> {
    let info = arnold_git::open(Path::new(&path)).await.map_err(text_error)?;
    let mut roots = HashSet::new();
    for checkout in std::iter::once(&info.root).chain(&info.worktrees) {
        if let Ok(canonical) = canonical(checkout) {
            roots.insert(canonical);
        }
    }
    *state.roots.lock().unwrap() = roots;
    *state.checkout_watch.lock().unwrap() = None;
    // Branches, checkouts and remotes are followed in every mode, from the start.
    *state.watch.lock().unwrap() = None;
    let git_dir = arnold_git::common_dir(&info.root).await.map_err(text_error)?;
    // Events arrive with resolved paths (`/private/var` for `/var` on macOS).
    let git_dir = git_dir.canonicalize().unwrap_or(git_dir);
    match watcher::watch(app, git_dir) {
        Ok(handle) => *state.watch.lock().unwrap() = Some(handle),
        Err(error) => eprintln!("[arnold] cannot watch {}: {error}", info.root.display()),
    }
    Ok(info)
}

/// Compares `head` against its merge-base with `base` (or shows a run of its
/// commits). When `head` is checked out (in the opened folder or a linked
/// worktree) all of its changes are read from that checkout's files, which
/// are then watched too, so edits made there or in Arnold show up.
/// `checkout`: where `head` (a commit) is checked out otherwise, such as a
/// pull request's worktree.
#[tauri::command]
pub async fn compare_branches(
    root: String,
    base: String,
    head: String,
    range: Option<CommitRange>,
    checkout: Option<String>,
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<Comparison, String> {
    let root = state.checked_root(&root)?;
    let branches = arnold_git::list_branches(&root).await.map_err(text_error)?;
    state.allow_checkouts(&branches);
    let checkout = match checkout {
        Some(checkout) => Some(state.checked_root(&checkout)?),
        None => branches
            .iter()
            .find(|branch| !branch.is_remote && branch.name == head)
            .and_then(|branch| branch.worktree_path.as_deref())
            .and_then(|path| state.checked_root(&path.to_string_lossy()).ok()),
    };
    let comparison = arnold_git::compare(&root, &base, &head, range.as_ref(), checkout.as_deref())
        .await
        .map_err(text_error)?;

    match &comparison.checkout {
        Some(checkout) => crate::working::watch_checkout(app, &state, checkout.clone()).await?,
        None => *state.checkout_watch.lock().unwrap() = None,
    }
    Ok(comparison)
}

/// Where pull request `id` is checked out to be edited in (the opened folder
/// or `.arnold/worktrees/pr-<id>`), moved to `commit` when it has no edits
/// (or with `discard` placement, after throwing them away). With `create`
/// it is checked out as `placement` says when it is nowhere; without,
/// `None` then.
#[tauri::command]
pub async fn pull_request_checkout(
    root: String,
    id: u64,
    commit: String,
    create: bool,
    placement: Placement,
    state: State<'_, AppState>,
) -> Result<Option<CheckedOut<Worktree>>, String> {
    let root = state.checked_root(&root)?;
    if commit.is_empty() || !commit.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(format!("not a commit id: {commit}"));
    }
    let checkout = arnold_git::pull_request_checkout(&root, id, &commit, create, placement).await.map_err(text_error)?;
    if let Some(CheckedOut::Done(worktree)) = &checkout {
        state.allow_root(&worktree.path);
    }
    Ok(checkout)
}

/// Where `branch` is checked out, checking it out when it is checked out
/// nowhere, so it can be edited and the language server runs on its files:
/// in the opened folder (refused while that has uncommitted changes, unless
/// `placement` throws them away), or in a worktree of its own.
#[tauri::command]
pub async fn branch_checkout(
    root: String,
    branch: String,
    placement: Placement,
    state: State<'_, AppState>,
) -> Result<CheckedOut<BranchCheckout>, String> {
    let root = state.checked_root(&root)?;
    let checkout = arnold_git::branch_checkout(&root, &branch, placement).await.map_err(text_error)?;
    if let CheckedOut::Done(done) = &checkout {
        state.allow_root(&done.path);
    }
    Ok(checkout)
}

/// The branches again, after the watcher saw them change.
#[tauri::command]
pub async fn list_branches(root: String, state: State<'_, AppState>) -> Result<Vec<Branch>, String> {
    let root = state.checked_root(&root)?;
    let branches = arnold_git::list_branches(&root).await.map_err(text_error)?;
    state.allow_checkouts(&branches);
    Ok(branches)
}

/// Every file of `commit`, for the file explorer; with `checkout`, the files
/// on disk there instead (the checkout the comparison reads).
#[tauri::command]
pub async fn list_files(
    root: String,
    commit: String,
    checkout: Option<String>,
    state: State<'_, AppState>,
) -> Result<Vec<String>, String> {
    if let Some(checkout) = checkout {
        let checkout = state.checked_root(&checkout)?;
        return arnold_git::worktree_files(&checkout).await.map_err(text_error);
    }
    let root = state.checked_root(&root)?;
    arnold_git::list_files(&root, &commit)
        .await
        .map_err(text_error)
}

/// Texts of the solution and project files at `paths`, for the explorer's
/// Solution view: from the files on disk in `checkout` when given, else from
/// `commit`. `None` for a path that is missing or not text.
#[tauri::command]
pub async fn read_texts(
    root: String,
    commit: String,
    checkout: Option<String>,
    paths: Vec<String>,
    state: State<'_, AppState>,
) -> Result<Vec<Option<String>>, String> {
    let checkout = checkout.map(|checkout| state.checked_root(&checkout)).transpose()?;
    let root = state.checked_root(&root)?;
    let mut texts = Vec::with_capacity(paths.len());
    for path in &paths {
        let bytes = match &checkout {
            Some(checkout) => arnold_git::read_worktree(checkout, path).await,
            None => arnold_git::read_blob(&root, &commit, path).await,
        }
        .map_err(text_error)?;
        texts.push(match bytes.as_deref().map(fsio::decode) {
            Some(Decoded::Text(text)) => Some(text),
            _ => None,
        });
    }
    Ok(texts)
}

#[derive(Debug, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FilePair {
    /// Text at the merge-base; `None` when the file did not exist there.
    base: Option<String>,
    /// Text being reviewed; `None` when the file was deleted.
    working: Option<String>,
    /// When the working side is read from a checkout: the text the branch's
    /// last commit has, which edits not committed yet differ from. `None`
    /// when that commit lacks the file (or for other diffs).
    committed: Option<String>,
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
    checkout: Option<String>,
    state: State<'_, AppState>,
) -> Result<FilePair, String> {
    let root = state.checked_root(&root)?;
    let base_path = old_path.as_deref().unwrap_or(&path);
    let base_bytes = arnold_git::read_blob(&root, &merge_base, base_path)
        .await
        .map_err(text_error)?;

    let Some(checkout) = checkout else {
        let working_bytes = arnold_git::read_blob(&root, &commit, &path).await.map_err(text_error)?;
        return Ok(file_pair(base_bytes, working_bytes));
    };
    let checkout = state.checked_root(&checkout)?;
    let (working_bytes, committed_bytes) = tokio::try_join!(
        arnold_git::read_worktree(&checkout, &path),
        arnold_git::read_blob(&root, &commit, &path),
    )
    .map_err(text_error)?;
    let mut pair = file_pair(base_bytes, working_bytes);
    if pair.working.is_some() {
        if let Some(Decoded::Text(text)) = committed_bytes.as_deref().map(fsio::decode) {
            pair.committed = Some(text);
        }
    }
    Ok(pair)
}

/// The two sides of a diff, decoded for the editor.
pub(crate) fn file_pair(base_bytes: Option<Vec<u8>>, working_bytes: Option<Vec<u8>>) -> FilePair {
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
    pair
}
