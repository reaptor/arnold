//! Watches the repository's branches and checkouts and tells the webview when
//! one moves, so the diff follows an agent that keeps committing, and the
//! language server leaves the opened folder when another branch is checked
//! out there. Reviewing only ever shows commits; files on disk are watched
//! only for the checkout whose uncommitted changes are listed (`watch_checkout`).

use std::any::Any;
use std::path::{Path, PathBuf};
use std::time::Duration;

use notify::RecursiveMode;
use notify_debouncer_full::{new_debouncer, DebounceEventResult};
use tauri::{AppHandle, Emitter};

pub const EVENT: &str = "refs-changed";
const DEBOUNCE: Duration = Duration::from_millis(400);

/// Keeps the watch alive; dropping it stops watching.
pub struct WatchHandle(#[allow(dead_code)] Box<dyn Any + Send>);

/// Watches `git_dir`, the `.git` folder shared by every checkout.
pub fn watch(app: AppHandle, git_dir: PathBuf) -> Result<WatchHandle, String> {
    let root = git_dir.clone();
    let mut debouncer = new_debouncer(DEBOUNCE, None, move |result: DebounceEventResult| {
        let Ok(events) = result else { return };
        let moved = events
            .iter()
            .filter(|event| !event.kind.is_access())
            .flat_map(|event| &event.paths)
            .any(|path| is_ref(&root, path));
        if moved {
            let _ = app.emit(EVENT, ());
        }
    })
    .map_err(|e| e.to_string())?;
    debouncer
        .watch(&git_dir, RecursiveMode::Recursive)
        .map_err(|e| e.to_string())?;
    Ok(WatchHandle(Box::new(debouncer)))
}

pub const CHECKOUT_EVENT: &str = "checkout-changed";

/// Watches the files of `checkout` and its index (in `git_dir`, its own
/// `.git` folder), so the lists of uncommitted changes follow edits made
/// anywhere: by an agent, an editor or Arnold itself.
pub fn watch_checkout(app: AppHandle, checkout: PathBuf, git_dir: PathBuf) -> Result<WatchHandle, String> {
    let (root, git) = (checkout.clone(), git_dir.clone());
    let mut debouncer = new_debouncer(DEBOUNCE, None, move |result: DebounceEventResult| {
        let Ok(events) = result else { return };
        let changed = events
            .iter()
            .filter(|event| !event.kind.is_access())
            .flat_map(|event| &event.paths)
            .any(|path| counts_for_checkout(&root, &git, path));
        if changed {
            let _ = app.emit(CHECKOUT_EVENT, ());
        }
    })
    .map_err(|e| e.to_string())?;
    debouncer
        .watch(&checkout, RecursiveMode::Recursive)
        .map_err(|e| e.to_string())?;
    // A linked worktree keeps its index outside the checkout.
    if !git_dir.starts_with(&checkout) {
        debouncer
            .watch(&git_dir, RecursiveMode::NonRecursive)
            .map_err(|e| e.to_string())?;
    }
    Ok(WatchHandle(Box::new(debouncer)))
}

/// A file of the checkout, or its index; not the rest of `.git`, nor
/// Arnold's own `.arnold` folder.
fn counts_for_checkout(checkout: &Path, git_dir: &Path, path: &Path) -> bool {
    if let Ok(inside) = path.strip_prefix(git_dir) {
        return inside == Path::new("index");
    }
    let Ok(relative) = path.strip_prefix(checkout) else {
        return false;
    };
    let first = relative.components().next().map(|component| component.as_os_str());
    !matches!(first, Some(name) if name == ".git" || name == ".arnold")
}

/// A branch or remote-tracking branch, loose or packed, or what a checkout
/// (the main one or a linked worktree) has checked out; not their lock files.
fn is_ref(git_dir: &Path, path: &Path) -> bool {
    let Ok(relative) = path.strip_prefix(git_dir) else {
        return false;
    };
    let relative = relative.to_string_lossy().replace('\\', "/");
    let checkout = relative == "HEAD"
        || relative
            .strip_prefix("worktrees/")
            .is_some_and(|rest| rest.split('/').nth(1) == Some("HEAD") && rest.split('/').count() == 2);
    (checkout || relative == "packed-refs" || relative.starts_with("refs/")) && !relative.ends_with(".lock")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checkout_files_and_the_index_count() {
        let checkout = Path::new("/repo");
        let git_dir = Path::new("/repo/.git");
        let counts = |path: &str| counts_for_checkout(checkout, git_dir, Path::new(path));
        assert!(counts("/repo/src/a.fs"));
        assert!(counts("/repo/.git/index"));
        assert!(!counts("/repo/.git/objects/ab/cd"));
        assert!(!counts("/repo/.git/index.lock"));
        assert!(!counts("/repo/.arnold/worktrees/x/a.fs"));
        assert!(!counts("/elsewhere/a.fs"));
        // A linked worktree's index.
        assert!(counts_for_checkout(checkout, Path::new("/main/.git/worktrees/wt"), Path::new("/main/.git/worktrees/wt/index")));
    }

    #[test]
    fn only_refs_and_checkouts_count() {
        let git_dir = Path::new("/repo/.git");
        let is_ref = |relative: &str| is_ref(git_dir, &git_dir.join(relative));

        assert!(is_ref("refs/heads/main"));
        assert!(is_ref("refs/heads/feature/x"));
        assert!(is_ref("refs/remotes/origin/main"));
        assert!(is_ref("packed-refs"));
        assert!(!is_ref("refs/heads/main.lock"));
        assert!(!is_ref("packed-refs.lock"));
        assert!(is_ref("HEAD"));
        assert!(is_ref("worktrees/wt/HEAD"));
        assert!(!is_ref("HEAD.lock"));
        assert!(!is_ref("ORIG_HEAD"));
        assert!(!is_ref("index"));
        assert!(!is_ref("objects/ab/cdef"));
        assert!(!is_ref("worktrees/wt/index"));
        assert!(!is_ref("worktrees/wt/logs/HEAD"));
        assert!(!is_ref("/elsewhere/refs/heads/main"));
    }
}
