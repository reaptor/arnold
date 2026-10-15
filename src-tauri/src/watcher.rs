//! Watches the repository's branches and tells the webview when one moves,
//! so the diff follows an agent that keeps committing. Files on disk are not
//! watched: the review only ever shows commits.

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

/// A branch or remote-tracking branch, loose or packed (not its lock file).
fn is_ref(git_dir: &Path, path: &Path) -> bool {
    let Ok(relative) = path.strip_prefix(git_dir) else {
        return false;
    };
    let relative = relative.to_string_lossy().replace('\\', "/");
    (relative == "packed-refs" || relative.starts_with("refs/")) && !relative.ends_with(".lock")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_refs_count() {
        let git_dir = Path::new("/repo/.git");
        let is_ref = |relative: &str| is_ref(git_dir, &git_dir.join(relative));

        assert!(is_ref("refs/heads/main"));
        assert!(is_ref("refs/heads/feature/x"));
        assert!(is_ref("refs/remotes/origin/main"));
        assert!(is_ref("packed-refs"));
        assert!(!is_ref("refs/heads/main.lock"));
        assert!(!is_ref("packed-refs.lock"));
        assert!(!is_ref("index"));
        assert!(!is_ref("HEAD"));
        assert!(!is_ref("objects/ab/cdef"));
        assert!(!is_ref("worktrees/wt/index"));
        assert!(!is_ref("/elsewhere/refs/heads/main"));
    }
}
