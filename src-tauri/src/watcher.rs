//! Watches the reviewed checkout and tells the webview when files change, so
//! the diff follows an agent that is still writing.

use std::any::Any;
use std::path::{Path, PathBuf};
use std::time::Duration;

use ignore::gitignore::{Gitignore, GitignoreBuilder};
use notify::RecursiveMode;
use notify_debouncer_full::{new_debouncer, DebounceEventResult};
use serde::Serialize;
use tauri::{AppHandle, Emitter};

pub const EVENT: &str = "worktree-changed";
const DEBOUNCE: Duration = Duration::from_millis(400);

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct Changed {
    root: PathBuf,
    /// Changed files, relative to `root`, forward slashes.
    paths: Vec<String>,
    /// HEAD, the index or a ref moved (commit, checkout, staging).
    git_state_changed: bool,
}

/// Keeps the watch alive; dropping it stops watching.
pub struct WatchHandle(#[allow(dead_code)] Box<dyn Any + Send>);

pub fn watch(app: AppHandle, root: PathBuf) -> Result<WatchHandle, String> {
    let ignore = load_gitignore(&root);
    let event_root = root.clone();
    let mut debouncer = new_debouncer(DEBOUNCE, None, move |result: DebounceEventResult| {
        let Ok(events) = result else { return };
        let mut paths = Vec::new();
        let mut git_state_changed = false;
        for event in events.iter().filter(|e| !e.kind.is_access()) {
            for path in &event.paths {
                match classify(&event_root, &ignore, path) {
                    Relevance::File(relative) => {
                        if !paths.contains(&relative) {
                            paths.push(relative);
                        }
                    }
                    Relevance::GitState => git_state_changed = true,
                    Relevance::Ignored => {}
                }
            }
        }
        if !paths.is_empty() || git_state_changed {
            let _ = app.emit(
                EVENT,
                Changed {
                    root: event_root.clone(),
                    paths,
                    git_state_changed,
                },
            );
        }
    })
    .map_err(|e| e.to_string())?;
    debouncer
        .watch(&root, RecursiveMode::Recursive)
        .map_err(|e| e.to_string())?;
    Ok(WatchHandle(Box::new(debouncer)))
}

fn load_gitignore(root: &Path) -> Gitignore {
    let mut builder = GitignoreBuilder::new(root);
    builder.add(root.join(".gitignore"));
    builder.add(root.join(".git/info/exclude"));
    builder.build().unwrap_or_else(|_| Gitignore::empty())
}

#[derive(Debug, PartialEq)]
enum Relevance {
    File(String),
    GitState,
    Ignored,
}

fn classify(root: &Path, ignore: &Gitignore, path: &Path) -> Relevance {
    let Ok(relative) = path.strip_prefix(root) else {
        return Relevance::Ignored;
    };
    let relative_text = relative.to_string_lossy().replace('\\', "/");
    if relative_text.is_empty() {
        return Relevance::Ignored;
    }
    if relative_text == ".git" || relative_text.starts_with(".git/") {
        let state = matches!(relative_text.as_str(), ".git/HEAD" | ".git/index")
            || relative_text.starts_with(".git/refs/");
        return if state {
            Relevance::GitState
        } else {
            Relevance::Ignored
        };
    }
    if ignore
        .matched_path_or_any_parents(relative, path.is_dir())
        .is_ignore()
    {
        return Relevance::Ignored;
    }
    Relevance::File(relative_text)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filters_ignored_and_git_internal_paths() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::write(root.join(".gitignore"), "bin/\nobj/\n*.log\n").unwrap();
        let ignore = load_gitignore(root);
        let classify = |relative: &str| classify(root, &ignore, &root.join(relative));

        assert_eq!(classify("src/Orders.fs"), Relevance::File("src/Orders.fs".into()));
        assert_eq!(classify("obj/Debug/net10.0/App.dll"), Relevance::Ignored);
        assert_eq!(classify("src/bin/x.dll"), Relevance::Ignored);
        assert_eq!(classify("build.log"), Relevance::Ignored);
        assert_eq!(classify(".git/index"), Relevance::GitState);
        assert_eq!(classify(".git/refs/heads/main"), Relevance::GitState);
        assert_eq!(classify(".git/objects/ab/cdef"), Relevance::Ignored);
        assert_eq!(classify(".git/index.lock"), Relevance::Ignored);
    }
}
