//! Prepares the reviewed commit for the F# language server: its files copied
//! to `.arnold/worktrees/<branch>` in the checkout (ignored by git, and not a
//! registered worktree), then
//! restored, and built only where FsAutoComplete needs compiled assemblies.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use arnold_lsp::dotnet;
use arnold_lsp::workspace::{self, Done, Plan};
use serde::Serialize;
use tauri::ipc::Channel;
use tauri::State;
use tokio::sync::{oneshot, Mutex};
use tokio::task::JoinHandle;

use crate::commands::AppState;

/// The preparation under way; a new one stops it.
#[derive(Default)]
pub struct PrepareState {
    job: Mutex<Option<JoinHandle<()>>>,
}

/// What the webview is told while a preparation runs.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum SetupEvent {
    /// A step started.
    Step { title: String },
    /// A step was not needed: what it would do was done earlier.
    Skipped { title: String, reason: String },
    /// A line a step printed.
    Output { line: String },
    /// The running step failed. Preparation goes on, but language features
    /// may be incomplete.
    Warning { text: String },
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Prepared {
    /// Folder to start the language server in; `None` when the commit has no
    /// F# projects.
    path: Option<PathBuf>,
}

/// Copies `commit` (the tip of branch `head`) of the repository at `root`
/// into its `.arnold/worktrees` folder
/// and gets it ready for FsAutoComplete. Steps are reported on `on_event`.
/// Starting another preparation stops this one, which then fails with
/// "cancelled".
#[tauri::command]
pub async fn prepare_workspace(
    root: String,
    head: String,
    commit: String,
    on_event: Channel<SetupEvent>,
    app_state: State<'_, AppState>,
    prepare_state: State<'_, PrepareState>,
) -> Result<Prepared, String> {
    let root = app_state.checked_root(&root)?;
    let (sender, receiver) = oneshot::channel();
    {
        let mut job = prepare_state.job.lock().await;
        if let Some(previous) = job.take() {
            // Dropping its futures kills the git or dotnet process it runs;
            // wait for that, as the next one may use the same folder.
            previous.abort();
            let _ = previous.await;
        }
        *job = Some(tokio::spawn(async move {
            let _ = sender.send(prepare(&root, &head, &commit, &on_event).await);
        }));
    }
    let prepared = receiver.await.map_err(|_| "cancelled".to_string())??;
    if let Some(path) = &prepared.path {
        app_state.allow_root(path);
    }
    Ok(prepared)
}

async fn prepare(root: &Path, head: &str, commit: &str, events: &Channel<SetupEvent>) -> Result<Prepared, String> {
    let emit = |event: SetupEvent| {
        let _ = events.send(event);
    };
    let files: Vec<(String, String)> = arnold_git::tree_files(root, commit)
        .await
        .map_err(|e| e.to_string())?
        .into_iter()
        .map(|file| (file.path, file.oid))
        .collect();
    let mut projects = HashMap::new();
    for (path, _) in files.iter().filter(|(path, _)| workspace::is_fsharp_project(path)) {
        if let Ok(Some(bytes)) = arnold_git::read_blob(root, commit, path).await {
            projects.insert(path.clone(), String::from_utf8_lossy(&bytes).into_owned());
        }
    }
    let Some(plan) = workspace::plan(&files, &projects) else {
        return Ok(Prepared { path: None });
    };

    let Folders { checkout, state } = folders_for(root, head).await?;
    emit(SetupEvent::Step {
        title: format!(
            "Copying {} to {}",
            &commit[..commit.len().min(8)],
            checkout.strip_prefix(root).unwrap_or(&checkout).display()
        ),
    });
    arnold_git::export_commit(root, commit, &checkout, &state.join("index"))
        .await
        .map_err(|e| e.to_string())?;
    // The server reports resolved paths (the repository may be opened through a symlink).
    // Not `std` canonicalize: on Windows that gives `\\?\C:\…`, which becomes a
    // root URI FsAutoComplete cannot load, so it never finishes loading.
    let checkout = dunce::canonicalize(&checkout).map_err(|e| e.to_string())?;

    let dotnet = dotnet::locate(&checkout).await?;
    run_plan(&plan, &dotnet, &checkout, &state.join("prepared.json"), &emit).await;
    Ok(Prepared { path: Some(checkout) })
}

/// Restores and builds what `plan` asks for, skipping what was done before
/// for the same inputs. Failures are reported as warnings, never errors: the
/// server still works for whatever could be loaded.
async fn run_plan(plan: &Plan, dotnet: &dotnet::Dotnet, checkout: &Path, done_file: &Path, emit: &(dyn Fn(SetupEvent) + Sync)) {
    let on_line = |line: String| emit(SetupEvent::Output { line });
    let warn = |text: String| emit(SetupEvent::Warning { text });
    let mut done = Done::load(done_file);

    let restored = if done.restore.as_deref() == Some(plan.restore_stamp.as_str()) {
        emit(SetupEvent::Skipped {
            title: "Restore packages".into(),
            reason: "already restored for these project files".into(),
        });
        true
    } else {
        if plan.tool_restore {
            emit(SetupEvent::Step {
                title: "Restoring .NET tools (dotnet tool restore)".into(),
            });
            if let Err(error) = workspace::run_dotnet(dotnet, checkout, &["tool", "restore"], &on_line).await {
                warn(error);
            }
        }
        let mut succeeded = true;
        for target in &plan.restore {
            emit(SetupEvent::Step {
                title: format!("Restoring packages for {target} (dotnet restore)"),
            });
            if let Err(error) = workspace::run_dotnet(dotnet, checkout, &["restore", target, "-nodeReuse:false", "-v:minimal"], &on_line).await {
                warn(error);
                succeeded = false;
            }
        }
        if succeeded {
            done.restore = Some(plan.restore_stamp.clone());
            done.save(done_file);
        }
        succeeded
    };

    if plan.build.is_empty() {
        return;
    }
    if done.build.as_deref() == Some(plan.build_stamp.as_str()) {
        emit(SetupEvent::Skipped {
            title: "Build referenced projects".into(),
            reason: "already built for these files".into(),
        });
        return;
    }
    if !restored {
        emit(SetupEvent::Skipped {
            title: "Build referenced projects".into(),
            reason: "restoring failed".into(),
        });
        return;
    }
    let mut succeeded = true;
    for target in &plan.build {
        emit(SetupEvent::Step {
            title: format!("Building {target} (dotnet build): F# code uses its compiled assembly"),
        });
        let args = ["build", target, "--no-restore", "-nodeReuse:false", "-v:minimal", "-p:UseSharedCompilation=false"];
        if let Err(error) = workspace::run_dotnet(dotnet, checkout, &args, &on_line).await {
            warn(error);
            succeeded = false;
        }
    }
    if succeeded {
        done.build = Some(plan.build_stamp.clone());
        done.save(done_file);
    }
}

struct Folders {
    /// `<root>/.arnold/worktrees/<branch>`: the copied files.
    checkout: PathBuf,
    /// `<root>/.arnold/state/<branch>`: the private index and what was prepared.
    state: PathBuf,
}

/// One copy per branch, so a branch that moves on is updated in place and its
/// restore output reused.
async fn folders_for(root: &Path, head: &str) -> Result<Folders, String> {
    let folder = arnold_git::app_folder(root).await.map_err(|e| e.to_string())?;
    let name = format!("{}-{:06x}", slug(head), fnv(head.as_bytes()) as u32 & 0xff_ffff);
    Ok(Folders {
        checkout: folder.join("worktrees").join(&name),
        state: folder.join("state").join(name),
    })
}

/// A name safe in a path on every platform.
fn slug(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '.' || c == '-' || c == '_' { c } else { '-' })
        .take(60)
        .collect()
}

fn fnv(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf2_9ce4_8422_2325, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x0100_0000_01b3)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn branch_folders_are_safe_and_distinct() {
        assert_eq!(slug("feature/agent work"), "feature-agent-work");
        assert_ne!(fnv(b"feature/x"), fnv(b"feature-x"));
    }
}
