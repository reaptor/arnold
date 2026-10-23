//! Building the solution on request (Ctrl+Shift+B) in the checkout the
//! language server runs in; the webview reads the errors and warnings from
//! the output and can cancel the build.

use std::path::Path;

use arnold_lsp::workspace;
use arnold_lsp::{build, dotnet};
use serde::Serialize;
use tauri::ipc::Channel;
use tauri::State;
use tokio::sync::{oneshot, Mutex};
use tokio::task::JoinHandle;

use crate::commands::AppState;

/// The build under way; cancelling or starting another one stops it.
#[derive(Default)]
pub struct BuildState {
    job: Mutex<Option<JoinHandle<()>>>,
}

/// What the webview is told while a build runs.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum BuildEvent {
    /// `dotnet build` started on a solution or project.
    Started { target: String },
    /// A line it printed.
    Output { line: String },
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Built {
    /// Every target built without errors.
    succeeded: bool,
}

/// Builds the solution in `root` (or its F# projects when it has none),
/// reporting on `on_event`. Fails with "cancelled" when `cancel_build` or
/// another build stops it.
#[tauri::command]
pub async fn build_solution(
    root: String,
    on_event: Channel<BuildEvent>,
    app_state: State<'_, AppState>,
    build_state: State<'_, BuildState>,
) -> Result<Built, String> {
    let root = app_state.checked_root(&root)?;
    let (sender, receiver) = oneshot::channel();
    {
        let mut job = build_state.job.lock().await;
        stop(&mut job).await;
        *job = Some(tokio::spawn(async move {
            let emit = |event: BuildEvent| {
                let _ = on_event.send(event);
            };
            let _ = sender.send(run(&root, &emit).await);
        }));
    }
    receiver.await.map_err(|_| "cancelled".to_string())?
}

/// Stops the build under way, if any.
#[tauri::command]
pub async fn cancel_build(build_state: State<'_, BuildState>) -> Result<(), String> {
    stop(&mut *build_state.job.lock().await).await;
    Ok(())
}

async fn stop(job: &mut Option<JoinHandle<()>>) {
    if let Some(previous) = job.take() {
        // Dropping its future kills the dotnet process; wait for that, as
        // the next build uses the same folder.
        previous.abort();
        let _ = previous.await;
    }
}

async fn run(root: &Path, emit: &(dyn Fn(BuildEvent) + Sync)) -> Result<Built, String> {
    let targets = build::targets(root);
    if targets.is_empty() {
        return Err(format!("There is no solution or project to build in {}.", root.display()));
    }
    let dotnet = dotnet::locate(root).await?;
    let on_line = |line: String| emit(BuildEvent::Output { line });
    let mut succeeded = true;
    for target in &targets {
        emit(BuildEvent::Started { target: target.clone() });
        if let Err(error) = workspace::run_dotnet(&dotnet, root, &build::arguments(target), &on_line).await {
            on_line(error);
            succeeded = false;
        }
    }
    Ok(Built { succeeded })
}
