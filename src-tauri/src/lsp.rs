//! Commands that start, feed and stop language servers for the webview.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use arnold_lsp::{dotnet, fsautocomplete, Event, Session};
use serde::Serialize;
use tauri::ipc::Channel;
use tauri::{AppHandle, Emitter, Manager, State};

use crate::commands::AppState;

pub const EVENT: &str = "lsp-event";

#[derive(Default)]
pub struct LspState {
    next_id: AtomicU32,
    sessions: Mutex<HashMap<u32, Session>>,
}

/// What the webview is told about a server, besides the LSP messages themselves.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
enum LspEvent {
    /// What the server (or its installer) is doing; `text` is null when idle.
    Status { text: Option<String> },
    Diagnostics { uri: String, errors: u32, warnings: u32 },
    Exited,
}

/// Starts the language server for `language` in the checkout `root` and
/// returns its id. Server messages arrive on `on_message`; everything else as
/// `lsp-event` events. The server is downloaded first if it is not installed.
#[tauri::command]
pub async fn lsp_start(
    language: String,
    root: String,
    on_message: Channel<String>,
    app: AppHandle,
    app_state: State<'_, AppState>,
    lsp: State<'_, LspState>,
) -> Result<u32, String> {
    if language != "fsharp" {
        return Err(format!("There is no language server for {language} yet."));
    }
    let root = app_state.checked_root(&root)?;
    let emit = {
        let app = app.clone();
        move |event: LspEvent| {
            let _ = app.emit(EVENT, event);
        }
    };
    let status = {
        let emit = emit.clone();
        move |text: Option<String>| emit(LspEvent::Status { text })
    };

    status(Some("Starting the F# language server…".into()));
    let dotnet = dotnet::locate(&root).await?;
    let install_root = app
        .path()
        .app_data_dir()
        .map_err(|e| e.to_string())?
        .join("servers")
        .join("fsautocomplete");
    let installer_status = {
        let status = status.clone();
        Arc::new(move |text: String| status(Some(text)))
    };
    let installed = fsautocomplete::ensure(&install_root, dotnet.sdk_major, installer_status).await?;
    status(Some(format!(
        "Starting FsAutoComplete {}…",
        fsautocomplete::format_version(installed.version)
    )));

    let session = Session::spawn(
        fsautocomplete::command(&dotnet, &installed, &root),
        fsautocomplete::session_config(),
        move |event| match event {
            Event::Message(message) => {
                let _ = on_message.send(message);
            }
            Event::Progress(text) => emit(LspEvent::Status { text }),
            Event::Diagnostics { uri, errors, warnings } => {
                emit(LspEvent::Diagnostics { uri, errors, warnings })
            }
            Event::Log(line) => {
                if cfg!(debug_assertions) && !line.trim().is_empty() {
                    eprintln!("[fsautocomplete] {line}");
                }
            }
            Event::Exited => emit(LspEvent::Exited),
        },
    )
    .map_err(|e| format!("could not start FsAutoComplete: {e}"))?;

    let id = lsp.next_id.fetch_add(1, Ordering::Relaxed) + 1;
    lsp.sessions.lock().unwrap().insert(id, session);
    Ok(id)
}

/// Passes one LSP message from the webview's client to the server. Not async,
/// so that messages are handled in the order they were sent.
#[tauri::command]
pub fn lsp_send(id: u32, message: String, lsp: State<'_, LspState>) {
    if let Some(session) = lsp.sessions.lock().unwrap().get(&id) {
        session.send(message);
    }
}

/// Keeps a file that no editor shows open with the server, so its errors are
/// reported too. Call again with new text when the file changes.
#[tauri::command]
pub fn lsp_open_background(id: u32, uri: String, language: String, text: String, lsp: State<'_, LspState>) {
    if let Some(session) = lsp.sessions.lock().unwrap().get(&id) {
        session.open_in_background(&uri, &language, &text);
    }
}

#[tauri::command]
pub async fn lsp_stop(id: u32, lsp: State<'_, LspState>) -> Result<(), String> {
    let session = lsp.sessions.lock().unwrap().remove(&id);
    if let Some(session) = session {
        session.shutdown().await;
    }
    Ok(())
}
