//! Explaining code with AI: signing in to GitHub for Copilot, asking
//! questions (answers stream back over a Channel), and the diff text a
//! question is about.

use std::sync::{Arc, OnceLock};

use arnold_ai::auth::{self, DeviceCode, TokenSource};
use arnold_ai::{AiEvent, AskRequest, Copilot, ModelInfo, Provider};
use serde::{Deserialize, Serialize};
use tauri::ipc::Channel;
use tauri::{AppHandle, Manager, State};
use tokio::sync::Mutex;
use tokio::task::JoinHandle;

use crate::commands::AppState;

/// About 40k tokens of diff: room for the answer, and fast to send.
const PATCH_LIMIT: usize = 150_000;

#[derive(Default)]
pub struct AiState {
    copilot: OnceLock<Arc<Copilot>>,
    sign_in: Mutex<Option<JoinHandle<()>>>,
    /// Login names by token, so the status does not ask GitHub every time.
    logins: Mutex<Vec<(String, Option<String>)>>,
}

impl AiState {
    fn copilot(&self, app: &AppHandle) -> Result<Arc<Copilot>, String> {
        if let Some(copilot) = self.copilot.get() {
            return Ok(copilot.clone());
        }
        let data = app.path().app_data_dir().map_err(|e| e.to_string())?;
        Ok(self.copilot.get_or_init(|| Arc::new(Copilot::new(&data))).clone())
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AiStatus {
    signed_in: bool,
    /// The GitHub account, when known.
    login: Option<String>,
    source: Option<TokenSource>,
    /// The saved sign-in is no longer accepted by GitHub.
    expired: bool,
    /// The Copilot CLI is downloaded already.
    installed: bool,
}

#[tauri::command]
pub async fn ai_status(app: AppHandle, state: State<'_, AiState>) -> Result<AiStatus, String> {
    let installed = state.copilot(&app)?.installed();
    let Some((token, source)) = auth::load()? else {
        return Ok(AiStatus {
            signed_in: false,
            login: None,
            source: None,
            expired: false,
            installed,
        });
    };
    let known = state.logins.lock().await.iter().find(|(t, _)| *t == token).map(|(_, login)| login.clone());
    let login = match known {
        Some(login) => Ok(login),
        None => {
            let asked = token.clone();
            let looked_up = tokio::task::spawn_blocking(move || auth::login(&asked))
                .await
                .map_err(|e| e.to_string())?;
            if let Ok(login) = &looked_up {
                state.logins.lock().await.push((token.clone(), login.clone()));
            }
            looked_up
        }
    };
    // Offline: assume the sign-in still works; asking will tell.
    let expired = matches!(login, Ok(None));
    Ok(AiStatus {
        signed_in: !expired,
        login: login.ok().flatten(),
        source: Some(source),
        expired,
        installed,
    })
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum SignInEvent {
    /// Show this code and open the page.
    Code {
        #[serde(flatten)]
        code: DeviceCode,
    },
}

/// Signs in to GitHub with a device code (sent on `on_event`); returns the
/// login name once the code was entered. A new sign-in or
/// `ai_cancel_sign_in` stops it ("cancelled").
#[tauri::command]
pub async fn ai_sign_in(
    on_event: Channel<SignInEvent>,
    app: AppHandle,
    state: State<'_, AiState>,
) -> Result<String, String> {
    let (sender, receiver) = tokio::sync::oneshot::channel();
    {
        let mut job = state.sign_in.lock().await;
        if let Some(previous) = job.take() {
            previous.abort();
        }
        *job = Some(tokio::spawn(async move {
            let result = auth::sign_in(|code| {
                let _ = on_event.send(SignInEvent::Code { code: code.clone() });
            })
            .await;
            let _ = sender.send(result);
        }));
    }
    let login = receiver.await.map_err(|_| "cancelled".to_string())??;
    // A running CLI still has the previous token; the next question restarts it.
    state.copilot(&app)?.stop().await;
    Ok(login)
}

#[tauri::command]
pub async fn ai_cancel_sign_in(state: State<'_, AiState>) -> Result<(), String> {
    if let Some(job) = state.sign_in.lock().await.take() {
        job.abort();
    }
    Ok(())
}

#[tauri::command]
pub async fn ai_sign_out(app: AppHandle, state: State<'_, AiState>) -> Result<(), String> {
    auth::sign_out()?;
    state.logins.lock().await.clear();
    state.copilot(&app)?.stop().await;
    Ok(())
}

fn emitter(channel: Channel<AiEvent>) -> arnold_ai::Events {
    Arc::new(move |event| {
        let _ = channel.send(event);
    })
}

/// Downloads and starts Copilot ahead of the first question, reporting progress.
#[tauri::command]
pub async fn ai_prepare(on_event: Channel<AiEvent>, app: AppHandle, state: State<'_, AiState>) -> Result<(), String> {
    state.copilot(&app)?.prepare(emitter(on_event)).await
}

/// Answers `request`, streaming on `on_event`; returns when the answer is complete.
#[tauri::command]
pub async fn ai_ask(
    request: AskRequest,
    on_event: Channel<AiEvent>,
    app: AppHandle,
    state: State<'_, AiState>,
) -> Result<(), String> {
    let copilot = state.copilot(&app)?;
    // Runs on its own task so the answer is not dropped if the webview's call goes away.
    tokio::spawn(async move { copilot.ask(request, emitter(on_event)).await })
        .await
        .map_err(|e| e.to_string())?
}

#[tauri::command]
pub async fn ai_cancel(conversation: String, app: AppHandle, state: State<'_, AiState>) -> Result<(), String> {
    state.copilot(&app)?.cancel(&conversation).await;
    Ok(())
}

#[tauri::command]
pub async fn ai_end(conversation: String, app: AppHandle, state: State<'_, AiState>) -> Result<(), String> {
    state.copilot(&app)?.end(&conversation).await;
    Ok(())
}

#[tauri::command]
pub async fn ai_models(app: AppHandle, state: State<'_, AiState>) -> Result<Vec<ModelInfo>, String> {
    state.copilot(&app)?.models().await
}

/// What `review_patch` diffs (see `arnold_git::PatchSource`).
#[derive(Debug, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum PatchOf {
    Commits { from: String, to: String },
    OnDisk { from: String },
    Unstaged,
    Staged,
}

/// The changes in `cwd` (the repository or one of its checkouts) as unified-diff text.
#[tauri::command]
pub async fn review_patch(
    cwd: String,
    of: PatchOf,
    paths: Vec<String>,
    app_state: State<'_, AppState>,
) -> Result<arnold_git::Patch, String> {
    let cwd = app_state.checked_root(&cwd)?;
    let source = match &of {
        PatchOf::Commits { from, to } => arnold_git::PatchSource::Commits { from, to },
        PatchOf::OnDisk { from } => arnold_git::PatchSource::OnDisk { from },
        PatchOf::Unstaged => arnold_git::PatchSource::Unstaged,
        PatchOf::Staged => arnold_git::PatchSource::Staged,
    };
    arnold_git::patch(&cwd, source, &paths, PATCH_LIMIT)
        .await
        .map_err(|e| e.to_string())
}
