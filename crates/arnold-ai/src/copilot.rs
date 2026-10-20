//! Answers from the user's GitHub Copilot subscription, through the Copilot
//! SDK and the Copilot CLI it drives.
//!
//! Arnold starts the CLI itself (`copilot --server --stdio`) with the GitHub
//! token from `auth`, in a home folder of its own (so the user's own
//! `~/.copilot` is never touched), and talks to it through the SDK. One CLI
//! serves every conversation; each conversation is one Copilot session, so
//! follow-up questions see the earlier ones.
//!
//! Explaining code needs no tools: sessions get none (the CLI's file, shell
//! and web tools are all switched off) and any permission request is denied.

use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::{Arc, Mutex as SyncMutex};

use github_copilot_sdk::handler::DenyAllHandler;
use github_copilot_sdk::subscription::RecvErrorKind;
use github_copilot_sdk::types::{MessageOptions, SessionConfig, SystemMessageConfig};
use github_copilot_sdk::session::Session;
use github_copilot_sdk::Client;
use serde_json::Value;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::Mutex;

use crate::auth;
use crate::cli;
use crate::prompt;
use crate::provider::{AiEvent, AskRequest, Events, ModelInfo, Provider, Purpose};

const STDERR_LINES: usize = 30;

pub struct Copilot {
    /// `<app data>/copilot`: `cli/` holds the CLI, `home/` its state.
    folder: PathBuf,
    running: Mutex<Option<Running>>,
    /// Held while the CLI is downloaded, so two callers never download it at once.
    installing: Mutex<()>,
}

struct Running {
    client: Client,
    child: Child,
    token: String,
    stderr: Arc<SyncMutex<VecDeque<String>>>,
    sessions: HashMap<String, Conversation>,
}

struct Conversation {
    session: Arc<Session>,
    model: Option<String>,
}

impl Copilot {
    pub fn new(app_data: &Path) -> Self {
        Self {
            folder: app_data.join("copilot"),
            running: Mutex::new(None),
            installing: Mutex::new(()),
        }
    }

    fn cli_root(&self) -> PathBuf {
        self.folder.join("cli")
    }

    /// Whether the CLI has been downloaded already.
    pub fn installed(&self) -> bool {
        cli::installed(&self.cli_root()).is_some()
    }

    /// Stops the CLI (after signing out, or when the app closes).
    pub async fn stop(&self) {
        if let Some(mut running) = self.running.lock().await.take() {
            running.client.force_stop();
            let _ = running.child.kill().await;
        }
    }

    /// The running CLI for the current sign-in, started (and downloaded) if needed.
    async fn client(&self, events: &Events) -> Result<tokio::sync::MappedMutexGuard<'_, Running>, String> {
        let (token, _) = auth::load()?.ok_or("Sign in to GitHub to use Copilot.")?;
        let status = {
            let events = events.clone();
            Arc::new(move |text: String| events(AiEvent::Status { text })) as cli::Status
        };
        let program = {
            let _installing = self.installing.lock().await;
            cli::ensure(&self.cli_root(), status).await?
        };

        let mut running = self.running.lock().await;
        let usable = match running.as_mut() {
            Some(current) => current.token == token && matches!(current.child.try_wait(), Ok(None)),
            None => false,
        };
        if !usable {
            if let Some(mut old) = running.take() {
                old.client.force_stop();
                let _ = old.child.kill().await;
            }
            events(AiEvent::Status {
                text: "Starting GitHub Copilot…".into(),
            });
            *running = Some(self.start(&program, token).await?);
        }
        Ok(tokio::sync::MutexGuard::map(running, |running| {
            running.as_mut().expect("started above")
        }))
    }

    async fn start(&self, program: &Path, token: String) -> Result<Running, String> {
        let home = self.folder.join("home");
        std::fs::create_dir_all(&home).map_err(|e| format!("{}: {e}", home.display()))?;
        let mut command = Command::new(program);
        command
            .args([
                "--server",
                "--stdio",
                "--no-auto-update",
                "--auth-token-env",
                "COPILOT_SDK_AUTH_TOKEN",
                "--no-auto-login",
                "--no-custom-instructions",
                "--disable-builtin-mcps",
                "--disable-client-telemetry",
            ])
            .env("COPILOT_SDK_AUTH_TOKEN", &token)
            .env("COPILOT_HOME", &home)
            .current_dir(&home)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true);
        #[cfg(windows)]
        command.creation_flags(0x0800_0000); // CREATE_NO_WINDOW
        let mut child = command
            .spawn()
            .map_err(|e| format!("Could not start the GitHub Copilot CLI ({}): {e}", program.display()))?;

        let stderr = Arc::new(SyncMutex::new(VecDeque::new()));
        if let Some(pipe) = child.stderr.take() {
            let stderr = stderr.clone();
            tokio::spawn(async move {
                let mut lines = BufReader::new(pipe).lines();
                while let Ok(Some(line)) = lines.next_line().await {
                    let mut tail = stderr.lock().unwrap();
                    if tail.len() == STDERR_LINES {
                        tail.pop_front();
                    }
                    tail.push_back(line);
                }
            });
        }
        let (Some(stdout), Some(stdin)) = (child.stdout.take(), child.stdin.take()) else {
            return Err("Could not talk to the GitHub Copilot CLI.".into());
        };
        let failed = |error: String, stderr: &Arc<SyncMutex<VecDeque<String>>>| {
            let tail: Vec<String> = stderr.lock().unwrap().iter().cloned().collect();
            if tail.is_empty() {
                error
            } else {
                format!("{error}\n\n{}", tail.join("\n"))
            }
        };
        let client = Client::from_streams(stdout, stdin, home.clone())
            .map_err(|e| failed(format!("Could not start GitHub Copilot: {e}"), &stderr))?;
        client
            .verify_protocol_version()
            .await
            .map_err(|e| failed(format!("Could not start GitHub Copilot: {e}"), &stderr))?;
        let auth = client
            .get_auth_status()
            .await
            .map_err(|e| failed(format!("GitHub Copilot could not check the sign-in: {e}"), &stderr))?;
        if !auth.is_authenticated {
            return Err(
                "GitHub did not accept the sign-in for Copilot. Sign out and sign in again.".into(),
            );
        }
        Ok(Running {
            client,
            child,
            token,
            stderr,
            sessions: HashMap::new(),
        })
    }

    async fn session(&self, request: &AskRequest, events: &Events) -> Result<Arc<Session>, String> {
        let mut running = self.client(events).await?;
        if let Some(conversation) = running.sessions.get_mut(&request.conversation) {
            if conversation.model != request.model {
                let model = request.model.as_deref().unwrap_or("auto");
                conversation
                    .session
                    .set_model(model, None)
                    .await
                    .map_err(|e| format!("Could not switch to {model}: {e}"))?;
                conversation.model = request.model.clone();
            }
            return Ok(conversation.session.clone());
        }
        let mut config = SessionConfig::default().with_permission_handler(Arc::new(DenyAllHandler));
        config.streaming = Some(true);
        config.client_name = Some("Arnold".into());
        config.model = Some(request.model.clone().unwrap_or_else(|| "auto".into()));
        // An empty allow-list: no built-in tools (files, shell, web) and no MCP tools.
        config.available_tools = Some(Vec::new());
        config.system_message = Some(
            SystemMessageConfig::new()
                .with_mode("replace")
                .with_content(match request.purpose {
                    Purpose::Explain => prompt::SYSTEM,
                    Purpose::Review => prompt::REVIEW,
                }),
        );
        let session = Arc::new(
            running
                .client
                .create_session(config)
                .await
                .map_err(|e| format!("Could not start a Copilot conversation: {e}"))?,
        );
        running.sessions.insert(
            request.conversation.clone(),
            Conversation {
                session: session.clone(),
                model: request.model.clone(),
            },
        );
        Ok(session)
    }

    async fn stderr_tail(&self) -> String {
        match self.running.lock().await.as_ref() {
            Some(running) => running.stderr.lock().unwrap().iter().cloned().collect::<Vec<_>>().join("\n"),
            None => String::new(),
        }
    }
}

#[async_trait::async_trait]
impl Provider for Copilot {
    async fn prepare(&self, events: Events) -> Result<(), String> {
        self.client(&events).await.map(|_| ())
    }

    async fn ask(&self, request: AskRequest, events: Events) -> Result<(), String> {
        let session = self.session(&request, &events).await?;
        let mut subscription = session.subscribe();
        session
            .send(MessageOptions::new(prompt::message(&request.question, &request.context)))
            .await
            .map_err(|e| describe(&e.to_string()))?;
        let mut model = None;
        loop {
            let event = match subscription.recv().await {
                Ok(event) => event,
                Err(error) => match error.kind() {
                    RecvErrorKind::Lagged(_) => continue,
                    _ => {
                        let tail = self.stderr_tail().await;
                        return Err(format!("GitHub Copilot stopped while answering.\n\n{tail}").trim().to_string());
                    }
                },
            };
            let text = |key: &str| event.data.get(key).and_then(Value::as_str).unwrap_or("").to_string();
            match event.event_type.as_str() {
                "assistant.message_delta" => events(AiEvent::Delta { text: text("deltaContent") }),
                "assistant.reasoning_delta" => events(AiEvent::Reasoning { text: text("deltaContent") }),
                "assistant.usage" => {
                    model = Some(text("modelDisplayName")).filter(|name| !name.is_empty()).or(model);
                }
                "session.error" => return Err(describe(&text("message"))),
                "session.idle" => {
                    events(AiEvent::Done { model });
                    return Ok(());
                }
                _ => {}
            }
        }
    }

    async fn cancel(&self, conversation: &str) {
        let session = self
            .running
            .lock()
            .await
            .as_ref()
            .and_then(|running| running.sessions.get(conversation))
            .map(|conversation| conversation.session.clone());
        if let Some(session) = session {
            let _ = session.abort().await;
        }
    }

    async fn end(&self, conversation: &str) {
        let removed = self
            .running
            .lock()
            .await
            .as_mut()
            .and_then(|running| running.sessions.remove(conversation));
        if let Some(conversation) = removed {
            let _ = conversation.session.disconnect().await;
        }
    }

    async fn models(&self) -> Result<Vec<ModelInfo>, String> {
        let running = self.running.lock().await;
        let Some(running) = running.as_ref() else {
            return Ok(Vec::new());
        };
        let list = running
            .client
            .call("models.list", Some(serde_json::json!({})))
            .await
            .map_err(|e| e.to_string())?;
        Ok(parse_models(&list))
    }
}

fn parse_models(list: &Value) -> Vec<ModelInfo> {
    list["models"]
        .as_array()
        .into_iter()
        .flatten()
        .filter(|model| model["availability"].as_str().is_none_or(|a| a != "unavailable"))
        .filter_map(|model| {
            let id = model["id"].as_str()?.to_string();
            let name = model["name"].as_str().unwrap_or(&id).to_string();
            Some(ModelInfo { id, name })
        })
        .collect()
}

/// Copilot's error text, with what to do about the common ones.
fn describe(message: &str) -> String {
    let lower = message.to_lowercase();
    if lower.contains("quota") || lower.contains("credits") || lower.contains("rate limit") {
        format!("{message}\n\nYour Copilot allowance may be used up; see github.com/settings/copilot.")
    } else if lower.contains("not authorized") || lower.contains("no access") || lower.contains("subscription") || lower.contains("403") {
        format!(
            "{message}\n\nThe signed-in GitHub account may have no Copilot plan, or your organization may not allow Copilot outside its approved editors."
        )
    } else if message.is_empty() {
        "GitHub Copilot could not answer.".into()
    } else {
        message.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn lists_only_available_models() {
        let list = json!({"models": [
            {"id": "gpt-6-luna", "name": "GPT-6 Luna"},
            {"id": "x", "availability": "unavailable"},
            {"id": "claude-sonnet-5-5"}
        ]});
        assert_eq!(
            parse_models(&list),
            [
                ModelInfo { id: "gpt-6-luna".into(), name: "GPT-6 Luna".into() },
                ModelInfo { id: "claude-sonnet-5-5".into(), name: "claude-sonnet-5-5".into() },
            ]
        );
        assert_eq!(parse_models(&json!({"models": []})), []);
    }

    #[test]
    fn explains_common_errors() {
        assert!(describe("Request failed: 403").contains("no Copilot plan"));
        assert!(describe("monthly quota exceeded").contains("allowance"));
        assert_eq!(describe("boom"), "boom");
    }
}
