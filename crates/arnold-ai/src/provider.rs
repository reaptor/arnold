//! What every AI provider offers Arnold, whoever runs the model: the user's
//! GitHub Copilot subscription now, Arnold's own hosted service later.

use std::path::PathBuf;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

/// Something the webview streams into an answer.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum AiEvent {
    /// Getting ready to answer (downloading or starting the provider's runtime).
    Status { text: String },
    /// More of the answer (Markdown).
    Delta { text: String },
    /// More of the model's reasoning summary, when it shares one.
    Reasoning { text: String },
    /// The answer is complete (or was stopped).
    Done { model: Option<String> },
    Error { message: String },
    // Agent coding will add tool calls and permission requests here.
}

pub type Events = Arc<dyn Fn(AiEvent) + Send + Sync>;

/// One question in a conversation; later questions in the same conversation
/// see the earlier ones and their answers.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AskRequest {
    pub conversation: String,
    /// `None` lets the provider pick.
    pub model: Option<String>,
    pub question: String,
    #[serde(default)]
    pub context: Vec<ContextItem>,
    /// The checkout being reviewed, if any.
    pub cwd: Option<PathBuf>,
}

/// Code the question is about.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ContextItem {
    /// Shown to the model as where the text comes from, e.g. "src/Orders.fs, lines 12–30 (new version)".
    pub label: String,
    pub kind: ContextKind,
    pub text: String,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum ContextKind {
    /// Selected lines of one version of a file.
    Selection,
    /// A whole file.
    File,
    /// A unified diff.
    Diff,
    /// Anything else worth knowing (commit messages, a pull request's description).
    Note,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ModelInfo {
    pub id: String,
    pub name: String,
}

#[async_trait::async_trait]
pub trait Provider: Send + Sync {
    /// Gets ready to answer (e.g. downloads and starts a runtime), reporting progress.
    async fn prepare(&self, events: Events) -> Result<(), String>;
    /// Answers `request`, streaming the answer as events, and returns once it is complete.
    async fn ask(&self, request: AskRequest, events: Events) -> Result<(), String>;
    /// Stops the answer being written in `conversation`, if any.
    async fn cancel(&self, conversation: &str);
    /// Forgets `conversation`.
    async fn end(&self, conversation: &str);
    /// Models the user can pick; empty when only the provider's own choice is available.
    async fn models(&self) -> Result<Vec<ModelInfo>, String>;
}
