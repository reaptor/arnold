//! AI assistance for Arnold: explaining code being reviewed, and suggesting
//! improvements to the changes.
//!
//! `provider` is what the app talks to, whoever runs the model. `copilot` is
//! the first provider: the user's own GitHub Copilot subscription, through
//! GitHub's Copilot SDK and the Copilot CLI (`cli` downloads it, `auth` signs
//! in to GitHub). Arnold's own hosted service will be another provider.
//!
//! What leaves the machine: the code the user asks about, sent to GitHub
//! Copilot with the question, and the GitHub sign-in.

pub mod auth;
pub mod cli;
mod copilot;
mod prompt;
pub mod provider;

pub use copilot::Copilot;
pub use provider::{AiEvent, AskRequest, ContextItem, ContextKind, Events, ModelInfo, Provider, Purpose};

pub(crate) fn network_error(url: &str, error: &ureq::Error) -> String {
    let host = url.split('/').nth(2).unwrap_or(url);
    let text = error.to_string();
    if text.contains("certificate") {
        format!(
            "Could not reach {host}: its certificate is not trusted ({text}). \
             On a company network, its root certificate has to be installed in the system's certificate store \
             (Keychain Access on macOS, Manage computer certificates on Windows); your IT department usually does this."
        )
    } else {
        format!("Could not reach {host}: {text}")
    }
}
