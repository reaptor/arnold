//! Pull requests of the code host a repository's remote points at, so a
//! pull request can be reviewed instead of a pair of branches.
//!
//! Only Azure DevOps (Services and Server) for now; the types are kept free
//! of it so that other hosts (GitHub) can be added next to it.
//!
//! Everything here blocks (HTTP, the system keychain): call it off the async
//! runtime's threads.

pub mod azure;
pub mod token;

use serde::Serialize;

/// A pull request, as far as Arnold needs one.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct PullRequest {
    pub id: u64,
    pub title: String,
    pub description: String,
    /// Display name of whoever opened it.
    pub author: String,
    /// When it was opened, ISO 8601.
    pub created: String,
    pub draft: bool,
    /// `active`, `completed` or `abandoned`.
    pub status: String,
    /// Branch names without `refs/heads/`.
    pub source_branch: String,
    pub target_branch: String,
    /// The commits the host last compared: the tip of the source branch and
    /// of the target branch. Missing while the host is still working on a
    /// new push.
    pub source_commit: Option<String>,
    pub target_commit: Option<String>,
    /// The pull request's page on the host.
    pub url: String,
    pub reviewers: Vec<Reviewer>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Reviewer {
    pub name: String,
    pub vote: Vote,
    /// A group (team) rather than a person.
    pub group: bool,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum Vote {
    Approved,
    ApprovedWithSuggestions,
    None,
    WaitingForAuthor,
    Rejected,
}

/// Why pull requests could not be read. Sent to the webview as
/// `{ kind, message }`.
#[derive(Debug, Clone, thiserror::Error, Serialize, PartialEq)]
#[serde(tag = "kind", content = "message", rename_all = "camelCase")]
pub enum PrError {
    /// No token has been set up for the organization.
    #[error("no personal access token is set up")]
    NoToken,
    /// The host turned the token down: wrong organization, expired, revoked
    /// or without the needed scope.
    #[error("the token was rejected: {0}")]
    TokenRejected(String),
    /// No such repository or pull request, or no access to it.
    #[error("{0}")]
    NotFound(String),
    /// The host could not be reached.
    #[error("{0}")]
    Network(String),
    #[error("{0}")]
    Other(String),
}
