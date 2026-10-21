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

use serde::{Deserialize, Serialize};

/// A pull request, as far as Arnold needs one.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct PullRequest {
    pub id: u64,
    pub title: String,
    pub description: String,
    /// Display name of whoever opened it.
    pub author: String,
    /// The host's id of whoever opened it.
    pub author_id: String,
    /// Opened by the owner of the token: whoever votes from Arnold.
    pub yours: bool,
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
    /// How merging the source into the target went when the host last tried:
    /// `succeeded`, `conflicts`, `rejectedByPolicy`, `failure`, `queued` or `notSet`.
    pub merge_status: String,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Reviewer {
    /// The host's id of the person or group.
    pub id: String,
    pub name: String,
    pub vote: Vote,
    /// A group (team) rather than a person.
    pub group: bool,
    /// The owner of the token: whoever votes from Arnold.
    pub you: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum Vote {
    Approved,
    ApprovedWithSuggestions,
    /// No vote yet, or the vote was reset.
    None,
    WaitingForAuthor,
    Rejected,
}

/// A pull request to open, from a branch that is on the host already.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct NewPullRequest {
    /// Branch names without `refs/heads/`.
    pub source_branch: String,
    pub target_branch: String,
    pub title: String,
    /// Markdown.
    pub description: String,
    pub draft: bool,
    /// Work items to link to it, by id.
    #[serde(default)]
    pub work_items: Vec<u64>,
}

/// A work item, to show which one a new pull request is linked to.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct WorkItem {
    pub id: u64,
    pub title: String,
    /// `Bug`, `User Story`, `Task`, ...
    pub kind: String,
    /// `New`, `Active`, ...
    pub state: String,
    /// Its page on the host.
    pub url: String,
}

/// Looking a work item up by id.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum WorkItemLookup {
    Found { item: WorkItem },
    /// No such work item, or none the token's owner can see.
    NotFound,
    /// The token may not read work items (it lacks Work Items → Read); a
    /// pull request can still be linked to it by id.
    NoAccess,
}

/// A change to a pull request as a whole.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum PrChange {
    /// Merges it into the target branch.
    Complete(Completion),
    /// Closes it without merging.
    Abandon,
    /// Opens an abandoned one again.
    Reactivate,
    /// Marks it as a draft (`true`) or publishes it for review (`false`).
    Draft { draft: bool },
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct Completion {
    /// The source commit that was reviewed: the host refuses to complete if
    /// the branch has moved on since.
    pub source_commit: String,
    pub merge_strategy: MergeStrategy,
    pub message: String,
    pub delete_source_branch: bool,
    /// Moves the linked work items to their next state.
    pub complete_work_items: bool,
}

/// How a completed pull request is merged; the names are Azure DevOps' own.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum MergeStrategy {
    /// A merge commit.
    NoFastForward,
    /// One new commit with all of the changes.
    Squash,
    /// The source commits rebased onto the target, which then fast-forwards.
    Rebase,
    /// The source commits rebased onto the target, then a merge commit.
    RebaseMerge,
}

impl MergeStrategy {
    /// Every one, in the order Azure DevOps lists them.
    pub const ALL: [MergeStrategy; 4] = [Self::NoFastForward, Self::Squash, Self::Rebase, Self::RebaseMerge];
}

/// What the branch policies of a pull request's target branch allow when it
/// is completed.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct CompletionPolicies {
    /// The merge types allowed, in `MergeStrategy::ALL`'s order; all of them
    /// unless a policy limits them.
    pub merge_strategies: Vec<MergeStrategy>,
    /// Required policies the pull request does not meet yet: the host does
    /// not complete it until they are.
    pub unmet: Vec<UnmetPolicy>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct UnmetPolicy {
    pub name: String,
    /// `queued`, `running`, `rejected` or `broken`, as the host has it.
    pub status: String,
    /// Met by reviewers approving: minimum number of reviewers, required reviewers.
    pub approval: bool,
}

/// A discussion on a pull request: about lines of a file, or about the pull
/// request as a whole.
#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Thread {
    pub id: u64,
    /// The file, from the repository's root (no leading slash); `None` for
    /// the pull request as a whole.
    pub path: Option<String>,
    /// Where in the file, as of the pull request's latest push; `None` for a
    /// thread about the whole file (or one whose lines the host lost track
    /// of), and for the pull request as a whole.
    pub position: Option<ThreadPosition>,
    /// `active`, `pending`, `fixed`, `wontFix`, `closed`, `byDesign` or `unknown`.
    pub status: String,
    /// What people wrote, oldest first; the host's own notes are left out.
    pub comments: Vec<Comment>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ThreadPosition {
    pub side: Side,
    /// 1-based, both included.
    pub start_line: u32,
    pub end_line: u32,
}

/// Which side of the diff a thread is on.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum Side {
    /// The merge-base: lines the pull request removes or leaves alone.
    Base,
    /// The source branch.
    Head,
}

/// Where a new thread goes: a file, and lines of it unless it is about the
/// whole file.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct NewThreadPlace {
    pub path: String,
    pub lines: Option<LineRange>,
}

/// Lines and 1-based character offsets, the end offset just past the last character.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct LineRange {
    pub side: Side,
    pub start_line: u32,
    pub start_offset: u32,
    pub end_line: u32,
    pub end_offset: u32,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Comment {
    pub id: u64,
    /// The comment this one answers; 0 for none.
    pub parent_id: u64,
    pub author: String,
    /// The host's id of the author, for telling the token's owner's comments apart.
    #[serde(skip)]
    pub author_id: String,
    /// Written by the token's owner, who may delete it.
    pub you: bool,
    /// Markdown.
    pub content: String,
    /// ISO 8601.
    pub published: String,
    pub edited: bool,
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
