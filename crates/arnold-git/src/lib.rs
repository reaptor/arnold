//! Read-only access to a git repository through the `git` CLI.
//!
//! Nothing here stages, commits or checks out: the app only reads git state.

mod cli;
mod compare;
mod refs;

pub use cli::GitError;
pub use compare::{compare, read_blob, ChangedFile, Comparison, FileStatus};
pub use refs::{open, Branch, RepoInfo};

pub type Result<T> = std::result::Result<T, GitError>;
