//! Read-only access to a git repository through the `git` CLI.
//!
//! Nothing here stages, commits or checks out: the app only reads git state.
//! The one thing written is a copy of a commit's files in a folder of the
//! app's own (`export_commit`), which the repository knows nothing about.

mod cli;
mod compare;
mod export;
mod files;
mod refs;

pub use cli::GitError;
pub use compare::{compare, read_blob, ChangedFile, Commit, CommitRange, Comparison, FileStatus};
pub use export::export_commit;
pub use files::{list_files, tree_files, TreeFile};
pub use refs::{common_dir, open, Branch, RepoInfo};

pub type Result<T> = std::result::Result<T, GitError>;
