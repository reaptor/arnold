//! Read-only access to a git repository through the `git` CLI.
//!
//! Nothing here stages, commits or checks out: the app only reads git state.
//! The things written are a copy of a commit's files in the app's own ignored
//! `.arnold` folder in the checkout (`app_folder`, `export_commit`), which git
//! does not track or register as a worktree, and the
//! objects of a pull request fetched into the object store (`fetch_objects`),
//! which no branch or ref points at. The one thing that leaves the machine
//! is `push_commit`, when a pull request is created from a reviewed branch
//! that is not on the server yet; it never forces.

mod cli;
mod compare;
mod export;
mod files;
mod refs;
mod push;
mod remotes;

pub use cli::GitError;
pub use compare::{compare, read_blob, ChangedFile, Commit, CommitRange, Comparison, FileStatus};
pub use export::{app_folder, export_commit};
pub use files::{list_files, tree_files, TreeFile};
pub use refs::{common_dir, list_branches, open, Branch, RepoInfo};
pub use push::{count_commits, is_ancestor, push_commit, set_remote_tracking};
pub use remotes::{fetch_objects, has_commits, remotes, Remote};

pub type Result<T> = std::result::Result<T, GitError>;
