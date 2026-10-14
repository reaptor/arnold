//! Access to a git repository through the `git` CLI.
//!
//! Writes happen only on request, in the checkout under review: a linked
//! worktree for a branch that is not checked out anywhere (`checkout`, so no
//! existing checkout ever switches branch), and commits and pushes (`commit`).

mod checkout;
mod cli;
mod commit;
mod compare;
mod files;
mod refs;

pub use checkout::ensure_checkout;
pub use cli::GitError;
pub use commit::{checkout_status, commit, push, CheckoutStatus, PushTarget, Uncommitted};
pub use compare::{compare, compare_in, read_blob, ChangedFile, Comparison, FileStatus};
pub use files::list_files;
pub use refs::{open, Branch, RepoInfo};

pub type Result<T> = std::result::Result<T, GitError>;
