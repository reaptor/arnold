//! Access to a git repository through the `git` CLI.
//!
//! Reviewing only reads git state. The things written are a copy of a commit's files in the app's own ignored
//! `.arnold` folder in the checkout (`app_folder`, `export_commit`), which git
//! does not track or register as a worktree, and the
//! objects of a pull request fetched into the object store (`fetch_objects`),
//! which no branch or ref points at. The one thing that leaves the machine
//! is `push_commit`, when a pull request is created from a reviewed branch
//! that is not on the server yet; it never forces.
//!
//! `branch_checkout` and `pull_request_checkout` check a branch or a pull
//! request checked out nowhere out, so it can be edited (and the language
//! server run on its files): in the opened folder by default, or, to leave
//! the uncommitted changes there untouched, in a registered worktree in
//! `.arnold/worktrees` (`branch_worktree`, `detached_worktree`).
//!
//! A comparison whose branch is checked out reads that checkout's files, so
//! they can be edited (saved through `working`).
//!
//! `working` is the exception: it stages, unstages, puts files back and
//! commits in the checkout it is given, for committing an agent's changes
//! before they are reviewed; `publish` pushes the branch checked out there,
//! when asked to, never forced, and `pull` fetches from the remotes and
//! brings branches up to them when asked to.

mod cli;
mod compare;
mod context;
mod export;
mod files;
mod patch;
mod refs;
mod publish;
mod pull;
mod push;
mod remotes;
mod working;
mod worktree;

pub use cli::GitError;
pub use compare::{compare, read_blob, ChangedFile, Commit, CommitRange, Comparison, FileStatus};
pub use context::{context_files, ContextFile, ContextLimits};
pub use export::{app_folder, export_commit};
pub use files::{list_files, tree_files, TreeFile};
pub use patch::{patch, OmitReason, Omitted, Patch, PatchSource};
pub use refs::{common_dir, git_dir, list_branches, open, Branch, RepoInfo};
pub use publish::{branch_sync, push_branch, BranchSync};
pub use pull::{fast_forward_branches, fetch_remote, pull_branch, Pulled};
pub use push::{count_commits, is_ancestor, push_commit, set_remote_tracking};
pub use remotes::{fetch_objects, has_commits, remotes, Remote};
pub use worktree::{
    branch_checkout, branch_worktree, detached_worktree, find_worktree, pull_request_checkout, BranchCheckout, CheckedOut, Placement, Worktree,
};
pub use working::{
    commit, delete_files, discard, encode_like, ignore, ignored_paths, is_tracked, on_disk, read_index, read_worktree, reset_to_head, set_index_text, stage, start_branch, undo_edits, unstage, untrack,
    working_status, worktree_files, write_worktree, Ignored, NewCommit, WorkingStatus,
};

pub type Result<T> = std::result::Result<T, GitError>;
