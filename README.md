<img src="src-tauri/icons/128x128@2x.png" width="96" alt="Arnold icon: an H drawn as a git graph">

# Arnold

**Review what your AI agents wrote before it gets merged.**

Arnold is a desktop app for macOS and Windows for reviewing a git branch the way you review a pull request: every change against the branch it goes into, one file at a time or all at once, with an F# language server in the diff. Once a branch is reviewed, Arnold creates the pull request for colleagues, and Azure DevOps pull requests can be reviewed from start to finish, including commenting, voting and completing. Arnold never changes your working copy; fixes are made elsewhere.

![Arnold reviewing an Azure DevOps pull request: the files dock on the left, the side-by-side diff with discussions on the right, and Approve and Complete in the toolbar](docs/screenshots/review.png)

## Highlights

- **A pull request's diff for any branch:** merge-base → the branch's last commit, the way pull requests show changes. It updates on its own when the branch moves.
- **Built for big reviews:** every changed file in one scrolling list, a file explorer next to it, and `F7` to step from change to change across files.
- **Azure DevOps pull requests:** create one from a branch you reviewed, or pick one from the active ones, read its discussions in the diff, comment on lines or files, approve and complete.
- **F# language features in the diff:** errors, hover, go to definition, find references and Rider's colors, from [FsAutoComplete](https://github.com/ionide/FsAutoComplete), which Arnold installs and updates itself.
- **Looks like Rider:** JetBrains Rider's *Islands Dark* theme, with the toolbar as the title bar.
- **Read-only by design:** Arnold never edits, commits or checks out; it pushes only the reviewed commit when you create a pull request.

**Contents:** [Install](#install) · [Getting started](#getting-started) · [Reviewing a branch](#reviewing-a-branch) · [Azure DevOps pull requests](#azure-devops-pull-requests) · [F# language features](#f-language-features) · [Keyboard shortcuts](#keyboard-shortcuts) · [What Arnold writes](#what-arnold-writes) · [Status](#status) · [License](#license) · [Development](#development)

## Install

Download the installer from [Releases](https://github.com/reaptor/arnold/releases): the `.pkg` for macOS (Apple silicon), the `-setup.exe` or `.msi` for Windows (x64). Arnold needs `git` on the PATH, and the .NET SDK (8 or newer) for F# language features.

The builds are not signed with a developer certificate yet:

- **macOS:** try to open the `.pkg` once, then choose *Open Anyway* under *System Settings → Privacy & Security*.
- **Windows:** choose *More info → Run anyway* in the SmartScreen prompt.

### Start from a terminal

The installers put two commands on the PATH, `arnold` and the shorter `ace`:

```bash
arnold path/to/repository
```

```bash
ace .
```

Without a folder they open the current one. Terminals that were open during the installation have to be reopened first. Started from the Dock or the Start menu instead, Arnold reopens the last repository.

| | Where the commands are | Uninstalling |
|---|---|---|
| macOS | links in `/usr/local/bin` to a script inside `Arnold.app` | delete `/Applications/Arnold.app`, `/usr/local/bin/arnold` and `/usr/local/bin/ace` |
| Windows | `bin` inside the installation folder, which is added to PATH (the user's for the `-setup.exe`, the machine's for the `.msi`) | the uninstaller takes the folder off PATH again |

## Getting started

1. **Open a repository:** with `arnold <folder>`, or the repository button at the left of the toolbar. Star a repository there to keep it in your favorites for switching between them.
2. **Choose what to review:**
   - *Branches*: the branch to review, and the base branch it goes into.
   - *Pull request*: shown when a remote of the repository is on Azure DevOps. Pick one of its active pull requests (see [Azure DevOps pull requests](#azure-devops-pull-requests)).
3. **Review:** go through the files in the dock or the all-files list, and step through the changes with `F7`.

A repository opens on the branch checked out in its folder, against the base branch you picked last (or its default branch). Arnold puts the window back where it was.

## Reviewing a branch

### What is compared

Arnold compares the merge-base of the two branches with the last commit of the reviewed branch, as a pull request does. Only commits count: uncommitted and untracked files in any checkout are left out. A new commit or a fetch updates the diff and the branch pickers on their own.

The commit picker narrows the diff to one or more consecutive commits of the branch, as in an Azure DevOps pull request. Tick commits to add them; click a row to see only that commit.

![The commit picker, listing the branch's commits with checkboxes](docs/screenshots/commits.png)

### The files dock

The left side has two parts:

- **Changes:** every file in the diff, with its status (A, M, D, R) and line counts.
- **All files:** a file explorer with every file of the reviewed commit. Changed files are marked with their status and folders holding changes with a dot. Files outside the diff open without a diff, for reading around.

The filter above them applies to both. Drag the gap between them to resize, click a heading to fold a part away, and press `Ctrl+B` to hide or show the whole dock.

### One file or all files

The toolbar switches between one file at a time and *All files*: every file of the diff in one scrolling list, as in a pull request's "all files" view. *All files* is where new reviews start. In it:

- each file has a header that stays in view while you scroll through it; click it to fold the file;
- clicking a file in the dock scrolls to it, and the dock follows as you scroll;
- deleted files start folded;
- files load as they come near the screen, so large diffs stay fast.

### The diff

- Side-by-side or unified, read-only. Drag the divider between the sides to resize them; double-click it to make them equal again.
- Compared line by line first, so small edits far apart stay small. Changed words are marked red and green, as in Azure DevOps.
- Added and deleted files show whole; deleted ones are tinted red.
- `F7` / `Shift+F7` step through the changes; in the all-files view they carry on into the next or previous file.
- Syntax highlighting for F#, C#, T-SQL, project and other XML files, JSON and Markdown.

## Azure DevOps pull requests

When a remote of the repository is on Azure DevOps (dev.azure.com, *.visualstudio.com or Azure DevOps Server, over HTTPS or SSH), the toolbar gets a *Branches | Pull request* switch.

### Connecting

Arnold needs a personal access token. Listing pull requests needs the **Code (Read)** scope; creating pull requests (and pushing their branch), commenting, voting and completing need **Code (Read & write)**. Arnold shows these steps itself when it finds an Azure DevOps remote and has no token:

1. In Azure DevOps, open *User settings → Personal access tokens* (`https://dev.azure.com/<organization>/_usersSettings/tokens`) and select *New Token*.
2. Pick the organization and an expiry date, choose *Custom defined* scopes and tick *Code → Read & write* (or *Read* only, to only read). Create it and copy the token.
3. In Arnold, switch to *Pull request*, paste the token and select *Connect*.

Arnold checks the token with Azure DevOps before saving it, one per organization, in the macOS Keychain or Windows Credential Manager. Without a saved token it uses the `AZURE_DEVOPS_EXT_PAT` environment variable (the Azure CLI's). The token goes only to the remote's Azure DevOps host: to read and change pull requests and to fetch their commits. The key button in the pull request list changes or removes it. HTTPS certificates are checked against the system's certificate store, so company proxies and internal certificate authorities work as they do for git.

### Creating a pull request

Once a branch is reviewed (an AI agent's, say), *Create pull request* in the toolbar opens one on Azure DevOps from the branch to review into the base branch, for colleagues to review. The title and description start out as Azure DevOps fills them in (the commit's subject for one commit; otherwise the branch name, with the commits listed), the description is Markdown with a preview, and *Create as a draft* is remembered.

When Azure DevOps does not have the branch yet, or has an older commit of it, Arnold first pushes the commit you reviewed, not whatever the branch points at by now, and says so in the dialog. It never forces: if the branch on Azure DevOps has commits you have not reviewed, nothing is pushed or created. The push goes to the remote's HTTPS address with the token and moves the remote-tracking branch (`origin/<branch>`) along, as `git push` would; the local branch and its upstream setting are left alone. When a pull request between the two branches is open already, the dialog offers to review it instead. The new pull request opens in Arnold.

### Picking a pull request

The list shows the active pull requests with their descriptions, branches, author, votes, drafts and an *Approved* tag; type to find one by title, number, branch or author.

![The list of active pull requests, with descriptions, votes, an Approved tag and a draft](docs/screenshots/pull-requests.png)

The one you pick is compared the way Azure DevOps does it: the merge-base of its target and source → the source branch's last commit, with the same commit picker. Commits the repository lacks are fetched from Azure DevOps into its object store only; no branch, remote-tracking branch or `FETCH_HEAD` changes. Arnold checks the pull request for new pushes, votes and discussions every minute while its window is shown, and when you come back to it.

### Discussions and comments

Discussions show as cards under the lines they are about, with resolved ones folded and the unchanged lines around them unfolded. Discussions about a whole file sit at the top of its diff. The comment button in the toolbar lists every discussion, including those about the pull request as a whole, and jumps to the others.

To comment, right-click a line or a selection and choose *Add comment*, or right-click a file under *Changes* to comment on the whole file. Every discussion has *Reply*, and the toolbar list takes comments on the pull request as a whole. Comments are Azure DevOps Markdown, with a preview: emoji shortcodes, `#123` work items, `!123` pull requests, suggestion blocks and task lists. `Ctrl+Enter` (`⌘+Enter`) sends.

![A new comment being written under a changed line, next to an existing discussion](docs/screenshots/comment.png)

Discussions show with all changes only: a commit picked in the commit picker has other line numbers.

### Voting and completing

As in Azure DevOps, the toolbar has *Approve* and *Complete*:

- **Approve** votes in one click. Its menu has *Approve with suggestions*, *Wait for author*, *Reject* and *Reset feedback*, and the button then shows your vote.
- **Complete** opens a dialog with the merge type (merge, squash, rebase and fast-forward, semi-linear), the commit message, and whether to complete the linked work items and delete the source branch. It warns about merge conflicts and reviewers who rejected or are waiting for the author. Arnold completes only the source commit you were shown, so a push you have not seen is refused.
- Complete's menu has **Mark as draft** and **Abandon**. A draft shows *Publish* instead of *Complete*, an abandoned pull request *Reactivate*.

![The Approve menu: Approve, Approve with suggestions, Wait for author, Reject, Reset feedback](docs/screenshots/vote.png)

![The Complete pull request dialog: merge type, commit message and what to do after merging](docs/screenshots/complete.png)

## F# language features

In F# files the diff has errors and warnings, hover, go to definition (`F12`, `Ctrl`/`Cmd`+click or the context menu; *Back* returns), find references (`Shift+F12`), semantic colors as in Rider, and unused `open`s faded out. The file headers of the all-files view count the errors of every changed F# file.

The language server needs the reviewed code on disk with its packages restored. For the branch checked out in the opened folder it runs right there; for any other branch or a pull request Arnold prepares a copy. While it does, the review goes on as usual; the status bar shows how far it has come, and its popup lists each step, problems, the output of `dotnet` and the build log of a project that failed. If something fails, the review goes on without language features, or with gaps.

![The F# language features popup: copying the commit, restoring, building, starting FsAutoComplete and loading projects](docs/screenshots/language.png)

How it works:

- Arnold uses [FsAutoComplete](https://github.com/ionide/FsAutoComplete) (Apache-2.0), which is not bundled. The first time an F# file is shown, Arnold downloads the latest release from nuget.org into its own data folder, checks it against the SHA-512 hash NuGet publishes, and starts it. Nothing is installed globally.
- After that it starts the installed copy straight away and, at most once a day, looks for a newer release in the background; a new one is used from the next start. Old versions are removed.
- It needs the .NET SDK (8 or newer). The SDK the repository selects (`global.json`) decides which build of the server runs.
- The server runs in the opened folder when the reviewed branch is checked out there, else in a copy of the reviewed commit in `.arnold/worktrees/<branch>`. Checking out another branch in the folder moves it to a copy. Arnold does only what FsAutoComplete needs there: `dotnet tool restore` and `dotnet restore` when project or package files changed since the last restore in that folder, and `dotnet build` only for projects in other languages (such as C#) that F# projects reference, when their files changed. F# projects themselves are never built. A new commit updates the copy in place, so restore and build output are reused.
- The server stops when Arnold closes or you pick another branch.

## Keyboard shortcuts

| Keys | What they do |
|---|---|
| `F7` / `Shift+F7` | Next / previous change (across files in the all-files view) |
| `F12`, `Ctrl`/`Cmd`+click | Go to definition (F#) |
| `Shift+F12` | Find references (F#) |
| `Ctrl+-` (macOS), `Alt+←` (Windows) | Back to where you jumped from |
| `Ctrl+B` (on macOS also `Cmd+B`) | Hide or show the files dock |
| `Ctrl+Enter` (`⌘+Enter`) | Send a comment |
| `Esc` | Close a menu, popup or dialog; cancel a comment |

## What Arnold writes

Arnold never edits, resets, stages, commits or checks out. It writes only:

- the objects of a pull request it fetched;
- when you create a pull request from a branch Azure DevOps does not have (or has an older commit of): the reviewed commit, pushed to that branch without forcing, and the remote-tracking branch moved to it;
- for the language server, when the reviewed branch is not the one checked out in the folder: a copy of the reviewed commit in `.arnold/worktrees/<branch>` inside the repository folder (like Claude Code's `.claude/worktrees`; a `.gitignore` there keeps it out of `git status`, and it is not a registered git worktree);
- when the reviewed branch is checked out in the folder: the output of `dotnet restore` (and of building referenced non-F# projects, when needed) in its `obj` and `bin` folders, as building it yourself would;
- to Azure DevOps, when you ask it to: new pull requests, comments, votes, and completing, abandoning, drafting or publishing a pull request.

## Status

Arnold is young and changes often; every push to `main` is released. It is used mostly on macOS; the Windows build is released alongside it. The builds are not signed yet ([`Signing.md`](Signing.md) has the plan).

Not there yet: C# language server, a problems list with messages, settings, GitHub pull requests, and in Azure DevOps: auto-complete, resolving or editing discussions, and adding reviewers.

## License

Arnold is released under the [MIT License](LICENSE). FsAutoComplete, which Arnold downloads, is under its own Apache-2.0 license.

## The name

Arnold is named after Arnold Rimmer, the hologram in the TV show *Red Dwarf*. People who describe his personality tend to reach for the word *git*, British slang for an unpleasant, annoying person, which is also where git itself got its name. This app works with git, so it is named after one.

The icon is the **H** Rimmer wears on his forehead to mark him as a hologram, drawn as a git graph: two branches joined by a crossbar, with a commit at each end. The red and green commits are the removed and added lines of a diff.

## Development

Arnold is built with [Tauri 2](https://tauri.app) (Rust), SolidJS and CodeMirror 6.

### Requirements

- `git` on PATH
- Rust (1.90+) and Node.js

### Run

```bash
npm install
```

```bash
npm run tauri dev -- -- path/to/repository
```

The path is optional; without it Arnold reopens the last repository, or use *Open repository…*. Either way it starts on the branch checked out there.

Build the app, and on Windows its installers:

```bash
npm run tauri build
```

On macOS the installer is a separate step:

```bash
src-tauri/installer/macos/build-pkg.sh target/release/bundle/macos/Arnold.app 0.1.0 Arnold.pkg
```

### Releases

Every push to `main` is built by [`.github/workflows/release.yml`](.github/workflows/release.yml) for macOS (Apple silicon) and Windows (x64) and published under [Releases](https://github.com/reaptor/arnold/releases).

The version goes up with every commit: `<major>.<minor>` comes from `src-tauri/tauri.conf.json` and the patch number is the number of commits on `main`, so the 57th commit is released as `v0.1.57`. To start a new minor or major version, change the version in `src-tauri/tauri.conf.json`. Local builds keep the version written in that file.

### Commands

| Command | What it does |
|---|---|
| `npm run dev` | UI alone in a browser at <http://localhost:1420>, against an in-memory stand-in backend (`src/ipc/devMock.ts`); see the options below |
| `npm test` | Frontend unit tests |
| `cargo test --workspace` | Backend tests (create temporary git repositories) |
| `cargo test -p arnold-lsp --test fsautocomplete_live -- --ignored --nocapture` | Downloads and runs the real FsAutoComplete on a small project |
| `cargo test -p arnold-pr -- --ignored` | Checks that dev.azure.com turns down a bogus token |
| `npm run screenshots` | Retakes the screenshots in `docs/screenshots/` from `npm run dev` (needs Google Chrome); `npm run screenshots -- review` takes one |
| `src-tauri/make-icons.sh` | Regenerates `src-tauri/icons/` from the icon source, `src-tauri/app-icon.svg` |

Options of the stand-in backend, added to the URL (`?devops&lsp`):

| Option | What it does |
|---|---|
| `devops` | An Azure DevOps remote with a saved token and four pull requests (!1270 has merge conflicts); `=nopat` without a token (pasting `bad` is rejected), `=rejected`, `=empty` |
| `readonlytoken` | Writing to pull requests fails, as with a Code (Read) token |
| `serverbranch` | For *Create pull request* from `feature/agent-work-wt` (not on the server otherwise): `=behind` one commit behind, `=ahead` with a commit that was not reviewed. `feature/agent-work` has !1287 open already |
| `lsp` | A stand-in language server that answers go-to-definition and reports unused `open`s; `projectfail` makes a project fail to load |
| `setupslow`, `setupfail` | Preparing the language server's copy is slow, or fails |
| `files=40` | Adds that many changed files, for a large diff |
| `platform=windows` | Shows the Windows title bar |

`zed-trial/setup-trial-repo.sh` creates a small F# repository with a feature branch and a linked worktree — handy for trying the app (its uncommitted and untracked changes do not show: Arnold compares commits only).

### Layout

| Path | Contents |
|---|---|
| `src/` | UI: SolidJS + CodeMirror 6 (`@codemirror/merge`) |
| `src-tauri/` | Tauri shell: commands, file decoding, branch watcher, preparing the language server's copy |
| `src-tauri/installer/` | What the installers add: the `arnold` and `ace` commands and the PATH setup |
| `crates/arnold-git/` | Read-only git access through the `git` CLI, and copying a commit's files out |
| `crates/arnold-pr/` | Pull requests of code hosts: Azure DevOps remotes, its REST API, tokens in the system credential store |
| `crates/arnold-lsp/` | Language servers: install/update, the minimal restore/build plan, and the proxy between editor and server |
| `scripts/` | Taking the README's screenshots |
| `docs/screenshots/` | The README's screenshots |
