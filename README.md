<img src="src-tauri/icons/128x128@2x.png" width="96" alt="Arnold icon: an H drawn as a git graph">

# Arnold

Desktop app for reviewing a git branch before it becomes a pull request, built for checking what AI agents wrote. Arnold only reads: the diff is read-only, and fixes are made elsewhere.

- **PR-style comparison:** merge-base of the base branch → the last commit of the reviewed branch. Only commits count: uncommitted and untracked files in any checkout are left out. The diff and the branch pickers refresh on their own when a branch moves or appears (a new commit, a fetch).
- **Azure DevOps pull requests:** when a remote of the repository is on Azure DevOps (dev.azure.com, *.visualstudio.com or Azure DevOps Server, HTTPS or SSH), the toolbar gets a *Branches | Pull request* switch. *Pull request* lists the active pull requests (title, number, branches, author, votes, drafts) and shows the one you pick the way Azure DevOps does: the merge-base of its target and source → the source branch's last commit, with the same commit picker. Commits the repository lacks are fetched from Azure DevOps into the object store only (no branch, remote-tracking branch or `FETCH_HEAD` changes). Arnold remembers per repository which way you reviewed and which pull request. Its discussions show as cards under the lines they are about (all changes only; the commit picker's subsets have other line numbers), with resolved ones folded and the lines between unfolded; the comment button in the toolbar lists every discussion, including those about the pull request as a whole, and jumps to file ones. Discussions are read-only: reply in Azure DevOps. Arnold checks the open pull request for new pushes every minute while its window is shown, and when you come back to it; the reload button in the pull request list reloads the list and checks right away. See [Azure DevOps](#azure-devops) for the token it needs.
- **Files dock:** on top, *Changes*: every file in the diff, with status and line counts. Below it, *All files*: a file explorer with every file in the last commit of the branch under review, changed files marked with their status and folders holding changes with a dot. Files outside the diff open on their own, without a diff. Drag the line between the two to resize them; click a heading to fold that part away. The filter applies to both.
- **One file or all files:** review one file at a time, or every file of the diff in one scrolling list, as in a pull request's "all files" view. Each file has a header that stays in view while you scroll through it, with its counts; click it to fold the file away. Clicking a file in the dock scrolls to it, and the dock follows as you scroll. Files load as they come near the screen, so large diffs stay fast.
- **Read-only diff:** side-by-side or unified. `F7` / `Shift+F7` step through the changes (in the all-files view they carry on into the next or previous expanded file). Drag the divider between the sides to resize them (double-click to make them equal again).
- **Syntax highlighting:** F#, C#, project files, JSON, Markdown.
- **F# language server in the diff:** errors, hover, go-to-definition (`F12` or Ctrl/Cmd+click; *Back* returns), find references. Error counts for every changed F# file show in the file headers of the all-files view. When a branch is selected, Arnold copies its last commit to a folder of its own under the system temp folder (`arnold/<repository>/<branch>`; not a git worktree, the repository knows nothing of it) and does only what FsAutoComplete needs there: `dotnet tool restore` and `dotnet restore` when project or package files changed since the last restore in that folder, and `dotnet build` only for projects in other languages (such as C#) that F# projects reference, when their files changed. F# projects themselves are never built. The status bar shows each step, and its popup lists the steps, problems and the output of `dotnet`; the diff can be used all along, and language features switch on once the server has loaded the projects. If something fails, the review goes on without them (or with gaps). A new commit on the branch updates the copy in place, so restore and build output are reused.

Arnold never writes to the repository: it does not edit, reset, stage, commit, push or check out. The only copy of the code it writes is for the language server, in `.arnold/worktrees/<branch>` inside the repository folder (like Claude Code's `.claude/worktrees`; a `.gitignore` there keeps it out of `git status`, and it is not a registered git worktree); the only thing it adds to the repository is the objects of a pull request it fetched (see above).

Not there yet: C# language server, a problems list with messages, settings, GitHub pull requests.

## Azure DevOps

Listing pull requests needs a personal access token with the **Code (Read)** scope. Arnold shows these steps itself when it finds an Azure DevOps remote and has no token:

1. In Azure DevOps, open *User settings → Personal access tokens* (`https://dev.azure.com/<organization>/_usersSettings/tokens`) and select *New Token*.
2. Pick the organization and an expiry date, choose *Custom defined* scopes and tick *Code → Read*. Create it and copy the token.
3. In Arnold, switch to *Pull request*, paste the token and select *Connect*.

Arnold checks the token with Azure DevOps before saving it, one per organization, in the macOS Keychain or Windows Credential Manager. Without a saved token it uses the `AZURE_DEVOPS_EXT_PAT` environment variable (the Azure CLI's). The token goes only to the remote's Azure DevOps host: to list pull requests and to fetch their commits. The key button in the pull request list changes or removes it.

## The name

Arnold is named after Arnold Rimmer, the hologram in the TV show *Red Dwarf*. People who describe his personality tend to reach for the word *git*, British slang for an unpleasant, annoying person, which is also where git itself got its name. This app works with git, so it is named after one.

The icon is the **H** Rimmer wears on his forehead to mark him as a hologram, drawn as a git graph: two branches joined by a crossbar, with a commit at each end. The red and green commits are the removed and added lines of a diff.

## F# language server

Arnold uses [FsAutoComplete](https://github.com/ionide/FsAutoComplete) (Apache-2.0). It is not bundled:

- The first time an F# file is shown, Arnold downloads the latest release from nuget.org into its own data folder, checks it against the SHA-512 hash NuGet publishes, and starts it. Nothing is installed globally.
- After that it starts the installed copy straight away and, at most once a day, looks for a newer release in the background; a new one is used from the next start. Old versions are removed.
- It needs the .NET SDK (8 or newer) on the machine. The SDK the repository selects (`global.json`) decides which build of the server runs.
- The server runs in the checkout being reviewed, so errors reflect that code, and it stops when Arnold closes or you switch branch.

## Install

Download the installer from [Releases](https://github.com/reaptor/arnold/releases): the `.pkg` for macOS (Apple silicon), the `-setup.exe` or `.msi` for Windows (x64). The builds are not signed with a developer certificate:

- **macOS:** try to open the `.pkg` once, then choose *Open Anyway* under *System Settings → Privacy & Security*.
- **Windows:** choose *More info → Run anyway* in the SmartScreen prompt.

## Start from a terminal

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

## Requirements

- `git` on PATH
- To build: Rust (1.90+) and Node.js

## Run

```bash
npm install
```

```bash
npm run tauri dev -- -- path/to/repository
```

The path is optional; without it Arnold reopens the last repository, or use *Open repository…*.

Build the app, and on Windows its installers:

```bash
npm run tauri build
```

On macOS the installer is a separate step:

```bash
src-tauri/installer/macos/build-pkg.sh target/release/bundle/macos/Arnold.app 0.1.0 Arnold.pkg
```

## Releases

Every push to `main` is built by [`.github/workflows/release.yml`](.github/workflows/release.yml) for macOS (Apple silicon) and Windows (x64) and published under [Releases](https://github.com/reaptor/arnold/releases).

The version goes up with every commit: `<major>.<minor>` comes from `src-tauri/tauri.conf.json` and the patch number is the number of commits on `main`, so the 57th commit is released as `v0.1.57`. To start a new minor or major version, change the version in `src-tauri/tauri.conf.json`. Local builds keep the version written in that file.

## Develop

| Command | What it does |
|---|---|
| `npm run dev` | UI alone in a browser at <http://localhost:1420>, against an in-memory stand-in backend (`src/ipc/devMock.ts`); add `?devops` for an Azure DevOps remote (`=nopat` without a token, where pasting `bad` is rejected; `=rejected`; `=empty`) |
| `npm test` | Frontend unit tests |
| `cargo test --workspace` | Backend tests (create temporary git repositories) |
| `cargo test -p arnold-lsp --test fsautocomplete_live -- --ignored --nocapture` | Downloads and runs the real FsAutoComplete on a small project |
| `cargo test -p arnold-pr -- --ignored` | Checks that dev.azure.com turns down a bogus token |
| `src-tauri/make-icons.sh` | Regenerates `src-tauri/icons/` from the icon source, `src-tauri/app-icon.svg` |

`zed-trial/setup-trial-repo.sh` creates a small F# repository with a feature branch, uncommitted and untracked changes and a linked worktree — handy for trying the app.

## Layout

| Path | Contents |
|---|---|
| `src/` | UI: SolidJS + CodeMirror 6 (`@codemirror/merge`) |
| `src-tauri/` | Tauri shell: commands, file decoding, branch watcher, preparing the language server's copy |
| `src-tauri/installer/` | What the installers add: the `arnold` and `ace` commands and the PATH setup |
| `crates/arnold-git/` | Read-only git access through the `git` CLI, and copying a commit's files out |
| `crates/arnold-pr/` | Pull requests of code hosts: Azure DevOps remotes, its REST API, tokens in the system credential store |
| `crates/arnold-lsp/` | Language servers: install/update, the minimal restore/build plan, and the proxy between editor and server |
