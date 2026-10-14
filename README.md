<img src="src-tauri/icons/128x128@2x.png" width="96" alt="Arnold icon: an H drawn as a git graph">

# Arnold

Desktop app for reviewing a git branch before it becomes a pull request, built for checking what AI agents wrote. The diff is the editor: you fix things where you see them.

- **PR-style comparison:** merge-base of the base branch → the working tree of the reviewed branch, including uncommitted and untracked files. Works with branches checked out in git worktrees.
- **Files dock:** every file in the diff, with status and line counts.
- **One file or all files:** review one file at a time, or every file of the diff in one scrolling list, as in a pull request's "all files" view. Each file has a header that stays in view while you scroll through it, with its counts, *Save* and *Reset*; click it to fold the file away. Clicking a file in the dock scrolls to it, and the dock follows as you scroll. Files load as they come near the screen, so large diffs stay fast.
- **Editable diff:** side-by-side or unified; the diff updates as you type. Drag the divider between the sides to resize them (double-click to make them equal again).
- **Reset to base:** selected lines (`Alt+R`, or `R` on the base side), one change (the arrow between the panes, or the *Reset* button in unified mode), or a whole file (`R` in the files dock).
- **Syntax highlighting:** F#, C#, project files, JSON, Markdown.
- **F# language server in the diff:** errors, hover, completion, signature help, go-to-definition (`F12` or Ctrl/Cmd+click; *Back* returns), find references, rename. Error counts for every changed F# file show in the files dock.

Arnold never stages, commits or checks out. It reads git state and writes the files you edit or reset.

Not there yet: C# language server, a problems list with messages, settings.

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
| `npm run dev` | UI alone in a browser at <http://localhost:1420>, against an in-memory stand-in backend (`src/ipc/devMock.ts`) |
| `npm test` | Frontend unit tests |
| `cargo test --workspace` | Backend tests (create temporary git repositories) |
| `cargo test -p arnold-lsp --test fsautocomplete_live -- --ignored --nocapture` | Downloads and runs the real FsAutoComplete on a small project |
| `src-tauri/make-icons.sh` | Regenerates `src-tauri/icons/` from the icon source, `src-tauri/app-icon.svg` |

`zed-trial/setup-trial-repo.sh` creates a small F# repository with a feature branch, uncommitted and untracked changes and a linked worktree — handy for trying the app.

## Layout

| Path | Contents |
|---|---|
| `src/` | UI: SolidJS + CodeMirror 6 (`@codemirror/merge`) |
| `src-tauri/` | Tauri shell: commands, file I/O, file watcher |
| `src-tauri/installer/` | What the installers add: the `arnold` and `ace` commands and the PATH setup |
| `crates/arnold-git/` | Read-only git access through the `git` CLI |
| `crates/arnold-lsp/` | Language servers: install/update, and the proxy between editor and server |
