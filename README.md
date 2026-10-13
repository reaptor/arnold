# Arnold

Desktop app for reviewing a git branch before it becomes a pull request, built for checking what AI agents wrote. The diff is the editor: you fix things where you see them.

- **PR-style comparison:** merge-base of the base branch → the working tree of the reviewed branch, including uncommitted and untracked files. Works with branches checked out in git worktrees.
- **Files dock:** every file in the diff, with status and line counts.
- **Editable diff:** side-by-side or unified; the diff updates as you type.
- **Reset to base:** selected lines (`Alt+R`, or `R` on the base side), one change (the arrow between the panes, or the *Reset* button in unified mode), or a whole file (`R` in the files dock).
- **Syntax highlighting:** F#, C#, project files, JSON, Markdown.
- **F# language server in the diff:** errors, hover, completion, signature help, go-to-definition (`F12` or Ctrl/Cmd+click; *Back* returns), find references, rename. Error counts for every changed F# file show in the files dock.

Arnold never stages, commits or checks out. It reads git state and writes the files you edit or reset.

Not there yet: C# language server, a problems list with messages, settings.

## F# language server

Arnold uses [FsAutoComplete](https://github.com/ionide/FsAutoComplete) (Apache-2.0). It is not bundled:

- The first time an F# file is shown, Arnold downloads the latest release from nuget.org into its own data folder, checks it against the SHA-512 hash NuGet publishes, and starts it. Nothing is installed globally.
- After that it starts the installed copy straight away and, at most once a day, looks for a newer release in the background; a new one is used from the next start. Old versions are removed.
- It needs the .NET SDK (8 or newer) on the machine. The SDK the repository selects (`global.json`) decides which build of the server runs.
- The server runs in the checkout being reviewed, so errors reflect that code, and it stops when Arnold closes or you switch branch.

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

Build an installable app:

```bash
npm run tauri build
```

## Releases

Every push to `main` is built by [`.github/workflows/release.yml`](.github/workflows/release.yml) for macOS (Apple silicon) and Windows (x64) and published under [Releases](https://github.com/reaptor/arnold/releases).

The version goes up with every commit: `<major>.<minor>` comes from `src-tauri/tauri.conf.json` and the patch number is the number of commits on `main`, so the 57th commit is released as `v0.1.57`. To start a new minor or major version, change the version in `src-tauri/tauri.conf.json`. Local builds keep the version written in that file.

The builds are not signed with a developer certificate. On macOS, try to open the app once, then choose *Open Anyway* under *System Settings → Privacy & Security* (or run `xattr -dr com.apple.quarantine /Applications/Arnold.app`); on Windows, choose *More info → Run anyway* in the SmartScreen prompt.

## Develop

| Command | What it does |
|---|---|
| `npm run dev` | UI alone in a browser at <http://localhost:1420>, against an in-memory stand-in backend (`src/ipc/devMock.ts`) |
| `npm test` | Frontend unit tests |
| `cargo test --workspace` | Backend tests (create temporary git repositories) |
| `cargo test -p arnold-lsp --test fsautocomplete_live -- --ignored --nocapture` | Downloads and runs the real FsAutoComplete on a small project |

`zed-trial/setup-trial-repo.sh` creates a small F# repository with a feature branch, uncommitted and untracked changes and a linked worktree — handy for trying the app.

## Layout

| Path | Contents |
|---|---|
| `src/` | UI: SolidJS + CodeMirror 6 (`@codemirror/merge`) |
| `src-tauri/` | Tauri shell: commands, file I/O, file watcher |
| `crates/arnold-git/` | Read-only git access through the `git` CLI |
| `crates/arnold-lsp/` | Language servers: install/update, and the proxy between editor and server |
