# Zed trial for F# branch review

Goal: find out in about ten minutes whether Zed already does what the planned "Arnold" app would do — review an agent's branch against its base, edit in the diff, and get F# errors and go-to-definition there.

## What is in this folder

| Path | What it is |
|---|---|
| `repo/` | Git repo with a small F# app, checked out on `feature/agent-work`. Has committed, uncommitted and untracked changes, and one planted compiler error. |
| `repo-worktree/` | Linked git worktree on `feature/agent-work-wt` (same commit). The planted error is fixed here and a different one is planted instead. |
| `setup-trial-repo.sh` | Recreates both folders from scratch. Run it to reset after you have edited things. |

`main` has one commit made after the feature branch split off (it edits `Program.fs` and adds `MAIN-ONLY.md`). A PR-style diff must not show either.

## Setup

1. **Install Zed** from <https://zed.dev/download>, or:

   ```bash
   brew install --cask zed
   ```

2. **Add the F# extension.** Open the extensions panel (`cmd-shift-x`), search "F#", install. Or put this in Zed's settings (`cmd-,` → *Edit in settings.json*):

   ```json
   {
     "auto_install_extensions": { "fsharp": true },
     "diff_view_style": "split"
   }
   ```

3. **Nothing else to install.** The extension downloads FsAutoComplete from NuGet itself and runs it with the `dotnet` on your PATH (confirmed from its source; .NET 10.0.203 is present here). The first start takes a little while.

4. **Open the trial repo:** *File → Open* and pick `~/Projects/arnold/zed-trial/repo`.

## Checklist

Actions are run from the command palette (`cmd-shift-p`).

| # | Do this | Pass looks like |
|---|---|---|
| 1 | Run `git: diff branch` (older name: `git: branch diff`) | A tab titled "Changes since main" listing `Orders.fs`, `Pricing.fs`, `ZedTrial.fsproj`. **Not** `Program.fs` or `MAIN-ONLY.md`. |
| 2 | Look in the same diff for the uncommitted and untracked work | `Pricing.fs` shows `0.10m // UNCOMMITTED edit`; `ZedTrial.fsproj` shows the added `Audit.fs` line; `Audit.fs` itself is listed as a new file. |
| 3 | Type in the right-hand side of the diff, save (`cmd-s`), undo. Then set `"diff_view_style": "unified"` and repeat | The file on disk changes; the diff updates as you type; both layouts are editable. |
| 4 | Find `let maxLinesPerOrder: int = "ten"` (`Orders.fs` line 20) in the diff | Red squiggle, and hovering shows error FS0001 (expected `int`, got `string`). |
| 5 | In the diff, press `F12` (or `cmd`-click) on `applyLoyaltyDiscount`, then on `Currency.format`. Press `ctrl--` to go back | First jump lands in `Pricing.fs`; second lands in `Currency.fs`, a file that is not in the diff. Back returns to the diff. |
| 6 | Open `repo-worktree` (*File → Open*, or `git: worktree`) and run `git: diff branch` there | `Orders.fs` line 20 has **no** error (it is `= 10` here). `Pricing.fs` line 13 has one (`"5%"` where a `decimal` is expected). This shows errors come from the worktree's own code. |
| 7 | Open a real project, e.g. `~/Projects/FSharpLint`, and work in it for a few minutes | Your judgement of F# highlighting, hover, completion and responsiveness. |
| 8 | With Zed still open, measure memory (below) | Your judgement; for reference, a webview app would idle at roughly 100–250 MB before the language server. |

Memory, in MB per process:

```bash
ps -axo rss=,command= | grep -iE "zed|fsautocomplete" | grep -v grep | awk '{printf "%7.0f MB  %s\n", $1/1024, $2}'
```

### Useful extras

- `git: compare with branch` — pick any base branch instead of the default one.
- `git: toggle diff base` — makes the plain `git: diff` command (`ctrl-g d`) show branch changes instead of uncommitted changes. Same as `"git": { "diff_base": "default_branch" }` in settings.
- `git: review diff` — hands the branch diff to an agent thread for review.
- If the F# server does not start, look for a language-server log command in the palette (search "language server logs").

## What is and is not confirmed

Confirmed from Zed's source and the F# extension's source (2026-10-13):

- The branch diff is taken from the merge-base with the default branch.
- Diff views are editable multibuffers; split is the default layout.
- The F# extension provides tree-sitter highlighting and FsAutoComplete, and fetches the server itself.

**Not confirmed — these are what the checklist is for:**

- Checks 4 and 5: diagnostics and go-to-definition *inside* the diff.
- Check 2: whether untracked files appear in the branch diff.
- Check 7: how solid the F# extension (v0.0.12, one maintainer) is in daily use.

None of this was run in Zed while writing the guide; Zed was not installed on this machine.

### Known gap: no "reset selected lines" in the branch diff

Read from Zed's source (main branch, 2026-10-13), not tried:

- In the **branch diff**, `git: restore` does nothing and the per-hunk buttons are hidden. Zed's own test dispatches the action there and asserts the diff is unchanged. To undo part of the agent's work you edit the text by hand.
- In the **uncommitted-changes diff** (`git: diff head`), `git: restore` (`cmd-alt-z`, Windows `ctrl-k ctrl-r`) reverts the hunks your selection touches, and `git: restore file` reverts whole files. It works on whole hunks, not on the exact lines selected, and only back to the last commit, not to the branch base.

Worth trying as check 9: put the cursor on a change in the branch diff and run `git: restore`.

## Results

| # | Pass / fail | Notes |
|---|---|---|
| 1 | | |
| 2 | | |
| 3 | | |
| 4 | | |
| 5 | | |
| 6 | | |
| 7 | | |
| 8 | | |

- **All pass:** use Zed; nothing needs building.
- **Some fail:** those gaps are the (much smaller) tool worth building.
- **4 or 5 fail outright:** fall back to the shelved Tauri + CodeMirror plan.

## Reset

```bash
~/Projects/arnold/zed-trial/setup-trial-repo.sh
```
