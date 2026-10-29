# Arnold

Arnold (reaptor/arnold) is a **public** repository: a Tauri desktop app (Rust in
`src-tauri/` and `crates/`, a TypeScript/Vite UI in `src/`). **Every push to `main`
publishes a public release** (`.github/workflows/release.yml`).

You run on a fresh GitHub-hosted ubuntu runner. The setup step (`.github/claude/setup.sh`)
has already installed Tauri's Linux libraries and run `npm ci` on main; Rust, node and
Google Chrome come with the runner. Everything you write on GitHub is public: never post
logs, environment variables or anything from the runner beyond what the change needs.

## The code

- Read `README.md` and the code around the change first; match the existing style, naming
  and comment density; keep changes as small as the request allows.
- On a PR branch, or after merging main: run `npm ci` again if `package-lock.json` differs.

## Checks before every commit

`npm test`, `npx tsc --noEmit`, and for Rust changes `cargo test --workspace` (a cold build
takes several minutes: run it in the background with a long timeout). Fix what your change
broke.

## Screenshots

The user wants them in the PR whenever the change is visible in the UI (a new or changed
panel, button, list, status bar, editor decoration, dialog…). Skip them for changes with
nothing to see (backend/Rust logic, refactors, tests, build files).

- Start the browser preview in the background: `npx vite --port 15000 --strictPort`. It runs
  the real UI against the in-memory stand-in backend in `src/ipc/devMock.ts`; URL query
  switches pick the scenario (`?files=40`, `?lsp`, `?uncommitted`, `?devops=…`, see
  devMock.ts and `scripts/screenshots.mjs`). If the feature needs data the mock does not
  have, extend devMock.ts as part of the change.
- Take the shots with Playwright like `scripts/screenshots.mjs` (`playwright-core`,
  `chromium.launch({ channel: "chrome" })`, viewport 1440×860, `deviceScaleFactor: 2`, dark
  color scheme, wait for the UI to settle, click to reach the state). Write the script in
  `$RUNNER_TEMP`, not in the repo. 1–3 images; look at each one and retake any that is
  blank, cut off or in the wrong state. Stop the preview afterwards.
- Never commit screenshots to the PR branch. Publish them on the orphan branch
  `pr-screenshots/issue-N`, from a temporary worktree:
  ```
  tmp=$(mktemp -d "$RUNNER_TEMP/shots.XXXX") && git worktree add --orphan -b pr-screenshots/issue-N "$tmp"
  cp <images> "$tmp"/ && git -C "$tmp" add . && git -C "$tmp" commit -m "Screenshots for #N"
  git -C "$tmp" push -u origin pr-screenshots/issue-N && git worktree remove "$tmp"
  ```
  If the branch already exists, `git fetch origin pr-screenshots/issue-N` and add it with
  `git worktree add "$tmp" pr-screenshots/issue-N` instead; use new file names for new
  states. `.github/workflows/pr-screenshots.yml` deletes the branch when the PR closes.
- Link images as
  `![<what it shows>](https://raw.githubusercontent.com/reaptor/arnold/pr-screenshots/issue-N/<file>.png)`.

## Pull requests

Arnold has no CI on pull requests, so there are no checks to wait for. The security review
reads every PR, and its findings come back to you as a "fix review findings" task.
