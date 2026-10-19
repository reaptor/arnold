# arnoldapp.dev

The website for Arnold: one static page, built with Astro (no UI framework) and served by
`httpd` in Docker behind Traefik on the Hetzner server, like reaptor.com and kontera.app.

```bash
npm install
npm run dev      # http://localhost:4321
npm run build    # static output in ./dist
npm run preview  # serve the built site
```

## Structure

- `src/pages/index.astro`: the page, copy included.
- `src/layouts/Base.astro`: `<head>`, Open Graph tags, fonts.
- `src/styles/site.css`: Rider's *Islands Dark* colors, as in the app.
- The screenshots (`../docs/screenshots`) and the icon (`../src-tauri/app-icon.svg`) are imported
  from the app's side of the repository, so there is one copy of each; Astro resizes the
  screenshots to WebP at build time. Retake them with `npm run screenshots` in the repository root.

## Downloads

The download buttons link to the latest release of
[reaptor/arnold](https://github.com/reaptor/arnold/releases); a small script asks the GitHub API
for that release and swaps in the direct `.pkg` / `-setup.exe` / `.msi` links and the version
number. Without it (rate limited, blocked) the buttons still work.

## Deploying

Pushing to `main` with changes under `site/`, `docs/screenshots/` or the icon runs
[`.github/workflows/site.yml`](../.github/workflows/site.yml): it builds `Dockerfile` with the
repository root as the context (`Dockerfile.dockerignore` keeps that to what the site uses),
publishes `ghcr.io/reaptor/arnoldapp` as `latest` and `sha-<short>`, and asks the server's
watchtower to pull it. Site-only pushes publish no app release.

One-time setup:

1. **DNS:** `A` (and `AAAA`) records for `arnoldapp.dev` and `www.arnoldapp.dev` pointing at the
   server. `.dev` is on the HSTS preload list, so browsers only ever use HTTPS; Traefik gets the
   certificate from Let's Encrypt (`myresolver`) on the first request.
2. **Secret:** `WATCHTOWER_HTTP_API_TOKEN` in this repository's Actions secrets (the same value as
   in the reaptorcom repository and the server's `watchtower/.env`).
3. **Registry access:** after the first run, either make the `arnoldapp` package public on GitHub,
   or rely on the server's existing `docker login ghcr.io`, which watchtower already uses.
4. **Server:** copy `docker-compose.yml` to the server (e.g. `~/arnoldapp/`) and run

   ```bash
   docker compose up -d
   ```

`www.arnoldapp.dev` redirects to `arnoldapp.dev`. Set `DOCKER_TAG` to pin a `sha-…` tag instead
of following `latest`.

To build and run the image locally (from this folder):

```bash
npm run docker:build
```

```bash
docker run --rm -p 8088:80 ghcr.io/reaptor/arnoldapp:latest
```
