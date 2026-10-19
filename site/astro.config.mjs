import { defineConfig } from 'astro/config';

// Static site — outputs to ./dist on `npm run build`.
export default defineConfig({
  site: 'https://arnoldapp.dev',
  vite: {
    // The screenshots (docs/screenshots) and the icon (src-tauri/app-icon.svg) are
    // imported from the app's side of the repository, so there is one copy of each.
    server: { fs: { allow: ['..'] } },
  },
});
