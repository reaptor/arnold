#!/bin/bash
# The setup step of the agent workflows (.github/workflows/claude*.yml) on GitHub's
# ubuntu-latest runners, which already have node, Rust and Google Chrome: the Linux
# libraries Tauri's crates build against, then the npm dependencies.
set -euo pipefail

sudo apt-get update -qq
sudo apt-get install -y -qq libwebkit2gtk-4.1-dev libayatana-appindicator3-dev librsvg2-dev \
  libxdo-dev libssl-dev patchelf build-essential >/dev/null
npm ci --no-audit --no-fund
