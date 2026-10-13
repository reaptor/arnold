#!/bin/sh
# Regenerates src-tauri/icons from src-tauri/app-icon.svg.
# The macOS icon is drawn at 824 of 1024 pixels, the margin Apple's icon grid expects;
# every other icon fills its canvas.
set -eu
cd "$(dirname "$0")"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

npx tauri icon app-icon.svg -o icons
rm -rf icons/android icons/ios

sed -e 's|viewBox="0 0 1024 1024">|&<g transform="translate(100 100) scale(0.8046875)">|' \
    -e 's|</svg>|</g></svg>|' app-icon.svg > "$tmp/macos.svg"
npx tauri icon "$tmp/macos.svg" -o "$tmp/macos"
cp "$tmp/macos/icon.icns" icons/icon.icns
