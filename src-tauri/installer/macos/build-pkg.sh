#!/bin/sh
# Builds the macOS installer: Arnold.app goes into /Applications, and the `arnold` and `ace`
# commands into /usr/local/bin (see pkg-scripts/postinstall).
#
#   build-pkg.sh path/to/Arnold.app version output.pkg
set -eu

app=$1
version=$2
output=$3

here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mkdir "$work/root"
ditto "$app" "$work/root/Arnold.app"

# By default the installer looks for a copy of the app anywhere on the disk and replaces that
# one (a build folder, say) instead of installing into /Applications.
pkgbuild --analyze --root "$work/root" "$work/components.plist"
/usr/libexec/PlistBuddy -c "Set :0:BundleIsRelocatable false" "$work/components.plist"

pkgbuild \
  --root "$work/root" \
  --component-plist "$work/components.plist" \
  --install-location /Applications \
  --scripts "$here/pkg-scripts" \
  --identifier app.arnold.review.pkg \
  --version "$version" \
  "$output"
