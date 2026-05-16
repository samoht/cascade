#!/usr/bin/env bash
# Refresh traces/css-syntax from the Web Platform Tests repository at
# the pinned commit.
#
# The "upstream tool" here is the WPT corpus itself: a curated set of
# CSS Syntax L3 parser-conformance vectors maintained by browser
# implementers. There is no binary to run; the corpus is the oracle.
# This script clones WPT at the pinned commit and copies the
# [css/css-syntax/] subtree into ../traces/css-syntax/.
#
# [@charset] with legacy encodings is intentionally dropped - Cascade is
# UTF-8 only and does not implement the CSS byte-stream decoder
# algorithm.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TRACE_DIR="$(cd "$SCRIPT_DIR/../traces" && pwd)"

WPT_REPO="https://github.com/web-platform-tests/wpt.git"
WPT_COMMIT="f900489fca393464f3379d7952d227997318b851"
WPT_PATH="css/css-syntax"

command -v git >/dev/null || { echo "git not on PATH" >&2; exit 1; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "Cloning $WPT_REPO @ $WPT_COMMIT (sparse: $WPT_PATH) ..."
git -C "$tmp" init -q
git -C "$tmp" remote add origin "$WPT_REPO"
git -C "$tmp" config core.sparseCheckout true
echo "$WPT_PATH/*" > "$tmp/.git/info/sparse-checkout"
git -C "$tmp" fetch --depth=1 origin "$WPT_COMMIT" -q
git -C "$tmp" checkout -q FETCH_HEAD

rm -rf "$TRACE_DIR/css-syntax"
mkdir -p "$TRACE_DIR"
cp -R "$tmp/$WPT_PATH" "$TRACE_DIR/css-syntax"
rm -rf "$TRACE_DIR/css-syntax/charset"
