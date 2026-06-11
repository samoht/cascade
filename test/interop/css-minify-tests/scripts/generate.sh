#!/bin/bash
# Refresh traces/tests from keithamus/css-minify-tests at the pinned commit.
#
# The upstream "tool" here is a hand-curated corpus of source.css /
# expected.css pairs maintained upstream. There is no binary to run: the
# corpus itself is the imported oracle. This script clones the corpus at a
# pinned commit and copies the
# pair files (and per-test README.md) into ../traces/tests/.
set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel 2>/dev/null || true)"
if [ -n "$repo_root" ] && [[ "$script_dir" == "$repo_root"/_build/default/* ]]; then
  SCRIPT_DIR="$repo_root/${script_dir#"$repo_root/_build/default/"}"
else
  SCRIPT_DIR="$script_dir"
fi
TRACE_DIR_ARG="${1:-$SCRIPT_DIR/../traces}"
mkdir -p "$TRACE_DIR_ARG"
TRACE_DIR="$(cd "$TRACE_DIR_ARG" && pwd)"

CMT_REPO=https://github.com/keithamus/css-minify-tests.git
CMT_COMMIT=83f224fbf9db27a81f398c1425e6000f22b4b5c9

command -v git >/dev/null || { echo "git not on PATH" >&2; exit 1; }
command -v rsync >/dev/null || { echo "rsync not on PATH" >&2; exit 1; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "Cloning $CMT_REPO @ $CMT_COMMIT ..."
git -C "$tmp" init -q
git -C "$tmp" remote add origin "$CMT_REPO"
git -C "$tmp" fetch --depth=1 origin "$CMT_COMMIT" -q
git -C "$tmp" checkout -q FETCH_HEAD

rm -rf "$TRACE_DIR/tests"
mkdir -p "$TRACE_DIR/tests"
rsync -a \
  --include='*/' \
  --include='source.css' \
  --include='expected.css' \
  --include='README.md' \
  --exclude='*' \
  "$tmp/tests/" "$TRACE_DIR/tests/"

cat > "$TRACE_DIR/UPSTREAM.md" <<EOF
Snapshot of <https://github.com/keithamus/css-minify-tests>.

- Commit: \`$CMT_COMMIT\`
- License: MIT (per upstream README)
- Vendored files: \`source.css\`, \`expected.css\`, per-test \`README.md\`.

Regenerate with \`dune build @regen-traces\` from the package root.
EOF

echo "Wrote $(find "$TRACE_DIR/tests" -name source.css | wc -l | tr -d ' ') test pairs."
