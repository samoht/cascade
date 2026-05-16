#!/usr/bin/env bash
# Refresh traces/ from the SatCSS benchmark corpus.
#
# Upstream: https://github.com/matthewhague/sat-css-tool (Hague, Lin, Hong;
# "CSS Minification via Constraint Solving", TOPLAS 2019). The benchmarks
# directory holds 75 real-world stylesheets (amazon, google, github, ...)
# each saved as [<site>-stripmq.css] plus six minifier outputs from
# [cleancss], [cssmin], [cssnano], [csso], [minify], and [yui].
#
# What this script writes
# -----------------------
# traces/inputs/<site>-stripmq.css   - raw upstream input, unchanged
# traces/oracles/<site>-<tool>.css   - cascade-canonical form of each oracle,
#                                      precomputed via [cascade fmt --minify]
#                                      so test-time is a plain string compare
# traces/UPSTREAM-NOTES.md           - upstream attribution
#
# Why traces/ is NOT committed to git
# -----------------------------------
# The upstream repository carries no LICENSE file, and the CSS itself remains
# the copyright of each originating website (Amazon, Google, etc.). Vendoring
# the corpus into a public Cascade checkout would redistribute third-party
# CSS without a clear permission. Pulling it locally at regen-time and using
# it as a research/interop oracle is fair use; the traces/ directory is
# therefore gitignored.
#
# Running [dune build @regen-traces] fetches the corpus into traces/ so the
# [(test ...)] stanza can run. Fresh checkouts produce zero alcotest cases
# until regen has been invoked once.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TRACE_DIR="$(cd "$SCRIPT_DIR/../traces" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

SATCSS_REPO="https://github.com/matthewhague/sat-css-tool.git"
SATCSS_COMMIT="1d983625032b224e80f7659e32285801167e16e0"

command -v git >/dev/null || { echo "git not on PATH" >&2; exit 1; }

# We need cascade's CLI to canonicalize oracles. Build it from this checkout
# so the canonical form matches the cascade currently under test.
echo "Building cascade CLI ..."
(cd "$REPO_ROOT" && dune build bin)
CASCADE_BIN="$REPO_ROOT/_build/default/bin/main.exe"
[ -x "$CASCADE_BIN" ] || { echo "cascade binary not found at $CASCADE_BIN" >&2; exit 1; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "Cloning $SATCSS_REPO @ $SATCSS_COMMIT ..."
git -C "$tmp" init -q
git -C "$tmp" remote add origin "$SATCSS_REPO"
git -C "$tmp" config core.sparseCheckout true
{ echo "benchmarks/*"; echo "NOTES.md"; } > "$tmp/.git/info/sparse-checkout"
git -C "$tmp" fetch --depth=1 origin "$SATCSS_COMMIT" -q
git -C "$tmp" checkout -q FETCH_HEAD

find "$TRACE_DIR" -mindepth 1 -not -name '.gitignore' -delete
mkdir -p "$TRACE_DIR/inputs" "$TRACE_DIR/oracles"
cp "$tmp/NOTES.md" "$TRACE_DIR/UPSTREAM-NOTES.md"

shopt -s nullglob
inputs=0
oracles=0
upstream_empty=0
for src in "$tmp"/benchmarks/*-stripmq.css; do
  base=$(basename "$src" -stripmq.css)
  cp "$src" "$TRACE_DIR/inputs/$base-stripmq.css"
  inputs=$((inputs + 1))
done
err_log="$tmp/cascade-err"
for src in "$tmp"/benchmarks/*-stripmq-*.css; do
  fname=$(basename "$src" .css)
  # fname is "<site>-stripmq-<tool>"; the last segment is the tool name.
  tool="${fname##*-}"
  site="${fname%-stripmq-$tool}"
  out="$TRACE_DIR/oracles/$site-$tool.css"
  if [ ! -s "$src" ]; then
    # Upstream minifier produced no output for this site; legitimate
    # skip - the corresponding oracle is intentionally absent.
    upstream_empty=$((upstream_empty + 1))
    continue
  fi
  if ! "$CASCADE_BIN" fmt --minify "$src" > "$out" 2>"$err_log"; then
    echo "ERROR: cascade failed to parse $(basename "$src"):" >&2
    cat "$err_log" >&2
    rm -f "$out"
    exit 1
  fi
  if [ ! -s "$out" ]; then
    echo "ERROR: cascade emitted empty output for non-empty $(basename "$src")" >&2
    rm -f "$out"
    exit 1
  fi
  oracles=$((oracles + 1))
done

echo "wrote $inputs inputs and $oracles canonicalised oracles"
echo "$upstream_empty oracle slots empty in upstream corpus (legitimate skip)"
