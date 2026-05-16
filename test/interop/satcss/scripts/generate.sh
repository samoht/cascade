#!/usr/bin/env bash
# Refresh traces/cases.trace from the SatCSS benchmark corpus.
#
# Upstream: https://github.com/matthewhague/sat-css-tool (Hague, Lin, Hong;
# "CSS Minification via Constraint Solving", TOPLAS 2019). The benchmarks
# directory holds 75 real-world stylesheets (amazon, google, github, ...)
# each saved as [<site>-stripmq.css] plus six minifier outputs from
# [cleancss], [cssmin], [cssnano], [csso], [minify], and [yui].
#
# This script writes a single binary trace [traces/cases.trace] consumed
# by the shared interop runner [test/interop/test.exe]. Format:
#
#     "CASCADE-INTEROP/v1\n"
#     ">>> <name_len> <input_len> <n_ok> <n_err>\n"
#     "<name bytes>\n"           e.g. "benchmarks/amazon"
#     "<input bytes>\n"
#     ok_oracle*                  "OK <tool_len> <css_len>\n<tool><css>\n"
#     err_oracle*                 "FAIL <tool_len> <reason_len>\n<tool><reason>\n"
#
# Why traces/ is gitignored
# -------------------------
# The upstream repository carries no LICENSE file and the CSS itself
# remains the copyright of each originating website. Vendoring the corpus
# into a public Cascade checkout would redistribute third-party CSS
# without a clear permission. Pulling it locally at regen-time and using
# it as a research/interop oracle is fair use; the trace file is
# therefore gitignored.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TRACE_DIR="$(cd "$SCRIPT_DIR/../traces" && pwd)"

SATCSS_REPO="https://github.com/matthewhague/sat-css-tool.git"
SATCSS_COMMIT="1d983625032b224e80f7659e32285801167e16e0"

command -v git >/dev/null || { echo "git not on PATH" >&2; exit 1; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "Cloning $SATCSS_REPO @ $SATCSS_COMMIT ..."
git -C "$tmp" init -q
git -C "$tmp" remote add origin "$SATCSS_REPO"
git -C "$tmp" config core.sparseCheckout true
echo "benchmarks/*" > "$tmp/.git/info/sparse-checkout"
git -C "$tmp" fetch --depth=1 origin "$SATCSS_COMMIT" -q
git -C "$tmp" checkout -q FETCH_HEAD

trace="$TRACE_DIR/cases.trace"
find "$TRACE_DIR" -mindepth 1 -not -name '.gitignore' -delete
printf 'CASCADE-INTEROP/v1\n' > "$trace"

shopt -s nullglob
inputs=0
oks_total=0
errs_total=0
exec 3>> "$trace"
for src in "$tmp"/benchmarks/*-stripmq.css; do
  site=$(basename "$src" -stripmq.css)
  name="benchmarks/$site"
  input_bytes=$(wc -c < "$src" | tr -d ' ')
  # Count OK / FAIL slots first so the record header carries the right counts.
  n_ok=0
  n_err=0
  for tool in cleancss cssmin cssnano csso minify yui; do
    oracle="$tmp/benchmarks/${site}-stripmq-${tool}.css"
    if [ -s "$oracle" ]; then
      n_ok=$((n_ok + 1))
    else
      n_err=$((n_err + 1))
    fi
  done
  name_len=${#name}
  printf '>>> %d %d %d %d\n%s\n' \
    "$name_len" "$input_bytes" "$n_ok" "$n_err" "$name" >&3
  cat "$src" >&3
  printf '\n' >&3
  for tool in cleancss cssmin cssnano csso minify yui; do
    oracle="$tmp/benchmarks/${site}-stripmq-${tool}.css"
    [ -s "$oracle" ] || continue
    css_bytes=$(wc -c < "$oracle" | tr -d ' ')
    t_len=${#tool}
    printf 'OK %d %d\n%s' "$t_len" "$css_bytes" "$tool" >&3
    cat "$oracle" >&3
    printf '\n' >&3
  done
  for tool in cleancss cssmin cssnano csso minify yui; do
    oracle="$tmp/benchmarks/${site}-stripmq-${tool}.css"
    [ -s "$oracle" ] && continue
    reason="upstream produced no output"
    r_len=${#reason}
    t_len=${#tool}
    printf 'FAIL %d %d\n%s%s\n' "$t_len" "$r_len" "$tool" "$reason" >&3
  done
  inputs=$((inputs + 1))
  oks_total=$((oks_total + n_ok))
  errs_total=$((errs_total + n_err))
done
exec 3>&-

echo "wrote $inputs records ($oks_total OK oracles, $errs_total upstream failures)"
