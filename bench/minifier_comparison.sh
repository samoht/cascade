#!/usr/bin/env bash
# Cross-minifier wall-clock comparison on real-world stylesheets.
#
# Each run reads from a file argument (the way each tool's CLI prefers). We
# report emitted raw/gzip/brotli bytes, plus median wall-clock time over N runs.
#
# Tools compared (all on PATH):
#   cascade fmt --minify  (Cascade, OCaml)
#   csso                  (Yandex, JS)
#   lightningcss --minify (Parcel, Rust)
#   esbuild --minify      (Evan Wallace, Go)
#   cssnano               (PostCSS, JS)
#
# Corpus: stripped real-world stylesheets from the SatCSS benchmark
# (Hague, Lin, Hong; TOPLAS 2019), regenerated under
# test/interop/satcss/scripts/.tool/benchmarks/. The CSS is third-party
# website content and is not redistributed by Cascade.
#
# Usage:
#   bench/minifier_comparison.sh                    # default: 4 fixtures, 3 runs
#   bench/minifier_comparison.sh -n 5 site1 site2   # custom run count and fixture list
#
# Site names omit the -stripmq.css suffix (e.g. github, guardian, youtube).

set -euo pipefail

RUNS=${RUNS:-3}
CORPUS="${CORPUS:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/test/interop/satcss/scripts/.tool/benchmarks}"
CASCADE="${CASCADE:-cascade}"
DEFAULT_SITES=(github guardian youtube netflix)

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//' >&2
  exit 1
}

while getopts ":n:c:h" opt; do
  case "$opt" in
    n) RUNS="$OPTARG" ;;
    c) CASCADE="$OPTARG" ;;
    h|*) usage ;;
  esac
done
shift $((OPTIND - 1))

SITES=("$@")
[ ${#SITES[@]} -eq 0 ] && SITES=("${DEFAULT_SITES[@]}")

if [ ! -d "$CORPUS" ]; then
  echo "error: corpus not found: $CORPUS" >&2
  echo "  hint: run 'dune build @regen-traces' under test/interop/satcss first" >&2
  exit 2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

median() { printf '%s\n' "$@" | sort -n | awk -v c=$# 'NR==int((c+1)/2){print; exit}'; }

bytes() { wc -c <"$1" | awk '{print $1}'; }
gzip_bytes() { gzip -9 -c "$1" | wc -c | awk '{print $1}'; }
brotli_bytes() {
  if command -v brotli >/dev/null 2>&1; then
    brotli -q 11 -c "$1" | wc -c | awk '{print $1}'
  else
    printf '-'
  fi
}

run_ms() {
  # $1: command to run with input redirected from $2, output to /dev/null
  local cmd="$1" in="$2"
  local times=()
  for _ in $(seq 1 "$RUNS"); do
    local t
    t=$( { /usr/bin/time -p sh -c "$cmd <\"$in\" >\"$TMP/o.css\" 2>/dev/null"; } 2>&1 \
         | awk '/^real/{printf "%d", $2*1000}')
    times+=("$t")
  done
  median "${times[@]}"
}

emit_stdout() {
  # $1: command to run with input redirected from $2, output to $3
  local cmd="$1" in="$2" out="$3"
  sh -c "$cmd <\"$in\" >\"$out\" 2>/dev/null"
}

# The `cssnano` binary on PATH is cssnano-cli, which pins cssnano 3.10.0 (2017)
# and is not the minifier anyone ships today. Drive the real plugin through
# postcss-cli instead.
emit_cssnano() {
  local in="$1" out="$2"
  postcss "$in" --use cssnano --no-map -o "$out" >/dev/null 2>/dev/null
}

run_cssnano_ms() {
  local in="$1"
  local times=()
  for _ in $(seq 1 "$RUNS"); do
    local t
    t=$( { /usr/bin/time -p postcss "$in" --use cssnano --no-map -o "$TMP/out.css" >/dev/null; } 2>&1 \
         | awk '/^real/{printf "%d", $2*1000}')
    times+=("$t")
  done
  median "${times[@]}"
}

print_tool_row() {
  local site="$1" tool="$2" time="$3" out="$4"
  printf '%-12s %-12s %8s %10s %10s %10s\n' \
    "$site" "$tool" "${time}ms" "$(bytes "$out")" "$(gzip_bytes "$out")" \
    "$(brotli_bytes "$out")"
}

printf '%-12s %-12s %8s %10s %10s %10s\n' \
  file tool time raw gzip brotli
printf '%-12s %-12s %8s %10s %10s %10s\n' \
  '----' '----' '----' '---' '----' '------'

print_input_row() {
  # Reference baseline so the [raw] column makes the actual byte reduction
  # obvious -- a fast tool that emits the input unchanged would have the
  # same [raw] as this row.
  local site="$1" in="$2"
  printf '%-12s %-12s %8s %10s %10s %10s\n' \
    "$site" '(input)' '-' "$(bytes "$in")" "$(gzip_bytes "$in")" \
    "$(brotli_bytes "$in")"
}

for site in "${SITES[@]}"; do
  in="$CORPUS/${site}-stripmq.css"
  if [ ! -f "$in" ]; then
    printf '%-12s (missing %s)\n' "$site" "$(basename "$in")"
    continue
  fi
  out_cascade="$TMP/$site.cascade.css"
  out_cascade_aggr="$TMP/$site.cascade_aggr.css"
  out_csso="$TMP/$site.csso.css"
  out_lightning="$TMP/$site.lightning.css"
  out_esbuild="$TMP/$site.esbuild.css"
  out_cssnano="$TMP/$site.cssnano.css"

  emit_stdout "$CASCADE fmt --minify -" "$in" "$out_cascade"
  emit_stdout "$CASCADE fmt --minify --aggressive -" "$in" "$out_cascade_aggr"
  emit_stdout "csso" "$in" "$out_csso"
  emit_stdout "lightningcss --minify" "$in" "$out_lightning"
  emit_stdout "esbuild --minify --loader=css" "$in" "$out_esbuild"
  emit_cssnano "$in" "$out_cssnano"

  t_cascade=$(run_ms "$CASCADE fmt --minify -" "$in")
  t_cascade_aggr=$(run_ms "$CASCADE fmt --minify --aggressive -" "$in")
  t_csso=$(run_ms "csso" "$in")
  t_lightning=$(run_ms "lightningcss --minify" "$in")
  t_esbuild=$(run_ms "esbuild --minify --loader=css" "$in")
  t_cssnano=$(run_cssnano_ms "$in")

  print_input_row "$site" "$in"
  print_tool_row "$site" cascade "$t_cascade" "$out_cascade"
  print_tool_row "$site" cascade-aggr "$t_cascade_aggr" "$out_cascade_aggr"
  print_tool_row "$site" csso "$t_csso" "$out_csso"
  print_tool_row "$site" lightningcss "$t_lightning" "$out_lightning"
  print_tool_row "$site" esbuild "$t_esbuild" "$out_esbuild"
  print_tool_row "$site" cssnano "$t_cssnano" "$out_cssnano"
done

echo
echo "Notes: median of ${RUNS} runs; wall clock; sizes are emitted bytes."
echo "       Each fixture lists its (input) row first so the [raw] column"
echo "       shows the actual byte reduction each minifier achieved."
echo "       cascade binary: $(command -v "$CASCADE")"
