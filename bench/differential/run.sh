#!/usr/bin/env bash
# One entrypoint for differential experiments:
#
#   run.sh outputs    --baseline TREE [--candidate TREE] [--corpus TREE]
#   run.sh candidates --baseline TREE [--candidate TREE] [--quick]
#   run.sh render     --tool TOOL --output DIR [--pages]
#   run.sh corpus     [bench/corpus_sweep.sh options]
#
# outputs and candidates build the same deterministic observation driver in
# two worktrees and require identical record streams.  render reuses the
# committed inline browser engine with a chosen minifier.  corpus delegates to
# the established interleaved performance/byte sweep.

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
INLINE="$ROOT/test/inline"

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//' >&2
  exit 2
}

absolute_dir() {
  (cd "$1" && pwd)
}

build_driver() {
  local tree=$1
  [ -f "$tree/bench/differential/driver.ml" ] || {
    echo "error: differential suite is absent from $tree" >&2
    echo "  stack the compared branch on the differential-suite commit" >&2
    return 2
  }
  (cd "$tree" && "$tree/scripts/with_switch.sh" dune build \
    bench/differential/driver.exe) >&2
  printf '%s\n' "$tree/_build/default/bench/differential/driver.exe"
}

compare_profile() {
  local profile=$1
  shift
  local baseline="" candidate="$ROOT" corpus="$ROOT" quick=0
  while [ $# -gt 0 ]; do
    case $1 in
      --baseline) [ $# -ge 2 ] || usage; baseline=$2; shift 2 ;;
      --candidate) [ $# -ge 2 ] || usage; candidate=$2; shift 2 ;;
      --corpus) [ $# -ge 2 ] || usage; corpus=$2; shift 2 ;;
      --quick) quick=1; shift ;;
      *) usage ;;
    esac
  done
  [ -n "$baseline" ] || { echo "error: --baseline TREE is required" >&2; exit 2; }
  baseline=$(absolute_dir "$baseline")
  candidate=$(absolute_dir "$candidate")
  corpus=$(absolute_dir "$corpus")

  local work
  work=$(mktemp -d)
  trap 'rm -rf "$work"' RETURN
  local baseline_driver candidate_driver
  baseline_driver=$(build_driver "$baseline")
  candidate_driver=$(build_driver "$candidate")
  local -a args
  if [ "$profile" = outputs ]; then
    args=(outputs "$corpus")
  else
    args=(candidates)
    [ "$quick" -eq 0 ] || args+=(--quick)
  fi
  "$baseline_driver" "${args[@]}" >"$work/baseline" 2>"$work/baseline.log"
  "$candidate_driver" "${args[@]}" >"$work/candidate" 2>"$work/candidate.log"
  if [ "$quick" -eq 1 ]; then
    echo "profile:   $profile (quick)"
  else
    echo "profile:   $profile"
  fi
  echo "baseline:  $baseline"
  echo "candidate: $candidate"
  [ "$profile" != outputs ] || echo "corpus:    $corpus"
  cat "$work/baseline.log" | sed 's/^/baseline:  /'
  cat "$work/candidate.log" | sed 's/^/candidate: /'
  if cmp -s "$work/baseline" "$work/candidate"; then
    echo "RESULT: IDENTICAL observations"
  else
    echo "RESULT: DIFFERENT observations"
    diff -u "$work/baseline" "$work/candidate" || true
    return 1
  fi
}

find_browser() {
  if [ -n "${CHROME:-}" ]; then printf '%s\n' "$CHROME"; return; fi
  local browser
  browser=$(command -v chromium 2>/dev/null || command -v google-chrome 2>/dev/null || true)
  if [ -z "$browser" ]; then
    browser=$(find "${HOME}/.cache/puppeteer" -type f -name chrome-headless-shell \
      2>/dev/null | sort -V | tail -1)
  fi
  printf '%s\n' "$browser"
}

short_sha256() {
  if command -v shasum >/dev/null 2>&1; then shasum -a 256 "$1"
  elif command -v sha256sum >/dev/null 2>&1; then sha256sum "$1"
  elif command -v openssl >/dev/null 2>&1; then openssl dgst -sha256 "$1" | sed 's/.*= //'
  else echo nohash
  fi | cut -c1-12
}

render_profile() {
  local tool="" output="" pages=0 command="" version="${MINIFIER_VERSION:-}"
  while [ $# -gt 0 ]; do
    case $1 in
      --tool) [ $# -ge 2 ] || usage; tool=$2; shift 2 ;;
      --command) [ $# -ge 2 ] || usage; command=$2; tool=custom; shift 2 ;;
      --output) [ $# -ge 2 ] || usage; output=$2; shift 2 ;;
      --pages) pages=1; shift ;;
      *) usage ;;
    esac
  done
  [ -n "$tool" ] || { echo "error: --tool TOOL or --command CMD is required" >&2; exit 2; }
  [ -n "$output" ] || { echo "error: --output DIR is required" >&2; exit 2; }
  case $tool in
    lightningcss)
      command -v lightningcss >/dev/null 2>&1 \
        || { echo "error: lightningcss is not on PATH" >&2; exit 2; }
      command='lightningcss --minify'
      [ -n "$version" ] || version=$(lightningcss --version 2>&1 | head -1)
      ;;
    cascade)
      (cd "$ROOT" && "$ROOT/scripts/with_switch.sh" dune build bin/main.exe) >&2
      command="\"$ROOT/_build/default/bin/main.exe\" fmt --minify -"
      [ -n "$version" ] \
        || version=$("$ROOT/_build/default/bin/main.exe" --version 2>&1 | head -1)
      ;;
    csso)
      command -v csso >/dev/null 2>&1 \
        || { echo "error: csso is not on PATH" >&2; exit 2; }
      command=csso
      [ -n "$version" ] || version=$(csso --version 2>&1 | head -1)
      ;;
    cleancss)
      command -v cleancss >/dev/null 2>&1 \
        || { echo "error: cleancss is not on PATH" >&2; exit 2; }
      command='cleancss -O2'
      [ -n "$version" ] || version=$(cleancss --version 2>&1 | head -1)
      ;;
    esbuild)
      command -v esbuild >/dev/null 2>&1 \
        || { echo "error: esbuild is not on PATH" >&2; exit 2; }
      command='esbuild --minify --loader=css'
      [ -n "$version" ] || version=$(esbuild --version 2>&1 | head -1)
      ;;
    custom) ;;
    *) echo "error: unknown tool: $tool" >&2; exit 2 ;;
  esac
  CHROME=$(find_browser)
  if [ -z "$CHROME" ] || ! command -v node >/dev/null 2>&1; then
    echo "error: render profile needs node and a headless Chrome" >&2
    exit 2
  fi
  export CHROME
  (cd "$ROOT" && "$ROOT/scripts/with_switch.sh" dune build \
    test/inline/canon_filter.exe) >&2
  CANON_FILTER="$ROOT/_build/default/test/inline/canon_filter.exe"
  export CANON_FILTER
  output=$(mkdir -p "$output" && absolute_dir "$output")
  : >"$output/summary.tsv"
  local status=0
  local -a inputs=("$INLINE"/fixtures/*.html)
  if [ "$pages" -eq 1 ]; then inputs+=("$INLINE"/pages/*.html); fi
  echo "tool:      $tool ($command; ${version:-version unreported})"
  echo "browser:   $("$CHROME" --version 2>/dev/null | tr -d '\r')"
  echo "artifacts: $output"
  local source name transformed report refrozen
  for source in "${inputs[@]}"; do
    [ -e "$source" ] || continue
    name=$(basename "$source" .html)
    case $source in
      "$INLINE"/pages/*)
        name="$name@$(short_sha256 "$source")"
        refrozen=$(mktemp)
        if ! node "$INLINE/freeze_page.js" "$source" "$refrozen" \
          || ! cmp -s "$source" "$refrozen"; then
          rm -f "$refrozen"
          printf 'LABEL\t%s\tUNUSABLE-STALE\t-\tRAW\t-\tFILTERED\t-\tSPLIT\t-\n' \
            "$name" | tee -a "$output/summary.tsv"
          status=1
          continue
        fi
        rm -f "$refrozen"
        if ! report=$(node "$INLINE/xtest.js" --self "$source" 2>&1) \
          || [[ $report != *"RESULT: SELF-STABLE"* ]]; then
          printf 'LABEL\t%s\tUNUSABLE-UNSTABLE\t-\tRAW\t-\tFILTERED\t-\tSPLIT\t-\n' \
            "$name" | tee -a "$output/summary.tsv"
          printf '%s\n' "$report" >>"$output/$name.err"
          status=1
          continue
        fi
        ;;
    esac
    transformed="$output/$name.min.html"
    if ! MINIFIER_CMD="$command" REQUIRE_STYLE=1 \
      node "$INLINE/minify_page.js" "$source" >"$transformed" \
      2>"$output/$name.err"; then
      printf 'LABEL\t%s\tCRASH\t-\tRAW\t-\tFILTERED\t-\tSPLIT\t-\n' "$name" \
        | tee -a "$output/summary.tsv"
      status=1
      continue
    fi
    if ! report=$(LABEL="$name" REPORT_FORMAT=tsv \
      RAW_OUT="$output/$name.raw.tsv" FILT_OUT="$output/$name.filtered.tsv" \
      node "$INLINE/xtest.js" "$source" "$transformed" \
      2>>"$output/$name.err"); then
      printf 'LABEL\t%s\tRENDER-CRASH\t-\tRAW\t-\tFILTERED\t-\tSPLIT\t-\n' \
        "$name" | tee -a "$output/summary.tsv"
      status=1
      continue
    fi
    printf '%s\n' "$report" | tee -a "$output/summary.tsv"
    case $report in *$'\tFILTERED\t0\t'*) ;; *) status=1 ;; esac
  done
  return "$status"
}

[ $# -gt 0 ] || usage
profile=$1
shift
case $profile in
  outputs | candidates) compare_profile "$profile" "$@" ;;
  render) render_profile "$@" ;;
  corpus) exec "$ROOT/bench/corpus_sweep.sh" "$@" ;;
  *) usage ;;
esac
