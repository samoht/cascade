#!/usr/bin/env bash
# Regenerate every table in the "A Typed CSS Toolkit in OCaml" blog post.
#
# One script, one run, every number. Prints each table in the order the post
# uses them, with the tool versions that produced them.
#
# Usage:
#   bench/blog_tables.sh                  # everything
#   bench/blog_tables.sh rewrites         # just one section
#   RUNS=5 bench/blog_tables.sh
#
# Sections: versions rewrites size speed lossless reminify
#
# Requirements (all on PATH): hyperfine, cascade (or -c PATH), csso, lightningcss,
# esbuild, postcss + cssnano. Note that the `cssnano` BINARY is cssnano-cli,
# which pins cssnano 3.10.0 from 2017; we drive the real plugin through
# postcss-cli instead. This is a trap worth remembering.
#
# Browser targets: lightningcss and esbuild gate their output on a target
# list. We run them both ways and report both, because it turns out not to
# change the value rewrites at all (see the rewrites table).

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CORPUS="${CORPUS:-$ROOT/test/interop/satcss/scripts/.tool/benchmarks}"
CASCADE="${CASCADE:-$ROOT/_build/default/bin/main.exe}"
RUNS="${RUNS:-5}"
SITES=(github guardian youtube netflix amazon cnn)
TARGETS="${TARGETS:->=0.25%}"  # no space: hyperfine --shell=none splits on whitespace
ESTARGET="${ESTARGET:-chrome80,safari14,firefox78}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

gz() { gzip -9 -c "$1" | wc -c | tr -d ' '; }
br() { brotli -c "$1" 2>/dev/null | wc -c | tr -d ' '; }
raw() { wc -c < "$1" | tr -d ' '; }
# The corpus also holds pre-minified variants (-cleancss, -csso, ...). The
# canonical input is plain -stripmq.css; anything else is somebody's output.
fixture() { echo "$CORPUS/$1-stripmq.css"; }

# Mean wall-clock milliseconds, +- one standard deviation, via hyperfine.
# /usr/bin/time only resolves to 10ms, which is useless for tools that finish
# in single-digit milliseconds: it reports them all as "10ms" or "0ms".
time_ms() {
  local json
  json="$(hyperfine --warmup 2 --runs "$RUNS" --style none --export-json /dev/stdout \
            --shell=none "$*" 2>/dev/null)" || { echo "?"; return; }
  echo "$json" | python3 -c '
import json,sys
r = json.load(sys.stdin)["results"][0]
print("%.1f+-%.1f" % (r["mean"]*1000, r["stddev"]*1000))'
}

run_cssnano() { postcss "$1" --use cssnano --no-map -o "$2" >/dev/null 2>&1; }

section_versions() {
  echo "## Versions"
  echo "cascade      $("$CASCADE" --version 2>/dev/null || echo '?')"
  echo "csso         $(csso --version 2>/dev/null || echo '?')"
  echo "lightningcss $(lightningcss --version 2>/dev/null || echo '?')"
  echo "esbuild      $(esbuild --version 2>/dev/null || echo '?')"
  echo "cssnano      $(node -p "require('cssnano/package.json').version" 2>/dev/null || echo '?')"
  echo "node         $(node --version 2>/dev/null || echo '?')"
  echo "machine      $(uname -sm), $(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo '?')"
  echo
}

# Table 1: what each tool does to declarations people actually write.
# lightningcss and esbuild are run BOTH with and without a target list, to
# show that targets gate the syntax they emit, not whether they rewrite values.
section_rewrites() {
  echo "## What each tool rewrites"
  cat > "$TMP/probe.css" <<'CSS'
.a { width: calc(100% / 3); }
.b { padding: 0.0000001px; }
.c { color: light-dark(white, black); }
.d { width: calc(infinity * 1px); }
.e { color: color-mix(in oklch, red 50%, blue); }
.f { .parent { color: red; & .child { color: blue; } } }
@layer reset, theme;
@layer reset { * { box-sizing: border-box; } }
.g { color: oklch(0.7 0.15 250); }
CSS
  echo "--- input ---"; cat "$TMP/probe.css"
  echo "--- cascade --lossless --enforce-spec ---"
  "$CASCADE" fmt --minify --lossless --enforce-spec "$TMP/probe.css" 2>/dev/null
  echo "--- cascade (default) ---"
  "$CASCADE" fmt --minify "$TMP/probe.css" 2>/dev/null
  echo "--- csso ---";           csso "$TMP/probe.css" 2>/dev/null
  echo "--- lightningcss (no targets) ---"
  lightningcss --minify "$TMP/probe.css" 2>/dev/null
  echo "--- lightningcss --targets '$TARGETS' ---"
  lightningcss --minify --targets "$TARGETS" "$TMP/probe.css" 2>/dev/null
  echo "--- esbuild (no target) ---"
  esbuild --minify "$TMP/probe.css" 2>/dev/null
  echo "--- esbuild --target=$ESTARGET ---"
  esbuild --minify --target="$ESTARGET" "$TMP/probe.css" 2>/dev/null
  echo "--- cssnano (default preset) ---"
  run_cssnano "$TMP/probe.css" "$TMP/probe.out"; cat "$TMP/probe.out"; echo
  echo
}

# Tables 2 and 3: emitted size, gzip (transfer objective) and raw (raw objective).
section_size() {
  echo "## Size (gzip bytes, transfer objective; raw bytes, --objective=raw)"
  printf '%-10s %8s %9s %9s %9s %9s %9s %9s\n' \
    site metric input cascade csso lightningcss esbuild cssnano
  for s in "${SITES[@]}"; do
    local f; f="$(fixture "$s")"; [ -n "$f" ] || continue
    "$CASCADE" fmt --minify "$f" > "$TMP/c.css" 2>/dev/null
    "$CASCADE" fmt --minify --objective=raw "$f" > "$TMP/craw.css" 2>/dev/null
    csso "$f" -o "$TMP/s.css" 2>/dev/null
    lightningcss --minify --targets "$TARGETS" "$f" -o "$TMP/l.css" 2>/dev/null
    esbuild --minify --target="$ESTARGET" "$f" > "$TMP/e.css" 2>/dev/null
    run_cssnano "$f" "$TMP/n.css"
    printf '%-10s %8s %9s %9s %9s %9s %9s %9s\n' "$s" gzip \
      "$(gz "$f")" "$(gz "$TMP/c.css")" "$(gz "$TMP/s.css")" \
      "$(gz "$TMP/l.css")" "$(gz "$TMP/e.css")" "$(gz "$TMP/n.css")"
    printf '%-10s %8s %9s %9s %9s %9s %9s %9s\n' "$s" raw \
      "$(raw "$f")" "$(raw "$TMP/craw.css")" "$(raw "$TMP/s.css")" \
      "$(raw "$TMP/l.css")" "$(raw "$TMP/e.css")" "$(raw "$TMP/n.css")"
  done
  echo
}

# Table 4: wall clock, measured with hyperfine (warmup runs, mean and stddev).
section_speed() {
  echo "## Speed (hyperfine, mean ms +- sd over $RUNS runs, 2 warmup)"
  printf '%-10s %14s %14s %14s %14s %14s\n' \
    site cascade csso lightningcss esbuild cssnano
  for s in "${SITES[@]}"; do
    local f; f="$(fixture "$s")"; [ -n "$f" ] || continue
    printf '%-10s %14s %14s %14s %14s %14s\n' "$s" \
      "$(time_ms "$CASCADE" fmt --minify "$f")" \
      "$(time_ms csso "$f" -o "$TMP/s.css")" \
      "$(time_ms lightningcss --minify --targets "$TARGETS" "$f" -o "$TMP/l.css")" \
      "$(time_ms esbuild --minify --target="$ESTARGET" "$f" --outfile="$TMP/e.css")" \
      "$(time_ms postcss "$f" --use cssnano --no-map -o "$TMP/n.css")"
  done
  echo
}

# Table 5: what the safety flags cost in bytes.
section_lossless() {
  echo "## Cost of the safety flags (gzip bytes vs default)"
  printf '%-10s %10s %22s\n' site default '--lossless --enforce-spec' 
  for s in "${SITES[@]}"; do
    local f; f="$(fixture "$s")"; [ -n "$f" ] || continue
    local d l
    d=$("$CASCADE" fmt --minify "$f" 2>/dev/null | gzip -9 | wc -c | tr -d ' ')
    l=$("$CASCADE" fmt --minify --lossless --enforce-spec "$f" 2>/dev/null | gzip -9 | wc -c | tr -d ' ')
    printf '%-10s %10s %+22d\n' "$s" "$d" "$((l - d))"
  done
  echo
}

# Table 6: re-minifying CSS that a minifier already processed.
section_reminify() {
  echo "## Re-minifying already-minified CSS (% saved by cascade, default)"
  echo "(set REMIN_DIR to a directory of .min.css files; skipped if unset)"
  [ -n "${REMIN_DIR:-}" ] || { echo; return; }
  printf '%-24s %8s %8s %8s\n' file raw% gzip% brotli%
  for f in "$REMIN_DIR"/*.css; do
    [ -f "$f" ] || continue
    "$CASCADE" fmt --minify "$f" > "$TMP/r.css" 2>/dev/null || continue
    local r0 r1 g0 g1 b0 b1
    r0=$(raw "$f"); r1=$(raw "$TMP/r.css")
    g0=$(gz "$f");  g1=$(gz "$TMP/r.css")
    b0=$(br "$f");  b1=$(br "$TMP/r.css")
    printf '%-24s %7.1f%% %7.1f%% %7.1f%%\n' "$(basename "$f")" \
      "$(echo "scale=4; ($r0-$r1)*100/$r0" | bc)" \
      "$(echo "scale=4; ($g0-$g1)*100/$g0" | bc)" \
      "$(echo "scale=4; ($b0-$b1)*100/$b0" | bc)"
  done
  echo
}

main() {
  local sections=("$@")
  [ ${#sections[@]} -gt 0 ] || sections=(versions rewrites size speed lossless reminify)
  for sec in "${sections[@]}"; do "section_$sec"; done
}

main "$@"
