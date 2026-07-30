#!/usr/bin/env bash
# Minify every stylesheet in the SatCSS corpus and report cost and output size.
#
# Two files are not a corpus. Optimiser work measured on one or two stylesheets
# overfits to their shape: on the two Tailwind fixtures 24 of 25 large factoring
# runs are discarded by the transfer gate, against 115 of 236 across the corpus.
# This sweep is the check that a change holds over all 504.
#
# Reports per-file CPU time (user), raw bytes and gzip bytes, then the totals
# and the slowest files. CPU time rather than wall-clock, because a loaded
# machine inflates wall-clock several-fold and makes small differences
# unreadable.
#
# With -b, runs a second binary interleaved with the first, file by file, so
# load drift affects both alike. It reports the ratio and, more importantly,
# whether the two produce byte-identical output: an optimiser change that moves
# the time and the bytes is a trade, not a speedup, and the difference is what
# tells them apart.
#
# Corpus: stripped real-world stylesheets from the SatCSS benchmark
# (Hague, Lin, Hong; TOPLAS 2019), regenerated under
# test/interop/satcss/scripts/.tool/benchmarks/. The CSS is third-party
# website content and is not redistributed by Cascade.
#
# Usage:
#   bench/corpus_sweep.sh                      # all 504 fixtures
#   bench/corpus_sweep.sh -n 20                # slowest 20 in the summary
#   bench/corpus_sweep.sh -b ./old/main.exe    # A/B against another binary
#   bench/corpus_sweep.sh -j 4                 # 4 files at a time
#   bench/corpus_sweep.sh github guardian      # only matching fixtures

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
CORPUS="${CORPUS:-$ROOT/test/interop/satcss/scripts/.tool/benchmarks}"
CASCADE="${CASCADE:-$ROOT/_build/default/bin/main.exe}"
BASELINE=""
TOP=10
JOBS=1
TIMEOUT=${TIMEOUT:-300}

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//' >&2
  exit 1
}

while getopts ":n:b:c:j:h" opt; do
  case "$opt" in
    n) TOP="$OPTARG" ;;
    b) BASELINE="$OPTARG" ;;
    c) CASCADE="$OPTARG" ;;
    j) JOBS="$OPTARG" ;;
    h | *) usage ;;
  esac
done
shift $((OPTIND - 1))

[ -x "$CASCADE" ] || {
  echo "error: cascade binary not found: $CASCADE" >&2
  echo "  hint: dune build bin/main.exe" >&2
  exit 2
}
[ -d "$CORPUS" ] || {
  echo "error: corpus not found: $CORPUS" >&2
  echo "  hint: run 'dune build @regen-traces' under test/interop/satcss first" >&2
  exit 2
}
[ -z "$BASELINE" ] || [ -x "$BASELINE" ] || {
  echo "error: baseline binary not found: $BASELINE" >&2
  exit 2
}

FILES=()
if [ $# -eq 0 ]; then
  while IFS= read -r f; do FILES+=("$f"); done < <(find "$CORPUS" -name '*.css' | sort)
else
  for pat in "$@"; do
    while IFS= read -r f; do FILES+=("$f"); done < <(find "$CORPUS" -name "*$pat*.css" | sort)
  done
fi
[ ${#FILES[@]} -gt 0 ] || {
  echo "error: no fixtures matched" >&2
  exit 2
}

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# User CPU seconds for one minify run, and the output written to $2. Prints
# "-" when the run fails or is killed, so one bad fixture cannot abort a sweep.
cpu_run() {
  local bin=$1 src=$2 out=$3 err="$TMP/err.$$"
  if { /usr/bin/time -p timeout "$TIMEOUT" "$bin" fmt --minify "$src" >"$out"; } 2>"$err"; then
    # Cascade's parse warnings quote the source, which is not always valid
    # UTF-8; parse the timing bytes with the C locale so awk cannot choke on it.
    LC_ALL=C awk '/^user/ { print $2; found = 1 } END { if (!found) print "-" }' "$err"
  else
    printf '%s\n' '-'
  fi
}

one_file() {
  local src=$1 name
  name=$(basename "$src" .css)
  local out="$TMP/$name.out" t
  t=$(cpu_run "$CASCADE" "$src" "$out")
  local raw=- gz=-
  if [ "$t" != "-" ]; then
    raw=$(wc -c <"$out" | tr -d ' ')
    gz=$(gzip -9 -c "$out" | wc -c | tr -d ' ')
  fi
  if [ -n "$BASELINE" ]; then
    local bout="$TMP/$name.base" bt
    bt=$(cpu_run "$BASELINE" "$src" "$bout")
    local same=n/a
    if [ "$t" != "-" ] && [ "$bt" != "-" ]; then
      if cmp -s "$out" "$bout"; then same=same; else same=DIFFERS; fi
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$t" "$raw" "$gz" "$bt" "$same"
  else
    printf '%s\t%s\t%s\t%s\n' "$name" "$t" "$raw" "$gz"
  fi
  rm -f "$out" "$TMP/$name.base"
}

export -f one_file cpu_run
export CASCADE BASELINE TMP TIMEOUT

echo "corpus: $CORPUS (${#FILES[@]} files)" >&2
echo "cascade: $CASCADE" >&2
[ -z "$BASELINE" ] || echo "baseline: $BASELINE" >&2

RESULTS="$TMP/results.tsv"
if [ "$JOBS" -gt 1 ]; then
  printf '%s\n' "${FILES[@]}" | xargs -P "$JOBS" -I{} bash -c 'one_file "$@"' _ {} >"$RESULTS"
else
  for f in "${FILES[@]}"; do one_file "$f"; done >"$RESULTS"
fi

sort -o "$RESULTS" "$RESULTS"

LC_ALL=C awk -F'\t' -v top="$TOP" -v ab="${BASELINE:+1}" '
  $2 != "-" { n++; total += $2; raw += $3; gz += $4; time[$1] = $2 }
  $2 == "-" { failed = failed " " $1; nfail++ }
  ab && $6 == "DIFFERS" { differs = differs " " $1; ndiff++ }
  ab && $2 != "-" && $5 != "-" { btotal += $5 }
  END {
    printf "\n== TOTALS ==\n"
    printf "files          : %d ok", n
    if (nfail) printf ", %d failed", nfail
    printf "\n"
    printf "cascade cpu    : %.2fs\n", total
    if (ab) {
      printf "baseline cpu   : %.2fs\n", btotal
      if (btotal > 0) printf "ratio          : %.3fx  (below 1 is faster)\n", total / btotal
      printf "output         : %s\n", (ndiff ? ndiff " files DIFFER" : "byte-identical on every file")
      if (ndiff) printf "differing      :%s\n", differs
    }
    printf "raw bytes      : %d\n", raw
    printf "gzip bytes     : %d\n", gz
    if (nfail) printf "failed         :%s\n", failed
    printf "\n== SLOWEST %d ==\n", top
    k = 0
    for (f in time) { order[++k] = f }
    for (i = 1; i <= k; i++)
      for (j = i + 1; j <= k; j++)
        if (time[order[j]] + 0 > time[order[i]] + 0) { t = order[i]; order[i] = order[j]; order[j] = t }
    for (i = 1; i <= k && i <= top; i++) printf "%8.2fs  %s\n", time[order[i]], order[i]
  }
' "$RESULTS"
