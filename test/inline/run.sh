#!/bin/sh
# Differential render tests in a real headless browser: for every element the
# complete computed style must be identical before and after a transform.
# Covers `cascade apply` (both modes) and `cascade --minify` (each <style>
# block minified in place). Skips cleanly with no browser or node (e.g. in CI).
#
# A difference list is reproducible, so two runs can be diffed against each
# other: fetch.sh freezes every downloaded page (freeze_page.js) and xtest.js
# pins the browser, leaving computed style a function of the CSS alone.
dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
root=$(dirname "$(dirname "$dir")")
export CASCADE=${CASCADE:-cascade}
if [ -z "$CHROME" ]; then
  CHROME=$(command -v chromium 2>/dev/null || command -v google-chrome 2>/dev/null || true)
fi
[ -z "$CHROME" ] && CHROME=$(find "$HOME/.cache/puppeteer" -type f -name chrome-headless-shell 2>/dev/null | sort | tail -1)
if [ -z "$CHROME" ] || ! command -v node >/dev/null 2>&1; then
  echo "SKIP: no headless browser or node available"; exit 0
fi
export CHROME

# Build through the checkout's own opam switch. A bare `dune` off $PATH can
# belong to a different switch, and building with it fills _build with
# artefacts this project's compiler cannot read. The switch is located from the
# script's own position, or from the main worktree whose _opam a linked
# worktree shares, so nothing here is a hardcoded path.
switch=
if command -v opam >/dev/null 2>&1; then
  common=$(git -C "$dir" rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
  for cand in "$root" "${common:+$(dirname "$common")}"; do
    if [ -n "$cand" ] && [ -d "$cand/_opam" ]; then switch=$cand; break; fi
  done
fi
dune_() {
  if [ -n "$switch" ]; then opam exec --switch="$switch" -- dune "$@"; else dune "$@"; fi
}

# Canonical-difference filter: compares values the way cascade does, so
# render-equivalent spellings (0% vs 0px, red vs rgb(...)) are not reported.
# Without it every such pair counts, and the run prints a number that looks
# like a result but is an inflated one, which is worse than no number at all.
# So a filter that is missing or does not work is fatal, not skipped.
if [ -z "$CANON_FILTER" ]; then
  (cd "$root" && dune_ build test/inline/canon_filter.exe) 2>/dev/null
  CANON_FILTER="$root/_build/default/test/inline/canon_filter.exe"
fi
# Prove it filters, rather than that a file exists: a stale or broken binary
# passes an existence check and silently restores raw comparison. The first
# line is one spelling of one value and must be dropped; the second is a real
# change and must survive.
canon_probe=$(printf '0\tX\tbackground-position\t0%% 0%%\t0px 0px\n0\tX\tcolor\tred\tblue\n' |
  "$CANON_FILTER" 2>/dev/null)
if [ "$canon_probe" != "$(printf '0\tX\tcolor\tred\tblue')" ]; then
  echo "ERROR: canonical filter missing or not filtering: $CANON_FILTER" >&2
  echo "  build it with: dune build test/inline/canon_filter.exe" >&2
  echo "  (or point CANON_FILTER at one). Without it, equivalent spellings" >&2
  echo "  count as differences and the reported total is not a result." >&2
  exit 1
fi
export CANON_FILTER

echo "compare: canonical"
fail=0
# xtest.js renders two pages, which is the whole cost of the run: keep its
# report rather than paying for it a second time to print the failure.
check() { # label before after
  report=$(node "$dir/xtest.js" "$2" "$3" 2>&1)
  case $report in
    *IDENTICAL*) echo "ok   $1" ;;
    *) echo "FAIL $1"
       printf '%s\n' "$report" | head -8 | sed 's/^/     /'
       fail=1 ;;
  esac
}
for f in "$dir"/fixtures/*.html; do
  for mode in "" "--minimal"; do
    tmp=$(mktemp)
    # shellcheck disable=SC2086 # an empty $mode must vanish, not pass ""
    "$CASCADE" apply $mode "$f" > "$tmp" 2>/dev/null
    check "$(basename "$f") ${mode:-full}" "$f" "$tmp"
    rm -f "$tmp"
  done
done
# cascade --minify preserves the render too: minify each <style> block in place
# and compare computed styles against the original page.
for f in "$dir"/fixtures/*.html; do
  tmp=$(mktemp)
  node "$dir/minify_page.js" "$f" > "$tmp" 2>/dev/null
  check "$(basename "$f") minify" "$f" "$tmp"
  rm -f "$tmp"
done
# Real pages downloaded by fetch.sh (gitignored, so absent until it is run).
# They gate like the fixtures do: a surviving difference is a defect in the
# transform, whichever page happened to find it. One that appears the day a
# site is redesigned is still a defect, but re-run fetch.sh before reading it
# as a regression in the working tree.
for f in "$dir"/pages/*.html; do
  [ -e "$f" ] || continue
  tmp=$(mktemp)
  "$CASCADE" apply --minimal "$f" > "$tmp" 2>/dev/null
  check "real $(basename "$f") minimal" "$f" "$tmp"
  rm -f "$tmp"
done
exit $fail
