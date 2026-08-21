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
export CASCADE=${CASCADE:-cascade}
if [ -z "$CHROME" ]; then
  CHROME=$(command -v chromium 2>/dev/null || command -v google-chrome 2>/dev/null || true)
fi
[ -z "$CHROME" ] && CHROME=$(find "$HOME/.cache/puppeteer" -type f -name chrome-headless-shell 2>/dev/null | sort | tail -1)
if [ -z "$CHROME" ] || ! command -v node >/dev/null 2>&1; then
  echo "SKIP: no headless browser or node available"; exit 0
fi
export CHROME
# Canonical-difference filter: compares values the way cascade does, so
# render-equivalent spellings (0% vs 0px, red vs rgb(...)) are not reported.
# The filter is optional, so a build that cannot run leaves it unset rather
# than stopping the sweep -- but it says why instead of discarding the reason.
root=$(CDPATH= cd "$dir/../.." && pwd)
if "$root/scripts/with_switch.sh" dune build test/inline/canon_filter.exe; then
  CANON_FILTER=$(cd "$root" && find _build -name canon_filter.exe | head -1)
  [ -n "$CANON_FILTER" ] && export CANON_FILTER="$root/$CANON_FILTER"
fi
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
