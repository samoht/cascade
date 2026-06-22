#!/bin/sh
# Differential test for `cascade apply`: the resolved page must produce the
# same computed style, for every element, as the original page with its
# <style> blocks, in a real headless browser. Both output modes are checked.
# Skips cleanly when no headless browser or node is available (e.g. in CI).
dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
CASCADE=${CASCADE:-cascade}
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
if command -v dune >/dev/null 2>&1; then
  dune build test/inline/canon_filter.exe 2>/dev/null
  CANON_FILTER=$(cd "$dir/../.." && find _build -name canon_filter.exe 2>/dev/null | head -1)
  [ -n "$CANON_FILTER" ] && export CANON_FILTER="$(cd "$dir/../.." && pwd)/$CANON_FILTER"
fi
fail=0
for f in "$dir"/fixtures/*.html; do
  for mode in "" "--minimal"; do
    out=$(mktemp)
    "$CASCADE" apply $mode "$f" > "$out" 2>/dev/null
    if node "$dir/xtest.js" "$f" "$out" 2>&1 | grep -q "IDENTICAL"; then
      echo "ok   $(basename "$f") ${mode:-full}"
    else
      echo "FAIL $(basename "$f") ${mode:-full}"; node "$dir/xtest.js" "$f" "$out" 2>&1 | tail -6; fail=1
    fi
    rm -f "$out"
  done
done
# Real pages downloaded by fetch.sh (gitignored): reported for coverage, but
# they do not gate the run - they change upstream and exercise features still
# being grown, so only the committed fixtures decide pass/fail.
if [ -d "$dir/pages" ]; then
  for f in "$dir"/pages/*.html; do
    [ -e "$f" ] || continue
    out=$(mktemp)
    "$CASCADE" apply --minimal "$f" > "$out" 2>/dev/null
    res=$(node "$dir/xtest.js" "$f" "$out" 2>/dev/null | grep RESULT)
    echo "real $(basename "$f"): ${res:-no result}"
    rm -f "$out"
  done
fi
exit $fail
