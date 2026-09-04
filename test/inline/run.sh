#!/bin/sh
# Differential render tests in a real headless browser: for every element the
# complete computed style must be identical before and after a transform.
# Covers `cascade apply` (both modes) and `cascade --minify` (each <style>
# block minified in place). Skips cleanly with no browser or node (e.g. in CI).
#
# A difference list is a measurement, so the run says what produced it: the
# binary under test and the browser version head the output, and each fetched
# page carries the hash of the bytes actually measured. fetch.sh freezes every
# downloaded page (freeze_page.js) and xtest.js pins the browser, leaving
# computed style a function of the CSS alone.
dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
root=$(CDPATH= cd "$dir/../.." && pwd)
# CASCADE_NO_BROWSER silences this gate, and a gate that did not run is not a
# pass: only the value that says the run checked nothing exits 0. Same contract
# as test/render/browser.ml, so one variable does not mean two things.
case ${CASCADE_NO_BROWSER-} in
  '') ;;
  unchecked)
    echo "SKIP: inline (CASCADE_NO_BROWSER=unchecked, so this run checks nothing)"
    exit 0 ;;
  *)
    echo "FAIL: inline is suppressed by CASCADE_NO_BROWSER=$CASCADE_NO_BROWSER;" \
         "a gate that did not run is not a pass. Set CASCADE_NO_BROWSER=unchecked" \
         "to exit 0 and say so." >&2
    exit 1 ;;
esac
if [ -z "$CHROME" ]; then
  CHROME=$(command -v chromium 2>/dev/null || command -v google-chrome 2>/dev/null || true)
fi
# sort -V, not sort: the cache holds mac_arm-<version> directories, and in
# lexical order a 99.x outranks a 146.x.
[ -z "$CHROME" ] && CHROME=$(find "$HOME/.cache/puppeteer" -type f -name chrome-headless-shell 2>/dev/null | sort -V | tail -1)
# The same places test/render/browser.ml looks, so a machine does not run one
# browser oracle and skip the other.
[ -z "$CHROME" ] && CHROME=$(find "$HOME/Library/Caches/ms-playwright" "$HOME/.cache/ms-playwright" -type f -name headless_shell 2>/dev/null | sort -V | tail -1)
[ -z "$CHROME" ] && [ -x "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" ] &&
  CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if [ -z "$CHROME" ] || ! command -v node >/dev/null 2>&1; then
  echo "SKIP: no headless browser or node available"; exit 0
fi
export CHROME

# Chrome versions do not agree on computed styles, so a count is comparable
# only against another from the same engine. Reporting the version keeps a
# number from being quoted without it; CHROME_VERSION demands a given build for
# a comparison that has to cross machines, and is unset by default because
# requiring one build outright would strand anyone who lacks it.
browser=$("$CHROME" --version 2>/dev/null | tr -d '\r')
[ -z "$browser" ] && browser="unknown ($CHROME)"
if [ -n "$CHROME_VERSION" ] && ! printf '%s\n' "$browser" | grep -qF "$CHROME_VERSION"; then
  echo "ERROR: CHROME_VERSION=$CHROME_VERSION, but the browser is $browser" >&2
  exit 1
fi

# The binary under test is this working tree's, built here. Falling back to a
# `cascade` off $PATH tests whichever release happens to be installed while
# still printing a confident result, so the run says nothing about the code it
# was pointed at. CASCADE still wins, for measuring a release on purpose.
if [ -z "$CASCADE" ]; then
  if ! (cd "$root" && "$root/scripts/with_switch.sh" dune build bin/main.exe); then
    echo "ERROR: cannot build bin/main.exe, so there is nothing to test." >&2
    echo "  Fix the build, or set CASCADE to the binary you mean to measure." >&2
    exit 1
  fi
  CASCADE="$root/_build/default/bin/main.exe"
fi
case $CASCADE in
  */*) ;;
  *) CASCADE=$(command -v "$CASCADE" 2>/dev/null || echo "$CASCADE") ;;
esac
if [ ! -x "$CASCADE" ]; then
  echo "ERROR: CASCADE is not an executable: $CASCADE" >&2
  exit 1
fi
# Absolute, so the header names one file however the run was started.
case $CASCADE in
  /*) ;;
  *) CASCADE=$(CDPATH= cd "$(dirname "$CASCADE")" && pwd)/$(basename "$CASCADE") ;;
esac
export CASCADE
cascade_version=$("$CASCADE" --version 2>/dev/null | head -1)

# Canonical-difference filter: compares values the way cascade does, so
# render-equivalent spellings (0% vs 0px, red vs rgb(...)) are not reported.
# Without it every such pair counts, and the run prints a number that looks
# like a result but is an inflated one, which is worse than no number at all.
# So a filter that is missing or does not work is fatal, not skipped.
if [ -z "$CANON_FILTER" ]; then
  # Not discarded: a swallowed build failure leaves the last binary in place,
  # and the probe below passes it.
  if ! (cd "$root" && "$root/scripts/with_switch.sh" dune build test/inline/canon_filter.exe); then
    echo "ERROR: cannot build test/inline/canon_filter.exe." >&2
    exit 1
  fi
  CANON_FILTER="$root/_build/default/test/inline/canon_filter.exe"
fi
# Absolute, because xtest.js runs it from a directory of its own choosing.
case $CANON_FILTER in
  /*) ;;
  */*) CANON_FILTER=$(CDPATH= cd "$(dirname "$CANON_FILTER")" && pwd)/$(basename "$CANON_FILTER") ;;
  *) CANON_FILTER=$(CDPATH= pwd)/$CANON_FILTER ;;
esac
# Prove it filters, rather than that a file exists: a stale or broken binary
# passes an existence check and silently restores raw comparison. The first two
# lines each spell one value two ways and must be dropped; the third is a real
# change and must survive. The prefixed line is there on purpose: cascade folds
# a colour only for a property it types, and the prefixed colours are the ones
# a real page carries by the thousand.
canon_probe=$(printf '0\tX\tbackground-position\t0%% 0%%\t0px 0px\n0\tX\t-webkit-text-fill-color\tlab(1.90334 0.278696 -5.48866)\trgb(3, 7, 18)\n0\tX\tcolor\tred\tblue\n' |
  "$CANON_FILTER" 2>/dev/null)
if [ "$canon_probe" != "$(printf '0\tX\tcolor\tred\tblue')" ]; then
  echo "ERROR: canonical filter missing or not filtering: $CANON_FILTER" >&2
  echo "  build it with: dune build test/inline/canon_filter.exe" >&2
  echo "  (or point CANON_FILTER at one). Without it, equivalent spellings" >&2
  echo "  count as differences and the reported total is not a result." >&2
  exit 1
fi
export CANON_FILTER

# Fetched pages are downloaded rather than committed, so record which bytes
# produced a result: the hash moves when the site or its CDN does, and a count
# that moved with it is explained instead of mysterious.
sha() { # file -> first 12 hex of sha256
  if command -v shasum >/dev/null 2>&1; then shasum -a 256 "$1"
  elif command -v sha256sum >/dev/null 2>&1; then sha256sum "$1"
  elif command -v openssl >/dev/null 2>&1; then openssl dgst -sha256 "$1" | sed 's/.*= //'
  else echo nohash
  fi | cut -c1-12
}

echo "cascade: $CASCADE (${cascade_version:-unknown version})"
echo "browser: $browser"
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
# A page is frozen once, at fetch time, and the freezer moves. One frozen
# before freeze_page.js learned to strip @font-face keeps the rules that swap
# text metrics mid-render, and it then differs from itself by a few hundred
# computed styles on about half of its runs; measured against a transform, that
# reads as a defect in the transform.
#
# Freezing is idempotent, so re-freezing a page that is current is a no-op: one
# that changes under the freezer in the tree was frozen by a different one and
# is stale, whatever it happens to render today. That is the deterministic half
# of the question, and it costs milliseconds, so ask it first.
frozen_now() { # label file
  refrozen=$(mktemp)
  # A freezer that will not run would mark every page stale, which reads as a
  # fetch.sh away from a working harness and is not. Stop, as a broken
  # canonical filter does.
  if ! node "$dir/freeze_page.js" "$2" "$refrozen"; then
    rm -f "$refrozen"
    echo "ERROR: freeze_page.js failed on $2, so no page can be checked" >&2
    exit 1
  fi
  if cmp -s "$2" "$refrozen"; then rm -f "$refrozen"; return 0; fi
  rm -f "$refrozen"
  echo "UNUSABLE $1"
  echo "     frozen by an older freeze_page.js, so what it renders is not"
  echo "     what it will render; re-run fetch.sh"
  fail=1
  return 1
}
# The other half: a count means something about a transform only if the page
# renders the same twice without one. Two renders catch only a page that is
# unstable on those two renders, which is why the check above is not enough on
# its own, but they catch instability the freezer does not know about. Prove
# the page before measuring it, and skip it rather than report a number from an
# instrument that is measuring something else.
selfstable() { # label file
  report=$(node "$dir/xtest.js" --self "$2" 2>&1)
  case $report in
    *"RESULT: SELF-STABLE"*) return 0 ;;
    *) echo "UNUSABLE $1"
       printf '%s\n' "$report" | head -8 | sed 's/^/     /'
       echo "     no count from this page means anything; re-run fetch.sh"
       fail=1
       return 1 ;;
  esac
}
# A transform that dies leaves an empty file, and an empty page differs from
# the original in every computed style, so the run blames the transform for a
# render change that never happened. Report the status, and the error the tool
# printed with it.
transform() { # label out cmd...
  label=$1; out=$2; shift 2
  err=$(mktemp)
  "$@" > "$out" 2> "$err"
  status=$?
  if [ "$status" -ne 0 ]; then
    echo "CRASH $label (exit $status): $*"
    head -8 "$err" | sed 's/^/     /'
    fail=1
  fi
  rm -f "$err"
  return "$status"
}
for f in "$dir"/fixtures/*.html; do
  for mode in "" "--minimal"; do
    tmp=$(mktemp)
    label="$(basename "$f") ${mode:-full}"
    # shellcheck disable=SC2086 # an empty $mode must vanish, not pass ""
    if transform "$label" "$tmp" "$CASCADE" apply $mode "$f"; then
      check "$label" "$f" "$tmp"
    fi
    rm -f "$tmp"
  done
done
# cascade --minify preserves the render too, and so does the closed-world
# --inline-vars cleanup layered on it: rewrite each <style> block in place and
# compare computed styles against the original page.
for flags in "" "--inline-vars"; do
  for f in "$dir"/fixtures/*.html; do
    tmp=$(mktemp)
    label="$(basename "$f") minify${flags:+ $flags}"
    # shellcheck disable=SC2086 # an empty $flags must vanish, not pass ""
    if transform "$label" "$tmp" node "$dir/minify_page.js" "$f" $flags; then
      check "$label" "$f" "$tmp"
    fi
    rm -f "$tmp"
  done
done
# Real pages downloaded by fetch.sh (gitignored, so absent until it is run).
# They gate like the fixtures do: a surviving difference is a defect in the
# transform, whichever page happened to find it. One that appears the day a
# site is redesigned is still a defect, but re-run fetch.sh before reading it
# as a regression in the working tree, and check the hash in the label first.
#
# Both transforms run: `apply` and `--minify` fail differently, and a real page
# carries selectors and feature queries no fixture does, so a minify defect
# that only a real page reaches goes unmeasured until the leg exists.
#
# Both checks are on the pages alone. A fixture is committed and changes only
# under review; a page arrives off the network and goes stale on its own, which
# is the way this has failed.
for f in "$dir"/pages/*.html; do
  [ -e "$f" ] || continue
  page="$(basename "$f")@$(sha "$f")"
  frozen_now "real $page" "$f" || continue
  selfstable "real $page" "$f" || continue
  tmp=$(mktemp)
  label="real $page minimal"
  if transform "$label" "$tmp" "$CASCADE" apply --minimal "$f"; then
    check "$label" "$f" "$tmp"
  fi
  label="real $page minify"
  if transform "$label" "$tmp" node "$dir/minify_page.js" "$f"; then
    check "$label" "$f" "$tmp"
  fi
  rm -f "$tmp"
done
exit $fail
