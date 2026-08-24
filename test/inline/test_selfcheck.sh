#!/bin/sh
# The harness must prove a page renders the same twice before it reports a
# difference count for it.
#
# A page whose layout has not settled by the time the computed styles are read
# differs from itself, and run.sh then attributes that difference to whichever
# transform it happened to be measuring: a defect report about code that is
# fine. It has happened twice. So a page that is not self-stable has to be
# reported as unusable, and its transform legs skipped, rather than measured.
#
# Testing that needs a browser that is unstable on demand, which a real one is
# not, so this drives run.sh through a stub: it answers `--version`, and for
# each `--dump-dom` it prints one of two computed-style payloads, alternating
# only for a page carrying the marker below. No browser is involved, so the
# gate is checked on every machine rather than only where Chrome is installed.
#
# Usage: sh test_selfcheck.sh <cascade> <canon_filter>
set -e
dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
CASCADE=$1
CANON_FILTER=$2
case $CASCADE in /*) ;; *) CASCADE=$(CDPATH= cd "$(dirname "$CASCADE")" && pwd)/$(basename "$CASCADE") ;; esac
case $CANON_FILTER in /*) ;; *) CANON_FILTER=$(CDPATH= cd "$(dirname "$CANON_FILTER")" && pwd)/$(basename "$CANON_FILTER") ;; esac

if ! command -v node >/dev/null 2>&1; then
  echo "SKIP: no node available"; exit 0
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/fixtures" "$work/pages"
cp "$dir/run.sh" "$dir/xtest.js" "$dir/minify_page.js" "$work/"

# The stub keys off the page text, not the invocation count, so the original
# page and each transform of it answer alike: only the marked page moves.
cat > "$work/browser" <<'STUB'
#!/bin/sh
case $1 in --version) echo "Stub Browser 0.0"; exit 0 ;; esac
for a in "$@"; do case $a in file://*) page=${a#file://} ;; esac; done
n=0
if grep -q xtest-stub-unstable "$page" 2>/dev/null; then
  n=$(cat "$STUB_STATE" 2>/dev/null || echo 0)
  n=$((n + 1))
  echo "$n" > "$STUB_STATE"
  n=$((n % 2))
fi
if [ "$n" = 0 ]
then body='[{"_tag":"DIV","color":"rgb(0, 0, 0)"}]'
else body='[{"_tag":"DIV","color":"rgb(1, 1, 1)"}]'
fi
printf '<html><body data-xtest="%s"></body></html>\n' \
  "$(printf '%s' "$body" | base64 | tr -d '\n')"
STUB
chmod +x "$work/browser"

page() { # path marker
  cat > "$1" <<EOF
<html><head><style>.a{color:#000}</style></head>
<body><div class="a" data-note="$2">x</div></body></html>
EOF
}
# One fixture, because run.sh always has fixtures to measure; the pages are
# what the control is for.
page "$work/fixtures/one.html" stable
page "$work/pages/stable.html" stable
page "$work/pages/moving.html" xtest-stub-unstable

STUB_STATE=$work/state
export STUB_STATE CASCADE CANON_FILTER
CHROME=$work/browser
export CHROME
status=0
sh "$work/run.sh" > "$work/out" 2>&1 || status=$?

fail() {
  echo "FAIL: $1"
  echo "--- run.sh output (exit $status) ---"
  cat "$work/out"
  exit 1
}

# The self-stable page is measured as before: the control must not cost a
# result on a page that passes it.
grep -q '^ok   real stable\.html@.* minimal$' "$work/out" ||
  fail "the self-stable page was not measured"
grep -q '^ok   real stable\.html@.* minify$' "$work/out" ||
  fail "the self-stable page was not measured under minify"

# The page that renders differently twice is named unusable.
grep -q '^UNUSABLE real moving\.html@' "$work/out" ||
  fail "a page that renders differently twice was not reported as unusable"

# And it produces no count: a difference between two renders of one page says
# nothing about a transform, so blaming one is the failure this gate exists to
# stop.
if grep -q 'moving\.html@.* \(minimal\|minify\)$' "$work/out"; then
  fail "an unusable page still reported a transform result"
fi

[ "$status" -ne 0 ] || fail "run.sh exited 0 with a page it could not measure"

echo "PASS: an unstable page is reported as unusable, not measured"
