#!/bin/sh
# The harness must prove a page is measurable before it reports a difference
# count for it, on both of the counts it can fail.
#
# A page whose layout has not settled by the time the computed styles are read
# differs from itself, and run.sh then attributes that difference to whichever
# transform it happened to be measuring: a defect report about code that is
# fine. It has happened twice. So a page that is not self-stable has to be
# reported as unusable, and its transform legs skipped, rather than measured.
#
# Two renders only catch a page that is unstable on those two renders, and the
# page that prompted this was unstable on three self-checks in eight. What is
# deterministic about it is that it was frozen by an older freeze_page.js:
# freezing is idempotent, so re-freezing a current page is a no-op, and a page
# that changes under the current freezer is stale whatever it renders today.
# That has to be caught too, and every time.
#
# Testing the first needs a browser that is unstable on demand, which a real
# one is not, so this drives run.sh through a stub: it answers `--version`, and
# for each `--dump-dom` it prints one of two computed-style payloads,
# alternating only for a page carrying the marker below. No browser is
# involved, so both gates are checked on every machine rather than only where
# Chrome is installed.
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
cp "$dir/run.sh" "$dir/xtest.js" "$dir/minify_page.js" "$dir/freeze_page.js" "$work/"

# Silencing the gate with CASCADE_NO_BROWSER must not read as a pass, and the
# one value that exits 0 has to say the run checked nothing. Both legs return
# before run.sh looks for a browser or builds anything, which is what makes
# them checkable here.
if CASCADE_NO_BROWSER=1 sh "$work/run.sh" >/dev/null 2>&1; then
  echo "FAIL: run.sh exits 0 under CASCADE_NO_BROWSER=1"
  exit 1
fi
if ! suppressed=$(CASCADE_NO_BROWSER=unchecked sh "$work/run.sh" 2>&1); then
  echo "FAIL: run.sh does not exit 0 for the acknowledged value"
  printf '%s\n' "$suppressed"
  exit 1
fi
case $suppressed in
  SKIP:*) ;;
  *)
    echo "FAIL: run.sh skipped without saying so: $suppressed"
    exit 1 ;;
esac

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
# Frozen by a freezer that did not strip <script>, so the current one changes
# it. The stub renders it as steadily as any other page: staleness is the only
# thing wrong with it.
page "$work/pages/unfrozen.html" stable
sed -i.bak 's|</body>|<script>void 0</script></body>|' "$work/pages/unfrozen.html"
rm -f "$work/pages/unfrozen.html.bak"

# The stub is the browser for the run below, so an ambient suppression switch
# has nothing to suppress; the two legs above set it themselves.
unset CASCADE_NO_BROWSER
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

# The page the current freezer would still change was frozen by an older one,
# so what it renders today says nothing about what it renders tomorrow.
grep -q '^UNUSABLE real unfrozen\.html@' "$work/out" ||
  fail "a page the current freezer still changes was not reported as unusable"
if grep -q 'unfrozen\.html@.* \(minimal\|minify\)$' "$work/out"; then
  fail "a stale page still reported a transform result"
fi

[ "$status" -ne 0 ] || fail "run.sh exited 0 with a page it could not measure"

echo "PASS: a suppressed gate fails, and an unstable or stale page is unusable"
