#!/bin/sh
# Differential render tests in a real headless browser: a stylesheet and its
# optimized forms must compute the same style for every element of a document
# derived from the stylesheet's own selectors.
#
# [dune test] runs the same harness on a small default sweep; this script runs
# it from the repository root, so the artefacts of a failure land in tmp/
# rather than inside _build, and passes its arguments through:
#
#   sh test/render/run.sh            # the default sweep
#   sh test/render/run.sh --full     # every corpus file and more seeds
#
# It skips cleanly, with status 0, when node or a headless browser is missing,
# and reads CHROME, NODE and CASCADE_NO_BROWSER.
set -eu
dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
root=$(CDPATH= cd "$dir/../.." && pwd)
cd "$root"
"$root/scripts/with_switch.sh" dune build test/render/render_diff.exe \
  test/render/driver.js test/render/dom.js
exec _build/default/test/render/render_diff.exe "$@"
