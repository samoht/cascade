#!/usr/bin/env bash
# Refresh traces/UTF-8-test.txt from Markus Kuhn's UCS examples.
#
# Source: https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
# Licence: CC BY 4.0 (per file header).
#
# Boundary and malformed-sequence cases the Cascade parser should accept
# without crashing (CSS Syntax L3 sec. 3.3: replace ill-formed bytes with
# U+FFFD).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TRACE_DIR="$(cd "$SCRIPT_DIR/../traces" && pwd)"

KUHN_URL="https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt"

command -v curl >/dev/null || { echo "curl not on PATH" >&2; exit 1; }

echo "Fetching $KUHN_URL ..."
curl -sSLf -o "$TRACE_DIR/UTF-8-test.txt" "$KUHN_URL"
