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

script_dir="$(cd "$(dirname "$0")" && pwd)"
SCRIPT_DIR="$script_dir"
TRACE_DIR_ARG="${1:-$SCRIPT_DIR/../traces}"
mkdir -p "$TRACE_DIR_ARG"
TRACE_DIR="$(cd "$TRACE_DIR_ARG" && pwd)"

KUHN_URL="https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt"
KUHN_SHA256="b51cfe9a8d2689c90b10a13a3624092d546e0837c6ff835b6e5d713c5749c8c6"

command -v curl >/dev/null || { echo "curl not on PATH" >&2; exit 1; }

echo "Fetching $KUHN_URL ..."
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
curl -sSLf -o "$tmp" "$KUHN_URL"
if command -v sha256sum >/dev/null 2>&1; then
  actual="$(sha256sum "$tmp" | cut -d ' ' -f 1)"
else
  actual="$(shasum -a 256 "$tmp" | cut -d ' ' -f 1)"
fi
if [ "$actual" != "$KUHN_SHA256" ]; then
  echo "UTF-8-test.txt SHA-256 mismatch: expected $KUHN_SHA256, got $actual" >&2
  exit 1
fi
mv "$tmp" "$TRACE_DIR/UTF-8-test.txt"
trap - EXIT
