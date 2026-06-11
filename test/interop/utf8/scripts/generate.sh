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
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel 2>/dev/null || true)"
if [ -n "$repo_root" ] && [[ "$script_dir" == "$repo_root"/_build/default/* ]]; then
  SCRIPT_DIR="$repo_root/${script_dir#"$repo_root/_build/default/"}"
else
  SCRIPT_DIR="$script_dir"
fi
TRACE_DIR_ARG="${1:-$SCRIPT_DIR/../traces}"
mkdir -p "$TRACE_DIR_ARG"
TRACE_DIR="$(cd "$TRACE_DIR_ARG" && pwd)"

KUHN_URL="https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt"

command -v curl >/dev/null || { echo "curl not on PATH" >&2; exit 1; }

echo "Fetching $KUHN_URL ..."
curl -sSLf -o "$TRACE_DIR/UTF-8-test.txt" "$KUHN_URL"
