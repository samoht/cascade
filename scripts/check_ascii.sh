#!/bin/sh
# Reject non-ASCII bytes in cascade source.
#
# Reads candidate paths from the arguments, or from stdin when there are none,
# keeps the ones the rule covers and reports every non-ASCII byte in them. With
# [--staged] the staged content is read instead of the worktree, so the
# pre-commit hook sees what the commit will contain.
#
# Source stays 7-bit: write "sec." not the section sign, "--" not the em dash,
# and \u{XXXX} escapes for a glyph a string must emit (e.g. the diff tree
# connectors). Prose (README.md, CHANGES.md) and the test fixtures are outside
# the filter, so they keep their non-ASCII content.
#
# The pre-commit hook pipes the staged names in; CI pipes [git ls-files] in.
# Both go through this one filter and this one pattern so the two cannot drift.

set -eu

# Covers the files that carry OCaml source or build rules.
COVERED='(\.(ml|mli|mll|mly)|(^|/)dune(-project)?)$'

# The class spells out the printable range instead of using [:print:], which a
# Unicode-aware grep reads as all of Unicode -- the complement is then empty and
# the pattern is rejected. [\x00-\x7F] is no better: neither BSD nor GNU grep
# expands \x inside a bracket expression, so it matches a literal x. An exit
# status above 1 is a rejected pattern rather than a clean file, so it aborts.
PATTERN='[^ -~[:cntrl:]]'

staged=false
if [ "${1-}" = "--staged" ]; then
    staged=true
    shift
fi

if [ "$#" -gt 0 ]; then
    names=$(printf '%s\n' "$@")
else
    names=$(cat)
fi
files=$(printf '%s\n' "$names" | grep -E "$COVERED" || true)
[ -n "$files" ] || exit 0

scan() {
    if [ "$staged" = true ]; then
        git show ":$1" | LC_ALL=C grep -n "$PATTERN"
    else
        LC_ALL=C grep -n "$PATTERN" -- "$1"
    fi
}

status=0
while IFS= read -r f; do
    [ -n "$f" ] || continue
    rc=0
    hits=$(scan "$f") || rc=$?
    case $rc in
    0)
        [ "$status" -eq 1 ] || echo "error: non-ASCII byte in source:" >&2
        status=1
        printf '%s\n' "$hits" | sed "s|^|  $f:|" >&2
        ;;
    1) ;;
    *)
        echo "error: the ASCII check could not run on $f (grep exit $rc)" >&2
        exit 1
        ;;
    esac
done <<EOF
$files
EOF

if [ "$status" -ne 0 ]; then
    echo "use ASCII in comments and \\u{XXXX} escapes in string literals." >&2
fi
exit $status
