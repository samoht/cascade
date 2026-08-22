#!/bin/sh
# Run a command in this project's opam switch.
#
#   scripts/with_switch.sh dune fmt
#
# Bare [dune] takes whatever compiler comes first on PATH, which on a machine
# with several switches is rarely this project's. The two then share _build and
# it ends up holding .cmt files of two magics; merlint reads only the one its
# own compiler emits, so it reports the rest as unchecked, fails on the
# coverage gate, and the typedtree rules stop running without saying so. Every
# entry point that builds goes through here, so none of them can drift.
#
# The switch lives in the checkout that owns the shared git directory: a linked
# worktree has no _opam of its own but builds against the same one.

set -eu

if [ "$#" -eq 0 ]; then
    echo "usage: with_switch.sh COMMAND [ARG]..." >&2
    exit 2
fi

here=$(CDPATH= cd "$(dirname "$0")" && pwd)
root=$(dirname "$here")

main=""
if common=$(git -C "$root" rev-parse --git-common-dir 2>/dev/null); then
    case $common in
    /*) ;;
    *) common=$root/$common ;;
    esac
    main=$(dirname "$(CDPATH= cd "$common" && pwd)")
fi

# ocamlc rather than the directory: an interrupted [opam switch create] leaves
# _opam behind with no compiler in it, and that is as unusable as no switch.
switch=""
for d in "$root" "$main"; do
    [ -n "$d" ] || continue
    if [ -x "$d/_opam/bin/ocamlc" ]; then
        switch=$d
        break
    fi
done

if [ -z "$switch" ]; then
    echo "error: no opam switch for this project; cannot run: $*" >&2
    echo "  no _opam/bin/ocamlc under $root" >&2
    if [ -n "$main" ] && [ "$main" != "$root" ]; then
        echo "  nor under $main" >&2
    fi
    echo "  create it with: opam switch create . --deps-only --with-test" >&2
    echo "  (falling back to the dune on PATH is what this script exists to" >&2
    echo "   prevent: another compiler writes artifacts merlint cannot read)" >&2
    exit 1
fi

if ! command -v opam >/dev/null 2>&1; then
    echo "error: opam is not on PATH; cannot enter $switch/_opam to run: $*" >&2
    exit 1
fi

# [opam exec] replaces the entry of whatever switch the caller's environment
# points at, rather than shadowing it, and sets OPAM_SWITCH_PREFIX and
# CAML_LD_LIBRARY_PATH to match. Prepending to PATH would leave those three
# disagreeing. It is also what CI runs.
exec opam exec --switch="$switch" -- "$@"
