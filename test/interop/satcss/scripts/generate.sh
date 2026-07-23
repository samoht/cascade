#!/usr/bin/env bash
# Regenerate the SatCSS oracle: traces/<site>/{input,expected}.css
#
# Oracle: matthewhague/sat-css-tool (Hague, Lin, Hong; "CSS Minification via
# Constraint Solving", TOPLAS 2019) - semantics-preserving rule merging via
# Max-SAT. We run the tool through its own CLI (never reimplement it) on each
# benchmark stylesheet and record:
#   input.css     the corpus stylesheet (media queries already stripped)
#   expected.css  the tool's --write-compact (minified) refactoring of it
#
# Reproducibility: the tool AND its corpus are pinned by commit below. The
# Python bindings are pinned in requirements.txt; the tool's own pins predate
# Python 3.14, so a 3.14-compatible set is used instead - the oracle's
# determinism comes from the pinned tool commit, not the host bindings.
#
# traces/ is gitignored on purpose: the corpus is third-party website CSS with
# no redistribution license, so the oracle is generated locally and never
# committed. Single trigger: REGEN=1 dune build @regen
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel 2>/dev/null || true)"
if [ -n "$repo_root" ] && [[ "$script_dir" == "$repo_root"/_build/default/* ]]; then
  SCRIPT_DIR="$repo_root/${script_dir#"$repo_root/_build/default/"}"
else
  SCRIPT_DIR="$script_dir"
fi
TRACE_DIR_ARG="${1:-$SCRIPT_DIR/../traces}"
mkdir -p "$TRACE_DIR_ARG"
TRACE_DIR="$(cd "$TRACE_DIR_ARG" && pwd)"

command -v git >/dev/null || { echo "git not on PATH" >&2; exit 1; }
command -v python3 >/dev/null || { echo "python3 not on PATH" >&2; exit 1; }

SATCSS_REPO="https://github.com/matthewhague/sat-css-tool.git"
SATCSS_COMMIT="1d983625032b224e80f7659e32285801167e16e0"

cd "$SCRIPT_DIR"

# 1. Python venv with the host bindings.
if [ ! -d .venv ]; then
  python3 -m venv .venv
  .venv/bin/pip install -q --upgrade pip
  .venv/bin/pip install -q -r requirements.txt
fi
PY="$SCRIPT_DIR/.venv/bin/python"

# 2. Pinned checkout of the tool (its repo also carries the benchmark corpus).
tool="$SCRIPT_DIR/.tool"
if [ ! -e "$tool/main.py" ]; then
  rm -rf "$tool"
  git init -q "$tool"
  git -C "$tool" remote add origin "$SATCSS_REPO"
  git -C "$tool" config core.sparseCheckout true
  printf 'satcss/*\nmain.py\nbenchmarks/*\n' > "$tool/.git/info/sparse-checkout"
  git -C "$tool" fetch --depth=1 origin "$SATCSS_COMMIT" -q
  git -C "$tool" checkout -q FETCH_HEAD
fi
# The tool shells out to ./satcss/z3; point it at the z3-solver wheel binary.
ln -sf "$SCRIPT_DIR/.venv/bin/z3" "$tool/satcss/z3"

# 3. Run the tool over the corpus.
find "$TRACE_DIR" -mindepth 1 -not -name '.gitignore' -delete
ok=0
skip=0
for src in "$tool"/benchmarks/*-stripmq.css; do
  site=$(basename "$src" -stripmq.css)
  out="$TRACE_DIR/$site"
  mkdir -p "$out"
  cp "$src" "$out/input.css"
  if (cd "$tool" && timeout 300 "$PY" main.py -o --write-compact \
        --file="$out/expected.css" "$src" >/dev/null 2>&1) \
     && [ -s "$out/expected.css" ]; then
    ok=$((ok + 1))
  else
    echo "  skip $site (sat-css-tool timed out or errored)" >&2
    rm -rf "$out"
    skip=$((skip + 1))
  fi
done
echo "wrote $ok SatCSS oracles to $TRACE_DIR ($skip skipped)"
