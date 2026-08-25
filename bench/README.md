# Benchmarks

Scripts that regenerate the numbers in [BENCHMARKS.md](../BENCHMARKS.md) and in
the "A Typed CSS Toolkit in OCaml" blog post. Build the CLI first, since the
installed `_opam/bin/cascade` may be stale:

```bash
dune build bin/main.exe
```

## `differential/run.sh` -- one differential suite

The differential suite replaces per-fix scratch drivers. Its named profiles
share build orchestration, deterministic observations, normalization and
reporting:

```bash
# Compare serialisation/inlining over the interop corpus.
bench/differential/run.sh outputs --baseline ../cascade-main

# Compare internal optimiser candidate sets (quick is suitable while iterating).
bench/differential/run.sh candidates --baseline ../cascade-main --quick

# Render-check every committed HTML fixture through an external minifier.
bench/differential/run.sh render --tool lightningcss --output /tmp/render-diff

# The existing interleaved SatCSS performance and byte comparison.
bench/differential/run.sh corpus -b ./old/main.exe
```

`outputs` and `candidates` expect the suite to exist in both compared trees;
stack a work branch on the suite commit before measuring it. They run the same
driver built against each tree and fail on the first differing record stream.
`render` reuses `test/inline`'s pinned-browser computed-style comparison and
writes raw and canonically filtered TSV artifacts. It reports the external
tool version; set `MINIFIER_VERSION` when using an opaque `--command`. Add `--pages` only after
`test/inline/fetch.sh` has frozen the optional real-page corpus. External tools
are developer dependencies and are never part of ordinary `dune test`.

## `corpus_sweep.sh` -- minify all 504 SatCSS fixtures

```bash
bench/corpus_sweep.sh                      # every fixture
bench/corpus_sweep.sh -n 20                # slowest 20 in the summary
bench/corpus_sweep.sh -b ./old/main.exe    # A/B against another binary
bench/corpus_sweep.sh -j 4                 # 4 files at a time
bench/corpus_sweep.sh github guardian      # only matching fixtures
```

Reports total CPU time, raw and gzip bytes, and the slowest fixtures. Use it
to check that an optimiser change holds across the corpus: a change measured
on one or two stylesheets tracks their shape rather than CSS in general. With
`-b` it interleaves the two binaries file by file, so machine load affects
both alike, and reports whether their output is byte-identical -- a change
that moves both the time and the bytes is a trade, not a speedup.

## `blog_tables.sh` -- every table in the blog post

```bash
bench/blog_tables.sh             # all sections
bench/blog_tables.sh rewrites    # just one section
RUNS=5 bench/blog_tables.sh
```

Sections: `versions rewrites size speed lossless reminify`. The re-minification
table is skipped unless `REMIN_DIR` points at a directory of `.min.css` files.

Requirements on `PATH`: `hyperfine`, `cascade` (or `CASCADE=<path>`), `csso`,
`lightningcss`, `esbuild`, `postcss` + `cssnano`. The `cssnano` binary published
on npm is `cssnano-cli`, which pins cssnano 3.10.0 (2017); the script drives the
current plugin through `postcss-cli` instead.

## `minifier_comparison.sh` -- cross-minifier size and speed

```bash
RUNS=5 CASCADE=_build/default/bin/main.exe bench/minifier_comparison.sh
```

## `source-order/bench_source_order.ml` -- rule-graph allocation scaling

```bash
dune exec bench/source-order/bench_source_order.exe
```

Reports allocated words per graph build while doubling same-specificity rule
sets. Its four shapes separate distinct declarations from identical ones and
class selectors from ID and type selectors, making regressions in source-order
candidate indexing visible without including parse or startup allocation.

## Corpus

The SatCSS corpus (Hague, Lin, Hong; TOPLAS 2019) is regenerated locally rather
than vendored: the upstream repository carries no licence to redistribute the
website CSS snapshots. See [../test/interop/satcss/](../test/interop/satcss/).
