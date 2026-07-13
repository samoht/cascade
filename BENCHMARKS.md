# Benchmarks

`cascade --minify` optimises the metric that matters for shipped CSS: the
compressed transfer size. Repeated declaration text is nearly free once
gzipped, so the default objective (`--objective=transfer`) keeps a
global-factoring result only when it also shrinks the estimated gzip size;
`--objective=raw` optimises uncompressed bytes instead, for output that ships
uncompressed (inline style attributes, email HTML).

Measured on the SatCSS corpus (Hague, Lin, Hong, TOPLAS 2019) of real-world
stylesheets. Sizes are gzip -9 bytes of the emitted CSS; wall clock is the
median over 5 runs.

| fixture | cascade `--minify` | csso | lightningcss | esbuild | cssnano |
|---|---|---|---|---|---|
| github | **34,289** / 570 ms | 34,887 / 120 ms | 34,420 / 10 ms | 34,736 / 10 ms | 34,729 / 240 ms |
| guardian | **25,979** / 290 ms | 26,653 / 100 ms | 26,008 / 0 ms | 27,616 / 10 ms | 26,408 / 220 ms |
| youtube | **33,779** / 680 ms | 34,841 / 130 ms | 34,321 / 10 ms | 34,782 / 10 ms | 33,930 / 300 ms |
| netflix | **29,497** / 740 ms | 33,102 / 140 ms | 31,948 / 10 ms | 33,820 / 10 ms | 31,411 / 280 ms |

Cascade emits the smallest gzip output on every fixture here, and the smallest
brotli output on all but guardian (where Lightning CSS keeps a 0.6% edge).
`--aggressive` changes little under the default objective: the transfer gate
already discards factoring that would grow the compressed output.

With `--objective=raw` the objective flips to uncompressed bytes and cascade
emits the smallest raw output on every fixture too (github 178,042 vs csso's
180,825; netflix 172,500 under `--aggressive` vs cssnano's 218,017).

Lightning CSS and esbuild are well over an order of magnitude faster but emit
0.4-15% more compressed bytes; csso and cssnano sit in the same wall-clock band
as cascade and emit more bytes.

## Reproducing

The SatCSS corpus is regenerated locally rather than vendored: the upstream
repository carries no licence for redistributing the website CSS snapshots (see
[test/interop/satcss/](test/interop/satcss/)). Run the comparison with a freshly
built CLI, since the installed `_opam/bin/cascade` may be stale:

```bash
RUNS=5 CASCADE=_build/default/bin/main.exe bench/minifier_comparison.sh
```
