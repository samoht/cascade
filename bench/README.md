# Benchmarks

Scripts that regenerate the numbers in [BENCHMARKS.md](../BENCHMARKS.md) and in
the "A Typed CSS Toolkit in OCaml" blog post. Build the CLI first, since the
installed `_opam/bin/cascade` may be stale:

```bash
dune build bin/main.exe
```

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

## Corpus

The SatCSS corpus (Hague, Lin, Hong; TOPLAS 2019) is regenerated locally rather
than vendored: the upstream repository carries no licence to redistribute the
website CSS snapshots. See [../test/interop/satcss/](../test/interop/satcss/).
