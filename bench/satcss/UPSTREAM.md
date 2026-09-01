# SatCSS benchmark provenance and redistribution status

- Tool: <https://github.com/matthewhague/sat-css-tool>
- Revision: `1d983625032b224e80f7659e32285801167e16e0`
- Corpus: the upstream repository's `benchmarks/*-stripmq.css` website
  snapshots

The pinned repository contains no `LICENSE`, `COPYING`, or license field in
`pyproject.toml`, and its README gives no redistribution terms for either the
tool or the website snapshots. Cascade therefore does not vendor these files
or consume them in normal tests. They remain an opt-in local benchmark:

```sh
REGEN=1 dune build @bench/satcss/regen-traces
dune exec bench/satcss/compare.exe
```
