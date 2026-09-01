# Lightning CSS minifier corpus provenance

- Project: <https://github.com/parcel-bundler/lightningcss>
- Revision: `df63db2c51c49a6a82f795f3a8988a3cd08ea03a`
- Imported material: the `minify_test_with_options` source/expected pairs from
  `src/lib.rs`, captured into the committed `traces/minify.pairs` replay file
- License: Mozilla Public License 2.0; the authoritative upstream text at the
  pinned revision is preserved in [LICENSE](LICENSE).

The same trace caches oracle output from these exact npm packages:

- `esbuild` 0.28.1
- `clean-css-cli` 5.6.3
- `csso-cli` 4.0.2 (CSSO 5.0.5)
- `cssnano-cli` 1.0.5 (cssnano 3.10.0)
- `lightningcss-cli` 1.32.0

`scripts/package-lock.json` pins their complete dependency graph. Normal tests
read only the committed trace; Cargo, npm, and the oracle CLIs run only during
an explicit refresh. Cached failure diagnostics retain the tool error but
normalize the build-local npm path and host Node version:

```sh
REGEN=1 dune build @test/interop/lightning/regen-traces
```
