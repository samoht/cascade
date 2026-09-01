# UTF-8 stress-test provenance

- Author/project: Markus Kuhn, UCS examples
- Source: <https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt>
- Upstream date in the file header: `2015-08-28`
- Pinned SHA-256:
  `b51cfe9a8d2689c90b10a13a3624092d546e0837c6ff835b6e5d713c5749c8c6`
- License: CC BY 4.0, as stated by the upstream file header and preserved in
  [LICENSE.md](LICENSE.md).

The generator rejects the download if its digest differs. Normal tests read
only the committed `traces/UTF-8-test.txt`:

```sh
REGEN=1 dune build @test/interop/utf8/regen-traces
```
