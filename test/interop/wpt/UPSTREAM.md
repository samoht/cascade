# Web Platform Tests CSS Syntax provenance

- Project: <https://github.com/web-platform-tests/wpt>
- Revision: `f900489fca393464f3379d7952d227997318b851`
- Imported material: `css/css-syntax/`, excluding `charset/` because Cascade
  receives decoded UTF-8 rather than implementing browser byte sniffing
- License: BSD 3-Clause; the authoritative upstream `LICENSE.md` at the pinned
  revision is preserved in [LICENSE.md](LICENSE.md).

Normal tests replay only the committed files under `traces/css-syntax/`.
Refresh the sparse snapshot with:

```sh
REGEN=1 dune build @test/interop/wpt/regen-traces
```
