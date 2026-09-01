# css-minify-tests provenance

- Project: <https://github.com/keithamus/css-minify-tests>
- Revision: `83f224fbf9db27a81f398c1425e6000f22b4b5c9`
- Imported material: `tests/**/{source.css,expected.css,README.md}`
- License: the upstream README at that revision says `MIT`; it has no
  dedicated license file. The exact upstream notice is preserved in
  [LICENSE.md](LICENSE.md).

The committed files under `traces/tests/` are the complete normal-test input.
The upstream checkout is needed only to refresh them:

```sh
REGEN=1 dune build @test/interop/css-minify-tests/regen-traces
```
