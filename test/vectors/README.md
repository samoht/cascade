# Vendored conformance vectors

Verbatim copies of upstream test inputs. Don't hand-edit.

## What's here

- `wpt/css-syntax/` — CSS Syntax Module Level 3 tests from
  [web-platform-tests](https://github.com/web-platform-tests/wpt),
  commit `f900489fc` (2026-04-23). Covers tokenization, ident code-point
  ranges, `urange`, serialization, unclosed-construct recovery.
- `utf8/UTF-8-test.txt` — Markus Kuhn's UTF-8 decoder stress test
  ([source](https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt)),
  CC BY 4.0. Boundary and malformed-sequence cases for whatever decoder
  layer sits in front of the tokenizer.

## Out of scope, not imported

- `@charset` with legacy encodings (Shift_JIS, Big5, EUC-*, windows-125x, …).
  Those are byte-stream decoder tests; Cascade is UTF-8-only (see the
  top-level README). The WPT `encoding/` tree and `css-syntax/charset/`
  are not mirrored here.
- Computed-value / layout tests. We're an AST library, not a UA.

## How these are used

Nothing is wired up yet. The vectors are in place so we can red/green
against them as `Uutf`-based decoding and §3/§4 code-point tables go in.
WPT tests are HTML + `testharness.js`, so a follow-up harness will
extract CSS inputs and expected outcomes into Alcotest cases. The Kuhn
file is raw bytes and will be read directly.

## Failure policy

Failing vectors are visible failures on `dune runtest`. No `.skip`
files, no known-failures list — a vector either passes or is a bug to
fix. If a vector turns out to require something structurally outside
the library (e.g. a rendering engine), the harness refuses to load it
with a reason, rather than loading and silently passing.

## Updating

```sh
make update-vectors
```

Refreshes both trees from the pins at the top of the `Makefile`. To move
to a newer WPT revision, bump `WPT_COMMIT`, rerun, and update the SHA in
this README. Any diff against upstream at the pinned commit should be
zero.
