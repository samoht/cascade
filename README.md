# Cascade

A command-line tool for **formatting**, **minifying**, **inlining**, and
**structurally diffing** CSS. Ships one binary (`cascade`) and, for OCaml users,
the library it is built on.

```text
$ cascade --minify style.css > style.min.css
$ cascade diff a.css b.css
```

Cascade aims to be the smallest minifier on real-world stylesheets while
staying competitive on speed: smallest output of any tool tested on three of
the four SatCSS fixtures with `--minify`, and on every fixture with
`--aggressive`. The diff subcommand reads structure, so refactors that move
rules around without changing semantics show up as a no-op rather than as a
wall of red and green. Both modes round-trip through a typed CSS AST, so the
output is always valid CSS.

## Install

On macOS, via the Homebrew tap:

<!-- $MDX skip -->
```bash
brew install samoht/tap/cascade
```

For OCaml/opam users (installs the CLI *and* the `cascade` library):

<!-- $MDX skip -->
```bash
opam install cascade
```

From source (OCaml 5.2+, opam, dune):

<!-- $MDX skip -->
```bash
git clone https://github.com/samoht/cascade.git
cd cascade
opam install . --with-test
cascade --help
```

## `cascade fmt`: format and minify

```text
cascade fmt [OPTIONS] [FILE]
```

`fmt` is the default subcommand, so `cascade FILE` and `cascade fmt FILE` do the
same thing. It reads a CSS file (or stdin when no file or `-` is given), parses
it into a structured CSS model, and writes formatted CSS to stdout.

Without flags it pretty-prints. With `--minify` it runs the standard safe
transforms (deduplication, rule merging, selector grouping, empty-rule
elimination, nested-rule flattening, shorthand composition, color
canonicalization) and emits minified output.

This is a parser/printer round trip, not a byte-preserving formatter: comments
are discarded during parsing, and empty rules and invalid declarations are
dropped in both pretty and minified output.

### Common recipes

<!-- $MDX skip -->
```bash
cascade style.css > style.formatted.css                            # pretty-print
cascade --minify style.css > style.min.css                         # minify
cascade --minify --aggressive style.css > style.min.css            # smallest output
cascade --inline-imports --inline-vars --minify style.css > out.css # bundle + minify
cascade --inline-vars --keep-vars=theme,brand style.css > themed.css
cat style.css | cascade -                                          # read stdin
```

### Flags

| Flag | Purpose |
|---|---|
| `-m, --minify` | Minify the output. Local linear rewrites always run; the expensive global factoring fixpoint runs only when its preflight predicts useful savings. |
| `--aggressive` | Force the global factoring fixpoint and re-run the top-level pipeline until the AST stops changing (capped at 5 iterations). Trades roughly 10-20x wall clock for the last few percent of bytes. Has no effect without `--minify`. |
| `--lossless` | Disable color approximation under `--minify`. Exact color canonicalization still runs; static modern color-space values and `color-mix()` stay functional. Has no effect without `--minify`. |
| `--enforce-spec` | Drop the evergreen-browser baseline target. Cascade still serializes to the shortest CSS form it knows, but it keeps every `@supports` and `supports()` guard unless the CSS text and spec alone prove the rewrite. Has no effect without `--minify`. |
| `--scope=fragment\|stylesheet` | How much surrounding CSS context to assume. `fragment` (default) treats the input as an excerpt; `stylesheet` asserts the input is the whole author CSS graph and unlocks partial-coverage shorthand synthesis. |
| `--flatten-nesting` | Desugar nested rules into flat top-level rules for browsers that pre-date CSS Nesting. By default cascade preserves nesting since modern browsers parse it natively and it is usually shorter. |
| `--inline-imports` | Resolve `@import` against files relative to the input. Closed-world: assumes you control file resolution. |
| `--inline-vars` | Substitute `var(--name)` references with their declared values, then drop unused custom properties. Closed-world: assumes no runtime mutation. |
| `--keep-vars=NAMES` | Comma-separated custom-property names to preserve under `--inline-vars`. |
| `--profile` | Print per-pass timings of the optimizer to stderr after the run. Useful to triage which pass dominates on a slow input. Has no effect without `--minify`. |
| `--memtrace=FILE` | Write a memtrace allocation trace to FILE. |
| `-q, --quiet` / `-v, --verbose` | Standard verbosity controls. |

### How it compares

Cascade's `--minify` is a fast, safe default; `--aggressive` is the
size-optimal mode. Benchmarked on the SatCSS corpus (Hague, Lin, Hong, TOPLAS
2019), median wall clock over 5 runs, against the major minifiers:

| fixture | cascade `--minify` | cascade `--aggressive` | csso | lightningcss | esbuild | cssnano |
|---|---|---|---|---|---|---|
| github | 181,408 / 40 ms | **173,194** / 1,280 ms | 180,825 / 120 ms | 182,772 / 10 ms | 183,965 / 10 ms | 182,039 / 240 ms |
| guardian | **158,382** / 410 ms | **158,382** / 880 ms | 168,671 / 100 ms | 170,266 / 0 ms | 190,082 / 10 ms | 166,617 / 220 ms |
| youtube | **220,324** / 40 ms | **213,955** / 1,270 ms | 224,878 / 130 ms | 224,538 / 10 ms | 229,566 / 10 ms | 221,002 / 270 ms |
| netflix | **192,151** / 60 ms | **177,945** / 1,730 ms | 223,780 / 140 ms | 231,113 / 10 ms | 247,294 / 10 ms | 218,017 / 290 ms |

Cascade `--aggressive` emits the smallest output on every fixture. Cascade
`--minify` (default) is smallest on three of four fixtures; on github it
trails csso by 583 bytes (0.3%) at a third of csso's wall clock. Lightning CSS
and esbuild are 4-40x faster than cascade `--minify` but emit 1-30% more
bytes; csso and cssnano are in the same wall-clock band as cascade `--minify`
on most fixtures but emit more bytes.

## `cascade diff`: structural CSS diff

```text
cascade diff [--color=WHEN] [--diff=MODE] FILE1 FILE2
```

Compares two CSS files through the parsed CSS structure rather than
character-by-character: added, removed, modified, and reordered rules are
detected structurally, and property value changes are reported in terms of CSS
values. Identical files exit 0; differences exit 1, making `cascade diff`
usable as a CI check.

`--diff=MODE` controls what counts as "no difference":

- `auto` (default): structural diff; falls back to a string diff when the
  parsed structures match but the strings don't, so cosmetic differences
  (whitespace, comment position) still surface.
- `tree`: structural diff only; formatting-only differences collapse to
  "identical".
- `string`: character-level comparison.
- `semantic`: passes when the two inputs share cascade's canonical minified
  form. This is not full CSS semantic equivalence: equivalent shorthand
  decompositions and cascade-affecting rule reorderings are not modelled.

<!-- $MDX skip -->
```bash
cascade diff reference.css output.css
cascade diff --diff=tree reference.css output.css
cascade diff --diff=semantic reference.css output.css
NO_COLOR=1 cascade diff reference.css output.css
```

## In a build, CI, or pre-commit hook

A common shape: minify on build, check formatting in CI, diff structurally
in pre-commit hooks.

<!-- $MDX skip -->
```bash
# build step
cascade --minify --inline-vars src/style.css > dist/style.min.css

# CI: fail when the committed file is not the formatted version
cascade src/style.css > /tmp/fmt.css
cascade diff --diff=tree src/style.css /tmp/fmt.css

# pre-commit: catch semantic-only changes
cascade diff --diff=semantic origin/main:src/style.css src/style.css
```

The exit code is 0 when the inputs are identical under the chosen mode and 1
otherwise, so cascade slots into any tool that branches on exit codes (`git`
hooks, `make`, GitHub Actions, ...). The `--minify` pipeline is fast enough
that a 200 KB stylesheet costs well under 100 ms on the SatCSS corpus;
`--aggressive` trades roughly an order of magnitude of wall clock for the
last few percent of bytes and fits a release build rather than a watcher
loop.

## `--minify` policy

Cascade picks the shortest behavior-preserving spelling at every choice point.
Where the CSS spec and browser-compatible recovery rules permit several valid
serializations, cascade chooses the shortest valid one.

### What runs

Value-level rewrites:

- **Colors:** hex when no longer than the name (`black` -> `#000`,
  `blue` -> `#00f`; `red` stays a name). Modern color functions
  (`lab`/`lch`/`oklab`/`oklch`/`color()`) fold to shorter sRGB only within the
  ΔE<sub>OK</sub> budget below.
- **Numbers and lengths:** drop leading/trailing zeros (`0.5` -> `.5`); convert
  compatible units only when shorter (`12pt` stays `12pt`).
- **Math:** `calc()`, `hypot()`, etc. fold constant subexpressions only when
  the serialized result is exact (`calc(100%/3)` stays `calc(100%/3)`).
- **Whitespace:** elided at safe token boundaries (`100% 0` -> `100%0`).

Selector-level rewrites:

- Branches sorted into cascade's canonical order
  (`div,.class,#id` -> `#id,.class,div`).
- Pseudo-elements in legacy single-colon form (`::before` -> `:before`).

Rule-level rewrites:

- Adjacent same-selector merging and identical-body combining across
  non-overlapping intermediates, with specificity and importance reasoning.
- `factor_anchor` extracts shared declarations into comma-list rules when
  cascade-safe and net smaller.
- Shorthands with unordered components serialize in cascade's canonical order
  (`animation:1s slide` -> `animation:slide 1s`).
- Dead-rule elimination, `@layer` consolidation, and
  `@supports`/`@media`/`@container` flattening when the condition is satisfied
  for the evergreen target.
- MQ4 range syntax when shorter (`(min-width:48px)` -> `(width>=48px)`).

These rules compose wherever cascade has a typed CSS value. An unregistered
custom-property value stays an opaque token stream, with one exception: a
substream whose type is fixed by its own syntax. A complete colour function
(`oklab(...)`, `color-mix(...)`, `rgb(...)`, ...) or a hex colour (`#abc`) is
unconditionally a colour in every `var()` substitution site, so it folds to its
shortest spelling and the fold preserves every rendered result. The fold never
produces a bare colour keyword: a name like `red` is also a valid
`<custom-ident>`, so it stays distinct from `#f00` even though it is shorter,
and hex stays hex. The fold changes the exact token string a script reads back
via `getPropertyValue`; cascade does not treat that byte-exact CSSOM
serialization as an observable to preserve.

### Color approximation

Cascade folds colors only within `0.002` ΔE<sub>OK</sub> (the CSS Color 4
Delta-E metric for Oklab/OkLCh). Alpha is separate: functional alpha rounds
to three decimals (`0.0005` tolerance); the 8-bit alpha of a hex fold is
its canonical spelling and is not gated by that tolerance.

Pass `--lossless` to keep color values exact: hex/named canonicalization and
modern-syntax rewrites still run, but channel rounding, within-budget
modern-space folds, and static `color-mix()` resolution are disabled.

### Scope

`--minify` is closed over the CSS text but open over runtime layout state.
Cascade uses source order, the cascade, dependencies, and dead-code reasoning,
but does not assume DOM shape, writing mode, computed direction, user styles,
or runtime custom-property mutation. The output stays sound when the minified
stylesheet is embedded in a larger page.

`--scope=stylesheet` asserts the input is the whole author CSS graph (after
`@import` resolution). The optimizer can then synthesize a partial-coverage
shorthand whose omitted longhand resets are proved not to disturb a prior
write the optimizer can't see.

### Target browsers

The default minify targets maintained evergreen browsers. Cascade may treat
baseline feature queries like `@supports(display:flex)` as true and remove the
wrapper, and may use the HTML direction model to shorten `:not(:dir(ltr))` to
`:dir(rtl)`.

`--enforce-spec` drops those facts. Cascade still serializes to the shortest
CSS form it knows, but feature queries stay and the direction model is not
assumed.

## CSS specification coverage

Cascade targets selected **CSS Level 3, Level 4, and Level 5** modules. Its
conformance target is CSS parsing, ASTs, printing, transforms, diffs, and
optimization; it is not a complete web-platform runtime.

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting, specificity |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | ~30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colors, 15 color spaces |
| [Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/) | `@media` (recovering a failed condition parse as `not all`), `@supports` property and selector checks, `@when` / `@else`, `@supports-condition` |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords, `all` reset semantics in the optimizer |
| [Nesting Module](https://www.w3.org/TR/css-nesting-1/) | Nested rules with `&`, nested `@media` and `@supports` |
| [Container Queries Level 5](https://www.w3.org/TR/css-conditional-5/#container-queries) | `@container` with size queries and typed `style()` / `scroll-state()` queries, including range operators |
| [Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/) | `var()` parsing/printing, typed fallbacks, theme/default substitution, `@property` registration |
| [Fonts Level 4](https://www.w3.org/TR/css-fonts-4/) | `@font-face` descriptors |
| [Animations Level 1](https://www.w3.org/TR/css-animations-1/) | `@keyframes`, `@starting-style` |

Typed CSS properties cover the box model, flexbox, grid, logical properties,
typography, borders, backgrounds, gradients, transforms, transitions,
animations, filters, masks, anchor positioning, view transitions, and
vendor-prefixed longhands. Together these cover the stylesheet surface
typically emitted by CSS generators, component libraries, and utility
frameworks.

## Limitations

- **UTF-8 input only.** Cascade parses already-decoded UTF-8 text. BOM
  handling, HTTP charset fallback, and `@charset "...";` byte sniffing are
  the caller's job; legacy encodings (`Shift_JIS`, UTF-16, ...) must be
  decoded upstream.
- **No runtime subsystems.** No implicit DOM, CSSOM, network loader, layout
  tree, renderer, or computed-style engine. CSS syntax for those features
  parses and prints; analyses that need runtime data take an explicit closed
  context.
- **Comments and source positions** are not preserved across the
  parser/printer round trip.
- **Unregistered custom properties** stay opaque token streams to the
  optimizer, apart from complete colour functions inside them, which fold to
  their shortest spelling.

## Using cascade as a library

The CLI is a thin wrapper over the public OCaml API exposed by the `cascade`
opam package.

```ocaml
# open Cascade.Css;;
# let button =
    rule ~selector:(Selector.class_ "btn")
      [ display Inline_block
      ; background_color (hex "#3b82f6")
      ; color (hex "#ffffff")
      ; padding [ Rem 0.5 ]
      ; border_radius (radius (Rem 0.375))
      ]
  in to_string (v [ button ]);;
- : string =
".btn {\n  display: inline-block;\n  background-color: #3b82f6;\n  color: #ffffff;\n  padding: .5rem;\n  border-radius: .375rem;\n}"
```

Output:

```css
.btn {
  display: inline-block;
  background-color: #3b82f6;
  color: #ffffff;
  padding: .5rem;
  border-radius: .375rem;
}
```

Properties, values, and selectors are sealed OCaml ADTs, so invalid
constructions are caught at compile time. Structural transforms (`fold`,
`map`, `sort`, `flatten_nesting`), `Css.inline_imports`, and
`Css.optimize ?flatten_nesting ?aggressive ?lossless ?enforce_spec ?scope` are
the main entry points for AST-level work. Transforms that need information
beyond CSS text take an explicit closed `Css.Context.t` rather than reading
ambient runtime state.

Structural diff lives in the separate `cascade.diff` sub-library
(`Cascade_diff.Css_compare`, `Cascade_diff.Tree_diff`,
`Cascade_diff.String_diff`); it is what `cascade diff` is built on.

### Parsing modes

`Css.of_string ~strict:false s` always returns
`Ok { stylesheet; warnings }`, with `warnings` listing recovered syntax and
declaration issues. `~strict:true` errors when the lenient parse would have
warned. When both succeed, their minified outputs are identical.

### Small runtime footprint

The core `cascade` library links only
[uutf](https://erratique.ch/software/uutf) and the OCaml runtime; it does not
pull `fmt`, so js_of_ocaml embedders stay lean. A local jsoo build that
parses and minifies one stylesheet compressed to under 200 KiB
(`--opt 3 --no-source-map`, size-oriented runtime flags).

## Development and testing

Three oracle corpora cover parser conformance and minified-output behaviour:

- **WPT parser conformance.** The `css/css-syntax/` subset of the [Web
  Platform Tests](https://github.com/web-platform-tests/wpt) is vendored
  under [test/interop/wpt/traces/css-syntax/](test/interop/wpt/traces/css-syntax/)
  and replayed by [test/interop/wpt/test.ml](test/interop/wpt/test.ml). Every
  CSS fragment in every `<style>`, `style="..."`, `support/*.css`, and
  `parseRule(\`...\`)` site goes through `Css.of_string`. A test fails when
  cascade rejects what browsers accept or accepts what browsers reject; there
  is no skip list. Refresh with `dune build @test/interop/wpt/regen-traces`.
- **Lightning CSS minify oracle.** Cascade's `--minify` output is compared
  with cached answers from `esbuild`, `cleancss`, `csso`, `cssnano`, and
  `lightningcss-cli` over the Lightning CSS test inputs
  ([trace](test/interop/lightning/traces/minify.pairs), regenerated via
  `dune build @regen-traces`). Each record is treated as the complete
  stylesheet (`scope: Stylesheet`). A case passes when cascade's output is no
  longer than the shortest *valid* cached answer; oracle answers that
  crashed, fail to round-trip, or change the parsed shape are excluded and
  logged.
- **keithamus/css-minify-tests.** A vendor-neutral hand-curated set of
  `source.css`/`expected.css` pairs covering 29 CSS feature categories. Each
  pair must equal the upstream `expected.css` after cascade's documented
  normalizations.

A fourth corpus, [SatCSS](test/interop/satcss/) (Hague-Lin-Hong's CSS
minification benchmark), is regenerated locally and not vendored: the upstream
repository carries no licence for redistributing the website CSS snapshots.

## References

**Other CSS tooling.** [Lightning CSS](https://github.com/parcel-bundler/lightningcss)
(Rust), [esbuild](https://github.com/evanw/esbuild) (Go), and the JS
optimizers [CSSO](https://github.com/css/csso),
[cssnano](https://github.com/cssnano/cssnano), and
[clean-css](https://github.com/clean-css/clean-css) all serve as cached
minifier oracles in the test suite.
[PostCSS](https://github.com/postcss/postcss) and
[CSSTree](https://github.com/csstree/csstree) are the broader JS
parser/AST projects worth comparing against. Earlier OCaml CSS work:
[css-parser](https://github.com/astrada/ocaml-css-parser) and
[OCaml-css](https://zoggy.frama.io/ocaml-css/).

**Optimization research.**
[Hague, Lin, Hong (2018)](https://arxiv.org/abs/1812.02989) formalize rule
merging as a CSS-graph problem: a merge is legal only when selector
intersection and the intervening cascade dependencies preserve semantics.
[Visscher, Punt, Zaytsev (2016)](https://grammarware.net/text/2016/aba-css.pdf)
catalogue A-B*-A patterns (a property set, overridden, then restored), useful
adversarial input for optimizers since source order, specificity,
inheritance, and implicit defaults all affect whether a rewrite is sound.
[CILLA](https://github.com/saltlab/cilla) (Mesbah, Mirshokraie) analyses
runtime DOM-CSS matching to flag dead selectors at the layout level, a useful
reference for what an AST-level dead-rule check can and cannot claim.

**Specifications cascade implements:**
[Syntax 3](https://www.w3.org/TR/css-syntax-3/),
[Selectors 4](https://www.w3.org/TR/selectors-4/),
[Values 4](https://www.w3.org/TR/css-values-4/),
[Color 4](https://www.w3.org/TR/css-color-4/),
[Cascade 5](https://www.w3.org/TR/css-cascade-5/),
[Conditional 5](https://www.w3.org/TR/css-conditional-5/),
[Nesting 1](https://www.w3.org/TR/css-nesting-1/).

## Licence

[ISC](LICENSE)
