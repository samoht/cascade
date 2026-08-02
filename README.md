# Cascade

A command-line tool for **formatting**, **minifying**, **inlining**, and
**structurally diffing** CSS. Ships one binary (`cascade`) and, for OCaml users,
the library it is built on.

```text
$ cascade --minify style.css > style.min.css
$ cascade diff a.css b.css
```

Cascade works from a typed CSS AST rather than the raw text, so every command
emits valid CSS by construction and reasons about the cascade instead of
guessing from bytes. `cascade fmt --minify` applies only cascade-safe transforms
(deduplication, rule merging, selector grouping, colour and value
canonicalisation), and optimises for the bytes that actually ship: compressed
transfer size rather than raw length. `cascade diff` compares the parsed
structure, so a refactor that reorders rules or regroups declarations without
changing what they compute reads as no difference rather than a wall of red and
green. The same engine backs the `cascade` OCaml library.

On the SatCSS corpus of real-world stylesheets, cascade is competitive on both
size and speed; the head-to-head numbers are in [BENCHMARKS.md](BENCHMARKS.md).

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
elimination, nested-rule flattening, shorthand composition, colour
canonicalisation) and emits minified output.

`calc()` arithmetic folds only when the fold is exactly value-preserving:
`calc(100/4)` becomes `25`, while `calc(1.75/1.125)` stays as written because
14/9 has no finite decimal form. Multiplication folds unconditionally, being
closed over finite decimals; division folds only when the quotient is exact. The
same rule governs a `calc()` inside a custom-property value, whose token stream
is otherwise left opaque.

This is a parser/printer round trip, not a byte-preserving formatter: comments
are discarded during parsing, and empty rules and invalid declarations are
dropped in both pretty and minified output.

### Common recipes

<!-- $MDX skip -->
```bash
cascade style.css > style.formatted.css                            # pretty-print
cascade --minify style.css > style.min.css                         # minify
cascade --minify --objective=raw style.css > style.min.css         # smallest uncompressed
cascade --inline-imports --inline-vars --minify style.css > out.css # bundle + minify
cascade --inline-vars --keep-vars=theme,brand style.css > themed.css
cat style.css | cascade -                                          # read stdin
```

### Flags

| Flag | Purpose |
|---|---|
| `-m, --minify` | Minify the output. Local linear rewrites always run; the expensive global factoring fixpoint runs only when its preflight predicts useful savings. The top-level pipeline re-runs until the AST stops changing (capped at 5 iterations), so the output is a fixed point: rule-order canonicalisation can expose a merge a single pass would miss. |
| `--objective=transfer\|raw` | Size metric `--minify` optimises for. `transfer` (default) keeps a global-factoring result only when it also shrinks the estimated gzip (DEFLATE) size of the output, since repeated declaration text is nearly free once compressed. `raw` keeps every raw-byte win and drives the factoring fixpoint to convergence, the right objective when the output ships uncompressed (inline style attributes, email HTML), at roughly 10-30x the wall clock. Has no effect without `--minify`. |
| `--lossless` | Disable colour approximation under `--minify`. Exact colour canonicalisation still runs; static modern colour-space values and `color-mix()` stay functional. Also sorts each rule's declarations into a canonical cross-rule order (keeping cascade-significant pairs in place) so gzip back-references line up. Has no effect without `--minify`. |
| `--enforce-spec` | Drop the evergreen-browser baseline target. Cascade still serialises to the shortest CSS form it knows, but it keeps every `@supports` and `supports()` guard, and every vendor-prefixed declaration, unless the CSS text and spec alone prove the rewrite. Has no effect without `--minify`. |
| `--scope=fragment\|stylesheet` | How much surrounding CSS context to assume. `fragment` (default) treats the input as an excerpt; `stylesheet` asserts the input is the whole author CSS graph and unlocks partial-coverage shorthand synthesis. |
| `--flatten-nesting` | Desugar nested rules into flat top-level rules for browsers that pre-date CSS Nesting. By default cascade preserves nesting since modern browsers parse it natively and it is usually shorter. |
| `--inline-imports` | Resolve `@import` against files relative to the input. Closed-world: assumes you control file resolution. |
| `--inline-vars` | Substitute `var(--name)` references with their declared values, then drop unused custom properties. Closed-world: assumes no runtime mutation. |
| `--keep-vars=NAMES` | Comma-separated custom-property names to preserve under `--inline-vars`. |
| `--profile` | Print per-pass timings of the optimiser to stderr after the run. Useful to triage which pass dominates on a slow input. Has no effect without `--minify`. |
| `-q, --quiet` / `-v, --verbose` | Standard verbosity controls. |

### Size

`--minify` optimises compressed transfer size by default; `--objective=raw`
optimises uncompressed bytes instead, for output that ships uncompressed
(inline style attributes, email HTML). Head-to-head sizes and timings against
other minifiers on the SatCSS corpus are in [BENCHMARKS.md](BENCHMARKS.md).

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
  parsed structures match but the strings don't, so formatting differences
  (whitespace, comment position) still surface.
- `tree`: structural diff only; formatting-only differences collapse to
  "identical".
- `string`: character-level comparison.
- `canonical`: passes when the two inputs share cascade's canonical minified
  form, modulo cascade-neutral reordering and regrouping. Declarations or
  rules whose footprints are disjoint (they write different properties) may
  swap freely, a `@media`/`@supports`/`@container` block containing only
  plain rules moves as a unit past statements its rules cannot conflict with,
  distinct custom properties may swap within any rule, and different
  factorings of the same content (a declaration hoisted into a shared
  selector-list group vs written inline, split vs grouped selector lists)
  compare equal, since none of those moves can change a computed value.
  Cascade-significant order is kept distinct (two writes of the same
  property, a shorthand and its longhand, a vendor-prefixed alias, `@layer`
  blocks). Equivalent shorthand decompositions are still not modelled.

<!-- $MDX skip -->
```bash
cascade diff reference.css output.css
cascade diff --diff=tree reference.css output.css
cascade diff --diff=canonical reference.css output.css
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

# pre-commit: catch changes beyond formatting
cascade diff --diff=canonical origin/main:src/style.css src/style.css
```

The exit code is 0 when the inputs are identical under the chosen mode and 1
otherwise, so cascade slots into any tool that branches on exit codes (`git`
hooks, `make`, GitHub Actions, ...). The `--minify` pipeline is fast enough
that a 200 KB stylesheet costs well under 100 ms on the SatCSS corpus;
`--objective=raw` trades roughly an order of magnitude of wall clock for the
last few percent of uncompressed bytes and fits a release build rather than a
watcher loop.

## `--minify` policy

Cascade picks the shortest behaviour-preserving spelling at every choice point.
Where the CSS spec and browser-compatible recovery rules permit several valid
serialisations, cascade chooses the shortest valid one.

### What runs

Value-level rewrites:

- **Colours:** hex when no longer than the name (`black` -> `#000`,
  `blue` -> `#00f`; `red` stays a name). Modern colour functions
  (`lab`/`lch`/`oklab`/`oklch`/`color()`) fold to shorter sRGB only within the
  ΔE<sub>OK</sub> budget below.
- **Numbers and lengths:** drop leading/trailing zeros (`0.5` -> `.5`); convert
  compatible units only when shorter (`12pt` stays `12pt`).
- **Math:** `calc()`, `hypot()`, etc. fold constant subexpressions only when
  the serialised result is exact (`calc(100%/3)` stays `calc(100%/3)`).
- **Whitespace:** elided at safe token boundaries (`100% 0` -> `100%0`).

Selector-level rewrites:

- Branches sorted into cascade's canonical order
  (`div,.class,#id` -> `#id,.class,div`).
- Pseudo-elements in legacy single-colon form (`::before` -> `:before`).

Rule-level rewrites:

- Adjacent same-selector merging and identical-body combining across
  non-overlapping intermediates, with specificity and importance reasoning.
- The DAG optimiser extracts shared declarations into comma-list rules when
  cascade-safe and net smaller.
- Rule ordering is stable and deterministic: the optimiser builds a
  cascade-dependency DAG, then emits a topological order whose tie-break is the
  first source appearance of each node. If two rules are not order-constrained,
  cascade does not invent a lexicographic CSS order for them; it keeps the
  source-order key.
- Shorthands with unordered components serialise in cascade's canonical order
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
shortest spelling and the fold preserves every rendered result. The same holds
for a complete math function whose units fix its dimension unambiguously: a
constant `calc()` reducing to an `<angle>` or `<time>` (`calc(1deg * 0)` ->
`0deg`) folds, while a `<percentage>` (ambiguous: length vs number percentage)
or a `calc()` that still references a `var()` stays verbatim. The colour fold
never produces a bare colour keyword: a name like `red` is also a valid
`<custom-ident>`, so it stays distinct from `#f00` even though it is shorter,
and hex stays hex. The fold changes the exact token string a script reads back
via `getPropertyValue`; cascade does not treat that byte-exact CSSOM
serialisation as an observable to preserve.

Whitespace inside an opaque value is likewise folded only where it is
insignificant: a `)` closing a non-substitution function or a block is a hard
token boundary, so the space after it is dropped (`drop-shadow(a) drop-shadow(b)`
-> `drop-shadow(a)drop-shadow(b)`). The space after a `var()` / `env()` / `attr()`
stays, since the substituted value could otherwise merge with its neighbour.

### How rule merging scales

The rule-level rewrites run on a cascade-dependency DAG, not on repeated linear
file scans. Graph edges represent only order-sensitive cascade dependencies:
same-origin rules whose equal-specificity selector branches may match the same
element and whose equal-importance declarations overlap after
shorthand/longhand expansion. Disjoint rules remain unordered in the graph.

Candidate rewrites are scheduled through an incremental priority queue,
largest byte-saving first. Applying a rewrite updates the graph and
re-enumerates only the affected neighbourhood; a full enumeration is kept as the
fallback when the queue drains. The final output is a deterministic
topological projection of the live graph, with first source appearance as the
stable tie-break key. Produced group/residual nodes inherit the earliest source
slot they represent, so the optimiser is source-stable whenever the cascade
does not force another order.

### Colour approximation

Cascade folds colours only within `0.002` ΔE<sub>OK</sub> (the CSS Color 4
Delta-E metric for Oklab/OkLCh). Alpha is separate: functional alpha rounds
to three decimals (`0.0005` tolerance); the 8-bit alpha of a hex fold is
its canonical spelling and is not gated by that tolerance.

Pass `--lossless` to keep colour values exact: hex/named canonicalisation and
modern-syntax rewrites still run, but channel rounding, within-budget
modern-space folds, and static `color-mix()` resolution are disabled. It also
sorts each rule's declarations into one canonical order across the stylesheet,
keeping any two whose footprints overlap (same property, or a shorthand and a
longhand) in place, so gzip back-references line up; the reorder never changes
a computed value.

### Scope

`--minify` is closed over the CSS text but open over runtime layout state.
Cascade uses source order, the cascade, dependencies, and dead-code reasoning,
but does not assume DOM shape, writing mode, computed direction, user styles,
or runtime custom-property mutation. The output stays sound when the minified
stylesheet is embedded in a larger page.

`--scope=stylesheet` asserts the input is the whole author CSS graph (after
`@import` resolution). The optimiser can then synthesise a partial-coverage
shorthand whose omitted longhand resets are proved not to disturb a prior
write the optimiser can't see.

### Target browsers

The default minify targets maintained evergreen browsers. Cascade may treat
baseline feature queries like `@supports(display:flex)` as true and remove the
wrapper, may use the HTML direction model to shorten `:not(:dir(ltr))` to
`:dir(rtl)`, and may drop a vendor-prefixed declaration (`-moz-box-sizing`)
whose unprefixed twin is present, since evergreen browsers understand the
unprefixed form.

`--enforce-spec` drops those facts. Cascade still serialises to the shortest
CSS form it knows, but feature queries stay, the direction model is not
assumed, and every vendor prefix is kept.

## CSS specification coverage

Cascade targets selected **CSS Level 3, Level 4, and Level 5** modules. Its
conformance target is CSS parsing, ASTs, printing, transforms, diffs, and
optimisation; it is not a complete web-platform runtime.

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting, specificity |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | ~30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colours, 15 colour spaces |
| [Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/) | `@media` (recovering a failed condition parse as `not all`), `@supports` property and selector checks, `@when` / `@else`, `@supports-condition` |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords, `all` reset semantics in the optimiser |
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
  optimiser, apart from substreams whose type their own syntax fixes
  unambiguously (complete colour functions, and constant math functions
  reducing to an `<angle>` or `<time>`), which fold to their shortest spelling.

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

### Theme resolution

`Css.resolve_theme ?theme ?theme_defaults` is the AST-level form of the
`--inline-vars --keep-vars` recipe above: it resolves design-token variables
against caller-supplied data.

- `theme` is the set of variable names whose `var()` references stay live. When
  `theme` is given, references to any other name are inlined to the value
  `theme_defaults` resolves for it.
- `theme_defaults` maps a custom-property name to its value and supplies the
  global token definitions. Every `var()` reference that is undefined in the
  stylesheet and resolvable through `theme_defaults` is emitted as a definition
  in the root-scope theme block: an existing `:root` / `:host` rule, or a fresh
  `:root`. Resolution is transitive, and a chain that cycles or hits a dead end
  is dropped. A name `theme_defaults` returns `None` on (for example a runtime
  `--tw-*` variable) is left free.

The definition lands at root scope by design. Custom properties are inherited
and resolved per element
([Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/)), so
`var(--x)` needs `--x` defined on the element or an ancestor. A theme token is
global: defining it on `:root` / `:host` makes it inherit to every element and
stay globally overridable, whereas defining it on the element-scoped rule that
happens to reference it would confine and shadow it.

### Parsing modes

`Css.of_string ~strict:false s` always returns
`Ok { stylesheet; warnings }`, with `warnings` listing recovered syntax and
declaration issues. `~strict:true` errors when the lenient parse would have
warned. When both succeed, their minified outputs are identical.

### Small runtime footprint

The core `cascade` library links only
[uutf](https://erratique.ch/software/uutf) and the OCaml runtime; it does not
pull `fmt`, so js_of_ocaml embedders stay lean. A local jsoo build that
parses and minifies one stylesheet compresses to under 200 KiB
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
  normalisations.

A fourth corpus, [SatCSS](test/interop/satcss/) (Hague-Lin-Hong's CSS
minification benchmark), is regenerated locally and not vendored: the upstream
repository carries no licence for redistributing the website CSS snapshots.

## References

**Other CSS tooling.** [Lightning CSS](https://github.com/parcel-bundler/lightningcss)
(Rust), [esbuild](https://github.com/evanw/esbuild) (Go), and the JS
optimisers [CSSO](https://github.com/css/csso),
[cssnano](https://github.com/cssnano/cssnano), and
[clean-css](https://github.com/clean-css/clean-css) all serve as cached
minifier oracles in the test suite.
[PostCSS](https://github.com/postcss/postcss) and
[CSSTree](https://github.com/csstree/csstree) are the broader JS
parser/AST projects worth comparing against. Earlier OCaml CSS work:
[css-parser](https://github.com/astrada/ocaml-css-parser) and
[OCaml-css](https://zoggy.frama.io/ocaml-css/).

**Optimisation research.**
[Hague, Lin, Hong (2018)](https://arxiv.org/abs/1812.02989) formalize rule
merging as a CSS-graph problem: a merge is legal only when selector
intersection and the intervening cascade dependencies preserve semantics.
[Visscher, Punt, Zaytsev (2016)](https://grammarware.net/text/2016/aba-css.pdf)
catalogue A-B*-A patterns (a property set, overridden, then restored), useful
adversarial input for optimisers since source order, specificity,
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
