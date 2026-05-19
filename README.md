# Cascade

Cascade is a command-line tool for formatting, minifying, inlining,
and structurally diffing CSS. It ships one binary (`cascade`) and,
for OCaml users, the library it is built on.

## Install

On macOS, the primary binary distribution is the Homebrew tap:

<!-- $MDX skip -->
```bash
brew install samoht/tap/cascade
```

This installs the `cascade` CLI. Homebrew is the only prerequisite
for this path.

For OCaml and opam users, the same package also installs the CLI and
the `cascade` library:

<!-- $MDX skip -->
```bash
opam install cascade
```

From source, use OCaml 5.2 or later, opam, and dune:

<!-- $MDX skip -->
```bash
git clone https://github.com/samoht/cascade.git
cd cascade
opam install . --with-test
cascade --help
```

## `cascade` -- format and minify CSS

<!-- $MDX skip -->
```bash
cascade [--minify] [--inline-imports] [--inline-vars] [--keep-vars=NAMES] [FILE]
cat style.css | cascade -                    # read stdin explicitly
```

`cascade` reads a CSS file, or stdin when no file is given (or with
`-`), parses it into a structured CSS model, and writes formatted CSS
to stdout. Without flags it pretty-prints. With `--minify` it runs the
standard safe transforms (deduplication, rule merging, selector
grouping, empty-rule elimination, nested-rule flattening) and emits
minified output.

This is a parser/printer round trip, not a byte-preserving formatter:
comments are discarded during parsing, and empty rules and invalid
declarations are dropped in both pretty and minified output.

The two `--inline-*` flags are explicit closed-world opt-ins:
`--inline-imports` resolves `@import` against files relative to the
input (assumes you control file resolution) and `--inline-vars`
substitutes `var(--name)` references with their declared values
(assumes no runtime mutation of custom properties).
`--keep-vars=NAMES` keeps the listed custom properties live.

<!-- $MDX skip -->
```bash
cascade style.css > style.formatted.css      # pretty-print
cascade --minify style.css > style.min.css
cascade --inline-imports --inline-vars --minify style.css > bundled.min.css
cascade --inline-vars --keep-vars=theme,brand style.css > themed.css
```

### Minify policy (`--minify`)

`--minify` picks the shortest spec-equivalent spelling at every choice
point. Where the CSS spec and browser-compatible recovery rules permit
several valid serializations, Cascade chooses the shortest valid one. For
perceptual color spaces, minified output may also round channels to bounded
precision when the visual difference is negligible.

`--minify` is closed over the CSS text it is given, but open over runtime
layout state. Cascade may use the whole parsed stylesheet for source-order,
cascade, dependency, and dead-code reasoning, but it does not assume inherited
or environment-dependent facts such as DOM shape, writing mode, direction, user
styles, or runtime custom-property mutation unless those facts are explicit in
the stylesheet or supplied through an explicit closed context. This keeps the
output sound when a minified stylesheet is embedded into a larger page.

- Colors: hex form when it's at most as long as the name (`black` -> `#000`, `blue` -> `#00f`; `red` stays a name).
- Modern color functions: `lab()`, `lch()`, `oklab()`, and `oklch()` may round lightness/chroma/a/b channels, hue, and alpha under `--minify`.
- Numbers: drop leading zero (`0.5` -> `.5`) and trailing zero (`10.0` -> `10`).
- Selector lists: sort branches into Cascade's canonical selector order (`div,.class,#id` -> `#id,.class,div`).
- Shorthands with unordered components serialize in Cascade's canonical order; when shortest forms tie, Cascade follows common minifier convention (`animation:1s slide` -> `animation:slide 1s`).
- Pseudo-elements: legacy single-colon form (`::before` -> `:before`).
- Whitespace elided at safe token boundaries (`100% 0` -> `100%0`).
- Math reduction: `calc()`, `hypot()` etc. fold constant subexpressions.
- Media queries: legacy -> range syntax (`(min-width:48px)` -> `(width>=48px)`).

These rules compose wherever Cascade has a typed CSS value: nested function
arguments, registered custom properties, and normal declarations. Unregistered
custom-property values remain opaque token streams.

Pretty mode (the default) preserves the authored form where the parsed
model permits it. Spec-mandated canonicalizations (CSS Syntax 3 section
4.3.7 NUL -> U+FFFD, single-quote -> double, ...) apply in both modes.

## `cascade diff` -- structural CSS diff

<!-- $MDX skip -->
```bash
cascade diff [--color=WHEN] [--diff=MODE] FILE1 FILE2
```

`cascade diff` compares two CSS files through the parsed CSS structure
rather than character-by-character: added, removed, modified, and
reordered rules are detected structurally, and property value changes
are reported in terms of CSS values.

Identical files exit 0. Differences exit 1, which makes `cascade diff`
usable as a CI check.

What counts as "no difference" depends on the mode:

- `auto` (default) — falls back to a string diff when the parsed
  structures match but the strings don't, so cosmetic differences
  (whitespace, comment position) still surface.
- `tree` — structural diff only; formatting-only differences collapse
  to "identical".
- `string` — character-level comparison.
- `semantic` — passes when the two inputs share Cascade's canonical
  minified form (whitespace, color spellings, leading-zero
  normalisations all collapse). This is not full CSS semantic
  equivalence: equivalent shorthand decompositions and
  cascade-affecting rule reorderings are not modelled.

<!-- $MDX skip -->
```bash
cascade diff reference.css output.css
cascade diff --diff=tree reference.css output.css
cascade diff --diff=semantic reference.css output.css
NO_COLOR=1 cascade diff reference.css output.css
```

## CSS specification coverage

Cascade targets selected **CSS Level 3, Level 4, and Level 5**
modules. Its conformance target is CSS parsing, ASTs, printing,
transforms, diffs, and optimization -- not a complete web-platform
runtime.

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | ~30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colors, 15 color spaces |
| [Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/) | `@media` (recovering a failed condition parse as `not all`), `@supports` property and selector checks, `@when` / `@else`, `@supports-condition` |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords, `all` reset semantics in the optimizer |
| [Nesting Module](https://www.w3.org/TR/css-nesting-1/) | Nested rules with `&`, nested `@media` and `@supports` |
| [Container Queries Level 5](https://www.w3.org/TR/css-conditional-5/#container-queries) | `@container` with size queries and typed `style()` / `scroll-state()` queries, including range operators |
| [Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/) | `var()` parsing/printing, typed fallbacks, theme/default substitution, `@property` registration |
| [Fonts Level 4](https://www.w3.org/TR/css-fonts-4/) | `@font-face` descriptors |
| [Animations Level 1](https://www.w3.org/TR/css-animations-1/) | `@keyframes`, `@starting-style` |

Typed CSS properties cover box model, flexbox, grid, logical
properties, typography, borders, backgrounds, gradients, transforms,
transitions, animations, filters, masks, anchor positioning, view
transitions, and vendor-prefixed properties. Together, these cover
the stylesheet surface typically emitted by CSS generators, component
libraries, and utility frameworks.

## Limitations

- **UTF-8 text input.** Cascade parses already-decoded UTF-8 text. It
  does not implement the CSS Syntax Level 3 section 3.2
  byte-stream decoding layer: BOM handling, HTTP/environment charset
  fallback, and exact `@charset "...";` byte sniffing are caller
  responsibilities before invoking Cascade. Stylesheets in legacy
  encodings (`Shift_JIS`, `Big5`, `EUC-*`, `windows-125x`, UTF-16,
  etc.) must be decoded upstream with a dedicated encoding library
  and passed to Cascade as UTF-8 text. Parsed `@charset` syntax is
  compatibility surface, not an encoding-decoding mechanism.
- Cascade does not provide implicit runtime subsystems such as a DOM,
  live CSSOM, network loader, layout tree, renderer, animation
  timeline, or ambient computed-style engine. CSS syntax for those
  features is still parsed and printed where the library models it,
  and explicit-context transforms can be added when the required
  context is represented in the API.

## Using Cascade as a library

The CLI above is a thin wrapper over the public OCaml API exposed by
the `cascade` opam package.

### Quick start

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
"\n.btn {\n  display: inline-block;\n  background-color: #3b82f6;\n  color: #ffffff;\n  padding: .5rem;\n  border-radius: .375rem;\n}\n"
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
constructions are caught at compile time. Structural transforms
(`fold`, `map`, `sort`, `flatten_nesting`), `Css.inline_imports`, and
`Css.optimize ?flatten_nesting` are the main entry points for
AST-level work. Transforms that need information beyond CSS text take
an explicit closed `Css.Context.t` rather than reading ambient runtime
state.

### Parsing modes

Parsing has two modes -- **strict** and **lenient** -- both routed
through `Css.of_string`:

- `Css.of_string ~strict:false s` is total: it always returns
  `Ok { stylesheet; warnings }`, where `warnings` carries recovered
  syntax and declaration issues.
- `Css.of_string ~strict:true s = Error _` iff the lenient parse
  returned non-empty `warnings`.
- When both succeed, their minified outputs are identical.

Lenient is the recovery surface (every input parses, warnings flag
deviations); strict is the gate (any warning becomes an error).

### Small runtime footprint, js_of_ocaml-friendly

The core `cascade` library links only
[uutf](https://erratique.ch/software/uutf) and the OCaml runtime; it
does not link `fmt`, so js_of_ocaml embedders do not pull `fmt` in
transitively. In one local measurement, a small js_of_ocaml executable
that parses and minifies one stylesheet compressed to less than 200
KiB (`--opt 3 --no-source-map` with size-oriented runtime flags); the
exact figure depends on the OCaml/jsoo versions and which Cascade
modules are linked.

Structural diff lives in the separate `cascade.diff` sub-library
(`Cascade_diff.Css_compare`, `Cascade_diff.Tree_diff`,
`Cascade_diff.String_diff`); it is what `cascade diff` is built on.

## Development and testing

Cascade is tested on two complementary oracle corpora: the upstream
Web Platform Tests for parser conformance, and a Lightning CSS-derived
trace for minified-output canonicalization.

### Parser conformance against WPT

The `css/css-syntax/` subset of the [Web Platform
Tests](https://github.com/web-platform-tests/wpt) is vendored under
[test/interop/wpt/traces/css-syntax/](test/interop/wpt/traces/css-syntax/)
and replayed by [test/interop/wpt/test.ml](test/interop/wpt/test.ml).
The harness pulls CSS out of `<style>` blocks, inline `style="..."`
attributes, linked `support/*.css` files, and `parseRule(\`...\`)`
template-literal calls inside `<script>` bodies, then feeds each
fragment through `Css.of_string`. A test fails when Cascade's parser
rejects an input that browsers accept or accepts one that browsers
reject; there is no skip list. Refresh the vendored snapshot with
`dune build @test/interop/wpt/regen-traces`.

### Minified-output interop against other minifiers

Cascade's minified output is compared with cached oracle answers
generated from the Lightning CSS test suite. Regenerating the
[trace](test/interop/lightning/traces/minify.pairs) (via `dune build
@regen-traces`) runs a patched Lightning CSS test build to capture
each input and Lightning's expected output, then runs the available
minifier CLIs (`esbuild`, `cleancss`, `csso`, `cssnano`,
`lightningcss-cli`) over the same inputs. Normal test runs use only
the cached trace; they do not shell out to external tools.

A case passes when Cascade's output is no longer than the shortest
*valid* cached oracle answer; longer Cascade outputs are recorded as
`longer-than-shortest` mismatches. A cached answer is excluded from
the shortest-valid comparison when:

- the tool crashed or rejected the input,
- the output fails to round-trip through Cascade's parser, or
- Cascade-parsing the output produces a semantic fingerprint different
  from the input (rule kinds, declaration set, computed shorthand
  shape).

Excluded answers are recorded in the per-test `*.output` logs and the
rolled-up `_build/_tests/lightning_minify/upstream-bugs.log`.

A second, stricter oracle is vendored from
[keithamus/css-minify-tests](https://github.com/keithamus/css-minify-tests):
a vendor-neutral hand-curated set of `source.css` / `expected.css`
pairs covering 29 CSS feature categories (at-rules, colors, comments,
duplicates, gradients, merging, selectors, shorthands, transforms,
values, whitespace, zero-units, ...). Inputs live under
[test/interop/css-minify-tests/traces/](test/interop/css-minify-tests/traces/);
refresh them with `dune build @regen-traces` from that directory. Each
pair passes when Cascade's minified output equals the
upstream-curated `expected.css` byte for byte (trailing whitespace
ignored). The suite runs under `@runtest` so divergences are visible
on every test run.

## References

### OCaml CSS libraries

- [css-parser](https://github.com/astrada/ocaml-css-parser) parses
  CSS Syntax Level 3 into a spec-shaped AST.
- [OCaml-css](https://zoggy.frama.io/ocaml-css/) is a parser and
  printer for CSS.

### Open-source CSS implementations

- [Lightning CSS](https://github.com/parcel-bundler/lightningcss)
  is a Rust parser, transformer, bundler, and minifier. Cascade uses
  a Lightning CSS-derived trace as the main minified-output oracle.
- [esbuild](https://github.com/evanw/esbuild) is a Go bundler and
  minifier with first-class CSS input support; its CLI is one of the
  cached minifier oracles used by `@regen-traces`.
- [clean-css](https://github.com/clean-css/clean-css),
  [CSSO](https://github.com/css/csso), and
  [cssnano](https://github.com/cssnano/cssnano) are JavaScript CSS
  optimizers/minifiers used as additional cached oracles.
- [PostCSS](https://github.com/postcss/postcss) and
  [CSSTree](https://github.com/csstree/csstree) are widely used
  JavaScript CSS parser/AST/tooling projects worth comparing against
  when extending Cascade's syntax surface.

### CSS optimization research

- [CSS Minification via Constraint Solving](https://arxiv.org/abs/1812.02989)
  by Hague, Lin, and Hong formalizes rule merging as a CSS-graph
  optimization problem: two declarations can be merged or reordered only
  when selector intersection and the intervening cascade dependencies make
  the transformation semantics-preserving.
- [The A-B*-A Pattern: Undoing Style in CSS](https://grammarware.net/text/2016/aba-css.pdf)
  by Visscher, Punt, and Zaytsev studies declarations that set a property
  to one value, override it, then restore the original value. The pattern is
  useful adversarial input for optimizers because source order, specificity,
  inheritance, pseudo selectors, and implicit/default values all affect
  whether a rewrite is sound.
- [CILLA: Automated CSS Analysis](https://github.com/saltlab/cilla)
  by Mesbah and Mirshokraie analyses runtime DOM-CSS matching to flag
  unmatched selectors, ineffective declarations, and properties later
  overridden in the cascade. A practical reference for what an
  AST-level "dead rule" or "useless declaration" check is allowed to
  claim without observing layout.

### Test suites and specifications

- [Web Platform Tests](https://github.com/web-platform-tests/wpt)
  provide the vendored CSS Syntax parser-conformance vectors.
- [keithamus/css-minify-tests](https://github.com/keithamus/css-minify-tests)
  is a vendor-neutral correctness corpus of CSS minifier
  transformations agreed by maintainers across Lightning CSS, esbuild,
  clean-css, CSSO, and cssnano. Vendored as the strict-equality
  minifier oracle.
- [CSS Syntax Level 3](https://www.w3.org/TR/css-syntax-3/) defines
  tokenization, parser recovery, and serialization boundaries.
- [Selectors Level 4](https://www.w3.org/TR/selectors-4/) defines
  selector parsing, specificity, pseudo-classes, and pseudo-elements.
- [CSS Values and Units Level 4](https://www.w3.org/TR/css-values-4/)
  defines numeric values, units, math functions, and `calc()`.
- [CSS Color Level 4](https://www.w3.org/TR/css-color-4/) defines
  modern color syntax, named colors, `oklab()`, `oklch()`, and
  `color-mix()`.
- [CSS Cascading and Inheritance Level 5](https://www.w3.org/TR/css-cascade-5/)
  defines cascade layers, CSS-wide keywords, and shorthand/defaulting
  behavior.
- [CSS Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/)
  covers `@media`, `@supports`, `@container`, `@when`, and related
  conditional grammar.
- [CSS Nesting Module](https://www.w3.org/TR/css-nesting-1/) defines
  nested style rules and the `&` nesting selector.

## Licence

[ISC](LICENSE)
