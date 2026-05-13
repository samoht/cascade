# Cascade

Tools to manipulate CSS files safely: spec-driven parsing,
minification, and structural diff. Cascade ships one binary
(`cascade`) and the OCaml library it is built on.

## `cascade fmt` -- format and minify CSS

<!-- $MDX skip -->
```bash
cascade fmt [--minify] [--inline-imports] [--inline-vars] [--keep-vars=NAMES] FILE
cat style.css | cascade fmt -                # read from stdin
cascade FILE                                 # shorthand for: cascade fmt FILE
```

`cascade fmt` reads a CSS file (or stdin with `-`), parses it through
the typed AST, and writes it back. Without flags it pretty-prints.
With `--minify` it runs the standard safe transforms (deduplication,
rule merging, selector grouping, empty-rule elimination, nested-rule
flattening) and emits minified output.

The two `--inline-*` flags are explicit closed-world opt-ins:
`--inline-imports` resolves `@import` against files relative to the
input (assumes you control file resolution) and `--inline-vars`
substitutes `var(--name)` references with their declared values
(assumes no runtime mutation of custom properties).
`--keep-vars=NAMES` keeps the listed custom properties live.

<!-- $MDX skip -->
```bash
cascade fmt style.css                            # pretty-print
cascade fmt --minify style.css
cascade fmt --inline-imports --inline-vars --minify style.css
cascade fmt --inline-vars --keep-vars=theme,brand style.css
```

### Minify policy (`--minify`)

`--minify` picks the shortest spec-equivalent spelling at every choice
point. Where the CSS spec and browser-compatible recovery rules permit
several valid serializations, Cascade chooses the shortest valid one.

- Colors: hex form when it's at most as long as the name (`black` -> `#000`, `blue` -> `#00f`; `red` stays a name).
- Numbers: drop leading zero (`0.5` -> `.5`) and trailing zero (`10.0` -> `10`).
- Pseudo-elements: legacy single-colon form (`::before` -> `:before`).
- Whitespace elided at safe token boundaries (`100% 0` -> `100%0`).
- Math reduction: `calc()`, `hypot()` etc. fold constant subexpressions.
- Media queries: legacy -> range syntax (`(min-width:48px)` -> `(width>=48px)`).

Pretty mode (the default) preserves the authored form where the AST
permits it. Spec-mandated canonicalizations (CSS Syntax 3 section
4.3.7 NUL -> U+FFFD, single-quote -> double, ...) apply in both
modes. Empty rules and invalid declarations are dropped in both
modes; comments are discarded during parsing.

### Interop testing against other minifiers

Cascade's minified output is compared with cached oracle answers
generated from the Lightning CSS test suite. Regenerating the
[trace](test/interop/lightning/traces/minify.pairs) runs a patched
Lightning CSS test build to capture each input and Lightning's
expected output, then runs the available minifier CLIs (`esbuild`,
`cleancss`, `csso`, `cssnano`, `lightningcss-cli`) over the same
inputs. Normal test runs use only the cached trace; they do not shell
out to external tools.

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

## `cascade diff` -- structural CSS diff

<!-- $MDX skip -->
```bash
cascade diff [--color=WHEN] [--diff=MODE] FILE1 FILE2
```

`cascade diff` compares two CSS files through the parsed AST rather
than character-by-character: added, removed, modified, and reordered
rules are detected structurally, and property value changes are
reported in terms of CSS values.

What counts as "no difference" depends on the mode:

- `auto` (default) — falls back to a string diff when the ASTs match
  but the strings don't, so cosmetic differences (whitespace, comment
  position) still surface.
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

## Using Cascade as a library

For OCaml users, the same engine ships as the `cascade` opam package.
The CLI above is a thin wrapper over its public API.

<!-- $MDX skip -->
```bash
opam install cascade
```

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

- **UTF-8 text input.** Cascade parses already-decoded UTF-8 OCaml
  strings. It does not implement the CSS Syntax Level 3 section 3.2
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

## Licence

[ISC](LICENSE)
