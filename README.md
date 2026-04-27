# Cascade

CSS tooling for OCaml -- a typed AST, parser, pretty-printer, structural
transform library, diff tool, and optimizer for modern CSS.

Most CSS toolchains target JavaScript runtimes. Cascade provides the same
core capabilities -- parsing, printing, structural transformation, and
structural comparison -- as a native OCaml library with zero runtime
dependencies beyond the stdlib.
Properties, values, and selectors are represented as OCaml types rather than
strings, so invalid constructs are caught at compile time.

## Project scope

Cascade works on **CSS text and CSS syntax trees**. Its core responsibilities
are:

- parse CSS files or already-decoded CSS strings;
- expose typed CSS ASTs for rules, declarations, selectors, values, and
  at-rules;
- print CSS, including minified output;
- transform CSS ASTs with helpers such as `fold`, `map`, `sort`, and
  structural comparison;
- optimize/minify only when the rewrite is valid from stylesheet structure or
  from caller-supplied context;
- support CSS custom-property workflows, including `var()` parsing, typed
  fallbacks, theme/default based variable output, and `@property` syntax.

Cascade is a CSS library. When a transform needs information beyond CSS text,
that information is passed as an explicit closed context record. Theme/default
variable substitution is current behavior, and `Css.Context.t` is
the context type for property-value transforms.

## CSS specification coverage

Cascade targets selected **CSS Level 3, Level 4, and Level 5** modules. It has
focused parser, printer, optimizer, property, and fuzz tests for many CSS
features, but its conformance target is CSS parsing, ASTs, printing, transforms,
diffs, and optimization rather than a complete web-platform runtime.

See [SPEC_COVERAGE.md](SPEC_COVERAGE.md) for the detailed coverage matrix,
which separates CSS-file syntax coverage from context-dependent runtime
behavior.

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | ~30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colors, 15 color spaces |
| [Conditional Rules Level 3](https://www.w3.org/TR/css-conditional-3/) | `@media` feature queries, `@supports` property and selector checks |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords |
| [Nesting Module](https://www.w3.org/TR/css-nesting-1/) | Nested rules with `&`, nested `@media` and `@supports` |
| [Container Queries Level 1](https://www.w3.org/TR/css-contain-3/) | `@container` with size queries |
| [Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/) | `var()` parsing/printing, typed fallbacks, theme/default substitution, `@property` registration |
| [Fonts Level 4](https://www.w3.org/TR/css-fonts-4/) | `@font-face` descriptors |
| [Animations Level 1](https://www.w3.org/TR/css-animations-1/) | `@keyframes`, `@starting-style` |

Over 100 typed CSS properties cover box model, flexbox, grid, logical
properties, typography, borders, backgrounds, gradients, transforms,
transitions, animations, filters, and vendor-prefixed properties.

## Installation

```bash
opam install cascade
```

## Quick start

```ocaml
open Cascade.Css

let button =
  rule ~selector:(Selector.class_ "btn")
    [ display Inline_block
    ; background_color (hex "#3b82f6")
    ; color (hex "#ffffff")
    ; padding [ Rem 0.5 ]
    ; border_radius (Rem 0.375)
    ]

let () = print_string (to_string (v [ button ]))
```

Output:

```css
.btn {
  display: inline-block;
  background-color: #3b82f6;
  color: #fff;
  padding: 0.5rem;
  border-radius: 0.375rem;
}
```

## CLI tools

### `cascade` -- CSS formatter

```
cascade [--minify] [--optimize] [--pretty] [FILE]
```

Reads a CSS file (or stdin with `-`) and outputs formatted CSS. The
`--optimize` flag merges duplicate rules and removes redundant declarations.

```bash
cascade style.css                        # pretty-print
cascade --minify style.css               # minify
cascade --optimize --minify style.css    # optimize and minify
cat style.css | cascade --minify -       # read from stdin
```

### `cssdiff` -- structural CSS diff

```
cssdiff [--color=WHEN] [--diff=MODE] FILE1 FILE2
```

Compares two CSS files using structural parsing, detecting added, removed,
and modified rules, property value changes, and reordered rules. Three diff
modes are available: `auto` (default -- uses tree diff for structural changes,
string diff otherwise), `tree` (force structural comparison), and `string`
(character-level comparison).

```bash
cssdiff reference.css output.css
cssdiff --diff=tree reference.css output.css
NO_COLOR=1 cssdiff reference.css output.css
```

## Libraries

- **`cascade`** -- typed CSS AST, parser, pretty-printer, structural
  transformation helpers, and optimizer.
  The main module is `Cascade.Css`.
- **`cascade.tools`** -- structural CSS comparison (`Css_tools.Css_compare`,
  `Css_tools.Tree_diff`, `Css_tools.String_diff`).

## Limitations

- **UTF-8 text input.** Cascade parses already-decoded UTF-8 OCaml strings. It
  does not implement the CSS Syntax Level 3 section 3.2 byte-stream decoding
  layer: BOM handling, HTTP/environment charset fallback, and exact
  `@charset "...";` byte sniffing are caller responsibilities before invoking
  Cascade. Stylesheets in legacy encodings (`Shift_JIS`, `Big5`, `EUC-*`,
  `windows-125x`, UTF-16, etc.) must be decoded upstream with a dedicated
  encoding library and passed to Cascade as UTF-8 text. Parsed `@charset`
  syntax is compatibility surface, not an encoding-decoding mechanism.
- **Two parse entry points.** `Css.of_string` fails fast on the first
  validator error. `Css.parse` runs the CSS Syntax Level 3 recovery path:
  unclosed blocks auto-close at EOF (5.3.7), an invalid declaration is
  dropped while its enclosing rule keeps its other declarations (5.4.4),
  and rules that don't validate at all surface as warnings in the returned
  `parse_result.warnings` while the rest of the stylesheet parses
  normally.
- CSS nesting is parsed and printed but the optimizer does not flatten nested
  rules. A round-trip through the parser preserves nesting structure.
- `@import` rules are preserved as-is; Cascade does not resolve or inline
  imported stylesheets.
- Cascade does not provide implicit runtime subsystems such as a DOM, live
  CSSOM, network loader, layout tree, renderer, animation timeline, or ambient
  computed-style engine. CSS syntax for those features is still parsed and
  printed where the library models it, and explicit-context transforms can be
  added when the required context is represented in the API.
- No source-map support.

## Licence

[ISC](LICENSE)
