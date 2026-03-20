# Cascade

CSS tooling for OCaml -- a typed AST, parser, pretty-printer, and optimiser
for modern CSS.

Most CSS toolchains target JavaScript runtimes. Cascade provides the same
core capabilities -- parsing, rendering, and structural comparison -- as a
native OCaml library with no runtime dependencies beyond `fmt` and `cmdliner`.
Properties, values, and selectors are represented as OCaml types rather than
strings, so invalid constructs are caught at compile time.

## CSS specification coverage

Cascade targets **CSS Level 3 and Level 4** modules. The parser handles the
full syntax defined in
[CSS Syntax Level 3](https://www.w3.org/TR/css-syntax-3/) and the printer
produces spec-conformant output with optional minification.

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | ~30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colours, 15 colour spaces |
| [Conditional Rules Level 3](https://www.w3.org/TR/css-conditional-3/) | `@media` feature queries, `@supports` property and selector checks |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords |
| [Nesting Module](https://www.w3.org/TR/css-nesting-1/) | Nested rules with `&`, nested `@media` and `@supports` |
| [Container Queries Level 1](https://www.w3.org/TR/css-contain-3/) | `@container` with size queries |
| [Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/) | `var()` with typed fallbacks, `@property` registration |
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
    ; padding (Rem 0.5)
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
cascade [--minify] [--optimise] [--pretty] [FILE]
```

Reads a CSS file (or stdin with `-`) and outputs formatted CSS. The
`--optimise` flag merges duplicate rules and removes redundant declarations.

```bash
cascade style.css                        # pretty-print
cascade --minify style.css               # minify
cascade --optimise --minify style.css    # optimise and minify
cat style.css | cascade --minify -       # read from stdin
```

### `cssdiff` -- structural CSS diff

```
cssdiff [--colour=WHEN] [--diff=MODE] FILE1 FILE2
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

- **`cascade`** -- typed CSS AST, parser, pretty-printer, and optimiser.
  The main module is `Cascade.Css`.
- **`cascade.tools`** -- structural CSS comparison (`Css_tools.Css_compare`,
  `Css_tools.Tree_diff`, `Css_tools.String_diff`).

## Limitations

- CSS nesting is parsed and printed but the optimiser does not flatten nested
  rules. A round-trip through the parser preserves nesting structure.
- The parser uses error recovery for declarations but does not yet implement
  the full error recovery algorithm from CSS Syntax Level 3 section 9.
- `@import` rules are preserved as-is; Cascade does not resolve or inline
  imported stylesheets.
- No source-map support.

## Licence

[ISC](LICENSE)
