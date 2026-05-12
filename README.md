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

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | ~30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colors, 15 color spaces |
| [Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/) | `@media` (with error recovery to `not all`), `@supports` property and selector checks, `@when` / `@else`, `@supports-condition` |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords, `all` reset semantics in the optimizer |
| [Nesting Module](https://www.w3.org/TR/css-nesting-1/) | Nested rules with `&`, nested `@media` and `@supports` |
| [Container Queries Level 5](https://www.w3.org/TR/css-conditional-5/#container-queries) | `@container` with size queries and typed `style()` / `scroll-state()` queries, including range operators |
| [Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/) | `var()` parsing/printing, typed fallbacks, theme/default substitution, `@property` registration |
| [Fonts Level 4](https://www.w3.org/TR/css-fonts-4/) | `@font-face` descriptors |
| [Animations Level 1](https://www.w3.org/TR/css-animations-1/) | `@keyframes`, `@starting-style` |

Over 380 typed CSS properties cover box model, flexbox, grid, logical
properties, typography, borders, backgrounds, gradients, transforms,
transitions, animations, filters, masks, anchor positioning, view transitions,
and vendor-prefixed properties.

## Installation

<!-- $MDX skip -->
```bash
opam install cascade
```

## Quick start

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

## Serialization rules

Cascade has two output modes: **minified** (shortest spec-equivalent) and
**pretty** (preserves authored form where the AST permits). Parsing has two
modes too: **strict** (escalates any warning to `Error`) and **lenient** (the
default; always returns `Ok` with a `warnings` list).

### Both output modes

- Empty rules (`.x { }`) are dropped (zero declarations = no effect).
- Invalid declarations are dropped per the dual-mode contract.
- CSSOM canonicalizations apply: single quotes → double, `@import url(x)` → `@import "x"`.
- Comments are discarded during parsing and do not survive serialization.

### Minified mode (`~minify:true`)

Picks the shortest spec-equivalent spelling at every choice point.
When the CSS spec and browser-compatible recovery rules allow multiple valid
serializations, Cascade chooses the shortest valid answer. Test oracles should
follow that rule; industry-grade minifiers are useful comparators, but if they
disagree on valid output, shortest valid output wins.

- Colors: hex form when it's at most as long as the name (`black` → `#000`, `blue` → `#00f`; `red` stays a name).
- Numbers: drop leading zero (`0.5` → `.5`) and trailing zero (`10.0` → `10`).
- Pseudo-elements: legacy single-colon form (`::before` → `:before`).
- Whitespace elided at safe token boundaries (`100% 0` → `100%0`).
- Math reduction: `calc()`, `hypot()` etc. fold constant subexpressions.
- Media queries: legacy → range syntax (`(min-width:48px)` → `(width>=48px)`).

### Pretty mode (`~minify:false`)

Preserves the authored form where the AST distinguishes it (`:before` stays
single-colon, `::before` stays double-colon). Whitespace is inserted for
readability. Spec-mandated canonicalizations still apply (e.g.,
`Css.Syntax 4.3.7` NUL → U+FFFD). Comments are not represented in the AST and
are discarded during parsing.

### Strict vs lenient parsing

For every input `s`:

- `Css.of_string ~strict:false s` is total — always returns `Ok _`.
- `Css.of_string ~strict:true s = Error _` iff the lenient parse returned non-empty `warnings`.
- When both succeed, their minified outputs are identical.

This means lenient is the recovery surface (everything parses, warnings flag
deviations) and strict is the gate (any warning becomes an error).

## CLI tools

### `cascade` -- CSS formatter

```
cascade [--minify] [--optimize] [--pretty] [FILE]
```

Reads a CSS file (or stdin with `-`) and outputs formatted CSS. The
`--optimize` flag merges duplicate rules and removes redundant declarations.

<!-- $MDX skip -->
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

<!-- $MDX skip -->
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
- **One warning-aware parse entry point.** `Css.of_string` runs the CSS Syntax
  Level 3 recovery path: unclosed blocks auto-close at EOF (5.3.7), an invalid
  declaration is dropped while its enclosing rule keeps its other declarations
  (5.4.4), and rules that don't validate at all surface as warnings in the
  returned `parse_result.warnings` while the rest of the stylesheet parses
  normally. Pass `~strict:true` to promote the first warning to `Error`, or use
  `Css.of_string_exn` when only the recovered stylesheet is needed.
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
