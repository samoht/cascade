# Cascade -- CSS generation and manipulation for OCaml

Cascade is a typed CSS library providing:

- **CSS AST**: Selectors, properties, values, declarations, rules, stylesheets
- **Parser**: Read CSS from strings with error recovery
- **Pretty-printer**: Emit CSS with minification support
- **Optimizer**: Deduplicate, merge rules, combine selectors
- **CSS variables**: Custom properties with `@property` registration
- **At-rules**: `@media`, `@supports`, `@layer`, `@keyframes`, `@font-face`,
  `@container`, `@property`, `@starting-style`
- **Tools**: CSS diff, tree diff for structural comparison

Extracted from [tw](https://github.com/samoht/tw) (Tailwind CSS v4 in OCaml).

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

## CSS support

### Selectors

Class, ID, element, universal, attribute selectors, pseudo-classes (`:hover`,
`:focus`, `:first-child`, `:nth-child()`, `:where()`, `:not()`, `:is()`,
`:has()`, etc.), pseudo-elements (`::before`, `::after`, `::placeholder`,
`::file-selector-button`, etc.), and combinators (descendant, child `>`,
adjacent `+`, sibling `~`).

### Values and units

- **Lengths**: `px`, `rem`, `em`, `%`, `vw`, `vh`, `vmin`, `vmax`, `dvh`,
  `dvw`, `lvh`, `lvw`, `svh`, `svw`, `ch`, `lh`, `ex`, `cap`, `ic`, `rlh`,
  `cm`, `mm`, `in`, `pt`, `pc`, `q`
- **Keywords**: `auto`, `none`, `inherit`, `initial`, `unset`, `revert`,
  `revert-layer`, `fit-content`, `min-content`, `max-content`
- **Functions**: `calc()`, `clamp()`, `min()`, `max()`, `minmax()`
- **Colors**: `hex`, `rgb()`, `rgba()`, `hsl()`, `hsla()`, `hwb()`,
  `oklch()`, `oklab()`, `lch()`, `color()`, `color-mix()`, named colors (148),
  system colors, `currentcolor`, `transparent`
- **Color spaces**: sRGB, sRGB-linear, display-p3, a98-rgb, prophoto-rgb,
  rec2020, lab, oklab, xyz, xyz-d50, xyz-d65, lch, oklch, hsl, hwb
- **Angles**: `deg`, `rad`, `turn`, `grad`
- **Durations**: `s`, `ms`

### Properties (100+)

- **Box model**: `width`, `height`, `min-*`, `max-*`, `padding`, `margin`,
  `box-sizing`, `aspect-ratio`
- **Display and positioning**: `display`, `position`, `top`/`right`/`bottom`/`left`,
  `inset`, `z-index`, `float`, `clear`, `overflow`, `visibility`
- **Flexbox**: `flex-direction`, `flex-wrap`, `justify-content`, `align-items`,
  `align-self`, `flex-grow`, `flex-shrink`, `flex-basis`, `gap`, `order`
- **Grid**: `grid-template-columns`/`rows`, `grid-column`/`row`,
  `grid-auto-flow`, `grid-auto-columns`/`rows`, `place-items`, `place-content`
- **Typography**: `font-family`, `font-size`, `font-weight`, `font-style`,
  `line-height`, `letter-spacing`, `text-align`, `text-decoration`,
  `text-transform`, `text-indent`, `white-space`, `word-break`, `word-spacing`,
  `text-wrap`, `hyphens`
- **Borders and outlines**: `border`, `border-radius`, `border-color`,
  `border-style`, `border-width`, `outline`, `outline-offset`
- **Backgrounds**: `background`, `background-color`, `background-image`,
  `background-position`, `background-size`, `background-repeat`, gradients
  (linear, radial, conic) with color interpolation
- **Colors and effects**: `color`, `opacity`, `box-shadow`, `text-shadow`,
  `filter`, `backdrop-filter`, `mix-blend-mode`
- **Transforms and animations**: `transform`, `translate`, `rotate`, `scale`,
  `transition`, `animation`, `@keyframes`
- **Logical properties**: `margin-inline`/`block`, `padding-inline`/`block`,
  `border-inline`/`block`, `inset-inline`/`block` (start/end variants)
- **Interaction**: `cursor`, `pointer-events`, `user-select`, `touch-action`,
  `scroll-behavior`, `scroll-snap-type`/`align`, `accent-color`, `caret-color`
- **Vendor prefixes**: `-webkit-*` and `-moz-*` properties for
  `appearance`, `text-fill-color`, `background-clip`, etc.

### At-rules

- `@media` with feature queries (`min-width`, `prefers-color-scheme`, etc.)
- `@supports` with property and selector checks
- `@container` with container queries
- `@layer` with ordering and nesting
- `@keyframes` with named animations
- `@font-face` with font descriptors
- `@property` with typed custom property registration
- `@starting-style` for entry animations

### CSS variables

Custom properties with typed syntax, fallback values, and `@property`
registration for inheritance and initial values.

## CLI tools

### `cascade` -- CSS formatter

```
cascade [--minify] [--optimize] [--pretty] [FILE]
```

Reads a CSS file (or stdin with `-`) and outputs formatted CSS.

```bash
# Pretty-print
cascade style.css

# Minify
cascade --minify style.css

# Optimize and minify
cascade --optimize --minify style.css

# Pipe from stdin
cat style.css | cascade --minify -
```

### `cssdiff` -- Structural CSS diff

```
cssdiff [--color=WHEN] [--diff=MODE] FILE1 FILE2
```

Compares two CSS files using structural parsing, detecting added/removed/modified
rules, property changes, and reordered rules.

```bash
# Compare two files
cssdiff reference.css output.css

# Force tree-based structural diff
cssdiff --diff=tree reference.css output.css

# Disable colors
NO_COLOR=1 cssdiff reference.css output.css
```

Diff modes: `auto` (default, smart detection), `tree` (structural), `string`
(character-level).

## Libraries

- **`cascade`** -- Main library. Typed CSS AST, parser, printer, optimizer.
- **`cascade.tools`** -- CSS comparison tools for structural diffing.

## License

[ISC](LICENSE)
