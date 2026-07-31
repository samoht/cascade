## Unreleased

### Diffing

- `--diff=canonical` keys a `@media not all and (X)` block as the
  `@media not (X)` that Media Queries 4 sec. 2.1 makes it equal to, since
  `all` is the identity media type. The two spellings used to compare as a
  removed container plus an added one. Emission still keeps what it read: a
  Level 3 parser rejects the shorter form, and an unrecognised query never
  matches (#231)
- `--diff=canonical` sorts a run of `@property` rules by name, keeping the last
  registration of each, at the top level and inside any block. CSS Properties
  and Values API 1 sec. 2 makes registrations for different names
  order-independent and gives the last registration of a name, so two
  stylesheets registering the same set differed only in the order they happened
  to emit them (#227)
- `--diff=canonical` skips the rule-regrouping passes: factoring a shared
  declaration into a selector list, and synthesising nesting from a run of
  adjacent rules. Both depend on how the input happened to order its rules - a
  rule sitting between two others decides whether either applies - so the same
  stylesheet written either way canonicalised to different forms, and the
  difference was reported as a missing declaration or an added rule
  (#215, #224)
- `Css.optimize` takes `?regroup` to turn those passes off (#215, #224)

### Parsing

- Read `perspective: none` and `text-underline-offset: auto`, and allow a
  negative `text-underline-offset`. Both keywords are the properties' own
  grammar (and `none` is `perspective`'s initial value), but the readers took
  a non-negative length only, so the declaration was dropped with a
  warning (#212)

### Nesting

- Flattening a nested rule distributes the parent over every branch of a
  nested selector list, per CSS Nesting 1: `.p { a, b { ... } }` is
  `.p a, .p b`. The parent was combined with the list as a whole, so the
  combinator landed on the first branch only and every later branch
  escaped as a top-level selector matching the whole document (#205)
- Substituting `&` wraps a complex parent selector in `:is()`, per CSS
  Nesting 1. Flattening `.a .b { .dark & { ... } }` produced
  `.dark .a .b`, which matches a different set of elements than
  `.dark :is(.a .b)` (#194)

### Parsing

- Parse a nested rule whose selector starts with an identifier, such as
  `h2:where(...)`. CSS Nesting 1 makes that prelude ambiguous with a
  declaration until the block appears, and the rule was dropped with a
  warning (#193)
- Parse the function form of `<general-enclosed>` in media queries, so a
  grammatical but unrecognised query such as `theme(static)` is kept as
  never-matching instead of discarding the `@media` block or
  `@import` (#192)

### Printing

- `background-position` and `mask-position` print one position per layer,
  comma-separated. The layers were joined with spaces, so
  `background-position: 30% 50%, 70% 50%` read back as a single four-value
  position and minified to `30% 70%` (#209)

### Minification

- The optimizer logs its factoring decisions at debug level on the
  `cascade.factor` and `cascade.optimize` sources, so
  `--log=cascade.factor:debug` shows each fixpoint iteration and every segment
  the transfer gate reverts or the preflight skips. The sources existed and
  nothing ever wrote to them (#239)

- `grid-auto-flow: row dense` minifies to `dense`. CSS Grid 2 sec. 7.6 gives
  the property as `[ row | column ] || dense` with `row` as the omitted axis,
  so the two are one value. `column dense` keeps its axis (#230)
- A zero angle written in radians canonicalises to `0deg`, so it reaches the
  folds the other units already reached: `filter: hue-rotate(0rad)` is
  `filter:hue-rotate()`. Radians are otherwise left alone, since the
  conversion goes through pi and is not exact, but zero is the same angle in
  every unit (#229)
- `stop-color`, `flood-color` and `lighting-color` minify as the `<color>`
  SVG 2 and Filter Effects 1 define them to be, rather than surviving as
  opaque unknown-property text: `stop-color: #ffffff` is
  `stop-color:#fff` (#228)
- `--minify` no longer re-runs the global factoring fixpoint on a segment
  whose result the transfer gate has already discarded. The pipeline
  re-presents one segment across its iterations with a rule or two moved,
  so the exact-match memo missed and the work repeated; on a large
  stylesheet that was seventeen runs over 2450 rules, every one thrown
  away. Output is unchanged (#221)
- A rule with nested children absorbs a later rule with the same
  selector, when the nested children and the declarations that would move
  past them are disjoint. A single nested block used to freeze a rule
  against every later rule sharing its selector (#203)
- A selector list that mixes a vendor pseudo-element with ordinary
  selectors is split. A browser that does not know
  `::-webkit-search-cancel-button` drops the whole rule, and with it the
  declarations of every other selector in the list (#203)
- A `font-stretch` keyword minifies to the percentage CSS Fonts 4 defines
  it as, which is never longer. The `font` shorthand keeps the keyword,
  where a percentage is invalid (#206)
- Adjacent gradient stops of one colour fold into the double-position
  stop CSS Images 4 defines as their exact equivalent, so
  `currentColor 0, currentColor 1px` is `currentColor 0 1px` (#214)
- A `0deg` linear-gradient angle is dropped and the stops reversed, since
  the default direction is that angle turned 180 degrees. Applies while
  no stop carries a position, which the reversal would have to mirror,
  and never to the legacy prefixed gradients, which measure their angle
  from a different zero (#214)
- `fill-opacity`, `stroke-opacity`, `stop-opacity` and `flood-opacity`
  minify as the `<alpha-value>` they are, rather than surviving as opaque
  unknown-property text: `fill-opacity: 0.1` is `fill-opacity:.1` (#214)

### Custom properties and `@layer`

- `Css.inline_vars` resolves `var()` across `@layer` boundaries, and
  folds a custom property redefined across layers on the same element to
  its cascade winner. A layer only orders competing declarations, it
  never scopes custom-property visibility, so a layered stylesheet now
  inlines like its unlayered form instead of leaving a live `var()` that
  could resolve to the wrong definition (#187, #189)
- `cascade apply` projects rules inside `@layer` onto elements; a fully
  layered stylesheet (such as Tailwind v4 output) previously inlined
  nothing (#188)

### Breaking

- `cascade fmt` and `cascade diff` drop `--memtrace`, and the library drops the
  `memtrace` dependency, which failed the build outright on a switch without
  it (#237)

- `Css.hex` raises `Invalid_argument` on a string that is not one of `#rgb`,
  `#rrggbb`, `#rgba` or `#rrggbbaa`. It used to return opaque black, so a
  caller's own bad hex reached the output as a plausible wrong colour instead
  of a failure. `Css.hex_opt` is the deciding form for callers that want to
  handle it. Parsing is unaffected: the declaration reader already warned and
  dropped (#232)

### New properties and values

- Read `stroke-miterlimit` as the `<number>` SVG 2 sec. 13.3 defines, so it
  minifies like one (`4.0` is `4`) and a constant `calc()` folds. A value
  below 1 is out of range, since the limit is a ratio that bottoms out
  at 1 (#236)

- Read `stroke-linecap` and `stroke-linejoin`, including the SVG 2 sec. 13.3
  additions `miter-clip` and `arcs`. Both parsed as unknown properties (#235)

- Read `fill-rule` and `clip-rule`, the SVG 2 sec. 13.5 / 14.4 properties that
  share one `<fill-rule>`. Both parsed as unknown properties, so their values
  survived as opaque text (#234)

- Complete the logical border properties: `Css.border_block_color`,
  `Css.border_block_start_color` and `Css.border_block_end_color`, the
  `Css.border_inline_width` and `Css.border_block_width` shorthands (with
  the `logical_border_width` type), and the start/end style longhands
  (`Css.border_inline_start_style` and its three siblings). A declaration
  such as `border-inline-start-style: dashed` used to parse as an
  unsupported property (#197, #198, #199, #200)
- Add `Css.parse_font_family`, `Css.parse_list_style_type` and
  `Css.parse_list_style_image`, the single-value readers behind the
  `font` and `list-style` shorthands. Reading one of those out of a theme
  token meant hand-rolling the keyword table, which cannot see a `var()`
  among the entries (#201, #202)
- Add `Css.Values.oklch_none_hue` to build achromatic colours with a
  missing hue component, printed as `oklch(55.6% 0 none)` per CSS
  Color 4 (#190)

### Diff report

- `cascade diff` bounds its report: it prints the whole difference tree
  while that stays short, and otherwise falls back to the deepest level
  that fits, with `--depth` to pin a level or ask for the tree in full.
  A whole-stylesheet comparison used to print every level, so the shape
  of the change was only reachable by grepping the tree connectors out of
  the output (#210)
- Parse warnings print above the difference report instead of below it,
  capped per side with the remainder counted. A declaration the parser
  dropped qualifies every difference under it, and a stylesheet that
  trips one unsupported syntax repeatedly used to bury the report (#210)
- Conditional blocks that only changed position are reported as a shift
  run. Merging two same-condition blocks renumbers every block after
  them, and each was listed twice, once per side, so a single merge read
  as a wholesale rewrite of the container (#210)

### Canonical diff

- `--diff=canonical` expands every selector-list rule onto its branches
  in the projection, not only the lists that share a branch with another
  rule. The canonical form used to depend on how the input happened to
  group its selectors, so `.a,.b{margin:0}` and `.a{margin:0}.b{margin:0}`
  compared as different stylesheets (#204)
- The projection folds two conditional blocks that share a condition
  into one, gated by the same interval test that gates folding two
  occurrences of a selector. Two sheets that split the same `@media`
  content differently never converged, so the comparator fell back to
  reporting the block structure positionally: a merge plus a phantom
  entry for every renumbered block (#211)
- The projection converges on more cascade-equivalent inputs: it
  normalises the space after a top-level comma in a custom-property value
  (leaving text inside quotes alone), drops a declaration that a later
  rule with the identical selector also writes (leaving `!important`
  alone, since that changes the winner), and pairs rules that match
  exactly before falling back to the property signature (#206)

## 1.0.0

First public release. Cascade was extracted from the [tw](https://github.com/samoht/tw)
(Tailwind CSS v4 in OCaml) project as a standalone CSS command-line tool and
library, then stabilised over several internal milestones.

### Library

- Typed CSS AST: selectors, declarations, values, statements, and
  stylesheets are sealed ADTs. Invalid constructions are caught at compile
  time.
- Single warning-aware parse entry point:
  - `Css.of_string` runs CSS Syntax Level 3 recovery and returns
    `(parse, Error.t) result`, where `parse = { stylesheet; warnings }`.
  - `~strict:true` promotes the first warning to `Error _`.
  - `Css.of_string_exn` returns the recovered stylesheet directly and raises
    `Error.Parse_error` on `Error`.
- Pretty-printer with separate pretty and minified contexts
  (`Css.to_string ?minify`), with several typed printers exposed
  (`pp_color`, `pp_length`, ...).
- Structural transforms (`fold`, `map`, `sort`, `flatten_nesting`,
  `inline_imports`) and structural CSS diff utilities via the
  `cascade.diff` sub-library.
- Optimizer with deduplication, rule merging, selector combining, and
  shorthand/longhand coverage including `all` reset folding. Rule merging is
  order-independent: rules are scheduled through a conflict DAG so cascade-safe
  reorderings converge on the same output regardless of source order.
- Minification optimises estimated compressed (gzip) transfer size by default:
  a global factoring that shrinks raw bytes but would grow the compressed
  output is not applied. Pass `~objective:\`Raw` (CLI `--objective=raw`) to
  optimise raw bytes instead, for output that ships uncompressed.
- Spec coverage:
  - Selectors Level 4 -- including `:has()`, `:is()`, `:where()`, `:not()`,
    nesting `&`, and full attribute syntax.
  - Values & Units Level 4 -- `calc()`, `clamp()`, `min()`, `max()`,
    `minmax()`, the modern length units, durations, angles.
  - Color Level 4 -- 15 colour spaces including `oklch()`, `oklab()`,
    `lch()`, `hwb()`, `color-mix()`, plus the 148 named colours.
  - Conditional Rules Level 3-5 -- `@media`, `@supports`, `@container`
    (including typed `style()`/`scroll-state()` queries with range
    operators), `@when` / `@else`.
  - Cascade Level 5 -- `@layer` declarations and blocks, CSS-wide
    keywords, and `all` reset semantics in the optimizer.
  - Custom Properties Level 1 -- `var()` parsing/printing, typed
    fallbacks, theme/default substitution, `@property` registration.
  - Fonts Level 4 (`@font-face` descriptors), Animations Level 1
    (`@keyframes`, `@starting-style`).
- Over 400 typed properties cover box model, flexbox, grid (including
  `grid-template-areas` validation), logical properties, typography
  (`font-variant-*`, `text-emphasis-*`, `text-decoration-skip-*`,
  `initial-letter*`), borders and `border-image`, backgrounds and
  gradients, transforms (`translate`, `scale`, `rotate`, `transform`),
  transitions, animations (`animation-range*`, scroll-driven timelines),
  filters, masks, scroll snap, anchor positioning (`position-anchor`,
  `position-area`, `position-try-fallbacks`), view transitions, and the
  common vendor-prefixed properties.
- Custom-property workflows: typed `<syntax>` parsing for `@property`,
  registered-property substitution against an explicit `Css.Context.t`,
  and round-trip-stable `var()` serialisation with literal fallbacks.

### CLI tools

- `cascade` -- pretty-print and minify CSS files. It accepts stdin via `-`
  or a missing file argument, and writes output to stdout.
- `cascade --minify` applies the standard safe transforms, including
  deduplication, rule merging, selector grouping, empty-rule elimination, and
  nested-rule flattening, optimising estimated gzip transfer size by default
  (`--objective=raw` optimises raw bytes instead).
- `cascade --inline-imports` resolves local `@import` rules relative to the
  input file, and `cascade --inline-vars` substitutes static custom-property
  references. `--keep-vars=NAMES` preserves selected custom properties.
- `cascade diff` provides structural CSS diffing between two files with
  `auto`, `tree`, `string`, and `canonical` modes; respects `NO_COLOR`,
  `CASCADE_COLOR`, and `--color`, and colours only when stdout is a tty.
  Identical files exit 0 and differing files exit 1, so the command slots
  into CI checks and git hooks. The `canonical` mode projects both sheets to a normal form
  first, so equivalent factorings -- different rule grouping, cascade-safe rule
  and declaration order -- compare identical rather than as spurious changes.
- The CLI is installable as a binary through the Homebrew tap
  `samoht/tap/cascade`, with opam installation still available for OCaml users.

### Notes

- `cascade` parses already-decoded UTF-8 strings. The CSS Syntax Level 3
  byte-stream decoding step (BOM handling, `@charset` byte sniffing,
  HTTP/environment charset fallback) is the caller's responsibility.
- CSS nesting round-trips through the parser and printer, and the minifier
  flattens nested rules when safe.
- `@import` rules are preserved by default. Use `--inline-imports` for
  explicit closed-world filesystem inlining.
- No source-map support.
