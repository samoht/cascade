## Unreleased

### Breaking

- `Css.hex` raises `Invalid_argument` on a string that is not one of `#rgb`,
  `#rrggbb`, `#rgba` or `#rrggbbaa`, where it used to return opaque black. A
  bad hex from your own code now fails instead of putting a plausible wrong
  colour in the output. `Css.hex_opt` returns an option for callers that want
  to decide. Parsing is unaffected: the declaration reader already warned and
  dropped (#232)
- `cascade fmt` and `cascade diff` drop `--memtrace`, and the library drops the
  `memtrace` dependency, which failed the build outright on a switch without it
  (#237)

### Minification

- A `hue-rotate()` with a zero argument drops it inside a custom property, the
  way it already did in `filter` directly. Filter Effects 1 sec. 8.5 makes an
  omitted argument 0, and `hue-rotate` names a filter function and nothing
  else, so the two spellings are one value wherever the stream is substituted.
  `--diff=canonical` reported them as a difference (#257)

- Every SVG presentation property is typed, so its value minifies like any
  other rather than surviving as opaque text. `stop-color: #ffffff` is
  `stop-color:#fff`, `fill-opacity: 0.1` is `fill-opacity:.1`,
  `stroke-miterlimit: 4.0` is `4`, `stroke-dashoffset: 0px` is `0`,
  `stroke-dasharray: 4, 2` is `4 2`, `paint-order: stroke fill markers` is
  `stroke` and `fill stroke markers` is `normal`, and
  `vector-effect: non-scaling-stroke viewport` drops the redundant space
  keyword. `fill-rule`, `clip-rule`, `stroke-linecap` and `stroke-linejoin`
  are read as their own keyword grammars, including the Level 2 `miter-clip`
  and `arcs` joins (#214, #228, #234, #235, #236, #240, #241, #242)
- `grid-auto-flow: row dense` minifies to `dense`: CSS Grid 2 sec. 7.6 makes
  `row` the omitted axis, so the two are one value. `column dense` keeps its
  axis (#230)
- A zero angle written in radians canonicalises to `0deg` and so reaches the
  folds the other units already reached, making `filter: hue-rotate(0rad)`
  into `filter:hue-rotate()`. Other radian values are left alone, since the
  conversion goes through pi and is not exact (#229)
- A `font-stretch` keyword minifies to the percentage CSS Fonts 4 defines it
  as, which is never longer. The `font` shorthand keeps the keyword, where a
  percentage is invalid (#206)
- Adjacent gradient stops of one colour fold into the double-position stop CSS
  Images 4 defines as their exact equivalent, so
  `currentColor 0, currentColor 1px` is `currentColor 0 1px`. A `0deg`
  linear-gradient angle is dropped and the stops reversed, since the default
  direction is that angle turned 180 degrees; this applies only while no stop
  carries a position, and never to the legacy prefixed gradients, which
  measure their angle from a different zero (#214)
- A rule with nested children absorbs a later rule with the same selector when
  the nested children and the declarations that would move past them are
  disjoint. A single nested block used to freeze a rule against every later
  rule sharing its selector (#203)
- A selector list that mixes a vendor pseudo-element with ordinary selectors is
  split, so a browser that does not know `::-webkit-search-cancel-button` no
  longer drops the declarations of every other selector in the list (#203)
- `--minify` no longer re-runs the global factoring fixpoint on a segment whose
  result the transfer gate has already discarded. Output is unchanged; on a
  large stylesheet this was seventeen runs over 2450 rules, every one thrown
  away (#221)
- The optimizer reports its factoring decisions at debug level on the
  `cascade.factor` and `cascade.optimize` sources, so
  `--log=cascade.factor:debug` shows each fixpoint iteration and every segment
  the transfer gate reverts or the preflight skips (#239)

### Parsing

- A non-ASCII identifier is read as any code point at or above U+0080, so a
  selector such as `.text-↗` parses instead of being dropped with a warning.
  `Css.of_string` takes `?enforce_spec` and `cascade` takes `--enforce-spec`
  to restrict identifiers to the CSS Syntax 3 sec. 4.2 range list. Output is
  unaffected either way, since a code point outside that list is hex-escaped,
  so `.text-↗` and `.text-\2197` read to the same selector (#254)
- Read `perspective: none` and `text-underline-offset: auto`, and allow a
  negative `text-underline-offset`. Both keywords are the properties' own
  grammar, but the readers took a non-negative length only, so the declaration
  was dropped with a warning (#212)
- Parse a nested rule whose selector starts with an identifier, such as
  `h2:where(...)`. CSS Nesting 1 makes that prelude ambiguous with a
  declaration until the block appears, and the rule was dropped with a
  warning (#193)
- Parse the function form of `<general-enclosed>` in media queries, so a
  grammatical but unrecognised query such as `theme(static)` is kept as
  never-matching instead of discarding the `@media` block or `@import` (#192)

### Nesting

- Flattening a nested rule distributes the parent over every branch of a nested
  selector list, per CSS Nesting 1: `.p { a, b { ... } }` is `.p a, .p b`. The
  parent was combined with the list as a whole, so the combinator landed on the
  first branch only and every later branch escaped as a top-level selector
  matching the whole document (#205)
- Substituting `&` wraps a complex parent selector in `:is()`, per CSS
  Nesting 1. Flattening `.a .b { .dark & { ... } }` produced `.dark .a .b`,
  which matches a different set of elements than `.dark :is(.a .b)` (#194)

### Printing

- `background-position` and `mask-position` print one position per layer,
  comma-separated. The layers were joined with spaces, so
  `background-position: 30% 50%, 70% 50%` read back as a single four-value
  position and minified to `30% 70%` (#209)

### New properties and values

- Complete the logical border properties: `Css.border_block_color`,
  `Css.border_block_start_color` and `Css.border_block_end_color`, the
  `Css.border_inline_width` and `Css.border_block_width` shorthands (with the
  `logical_border_width` type), and the start/end style longhands
  (`Css.border_inline_start_style` and its three siblings). A declaration such
  as `border-inline-start-style: dashed` used to parse as an unsupported
  property (#197, #198, #199, #200)
- Add `Css.parse_font_family`, `Css.parse_list_style_type` and
  `Css.parse_list_style_image`, the single-value readers behind the `font` and
  `list-style` shorthands. Reading one of those out of a theme token meant
  hand-rolling the keyword table, which cannot see a `var()` among the
  entries (#201, #202)
- Add `Css.Values.oklch_none_hue` to build achromatic colours with a missing
  hue component, printed as `oklch(55.6% 0 none)` per CSS Color 4 (#190)

### Custom properties and `@layer`

- `Css.inline_vars` resolves `var()` across `@layer` boundaries, and folds a
  custom property redefined across layers on the same element to its cascade
  winner. A layer only orders competing declarations, it never scopes
  custom-property visibility, so a layered stylesheet now inlines like its
  unlayered form instead of leaving a live `var()` that could resolve to the
  wrong definition (#187, #189)
- `cascade apply` projects rules inside `@layer` onto elements; a fully layered
  stylesheet, such as Tailwind v4 output, previously inlined nothing (#188)
- `Css.vars_of_declarations` reports the `var()` references of the 39 properties
  a wildcard used to answer with none, so `Css.resolve_theme` emits the theme
  binding for `inline-size: var(--w)` as it already did for `width: var(--w)`
  instead of leaving the reference undefined (#266)

### Canonical diff

Equivalent stylesheets that differ only in how they were written now compare
identical, rather than as spurious changes.

- Selector-list rules expand onto their branches, so `.a,.b{margin:0}` and
  `.a{margin:0}.b{margin:0}` converge; previously the canonical form depended
  on how the input happened to group its selectors (#204)
- Two conditional blocks sharing a condition fold into one, gated by the same
  interval test that gates folding two occurrences of a selector. Two sheets
  that split the same `@media` content differently never converged, so the
  comparator fell back to reporting the block structure positionally (#211)
- A `@media not all and (X)` block is keyed as the `@media not (X)` that Media
  Queries 4 sec. 2.1 makes it equal to. The two spellings used to compare as a
  removed container plus an added one. Emission still keeps what it read, since
  a Level 3 parser rejects the shorter form (#231)
- A run of `@property` rules is keyed by name, keeping the last registration of
  each, at the top level and inside any block. Registrations for different
  names are order-independent, so two stylesheets registering the same set
  differed only in the order they happened to emit them (#227)
- The projection skips the rule-regrouping passes, which depend on how the
  input happened to order its rules, so the same stylesheet written either way
  no longer canonicalises to different forms. `Css.optimize` takes `?regroup`
  to turn those passes off (#215, #224)
- The projection also normalises the space after a top-level comma in a
  custom-property value, drops a declaration that a later rule with the
  identical selector also writes (leaving `!important` alone, since that
  changes the winner), and pairs rules that match exactly before falling back
  to the property signature (#206)

### Diff report

- An `@property` is compared on its whole body, and the entry names the
  descriptors that differ. Two registrations for one name differing in `syntax`
  or `initial-value` compared equal, so `--diff=canonical` called the
  stylesheets identical, and an `inherits`-only change was reported as a
  position change (#264)

- A rule is reported as reordered only when it moved against another rule.
  Dropping a rule shifted every position after it, so an unmoved rule was
  reported as reordered, and one move was reported on every rule it passed
  (#263)

- The size summary lists the two files in the order of the `---` and `+++`
  headers below it. It printed the second file first, so a comparison that
  added rules read as a shrink and every percentage was measured against the
  wrong baseline (#261)

- A selector written by more than one rule is reported once, at the top level
  as well as inside a container. Two rules under one selector produced a node
  each carrying the same label, one saying a declaration was added and the
  other that a different one was removed, which reads as a contradiction. When
  every declaration the selector writes survives on both sides, spread
  differently over those rules, the entry names the move and lists them, and
  the summary counts it as a rearranged rule. The difference is still reported:
  which rule carries a declaration decides how it resolves against an
  overlapping selector's rules (#259, #260)

- Blocks sharing a condition are reconciled one for one instead of by whether
  the condition appears at all. Three `@container` blocks with one condition
  against two of them reported two changed containers, each inventing an added
  rule, rather than the one block that was removed. The same pairing decides
  media, layer and supports blocks (#258)

- `cascade diff` bounds its report: it prints the whole difference tree while
  that stays short, and otherwise falls back to the deepest level that fits,
  with `--depth` to pin a level or ask for the tree in full. A whole-stylesheet
  comparison used to print every level (#210)
- Every statement of an added or removed container is reported. Statements the
  renderer could not see as rules, such as a nested `@media`, were dropped
  without a trace while still being counted, which also put the last-child
  connector on the wrong entry. A container is no longer counted a second time
  as a rule difference with an empty selector (#253)
- Parse warnings print above the difference report instead of below it, capped
  per side with the remainder counted. A stylesheet that trips one unsupported
  syntax repeatedly used to bury the report (#210)
- Conditional blocks that only changed position are reported as a shift run.
  Merging two same-condition blocks renumbers every block after them, and each
  was listed twice, once per side, so a single merge read as a wholesale
  rewrite of the container (#210)

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
