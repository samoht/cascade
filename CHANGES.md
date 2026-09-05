## 1.2.0 (unreleased)

Most entries below are defect fixes, and the largest group of them has one
cause. Readers, printers and optimizer passes each walked the statement tree
with their own match ending in a catch-all case, so an at-rule added to the AST
later was skipped by all of them instead of causing a compile error. They now
share a single exhaustive walk, so adding a statement kind after this release
breaks the build at every place that has to decide about it.

Correctness was checked against a browser rather than against cascade. The
suite renders a sheet and its optimised form in headless Chrome, then compares
every property `getComputedStyle` reports on every element. It also replays
2960 recorded minification cases carrying six minifiers' answers. Several of
the fixes below are cases where Chrome disagreed with the CSS cascade emitted,
not readings of the spec.

**Upgrading from 1.1.0.** Many of the fixes below change the CSS cascade emits
for input 1.1.0 already accepted, and enough of them are miscompiles that a
page can render differently. If you shipped minified output built with 1.1.0,
re-run it and compare: `cascade diff --diff=canonical old.css new.css` exits 1
and prints the difference wherever the two are not equivalent. Library callers
should start at `### Breaking`, where the public module set and the parser
entry points both moved.

### Breaking

- `Css.filter` gains `Omitted of filter_function` to retain empty calls.
  Exhaustive visitors must handle this leaf; normalization equates it with
  its specified default (#870).

- `Css.Supports.t` gains `General_enclosed` for opaque parenthesized feature
  tests. Exhaustive visitors must preserve this new leaf (#869).

- `Css.Context.query` gains `media_inapplicable`, distinguishing a recognized
  feature that matches no value from an unknown feature. Direct record
  constructors need the new field; `Context.query ()` defaults it to `[]`
  (#868).

- `cascade diff` exits 2, not 0, when it finds no difference and had to drop a
  declaration or a rule it could not read: what it dropped reached neither side
  of the comparison, so identity is not a verdict it can give. Two sides that
  dropped the same source text still exit 0: the comparison did see the same
  thing twice there. A gate that reads any non-zero status as "differs" needs
  updating (#832, #833, #834, #835, #836)
- `Cascade.Error.t` gains `recovery`, which says whether the reader dropped the
  construct the error is about or kept it in the output. Exhaustive record
  patterns must bind it or add `_`; `Cascade.Error.v` fills it in (#834)
- `Cascade.Parser.to_string_custom` is gone. Call
  `Cascade.Parser.string_of_components`, which renders a custom-property token
  stream identically (#806)
- `cascade` requires `cmdliner >= 2.0.0`, the release that lets either side of
  `cascade diff` name standard input as `-` (#796)
- `Css.parse` adds `source : Css.Source.t option`; exhaustive record patterns
  must bind it or add `_`. `Css.of_string ~preserve_source:true` fills it with
  exact authored comments, syntax, trivia ownership and coordinates (#747)
- Implementation modules are no longer usable through accidental `Cascade.*`
  aliases: `Baseline`, `Block`, `Common`, `Factor`, `Flatten`, `Inline`,
  `Merge`, `Rule`, `Rule_index`, `Rule_order`, `Shorthand`, `Size`, `Summary`,
  `Rule_graph`, `Rule_scheduler`, `Pool`, `Order_maintenance`, `Preflight`,
  `Ctx`, `Cover`, `Edge`, `Loop`, `Rule_candidate`, `Rule_rewrite`,
  `Factor_safe`, `Gzip_size` and `Index` are private, as are the six shared
  `*_intf` modules. The supported `Css` aliases and parser roots remain public;
  `Aria`, `Color_space` and `Nest` gain coherent `Css` aliases (#539)
- Redundant public aliases are gone. Use `Declaration.pp` and
  `Declaration.to_string` instead of `pp_declaration` and
  `string_of_declaration`; `Stylesheet.empty`, `Stylesheet.read` and
  `Stylesheet.to_string` instead of `empty_stylesheet`, `read_stylesheet` and
  the old string-valued `Stylesheet.pp`; `Keyframe.to_string` instead of
  `string_of_selector`; `Pp.float` instead of `Pp.float_compact`; and
  `Css.inline_style_of_declarations`, which keeps the `optimize` option,
  instead of `Stylesheet.inline_style_of_declarations`. `Stylesheet.pp`,
  `Css.pp` and `Container.pp` are composable `Pp.t` printers rather than
  string-returning aliases (#544)
- Also gone: the one-off `Css.Transform`, `Css.Transform_origin`,
  `Css.Perspective_origin` and `Css.Animation` string parser modules, replaced
  by the matching `Properties.read_*` parser over a `Cursor`; the
  Tailwind-specific `inset_ring_shadow`, replaced by `shadow ~inset:true`; and
  the duplicate `Box_shadow` constructor of the typed `kind` GADT, replaced by
  `Shadow`. `Css.Gradient_direction.of_string` and the `Box_shadow` property
  constructor are unchanged (#544)
- `Cascade.Reader` is the character cursor `Lexer` drives and no longer reads
  CSS: its combinators, value readers and delimiter helpers are gone.
  `Cascade.Cursor` carries the same names over a component-value stream, so
  read CSS from a `Cursor.of_string` or `Cursor.of_reader` instead (#509, #514)
- `Cascade.Cursor.pair` and `triple` rewind the cursor when the separator or a
  later parser fails. Code that caught `Parse_error` from either and read on
  from the advanced position now re-reads what the first parser consumed (#509)
- Public declaration helpers whose CSS values require a non-empty list raise
  `Invalid_argument` for `[]`, and `Cursor.list` requires at least one item
  unless a grammar opts out with `~at_least:0`. Empty CSS grammar lists and
  non-positive explicit `repeat()` counts are rejected (#691)
- `Declaration.declaration` is a private variant. Its constructors remain
  available for pattern matching, but construct values with `Declaration.v` or
  `Declaration.theme_guarded`: the public records exposed their cached hash, so
  a caller could make the optimizer treat equal declarations as unequal (#527)
- `Css.Stylesheet.edit_statements` callbacks return `Css.Stylesheet.Keep`,
  `Replace` or `Drop` through the new `Css.Stylesheet.edit` type, where they
  leaked the otherwise-internal `Common.List.edit` type (#539)
- `Css.Container.of_components` and `Css.Media.of_components` are gone: call
  `Container.read` / `Media.read` with a cursor over the prelude's components
  (`Cursor.sub`). `Container.of_string`, `Media.of_string_strict`,
  `Supports.of_string` and the three `Font_face.*_of_string` raise
  `Cursor.Parse_error` where they raised `Failure`, so replace a `Failure`
  handler around any of them (#496, #497, #499, #501)
- `Declaration.of_string` raises `Cursor.Parse_error` for every input it
  refuses, anchored on the text that failed, where empty, blank and
  selector-shaped input raised `Failure` (#535)
- `Css.Selector.of_string ""` raises `Error.Parse_error`, like every other
  malformed selector, where it raised `Invalid_argument` (#528)
- `Css.Supports.property` raises `Failure` on a value that is not a
  `<declaration-value>`, where it wrote the text unchecked:
  `property "color" "red) or (color:blue"` emitted a condition a browser
  answers true for, so the rules the caller meant to guard applied (#459)
- `Css.Declaration.custom_property` raises `Failure` on a name and value that
  do not write back as the one declaration they name:
  `custom_property "--a" "red;--b:blue"` wrote a second declaration. For
  strings from outside the parser use `parse_custom_property`, the same check
  as an option (#421, #428)
- `Css.statement_declarations` is gone. Call the exhaustive
  `Css.Stylesheet.statement_declarations`, which reaches every declaration a
  statement holds (#348)
- `Css.vars_of_rules` is `Css.vars_of_stylesheet`, and now reports a `var()`
  inside a nested rule, an animation frame or a page margin box as well as in a
  top-level rule (#382)
- `Css.Stylesheet.layer_name` is the identifiers a `<layer-name>` is made of
  rather than the text between them, so `Css.layers`, `layer_block`,
  `layer_decl`, `layer`, `layer_of`, `as_layer`, `layer_block_name`,
  `layer_statement_name_list`, `import_layer_name` and the `?layer` argument of
  `custom_props` carry a `string list`. `read_layer_name` and
  `string_of_layer_name` convert between the two (#442)
- `Css.Media.equal` reads normalised query structure where it read serialised
  text, so it is no longer `Css.Media.compare a b = 0`: `(min-width: 10px)` and
  `(width >= 10px)` are now equal, and two queries that print alike but parse
  apart are not. `Css.Media.normalize` exposes the normal form (#516)
- `Css.Media.kind` classifies a negated width bound by the range it actually
  matches, so `not (min-width: 640px)` sorts with the upper bounds and two
  `not`s cancel. `sort_key`, `group_order` and `compare` follow, so a caller
  sorting media queries gets a different order (#328)
- `Css.color` keeps the origin of a relative colour as a colour rather than in
  the opaque tail: `Relative_rgb` carries `color * string` and `Relative_color`
  carries `string * color * string`, so an expression or a pattern naming
  either takes the extra field (#313)
- `Css.kind` gains `Radial_shape`, `Radial_size` and `Position_value`, so a
  match on it is no longer exhaustive. They let `Css.Variables.var` bind a
  radial gradient's shape, size or centre `<position>` as a typed custom
  property rather than an opaque token stream (#508)
- `Css.Stylesheet.moz_document_condition` gains `Url_exact`, `Domain`,
  `Media_document` and `Regexp`, so a match on it is no longer exhaustive
  (#461)
- `Css.border_width` gains `Dimension`, so a match on it is no longer
  exhaustive. The reader accepted only a fixed list of units, so
  `border-width: 3dvh` was dropped as invalid while `margin: 3dvh` was read
  (#612)
- `Css.Pp.ctx` gains `in_style_rule`; record expressions must set it and record
  patterns must bind it or use `; _` (#374)
- `Cascade.Reader.parse_error` gains `line` and `col`, and `filename` holds a
  source name where it packed `"<CSS input>:L:C"`: read the location from the
  two new fields, and `with_filename` keeps it instead of overwriting (#491)
- `Tree_diff.Content_changed` carries property/value pairs in
  `added_properties` and `removed_properties`, so callers can report the value
  and priority of a declaration that exists on only one side (#723)
- `font_weight.Weight` carries a number rather than an integer, and font
  feature and variation settings carry structured tag/value lists rather than
  pre-rendered strings, preserving fractional weights and variation values and
  feature indexes above one (#685)
- `place_items` gains `First_baseline` and `Last_baseline`, preserving the two
  modifier-first baseline positions accepted by `place-items` (#725)
- `position_value.Axis_edge_offset` carries a `length_percentage`, matching the
  percentage-capable `<bg-position>` offset it represents (#673)
- `text_box.Normal` represents the `normal` shorthand branch, which now parses
  and round-trips instead of being dropped (#671)
- `text_box.Box` carries an optional trim value, so edge-only values such as
  `cap alphabetic` parse and preserve the omitted slot (#670)
- `Properties.View_timeline` carries a `view_timeline_shorthand`, whose items
  include the optional inset slot, so `view-timeline:--v 10% 20%` parses and
  round-trips (#669)
- `timeline_shorthand_item.axis` is optional, so an omitted axis parses as
  `None` and round-trips without being rewritten as `block` (#668)
- `flex_basis.From_font` is gone. `from-font` is not part of the `<width>`
  grammar accepted by `flex-basis`; the constructor printed CSS that the
  `flex-basis` reader correctly rejects (#658)
- `Css.Properties.ray_size` names the five sizes `ray()` accepts directly, so
  write `Closest_side` for `Radial Closest_side`. The dropped `Radial` wrapper
  also admitted lengths and radii that `pp_ray_size` raised on (#549)
- `Css.Selector_summary.clear_memo` is gone. It did nothing, so a caller that
  called it can drop the call and see no change (#548)
- `Cascade.Component.pp` prints a debug dump with source locations, which is
  what it always printed, and now says so. For source text, call
  `Cascade.Parser.string_of_components` (#504)
- `--minify` keeps an author's `@supports` guard, and
  `Css.Supports.simplify_baseline` is gone. `@supports (height: stretch)` was
  treated as a test for the `height` property alone, so the guarded fallback
  was dropped and every browser without that value was left with nothing (#584)
- An `@supports` condition keeps the value the author wrote. Cascade used to
  rewrite it through the property's typed grammar, which changed what the
  condition tested. `Css.Values.normalize_color` loses its unused
  `in_feature_query` argument, and `Css.Declaration.parse_opaque_declaration`
  reads a declaration without its typed grammar (#587)
- `cascade diff --depth` is gone: a report is bounded by whole differences
  now, not by tree levels. Pass `--limit=none` where `--depth=max` was, and
  `--limit=N` where a level was pinned (#792)

### Parsing

Cascade used to keep the part of a value it recognised and ignore the rest, and
to lose a whole rule over one bad piece. Both are gone.

- A value a browser rejects is rejected whole, where the recognised prefix used
  to survive: `translateX(10px red)`, `max(1px, red)`, a mismatched `calc()`, a
  shadow whose colour interrupts its lengths, a fourth `text-shadow` length, a
  repeated `color-scheme: only`, a fractional `steps()` count, two edges on one
  axis of a `<position>`, `auto` as a colour or an SVG paint, a sizing keyword
  as the corner radius of a basic shape, and an unrelated keyword on a box size
  (#417, #617, #627, #629, #631, #673, #680, #688, #699, #701, #878, #882,
  #890, #891)

- A property whose grammar names a `<length>` takes neither a percentage nor an
  intrinsic-sizing keyword, and a `<time>` or `<angle>` needs its unit. Cascade
  read them wherever it read a length, so `border-width: 50%`, `perspective:
  50%`, `top: min-content`, `margin-top: none`, `transition-duration: 0` and
  `rotate: 0` all parsed, the last two turning a declaration browsers drop into
  one that works. `Css.Values.read_length` gains `?sizing` and
  `read_non_negative_length` gains `?length_only` (#871, #879, #880, #951,
  #952, #953, #954)

- Each grid property takes its own grammar. Values a browser drops are dropped
  (`grid-template-columns: [a]`, `[span] 1px`, `grid-column-start: 0`, `span
  -1`), `grid-auto-rows` refuses the area forms `grid-template` owns, `subgrid`
  takes its line-name list, and a track size accepts math resolving to `<flex>`
  (#708, #711, #712, #714, #717, #718, #724, #749)

- `font-variant` and `font-variant-alternates` have typed values, and
  `@font-face` and `@font-palette-values` read their own `font-family`
  grammars. Carried as opaque text the first two accepted anything, so
  `small-caps unicase`, `jis78 jis83` and `swash(inherit)` parsed where Chrome
  refuses them (#695, #963, #964)

- The `font` shorthand reads and writes all nine `<font-width-css3>` keywords.
  It read two of them, and printed the width its longhand had normalised to a
  percentage, which the slot has no room for and cascade's own reader refused,
  so `font: ultra-condensed 12px serif` was dropped and `font-stretch:
  expanded` contracted to a `font` no browser reads (#965)

- An `@property` syntax rejects a component with two multipliers, a multiplied
  `<transform-list>` and a `*` combined with anything, so the registration
  drops rather than typing values by a grammar it cannot honour. Two syntaxes
  that used to hang the parser are read (#398, #707, #710, #713)

- Values browsers accept are read rather than dropped: `animation-duration:
  auto`, `grid: none / 200px`, `place-items: flex-start baseline`, the full
  `text-wrap` grammar, `row-gap: normal`, a negative
  `text-decoration-thickness`, a custom counter-style name,
  `transition-property: all, opacity`, a zero-offset `box-shadow`, an empty
  `blur()`, a `style()` range query, a `calc()` resolving to a number, a sole
  baseline in `place-content`, `display: flow-root list-item`, `outline: thin
  solid red`, a bare `stroke-width` number, the `svg||td` column combinator,
  `border-inline`, a descending `@font-face` range, a valid
  `@font-palette-values` rule, `:dir(auto)`, `@media (min-width: 0)`,
  `@-moz-document`'s URL matchers, a `border-image` repeat keyword standing
  alone, a `text-decoration` component with no line, `white-space: collapse`, a
  time-valued `round()` in a delay, and a value whose grammar ends in an
  optional component (#334, #335, #427, #456, #461, #551, #572, #579, #594,
  #633, #641, #644, #646, #665, #666, #667, #682, #739, #805, #827, #870,
  #874, #877, #881, #884, #885, #886, #887, #889, #909)

- `margin` mixes `auto` with lengths in any slot, `border-style` takes the four
  side styles, `border-block-style` and `border-inline-style` take the start and
  end edges, and every `border-*-radius` corner takes a horizontal and a
  vertical radius

- `display` accepts `grid-lanes` and `inline-grid-lanes`, the two values CSS
  Grid 3 sec. 2.2 establishes grid lanes layout with

- An empty value is no longer a declaration: `border:`, its per-side and logical
  variants, `column-rule:` and `outline:` are dropped with a warning, where
  `border:` read as `border: none` (#640)

- `offset`, `offset-anchor`, `offset-position`, `position-area`,
  `position-try-fallbacks` and `white-space` are typed against their own
  grammars, and a literal reads as the node the library exports, so a
  constructed declaration and a parsed one compare and hash equally (#478,
  #485, #495, #651, #652, #653, #654, #657, #674, #683, #888)

- A bad piece is dropped on its own and the rule around it survives. A nested
  rule, a descriptor, a stray `;`, a margin at-rule, an `@counter-style` with no
  descriptor, an `@media` condition, an `@font-face` descriptor name, an
  `@scope` bound and leftover tokens after a value are each dropped alone, and
  an unrecognised at-rule reaches the output with its block intact.
  `Optimize.drop_unknown_at_rules` drops such a rule on request (#374, #380,
  #384, #388, #392, #399, #402, #403, #404, #405, #419, #420, #469, #483,
  #727, #895, #897, #946, #947, #948, #949)

- An at-rule written among `@font-face` descriptors costs itself alone, as CSS
  Syntax 3 §5.5.5 requires, instead of taking the whole rule with it

- An unknown `@supports` or media condition keeps its guard, so an applicable
  `or` branch and a negated capability test no longer lose their rules (#867,
  #869)

- A selector no browser can match is dropped rather than written back, and a
  pseudo-element that carries structure keeps the compounds it allows:
  `::file-selector-button:hover` and a chained `::part(label)::before` survive
  where a combinator after a pseudo-element does not. `Css.Selector.of_string`
  refuses a string that is not a selector rather than reading it as an element
  name (#418, #426, #430, #441, #552, #553, #556, #559, #950)

- Keyword, at-rule and function names match without regard to case, so
  `grid-column: SPAN 2`, `@MEDIA`, `RGB()`, `VAR(--x)` and `:dir(LTR)` read
  (#602, #603, #604, #620, #622, #767)

- An escaped name reads as the name it spells: `@supports (--x\3b y: red)` is
  read, and `@layer a\2e b` names the layer `a.b` rather than the sublayer `b`
  of `a` (#437, #442)

- A `var()`, `env()` or `attr()` call keeps the type of the component it stands
  for and defers CSS-wide keyword validation to substitution, so one written as
  a slot of `border-radius`, `gap`, `place-*`, `grid-auto-flow`,
  `transform-origin`, `border-spacing`, a border-image dimension or a
  `page-break-*` property is read as that slot. A value the typed reader
  refuses is preserved opaquely when the property is unknown or the value is a
  runtime substitution (#511, #518, #726, #729, #734, #735, #787, #813)

- Cascade reads back everything it writes. Minified `@scope to (...)`, `rotate`
  with a negative axis, a relative colour's channels, a `-webkit-gradient`
  `color-stop()` and `center` point, `text-decoration: none solid`,
  `transition: none 1s`, an unterminated string, a quoted `animation-name`, a
  keyword-shaped keyframe name, an at-rule body ending on a backslash, a `u+a`
  selector outside `unicode-range`, an explicit `animation: spin 0s`, a `url(`
  at end of input and a repeating gradient built from a `var()` each survive
  the round trip (#558, #656, #875, #876, #894, #896, #898, #899, #900, #901,
  #902, #903, #910, #911, #912, #913)

- An out-of-range `oklab()` or `oklch()` lightness written as a percentage
  clamps as the bare number already did, and `color(from <origin> srgb r g b)`
  folds to the origin when that origin is itself a `color()` (#904, #905)

- Everything the parser repaired or dropped is reported, so strict mode rejects
  it and the caret points at the value the author wrote rather than past the
  last byte. `@media screen {` used to swallow the rest of the file and still
  return `Ok` with no warnings. `Cascade.Reader.int` raises `Parse_error` on a
  fractional or out-of-range number instead of truncating (#466, #472, #473,
  #477, #484, #496, #497, #499, #501, #538, #789, #793, #801)

### Printing

- `text-decoration`, `mask-border`, `animation` and `mask` no longer minify to
  an empty value. `text-decoration:solid` printed `text-decoration:`, which no
  parser reads back, once dropping the initial style left no slot, and a `mask`
  layer holding only initials printed the same way (#682, #955)

- An identifier is printed with the escapes needed to read it back as the same
  name, where `@layer a\3b b` printed two statements naming a layer the input
  never had and `--x\3b y` split into two declarations (#435, #436, #526, #598)

- A compound operand of `not`, `and` or `or` in a `@media` condition keeps its
  parentheses, and unwrapping a `@supports` nested in a style rule keeps the
  `;` before the sibling that follows. Both printed CSS browsers and cascade's
  own reader reject, losing the block or running two declarations together as
  `color:#00fcolor:green` (#319, #370)

- A `calc()` printed without `--minify` keeps the parentheses the author wrote,
  redundant ones included, while `--minify` removes those and keeps a
  precedence-sensitive `calc((1px - var(--a)) * 3)` (#721)

- A NaN-valued number prints as `calc(NaN)` and a NaN-valued dimension as
  `calc(NaN * 1unit)`, where `calc(sqrt(-1) * 1px)` printed `NaNpx`, which
  browsers drop (#425)

- `Css.to_string` renders the sheet once. It used to render it twice, the first
  pass only to measure the output and size the buffer exactly (#479)

### Minification

- `--minify` contracts every shorthand family from the longhands that name it.
  This release adds the four-sided box families (`border-width`, `border-style`,
  `border-color`, `scroll-margin`, `scroll-padding`), the eight border sides and
  the logical axes, `border-radius` with elliptical corners, `border-image`,
  `background`, `mask`, `columns`, `column-rule`, `offset`, `font-synthesis`,
  `-webkit-text-stroke`, `text-decoration`, `flex-flow`, `text-emphasis`,
  `grid-row`, `grid-column`, `grid-area`, `grid-template`, `grid`,
  `overscroll-behavior`, `contain-intrinsic-size`, `animation-range`,
  `scroll-timeline`, `view-timeline`, `container`, `background-position`,
  `white-space`, `text-wrap` and `font` (#915, #916, #918, #919, #920, #921,
  #922, #923, #924, #925, #926, #927, #928, #929, #930, #931, #932, #933,
  #935, #937, #939, #940, #942, #959, #960, #961, #965)

- A component written at its own initial, or written `initial`, reads as the
  slot the shorthand leaves out, so `flex-flow: row wrap` minifies to
  `flex-flow: wrap` and `flex: 0 0 auto` to `flex: none`. A repeated side folds
  to the shortest spelling naming the same sides (#917, #923, #924, #925,
  #934, #959)

- Contraction never resets a longhand the run did not write. A run that leaves
  one of the shorthand's own longhands unwritten now needs `--scope=stylesheet`,
  which is what the flag promises: under the default fragment scope an earlier
  `transition-timing-function` from another file is not cascade's to reset. A
  slot the same rule writes back after the run is answered and still contracts
  (#843, #845, #914, #943, #957, #958)

- `--minify` drops a longhand that only restates what the shorthand in front of
  it already wrote, so the whole border family written out minifies to
  `border: 1px solid red` alone (#956)

- `--minify` no longer contracts a run of longhands one of which is `inherit`,
  `unset`, `revert` or `revert-layer`. A CSS-wide keyword is a whole
  declaration value, so pasting one into a shorthand made a declaration every
  browser drops: `padding-left: inherit` beside its three siblings became
  `padding: 0 2em 10% inherit` and the element lost all four paddings

- `Css.Properties` gains the typed properties the contractions need:
  `background-position-x` / `-y` and their `-webkit-mask` twins,
  `column-rule-width` / `-style`, `white-space-collapse`, `column-height` and
  `column-wrap`. `border-image-outset` and `border-image-width` reject a keyword
  their grammar does not carry (#926, #927, #928, #936, #938, #941)

- A value keeps every digit and every unit the author wrote unless the shorter
  spelling means the same number. `.4285714em` came out short, `calc(hypot(1px,
  1px))` came out as `1.41421356`, and a computed dimension past a million units
  lost its own digits (#350, #354, #362, #367, #676, #731)

- `--minify` folds a value's spelling before two rules are compared, so a hex
  colour, a NaN, an unreduced `min()`, a same-unit `calc()`, a shorthand
  component at its longhand's initial, a repeated `font-family` entry, a
  `color-mix()` in a wide-gamut space, the `sin()`-`atan2()` family inside a
  custom property, an unitless `line-height: calc()`, logical minimum sizes,
  duration units, stepped functions, hue-angle units and `steps(1)` all reach
  one form (#471, #482, #597, #618, #624, #626, #635, #636, #637, #639, #640,
  #641, #647, #650, #672, #675, #681, #678, #755, #756)

- `--minify` keeps a value whose shorter spelling would mean something else:
  `display: block ruby`, `transition: opacity 0s 2s`, a lone `background`
  position, both disagreeing `<box>` values of a layer, `position-area: top
  center`, a box shorthand holding a top-level `var()`, an `src:` outside
  `@font-face`, a time unit in a `var()` fallback, a vendor-prefixed gradient
  written through a shorthand, a `font-family` name the author spelled out, and
  the `page-break-*` twin of a `break-*` property (#387, #390, #401, #457,
  #470, #547, #633, #634, #637, #640, #641, #736, #766, #782, #803)

- `--minify` drops the space at a `%` or `)` boundary in the box shorthands and
  in `inset()`, and prints a custom property as the author's own token stream,
  keeping the space a typed position or colour needs after a closing
  parenthesis and treating `[]` and `{}` as ordinary tokens (#614, #619, #623,
  #697, #700, #703, #709, #719, #720, #722)

- A selector rewrite that changes what an element matches is gone.
  `.c:not(:enabled)`, an author's `:not(:dir(ltr))` under `--enforce-spec`, a
  single-argument `:is()` and a double `:not()` all keep their form, while a
  whole-rule `:is(a, b)` whose arguments share one specificity splits into the
  list `a, b` before rules are compared (#377, #431, #593, #596, #655)

- Rules merge on what they mean rather than on how they are spelled: `@media`
  and `@container` blocks merge by query structure, adjacent `@starting-style`
  and `@container` runs merge, a nested `@supports` condition simplifies
  against the ones enclosing it, and `@media not all and (X)` minifies to the
  Level 4 `@media not (X)` (#323, #465, #516, #519, #585, #592, #809)

- A declaration keeps its place whenever moving it would change what an element
  computes, whether the neighbour writes the same longhand under another
  property name, sits behind a nested rule, or reaches the same element from
  another rule (#352, #364, #376, #383, #386, #414, #415, #447, #452, #453,
  #454, #742)

- `--minify` reaches inside every at-rule that has a body. `@-moz-document`,
  `@starting-style`, `@when` and `@else` are optimised and flattened, an empty
  one is dropped, a `@layer` whose own rules nest declarations is kept, and an
  unrecognised at-rule's opaque body is written back as the token stream it was
  read from (#341, #343, #344, #349, #372, #374, #389, #396, #560)

- Nesting emits only selectors a browser can match: a rule nested under a
  pseudo-element parent is dropped wherever it sits, flattening a rule nested
  under a selector list wraps the parent in `:is()`, and `--flatten-nesting`
  leaves the result flat (#759, #800, #818, #822, #823)

- Default minification adds the WebKit fallbacks Safari 16.4 and Chrome 111
  need, with matching `@supports` tests, and drops a vendor prefix only when its
  unprefixed twin is Baseline widely available. A feature query on a prefixed
  property keeps its guard (#325, #378, #447, #751, #758, #797)

- `Css.Resolve` matches an attribute selector the way an HTML document does:
  the name folds to ASCII lowercase, the values of the HTML attributes that
  ignore case fold with it, whitespace splits on every ASCII space, and an
  unquoted value's escapes are decoded (#944, #945)

- Computed-value evaluation resolves a direct `inherit`, `initial`, `unset`,
  `revert` or `revert-layer` for every typed property, and whether a property
  inherits is decided in one place from the typed property (#763, #764)

- `--minify` and `cascade diff` are faster on a large stylesheet, for
  byte-identical output. The slowest corpus stylesheet drops sharply, a long run
  of rules sharing one selector no longer allocates quadratically, a 4,000
  statement sheet is linear, nested group-rule merges optimise only the newly
  joined list, and a single invocation reaches a stable result where it used to
  need a second (#413, #422, #424, #468, #480, #486, #487, #493, #502, #505,
  #507, #517, #519, #523, #542, #543, #566, #664, #746, #750)

### Custom properties

- `Css.inline_vars` sees every place a `var()` can be written: an `@font-face`
  descriptor, `@page` and its margin boxes, a `@keyframes` frame,
  `@position-try`, a `@supports` condition and a nested rule. A descriptor
  reference used to be dropped at parse time, and `Css.custom_props` missed a
  name declared inside `@scope`, `@starting-style`, `@-moz-document`, `@when`
  or `@else` (#322, #341, #342, #375, #423, #555, #571, #573, #575, #577)

- Substitution preserves what the declaration was. A custom property keeps its
  cascade layer and caller metadata, a `page-break-*` declaration survives as
  itself, a reference marked resolved at runtime stays marked, and an
  overridden variable is reported through `~warn` (#315, #506, #520)

- `Css.inline_vars` resolves a property defined across cascade layers against
  the sheet's layer order, unwraps an `@layer` only where layer order and
  document order already pick the same winner, and keeps the `@property`
  registration of a custom property it leaves live (#357, #371, #373, #416)

- `--minify` keeps the quotes on a `<string>` written to a custom property whose
  `@property` syntax accepts only an ident, and the space between the
  repetitions of a `<type>+` initial value (#626, #704)

- `Css.resolve_theme` accounts for the declarations `@keyframes`, `@page`,
  `@position-try` and a `@supports` condition carry, builds each
  `theme_defaults` binding with `parse_custom_property` rather than reparsing
  assembled text, and binds a name that needs escaping instead of refusing it
  (#317, #324, #327, #421, #439)

- `Css.inline_vars` stays linear in at-rule nesting depth and no longer costs a
  square in the variable count: a 12,800-variable sheet is no longer quadratic
  (#481, #568, #569)

- `Css.Variables.read_reference_body` reads a `var()` argument list into a typed
  variable handle, and `typed_custom_property` writes a declaration from a value
  already typed by its `@property` registration (#626, #630, #642)

### Canonical diff

- A browser-backed sweep checks the guarantee itself: every pair
  `--diff=canonical` reports identical is rendered in headless Chrome and every
  computed-style difference is a conflation

- Canonical diff equates the rewrites that cannot change what a browser
  computes: a shorthand against its four side longhands, `:is(a, b)` against the
  list `a, b` when the arguments share one specificity, equal `@supports` blocks
  hoisted apart, `calc(28/14)` against `2`, and a redundant `@layer` order pin
  (#475, #655, #753, #756, #775, #776, #777, #842)

- A colour compares as a colour wherever it is written: a `none` channel as the
  zero CSS Color 4 sec. 4.4 makes it behave as, a relative colour's origin as a
  typed colour, a shadow in an unregistered custom property, and a one-word
  family name quoted or unquoted when the stream proves it is a family (#312,
  #313, #314, #440, #696, #705, #847)

- Canonical diff reports only reorderings that are really present in the two
  inputs, so a changed declaration no longer makes a neighbour read as moved, a
  cascade-neutral reordering stays unreported when the sheets also differ
  elsewhere, and writing one selector as a nested branch or as its own rule is
  not a move (#779, #780, #781, #819, #825, #826, #831)

- The canonical projection normalises the rules inside every at-rule that has a
  block, flattens authored nesting before comparing so a nested stylesheet and
  its flat equivalent read as identical, keeps structurally distinct
  `@container` conditions in separate cascade slots, and keeps content that is
  dead only under an assumption about browser support (#393, #529, #576, #760)

- `--diff=tree` compares a value on its minified spelling and prints it the way
  its own file spells it, prints the body of an added or removed rule as
  declarations, and pairs repeated occurrences of one selector by their
  declaration properties (#702, #706, #752)

- `cascade diff` no longer aborts when a reordered selector has the same rule
  index on both sides (#582)

### Library

- `Css.Resolve` answers the selectors engines answer: `:nth-child(... of S)`,
  the typed `:nth-of-type()` family, `:has()`, the `i` and `s` attribute case
  flags, and `:empty` for the elements Selectors 4 and the engines agree on
  (#607, #873)

- `Css.Resolve` ranks a cascade layer's own rules after every one of its
  sublayers, as css-cascade-5 sec. 6.4.3 requires, so `@layer a` outranks
  `@layer a.b` however the two were declared

- `Resolve.prepare` and `Resolve.Make.resolve_prepared` split the sheet-only
  work out of `resolve`, so a caller walking a document pays it once.
  `Resolve.Make.resolve` and `layer_order` document every block they leave out
  (#394, #567)

- `Css.Context.matches_media` respects zero-valued boolean features and
  resolution units and preserves unknown through negation, and
  `matches_container` requires the supplied container to support every queried
  feature (#868)

- `Css.Stylesheet` reaches every statement and declaration a sheet holds:
  `fold_statements`, `iter_statements`, `edit_statements`,
  `fold_declarations`, `statement_declarations`, `statement_children`,
  `map_statement_children`, `map_statement_declarations` and
  `at_declaration_site`. `Css.map`, `Css.sort`, `Css.layers`,
  `Css.layer_block` and `Css.flatten_nesting` reach a rule inside `@scope`,
  `@starting-style`, `@-moz-document`, `@when` or `@else` (#317, #337, #355,
  #356, #363, #368, #381, #382, #384, #389)

- `Css.equal_statement` and `Css.hash_statement` compare and key a statement
  without rendering it to CSS text, `Css.Values.hash_color` keys a colour,
  `with_alpha` sets one, and `Css.Properties.compare_property` and
  `Declaration.compare_prop_key` are a total order on a property identity
  (#513, #595)

- New building blocks: `Css.unknown_at_rule` for an at-rule cascade has no
  grammar for, `Css.Declaration.value_of` to read a value at a property
  witness, `Css.Properties.read_grid_template_tracks`,
  `Cascade.Syntax.is_ident`, `Css.Color_space.gamut_mapped_srgb_of_oklch`,
  `Css.Values.gamut_map_color`, `Properties.read_filter_function` and
  `pp_filter_function`, and the statement-merging passes as callable functions
  (#591, #592, #600, #616, #626, #717, #872)

- `Cascade_diff.Tree_diff.has_container_added_of_type` and
  `has_container_removed_of_type` look inside a container reported as a whole,
  and `Cascade.Error.to_string` prints back a snippet that is valid UTF-8
  (#395, #472)

- `cascade` drops its `uutf` dependency for the stdlib UTF-8 decoder, and the
  library no longer links `unix`: `mtime` reads the monotonic clock `--profile`
  wanted and ships a js_of_ocaml implementation (#609, #788)

### CLI tools

- `cascade prune PAGE.html... STYLE.css` removes the rules a set of HTML
  documents cannot use, `--dry-run` reporting instead of writing. It and
  `cascade apply` leave alone the two selector forms Selectors 4 defines and no
  engine implements (#605, #863)

- `cascade fmt --import-root DIR` bounds `--inline-imports` filesystem reads to
  the canonical root and its descendants (#744)

- `cascade diff` reads either side from standard input when the argument is `-`,
  writes the comparison as one JSON document with `--json`, and bounds a report
  by whole differences with `--limit` (#792, #796, #799)

- A `cascade diff` report counts one difference for each thing that really
  differs, prints rule differences in the order the expected side names them,
  states a selector's move once, names a rule's nested block after the rule it
  belongs to, shows the contents of a block added or removed wholesale, and
  reports a rule that changed places next to whatever else the two sheets
  differ on (#345, #385, #389, #474, #580, #581, #783, #784, #794, #798, #814,
  #815)

- `cascade diff` shows the declarations a `@keyframes` frame gained, lost or
  changed, and counts a rule as changed only when its own declarations changed,
  so a rule holding an edited nested rule is no longer summarised as a
  difference the report cannot show (#906, #907)

- A `cascade diff` character-level hunk escapes a byte with no glyph, a parse
  warning both inputs raise prints once under a label naming both files, and
  the tool scales near-linearly on a sheet where every rule changed (#785,
  #786, #790, #791, #795)

- `cascade apply` reads a `style` attribute in source order, empties the
  `<style>` blocks it projects rather than removing them, keeps the comments a
  page holds, and keeps a declaration whose longhand a kept rule writes under
  another property name. `--minimal` drops an inherited declaration only when it
  truly restates what the element would inherit (#326, #329, #332, #339, #340,
  #346)

- Exit statuses say what happened: `cascade fmt` exits 1 when parse recovery
  left no statement at all, and `cascade apply` exits 0 when a `<style>` block
  parses to at least one statement (#489, #494)

- CLI help lists each option and exit status once, says what `--enforce-spec`
  gates, and classifies representative selectors for `cascade prune`.
  `--enforce-spec` can drop a rule with a raw non-ASCII selector without
  `--minify`, and `--profile` without `--minify` no longer prints an empty
  factoring report (#611, #625, #628, #740)


## 1.1.0

### Breaking

- `Css.hex` raises `Invalid_argument` on a malformed hex string instead of
  returning opaque black; `Css.hex_opt` returns an option. Parsing is
  unaffected (#232)
- `cascade fmt` and `cascade diff` drop `--memtrace`, and the library drops the
  `memtrace` dependency (#237)
- A parse failure that drops every rule makes `cascade fmt` exit 1 instead of
  writing an empty stylesheet with a green status (#273)
- `cascade diff --diff=canonical` exits 1 whenever the two canonical forms
  differ, and prints the difference. It called such a pair equivalent and
  exited 0 when the structural walk reached no difference, which left a missing
  normalisation key and a blind spot in the walk both reading as success.
  ``Css_compare.equal ~mode:`Canonical`` answers on the same bytes, and
  `Css_compare.No_diff` no longer carries the two canonical forms (#290)
- `Tree_diff.t` gains `layer_order`; record expressions must set it and record
  patterns must bind it or use `; _` (#295)
- Optimizer profiling is per run: `Css.optimize` and `Optimize.stylesheet` take
  `?stats:Stats.t` and `Stats.snapshot` reads back an immutable record.
  `Optimize.counters`, `Optimize.pass_times`, `Optimize.iteration_stats` and
  `Optimize.set_profile` are gone, along with the per-pass table and the
  marginal-stop counter, which no pass had written since the DAG scheduler
  replaced the multi-pass fixpoint (#288)
- `Resolve.NODE` gains `text_children`, the data of a node's direct child text
  nodes, which `:empty` needs (#286)
- `Apply.Make(...).compute` takes `~sheet:Stylesheet.t` instead of
  `~css:string`. It parsed the text itself and answered an empty result when
  the parse failed, so a caller could not tell invalid CSS from empty CSS; the
  parse, and the warnings `Css.of_string` collects with it, now stay with the
  caller
- `Apply.result.kept` counts the rules it says it counts. A block at-rule
  counted once for its wrapper, so a `@media` holding three rules reported one (#287)
- `cascade apply` exits 1 when a `<style>` block or the supplementary
  stylesheet parses to nothing, and leaves such a block in the page instead of
  deleting it. It used to delete the block and exit 0, shipping an unstyled
  page under a green status. A supplementary stylesheet that cannot be read is
  an error rather than no stylesheet at all (#287)

### Parsing

- `Reader.peek_utf8_at` returns `None` for negative and out-of-range offsets
  instead of indexing outside the input (#308)
- URL references use RFC 3986 resolution, preserving data, scheme-relative,
  query-only and fragment-only URLs (#304)
- Container conditions parse component values directly, so strings and escaped
  or case-insensitive boolean keywords keep their CSS meaning (#305)
- Media queries parse component values directly, so escaped identifiers and
  balanced general-enclosed values keep their CSS meaning (#306)
- Typed `-webkit-` and `-moz-` aliases use their standard value grammar.
- `shape-outside` reads its whole grammar. Only `none`, `circle()`, a non-empty
  `inset()` and the CSS-wide keywords were accepted, so `margin-box`,
  `circle(50%) content-box`, `url(shape.png)` and every other basic shape were
  rejected and the declaration dropped
- An identifier takes any code point at or above U+0080, so a selector such as
  `.text-↗` parses. `Css.of_string` takes `?enforce_spec` and `cascade` takes
  `--enforce-spec` to restrict identifiers to the CSS Syntax 3 range list;
  output is the same either way (#254)
- Parsed rather than dropped with a warning: `perspective: none`,
  `text-underline-offset: auto` and a negative `text-underline-offset` (#212);
  a nested rule whose selector starts with an identifier, such as
  `h2:where(...)` (#193); and an unrecognised media query such as
  `theme(static)`, kept as never-matching instead of discarding the `@media`
  block or the `@import` (#192)
- The eleven `scroll-margin` properties take a negative length, as CSS Scroll
  Snap 1 allows; only `scroll-padding` is non-negative (#280)
- A `}` ends a declaration value in `Parser.block_contents` instead of being
  swallowed along with the rest of the block, and a bad string serialises back
  to source that reads as one instead of vanishing from an at-rule prelude (#284)

### Printing and nesting

- `background-position` and `mask-position` print one position per layer,
  comma-separated (#209)
- A non-integer above roughly 4.6e10 prints as a number again, not as
  `scale(1.2345678.9012e+19)` (#265)
- An unknown at-rule keeps the space before its prelude under `--minify`;
  `@foo bar {x:1}` printed `@foobar{x:1}`, a different at-rule (#272)
- Flattening distributes the parent over every branch of a nested selector
  list: `.p { a, b { ... } }` is `.p a, .p b` (#205)
- Substituting `&` wraps a complex parent in `:is()`:
  `.a .b { .dark & { ... } }` flattens to `.dark :is(.a .b)` (#194)

### Minification

- `--lossless` preserves logical sizing and corner aliases in source order.
- A hex colour folds to its name whenever the name is no longer, which the
  hand-copied inversion table missed for `bisque`, `indigo`, `orchid`,
  `salmon`, `sienna`, `tomato` and `violet`: `#ffe4c4` stayed hex where
  `#f0ffff` already became `azure` (#289)
- `Css.Values.read_color_name` reads every name `pp_color_name` prints, not 21
  of the 148, and `grey` reads as `Grey` rather than the `Gray` that prints
  `gray` (#289)
- `--lossless` keeps a longhand in source order against the shorthand that
  resets it, whether the property is typed or not; moving the pair changed what
  the rule rendered (#267, #270)
- `--lossless` keeps a colour's alpha exact, like its other channels;
  `oklch(... / .74567)` printed `/.746` (#278)
- `--lossless` keeps a flow-relative property in source order against a
  physical one of the same family, since the writing mode decides which
  physical side it resolves to (#277)
- A vendor-prefixed value repeated with `!important` collapses to the later
  declaration; only a genuine value difference is kept as a legacy fallback.
  `display:-webkit-box;display:-webkit-box!important` survived as both
- A selector list mixing a vendor pseudo-element with ordinary selectors is
  split, so a browser that ignores `::-webkit-search-cancel-button` keeps the
  other selectors' declarations (#203)
- A rule with nested children absorbs a later rule with the same selector when
  the two are disjoint (#203)
- SVG presentation properties are typed and minify like any other value:
  `stop-color:#fff`, `fill-opacity:.1`, `stroke-dashoffset:0`,
  `stroke-dasharray:4 2`, `paint-order: stroke fill markers` to `stroke`, a
  redundant `vector-effect` keyword dropped, and `fill-rule`, `clip-rule`,
  `stroke-linecap` and `stroke-linejoin` read as their own grammars, including
  `miter-clip` and `arcs` (#214, #228, #234, #235, #236, #240, #241, #242)
- More values fold to their exact equivalent: `grid-auto-flow: row dense` to
  `dense` (#230); a zero angle in radians to `0deg`, so `hue-rotate(0rad)`
  folds like the other units (#229); `hue-rotate()` with a zero argument inside
  a custom property (#257); a `font-stretch` keyword to its percentage, except
  in the `font` shorthand (#206); and adjacent gradient stops of one colour to
  a double-position stop, with a `0deg` linear-gradient angle dropped and the
  stops reversed, never for the legacy prefixed gradients (#214)
- `--log=cascade.factor:debug` reports the optimizer's factoring decisions:
  each fixpoint iteration and every segment reverted or skipped (#239)
- `--minify` is faster on a large stylesheet, for the same output (#221)

### Custom properties and `@layer`

- `Css.inline_vars` resolves `var()` across `@layer` boundaries and folds a
  custom property redefined across layers to its cascade winner, so a layered
  stylesheet inlines like its unlayered form (#187, #189)
- `cascade apply` projects rules inside `@layer` onto elements; a fully layered
  stylesheet, such as Tailwind v4 output, inlined nothing (#188)
- `cascade apply` weighs cascade layers instead of ignoring them: an unlayered
  declaration beats a layered one, `!important` reverses that, and a rule with
  no inline form stays inside its layer (#283)
- `cascade apply` leaves a declaration in the stylesheet when a rule inside
  `@scope`, `@starting-style`, `@when`, `@else` or `@-moz-document` sets the
  same property; it moved inline, above the rule that was kept (#286)
- `cascade apply` only inlines a rule whose selector its matcher can represent,
  so `[data-k="X" i]`, a namespaced selector and the `>>>` and `||` combinators
  stay in the stylesheet rather than being inlined onto nobody and dropped, and
  `:empty` counts an element's text: `<p>text</p>` is not empty (#286)
- `Css.vars_of_declarations` reports the `var()` references of 39 properties it
  answered with none, so `Css.resolve_theme` emits the theme binding for
  `inline-size: var(--w)` as it does for `width: var(--w)` (#266)

### Canonical diff

- The projection expands selector-list rules onto their branches (#204), folds
  two conditional blocks sharing a condition (#211), keys `@media not all and
  (X)` as the `@media not (X)` it equals while still emitting what it read
  (#231), and keys a run of `@property` rules by name, keeping the last
  registration (#227)
- It skips the rule-regrouping passes, which depend on input order;
  `Css.optimize` takes `?regroup` to turn them off (#215, #224)
- It normalises the space after a top-level comma in a custom-property value,
  drops a declaration a later rule with the identical selector also writes
  (leaving `!important` alone), and pairs exactly matching rules before falling
  back to the property signature (#206)
- A custom property keeps its `!important`, its layer and its metadata through
  the projection, so the differ no longer calls two sheets that disagree about
  the flag identical (#271)
- It rewrites a quoted multi-word font name in a custom property as the
  `<ident>` sequence it unquotes to, the same family name under CSS Fonts 4
  sec. 2.1.1: `--font-sans: ui-sans-serif, "Noto Color Emoji"` and
  `--font-sans: ui-sans-serif, Noto Color Emoji` reach one form. The structural
  comparator already folded the two together; the projection did not (#290)
- Under `--lossless` it keys a `color(srgb ...)` whose channels land on whole
  bytes as the `rgb()` spelling of the same colour, so `color(srgb 1 0 0)` and
  `rgb(255 0 0)` stop reading as a difference. Exact conversions only:
  `color(display-p3 1 0 0)` and an off-grid channel stay distinct, and the
  printer still emits the function that was written (#289)

### Diff report

- Grouping repeated selectors and conditions is linear in the number of rules
  while preserving source order (#307)
- The cascade layer order the two sheets declare is compared, and the report
  names the layer pairs that swapped. Dropping an `@layer a;` pin, which makes
  the other layer the weaker one, read as no difference at all
- Every entry the report prints names what it is about. A `@property` or
  `@keyframes` surplus reached the rule level, which has no rule to name and
  printed a bare tree connector while still counting the entry towards the
  summary; `@charset`, `@namespace` and `@layer a, b;` had no name at all
- A container that changed places is reported whether or not its body changed,
  on the same order keys the rule level uses. Three absolute-index distances
  gated it before, so swapping an `@media` with the rule below it - which
  changes which declaration wins above the breakpoint - printed `CSS files are
  identical` and exited 0
- A rule that writes one property more than once, as a fallback chain does, is
  compared occurrence by occurrence; matching by name alone made
  `a{color:red;color:blue}` against `a{color:red;color:green}` report
  `color: blue -> red`, a value neither side holds (#285)
- Containers are compared however deep they nest. The walk stopped at three
  levels, so a leaf difference under five at-rules was reported as no
  difference at all, exit code included (#285)
- A container entry with nothing to show under it reads as `(modified, no
  details)`; it claimed a position change, which only a `Reordered` entry
  establishes (#285)
- A comparison that classified nothing says so: `Changes: none classified
  structurally (see report below)`. It read `No structural differences`, which
  claimed equivalence over a comparison that fell through to a string diff, and
  over a side whose content the parser discarded (#285)
- A canonical-form difference the structural walk did not reach is printed as
  `Canonical forms differ:` above a string diff of the two forms (#290)
- A string diff names the two sides with the labels it was given, so `cascade
  diff` heads it with the two file names instead of `Expected` and `Actual` (#290)
- A declaration reorder that decides the cascade is reported inside `@media`,
  `@layer` and `@supports` (#268), and the at-rules that carry no selector -
  `@page`, `@starting-style`, `@counter-style`, `@scope`, a second
  `@font-face` - are compared on their bodies (#269)
- An `@property` is compared on its whole body and the entry names the
  descriptors that differ (#264); a rule is reported as reordered only when it
  moved against another rule (#263); and the size summary lists the two files
  in the order of the `---` and `+++` headers, so an addition no longer reads
  as a shrink (#261)
- A selector written by more than one rule is reported once, at the top level
  and inside a container; when its declarations survive on both sides, spread
  differently, the entry names the move and counts as a rearranged rule
  (#259, #260)
- Blocks sharing a condition are reconciled one for one, so three `@container`
  blocks against two report the removed block rather than two changed
  containers; the same pairing decides media, layer and supports (#258)
- Every statement of an added or removed container is reported, including a
  nested `@media`, and a container is no longer counted again as a rule
  difference with an empty selector (#253)
- `cascade diff` bounds its report to the deepest level that fits, with
  `--depth` to pin a level or print the tree in full; parse warnings print
  above the report, capped per side; and blocks that only moved are reported as
  a shift run (#210)

### New properties and values

- Complete the logical border properties: `Css.border_block_color` and its
  start/end siblings, the `Css.border_inline_width` and
  `Css.border_block_width` shorthands (type `logical_border_width`), and the
  start/end style longhands such as `Css.border_inline_start_style`
  (#197, #198, #199, #200)
- Add `Css.parse_font_family`, `Css.parse_list_style_type` and
  `Css.parse_list_style_image`, the single-value readers behind the `font` and
  `list-style` shorthands (#201, #202)
- Add `Css.Values.oklch_none_hue` for an achromatic colour with a missing hue,
  printed as `oklch(55.6% 0 none)` (#190)

### Testing

- CI rejects merlint findings and incomplete library record patterns (#310)
- `dune test` renders a stylesheet and its optimised forms in a headless
  browser and compares the computed style of every element, on a document
  derived from the stylesheet's own selectors; it skips where no browser is
  installed (#275)

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
  - Selectors Level 4: including `:has()`, `:is()`, `:where()`, `:not()`,
    nesting `&`, and full attribute syntax.
  - Values & Units Level 4: `calc()`, `clamp()`, `min()`, `max()`,
    `minmax()`, the modern length units, durations, angles.
  - Color Level 4: 15 colour spaces including `oklch()`, `oklab()`,
    `lch()`, `hwb()`, `color-mix()`, plus the 148 named colours.
  - Conditional Rules Level 3-5: `@media`, `@supports`, `@container`
    (including typed `style()`/`scroll-state()` queries with range
    operators), `@when` / `@else`.
  - Cascade Level 5: `@layer` declarations and blocks, CSS-wide
    keywords, and `all` reset semantics in the optimizer.
  - Custom Properties Level 1: `var()` parsing/printing, typed
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

- CLI commands share one binary file reader that closes its descriptor when a
  read fails (#309)
- `cascade`: pretty-print and minify CSS files. It accepts stdin via `-`
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
  first, so equivalent factorings (different rule grouping, cascade-safe rule
  and declaration order) compare identical rather than as spurious changes.
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
