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

- `cascade diff` exits 2, not 0, when it finds no difference and had to drop a
  declaration or a rule it could not read: what it dropped reached neither side
  of the comparison, so identity is not a verdict it can give. A gate that
  reads any non-zero status as "differs" needs updating (#832, #833, #834)
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

- A `style()` container query takes the single-comparison range CSS Conditional
  Rules 5 defines, `style(--gap = 10px)` included, and rejects an interval whose
  two bounds point different ways; `Css.inline_vars` reads its operands (#805)
- A parse error marks the text that failed, the caret at the line and column of
  the value the author wrote rather than under the token after it (#472, #477,
  #789, #793, #801)
- Lenient parsing preserves a value opaquely when the property is unknown, the
  value carries a runtime substitution, or it is a colour fallback; a value the
  typed reader rejects is an invalid declaration and is dropped (#787, #813)
- Grid track sizes accept math functions that resolve to `<flex>`, including
  `calc(1fr * 2)`, `min(1fr, 2fr)` and `clamp(100px, 1fr, 300px)`, in explicit,
  repeated and automatic tracks (#749)
- `tab-size`, `column-count`, the `columns` shorthand, a `repeat()` track count
  and a `span` grid line accept a `calc()` resolving to a number, which CSS
  Values 4 allows wherever an integer is (#827)
- A sole baseline position in `place-content` is accepted and defaults its
  omitted `justify-content` slot to `start`, as required by CSS Align (#739)
- Custom-property declarations containing a `<bad-string-token>` are dropped
  during stylesheet recovery, so minified output no longer swallows the
  containing rule's closing brace when it is parsed again (#727)
- Declarations containing a non-empty `var()`, `env()` or `attr()` call defer
  CSS-wide keyword mix validation until substitution, instead of being dropped
  while the substituted token stream is still unknown (#726)
- A `var()` written as one component of `border-radius`, `gap`, `place-*`,
  `grid-auto-flow`, `transform-origin`, `border-spacing` or a border-image
  dimension keeps its type, so the values beside it still fold and the variable
  is still found when cascade lists the ones a sheet uses (#729, #734, #735)
- Grid values a browser drops are dropped: `grid-template-columns: [a]` and
  `1px [a] [b] 2px`, `[span] 1px`, `grid-column-start: 0`, `span -1` and
  `grid-column: 2 / initial` (#708, #712, #714, #724)
- Each grid property takes its own grammar: `grid-auto-rows` rejects the area
  forms `grid-template` owns, `subgrid` takes its optional line-name list, and
  `grid-column-start: 3 span` prints `span 3` (#711, #717, #718)
- An `@property` syntax rejects a component with two multipliers, a multiplied
  `<transform-list>` and a `*` combined with anything, so the registration
  drops rather than typing the property as something else (#707, #713)
- A stylesheet that used to hang the parser is read: an `@property` syntax
  multiplying `<transform-function>` or `<resolution>`, and
  `@page { @top-center { .a { b: c } } }` (#398, #710)
- A value a browser rejects is dropped, where cascade used to keep the part of
  it that it recognised and ignore the rest: `translateX(10px red)`,
  `max(1px, red)`, `calc((1px 2px))` and `calc(100%- 10px)`
  (#617, #627, #629, #631, #699, #701)
- Keyword, at-rule and function names are matched without regard to case, so
  `grid-column: SPAN 2`, `@MEDIA`, `RGB()`, `VAR(--x)`, `:dir(LTR)` and a
  capitalised `COLOR-STOP()` read and optimise like their lower-case spellings.
  A name the author chose keeps its case, and `@charset` stays the byte
  sequence CSS Syntax 3 sec. 8.2 matches (#602, #603, #604, #620, #622, #767)
- `:dir()` accepts any single identifier, so `:dir(auto)` is read and written
  back instead of invalidating its rule. A direction that cascade cannot model
  matches no element, so `:not(:dir(auto))` matches every element (#594)
- `@font-face` and `@font-palette-values` use their descriptor-specific
  `font-family` grammars: one named family for the former, a non-empty list for
  the latter, and neither takes a generic family or CSS-wide keyword (#695)
- `steps()` rejects fractional counts, and `jump-none` requires at least two
  steps (#688)
- `offset`, `offset-anchor` and `offset-position` are typed: they validate the
  Motion Path grammar and canonicalize their `<position>` branches, so
  `offset: total nonsense here` and a `normal` coordinate drop (#674, #683)
- The three- and four-value `<position>` readers reject two edges on one axis
  and any identifier that is not an edge, so generic `<position>` takes no
  three-value form and `transform-origin` no edge-offset form (#673, #680)
- `border-image` accepts a repeat keyword with no source or slice, the omitted
  slots taking their initial values, and rejects the mode keyword only
  `mask-border` has a slot for (#667, #682)
- `white-space: collapse` parses, as the new public `white_space.Collapse`
  node. The shorthand's other longhands are optional, so a
  white-space-collapse value stands on its own (#666)
- `text-decoration` accepts a colour, style or thickness with no line value.
  None of its four components is mandatory (#665)
- A CSS-wide keyword mixed into a `font-family` list reads as the exported
  `font_family.Invalid` node instead of raising during property parsing (#657)
- `repeating-linear-gradient(var(...))` keeps its repeating function name
  instead of serializing back as a non-repeating linear gradient (#656)
- A literal value reads as the node the library exports, so constructed and
  parsed declarations compare and hash equally: `caret: auto`, `aspect-ratio`,
  a `var()` gradient body and a typed percentage (#651, #652, #653, #654)
- The animation and transition delay longhands read time-valued `round()`,
  `mod()` and `rem()` calls instead of dropping the declaration (#646)
- A declaration whose grammar ends in an optional component keeps its value:
  `rotate: 45deg !important` and `text-box: none;` read the `;` and the
  `!important` as part of the value and went missing with only a warning (#644)
- An empty value is no longer a declaration: `border:`, its per-side and
  logical variants, `column-rule:` and `outline:` are dropped with a warning,
  where `border:` read as `border: none` (#640)
- `display: flow-root list-item` and `display: flow list-item` are read, since
  CSS Display 3 sec. 2 puts no order on the three components, while
  `display: list-item table` and a repeated `list-item` are rejected (#641)
- `outline-width` and the width slot of the `outline` shorthand read a
  `<length>` where CSS UI 4 sec. 3.2 gives them a `<line-width>`, so
  `outline: thin solid red` was dropped as invalid (#633)
- `stroke-width` accepts a bare number, which cascade printed but its own
  reader then rejected, and `stroke-miterlimit` accepts a value between 0 and 1
  (#334, #579)
- A selector using the column combinator, such as `svg||td`, is read and
  written back instead of dropped. The namespace reader consumed the first bar
  of the `||` that CSS Selectors 4 sec. 15.2 defines (#572)
- A selector no browser can ever match is dropped instead of being written
  back: a combinator after a pseudo-element, a rule nested under a
  pseudo-element parent, `:not(::before)` and `::before:hover` (#426, #430,
  #552, #559)
- A pseudo-element that carries structure keeps the compounds it allows, so
  `::file-selector-button:hover`, `::part(p):hover`, a chained
  `::part(label)::before` and WebKit's scrollbar state pseudo-classes read and
  print back (#418, #441, #553, #556)
- `position-area` takes the logical `start`/`end` keywords with their `self-`
  and `span-` forms, and rejects a pair from different branches of its grammar
  such as `left block-start`, which Chrome 151 rejects too (#478, #485, #495)
- `@media (min-width: 0)` and four of `@-moz-document`'s five URL-matching
  functions are read, where a condition that cascade could not parse took the
  at-rule and every rule inside it down with it (#427, #461)
- The `border-inline` and `border-block` properties keep their value. None of
  them had a value reader, so a file containing nothing else exited 1 (#456)
- A descending `@font-face` range such as `font-weight: 700 400` parses without
  a warning, only `unicode-range` keeping an ordering rule (#335)
- A valid `@font-palette-values` rule no longer warns, so `cascade apply`
  accepts it: a missing `base-palette` defaults to 0, and `font-family` is the
  mandatory descriptor whose absence warns in its place (#551)
- `clip-path`, `shape-outside` and `object-view-box` stop taking an
  intrinsic-sizing or CSS-wide keyword as the corner radius of a basic shape,
  which browsers drop anyway (#417)
- A `var()` in a `page-break-*` declaration is read as the property it names,
  and an error in one is reported against the property the author wrote rather
  than the `break-*` property it minifies to (#511, #518)
- `@supports (--x\3b y: red)` is read instead of the whole rule being dropped.
  The reader rejected an escaped property name because reading its text back
  did not give the same name (#437)
- A `@layer` name is read as the identifiers it is made of, so the layer named
  `a.b` written `@layer a\2e b` is no longer the sublayer `b` of `a`; both
  printed `@layer a.b`, and minification merged the two blocks (#442)
- An at-rule nested inside a style rule reads its body as nested rules, where
  `.a { @starting-style { color: blue } }` dropped the declaration and
  `.a { @layer n { color: red } }` read its body as a selector list (#374, #384)
- An at-rule whose body holds no style rule is dropped inside a style rule, and
  the discard ends there, where `.a { @import url(x) { } color: red }` lost
  `color: red` along with the `@import` (#388)
- A declaration written after a nested rule keeps its place, so
  `.a { @supports (color: red) { color: blue } color: green }` computes green
  as browsers do, where cascade printed a sheet that computed blue (#380)
- A bad descriptor, a stray `;`, a nested rule or a margin at-rule inside an
  at-rule body is dropped on its own rather than taking the block and the
  stylesheet holding it (#392, #398, #399, #402, #419, #420)
- Tokens left after a descriptor's value, such as a trailing `!important`,
  invalidate the declaration they follow rather than the leftover alone
  (#399, #419)
- `@page` keeps what it is given: any property in its body or a margin box,
  where a seven-name allowlist rejected them, and an empty margin box is elided
  rather than taking the `@page` with it (#403, #405)
- A duplicate descriptor in a `@page` body or a page margin box keeps the
  important declaration rather than the one written last (#404)
- An unrecognised at-rule reaches the output with its block intact, where
  `cascade fmt` silently deleted it and every rule inside it.
  `Optimize.drop_unknown_at_rules` drops such at-rules on request (#469, #483)
- An unknown at-rule whose raw body ends on a backslash is written with a
  closer that closes it, where the backslash escaped the `}` the printer wrote
  and the next statement was swallowed into the body (#558)
- A qualified rule whose prelude reads as a custom property, such as
  `--x:hover { color: red }`, is reported when it is dropped, and `~strict:true`
  rejects it; the drop itself was silent (#473)
- Everything the parser repaired or dropped is reported, so strict mode rejects
  it. `@media screen {` swallowed the rest of the file and still returned `Ok`
  with no warnings, hiding a truncated stylesheet (#484)
- An error inside an at-rule condition or an `@font-face` descriptor points at
  the slice that failed, not at the end of the file with the caret past the
  last byte (#496, #497, #499, #501, #538)
- `Cascade.Reader.int` raises `Parse_error` on a number with a fractional part
  or one outside the `int` range, where it truncated `3.9` to `3` and answered
  `-1` for `1e30` (#466)

### Printing

- `text-decoration`, `mask-border` and `animation` no longer minify to an empty
  value. `text-decoration:solid` printed `text-decoration:`, which no parser
  reads back, once dropping the initial style left no slot (#682)
- A `calc()` printed without `--minify` keeps the parentheses the author wrote,
  redundant ones included, while `--minify` removes those and keeps a
  precedence-sensitive `calc((1px - var(--a)) * 3)` (#721)
- An identifier is printed with the escapes needed to read it back as the same
  name, where `@layer a\3b b` printed two statements naming a layer the input
  never had and `--x\3b y` split into two declarations (#435, #436, #526, #598)
- A compound operand of `not`, `and` or `or` in a `@media` condition keeps its
  parentheses, where `not ((min-width:1px) or (max-width:2px))` printed CSS
  browsers and cascade's own reader reject, losing the whole block (#319)
- Unwrapping a `@supports` nested in a style rule keeps the `;` before the
  sibling that follows, where the declaration it held ran into the next one as
  `color:#00fcolor:green` (#370)
- A NaN-valued number prints as `calc(NaN)` and a NaN-valued dimension as
  `calc(NaN * 1unit)`, where `calc(sqrt(-1) * 1px)` printed `NaNpx`, which
  browsers drop (#425)
- `Css.to_string` renders the sheet once. It used to render it twice, the first
  pass only to measure the output and size the buffer exactly (#479)

### Minification

- `cascade diff --diff=canonical` reports a rule as moved only when it really
  moved. Writing one selector as a nested branch or as its flat expansion, and
  writing two rules of one selector next to each other or as a single rule, now
  compare equal, so a sheet and its minified form agree. A declaration hoisted
  into a selector-list group also compares equal to the same declaration
  written inline in every branch of that group (#825, #826, #831)
- Nesting emits only selectors a browser can match. A rule nested under a
  pseudo-element parent is dropped wherever it sits, and so is a branch that
  adds a pseudo-class the pseudo-element does not accept (#818, #822, #823)
- `--minify` merges a `@container` block into an earlier one carrying the same
  name and query across the rules written between them, where only adjacent
  blocks merged and a hoist over a conflicting rule stays refused (#809)
- `--minify` keeps the time unit the author wrote for a `var()` fallback inside
  a `calc()`, as it already did for the operand beside it. Rewriting the unit
  left two declarations printing the same text while differing internally, so
  they did not factor into one rule (#803)
- `--minify` keeps a vendor-prefixed gradient written with the `background`,
  `mask`, `border-image` or `mask-border` shorthand, so deduplication no longer
  deletes the fallback a browser without the standard function needs (#782)
- Invalid math results in `shape-margin` and `offset-distance` are dropped like
  those in every other `<length-percentage>` property (#766)
- Flattening a rule nested under a selector list wraps the parent in `:is()`,
  where `.a,.b{&:hover{color:red}}` printed `.a,.b:hover` and bound `:hover` to
  the last branch, CSS Nesting 1 sec. 4 (ED) reading `&` as the group (#800)
- `--flatten-nesting` leaves the optimised stylesheet flat even when later
  regrouping can shorten adjacent selectors by synthesizing nesting (#759)
- Canonical diff flattens authored nesting before comparing, so a nested
  stylesheet and its flat equivalent are reported as identical (#760)
- Computed-value evaluation resolves direct `inherit`, `initial`, `unset`,
  `revert` and `revert-layer` values for every typed property, including value
  shapes without a property-specific optimizer (#764)
- Whether a property inherits is decided in one place, from the typed property,
  so `cascade apply --minimal` drops a restated `writing-mode` and `unset`
  resolution uses that same answer (#763)
- Default `--minify` evaluates all-static unitless `line-height: calc()` to its
  six-significant-figure budget, so `calc(28/18)` becomes `1.55556`;
  `--lossless` keeps a repeating quotient symbolic (#756)
- `--minify` converts modern colour operands to floating-point sRGB before
  resolving an `in srgb` `color-mix()`, so channel bytes are rounded once after
  interpolation instead of once per operand and again afterward (#755)
- `--minify` reaches a stable result in one invocation on large stylesheets,
  where synthesized shorthands, vendor transition aliases, factoring and
  selector-branch order each needed another pass to settle (#750)
- Default minification adds the WebKit fallbacks Safari 16.4 and Chrome 111
  need, with matching `@supports` tests, for `user-select`, `backdrop-filter`,
  `hyphens`, `mask` and `-webkit-text-decoration-color` (#751, #758, #797)
- Lossless optimisation keeps otherwise-independent declarations in authored
  order, so stylesheet text and CSSOM enumeration no longer change solely for
  gzip alignment (#742)
- Numeric tokens in a custom-property or unknown-property stream take their
  shortest exact spelling, so `--x: 1.0px` becomes `--x:1px`. A declaration
  feature query keeps the author's spelling (#719)
- `--minify` prints a custom property as the author's own token stream: no
  separator between tokens held side by side, and the space a math `+` or `-`
  needs for a browser to read the value at all (#697, #709)
- `--minify` keeps the space after a closing parenthesis in a typed position or
  colour value, so `background-position: var(--x) 20%` minifies to itself
  (#700, #703, #722)
- `--minify` treats `[]` and `{}` blocks as ordinary tokens, since only `()`
  groups a math expression, so a bracketed run inside a custom property has its
  separators minified like any other token stream (#720)
- `--minify` drops the space at a `%` or `)` boundary in the box shorthands and
  in `inset()`, so `margin:10% 0` prints as `margin:10%0`. A value ending in a
  unit keeps its space, `10px 0` re-tokenising as `10px0` (#614, #619, #623)
- `Css.to_string ~minify:true` keeps choosing the shorter exact spelling for
  constructed millisecond durations and degree hues without running the AST
  optimisation phase (#678)
- `calc()` keeps type-bearing zero terms with compatible dimensions and rejects
  incompatible result types at property boundaries, where
  `width:calc(1px + 0)` used to survive (#676, #731)
- `--minify` folds a value's spelling before two rules are compared, so a hex
  colour, a NaN, an unreduced `min()` and a same-unit `calc()` written two ways
  factor into one rule (#471, #482, #597, #624, #639)
- A shorthand component left at its longhand's initial, a repeated
  `font-family` entry and a box shorthand whose sides repeat fold before
  comparison (#635, #636, #637, #639, #640, #641)
- Logical minimum sizes, duration units, stepped functions, hue-angle units,
  constructed `transform-origin` positions and repeated `scroll-margin` sides
  canonicalise before declaration hashes are compared (#647, #650, #672)
- A whole-rule `:is(a, b)` whose arguments share one specificity splits into the
  list `a, b` before rules are compared, so it factors with a rule that wrote
  the list out; `:is(a)` and `:is(*)` lose the wrapper too (#655)
- A box shorthand whose value holds a top-level substitution function such as
  `var()` keeps the number of components the author wrote, so the value stays
  valid and each side keeps the value meant for it (#736)
- `--minify` collapses `margin-inline` and `margin-block` to one value when the
  two edges match, which the four-sided box shorthands already did (#641)
- `--minify` folds `steps(1)` to `step-end`, CSS Easing 1 sec. 2.3 assuming
  `end` when the step position is left out (#641)
- `--minify` keeps `display: block ruby`. It printed `ruby`, which CSS Display
  3 reads as `inline ruby`, so a block-level ruby container came back
  inline-level (#637)
- `--minify` keeps the zero duration a `transition` delay stands behind.
  `transition: opacity 0s 2s` printed `transition:opacity 2s`, so the delayed
  instant change came back as a two-second one starting straight away (#641)
- `--minify` keeps a lone `background` position value and both `<box>` values of
  a layer that disagree, where `background: url(a.png) 0` came back
  top-aligned and a `content-box border-box` pair lost its origin (#640)
- `--minify` folds the width slot of the `border` shorthands, `column-rule` and
  `outline`, so `border: 0px solid red` prints as `border:0 solid red` and
  `border: calc(1px + 1px) solid red` as `border:2px solid red` (#633, #634)
- `--minify` folds `sin()` through `atan2()` inside a custom-property stream,
  matching `calc()`'s family (#626)
- `--minify` folds a `color-mix()` in a wide-gamut rectangular space, and one
  in `srgb` whose result leaves that gamut; both used to reach the browser
  unfolded. The result keeps the space that was named (#618)
- `--minify` keeps `.c:not(:enabled)` rather than rewriting it to
  `.c:disabled`, since `<p class=c>` matched before the rewrite and not after;
  the rewrite needs a compound that proves the subject carries the state (#596)
- `--minify --enforce-spec` keeps the author's `:not(:dir(ltr))` instead of
  shortening it to `:dir(rtl)`: only a host document like HTML makes `ltr` and
  `rtl` a partition, and that is one of the facts `--enforce-spec` drops (#593)
- `--minify` merges a run of adjacent `@starting-style` blocks, which take no
  prelude, so the run holds the same starting styles as one block over their
  concatenation (#592)
- `--minify` simplifies a nested `@supports` condition against the ones
  enclosing it: under `@supports (A)`, an inner `(A)` loses its guard, an inner
  `(A) and (B)` narrows to `(B)`, and an inner `(not (A))` is dead code (#585)
- `Css.optimize` writes an unrecognised at-rule's opaque body back as the token
  stream it read, so `@foo{ .a { color: red } }` minifies to
  `@foo{.a{color:red}}`. `~lossless:true` leaves the body as authored (#560)
- `--minify` merges `@media` and `@container` blocks by query structure, not
  serialised text, so `(min-width: 10px)` and `(width >= 10px)` are one bound;
  `Css.Container.equal` and `.normalize` expose the comparison (#516, #519)
- `--minify` merges adjacent `@container` blocks whose `style()` conditions are
  written the same way, where two byte-identical `style(--x: 1)` queries never
  compared equal and their blocks stayed separate (#465)
- `--minify` shortens an `src:` declaration outside `@font-face` the way it
  already shortened the descriptor, where the declaration route spelled a
  multi-word `format("...")` unquoted, which no browser reads back (#470)
- `--minify` keeps the `center` in `position-area: top center`. A lone keyword
  stands for `X span-all`, not `X center`, so dropping it moved the box to a
  different area (#457)
- `--minify` keeps a `@layer` whose own rules write no declarations but nest
  rules that do, where `@layer a { .x { .y { color: red } } }` collapsed to
  `@layer a;` and every declaration below the brace was deleted (#389)
- `--minify` optimises the body of `@-moz-document`, `@starting-style`, `@when`
  and `@else`, which it walked past (#343)
- An empty `@-moz-document`, `@when` or `@else` is dropped, as an empty
  `@media` already was. An empty `@when` or `@else` stays while a later `@else`
  binds to it, since dropping it leaves a bare `@else` no parser accepts (#396)
- `.a { @layer n {} }` keeps its block form: the statement form is a
  layer-order declaration, which no style rule accepts, so `.a{@layer n;}` was
  CSS neither a browser nor cascade's own reader takes back (#374)
- A drop that already applied in a style rule applies inside a `@keyframes`
  frame: a spec-invalid value, and under `--scope=stylesheet` a
  `position-try-fallbacks` name with no `@position-try` rule (#341, #372)
- A custom property registered by `@property` is typed wherever it is declared,
  so the same value minifies the same way inside `@keyframes`, `@position-try`
  and a `@supports` condition (#337, #349)
- `--flatten-nesting` treats `@-moz-document` as the grouping at-rule it is:
  nesting inside one flattens, and a rule wrapping one keeps its selector
  instead of the at-rule being emitted at top level under no parent (#344)
- A rule whose declarations a later rule all rewrites is dropped only when it
  nests nothing, where a rule holding both `all: unset` and a nested `@media`
  lost its nested block along with the rule (#376)
- A declaration written after a nested rule rejoins the ones before it when
  nothing it crosses writes the same property at the same importance; one that
  clashes keeps its place (#383, #386)
- Merging same-selector or same-condition rules reads source order and what
  each side writes, so neither a nested conditional group nor a declaration
  after a nested rule is merged across (#352, #364, #376, #414, #415)
- A declaration keeps its place against one whose shorthand or alias writes the
  same longhand, where `column-rule-color`, the flow-relative border styles,
  `word-wrap` and `-webkit-transform` merged across it (#447, #452, #453, #454)
- A value keeps every digit the author wrote, and a computed dimension past a
  million units keeps its own, where `.4285714em` came out `.428571em` and
  `calc(1in + 999999999px)` came out 95px short (#350, #354, #367)
- A math function inside `calc()` keeps its unit, where `calc(hypot(1px, 1px))`
  came out as `1.41421356`, a declaration browsers and cascade's own reader
  both drop (#362)
- A `font-family` name keeps the author's spelling and the quotes it needs,
  where `open-sans` became `"Open Sans"` from a table of known names and
  `"2Brand"` came out bare for a browser to drop (#387, #390, #401)
- `column-rule-color` and `-webkit-text-stroke-color` are typed as colours and
  `-webkit-text-fill-color` minified as one, so a colour-valued property
  minifies to the same spelling whatever its name (#447)
- A single-argument `:is()` and a double `:not()` keep their wrapper where
  removing it breaks the selector: `.a:is(code)` printed `.acode`, and
  `.a::before:is(.b)` printed the `.a:before.b` browsers drop (#377, #431)
- A vendor prefix is dropped only when its unprefixed twin is Baseline widely
  available, where `-webkit-backdrop-filter` and `-webkit-user-select` were
  dropped against a twin no shipping Safari understands (#325)
- A feature query on a vendor-prefixed property keeps its guard, so
  `@supports (-webkit-hyphens: none)` is no longer read as true. The
  web-features dataset behind the Baseline facts covers unprefixed features
  only (#378)
- `@media not all and (X)` minifies to the Level 4 `@media not (X)`, `all`
  matching every device. `--enforce-spec` keeps both Level 3 spellings (#323)
- `--minify` keeps one declaration where a rule writes both a `page-break-*`
  property and its `break-*` twin, CSS Fragmentation 3 sec. 3.4 making the pair
  one property (#547)
- `min-inline-size: initial` and `min-block-size: initial` minify to `auto`,
  the initial value they share with `min-width` and `min-height`, rather than
  to a zero that drops a flex item's automatic minimum size (#675, #681)
- `--minify` and `cascade diff` are faster on a large stylesheet, for
  byte-identical output: the slowest corpus stylesheet drops from about 80s of
  CPU to under 2s (#413, #422, #424, #468, #493, #507, #542, #664)
- `--minify` no longer allocates quadratically on a long run of rules sharing
  one selector, body or declaration: a 4,000-statement sheet's distant-`@media`
  merge falls from 24.4M words to 0.4M (#480, #486, #487, #502, #505, #517,
  #519, #523, #543, #566)
- Nested group-rule merges optimise only the newly joined statement list rather
  than walking already-optimised child blocks again at every ancestor, so
  doubling a repeated `@media` merge's depth doubles the work (#746)

### Custom properties

- `--minify` keeps the quotes on a `<string>` written to a custom property whose
  `@property` syntax accepts only an ident sequence, where unquoted it computed
  the name rather than the initial value (#704)
- `--minify` keeps the space between the repetitions of a `@property`
  `<type>+` initial value, CSS Values 4 sec. 2.3 making it the separator:
  dropped, `10px 20px` read back as one `px20px` dimension (#626)
- `Css.Variables.read_reference_body` reads a `var()` argument list into a typed
  variable handle, and `read_reference_body_as_string` returns the name and
  fallback as text for a caller with no value type (#630, #642)
- `Css.Variables.typed_custom_property` writes a custom-property declaration
  from a value already typed by a `@property` registration's syntax, where
  `Declaration.custom_property` takes a plain string (#626)
- A `var()` in an `@font-face` descriptor is resolved by `Css.inline_vars`
  rather than dropped at parse time, for every descriptor and for a single
  endpoint of a `font-weight`, `font-style` or `font-stretch` range. The parse
  keeps the reference without a warning and `~strict:true` accepts it, where
  `var()` substitutes in property values only and a browser drops the whole
  declaration (#322, #571, #573, #575, #577)
- `Css.inline_vars` resolves every `@page` descriptor under one unit policy,
  where `margin-top` alone kept the authored unit and one block answered `1cm`
  for it and `37.79527559px` for the `margin-left` beside it (#555)
- A custom property keeps its cascade layer and caller metadata through
  `Css.inline_vars`, where rewriting a custom value rebuilt the declaration
  from the value alone and lost both (#520)
- A `page-break-*` declaration survives `Css.inline_vars` as itself, where
  substituting a `var()` brought `page-break-inside` back as `break-inside`
  with a value that property does not accept (#506)
- `Css.inline_vars` keeps a `var()` reference marked as resolved at runtime,
  including one whose typed fallback is simplified through a scalar value or a
  shorthand. Such a reference is a point the browser can override, and it was
  being replaced with a compile-time default (#315)
- `Css.inline_vars` resolves a custom property defined across cascade layers
  against the order every `@layer` in the sheet gives it, counting the ones a
  rule nests and the ones a conditional group holds (#357)
- `Css.inline_vars` unwraps an `@layer` only where layer order and document
  order already pick the same winner for every property two layers write, where
  unwrapping handed the decision back to specificity (#371)
- `Css.inline_vars` unwraps an `@layer` and drops an `@property` registration
  written inside a rule, as it already did at top level, so a sheet using CSS
  nesting no longer comes back half cleaned (#373)
- `Css.inline_vars` keeps the `@property` registration of a custom property it
  leaves live, where every registration was dropped and a property it could not
  inline lost its `initial-value` and its `inherits: false` (#416)
- `Css.inline_vars` counts a `var()` in a `@keyframes` frame, `@page` and its
  margin boxes, `@position-try`, a `@supports` condition or a `style()` query
  as a reference, so pruning keeps the bindings they use (#341, #342, #423)
- `Css.inline_vars` reports an overridden variable through `~warn` (#341, #342,
  #423)
- `Css.inline_vars` stays linear in at-rule nesting depth and no longer costs a
  square in the variable count: a 12,800-variable sheet goes from 2.4s to
  0.11s (#481, #568, #569)
- `Css.resolve_theme` accounts for the declarations `@keyframes`, `@page`,
  `@position-try` and `@supports-condition` carry, so a name referenced only
  from inside one keeps its theme binding (#317, #324, #327)
- `Css.resolve_theme` builds each `theme_defaults` binding with
  `parse_custom_property` rather than assembling and reparsing `:root { ... }`
  text, where one bad value could drop every other theme default (#421)
- A custom-property name that needs escaping binds instead of being refused, so
  a `theme_defaults` answer for `x;y` emits `:root{--x\;y:red}` (#439)
- `Css.custom_props` reports a name declared inside `@scope`,
  `@starting-style`, `@-moz-document`, `@when`, `@else` or a bare nesting
  block, as it already did for `@media` and `@supports` (#375)

### Canonical diff

- A cascade-neutral reordering stays unreported when the two sheets also
  differ elsewhere, where one changed rule turned every such move in the
  sheet into a reported difference (#819)
- Canonical diff equates more rewrites that cannot change what a browser
  computes, such as equal `@supports` blocks hoisted together; a crossing that
  changes which declaration wins stays distinct (#775, #776, #777)
- Canonical numeric arithmetic has an explicit precision contract:
  `calc(28/14)` compares equal to `2`; default mode equates `calc(28/18)` with
  the minifier's `1.55556`, while `--lossless` keeps them distinct (#753, #756)
- `:is(a, b)` and the selector list `a, b` compare equal when the arguments
  share one specificity (#655)
- The canonical projection keeps structurally distinct `@container` conditions
  in separate cascade slots even when their minified text is identical, so an
  escaped unknown feature no longer merges into the real range (#529)
- Canonical diff keeps content that is dead only under an assumption about
  browser support, where deleting it made two sheets that render differently
  compare equal (#576)
- A redundant `@layer` order pin no longer reads as a difference, so
  `@layer a;@layer a{...}` and `@layer a{...}` compare equal. A pin that fixes
  the order, or one over a position the projection cannot read, is kept (#475)
- Canonical diff normalises the rules inside every at-rule that has a block, so
  the result no longer depends on which at-rule encloses a rule. It normalised
  inside `@layer` and `@media` but not inside `@scope` or `@when` (#393)
- A fully transparent `oklab()` with a missing axis, such as
  `oklab(0% none none / 0)`, compares equal to transparent black.
  Non-transparent forms stay distinct (#312)
- A relative-colour function keeps its origin as a typed colour, so `red` and
  `#f00` compare equal in `rgb()`, `oklab()` and the rest, including inside
  custom properties (#313)
- A complete shadow value in an unregistered custom property has its colour
  compared as a colour. Non-colour identifiers stay opaque (#314)
- A one-word family name in a custom property compares equal quoted and
  unquoted when a generic family in the stream proves the value is a font
  stack; without one the two spellings stay distinct diff keys (#696, #705)
- `Css_compare.equivalent_value ~property` spells the property name the way the
  printer does before comparing two values under it, where a name carrying an
  escaped `}` made any two values under it compare equal (#440)
- `cascade diff` no longer aborts when a reordered selector has the same rule
  index on both sides. An internal assertion fired and the whole report was
  lost; the move is now reported without a position (#582)
- `--diff=tree` compares a value on its minified spelling, so `16 / 9` matches
  `16/9` and `padding: 0.50px` matches `padding: .5px`. The space a math `+` or
  `-` needs, and the space beside a `var()`, still separate two values (#702)
- `--diff=tree` prints a changed declaration the way its own file spells it,
  where the value was read off the comparison key and a quoted multi-word
  family name was reported unquoted on both sides (#702)
- `--diff=tree` prints the body of an added or removed rule as declarations,
  with the separator and the `!important` flag, where a rule gaining
  `color: red !important` read like one gaining `color: red` (#706)
- `--diff=tree` pairs repeated occurrences of one selector by their declaration
  properties, source order breaking ties, so a compatibility block no longer
  makes declarations present on both sides read as added or removed (#752)
- Canonical diff reports only reorderings that are really present in the two
  inputs, so a changed declaration no longer makes unchanged rules look moved
  (#779, #780, #781)

### Library

- `cascade` drops its `uutf` dependency for the stdlib UTF-8 decoder, which
  counts one replacement character per maximal subpart of an ill-formed
  sequence, as a browser does (#788)
- The library no longer links `unix`: `mtime` reads the monotonic clock
  `--profile` wanted and ships a js_of_ocaml implementation, so embedding
  cascade in a browser links no operating system (#609)
- `Css.Resolve` answers `:nth-child(... of S)`, the typed `:nth-of-type()`
  family, `:has()`, the `i` and `s` attribute case flags and `:scope`, which
  read `Unsupported` and left `Css.Apply` keeping the rule (#607)
- `Resolve.prepare` and `Resolve.Make.resolve_prepared` split the sheet-only
  work out of `resolve`, so a caller walking a document buckets the rules once
  rather than per node, allocating 4.6x less over ten queries (#567)
- `Css.unknown_at_rule` builds an at-rule cascade has no grammar for, where
  emitting one meant assembling a sheet as text and reading it back. The
  constructor refuses one that ends the at-rule early (#600)
- `Css.Color_space.gamut_mapped_srgb_of_oklch` and `Css.Values.gamut_map_color`
  name the sRGB colour to write for an OKLCh colour sRGB cannot hold, per CSS
  Color 4 sec. 14.2. Minify still keeps the colour the author wrote (#591)
- `Css.equal_statement` and `Css.hash_statement` compare and key a statement
  without rendering it to CSS text, so two `@media` blocks selecting the same
  media are one statement however their queries are spelled (#595)
- `Css.Values.hash_color` keys a colour and `Css.Values.with_alpha` sets a
  colour's alpha (#595)
- The statement-merging passes are callable on their own:
  `Css.Optimize.merge_consecutive_layers`, `merge_named_layers_by_name`,
  `merge_consecutive_media`, `merge_distant_media`,
  `merge_consecutive_supports`, `merge_consecutive_containers` and
  `merge_consecutive_starting_style` (#592)
- `Css.Declaration.value_of` reads a declaration's value at a property witness,
  the counterpart of `Declaration.property_key`, where the value was reachable
  only as text; `Properties.eq_property` carries the type equality (#616)
- `Css.Properties.compare_property` and `Css.Declaration.compare_prop_key` are a
  total order on a property identity, `0` exactly where equality holds (#513)
- `Css.Properties.read_grid_template_tracks` parses the track-list grammar of
  `grid-template-columns` and `grid-template-rows` without accepting the wider
  shorthand forms handled by `read_grid_template` (#717)
- `Cascade.Syntax.is_ident` answers whether a whole string is a single CSS
  ident, the same check `Cascade.Parser.escape_ident` makes when printing. A
  per-character scan reads the leading `-` of `-4` as an ident start and gets
  the answer wrong (#626)
- `Css.Stylesheet` traverses a whole block with `fold_statements`,
  `iter_statements`, `edit_statements`, and `fold_declarations`,
  `iter_declarations` and `map_declarations`, which take `?sites` (#356, #363)
- `Css.Stylesheet.statement_declarations`, `statement_children`,
  `map_statement_children` and `map_statement_declarations` read and rebuild a
  statement's declarations and its children (#317, #337, #355)
- `Css.Stylesheet.at_declaration_site` answers whether a statement holds its
  declarations in one of the places a `declaration_sites` record names (#368)
- `Css.map` and `Css.sort` reach a rule inside `@scope`, `@starting-style`,
  `@-moz-document`, `@when` or `@else`, where a caller rewriting all rules at
  all nesting levels got five of them silently untouched (#381)
- `Css.layers` and `Css.layer_block` find a layer declared inside a grouping
  at-rule, so a caller asking which layers a sheet declares was given a wrong
  answer rather than a partial one (#382)
- `Css.Stylesheet.layers` is what `Css.layers` calls, so the two answer the same
  question alike. `Css.Stylesheet.media_queries`, `container_queries` and
  `Css.media_queries` report a query written inside a grouping at-rule (#389)
- `Css.flatten_nesting` carries the parent selector into an `@-moz-document`
  block, the one grouping at-rule it did not descend into, where the
  declarations came out bare at the top of the block (#384)
- `Cascade_diff.Tree_diff.has_container_added_of_type` and
  `has_container_removed_of_type` look inside a container reported as modified,
  as `count_containers_by_type` already did (#395)
- `Cascade.Resolve.Make.resolve` and `Cascade.Resolve.layer_order` document
  every block they leave out: `@starting-style`, `@scope` and an origin wrapper
  each carry something the resolver does not model (#394)
- `Cascade.Error.to_string` prints back a snippet that is valid UTF-8, where a
  byte-counted column could open the snippet inside a multibyte code point
  (#472)

### CLI tools

- `cascade diff` prints rule differences in the order the expected side names
  the rules, where a hash table's bucket layout decided it, so a report reads
  down the sheet and `--limit` keeps the ones nearest the top (#814, #815)
- `cascade diff --json` writes the comparison as one JSON document on standard
  output in place of the report, so a harness reads what changed rather than
  parsing prose. The exit status is unchanged (#799)
- `cascade diff` reads either side from standard input when the argument is
  `-`, so the output of a build can be compared without a temporary file. The
  report names that side `<stdin>` (#796)
- `cascade diff --limit` bounds a report by whole differences: `auto` keeps as
  many as stay readable, `none` prints every one, and an integer prints exactly
  that many, where a wide report named every selector and explained none (#792)
- `cascade diff` counts one difference for each thing that really differs, so
  two `@namespace` rules with different URLs count once and a selector list the
  two sides write in a different order counts as nothing at all (#783, #784,
  #794, #798)
- A `cascade diff` report on an empty file no longer prints `(0.0% diff)`
  (#783, #784, #794, #798)
- `cascade diff` prints a parse warning both inputs raise once, under a label
  naming both files, so the report's warning budget reaches the warnings only
  one side raised (#795)
- A `cascade diff` character-level hunk escapes a byte with no glyph, so a
  line-ending difference is visible and a control byte cannot drive the
  reader's terminal (#785, #790, #791)
- `cascade diff` scales near-linearly on a sheet where every rule changed
  (#786)
- `cascade fmt --import-root DIR` bounds `--inline-imports` filesystem reads to
  the canonical root and its descendants, rejecting both lexical and symlink
  escapes. Omitting it retains unrestricted resolution for trusted CSS (#744)
- CLI help lists each option and exit status once, and `cascade prune --help`
  classifies representative selectors through the resolver instead of naming
  `:nth-child()` as unsupported (#740)
- `cascade prune PAGE.html... STYLE.css` removes the rules a set of HTML
  documents cannot use, `--dry-run` reporting instead. A rule goes only when the
  matcher has a model for its selector and every element answers that it does
  not match, so a class a script adds at runtime takes its rule with it (#605)
- `cascade fmt --help` says what `--enforce-spec` gates, from the vendor-prefix
  drop and the media range grammar to the ident code points the reader accepts
  (#611)
- `cascade fmt --enforce-spec` can drop a rule with a raw non-ASCII selector
  without `--minify`: the flag also gates the parser's ident range, so the
  "has no effect without --minify" warning was false (#625)
- `cascade fmt --profile` without `--minify` printed an empty
  factoring-fixpoint report, contradicting its own "has no effect without
  --minify" warning; the report is skipped when nothing ran (#628)
- `cascade fmt` exits 1 when parse recovery left no statement at all, where it
  exited 1 whenever the printed output was empty and the parse had warned, so a
  build failed over CSS the parser used in full (#494)
- `cascade apply` exits 0 when a `<style>` block parses to at least one
  statement, so a build gating on the exit status passes on valid CSS. The
  check looked at the printed text instead, and an empty rule prints nothing
  even though nothing was lost (#489)
- `cascade apply` reads a `style` attribute as a declaration list in source
  order, where the declarations came out reversed and a `}` inside the attribute
  closed the list early (#326)
- `cascade apply --minimal` drops an inherited declaration only when it truly
  restates the value the element would inherit, where it uncovered the
  user-agent link colour and halved a paragraph's `2em` font size (#326, #329)
- `cascade apply --minimal` keeps a restated inherited shorthand when an
  element in between sets one of the longhands it resets, where a
  `#mid{font-weight:bold}` above `#c{font:16px serif}` left `#c` bold (#332)
- `cascade apply` empties the `<style>` blocks it projects instead of removing
  them: a `<style>` element is a sibling like any other, so unlinking it
  stopped `.navbox + style + .portal-bar` from matching (#339)
- `cascade apply` keeps a declaration in the sheet when a kept rule writes the
  same longhand under another property name, where `p{margin:0}` went into a
  style attribute and outranked the `.my-7{margin-top:1.75rem}` above it (#340)
- `cascade apply` keeps the comments a page holds, React writing an empty one
  between two adjacent text nodes to keep them apart. The page is parsed and
  printed with markup.ml in place of lambdasoup (#346)
- `cascade diff` reports a rule that changed places next to whatever else the
  two sheets differ on, where one modified or added rule anywhere in the sheet
  hid every transposition (#474)
- `cascade diff` identifies an at-rule with no condition of its own by the
  header it prints, which is what the ordering comparison keys on, so a
  `@media` that moved between a `@page` and a `@starting-style` is reported as
  a move rather than as no change (#345)
- `cascade diff` names a rule's nested block after the rule it belongs to,
  where the block printed as `& .a`, a selector matching a `.a` inside the
  parent rather than the parent's own block (#385)
- `cascade diff --diff=tree` shows the contents of a block added or removed
  wholesale whatever the block holds, where a `@scope` or a rule nested in
  another rule printed a header with nothing beneath it (#389)
- `cascade diff --diff=tree` states a selector's move once. The entry names the
  selector, not the rule, so a selector whose several rules cross together
  printed the same line once per rule (#581)
- `cascade diff` names a selector whose rules a feature query splits once,
  where two entries under it claimed a declaration gained and another lost that
  the selector never stopped writing (#580)

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
