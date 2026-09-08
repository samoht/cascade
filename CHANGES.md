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

- A calculation whose sum has no consistent type is dropped, so
  `width: calc(sqrt(4) - 1px)`, `border-width: abs(-1)` and
  `width: calc(sign(-1px))` are refused where they were written back as
  `calc(2 - 1px)` and the like, which cascade's own reader also refuses. CSS
  Values 4 sec. 10.9 fails the whole calculation when adding the types of a
  `+` or `-` fails; an angle-valued call still adds to an angle, so
  `rotate: calc(atan(1) + 10deg)` reads (#1151)
- A math function reads wherever the grammar allows its type, with no `calc()`
  wrapper needed, so `font-weight: calc(400)`, `zoom: calc(.5)`,
  `grid-row-start: calc(2) center`, `width: abs(-1px)`, `opacity: pow(2, 3)`
  and `border-top-width: hypot(3px, 4px)` read where they were dropped. CSS
  Values 4 sec. 10.6 gives `abs()` the type of its input and `sign()` a
  `<number>` whatever goes in, so `width: sign(-1px)` is dropped where
  `opacity: sign(-1px)` reads, and `round()`, `mod()` and `rem()` take the
  dimension arguments sec. 10.9 grants them, so `border-top-width:
  round(1.5px, 1px)` reads where only `width` did. Thirteen readers that
  admitted no math at all now do, among them `stroke-width`, `initial-letter`,
  `font-size-adjust`, `baseline-shift`, `columns`, `font-stretch` and
  `interest-delay`. `Cascade.Properties.font_weight`,
  `webkit_line_clamp`, `zoom`, `border_image_slice_item` and
  `shape_image_threshold` gain `Calc`, and `grid_line` gains `Calc_name`
  (#1072, #1086, #1124, #1125, #1126, #1145, #1148)
- A math function takes only the operands CSS Values 4 sec. 10.8 grants, so
  `width: calc(inherit)`, `height: calc(auto)`, `border-width: calc(medium)`,
  `width: calc(fit-content(20rem))` and `width: min(unset, 1px)` are dropped
  with a warning where they used to read. `calc(inherit)` was written back as a
  live `inherit`, which changed what the element computed. Sec. 10.2 also
  requires the arguments of `min()`, `max()` and `clamp()` to share a
  consistent type, so `width: min(0, 1px)` is dropped as well. The same holds
  at `font-size`, `line-height`, `flex-basis`, the opacity family and the
  `<line-width>` slots, where `border-width: min(0, 1px)` used to fold to `0`
  (#1139, #1144)
- The gap decoration, scroll-driven animation and interest properties carry a
  comma-separated list where they carried a single value, one entry per rule
  line or per timeline, so `column-rule: 1px solid red, 2px dashed blue` and
  `scroll-timeline-axis: block, inline` read. A caller writing one entry passes
  a one-element list. `Cascade.Properties.animation_range_item`,
  `animation_range`, `animation_timeline`, `timeline_axis`, `timeline_inset`
  and `interest_delay` gain list arms, `timeline_name` carries
  `timeline_ident` entries, and the two timeline shorthands drop their `None`
  arm for the `none` that CSS puts inside the list; `timeline-scope`, which
  keeps it for the whole value, moves to its own `timeline_scope`
  (#1024, #1050, #1051, #1052, #1053, #1074, #1083)
- `Cascade.Stylesheet.font_face_descriptor` follows CSS Fonts 4 sec. 4:
  `font-style: auto`, `font-weight: auto` and `font-stretch: auto` read
  through three new `_auto` arms, while `font-style: normal italic`,
  `font-weight: lighter` and the whole `font-tech` descriptor are dropped
  where they were read. `font_style_range`, `font_tech_descriptor` and
  `read_font_tech_descriptor` are gone; `tech()` inside `src` and
  `font-tech()` in `@supports` still read (#1116, #1117, #1118)
- The numeric and length leaves carry the full authored grammar, so
  `tab-size: 10.5`, `flex-basis: 1e3px`, `border-width: 3dvh` and
  `vertical-align: 1cap` read where they were dropped with a warning. `Int`
  becomes `Number of number` on `tab_size`, `float` becomes `number` on
  `dash_length`, `animation_iteration_count`, `font_weight.Weight` and the
  three `border_image_*` items, `flex_basis` and `border_width` gain
  `Dimension` for a spelling no typed constructor carries, `vertical_align`
  carries one `Length of length_percentage`, and
  `position_value.Axis_edge_offset` carries a `length_percentage`. A caller
  building a literal writes `Number (Num 4.)`
  (#612, #673, #685, #1021, #1023, #1048, #1054, #1070)
- `Cascade.Properties.page_size`, `page_break_value`, `page_break_inside_value`
  and `border_image_slice` carry the CSS-wide keywords, the way their siblings
  already did. Exhaustive visitors must handle the new leaves
  (#993, #994, #1004)
- Properties read the rest of their grammar, so `ruby-overhang: spaces`,
  `content: url(a.png)`, `flex-wrap: balance`, `list-style-image:
  linear-gradient(red, blue)`, `offset-path: circle(50%) content-box`,
  `hyphenate-limit-chars: auto 3`, `contain-intrinsic-width: auto none`,
  `container: markers stroke fill`, `-webkit-appearance: base-select` and
  `text-box: normal` read where they were dropped with a warning. The value
  types gain the arms that carry them, `position_try_fallbacks` holds typed
  fallback entries, `ray_size` names the five sizes `ray()` accepts directly,
  `place_items` gains the two modifier-first baselines, the `view-timeline`
  and `text-box` types carry their optional slots, and `-webkit-mask-origin`
  and `-webkit-mask-clip` carry WebKit's own box vocabulary rather than the
  `mask_box` of their unprefixed namesakes. Exhaustive visitors must handle
  the new leaves (#549, #668, #669, #670, #671, #725, #995, #1011, #1012,
  #1013, #1018, #1019, #1020, #1049, #1071, #1087, #1088)
- Constructors that printed CSS no grammar grants are gone:
  `flex_basis.From_font`, and `Left`, `Right` and their four `Safe_`/`Unsafe_`
  spellings on `align_content` (#658, #1014)
- `Css.filter` gains `Omitted of filter_function`, `Css.Supports.t` gains
  `General_enclosed`, `Css.kind` gains `Radial_shape`, `Radial_size` and
  `Position_value`, and `Css.Stylesheet.moz_document_condition` gains
  `Url_exact`, `Domain`, `Media_document` and `Regexp`, so a match on any of
  them is no longer exhaustive. They retain what the reader used to discard
  (#461, #508, #869, #870)
- `Css.color` keeps the origin of a relative colour as a colour rather than in
  the opaque tail: `Relative_rgb` carries `color * string` and
  `Relative_color_mix` a pair of them (#313)
- Values every browser drops are dropped with a warning, where they used to
  read: a colour anywhere but the final background layer, a second
  `-webkit-text-stroke` width, `gap: -10%`, `text-decoration:
  fit-content(20rem)`, and a sizing function on a margin or inset property.
  This release adds `Cascade.Values.read_margin_length`
  (#1096, #1097, #1101, #1108)
- A `var()` whose custom property resolves through another one keeps its
  reference rather than taking its fallback, so `--n: 5px; --x: var(--n);
  color: var(--x, lime)` computes the inherited colour as every browser does
  instead of `lime`. CSS Variables 1 sec. 3 puts the fallback in only where
  the custom property is the guaranteed-invalid value (#1100, #1102)
- `border-image` and `mask-border` fill their slots in any order, so
  `border-image: 50% none` and `round none 30` read where the reader took them
  in one fixed order (#1089)
- Browser support comes from the web-features dataset rather than from
  versions written into the source, so a `backdrop-filter` targeting Safari
  17.0 to 17.6 keeps its `-webkit-` twin, which the old boundary dropped. This
  release adds `Cascade.Support`, whose `measured` records what this project
  measured for productions the dataset gives no compat key and whose
  `self_measured` says which answers came from there;
  `Cascade.Optimize.targets` is an alias of `Cascade.Support.targets`
  (#1090, #1110, #1111)
- An `@supports` condition keeps what the author wrote: the property name with
  its escapes, and the value unrewritten through the property's typed grammar,
  which changed what the condition tested. `--minify` keeps the guard rather
  than deciding it, and `Css.Supports.simplify_baseline` is gone. Two
  spellings of one condition are still one block to the diff;
  `Cascade.Supports.pp` and `to_string` gain `?verbatim`, which tells a
  serialiser from an identity, and `Cascade.Supports.Declaration` carries the
  name beside the declaration (#584, #587, #1092, #1095)
- Implementation modules are no longer reachable through accidental
  `Cascade.*` aliases, and the redundant public ones are gone. Use
  `Declaration.pp` and `Declaration.to_string` rather than `pp_declaration`
  and `string_of_declaration`, `Cascade.Parser.string_of_components` rather
  than `Parser.to_string_custom`, `Container.read` / `Media.read` over a
  cursor rather than `of_components`, the exhaustive
  `Css.Stylesheet.statement_declarations` rather than
  `Css.statement_declarations`, and `Css.vars_of_stylesheet` rather than
  `Css.vars_of_rules`. `Cascade.Reader` is the character cursor `Lexer`
  drives and no longer reads CSS, the one-off `Css.Transform`,
  `Transform_origin`, `Perspective_origin` and `Animation` string parsers are
  replaced by the main readers, `Css.Selector_summary.clear_memo` is gone (it
  did nothing), and `Cascade.Component.pp` says that it prints a debug dump
  (#348, #382, #496, #497, #499, #501, #504, #509, #514, #539, #544, #548,
  #806)
- `Css.Stylesheet.layer_name` is the identifiers a `<layer-name>` is made of
  rather than the text between them, and `Css.Stylesheet.edit_statements`
  callbacks return `Keep`, `Replace` or `Drop` through the new
  `Css.Stylesheet.edit` type (#442, #539)
- `Declaration.declaration` is a private variant: its constructors still
  match, but build values with `Declaration.v` (#527)
- The public constructors raise where they used to accept: `Invalid_argument`
  for `[]` on a helper whose CSS value requires a non-empty list,
  `Cursor.Parse_error` from `Declaration.of_string` for every input it
  refuses, `Error.Parse_error` from `Css.Selector.of_string ""`, and `Failure`
  from `Css.Supports.property` and `Css.Declaration.custom_property` on text
  that does not write back as the one declaration it names. `Cursor.pair` and
  `triple` rewind the cursor when the separator or a later parser fails, so
  code that caught `Parse_error` and read on sees a different position
  (#421, #428, #459, #509, #528, #535, #691)
- Records gained fields, so expressions must set them and patterns must bind
  them or use `; _`: `recovery` on `Cascade.Error.t`, which says whether the
  reader dropped the construct or kept it; `source` on `Css.parse`, filled by
  `Css.of_string ~preserve_source:true`; `line` and `col` on
  `Cascade.Reader.parse_error`, whose `filename` now holds a source name
  rather than a packed `"<CSS input>:L:C"`; `in_style_rule` on `Css.Pp.ctx`;
  `media_inapplicable` on `Css.Context.query`, which tells a recognised
  feature matching no value from an unknown one; and `added_properties` and
  `removed_properties` on `Tree_diff.Content_changed`
  (#374, #491, #723, #747, #834, #868)
- `Css.Media.equal` reads normalised query structure where it read serialised
  text, so it is no longer `Css.Media.compare a b = 0`, and `Css.Media.kind`
  classifies a negated width bound by the range it actually matches, so
  `not (min-width: 640px)` sorts with the upper bounds (#328, #516)
- `cascade diff` exits 2, not 0, when it finds no difference but had to drop a
  declaration or a rule it could not read: what it dropped reached neither
  side of the comparison. `--depth` is gone, since a report is bounded by
  whole differences rather than by tree levels; pass `--limit=none` where
  `--depth=max` was. `cascade` requires `cmdliner >= 2.0.0`, the release that
  lets either side name standard input as `-`
  (#792, #796, #832, #833, #834, #835, #836)

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
- A property refuses what its own grammar refuses, so a value every browser
  drops is dropped with a warning rather than written back: `zoom: -1`,
  `line-height: 45deg`, `will-change: none`, `font-size: fit-content(20rem)`,
  `padding-bottom: anchor-size(width)`, `translate: auto`,
  `border-top-color: calc(2px - 3px)`, `border-image: none, none`,
  `font-language-override: "default"`, `grid-template-rows: -2px`,
  `grid-template: 10px`, `quotes:`, `border:` and a `@page size` given a
  percentage. Ranges, units, sizing functions and the closed `<color>`
  production are each checked, where the reader carried any dimension or any
  well-formed token run through, and an empty value is a declaration only for a
  custom property. This release adds
  `Cascade.Properties.read_border_image_source`
  (#640, #982, #1000, #1001, #1002, #1003, #1004, #1005, #1006, #1007, #1009,
  #1010, #1015, #1077, #1078)
- A `<custom-ident>` refuses the names CSS Values 4 sec. 4.2 reserves, in every
  ASCII case permutation, so `animation-name: default`, `counter-reset: DEFAULT`,
  `view-transition-name: default` and a `default` counter-style symbol are
  dropped with a warning. The string spelling is untouched, so
  `animation-name: "default"` still reads. Six productions each carried their
  own exclusion list, five of which had forgotten `default`; they now share
  `Cascade.Cursor.custom_ident` (#1143)
- A value list carries the number of items its grammar grants, so
  `mask: none, none` and `transition-behavior: normal, normal` read where they
  were dropped, while `text-shadow: none, none`, `box-shadow: none, none` and
  `list-style: none none none` are dropped with a warning: `none` is a whole
  value there, not a list item, and CSS Values 4 sec. 2.2 takes each option of
  a `||` at most once. An unquoted `font-family` name refuses a generic keyword
  only where it heads the sequence, so `font-family: serif serif` is dropped
  and `font-family: Cambria Math` reads (#1147)
- A `@keyframes` name is spelled the way its value requires, so
  `@keyframes "default"` keeps its quotes where it used to print as
  `@keyframes default`, which every browser drops, and `@keyframes "none"` is
  read where it was refused. CSS Animations 1 sec. 3 makes the two syntaxes
  equivalent and the name the value of the ident or string, serialised as an
  ident unless it is a disallowed keyword. `color-scheme` reads its list items
  as `<custom-ident>`, so `color-scheme: default` is dropped with a warning
  (#1146)
- A property whose grammar names a `<length>` takes neither a percentage nor an
  intrinsic-sizing keyword, and a `<time>` or `<angle>` needs its unit. Cascade
  read them wherever it read a length, so `border-width: 50%`, `top:
  min-content`, `transition-duration: 0`, `rotate: 0` and `box-shadow: 20px
  10%` all parsed, most of them turning a declaration browsers drop into one
  that works. `Css.Values.read_length` gains `?sizing` and
  `read_non_negative_length` gains `?length_only` (#871, #879, #880, #951,
  #952, #953, #954, #1056, #1058, #1059)
- Values browsers accept are read rather than dropped: `animation-duration:
  auto`, `grid: none / 200px`, `place-items: flex-start baseline`, the full
  `text-wrap` grammar, `row-gap: normal`, a custom counter-style name,
  `transition-property: all, opacity`, an empty `blur()`, a `style()` range
  query, `display: flow-root list-item`, `outline: thin solid red`, the
  `svg||td` column combinator, `:dir(auto)`, `@-moz-document`'s URL matchers,
  `white-space: collapse`, `text-box: auto`, `background-position: 50% bottom`,
  every `<image>` at `list-style-image`, a CSS-wide keyword at the
  `page-break-*` aliases and at `border-image-slice`, `margin` mixing `auto`
  with lengths in any slot, the four side styles at `border-style`, both radii
  at every `border-*-radius` corner, `display: grid-lanes`, and a value whose
  grammar ends in an optional component (#334, #335, #427, #456, #461, #551,
  #572, #579, #594, #633, #641, #644, #646, #665, #666, #667, #682, #739,
  #805, #827, #870, #874, #877, #881, #884, #885, #886, #887, #889, #909,
  #993, #994, #995, #998, #1022, #1055, #1060, #1085, #1122)
- A value is checked against the range its specification gives rather than one
  the reader assumed, so `overflow-clip-margin: -1px`, `offset-distance: -10px`,
  `shape-image-threshold: 50%`, `scroll-margin-block: 0 0`,
  `transition: opacity 1s -1s`, `border-right-width: calc(-1px)` and
  `z-index: calc(.5)` read where they were dropped with a warning. A math
  function keeps its call in minified output rather than being unwrapped to a
  bare value the browser drops, since CSS Values 4 sec. 10.12 checks the range
  and rounds on what the function resolves to
  (#996, #999, #1026, #1047, #1080, #1082, #1123)
- Each grid property takes its own grammar. Values a browser drops are dropped
  (`grid-template-columns: [a]`, `[span] 1px`, `grid-column-start: 0`,
  `span -1`, `grid-template: 50% "text" infinite`, `grid: "a b" "b a"`),
  `grid-auto-rows` refuses the area forms `grid-template` owns, `subgrid` takes
  its line-name list, a track size accepts math resolving to `<flex>`, and the
  areas form is read against its grammar rather than kept as any well-formed
  token run (#708, #711, #712, #714, #717, #718, #724, #749, #1016, #1076)
- The font grammars are the ones the specifications spell. `font-variant` and
  `font-variant-alternates` are typed rather than opaque text, `@font-face` and
  `@font-palette-values` read their own `font-family` grammars, the `font`
  shorthand reads and writes all nine `<font-width-css3>` keywords where it
  read two and printed a percentage the slot has no room for, a CSS-wide
  keyword in a descriptor is dropped, and the `src` descriptor reads an empty
  `url()` and drops only the item it cannot read, so a trailing comma no longer
  costs the whole font (#695, #963, #964, #965, #972, #1115)
- Six `@counter-style` descriptors are read against their grammars, so
  `range: bogus`, `pad: 3`, `negative: 1` and `fallback: "decimal"` are dropped
  where each was kept as an opaque string, and a counter symbol may be an image
  (#1130)
- An `@property` registration drops rather than typing values by a grammar it
  cannot honour. Its syntax rejects a component with two multipliers, a
  multiplied `<transform-list>` and a `*` combined with anything, and its
  `initial-value` rejects a `var()`, `attr()` or `env()` at any depth, which
  has nothing to substitute from at registration time. Two syntaxes that used
  to hang the parser are read (#398, #707, #710, #713, #1142)
- A bad piece is dropped on its own and the rule around it survives. A nested
  rule, a descriptor, a stray `;`, a margin at-rule, an `@counter-style` with
  no descriptor, an `@media` condition, an `@font-face` descriptor name, an
  `@scope` bound, an at-rule among `@font-face` descriptors and leftover tokens
  after a value are each dropped alone, an unrecognised at-rule reaches the
  output with its block intact, an unknown `@supports` or media condition keeps
  its guard, and a `@media` prelude replaces only the query of the list that
  fails, so `@media ,(min-width: 10px)` still guards its rules on the width.
  `Optimize.drop_unknown_at_rules` drops such a rule on request (#374, #380,
  #384, #388, #392, #399, #402, #403, #404, #405, #419, #420, #469, #483,
  #727, #867, #869, #895, #897, #946, #947, #948, #949, #974, #977)
- A selector no browser can match is dropped rather than written back, in a
  nested rule's prelude as well as at top level, and a pseudo-element that
  carries structure keeps the compounds it allows:
  `::file-selector-button:hover` and a chained `::part(label)::before` survive
  where a combinator after a pseudo-element does not. A `.` or a `:` names
  nothing across a space, so `. x a` and `: hover` are dropped rather than read
  as `.x a` and `:hover`, which matched elements the author never named.
  `Css.Selector.of_string` refuses a string that is not a selector rather than
  reading it as an element name (#418, #426, #430, #441, #552, #553, #556,
  #559, #950, #976, #982)
- A `var()`, `env()` or `attr()` call keeps the type of the component it stands
  for and defers CSS-wide keyword validation to substitution, whether it is
  written as a slot of the value or inside another function, so
  `offset-rotate: color-mix(var(--a), var(--b))` is kept where only a top-level
  call was. A value the typed reader refuses is preserved opaquely when the
  property is unknown or the value is a runtime substitution, and a value
  carrying an unmatched `)`, `]` or `}` is dropped with a warning even where a
  `var()` sends it down the opaque path (#511, #518, #726, #729, #734, #735,
  #787, #813, #978, #997)
- A `var()` the evaluator cannot read a value from keeps its reference rather
  than taking its fallback: an empty binding, one the property's grammar
  refuses, one resolving through another custom property, and a CSS-wide
  keyword. CSS Variables 1 sec. 3 puts the fallback in only for a property
  holding its guaranteed-invalid initial value, so
  `width: 37px; width: var(--nope, notalength)` measured 37px where the browser
  lays out `auto`. `--inline-vars` keeps such a `var()` live where the
  definition it names sits on a selector or in an `@media` the pass cannot
  prove reaches the element, and every definition of a name still referenced
  reaches the output. `Css.Declaration.parse_custom_property` takes the empty
  value, so it accepts the pairs `custom_property` accepts
  (#985, #986, #987, #988, #989, #990, #991, #992)
- Keyword, at-rule and function names match without regard to case, so
  `grid-column: SPAN 2`, `@MEDIA`, `RGB()`, `VAR(--x)`, `:dir(LTR)`,
  `touch-action: NONE`, `display: LIST-ITEM FLOW-ROOT`, `align-items: FIRST
  BASELINE`, `grid-template-columns: REPEAT(3, 1FR)` and `color: COLOR(DISPLAY-P3
  1 0 0)` read, while an author-defined name keeps the case it was written in.
  An escaped name reads as the name it spells: `@supports (--x\3b y: red)` is
  read, and `@layer a\2e b` names the layer `a.b` rather than the sublayer `b`
  of `a` (#437, #442, #602, #603, #604, #620, #622, #767, #1141)
- Cascade reads back everything it writes. Minified `@scope to (...)`, `rotate`
  with a negative axis, a relative colour's channels, a `-webkit-gradient`
  `color-stop()` and `center` point, `text-decoration: none solid`,
  `transition: none 1s`, an unterminated string, a quoted `animation-name`, a
  keyword-shaped keyframe name, an at-rule body ending on a backslash, a `u+a`
  selector outside `unicode-range`, an explicit `animation: spin 0s`, a `url(`
  at end of input and a repeating gradient built from a `var()` each survive
  the round trip (#558, #656, #875, #876, #894, #896, #898, #899, #900, #901,
  #902, #903, #910, #911, #912, #913)
- A colour is read as the closed production CSS Color 5 sec. 3 defines. A
  relative colour's channel expressions are type-checked, so
  `rgb(from red calc(r + 10%) g b)` is dropped as every browser drops it while
  the multiplying forms still read; an out-of-range `oklab()` or `oklch()`
  lightness written as a percentage clamps as the bare number already did; and
  `color(from <origin> srgb r g b)` folds to the origin when that origin is
  itself a `color()` (#904, #905, #1061)
- `offset`, `offset-anchor`, `offset-position`, `position-area`,
  `position-try-fallbacks` and `white-space` are typed against their own
  grammars, and a literal reads as the node the library exports, so a
  constructed declaration and a parsed one compare and hash equally. A
  `border-image` slice compares against its initial without a polymorphic
  equality, which walked another module's representation (#478, #485, #495,
  #651, #652, #653, #654, #657, #674, #683, #888, #935)
- An input that ends mid-value keeps its declaration, and an `@page` prelude
  naming neither a page nor a pseudo-page invalidates the rule. CSS Syntax 3
  sec. 4.3.5 ends a string at EOF as the string it read, and cascade dropped
  the rule and printed the value back without its closing quote, so the rest of
  the sheet was swallowed. A custom property holding a `!` outside its
  `!important` flag is dropped with a warning, and `--x: 1 ! important` is
  important with the value `1` (#972, #973, #979)
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
  own reader reject, losing the block or running two declarations together
  (#319, #370)
- A `calc()` printed without `--minify` keeps the parentheses the author wrote,
  redundant ones included, while `--minify` removes those and keeps a
  precedence-sensitive `calc((1px - var(--a)) * 3)` (#721)
- A NaN-valued number prints as `calc(NaN)` and a NaN-valued dimension as
  `calc(NaN * 1unit)`, where `calc(sqrt(-1) * 1px)` printed `NaNpx`, which
  browsers drop (#425)
- `Css.to_string` renders the sheet once, where it rendered it twice, the first
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
  `white-space`, `text-wrap` and `font`. A component written at its own
  initial, or written `initial`, reads as the slot the shorthand leaves out, so
  `flex-flow: row wrap` minifies to `flex-flow: wrap` and `flex: 0 0 auto` to
  `flex: none`; a repeated side folds to the shortest spelling naming the same
  sides; and a longhand that only restates what the shorthand in front of it
  wrote is dropped, so the whole border family written out minifies to
  `border: 1px solid red` alone (#915, #916, #917, #918, #919, #920, #921,
  #922, #923, #924, #925, #926, #927, #928, #929, #930, #931, #932, #933,
  #934, #935, #937, #939, #940, #942, #956, #959, #960, #961, #965)
- Contraction never writes a declaration the input did not mean. It resets no
  longhand the run did not write, so a run leaving one of the shorthand's own
  longhands unwritten needs `--scope=stylesheet`: under the default fragment
  scope an earlier `transition-timing-function` from another file is not
  cascade's to reset. It no longer contracts a run holding `inherit`, `unset`,
  `revert` or `revert-layer`, since a CSS-wide keyword is a whole declaration
  value and pasting one into a shorthand made `padding-left: inherit` beside
  its three siblings into `padding: 0 2em 10% inherit`, which every browser
  drops and which lost the element all four paddings. And it no longer loses a
  `border-image` a neighbouring rule set: the family shared one slot in the
  hazard model, so a rule holding the slice answered for a neighbour holding
  the source and the picture went missing (#843, #845, #914, #943, #957, #958,
  #968)
- `Css.Properties` gains the typed properties the contractions need:
  `background-position-x` / `-y` and their `-webkit-mask` twins,
  `column-rule-width` / `-style`, `white-space-collapse`, `column-height` and
  `column-wrap`. `border-image-outset` and `border-image-width` reject a
  keyword their grammar does not carry (#926, #927, #928, #936, #938, #941)
- A value keeps every digit and every unit the author wrote unless the shorter
  spelling means the same number. `.4285714em` came out short,
  `calc(hypot(1px, 1px))` came out as `1.41421356`, and a computed dimension
  past a million units lost its own digits. A `calc()` whose result falls below
  zero keeps its call on a property whose range starts there, since CSS Values
  4 sec. 10.3 clamps at used-value time: `width: calc(-10px)` computes to `0px`
  where `width: -10px` is dropped, and folding the call away turned a working
  declaration into one browsers throw out. That now holds for a `<number>` as
  well as a length, so `aspect-ratio: sign(-1px)` keeps `calc(-1)` where it
  wrote `-1`, output its own reader then refused, at eleven properties among
  them `flex-grow`, `line-height`, `tab-size`, `stroke-width` and
  `border-radius`. A sum leads with a positive term where it has one, so
  `calc(-10px + 100vw)` minifies to `calc(100vw - 10px)` rather than growing a
  sign into `calc(3px + -2em)` (#350, #354, #362, #367, #676, #731, #967,
  #1113, #1149)
- `--minify` folds a value's spelling before two rules are compared, so a hex
  colour, a NaN, an unreduced `min()`, a same-unit `calc()`, a shorthand
  component at its longhand's initial, a repeated `font-family` entry, a
  `color-mix()` in a wide-gamut space, the `sin()`-`atan2()` family inside a
  custom property, an unitless `line-height: calc()`, logical minimum sizes,
  duration units, stepped functions, hue-angle units and `steps(1)` all reach
  one form (#471, #482, #597, #618, #624, #626, #635, #636, #637, #639, #640,
  #641, #647, #650, #672, #675, #678, #681, #755, #756)
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
  pseudo-element parent is dropped wherever it sits, a selector-list or
  type-selector parent keeps its `:is()` wrapper so `a { .x& { } }` flattens to
  `.x:is(a)` and not the class `.xa`, and `--flatten-nesting` leaves the result
  flat (#759, #800, #818, #822, #823, #975)
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
- A length reaching one minified text through two nodes is one node, so
  `a{width:1.0px}b{width:1px}` merges in the pass that minifies it rather than
  the one after. `Declaration.hash` keys the structural value and short-circuits
  `Declaration.same_minified`, and the reader kept an authored spelling the
  printer already discards, so one text arrived through two nodes (#1150)
- `--minify` and `cascade diff` are faster on a large stylesheet, for
  byte-identical output. The slowest corpus stylesheet drops sharply, a long run
  of rules sharing one selector no longer allocates quadratically, a 4,000
  statement sheet is linear, nested group-rule merges optimise only the newly
  joined list, and a single invocation reaches a stable result where it used to
  need a second (#413, #422, #424, #468, #480, #486, #487, #493, #502, #505,
  #507, #517, #519, #523, #542, #543, #566, #664, #746, #750)

### Custom properties

- A custom property's value, a container or media query, and an `@supports`
  condition all keep the text the author wrote. Whitespace runs survive, where
  `--x: a  b` came back as `a b`; escapes survive, where `--v: gre\en` came
  back as `gre\E n` and `@container (min-width: 1\0px)` reached the output with
  a raw U+FFFD. A condition is the question the rendering browser is asked
  rather than a value to respell, and CSS Custom Properties 1 sec. 4.1 forbids
  normalizing a custom property; the ends are still trimmed, and `--minify`
  still makes its own separator decisions. `Cascade.Token.t` gains `repr`, the
  source text of a token whose spelling reserialization does not give back,
  `Cascade.Token.Whitespace` carries its run, and
  `Cascade.Parser.to_string_verbatim` writes them back
  (#1064, #1065, #1066, #1067)
- `Css.inline_vars` sees every place a `var()` can be written: an `@font-face`
  descriptor, `@page` and its margin boxes, a `@keyframes` frame,
  `@position-try`, a `@supports` condition and a nested rule. A descriptor
  reference used to be dropped at parse time, and `Css.custom_props` missed a
  name declared inside `@scope`, `@starting-style`, `@-moz-document`, `@when`
  or `@else`. `Css.vars_of_declarations` reports the `var()` inside a
  `stroke-dasharray` or `stroke-dashoffset` dash, where it returned nothing
  (#322, #341, #342, #375, #423, #555, #571, #573, #575, #577, #1048)
- `Css.inline_vars` resolves a property defined across cascade layers against
  the sheet's layer order, unwraps an `@layer` only where layer order and
  document order already pick the same winner, and keeps the `@property`
  registration of a custom property it leaves live (#357, #371, #373, #416)
- Substitution preserves what the declaration was. A custom property keeps its
  cascade layer and caller metadata, a `page-break-*` declaration survives as
  itself, a reference marked resolved at runtime stays marked, and an
  overridden variable is reported through `~warn` (#315, #506, #520)
- `@scope { color: green }` reads, where the whole at-rule used to be dropped
  with a warning, and an at-rule inside a keyframe or descriptor block costs
  only itself rather than the rest of the block, so
  `@keyframes k { to { @e {} opacity: 1 } }` keeps the opacity (#1068, #1069)
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

- `Css` builds every property the AST models, bar `Unknown_property` and the
  `@font-face` `src` descriptor, neither of which is a plain property. This
  release adds the SVG presentation longhands, the CSS Anchor Positioning 1 and
  Motion Path 1 families, the scroll-driven animation properties, the
  border-image and multi-column families, `contain_intrinsic_size` and its axis
  longhands, the flow-relative border shorthands, the decoration-skip and
  typography longhands, the `-webkit-`, `-moz-`, `-ms-` and `-o-` spellings its
  `{2:vendor_specific}` section documents, `view_transition_name` and
  `view_transition_class`, `Css.all`, and the shape, overflow, image,
  margin-trim, overlay, animation-composition and background-position-axis
  families, each with its value types (#1027, #1028, #1029, #1030, #1031,
  #1032, #1033, #1034, #1035, #1036, #1037, #1038, #1039, #1040)
- `Css.Stylesheet` reaches every statement and declaration a sheet holds:
  `fold_statements`, `iter_statements`, `edit_statements`,
  `fold_declarations`, `statement_declarations`, `statement_children`,
  `map_statement_children`, `map_statement_declarations` and
  `at_declaration_site`. `Css.map`, `Css.sort`, `Css.layers`,
  `Css.layer_block` and `Css.flatten_nesting` reach a rule inside `@scope`,
  `@starting-style`, `@-moz-document`, `@when` or `@else` (#317, #337, #355,
  #356, #363, #368, #381, #382, #384, #389)
- `Css.Resolve` answers the selectors engines answer: `:nth-child(... of S)`,
  the typed `:nth-of-type()` family, `:has()`, the `i` and `s` attribute case
  flags, and `:empty` for the elements Selectors 4 and the engines agree on. It
  ranks a cascade layer's own rules after every one of its sublayers, as
  css-cascade-5 sec. 6.4.3 requires, so `@layer a` outranks `@layer a.b`
  however the two were declared (#607, #873)
- `Resolve.prepare` and `Resolve.Make.resolve_prepared` split the sheet-only
  work out of `resolve`, so a caller walking a document pays it once.
  `Resolve.Make.resolve` and `layer_order` document every block they leave out
  (#394, #567)
- `Css.Context.matches_media` respects zero-valued boolean features and
  resolution units and preserves unknown through negation, and
  `matches_container` requires the supplied container to support every queried
  feature (#868)
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
  differ on. It shows the declarations a `@keyframes` frame gained, lost or
  changed, and counts a rule as changed only when its own declarations changed,
  so a rule holding an edited nested rule is no longer summarised as a
  difference the report cannot show (#345, #385, #389, #474, #580, #581, #783,
  #784, #794, #798, #814, #815, #906, #907)
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
