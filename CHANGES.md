## 1.2.0 (unreleased)

Most entries below are defect fixes, and the largest group of them has one
cause. Readers, printers and optimizer passes each walked the statement tree by
hand and closed with a wildcard, so an at-rule added to the AST later fell
through them unseen rather than failing to compile. They share one exhaustive
walk now, and a statement kind added after this release stops every site that
decides about it from compiling. Correctness was judged against a browser
rather than against cascade. The suite renders a sheet and its optimised form
in headless Chrome, then compares every property `getComputedStyle` reports on
every element. Behind that sit 504 CSS files drawn from 72 production sites and
2960 recorded cases carrying six minifiers' answers, and several of the fixes
below are miscompiles Chrome contradicted rather than readings of the spec.

**Upgrading from 1.1.0.** Many of the fixes below change the CSS cascade emits
for input 1.1.0 already accepted, and a dozen of them change how the page
renders. If you shipped minified output built with 1.1.0, re-run it and
compare: `cascade diff --diff=canonical old.css new.css` exits 1 and prints the
difference wherever the two are not equivalent.

### Breaking

- `cascade` requires `cmdliner >= 2.0.0`, the release that stops `Arg.file`
  checking a `-` argument for existence, which is what lets either side of
  `cascade diff` name standard input (#796)
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
  later parser fails, as `option`, `one_of`, `try_parse_err` and `list` already
  do. Code that caught `Parse_error` from either and read on from the advanced
  position now re-reads what the first parser consumed (#509)
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
  selector-shaped input raised `Failure` and everything else already raised
  `Cursor.Parse_error` (#535)
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
- `Css.statement_declarations` is gone. It answered for a rule and a bare
  nesting block only, sharing its name with the exhaustive
  `Css.Stylesheet.statement_declarations`, which reaches every declaration a
  statement holds; call that one instead (#348)
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
- `Css.Media.kind` classifies a negated width bound by the side it bounds, so
  `not (min-width: 640px)` sorts with the upper bounds it matches and a doubled
  `not` cancels. `sort_key`, `group_order` and `compare` follow, so a caller
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
  exhaustive. `border-width` takes a `<length>`, but the reader named its own
  units, so `border-width: 3dvh` was dropped as invalid while `margin: 3dvh`
  was read (#612)
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
- `Cascade.Component.pp` documents and renders itself as the located debug dump
  it always was, where being documented as source text sent a caller down a
  check that could never fire. Source text comes from
  `Cascade.Parser.string_of_components` (#504)
- An author's `@supports` guard survives `--minify`, and
  `Css.Supports.simplify_baseline` is gone. Cascade decided the condition
  against a property-granular table, so `@supports (height: stretch)` was read
  as a question about `height` and every browser lacking that value lost the
  guarded fallback (#584)
- An `@supports` condition keeps the value the author wrote, where cascade
  re-spelled it through the property's typed grammar and asked a different
  question. `Css.Values.normalize_color` loses its unused `in_feature_query`
  argument, and `Css.Declaration.parse_opaque_declaration` reads a declaration
  without its typed grammar (#587)
- `cascade diff --depth` is gone: a report is bounded by whole differences
  now, not by tree levels. Pass `--limit=none` where `--depth=max` was, and
  `--limit=N` where a level was pinned (#792)

### Parsing

- A `style()` container query takes the single-comparison range CSS Conditional
  Rules 5 defines, `style(--gap = 10px)` included, and rejects an interval whose
  two bounds point different ways; `Css.inline_vars` reads its operands (#805)
- A parse warning whose value matched none of a property's forms says so,
  in place of a reason that opened a list of accepted forms and named
  none of them (#801)
- A parse warning marks the value the author wrote rather than the token after
  it, so the caret no longer lands on the semicolon that closes the
  declaration (#801)
- Lenient parsing preserves a declaration-safe value opaquely when a known
  property's typed reader rejects it, while retaining the warning that makes
  strict parsing reject the declaration (#787)
- Mixed-case keywords in `animation-range` and `scroll()` parse and serialize
  in their canonical lowercase form (#767)
- Grid track sizes accept math functions that resolve to `<flex>`, including
  `calc(1fr * 2)`, `min(1fr, 2fr)` and `clamp(100px, 1fr, 300px)`, in explicit,
  repeated and automatic tracks (#749)
- A sole baseline position in `place-content` is accepted and defaults its
  omitted `justify-content` slot to `start`, as required by CSS Align (#739)
- Custom-property declarations containing a `<bad-string-token>` are dropped
  during stylesheet recovery, so minified output no longer swallows the
  containing rule's closing brace when it is parsed again (#727)
- Declarations containing a non-empty `var()`, `env()` or `attr()` call defer
  CSS-wide keyword mix validation until substitution, instead of being dropped
  while the substituted token stream is still unknown (#726)
- Component `var()` values in `border-radius`, `gap`, `transform-origin`,
  `border-spacing` and `place-content` stay typed, preserving adjacent folds
  and variable discovery (#729, #734)
- Component `var()` values in `place-items`, `grid-auto-flow` and border-image
  dimensions stay typed, preserving adjacent folds, ASCII keyword handling and
  variable discovery (#735)
- `revert-rule` is reserved wherever a grammar accepts a `<custom-ident>`, so
  it no longer survives inside grid line-name lists as an ordinary name (#724)
- The grid track-list grammars are the ones a browser applies: `grid-auto-rows`
  and `grid-auto-columns` take a plain track-size list, `grid-template-rows`
  and `grid-template-columns` reject the slash and string area forms the
  `grid-template` shorthand owns, and `subgrid` takes its optional line-name
  list (#712, #717, #718)
- A grid track list carries at least one track size and never two line-name
  blocks in a row, so `grid-template-columns: [a]` and `1px [a] [b] 2px` are
  dropped the way a browser drops them, inside `repeat()` and the shorthands
  as well (#712)
- Grid line names reject the idents that are not `<custom-ident>`s there,
  `span`, `auto`, `default` and the CSS-wide keywords in every ASCII case; a
  line index cannot be zero and a span count must be positive, so `[span] 1px`,
  `grid-column-start: 0` and `span -1` drop the way a browser drops them
  (#708, #714)
- The span form of `<grid-line>` takes its operands in any order, so
  `grid-column-start: 3 span` is kept and printed `span 3` where cascade used
  to drop the declaration (#711)
- The grid placement properties go through the CSS-wide keyword check.
  `grid-column: 2 / initial` is dropped, while a lone `grid-column: initial`
  still reads as explicit defaulting (#712)
- An `@property` syntax component carries at most one multiplier, and the
  pre-multiplied `<transform-list>` takes neither `+` nor `#`, so
  `"<custom-ident>+#"` and `"<transform-list>+"` drop the registration rather
  than typing the property as something else (#707, #713)
- A `@property` syntax multiplying `<transform-function>`, `<transform-list>`
  or `<resolution>` no longer hangs the parser, and `*` is a syntax on its own
  only, so `"*+"` and `"* | <length>"` are rejected (#710)
- A function reader that cannot read all of its arguments invalidates the
  declaration instead of answering with the arguments it had, so
  `transform: translateX(10px red)`, `skew(10deg,red)`, `matrix(1,2,3,4,5,6,)`
  and `border-width: max(1px, red)` drop instead of reading as
  `translateX(10px)`, `skew(10deg)` and `1px` (#617, #627, #629, #631)
- Trailing content inside a parenthesised or bracketed sub-expression makes the
  whole value invalid, as it does in a browser. `width: calc((1px 2px))` used
  to keep the prefix a reader recognised and drop the rest (#701)
- A math function requires whitespace on both sides of its `+` and `-`, so
  `calc(100%- 10px)` is dropped the way a browser drops it, where cascade
  accepted it and printed back the valid spelling (#699)
- A keyword written in another case is read as that keyword, so
  `grid-column: SPAN 2` is a span of two tracks rather than the reordered
  `2 SPAN` it printed, and a `FROM` keyframe, an `@import URL()` and an `INSET`
  shadow reach the output. A name the author owns keeps its case (#603)
- An at-rule or function name written in another case names what it spells, so
  `@MEDIA`, `RGB()`, `color-mix(IN srgb, ...)`, `@page :FIRST`, `@TOP-LEFT` and
  the legacy `/DEEP/` read as their lower-case spelling and optimise like it.
  `@charset` stays the byte sequence CSS Syntax 3 sec. 8.2 matches (#604)
- A `var()`, a `-webkit-gradient()` colour stop, an `animation-timeline`
  `scroll()`/`view()` or `box-shadow`'s `inset` written in another case is read
  as that keyword, where `transition: VAR(--x) 1s ease` came out as
  `transition: all var(--x) ease 1s` and a capitalised `COLOR-STOP()` dropped
  the whole declaration (#620, #622)
- A `:dir()` argument written in another case names the directionality it
  spells, so `:not(:dir(LTR))` shortens to `:dir(rtl)` like its lower-case twin
  and rules that differ only in that case merge (#602)
- `:dir()` accepts any single identifier, so `:dir(auto)` is read and written
  back instead of taking its whole rule down. CSS Selectors 4 sec. 7.1 makes a
  value other than `ltr` or `rtl` match nothing rather than invalid, which
  through `:not()` is the difference between matching every element and
  none (#594)
- `@font-face` and `@font-palette-values` use their descriptor-specific
  `font-family` grammars: one named family for the former, a non-empty
  comma-separated list for the latter, and neither takes an unquoted
  generic-family or CSS-wide keyword (#695)
- `steps()` rejects fractional counts, and `jump-none` requires at least two
  steps (#688)
- `offset` is a typed shorthand: it validates the Motion Path grammar, so
  `offset: total nonsense here` is dropped rather than carried as opaque text,
  and its slots canonicalize the way the longhands do (#683)
- `offset-anchor` and `offset-position` are typed properties. They validate
  their Motion Path grammars and canonicalize their `<position>` branches, and
  no longer accept length-only keywords such as `normal` as coordinates (#674)
- The three- and four-value position readers validate horizontal/vertical edge
  pairs instead of accepting arbitrary identifiers or two edges on one axis.
  Generic `<position>` rejects three-value forms, `background-position` keeps
  its valid extension, and `transform-origin` rejects the four-component
  edge-offset forms its grammar never had (#673, #680)
- `mask-border` accepts a lone mode keyword and `border-image` rejects one
  anywhere: only mask-border's grammar carries a `<'mask-border-mode'>` slot
  (#682)
- `border-image` accepts a repeat keyword without a source or slice, omitted
  slots taking their initial values, so `border-image: round` is no longer
  dropped (#667)
- `white-space: collapse` reads as the new public `white_space.Collapse` node,
  a white-space-collapse component being valid without the shorthand's other
  optional longhands (#666)
- `text-decoration` accepts a colour, style or thickness without a line value:
  its four components are joined by `||`, so none is individually mandatory
  (#665)
- A CSS-wide keyword mixed into a `font-family` list reads as the exported
  `font_family.Invalid` node, preserving the source for typed invalid-value
  recovery instead of raising during property parsing (#657)
- `repeating-linear-gradient(var(...))` keeps its repeating function name
  instead of serializing back as a non-repeating linear gradient (#656)
- A `var()` that supplies the complete body of a radial or conic gradient reads
  as the exported `Radial_gradient_var` or `Conic_gradient_var` node, matching
  declarations built through those constructors (#654)
- A literal `caret:auto` reads as the exported `caret.Auto` node, and literal
  `aspect-ratio` values use the exported `Ratio` and `Auto_ratio` nodes, so
  constructed and parsed declarations compare and hash equally (#652, #653)
- Percentage tokens in `color-mix()` variable fallbacks and `font-size` retain
  their typed percentage nodes, CSS tokenizing a number followed by `%` as one
  percentage (#651)
- The animation and transition delay longhands read time-valued `round()`,
  `mod()` and `rem()` calls instead of dropping the declaration with an
  internal list-parser diagnostic (#646)
- A declaration whose grammar ends in an optional component is no longer
  dropped over the tail the declaration consumer strips. `font-style: oblique
  !important`, `rotate: 45deg !important` and `text-box: none;` read the `;`
  and the `!important` as part of the value and went missing with nothing but
  a warning (#644)
- An empty value is no longer a declaration. `border:`, its per-side and
  logical variants and `column-rule:` read as `border: none`, and `outline:`
  printed a value no parser reads back; both are now dropped with a warning
  (#640)
- `display: flow-root list-item` and `display: flow list-item` are read as the
  values they are, CSS Display 3 sec. 2 ordering none of the three components,
  while `display: list-item table` and a repeated `list-item` are rejected
  (#641)
- `outline-width` and the width slot of the `outline` shorthand read a
  `<length>` where CSS UI 4 sec. 3.2 gives them a `<line-width>`, so
  `outline: thin solid red` was dropped as invalid (#633)
- `stroke-width` reads a bare number, so `stroke-width: 1.5` round-trips
  instead of being printed and then refused: SVG 2 sec. 13.5.3 gives it
  `<length-percentage> | <number>` (#579)
- `stroke-miterlimit` takes a value between 0 and 1. SVG 2 makes only a
  negative value illegal, having dropped SVG 1.1's "at least 1" rule (#334)
- A selector using the column combinator, such as `svg||td`, is read and
  written back instead of dropped: the namespace read claimed the first bar of
  the `||` that CSS Selectors 4 sec. 15.2 defines (#572)
- A rule whose selector chains one pseudo-element onto another, such as
  `::before::marker` or `::part(label)::before`, is read and written back
  instead of dropped, CSS Selectors 4 sec. 3.6.4 (ED) making such a chain valid
  where another specification defines the sub-pseudo-element (#553)
- A rule whose selector puts a combinator after a pseudo-element, such as
  `.a::before .b`, is reported and dropped instead of written back: CSS
  Selectors 4 sec. 3.6.5 (ED) makes it invalid unless the pseudo-element has
  internal structure, and none that ships has (#552)
- A rule nested under a pseudo-element parent, such as `.b` in
  `.a::before{.b{color:red}}`, is dropped instead of flattened to
  `.a::before .b`, which the same section makes invalid and every engine
  matches nothing with (#559)
- Bare `::cue`, `::cue-region` and the scrollbar parts past
  `::-webkit-scrollbar` are read as the pseudo-elements they are, so
  `::cue::before` and `.a::-webkit-scrollbar-thumb .b` are dropped the way
  Chrome, WebKit and Lightning CSS drop them (#556)
- Which pseudo-classes may follow a pseudo-element is read per pseudo-element,
  so `::before:hover` and `::selection:hover` stop parsing while
  `::file-selector-button:hover`, `::part(p):hover` and `::cue:hover` keep
  parsing. `::before:is(.b)` parses and matches nothing (#430)
- `::target-text`, `::spelling-error`, `::grammar-error` and the framework-only
  `::deep` family obey the compound and `:has()` rules the other
  pseudo-elements already did (#418)
- `:not()` rejects a pseudo-element in its argument, as `:has()` already did,
  so `.a:not(::before)` no longer parses and prints back as `.a:not(:before)`,
  which browsers drop the whole rule over (#426)
- The eleven pseudo-classes WebKit's scrollbar parts report their state through
  are read as pseudo-classes, so `::-webkit-scrollbar:vertical` and
  `::-webkit-scrollbar-thumb:window-inactive` keep their rule (#441)
- `position-area` takes the logical `start`/`end` keywords with their `self-`
  and `span-` forms, which css-anchor-position-1 sec. 3.1.2 gives a branch of
  the grammar and browsers lay out, where cascade dropped the declaration
  (#478, #485)
- `position-area` rejects two keywords from different branches of its grammar,
  such as `left block-start`. Cascade checked only that they named different
  axes, so it accepted 1120 ordered pairs that the spec and Chrome 151 both
  reject (#495)
- A media or container size feature takes a unitless zero, where
  `@media (min-width: 0)` was rejected and the at-rule went down with the
  condition, taking every rule inside it. The allowance is for a zero
  `<length>`, so `(min-resolution: 0)` stays invalid (#427)
- `@-moz-document` reads all five of its URL-matching functions. Only
  `url-prefix()` had a grammar, so `url()`, `domain()`, `media-document()` and
  `regexp()` took the at-rule down with every rule inside it (#461)
- `border-inline`, `border-inline-start`, `border-inline-end`,
  `border-block-start` and `border-block-end` keep their value, where none of
  the five had a value reader and a file holding nothing else exited 1 (#456)
- A descending `@font-face` range such as `font-weight: 700 400` parses without
  a warning, the user agent swapping the endpoints for font matching. Only
  `unicode-range` keeps an ordering rule (#335)
- An `@font-face` descriptor whose value holds a `var()` is dropped with a
  warning, and `~strict:true` rejects it: `var()` substitutes in property
  values only. `src` and `unicode-range` keep theirs, since `Css.inline_vars`
  resolves those at build time (#322)
- A valid `@font-palette-values` rule no longer warns, so `cascade apply`
  accepts it. CSS Fonts 4 sec. 9.2.2 defaults a missing `base-palette` to 0,
  and makes `font-family` the mandatory descriptor, whose absence warns in its
  place (#551)
- `clip-path`, `shape-outside` and `object-view-box` stop taking an
  intrinsic-sizing or CSS-wide keyword as a corner radius in the
  `round <'border-radius'>` suffix of a basic shape, which browsers drop anyway
  (#417)
- A `var()` in a `page-break-before`, `page-break-after` or `page-break-inside`
  declaration is read as the property it names, and a parse error in one is
  reported against the property the author wrote rather than the `break-*`
  property it minifies to (#511, #518)
- `@supports (--x\3b y: red)` is read instead of the whole rule being dropped,
  where the reader demanded a property name that re-tokenizes from its own
  bytes (#437)
- A `@layer` name is read as the identifiers it is made of, so `@layer a\2e b`,
  the layer named `a.b`, is no longer the same value as `@layer a.b`, the
  sublayer `b` of `a`. Both printed `@layer a.b`, and minification merged the
  two blocks, moving declarations into a layer the input never wrote them in
  (#442)
- Every at-rule that nests inside a style rule reads its body as nested rules.
  `.a { @starting-style { color: blue } }` kept the wrapper and dropped the
  declaration, `@-moz-document`, `@when` and `@else` lost theirs the same way,
  and `.a { @layer n { color: red } }` read its body as a selector list
  (#374, #384)
- An at-rule with no style rule in its body is invalid inside a style rule and
  dropped with a warning `~strict:true` turns into an error, `@font-face`,
  `@keyframes`, `@property`, `@page` and seven more included. The discard ends
  at the at-rule, where `.a { @import url(x) { } color: red }` lost
  `color: red` (#388)
- A declaration written after a nested rule keeps its place instead of moving
  to the top of the block, so
  `.a { @supports (color: red) { color: blue } color: green }` now computes
  green as browsers do, where cascade printed a sheet that computed blue (#380)
- A bad item inside a block is dropped on its own rather than taking the block
  and the stylesheet holding it. Each of these parsed to nothing:
    - an invalid declaration, a stray `;`, or a nested rule whose prelude
      starts with an identifier, inside a nested at-rule (#392)
    - a descriptor a `@page` body or a page margin box rejects. An invalid
      margin at-rule is discarded to the end of its block rather than to the
      next `;`, and `@page { @top-center { .a { b: c } } }` no longer loops
      forever (#398)
    - a descriptor an `@property` body rejects, and a stray `;` between two
      valid ones (#399)
    - a rule a grouping at-rule's block rejects, now discarded to the end of
      that rule rather than to the first block of any kind in its prelude, so
      a bad selector no longer costs a second warning (#402)
    - a descriptor a `@counter-style`, `@font-palette-values` or
      `@view-transition` body rejects, and a feature block an
      `@font-feature-values` body rejects (#419)
    - an at-rule inside a `@keyframes` body (#420)
- Tokens left after a descriptor's value, such as a trailing `!important`,
  invalidate the declaration they follow rather than the leftover alone
  (#399, #419)
- A `@page` body and a page margin box keep any property they are given, so
  `@page { color: red }` and `@page { @top-center { display: block } }` are
  read where a seven-name allowlist rejected them. A value the property's
  grammar rejects is still rejected (#403)
- A duplicate descriptor in a `@page` body or a page margin box keeps the
  important declaration rather than the one written last (#404)
- `@page { @top-center { } }` no longer takes its `@page` with it: an empty
  margin box is elided on output, as an empty style rule already was (#405)
- An at-rule cascade has no handler for reaches the output with its block
  intact, where `cascade fmt` deleted it and every rule inside it without
  saying so. Discarding one is the user agent's step, and
  `Optimize.drop_unknown_at_rules` serves a caller writing for a browser
  (#469, #483)
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
  redundant ones such as `calc(100% - (10px))` included; `--minify` removes
  those instead, and a precedence-sensitive group such as
  `calc((1px - var(--a)) * 3)` survives both (#721)
- An ident that needs an escape to read back as one keeps it. `.x{--a:-\34 }`
  printed `.x{--a:-4}`, a number rather than the ident `-4`, and
  `@media (-\34 :1)` lost its feature name the same way (#598)
- An unknown media type, media feature name or media identifier value keeps the
  escapes needed to read it back as one identifier. `@media (width\ \>\=\
  10px)` printed `@media (width >= 10px)`, turning an unknown boolean feature
  into a real size range (#526)
- A `<custom-ident>` or `<dashed-ident>` an at-rule prelude or a declaration
  value names is printed with the escapes needed to read it back as the same
  name: `@layer a\3b b` printed `@layer a;b`, two statements naming a layer the
  input never had. `@counter-style`, `@position-try`, `@container` and
  `@import layer()` all take the escaping (#436)
- A custom-property name written with an escape is printed with its escapes.
  `:root{--x\3b y:red}` printed `:root{--x;y:red}`, which cascade's own reader
  splits into two declarations, and a reference to it printed `var(--x}y)`,
  which closes the rule around it (#435)
- A compound operand of `not`, `and` or `or` in a `@media` condition keeps its
  parentheses: `not ((min-width:1px) or (max-width:2px))` printed as
  `not (min-width:1px)or (max-width:2px)`, which browsers and cascade's own
  reader reject, losing the whole block (#319)
- Unwrapping a `@supports` nested in a style rule keeps the `;` before the
  sibling that follows, where
  `.a{@supports (color:red){color:blue}@supports (color:red){color:green}}`
  printed `.a{color:#00fcolor:green}` (#370)
- A NaN-valued number prints as `calc(NaN)` and a NaN-valued dimension as
  `calc(NaN * 1unit)`. A bare `NaN` is not a CSS token, so
  `width: calc(sqrt(-1) * 1px)` printed `width: NaNpx`, which browsers drop
  (#425)
- `Css.to_string` renders the sheet once rather than twice, where it sized its
  buffer exactly by first running the sheet through a counter (#479)

### Minification

- `--minify` keeps the authored time unit of a `var()` fallback nested inside a
  `calc()`, as it already did for the operand beside it. The two spellings
  printed alike and stopped factoring (#803)
- `--minify` keeps a vendor-prefixed gradient written with the `background`,
  `mask`, `border-image` or `mask-border` shorthand, as it already did for the
  longhand. Canonical diff had reported two sheets differing only there as
  identical (#782)
- Invalid math results in `shape-margin` and `offset-distance` are dropped like
  those in every other `<length-percentage>` property (#766)
- Flattening a rule nested under a selector list wraps the parent in `:is()`,
  where `.a,.b{&:hover{color:red}}` printed `.a,.b:hover` and bound `:hover` to
  the last branch, CSS Nesting 1 sec. 4 (ED) reading `&` as the group (#800)
- `--flatten-nesting` leaves the optimised stylesheet flat even when later
  regrouping can shorten adjacent selectors by synthesizing nesting (#759)
- Canonical diff compares authored nesting through its flattened selector
  expansion, so equivalent nested and flat stylesheets do not leave residuals
  (#760)
- Computed-value evaluation resolves direct `inherit`, `initial`, `unset`,
  `revert` and `revert-layer` values for every typed property, including value
  shapes without a property-specific optimizer (#764)
- Property inheritance is one exhaustive classification keyed by the typed
  property identity. `cascade apply --minimal` now drops inherited
  `writing-mode` restatements, and computed-value `unset` resolution cannot
  drift onto a different table (#763)
- Default `--minify` evaluates all-static unitless `line-height:calc()`
  arithmetic to its six-significant-figure output budget, so `calc(28/18)`
  becomes `1.55556`. `--lossless` keeps repeating quotients symbolic, and
  canonical diff follows the same precision mode (#756)
- `--minify` converts modern colour operands to floating-point sRGB before
  resolving an `in srgb` `color-mix()`, so channel bytes are rounded once after
  interpolation instead of once per operand and again afterward (#755)
- `--minify` reaches a stable result in one invocation on large stylesheets:
  synthesized shorthands and vendor transition/animation aliases normalize
  before comparison, factoring settles both transfer-size alternatives and
  retries once when settling changes its graph, and selector-branch factoring
  preserves source order after earlier rewrites (#750)
- Default minification adds the WebKit fallbacks Safari 16.4 and Chrome 111
  need for `user-select`, `backdrop-filter`, `hyphens`, `mask` and its
  compatible layer longhands, including matching `@supports` tests. An
  authored prefixed declaration remains authoritative (#751, #758)
- Default minification also adds `-webkit-text-decoration-color` beside a
  `text-decoration-color` whose value is not settled at parse time, which
  Safari and iOS Safari read under both names through 26.1. A value that reads
  as a colour keeps the standard property alone (#797)
- The README states the default minifier's evergreen target, colour tolerance,
  and CSSOM-visible normalisation beside its first example, and describes
  `--lossless --enforce-spec` as conservative rather than source-exact (#743)
- Lossless optimisation keeps otherwise-independent declarations in authored
  order, so stylesheet text and CSSOM enumeration no longer change solely for
  gzip alignment (#742)
- Numeric tokens in custom-property and unknown-property declaration streams
  use their shortest exact spelling, so `--x: 1.0px` becomes `--x:1px` without
  changing adjacent token boundaries. Declaration feature queries keep the
  author's spelling, being a question asked of another parser (#719)
- `--minify` writes no separator into a pair of tokens the source held side by
  side. `--t: x 1px+2px` came out as `--t:x 1px +2px`, handing every `var()`
  that read it a whitespace token the author never wrote (#709)
- `--minify` keeps the space after a function-closing parenthesis in a typed
  position or colour value, so `background-position: var(--x) 20%` minifies to
  itself and matches the same value written into a custom property
  (#700, #703, #722)
- `--minify` keeps the whitespace CSS Values 4 requires on both sides of a math
  function's `+` and `-` when it prints a custom property or an unknown
  property. `--w: calc(100% - 10px)` came out as `--w:calc(100%- 10px)`, which
  browsers discard, taking every `var(--w)` that read it with them (#697)
- `--minify` treats a square or curly block as ordinary tokens rather than as
  math context, only `()` grouping a math expression, so a bracketed run in a
  custom property folds its separators like the component-value printer (#720)
- `--minify` drops the space at a `%` boundary between the values of `margin`,
  `padding`, `inset`, `border-radius`, `border-color`, their logical forms and
  a position value, so `margin:10% 0` prints as `margin:10%0`. CSS Syntax 3
  sec. 4.3.3 consumes the `%` into the percentage token, so whatever follows
  starts a fresh token; a value ending in a unit keeps its space, since
  `10px 0` would re-tokenise as `10px0` (#614)
- `--minify` drops that space at a `)` boundary as well in the box shorthands
  and in the `inset()` of `clip-path` and `object-view-box`, so
  `padding:var(--x) 0` prints as `padding:var(--x)0`. A typed position or
  colour value keeps it (#619, #623)
- `Css.to_string ~minify:true` keeps choosing the shorter exact spelling for
  constructed millisecond durations and degree hues without running the AST
  optimisation phase (#678)
- `calc()` keeps type-bearing zero terms with compatible dimensions and rejects
  incompatible result types at property boundaries, where
  `width:calc(1px + 0)` used to survive (#676, #731)
- `--minify` folds a value's spelling before two rules are compared, so rules
  that wrote one declaration two ways factor into one: a component left at its
  longhand's initial in the `border`, `column-rule`, `outline`, `list-style`,
  `text-decoration`, `transition` and `font` shorthands, a repeated
  `font-family` entry, a two-value `display` beside its legacy keyword, and a
  box shorthand whose sides repeat (#635, #636, #637, #639, #640, #641)
- Box shorthands keep authored component counts across top-level substitution
  functions, preserving computed-value validity and side assignment (#736)
- `--minify` canonicalises logical minimum sizes, duration units, stepped
  functions, hue-angle units, constructed `transform-origin` positions and
  repeated sides in the `scroll-margin` and `scroll-padding` shorthands before
  declaration hashes are compared, so equivalent rules factor under every
  spelling (#647, #650, #672)
- `--minify` splits a whole-rule `:is(a, b)` whose arguments share one
  specificity into the selector list `a, b` before rules are compared, so it
  factors with a rule that wrote the list out. A whole-rule `:is(a)` or
  `:is(*)` loses its wrapper there too (#655)
- `--minify` folds a same-unit `calc()` in `font-size`, so `calc(1px + 1px)`
  prints as `2px` and merges with a rule that wrote `2px` (#639)
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
- `--minify` keeps a lone `background` position value and both `<box>` values
  of a layer that disagree: `background: url(a.png) 0` came back top-aligned,
  and `background: red content-box border-box` came back painted over the
  content box alone (#640)
- `--minify` folds the width slot of the `border` shorthands and of
  `column-rule`, so `border: 0px solid red` prints as `border:0 solid red` and
  `border: calc(1px + 1px) solid red` as `border:2px solid red`; #633 gave
  `outline` the same fold (#634)
- `--minify` folds `sin()` through `atan2()` inside a custom-property stream,
  matching `calc()`'s family, where the table gating the fold and the one
  exported as `Properties.is_math_function` had drifted apart (#626)
- `--minify` merges two rules whose `border-width` differ only in an unreduced
  `min()`/`max()`/`clamp()`, such as `min(2cqi,3cqi)` and `2cqi`, where a
  second pass used to be needed before they merged (#624)
- `--minify` folds a `color-mix()` in a wide-gamut rectangular space, and one
  in `srgb` whose result leaves that gamut; both used to reach the browser
  unfolded. The result keeps the space that was named (#618)
- `--minify` merges rules whose colours differ only in how a hex was spelled,
  where `#FFF`, `#fff` and `#ffffff` reached the merge pass as three
  declarations and stayed three rules (#597)
- `--minify` keeps `.c:not(:enabled)` instead of rewriting it to
  `.c:disabled`: an element outside a state pseudo-class pair's own set matches
  neither half of it, so `<p class=c>` matched the rule before the rewrite and
  not after. The rewrite now needs a compound that proves its subject carries
  the state (#596)
- `--minify --enforce-spec` keeps the author's `:not(:dir(ltr))` instead of
  shortening it to `:dir(rtl)`: only a host document like HTML makes `ltr` and
  `rtl` a partition, and that is one of the facts `--enforce-spec` drops (#593)
- `--minify` merges a run of adjacent `@starting-style` blocks, which take no
  prelude, so the run holds the same starting styles as one block over their
  concatenation (#592)
- `--minify` simplifies a nested `@supports` condition against the conditions
  enclosing it: under `@supports (A)`, an inner `@supports (A)` loses its
  guard, an inner `@supports (A) and (B)` narrows to `(B)`, and an inner
  `@supports (not (A))` is dead code (#585)
- `Css.optimize` writes an unrecognised at-rule's opaque body back as the token
  stream it read, so `@foo{ .a { color: red } }` minifies to
  `@foo{.a{color:red}}`. `~lossless:true` leaves the body as authored (#560)
- `--minify` merges `@media` and `@container` blocks by query structure rather
  than serialised text, so `(min-width: 10px)` and `(width >= 10px)` are one
  bound, while an escaped `@media screen\ and\ \(min-width\:\ 10px\)` names an
  unknown media type that never matches and no longer merges into the real
  query. `Css.Container.equal` decides that, and `Css.Container.normalize`
  exposes the normal form it compares (#516, #519)
- `--minify` merges two rules that declare the same NaN, whichever way each
  spelled it: `opacity: calc(NaN)` and `opacity: calc(infinity - infinity)`
  are one declaration (#471, #482)
- `--minify` merges adjacent `@container` blocks whose `style()` conditions are
  written the same way, where two byte-identical `style(--x: 1)` queries never
  compared equal and their blocks stayed separate (#465)
- `--minify` shortens an `src:` declaration outside `@font-face` the way it
  already shortened the descriptor: `url("a.woff2")` drops its quotes and
  `local("Arial")` becomes `local(Arial)`. The declaration route spelled a
  multi-word `format("...")` unquoted, which no browser reads back as that
  format (#470)
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
- A declaration whose value is spec-invalid is discarded inside a `@keyframes`
  frame, as it already was in a style rule (#341)
- Under `--scope=stylesheet` a `position-try-fallbacks` name with no
  `@position-try` rule is dropped inside a `@keyframes` frame, as it already
  was in a style rule (#372)
- A custom property registered by `@property` is typed wherever it is declared,
  so the same value minifies the same way inside `@keyframes`, `@position-try`
  and `@supports-condition`, where a `<color>` registration left
  `box-shadow: 0 0 var(--ring)` keeping the reference in the blur radius
  (#337, #349)
- `--flatten-nesting` treats `@-moz-document` as the grouping at-rule it is:
  nesting inside one flattens, and a rule wrapping one keeps its selector
  instead of the at-rule being emitted at top level under no parent (#344)
- A rule whose declarations a later rule all rewrites is dropped only when it
  nests nothing, where
  `.a { all: unset; @media (min-width: 1px) { width: 1px } } .a { all: initial }`
  lost its nested block along with the rule (#376)
- A declaration written after a nested rule rejoins the declarations before it
  when nothing it crosses writes the same property at the same importance, so
  `.a { & b { width: 1px } color: red }` minifies to
  `.a{color:red;b{width:1px}}`. One that clashes keeps its place, and such
  declarations are deduplicated like any other list (#383, #386)
- Merging same-selector rules keeps a later declaration behind a nested
  conditional group that writes the same property, where the safety check saw a
  nested style rule only and handed the conditional the win (#352)
- That merge reads the source order rather than the merged order, and compares
  a nested block with a later declaration by the longhands each writes rather
  than by property name: a nested `margin: 2rem` and a later `margin-top: 1rem`
  were read as disjoint (#364)
- Merging two same-condition `@media` blocks no longer moves the first past a
  rule whose shorthand writes a longhand the block also writes, nor past a
  declaration written after a nested rule. A `background-color` pair merged
  across `background:blue` into a sheet browsers compute blue for where the
  source computes green (#414, #415)
- A declaration writing `column-rule-color` keeps its place against one writing
  `column-rule`. The shorthand resets the colour, but the longhand was not
  counted among what it resets, so two rules writing the colour merged across
  `column-rule: 1px solid red` (#447)
- A declaration writing one of the four flow-relative border style longhands
  keeps its place against one writing `border` or `border-style`:
  `.a{border:1px solid red;border-block-end-style:dashed}` reordered into a
  rule computing `border-bottom-style: solid` where the source says `dashed`
  (#453)
- Two rules stay apart across a rule whose property name cascade cannot resolve
  to a set of longhands:
  `.a{word-wrap:break-word}.b{overflow-wrap:normal}.c{word-wrap:break-word}`
  minified to a sheet computing `overflow-wrap: normal` where the source
  computes `break-word` (#452)
- A vendor-prefixed declaration keeps its place against the property it
  aliases, each having counted as a property of its own:
  `.a{-webkit-transform:none}.b{transform:rotate(45deg)}.c{-webkit-transform:none}`
  minified to a sheet computing a rotation where the source computes `none`
  (#454)
- An authored value keeps every digit. Any dimension was rounded to six
  significant figures, so `.4285714em` came out as `.428571em`, which is
  `5.99999px` rather than `6px` at a `14px` font size, and a `<number>`
  registration lost its digits the same way. A value cascade computes itself
  still pays that budget (#350, #354)
- A computed dimension past a million units keeps every digit, where
  `calc(1in + 999999999px)` came out as `1000000000px`, 95px short. A narrower
  value still pays the budget: `calc(1cm + 1px)` is `38.7953px` (#367)
- A math function inside `calc()` keeps its unit, where `calc(hypot(1px, 1px))`
  came out as `1.41421356`, a declaration browsers and cascade's own reader
  both drop (#362)
- A `font-family` name keeps the spelling the author wrote, where a table of
  known names re-emitted its own: `font-family: open-sans` became
  `"Open Sans"` and `font-family: ny` became `"New York"`, naming a different
  family, in the `@font-face` descriptor that *defines* the name as much as in
  the properties that reference it (#387)
- A `font-family` name that cannot be spelled as an identifier keeps its
  quotes, whether for how it starts or for a reserved word it holds: `"2Brand"`
  and `"inherit test"` came out bare, and a browser dropped the whole
  declaration, taking the rest of the stack (#390, #401)
- `column-rule-color` and `-webkit-text-stroke-color` are typed as colours and
  `-webkit-text-fill-color` is minified as one, so a colour-valued property
  minifies to the same spelling whatever its name:
  `lab(1.90334 0.278696 -5.48866)` and `rgb(3, 7, 18)` both print `#030712`
  (#447)
- A single-argument `:is()` and a double `:not()` keep their wrapper around a
  type or universal selector, where removing it fused the two names:
  `.a:is(code)` printed `.acode`, which browsers drop (#377)
- A single-argument `:is()` keeps its wrapper after a pseudo-element that
  cannot take its argument, so `.a::before:is(.b)` minifies to
  `.a:before:is(.b)` rather than the `.a:before.b` browsers drop. It still
  unwraps where the compound can hold the argument (#431)
- A vendor prefix is dropped only when its unprefixed twin is Baseline "widely
  available": `-webkit-backdrop-filter`, `-webkit-user-select`,
  `-webkit-text-size-adjust` and `-webkit-print-color-adjust` were dropped
  against a twin no shipping Safari understands (#325)
- A feature query on a vendor-prefixed property keeps its guard: the
  web-features dataset behind the Baseline facts tracks unprefixed features
  only, so treating the query as true turned Tailwind's legacy-browser reset on
  in Chrome, where `@supports (-webkit-hyphens: none)` is false (#378)
- `@media not all and (X)` minifies to the Level 4 `@media not (X)`, `all`
  matching every device. `--enforce-spec` keeps both Level 3 spellings (#323)
- `--minify` keeps one declaration where a rule writes both a `page-break-*`
  property and its `break-*` twin, CSS Fragmentation 3 sec. 3.4 making the pair
  one property (#547)
- `min-inline-size: initial` and `min-block-size: initial` minify to `auto`,
  the initial value they share with `min-width` and `min-height`, rather than
  to a zero that drops a flex item's automatic minimum size (#675, #681)
- `--minify` and `cascade diff` are faster on a large stylesheet, for
  byte-identical output: the slowest corpus stylesheet took about 80s of CPU
  and now takes under 2s, and the 8,000-rule same-selector benchmark is about
  108x faster (#413, #422, #424, #468, #493, #507, #542, #664)
- `--minify` allocates less and no longer allocates quadratically on a long run
  of rules sharing one selector, one body or one declaration. A 4,000-statement
  sheet's distant-`@media` merge falls from 24.4M words to 0.4M, and the
  504-file corpus allocates a twentieth less
  (#480, #486, #487, #502, #505, #517, #519, #523, #543, #566)
- Nested group-rule merges optimise only the newly joined statement list
  instead of walking already-optimised child blocks again at every ancestor.
  Doubling the depth of a repeated `@media` merge now doubles optimizer work
  rather than quadrupling it (#746)

### Custom properties

- `--minify` keeps the quotes on a `<string>` written to a custom property
  whose `@property` syntax accepts only an ident sequence: the string matches
  no arm of that registration, so it computes to the initial value, where
  unquoted it computed the name (#704)
- `--minify` keeps the space between the repetitions of a `@property`
  `<type>+` initial value, CSS Values 4 sec. 2.3 making it the separator:
  dropped, `10px 20px` read back as one `px20px` dimension (#626)
- `Css.Variables.read_reference_body` reads a `var()` argument list into a
  typed variable handle from a cursor already positioned at the arguments;
  `read_reference_body_as_string` returns the name and fallback as text, for a
  caller with no value type to pick a typed fallback reader from (#630, #642)
- `Css.Variables.typed_custom_property` writes a custom-property declaration
  from a value already typed by a `@property` registration's syntax, where
  `Declaration.custom_property` takes a plain string (#626)
- A `var()` in an `@font-face` descriptor is resolved by `Css.inline_vars`
  rather than dropped at parse time, covering the metric overrides,
  `font-family`, `font-variant`, `size-adjust`, `font-tech`, a `font-stretch`
  endpoint and the style, weight, display and settings descriptors. One whose
  typed value has no `Var` arm is still dropped with a warning, as a browser
  does (#571, #573, #575, #577)
- `Css.inline_vars` resolves every `@page` descriptor under one unit policy,
  where `margin-top` alone kept the authored unit and one block answered `1cm`
  for it and `37.79527559px` for the `margin-left` beside it (#555)
- A cascade layer and caller metadata on a custom property survive
  `Css.inline_vars`: both belong to the declaration rather than to its value,
  and the rewrites that read a custom value back rebuilt the declaration from
  the value alone (#520)
- A `page-break-*` declaration survives `Css.inline_vars` as itself, where
  substituting a `var()` rebuilt it from its minified name and
  `page-break-inside` came back as `break-inside` with a value that property
  does not accept (#506)
- `Css.inline_vars` preserves runtime-marked `var()` references, typed
  fallbacks simplified through scalar values or shorthands included, instead of
  replacing browser-time override points with compile-time defaults (#315)
- `Css.inline_vars` resolves a custom property defined across cascade layers
  against the order every `@layer` in the sheet gives it, counting the ones a
  rule nests and the ones a conditional group holds. A layer first named inside
  a conditional group has no decidable order and is left standing (#357)
- `Css.inline_vars` unwraps an `@layer` only where the layer order and document
  order already pick the same winner for every property two layers write, where
  unwrapping handed the decision back to specificity and a sheet came out
  rendering a different value with no custom property involved (#371)
- `Css.inline_vars` unwraps an `@layer` and drops an `@property` registration
  written inside a rule, as it already did at top level, so a sheet using CSS
  nesting no longer comes back half cleaned (#373)
- `Css.inline_vars` keeps the `@property` registration of a custom property it
  leaves live. Every registration was dropped, so a property it could not
  inline lost the `initial-value` its references fall back on and the
  `inherits: false` that stops it inheriting, repainting the page (#416)
- `Css.inline_vars` counts a `var()` in a `@keyframes` frame, `@page` and its
  margin boxes, `@position-try`, a `@supports-condition` body or a `style()`
  container query as a reference, so pruning no longer deletes a binding those
  at-rules still use and leaves a block that never matches. An overridden
  variable is reported through `~warn` (#341, #342, #423)
- `Css.inline_vars` stays linear in at-rule nesting depth and no longer costs a
  square in the variable count: a 12,800-variable sheet goes from 2.4s to
  0.11s (#481, #568, #569)
- `Css.resolve_theme` accounts for the declarations `@keyframes`, `@page`,
  `@position-try` and `@supports-condition` carry: a name referenced only from
  inside one keeps its theme binding, a name whose only declaration sits in
  `@keyframes` keeps the root binding, and an unselected theme guard is dropped
  rather than printed as a declaration (#317, #324, #327)
- `Css.resolve_theme` builds each `theme_defaults` binding with
  `parse_custom_property` instead of synthesising `:root { ... }` text and
  reparsing it, where one value could add a rule to the output or take every
  other theme default down with it (#421)
- A custom-property name that needs escaping binds instead of being refused, so
  a `theme_defaults` answer for `x;y` emits `:root{--x\;y:red}` (#439)
- `Css.custom_props` reports a name declared inside `@scope`,
  `@starting-style`, `@-moz-document`, `@when`, `@else` or a bare nesting
  block, as it already did for `@media` and `@supports` (#375)

### Canonical diff

- Canonical diff treats identical `-webkit-text-decoration-color` and
  `text-decoration-color` declarations as Cascade's configured redundant
  alias. A differing fallback or prefixed-only declaration remains distinct
  (#777)
- Canonical diff lets a declaration reading `var()` cross a conditional write
  to that custom property when the rules write disjoint cascade slots. A
  competing write to the declaration's own property remains order-sensitive
  (#776)
- Canonical diff no longer reports a reorder when equal `@supports` blocks are
  hoisted together across a declaration the later block shadows whenever their
  condition holds. A crossing that changes the winner stays distinct (#775)
- Canonical numeric arithmetic has an explicit precision contract:
  `calc(28/14)` compares equal to `2`; default mode equates `calc(28/18)` with
  the minifier's `1.55556`, while `--lossless` keeps them distinct (#753, #756)
- `:is(a, b)` and the selector list `a, b` compare equal when the arguments
  share one specificity, which is the split-against-grouped selector list the
  mode promises to equate (#655)
- The canonical projection keeps structurally distinct `@container` conditions
  in separate cascade slots even when their minified text is identical, so an
  escaped unknown feature no longer merges into the real
  `(inline-size >= 10px)` range and moves its declarations under a condition
  the input never gave them (#529)
- The canonical projection no longer deletes content that only a
  browser-support assumption makes dead, which had it report no difference
  between sheets that render differently: a fallback under a baseline-true
  `@supports`, a load-bearing vendor-prefixed declaration, an `@import
  supports()` guard. The respellings gated with those still compare equal
  (#576)
- A redundant `@layer` order pin no longer reads as a difference, so
  `@layer a;@layer a{...}` and `@layer a{...}` compare equal. A pin that fixes
  the order, or one over a position the projection cannot read, is kept (#475)
- The canonical form is taken inside every at-rule with a block, so the verdict
  no longer depends on which at-rule encloses the rule: three foldings applied
  inside `@layer` and `@media` but not inside `@scope`, `@starting-style`,
  `@-moz-document`, `@when` or `@else` (#393)
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
  stack; without one, `--font: "Noto Color Emoji"` and
  `--font: Noto Color Emoji` stay distinct diff keys (#696, #705)
- `Css_compare.equivalent_value ~property` spells the property name the way the
  printer spells it before comparing two values under it, where a name carrying
  an escaped `}` closed the rule and made any two values under it compare
  equal (#440)
- `cascade diff` no longer aborts on a reordered selector holding the same rule
  index on both sides, where the assertion that met such a move cost the whole
  buffered report; the move is named without a coordinate (#582)
- `--diff=tree` compares a value on its minified spelling, so insignificant
  whitespace stops reading as a change: `16 / 9` matches `16/9`, and
  `padding: 0.50px` matches `padding: .5px`. The space required around a math
  `+` or `-`, and the space beside a `var()`, still separate two values (#702)
- `--diff=tree` prints a changed declaration the way its own file spells it,
  where the value was read off the comparison key and a quoted multi-word
  family name was reported unquoted on both sides (#702)
- `--diff=tree` prints the body of an added or removed rule as declarations,
  with the separator and the `!important` flag, where a rule gaining
  `color: red !important` read like one gaining `color: red` (#706)
- `--diff=tree` pairs repeated occurrences of one selector by their declaration
  properties, with source order breaking ties, so a compatibility block no
  longer makes declarations present on both sides read as added or removed
  (#752)
- `--diff=tree` tracks each repeated conditional block in source order, so
  moving a later block past a rule is reported even when an earlier block has
  the same condition (#779)
- Canonical diff omits generated-tree positions from reordered rules and
  containers, where those numbers identified neither input and the rule paired
  with one could be an unrelated container (#780)
- Canonical diff derives reorder findings from the parsed inputs instead of the
  independently ordered projection, where one declaration change could make
  unchanged rules read as reordered (#781)

### Library

- `Reader.pp_parse_error` puts its caret immediately below the line it marks,
  where a multi-line context window printed the caret below the whole window
  at a column measured from its start (#793)
- `cascade` drops its `uutf` dependency for the stdlib UTF-8 decoder. A parse
  error's column now counts one replacement character per maximal subpart of
  an ill-formed sequence, as a browser does, where the previous decoder
  counted the whole run as one (#788)
- The library no longer links `unix`. Timing a factoring iteration for
  `--profile` was its only use of it, and `mtime` reads the monotonic clock the
  measurement wants and ships a js_of_ocaml implementation, so embedding
  cascade in a browser no longer means linking an operating system to time a
  loop (#609)
- `Css.Resolve` answers the selectors Selectors 4 settles from the element tree
  a `NODE` supplies: `:nth-child()` and `:nth-last-child()` with or without
  `of S`, the typed `:nth-of-type()` family, the relational `:has()`, the `i`
  and `s` attribute case flags, and `:scope`. Each read `Unsupported` before,
  so `Css.Apply` kept such a rule rather than projecting it. A namespace and
  `:lang()` stay unsupported (#607)
- `Resolve.prepare` and `Resolve.Make.resolve_prepared` split the sheet-only
  work out of `resolve`, so a caller walking a document flattens the nesting
  and buckets the rules by layer once rather than per node. Ten queries against
  a 2,000-rule sheet allocate 4.6x less (#567)
- `Css.unknown_at_rule` builds an at-rule cascade has no grammar for, such as
  one a tool of the caller's own defines, where emitting one meant assembling a
  sheet as text and reading it back. The constructor refuses one that ends the
  at-rule early (#600)
- `Css.Color_space.gamut_mapped_srgb_of_oklch` and `Css.Values.gamut_map_color`
  name the sRGB colour to write for an OKLCh colour sRGB cannot hold, reducing
  its chroma at constant lightness and hue per CSS Color 4 sec. 14.2. Minify is
  untouched and still keeps the colour the author wrote (#591)
- `Css.equal_statement` and `Css.hash_statement` answer whether two statements
  are the same, and key one in a hash table, without rendering either to CSS
  text: two `@media` blocks that select the same media are one statement
  however their queries are spelled. `Css.Values.hash_color` is that pair for a
  colour, and `Css.Values.with_alpha` sets a colour's alpha (#595)
- The statement-merging passes are callable on their own:
  `Css.Optimize.merge_consecutive_layers`, `merge_named_layers_by_name`,
  `merge_consecutive_media`, `merge_distant_media`,
  `merge_consecutive_supports`, `merge_consecutive_containers` and
  `merge_consecutive_starting_style`, so a caller that runs `Css.optimize`
  behind a flag of its own can collapse the block structure of a sheet it only
  serialises (#592)
- `Css.Declaration.value_of` reads a declaration's value at a property witness,
  the counterpart of `Declaration.property_key`. The value was reachable only
  as text, so a caller telling a `var()` carrier apart from a declared value
  compared printed spellings; `Properties.eq_property` carries the type
  equality the comparison cannot express (#616)
- `Css.Properties.compare_property` and `Css.Declaration.compare_prop_key` are
  a total order on a property identity, `0` exactly where equality holds. The
  table that drops shadowed rules orders its coverage set with it, which takes
  a fifth off that set on a sheet writing many properties under one selector
  (#513)
- `Css.Properties.read_grid_template_tracks` parses the track-list grammar of
  `grid-template-columns` and `grid-template-rows` without accepting the wider
  shorthand forms handled by `read_grid_template` (#717)
- `Cascade.Syntax.is_ident` answers whether a whole string is one CSS ident,
  the check `Cascade.Parser.escape_ident` already made for emission, where a
  per-character scan reads a leading `-` as ident-start and misses `-4` (#626)
- `Css.Stylesheet` traverses a whole block: `fold_statements`,
  `iter_statements`, `fold_declarations`, `iter_declarations`,
  `map_declarations`, and `edit_statements`, which keeps, replaces or drops
  each statement and descends into what survives. The declaration functions
  take `?sites`, so a place added to `declaration_sites` stops every caller
  that made a choice from compiling (#356, #363)
- `Css.Stylesheet.statement_declarations` is the declarations a statement holds
  directly, and with `statement_children` reaches every declaration in a
  stylesheet. `map_statement_children` and `map_statement_declarations` rebuild
  a statement, returning the one they were given when nothing changed
  physically (#317, #337, #355)
- `Css.Stylesheet.at_declaration_site` answers whether a statement holds its
  declarations in one of the places a `declaration_sites` record names, for a
  traversal carrying state down the tree that cannot be a fold (#368)
- `Css.map` and `Css.sort` reach a rule inside `@scope`, `@starting-style`,
  `@-moz-document`, `@when` or `@else`, where a caller rewriting or reordering
  "all rules at all nesting levels" got a sheet with five of them silently
  untouched (#381)
- `Css.layers` and `Css.layer_block` find a layer declared inside a grouping
  at-rule, so a caller asking which layers a sheet declares was given a wrong
  answer rather than a partial one (#382)
- `Css.Stylesheet.layers` is what `Css.layers` calls, so the two no longer give
  different answers to the same question. `Css.Stylesheet.media_queries`,
  `container_queries` and `Css.media_queries` report a query written inside a
  grouping at-rule, and pair each query with every rule below its brace (#389)
- `Css.flatten_nesting` carries the parent selector into an `@-moz-document`
  block, the one grouping at-rule it did not descend into, where the
  declarations came out bare at the top of the block (#384)
- `Cascade_diff.Tree_diff.has_container_added_of_type` and
  `has_container_removed_of_type` look inside a container reported as modified,
  as `count_containers_by_type` already did, where a `@supports` added inside
  an existing `@media` was counted but not found (#395)
- `Cascade.Resolve.Make.resolve` and `Cascade.Resolve.layer_order` document
  every block they leave out, not just conditional groups: `@starting-style`,
  `@scope` and an origin wrapper each carry something the resolver does not
  model (#394)
- `Cascade.Error.to_string` puts the caret under the character that failed and
  prints back a snippet that is valid UTF-8, where the column was a byte count
  and a multibyte class name could open the snippet inside a code point (#472)
- `Cascade.Reader.parse_error` reports the line and column of the failing byte
  from a forward scan and counts its caret in characters, where the backward
  walk counted the line before the error and stopped one column short at end of
  input (#477)

### Testing

- Every normal interop corpus records its pinned upstream, exact regeneration
  command, and authoritative license notice; regenerate rules are `REGEN=1`
  gated and promote declared committed traces. The unlicensed SatCSS website
  corpus moves to opt-in benchmark tooling instead of reading ignored files in
  normal tests (#745)

### CLI tools

- `cascade diff --json` writes the comparison as one JSON document on standard
  output in place of the report, so a harness reads what changed rather than
  parsing prose. The exit status is unchanged (#799)
- `cascade diff` compares a rule's nested body whatever order the two sides
  write its selector list in, and stops calling that reordering a change: a
  comma group is a set, so the body held the only difference (#798)
- `cascade diff` reads either side from standard input when the argument is
  `-`, so the output of a build can be compared without a temporary file. The
  report names that side `<stdin>` (#796)
- `cascade diff` prints a parse warning both inputs raise once, under a label
  naming both files, so the report's warning budget reaches the warnings only
  one side raised (#795)
- `cascade diff --diff=tree` reports two stylesheets that declare different
  `@namespace` URLs as different. It called them identical, though a type
  selector matches in the namespace its sheet declares (#794)
- `cascade diff --limit` bounds how many differences a report prints, and the
  automatic shaping now spends its budget on whole entries rather than cutting
  every entry's body: a wide report named every selector and explained none
  (#792)
- `cascade diff` renders a character with no glyph as an escape, so a
  difference in line endings shows as one and a control byte in a stylesheet
  can no longer drive the reader's terminal (#791)
- A `cascade diff` character-level hunk no longer ends with a context line
  holding one space when the inputs end in a newline: the empty string a
  final newline leaves behind is the terminator, not a line (#790)
- `cascade diff` indexes structural, nesting, and reported-selector matches,
  making a sheet where every rule changes scale near-linearly (#786)
- `cascade diff` keeps trailing context around a difference between short
  lines, where it previously stopped immediately after the caret (#785)
- A parse warning puts its caret under the line it marks, at that line's own
  column. A snippet spanning several lines drew it below the whole window,
  indented from the first line, so it underlined nothing (#789)
- `cascade diff` names a changed `@keyframes` block as the at-rule it is,
  in place of the `@layer @keyframes spin` line it printed (#784)
- `cascade diff` reports the two sizes without a percentage when the first
  file is empty, in place of the `(0.0% diff)` it printed for a pair sharing
  nothing (#783)
- `cascade fmt --import-root DIR` bounds `--inline-imports` filesystem reads to
  the canonical root and its descendants, rejecting both lexical and symlink
  escapes. Omitting it retains unrestricted resolution for trusted CSS (#744)
- CLI help lists each option and exit status once, and `cascade prune --help`
  classifies representative selectors through the resolver instead of naming
  `:nth-child()` as unsupported (#740)
- `cascade prune PAGE.html... STYLE.css` removes the rules a set of HTML
  documents cannot use, and `--dry-run` reports instead, ranking what survives
  by how few elements matched it. A rule goes only when the matcher has a model
  for its selector and every element answers that it does not match, so
  `:hover`, a pseudo-element, an at-rule condition and a statement naming no
  element are all kept. A class a script adds at runtime is in none of the
  documents, so a rule waiting for one is removed (#605)
- `cascade fmt --help` says what `--enforce-spec` gates: the vendor-prefix
  drop, the range grammar for a media or container feature, the `&` prefix on a
  nested selector, the `:dir()` and form-control state negations, the
  percentage spelling of an `oklab` axis, the unquoted multi-word font family,
  and the ident code points the reader accepts (#611)
- `cascade fmt --enforce-spec` can drop a rule with a raw non-ASCII selector
  without `--minify`: the flag also gates the parser's ident range, so the
  "has no effect without --minify" warning was false (#625)
- `cascade fmt --profile` without `--minify` printed an empty
  factoring-fixpoint report, contradicting its own "has no effect without
  --minify" warning; the report is skipped when nothing ran (#628)
- `cascade fmt` exits 1 when parse recovery left no statement at all, where it
  exited 1 when the printed output was empty and the parse had warned. A rule
  survives a declaration the parser could not read, so a build failed over CSS
  the parser used in full (#494)
- `cascade apply` exits 0 for a `<style>` block whose parse kept a statement,
  so a build gating on the exit status passes on valid CSS: the check rendered
  the sheet, and an empty rule prints nothing while losing nothing (#489)
- `cascade apply` reads a `style` attribute as a declaration list in source
  order. The declarations came out reversed, so a longhand beat the shorthand
  it was written after, and a `}` inside the attribute closed the list early
  (#326)
- `cascade apply --minimal` drops an inherited declaration only when it truly
  restates the value the element would inherit. It dropped one whose property a
  user-agent rule declares, uncovering the user-agent link colour and heading
  size; and one whose value resolves against the element it lands on, where
  `div{font-size:2em}div p{font-size:2em}` halved the paragraph (#326, #329)
- `cascade apply --minimal` keeps a restated inherited shorthand when an
  element in between sets one of the longhands it resets, where
  `#p{font:16px serif}#mid{font-weight:bold}#c{font:16px serif}` left `#c`
  bold (#332)
- `cascade apply` empties the `<style>` blocks it projects instead of removing
  them: a `<style>` element is a sibling like any other, so unlinking it
  stopped a kept rule such as `.navbox + style + .portal-bar` from matching
  (#339)
- `cascade apply` keeps a declaration in the sheet when a kept rule writes the
  same longhand under another property name: `p{margin:0}` went into a style
  attribute while the `.my-7{margin-top:1.75rem}` it loses to stayed behind,
  and a style attribute outranks every selector (#340)
- `cascade apply` keeps the comments a page holds. React writes an empty
  comment between two adjacent text nodes to keep them apart, and a browser
  measures each text run on its own, so merging them moved the element's width.
  The page is parsed and printed with markup.ml in place of lambdasoup (#346)
- `cascade diff` reports a rule that changed places next to whatever else the
  two sheets differ on, where one modified or added rule anywhere in the sheet
  hid every transposition (#474)
- `cascade diff` names an at-rule that carries no condition of its own by the
  head it prints to. That description keys the ordering comparison, so a
  `@media` that moved between a `@page` and a `@starting-style` was reported as
  no change (#345)
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
