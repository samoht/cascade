## 1.2.0

Most of the entries below are defect fixes, and the largest group of them has
one cause. Readers, printers and optimizer passes each walked the statement
tree by hand, matching the constructors they knew and closing with a wildcard,
and no two of those lists agreed, so an at-rule added to the AST later fell
through them unseen rather than failing to compile. They share one traversal
now, written without that wildcard, so a statement kind added after this
release stops every site that has to decide about it from compiling, and a test
walks all twenty-three statement shapes and every place a declaration can sit
to pin that the shared walk reaches them. Correctness was judged against a
browser rather than against cascade: the test suite renders a sheet and its
optimised form in headless Chrome and compares every property
`getComputedStyle` reports on every element, and a manual sweep does the same
to live pages, so several of the fixes below are miscompiles Chrome
contradicted rather than readings of the spec. Behind those sit 504 CSS files
drawn from 72 production sites and 2960 recorded cases carrying six minifiers'
answers.

### Breaking

- **IMPORTANT** Many of the fixes below change the CSS cascade emits for input
  1.1.0 already accepted, and a dozen of them change how the page renders. If
  you shipped minified output built with 1.1.0, re-run it and compare:
  `cascade diff --diff=canonical old.css new.css` exits 1 and prints the
  difference wherever the two are not equivalent.
- `Css.Container.of_components` and `Css.Media.of_components` are gone: call
  `Container.read` / `Media.read` with a cursor over the prelude's components
  (`Cursor.sub`). `Container.of_string`, `Media.of_string_strict`,
  `Supports.of_string` and the three `Font_face.*_of_string` raise
  `Cursor.Parse_error` where they raised `Failure`, so a `Failure` handler
  around any of them stops catching and the exception escapes; match
  `Cursor.Parse_error` instead (#496, #497, #499, #501)
- `Cascade.Component.pp` documents and renders itself as the located debug
  dump it always was. It was documented as source text, which sent a caller
  down a check that could never fire. Every node now shows its own location
  and an unclosed block or unterminated function is tagged, so two components
  `Component.equal` separates no longer print alike, and the children stay
  apart under minify. Source text comes from
  `Cascade.Parser.string_of_components` (#504)
- `Css.statement_declarations` is gone. It answered for a rule and a bare
  nesting block only, sharing its name with the exhaustive
  `Css.Stylesheet.statement_declarations`, which reaches every declaration a
  statement holds; call that one instead (#348)
- `Css.Stylesheet.moz_document_condition` gains `Url_exact`, `Domain`,
  `Media_document` and `Regexp`, so a match on it is no longer exhaustive (#461)
- `Css.Supports.property` raises `Failure` on a value that is not a
  `<declaration-value>`, where it wrote the text unchecked:
  `property "color" "red) or (color:blue"` emitted a condition a browser answers
  true for, so the rules the caller meant to guard applied (#459)
- `Css.Declaration.custom_property` raises `Failure` on a name and value that
  do not write back as the one declaration they name, where it stored the token
  stream unchecked: `custom_property "--a" "red;--b:blue"` wrote a second
  declaration and `"red} .evil{color:lime"` closed the rule and opened another.
  For strings from outside the parser use `parse_custom_property`, which is the
  same check as an option (#421, #428)
- `Css.Media.kind` classifies a negated width bound by the side it bounds, so
  `not (min-width: 640px)` sorts with the upper bounds it matches rather than
  with the lower bound it negates, and a doubled `not` cancels. The negation of
  a range such as `not (640px <= width <= 1024px)` is `Other`. `sort_key`,
  `group_order` and `compare` follow, so a caller sorting media queries gets a
  different order (#328)
- `Css.vars_of_rules` is `Css.vars_of_stylesheet`. It reported only what a
  top-level rule holds; it now also reports a `var()` inside a nested rule, an
  animation frame or a page margin box (#382)
- `Css.Stylesheet.layer_name` is the identifiers a `<layer-name>` is made of
  rather than the text between them, so `Css.layers`, `layer_block`,
  `layer_decl`, `layer`, `layer_of`, `as_layer`, `layer_block_name`,
  `layer_statement_name_list`, `import_layer_name` and the `?layer` argument of
  `custom_props` carry a `string list` where they carried a `string`.
  `Css.Stylesheet.read_layer_name` and `string_of_layer_name` convert between
  the two (#442)
- `Css.color` keeps the origin of a relative colour as a colour rather than in
  the opaque tail: `Relative_rgb` carries `color * string` and
  `Relative_color` carries `string * color * string`, so an expression or a
  pattern naming either takes the extra field (#313)
- `Css.Pp.ctx` gains `in_style_rule`; record expressions must set it and record
  patterns must bind it or use `; _` (#374)
- `Cascade.Reader.parse_error` gains `line` and `col`, and `filename` holds a
  source name where it packed `"<CSS input>:L:C"`: read the location from the
  two new fields, and `with_filename` keeps it instead of overwriting (#491)
- `Css.kind` gains `Radial_shape`, `Radial_size` and `Position_value`, so a
  match on it is no longer exhaustive and needs the three arms. They let
  `Css.Variables.var` bind a radial gradient's shape, size or centre
  `<position>` as a typed custom property, where it could only take those as an
  opaque token stream (#508)

### Parsing

- An error inside an at-rule condition or an `@font-face` descriptor points at
  the slice that failed, not at the end of the file with the caret past the
  last byte (#496, #497, #499, #501)
- Everything the parser repaired or dropped is reported, so strict mode
  rejects it. `@media screen {` swallowed the rest of the file and still
  returned `Ok` with no warnings, hiding a truncated stylesheet (#484)
- `Reader.int` raises `Parse_error` on a number with a fractional part or one
  outside the `int` range. It truncated `3.9` to `3` and answered `-1` for
  `1e30`, `1e999` and `9223372036854775808`, `int_of_float` being undefined
  past that range (#466)
- `border-inline`, `border-inline-start`, `border-inline-end`,
  `border-block-start` and `border-block-end` keep their value. None of the five
  had a value reader, so the declaration was dropped with a warning and a file
  holding nothing else exited 1 (#456)
- `@-moz-document` reads all five of its URL-matching functions. Only
  `url-prefix()` had a grammar, so `url()`, `domain()`, `media-document()` and
  `regexp()` took the at-rule down with every rule inside it (#461)
- `stroke-miterlimit` takes a value between 0 and 1. SVG 2 makes only a
  negative value illegal, having dropped SVG 1.1's "at least 1" rule because
  CSS parsers never enforced it (#334)
- A descending `@font-face` range such as `font-weight: 700 400` parses without
  a warning, the user agent swapping the endpoints for font matching. Only
  `unicode-range` keeps an ordering rule (#335)
- A media or container size feature takes a unitless zero. `@media (min-width: 0)`,
  `(width >= 0)` and `@container (min-width: 0)` were rejected and the at-rule
  went down with the condition, taking every rule inside it. The allowance is
  for a zero `<length>`, so `(min-resolution: 0)` stays invalid (#427)
- An `@font-face` descriptor whose value holds a `var()` is dropped with a
  warning, and `Css.of_string ~strict:true` rejects it: `var()` substitutes in
  property values only, so no descriptor grammar accepts one. `src` and
  `unicode-range` keep theirs, since `Css.inline_vars` resolves those at build
  time (#322)
- `clip-path`, `shape-outside` and `object-view-box` stop taking an
  intrinsic-sizing or CSS-wide keyword as a corner radius in the
  `round <'border-radius'>` suffix of a basic shape, which browsers drop anyway
  (#417)
- Which pseudo-classes may follow a pseudo-element is read per pseudo-element,
  so `::before:hover`, `::marker:hover` and `::selection:hover` stop parsing
  while `::file-selector-button:hover`, `::part(p):hover`, `::cue:hover` and
  `::-webkit-scrollbar:hover` keep parsing. One cascade does not recognise
  still takes any pseudo-class. A logical combination carries the list into its
  argument, so `::before:is(.b)` parses and matches nothing while
  `::before:not(.b)` stops parsing (#430)
- `::target-text`, `::spelling-error`, `::grammar-error` and the framework-only
  `::deep` family obey the compound and `:has()` rules the other
  pseudo-elements already did (#418)
- `:not()` rejects a pseudo-element in its argument, as `:has()` already did,
  so `.a:not(::before)` no longer parses and prints back as `.a:not(:before)`.
  Its argument is an unforgiving selector list, so browsers drop the whole rule
  where `:is()` and `:where()` drop only the offending item (#426)
- The eleven pseudo-classes WebKit's scrollbar parts report their state through
  are read as pseudo-classes, so `::-webkit-scrollbar:vertical` and
  `::-webkit-scrollbar-thumb:window-inactive` keep their rule instead of losing
  it at an unforgiving selector list. After a pseudo-element only a scrollbar
  part takes them, bar `:window-inactive`, which also reaches `::selection` and
  `::part()` (#441)
- A `<custom-ident>` or `<dashed-ident>` an at-rule prelude or a declaration
  value names is printed with the escapes needed to read it back as the same
  name: `@layer a\3b b` printed `@layer a;b`, two statements naming a layer the
  input never had. `@counter-style`, `@position-try`, `@font-palette-values`,
  `@container`, `@import layer()`, `@font-feature-values` feature names and
  every name a declaration value carries all take the escaping (#436)
- `@supports (--x\3b y: red)` is read instead of the whole rule being dropped.
  The reader demanded a property name that re-tokenizes from its own bytes,
  which holds only while the printer writes that name raw (#437)
- A `@layer` name is read as the identifiers it is made of, so `@layer a\2e b`,
  the layer named `a.b`, is no longer the same value as `@layer a.b`, the
  sublayer `b` of `a`. Both printed `@layer a.b`, and minification merged the
  two blocks into one layer, moving declarations into a layer the input never
  wrote them in (#442)
- Every at-rule that nests inside a style rule reads its body as nested rules.
  `.a { @starting-style { color: blue } }` kept the wrapper and dropped the
  declaration; `@-moz-document`, `@when` and `@else` lost theirs the same way,
  and `.a { @layer n { @media screen { color: red } } }` reached the
  stylesheet-level reader and lost its block too (#384)
- `.a { @layer n { color: red } }` keeps its declaration instead of reading the
  body as a selector list and dropping it. `@layer` was the last at-rule
  cascade nests that still took a stylesheet block (#374)
- An at-rule with no style rule in its body is invalid inside a style rule and
  dropped with a warning `Css.of_string ~strict:true` turns into an error:
  `@font-face`, `@keyframes`, `@property`, `@page`, `@counter-style`,
  `@position-try`, `@font-palette-values`, `@font-feature-values`, `@viewport`,
  `@supports-condition`, and an `@else` with no preceding `@when`. The discard
  ends at the at-rule rather than at the next `;`, where
  `.a { @import url(x) { } color: red }` lost `color: red`. `@view-transition`
  is kept, since browsers still read it there (#388)
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
  `@page { color: red }`, `@page { orphans: 3 }` and
  `@page { @top-center { display: block } }` are read where a seven-name
  allowlist rejected them, `bleed` and `page-orientation` included. A value the
  property's grammar rejects is still rejected (#403)
- A duplicate descriptor in a `@page` body or a page margin box keeps the
  important declaration rather than the one written last, so
  `@page { margin: 1cm !important; margin: 2cm }` keeps `margin: 1cm !important`
  (#404)
- `@page { @top-center { } }` no longer takes its `@page` with it. An empty
  margin box is elided on output, as an empty style rule already was (#405)
- An at-rule cascade has no handler for reaches the output with its block
  intact, where `cascade fmt` deleted it and every rule inside it without
  saying so. CSS Syntax 3 sec. 5.4.2 consumes an at-rule whatever its
  at-keyword; discarding one is the user agent's step, and
  `Optimize.drop_unknown_at_rules` serves a caller writing for a browser. A
  string, url, function, bracket or comment the source stopped inside is closed
  on the way in, so what is written back ends the at-rule instead of swallowing
  its `;` or `}` (#469, #483)
- A qualified rule whose prelude reads as a custom property, such as
  `--x:hover { color: red }`, is reported when it is dropped, and
  `Css.of_string ~strict:true` rejects it. CSS Syntax 3 sec. 5.5.3 makes the
  shape invalid so that a `{}`-block inside a custom property value is never
  misread as a rule; the drop itself was silent (#473)
- `position-area` takes the logical `start`/`end` keywords along with their
  `self-`, `self-x-`, `self-y-`, `self-block-` and `self-inline-` forms and
  every `span-` spelling. css-anchor-position-1 sec. 3.1.2 gives them a branch
  of the grammar and browsers lay them out, but cascade had no keyword for any
  of them and dropped the declaration (#478, #485)
- `position-area` rejects two keywords taken from different branches of its
  grammar, such as `left block-start` or `start top`. Cascade checked only that
  they named different axes, so it accepted 1120 ordered keyword pairs that
  css-anchor-position-1 sec. 3.1.2 and Chrome 151 both reject (#495)

### Printing

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
  `width: calc(sqrt(-1) * 1px)` printed `width: NaNpx` and `rotate: asin(-20)`
  printed `rotate: NaNdeg`, which browsers drop (#425)
- A custom-property name written with an escape is printed with its escapes.
  `:root{--x\3b y:red}` printed `:root{--x;y:red}`, which cascade's own reader
  splits into two declarations, and a reference to it printed `var(--x}y)`,
  which closes the rule around it. The declaration name, the `var()` reference
  and its fallback, the `@property` prelude and a `style()` container query all
  take the escaping (#435)
- `Css.to_string` renders the sheet once. It sized its buffer exactly by
  first running the sheet through a counter, so every printer below it ran
  twice to save a buffer growth that is amortised anyway (#479)

### Minification

- `--minify` merges two rules that declare the same NaN, whichever way each
  spelled it: `opacity: calc(NaN)` and `opacity: calc(infinity - infinity)`
  are one declaration (#471, #482)
- `--minify` merges adjacent `@container` blocks whose `style()` conditions
  are written the same way. The comparison reached the source byte offsets
  every token carries, so two byte-identical `style(--x: 1)` queries never
  compared equal and their blocks stayed separate, while size queries and the
  bare `style(--x)` form already merged (#465)
- `--minify` shortens an `src:` declaration outside `@font-face` the way it
  already shortened the `@font-face` descriptor: `url("a.woff2")` drops its
  quotes, `local("Arial")` becomes `local(Arial)` and a known `format()`
  keyword loses its quotes. The two routes into the `src` printer had
  drifted, and the declaration route spelled a multi-word `format("...")`
  string unquoted, which no browser reads back as that format. A family name
  of `default` keeps its quotes on both routes, the reserved word being no
  `<custom-ident>` (#470)
- `--minify` keeps the `center` in `position-area: top center`. A lone keyword
  stands for `X span-all`, not `X center`, so dropping it moved the box to a
  different area (#457)
- `--minify` keeps a `@layer` whose own rules write no declarations but nest
  rules that do. The emptiness test read only the declarations, so
  `@layer a { .x { .y { color: red } } }` collapsed to `@layer a;` and every
  declaration below the brace was deleted (#389)
- `--minify` optimises the body of `@-moz-document`, `@starting-style`, `@when`
  and `@else`, which it walked past: rules inside one kept whatever the author
  wrote (#343)
- An empty `@-moz-document`, `@when` or `@else` is dropped, as an empty
  `@media` already was. An empty `@when` or `@else` stays while a later `@else`
  binds to it, since dropping it leaves a bare `@else` no parser accepts (#396)
- `.a { @layer n {} }` keeps its block form. The statement form is a
  layer-order declaration, which no style rule accepts, so minifying it to
  `.a{@layer n;}` produced CSS neither a browser nor cascade's own reader takes
  back (#374)
- A declaration whose value is spec-invalid is discarded inside a `@keyframes`
  frame, as it already was in a style rule (#341)
- Under `--scope=stylesheet` a `position-try-fallbacks` name with no
  `@position-try` rule is dropped inside a `@keyframes` frame, as it already
  was in a style rule (#372)
- A custom property registered by `@property` as a `<color>` is typed as one
  inside `@keyframes`, where `box-shadow: 0 0 var(--ring)` kept the reference
  in the blur radius (#337)
- A custom property registered by `@property` is typed wherever it is declared,
  so the same value minifies the same way inside `@keyframes`,
  `@position-try` and `@supports-condition` (#349)
- `--flatten-nesting` treats `@-moz-document` as the grouping at-rule it is:
  nesting inside one flattens, and a rule wrapping one keeps its selector
  instead of the at-rule being emitted at top level under no parent (#344)
- A rule whose declarations a later rule all rewrites is dropped only when it
  nests nothing. Only the declarations written before a rule's first nested
  rule belong to the rule itself, which says nothing about the rest of the
  body, and
  `.a { all: unset; @media (min-width: 1px) { width: 1px } } .a { all: initial }`
  lost its nested block along with the rule (#376)
- A declaration written after a nested rule rejoins the declarations before it
  when nothing it crosses writes the same property at the same importance, so
  `.a { & b { width: 1px } color: red }` minifies to
  `.a{color:red;b{width:1px}}` and `--diff=canonical` stops calling it
  different from the same rule written the other way round. One that clashes
  keeps its place (#383)
- Declarations written after a nested rule are deduplicated like any other
  declaration list, so `.a { & b { color: red } color: blue; color: green }`
  minifies to `.a{b{color:red}color:green}` (#386)
- Merging same-selector rules keeps a later declaration behind a nested
  conditional group that writes the same property. The safety check saw a
  nested style rule only, so `@media`, `@container` and friends let the merge
  move the declaration past them and hand the conditional the win (#352)
- That merge reads the source order rather than the merged order, and compares
  a nested block with a later declaration by the longhands each writes rather
  than by property name. A nested `@media` won over a later declaration that
  overrode it, and a nested `margin: 2rem` and a later `margin-top: 1rem` were
  read as disjoint (#364)
- Merging two same-condition `@media` blocks no longer moves the first past a
  rule whose shorthand writes a longhand the block also writes:
  `@media (width>=1px){a{background-color:red}}a{background:blue}@media (width>=1px){a{background-color:green}}`
  minified to a sheet browsers compute blue for where the source computes green
  (#415)
- That merge no longer moves a block past a declaration written after a nested
  rule, which stays behind the rule it follows and is therefore one more rule
  in the way (#414)
- A declaration writing `column-rule-color` keeps its place against one writing
  `column-rule`. The shorthand resets the rule width, style and colour, but the
  colour longhand was not counted among them, so two rules writing the colour
  merged across `column-rule: 1px solid red` and an element matching both
  painted the rule red (#447)
- A declaration writing one of the four flow-relative border style longhands
  keeps its place against one writing `border` or `border-style`, neither
  shorthand having counted them:
  `.a{border:1px solid red;border-block-end-style:dashed}` reordered under
  `--minify --lossless` into a rule computing `border-bottom-style: solid`
  where the source computes `dashed` (#453)
- Two rules stay apart across a rule whose property name cascade cannot resolve
  to a set of longhands, where it was ruled out first on the property name
  itself: `.a{word-wrap:break-word}.b{overflow-wrap:normal}.c{word-wrap:break-word}`
  minified to a sheet computing `overflow-wrap: normal` where the source
  computes `break-word` (#452)
- A vendor-prefixed declaration keeps its place against the property it
  aliases, each having counted as a property of its own:
  `.a{-webkit-transform:none}.b{transform:rotate(45deg)}.c{-webkit-transform:none}`
  minified to a sheet computing `transform: matrix(...)` where the source
  computes `none`. Whether a dead prefix is then dropped outright stays the
  Baseline question `drop_vendor_aliases` answers on its own (#454)
- An authored value keeps every digit. Any dimension was rounded to six
  significant figures on the way out, so `.4285714em` came out as `.428571em`,
  which is `5.99999px` rather than `6px` at a `14px` font size, and
  `999999999px` came out a pixel wider. Six figures still bound a value
  cascade computes itself, so `calc(2px * pi)` is still `6.28319px` (#350)
- A `--name` registered by `@property` as a `<number>` keeps the digits it was
  written with, `--n: 1.4285714` having come out as `1.42857`. `calc(1 / 3)` is
  still `.333333` (#354)
- A math function inside `calc()` keeps its unit, where `calc(hypot(1px, 1px))`
  came out as `1.41421356`, a declaration browsers and cascade's own reader
  both drop. `abs()` and `hypot()` carry their arguments' unit out, the inverse
  trigonometric functions carry `deg`, and a call whose operands mix units
  keeps its spelling (#362)
- A computed dimension past a million units keeps every digit, where six
  significant figures stopped reaching its fraction and the budget was paid in
  integer digits the arithmetic got right: `calc(1in + 999999999px)` came out
  as `1000000000px`, 95px short. A narrower value still pays the budget:
  `calc(1cm + 1px)` is `38.7953px` (#367)
- A `font-family` name keeps the spelling the author wrote. A table of known
  names matched case-insensitively with hyphens folded to spaces and re-emitted
  its own spelling, so `font-family: open-sans` became `"Open Sans"` and
  `font-family: ny` became `"New York"`, naming a different family, in the
  `@font-face` descriptor that *defines* the name as much as in the properties
  that reference it. A multi-word name unquotes wherever it appears, where
  `"Source Code Pro"` stayed quoted next to a bare `SF Mono` in the same stack
  (#387)
- A `font-family` name that cannot be spelled as an identifier keeps its
  quotes. The guard checked which characters a name is made of but not how an
  identifier may start, so `"2Brand"`, `"-2x"` and `"-"` came out bare and a
  browser dropped the whole declaration, taking the rest of the stack (#390)
- A multi-word `font-family` name holding a reserved word keeps its quotes.
  `"inherit test"`, `"revert serif"` and `"default x"` came out bare and
  cascade's own reader then rejected the declaration it had just written (#401)
- `column-rule-color` and `-webkit-text-stroke-color` are typed as colours and
  `-webkit-text-fill-color` is minified as one, so a colour-valued property
  minifies to the same spelling whatever its name:
  `lab(1.90334 0.278696 -5.48866)` and `rgb(3, 7, 18)` both print `#030712`
  (#447)
- A single-argument `:is()` and a double `:not()` keep their wrapper around a
  type or universal selector, where removing it fused the two names:
  `.a:is(code)` printed `.acode` and `:is(.a *):is(code)` printed
  `:is(.a *)code`, which browsers drop (#377)
- A single-argument `:is()` keeps its wrapper after a pseudo-element that
  cannot take its argument, so `.a::before:is(.b)` minifies to
  `.a:before:is(.b)` rather than the `.a:before.b` that cascade's own reader
  and browsers all drop. It still unwraps where the compound can hold the
  argument, `.a::part(p):is(:hover)` included (#431)
- A vendor prefix is dropped only when its unprefixed twin is Baseline "widely
  available": `-webkit-backdrop-filter`, `-webkit-user-select`,
  `-webkit-text-size-adjust` and `-webkit-print-color-adjust` were dropped
  against a twin no shipping Safari understands (#325)
- A feature query on a vendor-prefixed property keeps its guard. The
  web-features dataset behind the Baseline facts tracks unprefixed features
  only, so a prefix is evidence of nothing, and treating the query as true
  turned Tailwind's legacy-browser reset on in Chrome, where
  `@supports (-webkit-hyphens: none)` is false (#378)
- `@media not all and (X)` minifies to the Level 4 `@media not (X)`. `all`
  matches every device, so the two spell the same query, and default minify
  already gives up Level 3 compatibility inside that very query by lowering
  `min-width` to range syntax. `--enforce-spec` keeps both Level 3 spellings
  (#323)
- `--minify` and `cascade diff` spend less time and memory on a large
  stylesheet, for the same output (#413, #422, #424, #468, #507)
- `--minify` no longer allocates quadratically on a long run of rules sharing
  one selector or one body, nor probes every pair of them to decide whether it
  may merge. The benchmark corpora hold no such run, so this bounds a worst
  case rather than speeding real input up (#480, #486, #487, #502, #505)
- `--minify` no longer scans quadratically when many rules share a deep
  selector prefix. The structural hash reads a fixed count of nodes, so
  `.a .b .c .d .e .f .g` and every sibling differing only past that prefix took
  one hash and the two tables that drop shadowed rules and declarations
  compared selector subtrees on every probe; the heaviest stylesheet in the
  corpus spends a third of what it did on that pass (#493)

### Custom properties

- A `page-break-before`, `page-break-after` or `page-break-inside` declaration
  survives `Css.inline_vars` as itself. Substituting a `var()` rebuilt the
  declaration from its minified name, which for these three is the `break-*`
  alias of a different property, so `page-break-inside` came back as
  `break-inside` with a value that property does not accept (#506)
- `Css.inline_vars` stays linear in at-rule nesting depth. Each of its four
  walks rebuilt the enclosing `@media`/`@layer`/`@supports` chain at every
  level, which cost 6.4% of the instructions on real stylesheets (#481)
- `Css.resolve_theme` accounts for the declarations `@keyframes`, `@page`,
  `@position-try` and `@supports-condition` carry: a name referenced only from
  inside one of them keeps its theme binding, a name whose only declaration
  sits in `@keyframes` or `@position-try` keeps the root binding, and a theme
  guard the theme did not select is dropped rather than printed as a
  declaration (#317, #324, #327)
- `Css.resolve_theme` builds each `theme_defaults` binding with
  `parse_custom_property` instead of writing every answer into synthesised
  `:root { ... }` text and reparsing it, where one value could add a rule, an
  at-rule or a second declaration to the output, or take every other theme
  default down with it (#421)
- A custom-property name that needs escaping binds instead of being refused.
  The name and the value are read as the tokens they are rather than as one
  `property ":" value` text, so a `theme_defaults` answer for `x;y` emits
  `:root{--x\;y:red}`, a name carrying a `:` names no declaration at all, and
  `@supports (--x\3b y: red)` builds its declaration the same way (#439)
- Pruning unreferenced custom properties counts a `var()` in a `@keyframes`
  frame, `@page` and its margin boxes, `@position-try` or
  `@supports-condition` as a reference, instead of deleting a binding those
  at-rules still use (#341)
- `Css.inline_vars` counts a `var()` in a page margin box or a
  `@supports-condition` body as a reference, so it no longer emits a name whose
  definition it deleted, and reports an overridden variable through `~warn`
  (#342)
- `Css.inline_vars` preserves runtime-marked `var()` references, typed
  fallbacks simplified through scalar values or shorthands included, instead of
  replacing browser-time override points with compile-time defaults (#315)
- `Css.inline_vars` resolves a custom property defined across cascade layers
  against the order every `@layer` in the sheet gives it, counting the ones a
  rule nests and the ones a `@container`, `@scope` or `@starting-style` block
  holds. A layer first named inside a conditional group has no decidable order,
  so those layers are left standing (#357)
- `Css.inline_vars` unwraps an `@layer` and drops an `@property` registration
  written inside a rule, as it already did at top level, so a sheet using CSS
  nesting no longer comes back half cleaned (#373)
- `Css.inline_vars` unwraps an `@layer` only where the layer order and document
  order already pick the same winner for every property two layers write.
  Unwrapping replays the layer order as document order and hands the decision
  back to specificity, so a sheet whose layers order competing declarations
  came out rendering a different value with no custom property involved. An
  `@layer` written inside a rule stays standing (#371)
- `Css.inline_vars` keeps the `@property` registration of a custom property it
  leaves live. Every registration was dropped, so a property it could not
  inline safely lost the `initial-value` its references fall back on and the
  `inherits: false` that stops it inheriting, repainting the page. A second run
  also pruned a declaration the first had kept (#416)
- `Css.inline_vars` reads a `style()` container query as a reference to the
  custom property it queries, so deleting the declaration behind it, or the
  registration standing in for one, no longer leaves a block that never matches
  (#423)
- `Css.custom_props` reports a name declared inside `@scope`,
  `@starting-style`, `@-moz-document`, `@when`, `@else` or a bare nesting
  block, as it already did for `@media` and `@supports` (#375)

### Canonical diff

- A fully transparent `oklab()` with a missing axis, such as
  `oklab(0% none none / 0)`, compares equal to transparent black.
  Non-transparent forms stay distinct (#312)
- A relative-colour function keeps its origin as a typed colour, so `red` and
  `#f00` compare equal in `rgb()`, `oklab()` and the rest, including inside
  custom properties (#313)
- A complete shadow value in an unregistered custom property has its colour
  compared as a colour. Non-colour identifiers stay opaque (#314)
- The canonical form is taken inside every at-rule with a block, so the verdict
  no longer depends on which at-rule encloses the rule. A quoted multi-word
  family name in a custom property, a `color(srgb ...)` naming an exact 8-bit
  colour and the Level 3 `not all and (...)` spelling were canonicalised inside
  `@layer` and `@media` but not inside `@scope`, `@starting-style`,
  `@-moz-document`, `@when` or `@else` (#393)
- `Css_compare.equivalent_value ~property` spells the property name the way the
  printer spells it before comparing two values under it. A name carrying an
  escaped `}` closed the rule and took both values with it, so any two values
  under such a name compared equal (#440)
- A redundant `@layer` order pin no longer reads as a difference: the canonical
  projection drops every name whose removal leaves the sheet's layer order
  alone, so `@layer a;@layer a{...}` and `@layer a{...}` compare equal. A pin
  that fixes the order, or one over a position the projection cannot read, is
  kept (#475)

### Library

- `Css.Stylesheet.statement_declarations` is the declarations a statement holds
  directly; with `statement_children` it reaches every declaration in a
  stylesheet (#317)
- `Css.Stylesheet.map_statement_children` and `map_statement_declarations`
  rebuild a statement with a function applied to the block or the declarations
  it holds (#337). They return the statement they were given when that function
  leaves every list physically unchanged, so a pass that short-circuits on
  physical equality can call them (#355)
- `Css.Stylesheet.fold_statements`, `iter_statements`, `fold_declarations`,
  `iter_declarations` and `map_declarations` traverse a whole block. The
  declaration functions take `?sites`, so a place added to `declaration_sites`
  stops every caller that made a choice from compiling (#356)
- `Css.Stylesheet.edit_statements` hands each statement to a function that
  keeps, replaces or drops it, and descends into what survives (#363)
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
  different answers to the same question, and `Css.Stylesheet.layer_block` is
  exposed alongside it. `Css.Stylesheet.media_queries`, `container_queries` and
  `Css.media_queries` report a query written inside a grouping at-rule, and
  pair each query with every rule below its brace rather than the ones sitting
  directly under it (#389)
- `Css.Flatten` carries the parent selector into an `@-moz-document` block, the
  one grouping at-rule it did not descend into, where the declarations came out
  bare at the top of the block and no reader takes that back (#384)
- `Cascade_diff.Tree_diff.has_container_added_of_type` and
  `has_container_removed_of_type` look inside a container reported as modified,
  as `count_containers_by_type` already did, where a `@supports` added inside
  an existing `@media` was counted but not found (#395)
- `Cascade.Resolve.Make.resolve` and `Cascade.Resolve.layer_order` document
  every block they leave out, not just conditional groups: `@starting-style`
  declares a before-change style, `@scope` brings a scoping root and the
  proximity criterion, and an origin wrapper carries an origin that outranks
  the layer. `Css.layers` remains the exhaustive count of what a sheet declares
  (#394)
- `Cascade.Error.to_string` puts the caret under the character that failed and
  prints back a snippet that is valid UTF-8. The caret column was a byte count,
  so a multibyte selector pushed the marker well past the error, and the window
  was sliced on byte offsets, so a long multibyte class name opened the snippet
  inside a code point and the line came out starting with a replacement
  character (#472)
- `Cascade.Reader.parse_error` reports the line and column of the failing byte
  from a forward scan and counts its caret in characters. The walk ran
  backwards from the error and reset the column at each newline, so it counted
  the line before the error and stopped one column short at end of input, and
  the caret was a byte count under a window that could open inside a code
  point (#477)

### CLI tools

- `cascade apply` reads a `style` attribute as a declaration list in source
  order. The declarations came out reversed, so a longhand beat the shorthand
  it was written after, and a `}` inside the attribute closed the list early
  instead of staying a preserved token, so `style="color:red}p{color:lime"`
  applied `color:red` where the whole attribute is one invalid declaration a
  browser drops (#326)
- `cascade apply --minimal` drops an inherited declaration only when it truly
  restates the value the element would inherit. It dropped one whose property a
  user-agent rule declares for the element or an ancestor, uncovering the
  user-agent value for link colour, heading size and `pre` family (#326); and
  one whose value resolves against the element it lands on, where a
  font-relative unit, a percentage, `larger`/`bolder`, a container unit,
  `var()`, `light-dark()` or `currentColor` inside `color-mix()` names two
  values across two elements, so `div{font-size:2em}div p{font-size:2em}`
  halved the paragraph (#329)
- `cascade apply --minimal` keeps a restated inherited shorthand when an
  element in between sets one of the longhands it resets, where
  `#p{font:16px serif}#mid{font-weight:bold}#c{font:16px serif}` left `#c`
  bold. `all` and a property cascade does not type reset everything, so neither
  is dropped either (#332)
- `cascade apply` empties the `<style>` blocks it projects instead of removing
  them: a `<style>` element is a sibling like any other, so unlinking it
  stopped a kept rule such as `.navbox + style + .portal-bar` from matching
  (#339)
- `cascade apply` keeps a declaration in the sheet when a kept rule writes the
  same longhand under another property name. `p{margin:0}` went into a style
  attribute while the `.my-7{margin-top:1.75rem}` it loses to stayed behind,
  and a style attribute outranks every selector, so the paragraph rendered with
  no margin (#340)
- `cascade apply` keeps the comments a page holds. React writes an empty
  comment between two adjacent text nodes to keep them apart, and a browser
  measures each text run on its own, so merging them moved the element's width;
  licence headers went missing outright. The page is parsed and printed with
  markup.ml in place of lambdasoup (#346)
- `cascade apply` exits 0 for a `<style>` block whose parse kept a statement,
  so a build gating on the exit status passes on valid CSS. The check rendered
  the sheet, and an empty rule, a redundant `@charset "UTF-8"` or an `src`-less
  `@font-face` prints nothing while losing nothing (#489)
- `cascade fmt` exits 1 when parse recovery left no statement at all, where it
  exited 1 when the printed output was empty and the parse had warned. A rule
  survives a declaration the parser could not read, and `--minify` removes a
  redundant `@charset "UTF-8"` or an `src`-less `@font-face` that nothing
  lost, so both failed a build over CSS the parser used in full. The status
  now answers the question `cascade apply` asks of each source (#494)
- `cascade diff` names an at-rule that carries no condition of its own by the
  head it prints to, rather than describing every one identically. That
  description keys the ordering comparison, so a `@media` that moved between a
  `@page` and a `@starting-style` was reported as no change (#345)
- `cascade diff` names a rule's nested block after the rule it belongs to,
  where the block printed as `& .a`, a selector matching a `.a` inside the
  parent rather than the parent's own block, and the declarations written after
  a nested rule were named by the first of them (#385)
- `cascade diff --diff=tree` shows the contents of a block added or removed
  wholesale whatever the block holds, where a `@scope`, a `@starting-style` or
  a rule nested in another rule printed a header with nothing beneath it and
  read as empty (#389)
- `cascade diff` reports a rule that changed places next to whatever else the
  two sheets differ on, where one modified or added rule anywhere in the sheet
  hid every transposition and the report named the content change alone (#474)

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
- `dune test` renders a stylesheet and its optimized forms in a headless
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

- CLI commands share one binary file reader that closes its descriptor when a
  read fails (#309)
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
