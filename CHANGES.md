## Unreleased

### Breaking

- `Css.statement_declarations` is gone. It answered for a rule and a bare
  nesting block only, sharing its name with the exhaustive
  `Css.Stylesheet.statement_declarations`, which reaches every declaration a
  statement holds; call that one instead (#348)

### Parsing

- A compound operand of `not`, `and` or `or` in a `@media` condition keeps the
  parentheses Media Queries 4 sec. 3 requires around a `<media-in-parens>`:
  `not ((min-width:1px) or (max-width:2px))` printed as
  `not (min-width:1px)or (max-width:2px)`, which browsers and cascade's own
  reader reject, losing the whole block (#319)
- An `@font-face` descriptor whose value holds a `var()` is dropped with a
  warning, and `Css.of_string ~strict:true` rejects it. `var()` substitutes in
  property values only (CSS Variables 1), so no descriptor grammar accepts one
  and browsers drop the declaration. `src` and `unicode-range` keep theirs,
  since `Css.inline_vars` resolves those references at build time (#322)
- `stroke-miterlimit` accepts a value between 0 and 1. SVG 2 sec. 13.5.5 makes
  only a negative value illegal, having dropped SVG 1.1's "at least 1" rule
  because CSS parsers never enforced it, and `stroke-miterlimit: 0.5` was
  rejected outright (#334)
- A media or container size feature takes the unitless zero CSS Values 4 sec. 5
  allows for a zero `<length>`, which Media Queries 4 sec. 1.3 inherits.
  `@media (min-width: 0)`, `(width >= 0)` and `@container (min-width: 0)` were
  rejected, and the at-rule went down with the condition, taking every rule
  inside it at exit 0. The allowance is `<length>`-only, so
  `(min-resolution: 0)` stays invalid (#427)
- A descending `@font-face` descriptor range such as `font-weight: 700 400`
  parses without a warning and `Css.of_string ~strict:true` accepts it. CSS
  Fonts 4 sec. 4.4 has the user agent swap the endpoints for font matching, so
  the range is well defined rather than an error, and only `unicode-range`
  keeps an ordering rule (#335)
- The `round <'border-radius'>` suffix of a basic shape reads its radii through
  the same reader as the `border-radius` property, so `clip-path`,
  `shape-outside` and `object-view-box` stop accepting an intrinsic-sizing or
  CSS-wide keyword as a corner radius. CSS Backgrounds 3 sec. 5.1 allows only a
  non-negative `<length-percentage>` there, and Chrome and WebKit drop every
  such declaration (#417)
- A `<custom-ident>` or `<dashed-ident>` an at-rule prelude or a declaration
  value names is printed with the escapes CSS Syntax 3 sec. 4.3.7 needs to read
  it back as the same name. `@layer a\3b b` printed `@layer a;b`, two
  statements naming a layer the input never had. `@counter-style`,
  `@position-try`, `@font-palette-values`, `@container`, `@import layer()`, a
  `@font-feature-values` feature name and every name a declaration value
  carries, from `anchor-name` to `will-change`, all take the escaping (#436)
- `@supports (--x\3b y: red)` is read instead of the whole rule being dropped.
  The reader demanded a property name that re-tokenizes from its own bytes,
  which holds only while the printer writes that name raw; Chrome and
  lightningcss both accept the escaped name (#437)
- A `@layer` name is read as the idents CSS Cascade 5 sec. 6.4.1 makes it, so
  `@layer a\2e b`, the layer named `a.b`, is no longer the same value as
  `@layer a.b`, the sublayer `b` of `a`. Both printed `@layer a.b` and the
  minifier merged the two blocks into one layer, moving declarations into a
  layer the input never wrote them in (#442)
- An `@layer` block inside a style rule holds nesting content, so
  `.a { @layer n { color: red } }` keeps its declaration instead of reading the
  body as a selector list and dropping it. CSS Nesting 1 sec. 3.1 admits nested
  at-rules, and `@layer` was the last of the at-rules cascade reads as a
  nesting context still taking a stylesheet block (#374)
- A declaration written after a nested rule keeps its place instead of being
  hoisted to the top of the block. CSS Nesting 1 sec. 3.4 wraps such a run in a
  nested declarations rule, and its worked example names the hoisted spelling as
  not equivalent, so `.a { @supports (color: red) { color: blue } color: green }`
  now computes green as Blink and WebKit do, where cascade printed a sheet that
  computed blue (#380)
- Every at-rule that nests inside a style rule reads its body as nesting
  content. `.a { @starting-style { color: blue } }` kept the wrapper and dropped
  the declaration, `@-moz-document`, `@when` and `@else` lost theirs the same
  way, and an at-rule written inside a nested group rule, as in
  `.a { @layer n { @media screen { color: red } } }`, reached the
  stylesheet-level reader and lost its block too. CSS Nesting 1 sec. 3.3 nests
  any at-rule whose body carries style rules, and Blink 146 keeps every one of
  these shapes whole (#384)
- An at-rule with no style rule in its body is rejected inside a style rule
  instead of being kept. CSS Nesting 1 sec. 3.3 nests only an at-rule whose body
  carries style rules, so `.a { @font-face { ... } }`, `@keyframes`, `@property`,
  `@page`, `@counter-style`, `@position-try`, `@font-palette-values`,
  `@font-feature-values`, `@viewport` and `@supports-condition` are invalid
  there, as is an `@else` with no preceding `@when`; each is dropped with a
  warning that `Css.of_string ~strict:true` turns into an error, and the
  declarations written around it stay in the rule. `@view-transition` is kept,
  since Blink 146 still reads it there (#388)
- Discarding an at-rule that is invalid inside a style rule ends at the at-rule
  rather than at the next semicolon. `.a { @import url(x) { } color: red }` lost
  `color: red`, and an `@import` inside a nested group rule took the whole group
  with it (#388)
- An invalid declaration inside a nested at-rule is dropped on its own rather
  than taking the whole stylesheet. `.a { @media screen { color: red;
  width: 10; background: blue } }` parsed to nothing, since the nested block had
  no recovery and the error unwound past every enclosing block. CSS Syntax 3
  sec. 5.4.4 ends a bad declaration at the next top-level `;`, counting a `{}`
  met on the way as one component value of the value being skipped, and Blink
  146 keeps both neighbours in one declaration run. A nested rule whose prelude
  starts with an identifier, as in `.a { @media screen { h2:where(.b) { color:
  red } } }`, and a stray `;` between two declarations there, were lost the same
  way (#392)
- A descriptor a `@page` body rejects is dropped on its own rather than taking
  the rule and the stylesheet holding it. `@page { margin: 1cm; width: 10;
  margin-top: 2cm }` parsed to nothing, and a page margin box lost its whole
  block the same way. CSS Paged Media 3 sec. 4.1 applies the parse-error rules
  inside a page or margin context, so the valid declarations around the bad one
  still apply, and Blink 146 keeps every neighbour. An invalid margin at-rule is
  discarded to the end of its block rather than to the next `;`, and a
  selector-shaped item in a margin box, such as `@page { @top-center { .a { b:
  c } } }`, no longer loops forever (#398)
- A descriptor an `@property` body rejects is dropped on its own rather than
  taking the registration and the stylesheet holding it. `@property --x { syntax:
  "<length>"; inherits: false; initial-value: 0px; zzz: 1 }` parsed to nothing,
  and so did a stray `;` between two valid descriptors. CSS Properties and Values
  API 1 sec. 2 ignores an unknown descriptor, CSS Syntax 3 sec. 5.4.3 discards a
  `;` with no declaration to validate, and Blink 146 reads the registration in
  both. Tokens left after a descriptor's value, such as a trailing `!important`,
  now invalidate the declaration they follow rather than the leftover alone
  (#399)
- A rule a grouping at-rule's block rejects is discarded to the end of that rule
  rather than to the first block of any kind in its prelude. In
  `@media screen { @supports (display: grid) bogus { a { color: red } } }` the
  discard stopped at `(display: grid)`, so `bogus { a { color: red } }` came back
  as a rule of its own, and a `[]` in a bad selector cost a second warning for
  one dropped rule. CSS Syntax 3 sec. 5.4.2 ends an at-rule at its block or its
  `;` and sec. 5.4.3 ends a qualified rule at its block, so a `(` or a `[`
  written before that block is part of what is discarded, and Blink 146 keeps no
  such rule (#402)
- A `@page` body and a page-margin box keep any property they are given, so
  `@page { color: red }`, `@page { orphans: 3 }` and
  `@page { @top-center { display: block } }` are read instead of erroring under
  `~strict:true` and being dropped with a warning otherwise. CSS Paged Media 3
  sec. 6 admits its Appendix A list of CSS 2.1 properties in both contexts and
  leaves anything outside CSS 2.1 undefined rather than invalid, and Blink 146
  keeps every property it knows there. The seven-name allowlist also rejected
  `bleed`, the name sec. 7.3 defines, and `page-orientation`. A value the
  property's grammar rejects, and an item that is no declaration, are still
  rejected (#403)
- A duplicate descriptor in a `@page` body or a page-margin box keeps the
  important declaration rather than the one written last, so
  `@page { margin: 1cm !important; margin: 2cm }` keeps `margin: 1cm !important`
  where it kept `margin: 2cm`. CSS Cascade 5 sec. 6.2 ranks an important author
  declaration above a normal one whatever their order, and Blink 146 reads the
  important one back in a page body and in a margin box alike. Two declarations
  of the same importance still keep the last (#404)
- A page-margin box with an empty block is read instead of erroring under
  `~strict:true` and being dropped with a warning otherwise, so
  `@page { @top-center { } }` no longer takes its `@page` with it. CSS Paged
  Media 3 sec. 5 gives a margin at-rule a `<declaration-list>`, which CSS Syntax
  3 sec. 5.4.4 consumes even when it holds nothing, and Blink 146 keeps the box.
  Sec. 5 generates the box only where `content` computes away from `none`, so an
  empty one is elided on output like an empty style rule and an empty `@page`
  already are (#405)
- A descriptor a `@counter-style`, `@font-palette-values` or `@view-transition`
  body rejects is dropped on its own, as is a feature block an
  `@font-feature-values` body rejects, rather than taking the at-rule and the
  stylesheet holding it. `@counter-style thumbs { system: cyclic; symbols: "x";
  zzz: 1 }` parsed to nothing, and the other three lost their whole rule the
  same way. CSS Syntax 3 sec. 5.4.3 keeps what a block's contents already
  yielded when one item fails to parse, and Blink 146 keeps all four rules. An
  `@font-feature-values` body is a list of rules, so an item discarded there
  ends at its own block rather than at a `;` it has not got, and a `;` with
  nothing before it costs nothing. Tokens left after a `@font-palette-values` or
  `@view-transition` or `@counter-style` descriptor value, such as a trailing
  `!important`, now invalidate the declaration they follow rather than the
  leftover alone (#419)
- An at-rule inside a `@keyframes` body is dropped on its own rather than taking
  the animation and the stylesheet holding it, so
  `@keyframes k { from { color: red } @media print { to { color: pink } } 50%
  { background: lime } }` keeps both keyframes where it parsed to nothing. The
  rejection was raised around the loop rather than inside it, so it unwound past
  the recovery the loop already ran for a keyframe selector it rejects. CSS
  Syntax 3 sec. 5.4.2 ends the at-rule being discarded at its own block, past
  any `(` or `[` in its prelude, and Blink 146 keeps the keyframes written
  around it (#420)

### Minification

- A NaN-valued number prints as `calc(NaN)`, and a NaN-valued dimension as
  `calc(NaN * 1unit)`, the forms CSS Values 4 sec. 10.13 defines. A bare `NaN`
  is not a CSS token, so `width: calc(sqrt(-1) * 1px)` minifying to
  `width: NaNpx` and `rotate: asin(-20)` to `rotate: NaNdeg` produced output
  Chrome drops and cascade's own reader rejects. A math function whose value is
  NaN keeps its function form instead of folding to a leaf (#425)
- A same-condition `@media` block is no longer hoisted over a crossed rule
  whose shorthand writes a longhand the hoisted block also writes. The hoist
  tied two declarations by property name, so
  `@media (width>=1px){a{background-color:red}}a{background:blue}@media (width>=1px){a{background-color:green}}`
  minified to a sheet Chrome computes blue for where the source computes green
  (#415)
- A `font-family` name keeps the spelling the author wrote. A table of known
  font names matched an authored name case-insensitively with hyphens folded
  to spaces and re-emitted the table's spelling, so `font-family: open-sans`
  became `font-family: "Open Sans"` and `font-family: ny` became
  `font-family: "New York"`. CSS Fonts 4 sec. 5.1 matches a `<family-name>`
  with Default Caseless Matching, a caseless string comparison that folds
  neither a hyphen to a space nor one name to another, so those rewrites named
  a different family, in the `@font-face` descriptor that *defines* the name as
  well as in the properties that reference it (#387)
- A multi-word `<family-name>` unquotes under minify wherever it appears:
  `"Source Code Pro"` and `"Fira Sans"` stayed quoted next to the `SF Mono` and
  `Roboto Mono` in the same stack (#387)
- An authored coefficient keeps every digit under `--minify`. Any dimension was
  rounded to six significant figures at print time, so `.4285714em` came out as
  `.428571em`, which is `5.99999px` rather than `6px` at a `14px` font size, and
  `999999999px` came out a pixel wider. The six-figure budget now belongs to the
  fold that computes a value, so `calc(2px * pi)` is still `6.28319px` (#350)
- A `--name` registered by `@property` as a `<number>` keeps the digits it was
  written with: `--n: 1.4285714` came out as `1.42857`. The six-figure budget
  stays on the `calc()` the printer folds itself, so `calc(1 / 3)` is still
  `.333333` (#354)
- A math function inside `calc()` keeps the unit CSS Values 4 sec. 10.7 gives
  it: `calc(hypot(1px, 1px))` came out as `1.41421356`, a declaration browsers
  and cascade's own reader both drop. `abs()` and `hypot()` now carry their
  arguments' unit out, the inverse trig functions carry `deg`, and a call whose
  operands mix units keeps its spelling (#362)
- A computed dimension past a million units keeps every digit under `--minify`.
  Six significant figures stop reaching its fraction there, so the budget was
  paid in integer digits the arithmetic got right: `calc(1in + 999999999px)`
  came out as `1000000000px`, 95px from the `1000000095px` an inch is worth (CSS
  Values 4 sec. 6.2), and `hypot(999999999px, 1px)` a pixel wider than its own
  longest side. A narrower value still pays the budget: `calc(1cm + 1px)` is
  `38.7953px` (#367)
- An empty `@layer name` inside a style rule keeps its block form. The
  statement form is a layer-order declaration, which no style rule accepts, so
  `.a { @layer n {} }` minified to `.a{@layer n;}`, which neither a browser nor
  cascade's own reader takes back (#374)
- A declaration whose value is spec-invalid is discarded inside a `@keyframes`
  frame, matching what a browser does with it there and what cascade already
  did in a style rule (#341)
- Under `--scope=stylesheet` a `position-try-fallbacks` name with no
  `@position-try` rule is dropped inside a `@keyframes` frame, as it already
  was in a style rule. The name cannot match at runtime wherever the
  declaration is written (#372)
- `--minify` optimises the body of `@-moz-document`, `@starting-style`,
  `@when` and `@else`, which it walked past: rules inside one of them kept
  whatever the author wrote (#343)
- `--minify` keeps a `@layer` whose rules write no declarations of their own
  but nest rules that do. The emptiness test read only the declarations, so
  `@layer a { .x { .y { color: red } } }` collapsed to `@layer a;` and every
  declaration below the brace was deleted (#389)
- `--flatten-nesting` treats `@-moz-document` as the grouping at-rule it is:
  nesting inside one flattens, and a rule wrapping one keeps its selector
  instead of emitting the at-rule at top level under no parent (#344)
- `@media not all and (X)` minifies to the Level 4 `@media not (X)`. `all` is
  the identity media type (Media Queries 4 sec. 2.3), so the two spell the same
  query, and default minify already spends Level 3 compatibility by lowering
  `min-width` to range syntax inside that very query. `--enforce-spec` keeps
  both Level 3 spellings (#323)
- A vendor prefix is dropped only when its unprefixed twin is Baseline "widely
  available", so `--minify` keeps the prefix a maintained browser still reads:
  `-webkit-backdrop-filter`, `-webkit-user-select`, `-webkit-text-size-adjust`
  and `-webkit-print-color-adjust` were dropped against an unprefixed twin no
  shipping Safari understands (#325)
- A rule whose declarations a later rule all rewrites is dropped only when it
  carries no nested content. A rule's declarations are only the run written
  before its first nested statement (CSS Nesting 1 sec. 3.4), so covering them
  says nothing about what the rest of the body sets, and
  `.a { all: unset; @media (min-width: 1px) { width: 1px } } .a { all: initial }`
  lost its nested block along with the rule (#376)
- A declaration written after a nested statement rejoins the rule's own run
  when nothing it crosses writes the same property at the same importance, so
  `.a { & b { width: 1px } color: red }` minifies to
  `.a{color:red;b{width:1px}}` and `--diff=canonical` stops reporting it as
  different from `.a { color: red; & b { width: 1px } }`. A declaration that
  does clash keeps the place CSS Nesting 1 sec. 3.4 gives it (#383)
- A run of declarations written after a nested statement is deduplicated like
  any other declaration list, so `.a { & b { color: red } color: blue;
  color: green }` minifies to `.a{b{color:red}color:green}` rather than keeping
  a write nothing can read. Nothing sits between two writes inside one run, so
  the later wins (CSS Cascade 5 sec. 6.4.4) (#386)
- `--minify` is faster on a stylesheet the optimizer factors heavily, for the
  same output. The rule graph's cycle check, topological order and
  selector-branch index each probed a generic `Hashtbl` once per edge or per
  branch, paying a hash and a structural comparison on a key that is a dense
  node id or a branch string (#413)
- Unwrapping a baseline-true `@supports` nested in a style rule keeps the `;`
  separating its declarations from the sibling that follows. CSS Syntax 3 sec.
  5.4.4 runs a declaration to the next `;` or to the block's `}`, so
  `.a{@supports (color:red){color:blue}@supports (color:red){color:green}}`
  minified to `.a{color:#00fcolor:green}`, which browsers and cascade's own
  reader reject, losing the declaration (#370)
- `--minify` spends less time deciding whether two rules conflict, for the same
  output. A rule's overlap keys are sorted and deduplicated once, so the
  conflict test walks the two key lists in step instead of scanning one per
  element of the other, and collecting them no longer rescans what it has
  already collected (#422)
- Merging same-selector rules keeps a later declaration behind a nested
  conditional group that sets the same property. The safety check saw a nested
  style rule only, so `@media`, `@container` and friends let the merge hoist
  the declaration past them and hand the conditional the win (#352)
- Merging same-selector rules orders the rules by what their nested blocks set
  as well as by their declarations, and reads the source order to decide
  whether a declaration crosses a nested block. The merge order could put the
  rule carrying the nested block last, leaving that check nothing to look at,
  so a nested `@media` won over a later declaration that overrode it (#364)
- Merging same-selector rules weighs what a nested block sets against a later
  declaration by cascade slot rather than by property name. A nested
  `margin: 2rem` and a later `margin-top: 1rem` write one slot under two names,
  so the merge read them as disjoint, hoisted the longhand ahead of the nested
  block and handed the shorthand the win (#364)
- A `font-family` name that cannot be spelled as an identifier keeps its
  quotes. The unquoting guard checked which characters a name is made of but
  not how CSS Syntax 3 sec. 4.3.9 lets an ident sequence start, so `"2Brand"`,
  `"-2x"` and `"-"` came out bare in every mode and a browser dropped the whole
  declaration, taking the rest of the font stack with it (#390)
- A multi-word `font-family` name holding a reserved word keeps its quotes.
  `"inherit test"`, `"revert serif"` and `"default x"` came out bare under
  `--minify`, and cascade's own reader then rejected the declaration it had
  just written. CSS Fonts 4 sec. 2.1.1 excludes a pre-defined `font-family`
  keyword and a CSS-wide keyword per identifier, and CSS Values 4 sec. 4.2 adds
  the reserved `default`, so the exclusion reaches every word of a
  `<custom-ident>+` sequence and the quoted form is the only valid one (#401)
- A same-condition `@media` block is no longer hoisted over a declaration
  written after a nested rule. CSS Nesting 1 sec. 3.4 keeps that run behind the
  rule it follows, so it is one more rule the hoist reorders, and
  `.a { @media (min-width: 1px) { color: red } color: blue; @media (min-width: 1px) { color: green } }`
  minified to a sheet Chrome computes blue for where the source computes green
  (#414)
- An empty `@-moz-document`, `@when` or `@else` is dropped, as an empty
  `@media` already was: a conditional group rule with no contents applies
  nothing whatever its condition. An empty `@when` or `@else` stays while a
  later `@else` binds to it, since dropping the antecedent would leave a bare
  `@else` that no parser accepts (#396)

### Custom properties

- `Css.inline_vars` resolves a custom property defined across cascade layers
  against the order every `@layer` in the sheet gives it, counting the ones a
  rule nests and the ones a `@container`, `@scope` or `@starting-style` block
  holds. A layer first named inside a conditional group is introduced there only
  when the condition holds, so the order is not decidable and the layers are
  left standing rather than folded to a winner (#357)
- `Css.inline_vars` unwraps an `@layer` only where the layer stack and document
  order already pick the same winner for every slot two layers write. Unwrapping
  replays the stack as document order and hands the decision back to
  specificity, so a sheet whose layers order competing declarations came out
  rendering a different value, with no custom property involved. It narrows the
  unwrapping #373 added: a sheet holding a nested rule is one the check cannot
  answer for, so an `@layer` written inside a rule stays standing (#371)

- `Css.resolve_theme` accounts for the declarations `@keyframes`, `@page`,
  `@position-try` and `@supports-condition` carry. A `var()` referenced only
  from inside one of them keeps its theme binding instead of leaving the name
  undefined in the emitted sheet; a name whose only declaration sits in
  `@keyframes` or `@position-try` keeps the root binding, since the animation
  and position fallback origins never defined it for the element referencing
  it; and a theme guard the keep-set rejects is dropped instead of printed as
  a declaration the theme never selected (#317, #324, #327)
- `Css.resolve_theme` binds a `theme_defaults` answer only when the name and
  the value make one custom-property declaration. The resolver used to write
  every answer into synthesised `:root { ... }` text and reparse it, so a value
  carrying a `}`, a top-level `;` or an unterminated string could close the
  block and add a rule, an at-rule or a second declaration to the output, or
  take every other theme default down with it.
  `Css.Declaration.parse_custom_property` is the checked constructor the
  resolver builds each binding with (#421)
- `Css.Declaration.custom_property` refuses a name and value it cannot write
  back as the one declaration they name, instead of storing the token stream
  unchecked: `custom_property "--a" "red;--b:blue"` wrote a second declaration,
  `"red} .evil{color:lime"` closed the rule and opened another, `"rgb(1,2,3"`
  gained a closing parenthesis it was never given, `"\"abc"` left a string open
  across the rest of the sheet, and a name carrying a `}` destroyed the rule
  around it. The pair it takes is a `<dashed-ident>` name and the
  `<declaration-value>?` CSS Variables 1 sec. 2 gives a custom property, the
  check `parse_custom_property` already made (#428)
- A custom-property name that needs escaping binds instead of being refused.
  `Css.Declaration.parse_declaration` read `property ":" value` back as one
  text, so a name carrying a `;` or a `}` (CSS Syntax 3 sec. 4.3.7 puts either
  there through an escape) ended its own declaration, and the guard on
  `custom_property` and `parse_custom_property` was there only to keep such a
  name out of that text. The name and the value are read as the tokens they
  are, so a `theme_defaults` answer for `x;y` emits `:root{--x\;y:red}` and a
  name carrying a `:` names no declaration at all. `@supports (--x\3b y: red)`
  builds its declaration the same way (#439)
- `Css.inline_vars` preserves runtime-marked `var()` references, including
  typed fallbacks simplified through scalar values or shorthands, instead of
  replacing browser-time override points with compile-time defaults (#315)
- A registered `<color>` custom property is promoted and fills the colour slot
  of a `box-shadow` inside `@keyframes`, so the same declaration minifies the
  same way wherever it sits (#337)
- A custom-property name written with an escape is printed with the escapes
  CSS Syntax 3 sec. 4.3.7 needs to read it back as the same name.
  `:root{--x\3b y:red}` printed `:root{--x;y:red}`, which cascade's own reader
  splits into two declarations, and a reference to it printed `var(--x}y)`,
  which closes the rule around it. The declaration name, the `var()` reference
  and every shape of its fallback, the `@property` prelude and a `style()`
  container query all take the escaping (#429)
- `Css.inline_vars` keeps the `@property` registration of a custom property it
  keeps live. Every registration was dropped, so a property the pass could not
  inline safely lost the `initial-value` its references fall back on and the
  `inherits: false` that stops it inheriting, repainting the page; and the pass
  missed its own fixpoint, the second run pruning the declaration the first had
  kept (#416)
- `Css.inline_vars` reads a `style()` container query as a reference to the
  custom property it queries. CSS Conditional 5 sec. 6.2 evaluates the query
  against that property's computed value, so deleting the declaration behind it,
  or the `@property` registration standing in for one, leaves a block that no
  longer matches (#423)
- Pruning unreferenced custom properties counts a `var()` in a `@keyframes`
  frame, `@page` and its margin boxes, `@position-try` or
  `@supports-condition` as a reference, instead of deleting a binding those
  at-rules still use (#341)
- `Css.inline_vars` counts a `var()` in a page margin box or a
  `@supports-condition` body as a reference, so it no longer emits a name whose
  definition it deleted, and reports an overridden variable referenced from any
  of those at-rules through `~warn` (#342)
- `Css.inline_vars` unwraps an `@layer` and drops an `@property` registration
  written inside a rule, as it already did at top level. CSS nesting puts both
  there, and a sheet using it came back half cleaned (#373)
- `Css.custom_props` reports a name declared inside `@scope`,
  `@starting-style`, `@-moz-document`, `@when`, `@else` or a bare nesting
  block, as it already did for `@media` and `@supports`. Those declarations
  reach the matching element just as an `@media` one does, and the names were
  missing from an answer callers use to decide what a sheet defines (#375)
- A custom property registered by `@property` is typed wherever it is
  declared, including inside `@keyframes`, `@position-try` and
  `@supports-condition`, so the same registered value canonicalises the same
  way in every one of them (#349)

### Canonical diff

- `Css_compare.equivalent_value ~property` spells the property name the way the
  printer spells it before giving the two values that declaration context. A
  name carrying a `}` (CSS Syntax 3 sec. 4.3.7 puts one there through an escape)
  closed the rule and took both values with it, so any two values under such a
  name compared equal (#440)
- A fully transparent missing-axis `oklab()` colour, such as
  `oklab(0% none none / 0)`, canonicalises to transparent black while
  non-transparent forms remain distinct (#312)
- Relative-colour functions retain their origin as a typed colour, so
  equivalent spellings such as `red` and `#f00` compare equal in `rgb()`,
  `oklab()` and the other relative functions, including inside custom
  properties (#313)
- A complete shadow value in an unregistered custom property types its colour
  slot, so named and hex colours and typed `var()` fallbacks compare
  canonically while non-colour identifiers remain opaque (#314)
- The projection's value folds reach inside every block at-rule. A quoted
  multi-word family name in a custom property, a whole-byte `color(srgb ...)`
  and the Level 3 `not all and (...)` media spelling folded inside `@layer` and
  `@media` but not inside `@scope`, `@starting-style`, `@-moz-document`,
  `@when`, `@else` or an origin wrapper, so the diff's verdict depended on
  which at-rule enclosed the rule. A custom property in a `@keyframes` frame or
  a `@page` box folds too (#393)

### Library

- `Cascade_diff.Tree_diff.has_container_added_of_type` and
  `has_container_removed_of_type` look inside a container reported as modified,
  as `count_containers_by_type` already did. A `@supports` added inside an
  existing `@media` was counted but not found (#395)
- `Cascade.Resolve.Make.resolve` and `Cascade.Resolve.layer_order` document
  every block they leave out, not just conditional groups: `@starting-style`
  declares a before-change style, `@scope` brings a scoping root and the
  proximity criterion, and an origin wrapper carries an origin that outranks
  the layer. `Css.layers` remains the exhaustive count of what a sheet declares
  (#394)
- `Css.Media.kind` classifies a negated width bound by the side it bounds, so
  `not (min-width: 640px)` groups and sorts with the upper bounds it matches
  instead of with the lower bound it negates, and a doubled `not` cancels. The
  negation of a range, such as `not (640px <= width <= 1024px)`, is `Other`:
  it matches the viewports on either side of the range, which no single bound
  describes. `Css.Media.sort_key`, `group_order` and `compare` follow (#328)
- `Css.Stylesheet.statement_declarations` returns the declarations a statement
  holds directly; paired with `statement_children` it reaches every declaration
  in a stylesheet (#317)
- `Css.Stylesheet.map_statement_children` and `map_statement_declarations`
  rebuild a statement with a function applied to the block or the declarations
  it holds, the rebuilding counterparts of the two readers (#337)
- `Css.Flatten` carries the parent selector into an `@-moz-document` block, the
  one group rule it did not descend into. A nested declaration run came out
  bare at the top of the block, which no reader takes back (#384)
- `Css.Stylesheet.map_statement_children` and `map_statement_declarations`
  return the very statement they were given when the function they run leaves
  every list physically unchanged. A pass that short-circuits on physical
  equality can now call them instead of hand-rolling its own walk (#355)
- `Css.Stylesheet.fold_statements`, `iter_statements`, `fold_declarations`,
  `iter_declarations` and `map_declarations` walk a whole block: every statement
  a rule nests, and every declaration an at-rule holds outside a block. The
  declaration walks take `?sites` to name the places a narrow walk wants, so a
  place added to `declaration_sites` stops every walk that made a choice from
  compiling (#356)
- `Css.Stylesheet.edit_statements` rewrites a whole block: the walk hands each
  statement to a function that keeps, replaces or drops it, and descends into
  what survives, so a pass that drops a statement no longer carries its own
  list of the at-rules that nest (#363)
- `Css.Stylesheet.at_declaration_site` answers whether a statement holds its
  declarations in one of the places a `declaration_sites` record names. A walk
  that carries a cascade layer or an `@supports` depth down the tree cannot be
  a fold, and can now name the sites it reads instead of matching on the
  statements it expects to meet (#368)
- `Css.map` and `Css.sort` reach a rule inside `@scope`, `@starting-style`,
  `@-moz-document`, `@when` or `@else`, as they already did for `@media` and
  `@supports`. Those at-rules group style rules like any other conditional
  group, and a caller rewriting or reordering "all rules at all nesting levels"
  got a sheet with five of them silently untouched (#381)
- `Css.layers` and `Css.layer_block` find a layer declared inside `@media`,
  `@supports`, `@container`, `@scope` or any other grouping at-rule. Such a
  block declares a real layer, so a caller asking which layers a sheet declares
  was given a wrong answer rather than a partial one; `layers` reports every
  name it finds in source order (#382)
- `Css.vars_of_rules` is `Css.vars_of_stylesheet`, so a `var()` inside a nested
  rule or inside any at-rule is reported instead of only one a top-level rule
  holds. Both fold over every declaration site, which also adds the references
  an animation frame and a page margin box hold (#382)
- `Css.Stylesheet.layers` is what `Css.layers` calls, so the two no longer give
  different answers to the same question: the lower one reported neither a
  layer declared inside a grouping at-rule nor the sublayer a dotted name
  declares. `Css.Stylesheet.layer_block` is exposed alongside it (#389)
- `Css.Stylesheet.media_queries`, `container_queries` and `Css.media_queries`
  report a query written inside a grouping at-rule, and pair each query with
  every rule below its brace rather than the ones sitting directly under it, so
  a rule nested in another rule or held by an inner group is no longer missing
  from the query it is written in (#389)

### CLI tools

- `cascade apply` keeps the comments a page holds. React writes an empty
  comment between two adjacent text nodes to keep them apart, and a browser
  measures and rounds each text run on its own, so merging them moved the
  element's width; licence headers and conditional comments went missing
  outright. The page is parsed and printed with markup.ml, which carries
  comments, in place of lambdasoup, which discards them (#346)
- `cascade diff` names an at-rule that carries no condition of its own by the
  head it prints to, rather than describing every one of them identically. That
  description keys the ordering comparison, so a `@media` that moved between a
  `@page` and a `@starting-style` was reported as no change (#345)
- `cascade diff --diff=tree` shows the contents of a block added or removed
  wholesale whatever the block holds. The report read the body through its own
  list of at-rules, so a `@scope`, a `@starting-style` or a rule nested in
  another rule printed a header with nothing beneath it and read as empty (#389)
- `cascade apply` keeps a declaration in the sheet when a kept rule can write
  the same cascade slot under another property name. `p{margin:0}` was
  projected into a style attribute while the `.my-7{margin-top:1.75rem}` it
  loses to stayed behind, and a style attribute outranks every selector, so the
  paragraph rendered with no margin (#340)
- `cascade apply` empties the `<style>` blocks it projects instead of removing
  them. A `<style>` element is a sibling like any other, so unlinking it stops
  a kept rule such as `.navbox + style + .portal-bar` from matching what it
  matches in the browser (#339)
- `cascade apply` reads a `style` attribute as a declaration list in source
  order. The declarations came out reversed, so a longhand beat the shorthand
  it was written after, and an empty attribute was enough to trigger it. A `}`
  inside the attribute closed the list early instead of staying a preserved
  token (CSS Syntax 3 sec. 5.4), so `style="color:red}p{color:lime"` applied
  `color:red`, where the whole attribute is one invalid declaration a browser
  drops. Both made the projected page render differently from its input (#326)
- `cascade apply --minimal` drops an inherited declaration only when it truly
  restates the value the element would inherit. Two kinds of declaration it
  dropped are not restatements: one whose property a user-agent rule declares
  for the element or for one of its ancestors, which css-cascade-5 sec. 6.1
  sorts by origin before inheritance, so the drop uncovered the UA value for
  link colour, heading size, `b` weight, `em` style, `pre` family and `ol`
  marker; and one whose value resolves against the element it lands on, where
  a font-relative unit, a percentage, `larger`/`bolder`, a container unit,
  `var()`, `light-dark()` or `currentColor` inside `color-mix()` names two
  values across two elements, so `div{font-size:2em}div p{font-size:2em}`
  halved the paragraph (#326, #329)
- `cascade apply --minimal` keeps a restated inherited shorthand when an
  element in between sets one of the longhands it resets. A shorthand resets
  every longhand it does not mention (css-fonts-4 sec. 2.7 for `font`), so the
  restatement is what puts that longhand back, and
  `#p{font:16px serif}#mid{font-weight:bold}#c{font:16px serif}` left `#c`
  bold. `all` and a property cascade does not type reset everything, so neither
  is dropped either (#332)
- `cascade diff` spends less time on a rule holding many declarations. Deciding
  whether a reorder is significant asked each pair of declarations for its
  overlap keys and for its position in the other rule, both of which name a
  property through a fresh buffer, where each is a fact about one declaration.
  `--minify` canonicalises a rule's declaration order the same way (#424)
- `cascade diff` names a rule's nested block after the rule it belongs to. The
  container printed as `& .a`, which is a selector matching a `.a` inside the
  parent rather than the parent's own block, and a run of declarations written
  after a nested statement was named by its first declaration instead of the
  `&` that CSS Nesting 1 sec. 3.4 makes it (#385)

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
