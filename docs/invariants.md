# Invariants

This page specifies what cascade preserves when it minifies a stylesheet and
what the canonical comparison promises. Code that departs from a statement
here is a bug in the code. [Evidence](#evidence) maps each statement to the
tests that check it, and [design.md](design.md) records how the
implementation keeps them.

## Definitions

- **B, the browser set.** In the default mode, B is every release of each
  target engine from its floor to the newest release in the compatibility data
  cascade ships, under default settings. The floors are Chrome 111, Firefox
  128, Safari 16.4 and iOS Safari 16.4. B is dated and finite, so a deletion made
  on target grounds is checked again when the data is refreshed. Under
  `--enforce-spec`, B is every conforming implementation, and no rewrite or
  deletion rests on what a particular browser parses.
- **Parses and types.** A browser *parses* a rule, selector or declaration
  when it keeps it after its own error recovery. Cascade *types* a declaration
  when its reader gives it a property and a value of that property's grammar;
  a declaration it does not type is kept as an opaque token sequence.
- **Observational equality over B.** Two stylesheets are observationally equal
  when, for every HTML document, every surrounding context (sheets before and
  after, inline styles, the UA sheet) and every environment (viewport, media,
  user preferences, interaction state), every browser in B gives every element
  and pseudo-element the same used values at rest and the same interpolation
  (transitions, animations, gradients, `color-mix()`), up to the bounds of
  [M6](#master-guarantees). CSSOM text, the spelling `getPropertyValue` and
  `getComputedStyle` return, is not part of it. A custom property's token
  stream is the exception and is preserved as written apart from whitespace,
  because `var()` substitutes it and its spelling can change rendering
  (`--z:1.0` is not an `<integer>`).
- **Cascade stratum.** CSS Cascade 5 sec. 6.1 sorts two declarations of one
  property on one element by origin and importance, context, element-attached
  styles, layer, specificity and order of appearance. A stratum is a set of
  declarations equal on every criterion but the last, so order of appearance
  decides only inside a stratum.
- **Independence.** Two declarations are independent when no element can
  match both under both sets of conditions, or when they write no common
  longhand. A shorthand writes every sub-property, reset-only ones included,
  and a flow-relative property writes every physical property it can map to.
- **Assumptions.** Documents are HTML. Three opt-in flags add a hypothesis
  about the page: `--scope=stylesheet` that the input is the whole author CSS,
  `--closed-world` that no element matches two selectors whose rules conflict,
  and `--inline-vars` that no script changes the custom properties it inlines.
  None of them fixes the writing mode, the direction or any other runtime
  value. Each guarantee below holds under the hypotheses of the flags given.

## Master guarantees

- **M1.** `--minify` output is observationally equal over B to its input, in
  every context, for every objective, with one intended rendering change: a
  vendor-prefixed declaration a browser in B needs is added beside its
  unprefixed twin ([Target layer](#target-layer)), so that browser applies
  what the other browsers in B apply.
- **M2.** `cascade diff --diff=canonical` exits 0 only when the two inputs are
  observationally equal over B, modulo the prefixes the targets need: such a
  prefix is written on both sides, the comparator's side of M1's exception.
  So `.a{user-select:none}` and `.a{-webkit-user-select:none;user-select:none}`
  compare equal on purpose, although Safari, which reads the property only
  prefixed, applies it under the second alone. A reported difference may paint
  nothing; over-reporting is allowed and each case is a defect to reduce.
- **M3.** The canonical form of a sheet equals the canonical form of its
  `--minify` output. This checks that minifier and comparator agree, not that
  either is right; the browser is the oracle for both.
- **M4.** `--minify` is idempotent, `minify (minify S) = minify S`, and
  deterministic: one input and one set of flags give one output.
- **M5.** The objective (`transfer` or `raw`) only chooses among
  observationally equal outputs.
- **M6.** A number is equal up to six significant figures and a colour up to
  the fold budget (0.002 ΔE<sub>OK</sub>; a functional alpha to the fewest
  decimals, from three, that paint its byte read as a double and as a 32-bit
  float). Each bound is chosen so that no browser in B paints a different
  pixel. `--lossless` sets every bound to zero. The comparator applies the
  same bounds and none looser.

## Lexical layer

- **The printer round-trips.** Reading the print of a node, pretty or
  minified, gives that node back. A spelling the printer chooses (`.5`,
  `#fff`, `:before`, elided whitespace) is one that reads back as the same
  node; a rewrite that changes the node (`#f00` to `red`, `calc` folding,
  `flex:1 1 0%` to `flex:1`) belongs to normalisation, not to the printer.
  The pretty printer writes back the spelling the author wrote where the node
  keeps it (`0.90rem`, `#0000FF`).
- **A number prints within one budget and no further.** A number prints at
  eight decimal places, rounding the shortest decimal that reads back as its
  double, or at eight significant digits where that rounds to zero. A number
  whose magnitude, scaled to the eight places, reaches 2^62 prints the
  shortest decimal itself, which reads back as the same double: a browser
  adds a `calc()`'s operands in double precision before it clamps a length,
  so a coarser print moves the sum.
- **Token boundaries are safe.** Elided whitespace never joins two tokens into
  one or splits one into two. Inside a math function the whitespace around
  `+` and `-` is kept (CSS Values 4 sec. 10.8), and so is the whitespace
  beside a `var()`, `env()` or `attr()` that may substitute one.
- **Strings, URLs and idents read back as the value they name.** Each is
  written escaped as CSSOM 1 sec. 2.1 serialises a string, a URL and an
  identifier, whatever characters it holds.
- **A custom property's token stream is written as read**, apart from
  whitespace (CSS Variables 1 sec. 2.1 and 4.1).

## Value layer

- **Normalisation preserves observational equality and is idempotent.** For a
  typed declaration `d` and `n = normalize d`, `normalize n = n`, and reading
  `n`'s print back and normalising gives `n`. Two spellings of one value reach
  one normal form, so the first `--minify` merges what a second would.
- **A zero loses its unit only where the grammar reads it as a length.** A
  bare `0` replaces a zero length only at a whole `<length>` slot with no
  `<number>` or `<percentage>` alternative, never inside a math function,
  where CSS Values 4 sec. 10.9 types it as a `<number>`. CSS Flexbox 1 sec. 7.1 reads a
  unitless zero in `flex` as a flex factor, so `flex:0px` keeps its unit.
- **A number and a percentage swap only at a slot whose type is known.** A
  math function takes its type from its operands (CSS Values 4 sec. 10.9),
  and a `var()` supplies its type only at substitution, so a math function
  holding a `var()`, and a `var()` fallback, keep every operand's type.
- **A `var()` component can stand for several.** A `var()` or `attr()` is
  substituted before the value is parsed (CSS Variables 1 sec. 3), so one
  written as a gradient stop's colour can supply its position or several
  stops. No rewrite reverses a stop list holding one, drops that stop's
  position or folds it with a neighbour.
- **A math function stands only where its type does.** An inverse
  trigonometric function answers an `<angle>` (CSS Values 4 sec. 10.4), so it
  is refused at a slot that takes no angle and is an angle inside `sin()`.
- **A property's range binds only a literal.** CSS Values 4 sec. 10.12 clamps
  a math function past the range at computed-value time, so a math function
  of either sign is kept, and a fold keeps the wrapper around a result the
  bare literal could not spell.
- **A percentage folds through a non-linear function only over a basis that
  cannot be negative.** A comparison, `abs()`, `sign()`, `hypot()` or a
  stepped function over a percentage turns on the sign of the basis it
  resolves against; in `background-position`, `mask-position` and
  `object-position` that basis is the box less the image (CSS Backgrounds 3
  sec. 3.6), so such a call stays as written there.
- **A math function folds over units with a fixed ratio.** CSS Values 4 sec.
  10.7 converts `s` and `ms`, `deg`, `grad` and `turn`, and the absolute
  lengths before it compares or sums them, so `min(1s,1000ms)` is `1s`. A call
  that still cannot fold is one value, and it prints bare wherever the slot
  reads the bare call back to the same node.
- **A custom property is single-valued only by `@property` registration.** A
  `calc()` identity that drops an operand (`x * 0`) folds only over an operand
  with no `var()`, over a `<number>` or `<integer>` registration, or over a
  `<length>` registration at a position that needs a length (CSS Values 4
  sec. 10.9), where the product is a zero length.
- **A registration types its property only where every browser in B applies
  it.** CSS Properties and Values API 1 sec. 2.1 lets `CSS.registerProperty()`
  and a later `@property` replace a registration, so a custom property takes a
  registration's type only under `--scope=stylesheet`, from the last
  `@property` for its name, outside any conditional group rule. Under
  `--enforce-spec` an `@property` the current sec. 3 reads and a shipping
  engine drops is kept, and its name has no known type.
- **A colour keeps its interpolation class.** CSS Color 4 sec. 13.3
  interpolates two legacy sRGB colours in gamma-encoded sRGB and other colours
  in Oklab, so a colour that can be interpolated keeps a spelling of its
  class. A missing component (`none`) folds to zero only where the colour is
  not interpolated (sec. 4.4 and 13.4).
- **A colour channel on the edge of a byte is kept as written.** A colour
  channel paints as one of 256 bytes. It is decidable when its number
  quantises to one byte read as a double, as the nearest 32-bit float and at
  the eight decimals the printer writes. An engine may read a number
  either way (Chromium 153 reads a comma-syntax `rgba()` alpha as a double and
  the other channels as 32-bit floats), and cascade may print the
  eight-decimal value, so when the three agree every one of those paths paints
  alike. The rule holds for every number of a colour function, beside a
  `var()` as much as in a static colour. A colour with an undecidable channel
  is kept in its own function, syntax and digits, bar the whitespace CSS reads
  as nothing, and equals only its own spelling; substituting its `var()`
  reads it again. Only Chromium is measured; for Firefox and Safari the rule
  is conservative.
  interpolates two legacy sRGB colours (hex, named, `rgb()`, `hsl()`,
  `hwb()`) in gamma-encoded sRGB and other colours in Oklab, so a colour that
  can be interpolated keeps a spelling of its class. A missing component
  (`none`), the alpha included, folds to zero only where the colour is not
  interpolated (sec. 4.4 and 13.4). Any colour value can be: a gradient, a
  `color-mix()` reading it through `currentcolor`, an animation's implicit
  keyframe, or a transition any sheet, user style or script declares, so the
  minifier and the comparator keep both in every mode, `--scope=stylesheet`
  included. A
  `color-mix()` is a colour of its interpolation space (CSS Color 5 sec. 3),
  never a legacy one.

## Declaration layer

- **A fallback is dead only when every browser in B that parses it also
  parses the declaration after it.** An earlier declaration that a later one
  of the same element and stratum covers is what a browser paints when it
  parses the earlier and drops the later.
- **A substitution keeps its place.** A `var()`, `env()` or `attr()` beside
  other components may supply any number of them, commas included (CSS
  Variables 1 sec. 3), so no component is added, dropped, merged or moved
  around one; a component holding none may be respelled where it stands. A
  substitution that is the whole value, or an operand of a math function,
  yields one value and leaves the rest of the declaration free to fold.
- **A declaration holding a substitution is never invalid.** CSS Variables 1
  sec. 3 takes it as valid at parse time, so no check of its operands' types
  deletes it before substitution.
- **A shorthand covers what it resets.** A shorthand covers an earlier
  longhand only if it writes that longhand, reset-only sub-properties
  included.
- **A shorthand built from part of its longhands resets only what nothing
  sets.** The longhands it leaves out go to their initial values, and author
  CSS beats the user-agent sheet (CSS Cascade 5 sec. 6.2). `--scope=stylesheet`
  rules out author CSS outside the sheet, so such a shorthand is built there
  only when no other rule of the sheet, in any block, and no user-agent rule
  or presentational hint of the HTML rendering section (sec. 15) sets one of
  them. Without the flag it is not built.
  So `border-width`, `border-style` and `border-color`, or the four side
  shorthands, become `border` only under `--scope=stylesheet`, since `border`
  also resets `border-image` (CSS Backgrounds 3 sec. 4.4).
- **`!important` moves a declaration to another stratum.** Deduplication and
  shorthand composition never combine a normal and an important declaration.
- **Logical and physical pairs keep their order.** A flow-relative property
  and a physical property it can map to are not independent, since the
  writing mode and direction decide which slot they share.
- **Composition moves no declaration past one it meets.** Folding longhands
  into a shorthand writes them where the shorthand stands, so it happens only
  past declarations of the same stratum that write none of their slots.
- **List longhands compose entry by entry.** A shorthand over comma lists
  (`background`, `mask`, `animation-range`) writes each layer or range whole,
  while a short longhand list repeats to the length of the image or name list.
  Longhands fold into one only when their lists have one length, none holds a
  `var()` (which can stand for another slot or several entries) and none is a
  CSS-wide keyword.

## Rule layer

- **Order matters only inside a stratum between rules that are not
  independent.** A rewrite may move a rule past any rule it is independent of,
  and past none other.
- **A rule writes everything its body holds.** Each rule nested in it, at any
  depth, writes under its composed selector (CSS Nesting 1 sec. 3), a nested
  conditional group's rules write under its condition, and a declaration
  written after a nested statement writes in its own position (sec. 3.4).
  Two rules are independent only when every write of one is independent of
  every write of the other.
- **A rule holding `revert-rule` keeps its bounds.** CSS Cascade 5 sec. 7.3.6
  rolls such a value back as if the style rule holding it were absent, so no
  rewrite merges that rule with another, moves its declarations into another
  rule (a parent's included, CSS Nesting 1 sec. 3.4), or lets one of them
  shadow a write of an earlier rule. The keyword counts whole, as a
  substitution fallback, and in a custom property's value.
- **Selectors share a list only when every browser in B parses both or
  neither.** A browser drops a whole list over a selector it does not parse
  (Selectors 4 sec. 3.9), so a list some browser in B parses apart is one
  rule, never the rules of its selectors, and a rule no browser in B parses
  applies nowhere.
- **A selector enters or leaves a forgiving `:is()` only where every browser
  in B parses it.** Sec. 4.2 drops an argument a browser does not parse from
  the `:is()` alone, so `.c,:is([x=a s])` keeps `.c` in Chrome and
  `.c,[x=a s]` does not; `:not()` is not forgiving, so `:not(:not(s))` is
  `:is(s)` only on the same terms.
- **Nesting is emitted only where every browser in B parses it.** Nesting is
  neither synthesised nor relaxed into a form an older member of B misreads.
  Chrome 111 and Safari 16.4 read no nested rule, so under the default floors
  and under `--enforce-spec` an author's nesting is flattened, which sec. 4
  and 5 of CSS Nesting 1 make exact. A nested declarations rule (sec. 3.4 and
  5), which releases that parse nesting read in another order until Chrome
  130, Firefox 132 and Safari 18.2 (BCD `api.CSSNestedDeclarations`), stays
  only where every browser in B is at or past those releases, and is
  flattened everywhere else. Where it stays, a nested `&` rule holding only
  declarations under a parent of one selector naming no pseudo-element is a
  nested declarations rule too, since the two match the same elements at the
  same weight (sec. 4 and 5).
- **A nested selector means what CSS Nesting 1 makes it mean.** `&` matches
  what `:is()` of the parent matches (sec. 4), so a parent branch carrying a
  pseudo-element gives it nothing to match, and a nested declarations rule
  matches the parent's elements and pseudo-elements at the parent's own weight
  (sec. 5). In an `@scope` block a selector holding neither `&` nor
  `:scope` has the scoping root implied at its start (CSS Cascade 6
  sec. 3.5.3), so a composed selector keeps that root where the parent had it.
  Flattening, merging a wrapper into its child and synthesis compose selectors
  under this reading.
- **At-rules keep their identity.** Of two `@keyframes` of one name in one
  layer the last wins, so dropping the earlier is safe and merging them is
  not. Every `@font-face` adds a face, so none is dropped for another.
  `@import` rules keep their order and position. A `@keyframes` or
  `@font-face` rule takes no part in the order of style rules, so it may move
  past any of them, but not past another definition of its name or family. A
  layer declared inside a conditional group rule contributes its order only
  when the condition holds (CSS Cascade 5 sec. 6.4.3).

## Target layer

- **A guard is unwrapped only if it holds in every browser in B, and deleted
  only if it fails in every browser in B.** A declaration leaf holds in a
  browser that parses its declaration, value included, so `(width:stretch)`
  holds only where `stretch` parses. `not` reverses a leaf, so a feature true
  from one release makes `not (feature)` false from that release on. A
  `<general-enclosed>` term is false in every browser in B (CSS Conditional 3
  sec. 6), so a guard over one alone is deleted.
- **A vendor-prefixed declaration is deleted only if no browser in B needs
  it**, and one a browser in B needs is added beside its unprefixed twin.
- **Nothing untyped is dropped.** What cascade cannot type is never assumed
  dropped by a browser in B. A declaration the reader refuses and some browser
  in B parses is a reader bug. A form no specification grants and a browser
  in B parses is kept as the author wrote it, never rewritten into a granted
  form, which would apply where that browser alone applied it.

## Modes

- **Default.** B as defined above, every bound as M6 states it.
- **`--enforce-spec`** makes B every conforming implementation: a rewrite
  rests only on what the specifications prove equal, and guards, prefixes,
  fallbacks and rules over a pseudo-class no target reads are kept.
- **`--lossless`** sets every approximation bound to zero.
- **`--scope=stylesheet`, `--closed-world` and `--inline-vars`** add the
  hypotheses under [Assumptions](#definitions) and keep B.
- **`--flatten-nesting`** keeps B and removes nesting from the output.

## Non-goals

- CSSOM text, apart from a custom property's token stream.
- Byte identity with the input, comments included.
- `@scope` proximity: a rewrite does not reason about it, so `@scope` blocks
  are opaque to it.

## Evidence

Each row names the tests that check a statement. A browser harness under
`test/render` or `test/spec/browser` skips without a headless Chromium.

| Statement | Held by |
|---|---|
| M1 | `test/render` "render_diff" (each transform paints the source's page in Chromium); the [release criterion](reference.md#release-criteria) |
| M2 | `test/render` "canonical_agree" (a pair called identical paints one page); `test/test_css_compare_structure.ml` |
| M1's prefix exception | `test/test_webkit_fallback.ml` "synthesis writes the pair", "an authored prefix is left alone" |
| M2's prefix exception | `test/test_css_compare_structure.ml` "canonical legacy name alias" ("a prefix a target needs is written on both sides"), "canonical judges for the targets" ("a prefix a target needs is on both sides", "a prefix a target needs survives a merge", "a mask composite's prefixed vocabulary too", "a supports guard a target reads prefixed too") |
| M3 | The release criterion, over 504 SatCSS sheets and Tailwind's output under both objectives |
| M4 | `test/fixpoint` (its `spellings` corpus holds the second pass still); `optimize_factor` "pp minify is idempotent"; `test/test_optimize.ml` "optimize preserves physical identity on a fixed point" |
| M5 | The release criterion, run under both objectives |
| M6 | `test/test_css_compare_values.ml` "canonical numeric precision modes"; the css-minify-tests colour fixtures |
| Printer round trip | `test/fixpoint` "values"; `test/test_declaration_identity.ml` "hex spellings have one node", "number spellings have one node"; `fuzz/fuzz_supports.ml` "generated tree reparses" (`@supports` conditions of nested `not`, `and` and `or`) |
| M6 | `test/test_css_compare_values.ml` "canonical numeric precision modes"; `test/test_declaration_identity.ml` "colour channels beside a var()" (the alpha rounding); the css-minify-tests colour fixtures |
| Printer round trip | `test/fixpoint` "values"; `test/test_declaration_identity.ml` "hex spellings have one node", "number spellings have one node" |
| Number budget | `pp` "float int boundary", "float large decimal reparses", "float large decimal in stylesheet"; fuzz `pp` "string_of_float rounds by its stated rule" |
| Token boundaries | `fuzz/fuzz_parser.ml` "component-value token boundaries roundtrip"; `test/test_lexer.ml` "spec token boundary edges" |
| Strings, URLs, idents | `pp` "string printers round-trip"; fuzz `pp` "string printers read back"; `selector` "identifiers escape control characters"; `stylesheet_families` "spec cssom 1 2.1 control characters in names" |
| Custom-property tokens | `test/test_declaration_identity.ml` "custom declarations ignore offsets" |
| Normalisation | `test/fixpoint` "normal form" and its `spellings` corpus; `test/test_declaration_identity.ml` "number spellings have one node" |
| Zero unit | `properties_layout` "flex"; `optimize_declarations` "target minify and enforce-spec split" |
| Number and percentage | `v4109_percentage_beside_var` in `test/test_stylesheet_calc.ml` |
| `var()` components | `declaration` "complex values"; `properties_background` "background-image" |
| Range | `values_math` "spec math range every family", "spec math infinite range" |
| Math result types | `values_math` "spec math inverse trig is an angle" |
| Registration | `v41011_zero_product_over_var` in `test/test_stylesheet_calc.ml`; `calc_zero_product_registered_number`, `calc_zero_product_registered_length` and `registration_known_only_in_closed_scope` in `test/test_optimize_boundaries.ml`; `runtime_var_keeps_its_calc` in `test/test_context_eval.ml` |
| Fixed-ratio units | `values_math` "spec math fixed-ratio units" |
| Zero operands | `values_math` "spec math zero operands" |
| Signed percentage basis | `values_math` "spec math signed percentage basis" |
| Whitespace beside a substitution | `values_math` "spec math var whitespace" |
| Undecidable channels | `test/test_declaration_identity.ml` "undecidable colour channels stay"; `test/test_css_compare_values.ml` "canonical undecidable colour channel"; the `test/fixpoint` `spellings` corpus |
| Undecidable channels | `test/test_declaration_identity.ml` "undecidable colour channels stay", "colour channels beside a var()"; `test/test_css_compare_values.ml` "canonical undecidable colour channel"; the `test/fixpoint` `spellings` corpus |
| Interpolation class | `test/test_css_compare_values.ml` "canonical missing components under interpolation", "under a transition"; no test of the legacy/Oklab class yet |
| Interpolation class | `values_color` "fold keeps interpolation class", "alpha none is missing"; `test/test_css_compare_values.ml` "interpolation class is a difference", "missing alpha is a difference", "canonical missing components under interpolation", "under a transition" |
| Substitution keeps its place | `declaration_shorthand` "spec variables 3 substitution takes no slot", "background leading var slot"; `test/test_optimize.ml` "var() colour functions preserved"; `test/test_css_compare_values.ml` "canonical shadow colour var fallback alpha" |
| Substitution never invalid | `values_math` "spec math substitution never invalid" |
| Fallbacks | `floor_fallbacks_survive`, `prefixed_fallbacks_across_rules` and `legacy_keyword_fallbacks_survive` in `test/test_optimize_declarations.ml`; `test/test_optimize_boundaries.ml` "spec values 10.2 clamp takes a none bound"; `equal_canonical_targets` in `test/test_css_compare_structure.ml` |
| Shorthand coverage | `optimize_declarations` "shorthand longhand rule order" |
| Partial shorthands | `stylesheet_scope_reset_held` in `test/test_optimize_boundaries.ml`; `test/test_shorthand_compose.ml` "font composes the whole run"; `optimize_declarations` "border keeps longhands beside a border-image", "border reset reads every block"; `test/test_css_compare_structure.ml` "canonical border sides" |
| `!important` | `test/test_optimize_boundaries.ml` "spec cascade 6.1 important shorthand blocks normal longhand"; `test/test_optimize_origins.ml` "spec cascade 6.3 later important beats earlier important" |
| Logical and physical | `optimize_declarations` "lossless keeps logical physical order" |
| Composition order | `optimize_declarations` "a box longhand folds past nothing it meets"; `shorthand_compose` "position-try composes past nothing it meets", "columns composes past nothing it meets"; `rule_order` "composition reads the order"; `fuzz/fuzz_rule_order.ml` "run projection is idempotent" |
| List composition | `shorthand_compose` "layer run is one plain layer"; `shorthand_compose` "timeline and range compose"; `declaration_shorthand` "mask boxes" |
| Rule order | `test/test_optimize_merge.ml` "spec cascade 6.1 conditional competitors keep source order"; `fuzz/fuzz_rule_order.ml` "independent rules commute" |
| Nested writes | `optimize_factor` "nested writes keep their order"; `test/test_css_compare_structure.ml` "canonical separates moved nested writes" |
| `revert-rule` | `test/test_optimize_boundaries.ml` "spec cascade 7.3.6 revert-rule keeps its rule"; `test/test_css_compare_structure.ml` "canonical keeps a revert-rule apart" |
| Selector lists | `test_compatible_past_the_floors` in `test/test_merge.ml`; `equal_canonical_targets` in `test/test_css_compare_structure.ml` (a list a browser reads apart is one rule, a list no target parses is no rule) |
| Forgiving `:is()` | `optimize_factor` "is keeps an argument some target drops"; `css_compare_structure` "canonical :is() keeps an argument some target drops" |
| Nesting | `test_nesting_follows_the_floors`, `test_nested_declarations_are_flattened` in `test/test_nest.ml`; `test/test_optimize_boundaries.ml` "conditional boundary blocks nesting synthesis" |
| Nesting | `test_nesting_follows_the_floors`, `test_nested_declarations_follow_the_floors` in `test/test_nest.ml`; the nested-declarations seeds of `test/render/render_diff.ml`; `test/test_optimize_boundaries.ml` "conditional boundary blocks nesting synthesis" |
| Nested selectors | `test_nesting_selector_names_no_pseudo_element`, `test_pseudo_element_parent_drops_invalid_compound` in `test/test_flatten.ml`; `test_rules_keep_a_pseudo_element_parent_flat` in `test/test_nest.ml`; `canonical_nested_and_flattened_selectors_are_equal` in `test/test_css_compare_structure.ml`; `test_scope_keeps_the_implied_root`, `test_nested_scope_roots_under_the_parent` in `test/test_flatten.ml`; `test_merge_lone_in_scope`, `test_nesting_selector_is_not_reparented` in `test/test_nest.ml` |
| At-rule identity | `test/test_optimize_origins.ml` "spec cascade 6.4 keyframes name collisions are layered", "spec cascade 6.4 conditional layer declarations stay nested"; `test/test_optimize_boundaries.ml` "spec cascade 2 import keeps substitution point"; `test/test_css_compare_structure.ml` "canonical name-defining rules move"; `test_distant_media_keeps_a_definition_in_place` in `test/test_block.ml` |
| Guards and prefixes | `equal_canonical_targets` in `test/test_css_compare_structure.ml` |
| Nothing untyped dropped | `test/spec/browser/accept_set` (every CSS length unit, `if()` and dashed functions among its values); `test/render/parse_recovery`; `properties_animation` "spec animations 3.11 negative delay"; `values_math` "spec math integer slots"; `declaration_spec` "contain-intrinsic-size keeps a trailing auto"; `test/test_declaration_spec.ml` "a substitution function defers the property", "a value a browser keeps reads"; `test/test_declaration_spec.ml` "mask shorthand keeps a three-value position"; `mask_three_value_position_kept_as_written` in `test/test_optimize_declarations.ml` |
| Modes | `optimize_declarations` "target minify and enforce-spec split"; `optimize_factor` "factoring weighs the flagged print"; `css` "nested colour follows print flags" |
| Assumptions | Convention only: no test exercises a page that breaks a hypothesis |
