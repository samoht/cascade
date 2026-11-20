# Invariants

These hold across the printer, the optimizer and the canonical comparison. A
change that breaks one is a defect even when every test passes.

- **pp is a serializer.** `to_string`, pretty or minified, prints the node it
  is given and picks only among spellings that parse to the same node (`.5`,
  `#fff`, `:before`, whitespace). Anything that changes the node (`#f00` to
  `red`, `scale(x,x)` to `scale(x)`, `calc` folding, `0deg` to `0`) is an
  AST rewrite in the optimizer's normalize pass. The litmus is
  `parse(A) = parse(B)`, not whether the code is gated on minify. Dropping a
  `url()`'s quotes, `center center` to `center`, `flex:1 1 0%` to `flex:1`
  and a computed float to the eight decimals the printer writes are such
  rewrites too. The AST holds a `<string>` or a url as the string it names,
  never its source spelling, bar an image's url, which also records whether it
  was quoted until normalize settles that. Every printer writes one through
  `Pp.quoted_string`, `Pp.url` or, for an image url held bare,
  `Pp.url_token` (CSSOM 1 sec. 2.1, serialize a string and serialize a URL),
  so it reads back as that string whatever it holds (`pp` "string printers
  round-trip", fuzz `pp` "string printers read back").
- **Normalize reaches a normal form.** For a typed declaration `d` with
  `n = normalize d`, `normalize n = n`, and reading `n`'s print back,
  minified or pretty, and normalising gives `n` again. Two normal forms with
  one minified text are then one node with one `Declaration.hash`, which
  factoring buckets on, so the first `--minify` pass merges what a second
  would. `test/fixpoint` holds this over every typed declaration its corpora
  read ("normal form"), and its `spellings` corpus holds the second pass
  still. The reader itself may keep an authored spelling the printer does not
  write back (`#ffffff`, `500ms`); the "values" pending list there names
  those. Open gaps: an authored number past eight decimals, which the printer
  rounds and normalize keeps (`width:33.33333333333333%` and
  `width:33.33333333%`), and a custom property's tokens, which the printer
  respells and CSS Variables 1 sec. 4.1 keeps normalize away from
  (`--x:1.0px` and `--x:1px`).
- **Type-changing rewrites fire only at typed leaves.** The number/percentage
  swap and zero-unit stripping are normalize rewrites at an operand whose whole
  type is known: a bare value, or a math function with no `var()` in it. CSS
  Values 4 sec. 10.9 types a math function by its operands, and a `var()`
  supplies its type only at substitution. A math function holding one keeps
  every operand's spelling, and so does a `var()` fallback; lexical number
  formatting still applies there. The readers keep a percentage a
  percentage until then (`v4109_percentage_beside_var` in
  `test/test_stylesheet_calc.ml`). Zero-unit stripping also stops where the
  grammar reads a bare `0` as another type. CSS Flexbox 1 sec. 7.2 reads a
  lone number in `flex` as the grow factor, so normalize leaves `flex:0px` and
  never builds a lone `Zero` basis. That node has no spelling of its own, and
  pp prints it as `0px` (`properties_layout` "flex", `optimize_declarations`
  "target minify and enforce-spec split").
- **A custom property is single-valued only by `@property` registration**,
  never by inference from a definition's value. A `calc()` identity that drops
  an operand (`x * 0`, `0 * x`) folds only over an operand with no `var()`, or
  over a `<number>` or `<integer>` registration, which substitutes one number
  (`v41011_zero_product_over_var` in `test/test_stylesheet_calc.ml`,
  `calc_zero_product_registered_number` in
  `test/test_optimize_boundaries.ml`). `x * 1` and `x / 1` keep `x` whatever it
  substitutes, but not the `calc()` or parentheses around it. `Css.Context` and
  `--inline-vars` keep those around an unresolved `var()`
  (`runtime_var_keeps_its_calc` in `test/test_context_eval.ml`), and the
  optimizer drops a nested one only around a single-valued registration.
- **Source-token fidelity is not a contract.** The AST carries no raw-text
  sidecars for byte-identical round trips.
- **The canonical diff inherits the optimizer.** It compares optimize+minify
  output and gets no comparison-only normalization of its own; the one
  exception is the `Tree_diff` comparison key
  (`Css.declaration_value_for_equivalence`), which may equate values the
  optimizer must not fold on emission, provided their type is proven.
  A phantom canonical difference on equal content is a projection gap in
  `rule_order.ml` or `rule_trace.ml`, not a renderer bug.
- **The canonical projection is confluent.** It reads a run of style rules as
  the trace of its declarations: an entry per declaration under its selector
  and conditions, two entries ordered only where swapping them side by side
  could change a computed value, and the form printed is a function of that
  partial order and the entries' text. Where the run's rules begin and end,
  which occurrences of a selector were folded and in which order, never reach
  it. A fact that frees a pair by reading a third entry (a slot a later write
  of the group rewrites, a later twin of one in the other's group) never rests
  on an entry the projection drops: an entry is dropped only when it is
  redundant in the run's own order and no such fact about another entry of its
  group is read off it, so the order of what is left is that order less the
  entry. A legacy name alias (WHATWG Compatibility Standard sec. 3.4.1) reads
  as its property before the trace, so `-webkit-transform` and `transform` are
  two writes of one property wherever they stand. The projection compares
  declarations by node and reads them in normal form: the optimizer hands it
  normal forms, and the declaration it builds for an alias is normalised
  before it is compared. One pass around the trace
  still reads one rule at a time and is an open gap: shorthand composition
  after it, which can move a declaration past an untyped one the trace orders
  it against.
- **Judge may delete only what every browser at or above the floors
  ignores.** A target version is a floor: that release and every later one.
  The canonical comparison judges for the default targets (`?judge`), and a
  deletion it makes on that ground holds for every browser at or above them,
  not only for the floor versions. A floor that implements a production
  answers for the releases after it, so a `@supports` guard every floor
  satisfies is unwrapped. A floor that lacks one says nothing about them, so
  a guard no floor satisfies stays unless the data shows the feature never
  ships. The web-features dataset never shows that. `Supports.implemented_by`
  answers the question; `equal_canonical_targets` in
  `test/test_css_compare_structure.ml` holds it (`field-sizing` and
  `anchor-name` guards separate, `not (display:grid)` does not).
- **A fallback is dead only when every browser at or above the floors that
  reads it reads the declaration after it.** An earlier declaration that a
  later one of the same selector covers is what a browser paints when it reads
  it and drops the later one at parse time. The minifier and the judge decide that through one function,
  `Declaration.fallback_for`, asked of the same floors (`Ctx.floors`) by
  in-rule deduplication (`Shorthand_dedup.deduplicate_step`), by same-selector
  shadowing across rules (`Cover.covered`, `Rule.drop_shadowed`) and by the
  projection's shadowing (`Rule_order`). So whatever the minifier deletes the
  judge also counts dead, which the release criterion assumes. The minifier
  keeps more: without a judge it also keeps the fallback before a CSS Color 4
  colour or a runtime substitution. `floor_fallbacks_survive` in
  `test/test_optimize_declarations.ml` and `equal_canonical_targets` hold it.
- **Two selectors share a list only when every browser at or above the floors
  drops both or neither.** A browser drops a whole list over a selector it
  does not parse (Selectors 4 sec. 3.9). A selector a floor reads is read
  from then on; one a floor drops may be read from a later release, so
  `Merge.compatible` lists two such selectors only when every engine ships
  them in the same release, and one whose production no release ships lists
  only with selectors waiting on the same productions.
  `test_compatible_past_the_floors` in `test/test_merge.ml` holds it.
- **Rule rewrites that move source order reason about overlap.** Factoring a
  default value into a shared block keeps a rule's own declaration when an
  earlier overlapping rule writes a different value. Nesting synthesis is
  adjacent-only, within one cascade context, with an exact selector extension
  under a non-list parent, and only when it shortens the output. Byte savings
  never justify a rewrite that is not cascade-safe.
- **`--enforce-spec` never emits output every browser renders differently.**
  It drops browser-target facts; it does not license spec literalism
  (`flex:1 1 0` stays in both modes).
- **`` `Stylesheet `` scope closes over the CSS text, not the runtime.**
  Rewrites needing writing mode, direction or runtime custom-property values
  stay forbidden in every scope.
- **Speed on real stylesheets is a requirement.** A slow path on realistic
  input is a defect.
