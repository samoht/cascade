# Invariants

These hold across the printer, the optimizer and the canonical comparison. A
change that breaks one is a defect even when every test passes.

- **pp is a serializer.** `to_string`, pretty or minified, prints the node it
  is given and picks only among spellings that parse to the same node (`.5`,
  `#fff`, `:before`, whitespace). Anything that changes the node (`#f00` to
  `red`, `scale(x,x)` to `scale(x)`, `calc` folding, `0deg` to `0`) is an
  AST rewrite in the optimizer's normalize pass. The litmus is
  `parse(A) = parse(B)`, not whether the code is gated on minify. Normalize
  reaches the node the parser produces for the printed text, which keeps the
  `Declaration.hash` invariant (same minified text, same hash) that factoring
  buckets on, and keeps `optimize` a fixed point. The AST holds a `<string>`
  or a url as the string it names, never its source spelling, and every
  printer writes one through `Pp.quoted_string` or `Pp.url` (CSSOM 1 sec. 2.1,
  serialize a string and serialize a URL), so it reads back as that string
  whatever it holds (`pp` "string printers round-trip", fuzz `pp` "string
  printers read back").
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
  two writes of one property wherever they stand. One pass around the trace
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
