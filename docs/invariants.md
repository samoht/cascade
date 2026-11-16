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
  buckets on, and keeps `optimize` a fixed point.
- **Type-changing rewrites fire only at typed leaves.** The number/percentage
  swap and zero-unit stripping are suppressed inside math-function value
  operands (`ctx.in_calc`) and `var()` fallbacks (`ctx.in_function`); lexical
  number formatting still applies there.
- **A custom property is single-valued only by `@property` registration**,
  never by inference from a definition's value.
- **Source-token fidelity is not a contract.** The AST carries no raw-text
  sidecars for byte-identical round trips.
- **The canonical diff inherits the optimizer.** It compares optimize+minify
  output and gets no comparison-only normalization of its own; the one
  exception is the `Tree_diff` comparison key
  (`Css.declaration_value_for_equivalence`), which may equate values the
  optimizer must not fold on emission, provided their type is proven.
  A phantom canonical difference on equal content is a projection gap in
  `rule_order.ml`, not a renderer bug.
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
