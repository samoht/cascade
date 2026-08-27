---
paths:
  - "lib/optimize.ml"
  - "lib/factor.ml"
  - "lib/factor_safe.ml"
  - "lib/merge.ml"
  - "lib/cover.ml"
  - "lib/shorthand.ml"
  - "lib/rule_graph.ml"
  - "lib/rule_scheduler.ml"
  - "lib/rule_order.ml"
---

# The optimizer matches the typed AST

Never compare or `String.starts_with` a property name here. Pattern-match the
property constructors. A longhand with no constructor is untyped: give it a
type first, then match it. A string test silently covers the wrong set the
moment a property is renamed or a vendor twin appears.

`scope` picks the world the pass may assume. `` `Fragment `` is the default and
assumes the sheet may be one part of a page. `` `Stylesheet `` is closed over
the CSS text only, never over runtime layout: it may not assume a writing mode
or a direction, and it knows nothing about the DOM. Shorthand synthesis from partial
coverage needs `` `Stylesheet ``.

The default objective is transfer size, gated on a compressed-size estimate. A
rewrite that shrinks the raw bytes and grows the compressed output is not a
win. `` `Raw `` is the opt-out.

Fold only when the result is exactly value-preserving. An approximate `calc`
fold that is right for the sampled inputs is still wrong.

Factoring a value into a shared block needs selector-overlap reasoning: taking
the first rule's value is unsafe when a later rule repeats it and an
intermediate rule with an overlapping selector writes a different one.
