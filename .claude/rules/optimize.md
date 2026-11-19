---
paths:
  - "lib/optimize.ml"
  - "lib/factor.ml"
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

The default objective is transfer size: every candidate kind scores its saving
with `Transfer_size`, an LZ77-style estimate of the compressed bytes, so the
one queue compares kinds in one unit. A selector branch or a declaration
printed before among the candidate's rules costs a match, a new one its bytes;
the constants are fitted against gzip on the corpus, not chosen. The estimate
is a pure function of the candidate's rules and their printed order, so a
second pass scores the output the same way. It sees tokens, not where a
selector prints, so rules with one body still merge only where they print
within 128 rules of each other: a selector moved far from its neighbours stops
compressing against them. `` `Raw `` scores raw bytes and merges them at any
distance.

Fold only when the result is exactly value-preserving. An approximate `calc`
fold that is right for the sampled inputs is still wrong.

Factoring a value into a shared block needs selector-overlap reasoning: taking
the first rule's value is unsafe when a later rule repeats it and an
intermediate rule with an overlapping selector writes a different one.
