---
paths:
  - "lib/diff/**"
---

# Structural diff

The canonical projection decides what two sheets have to agree on. It may fold
a difference in spelling, and it may not delete content a browser still
paints. A progressive-enhancement fallback and a vendor-prefixed declaration
are both live, and so is an `@import supports()` guard. Deleting from both
sides makes sheets that render differently compare equal, which is the failure
mode this projection has.

The comparator may equate spellings the optimizer has to keep verbatim, since
a comparison emits nothing. That asymmetry is deliberate. It is not a licence
for a diff-only shim: canonical comparison inherits the optimizer's passes.

Rule order is judged on a longest order-preserving matching of the statements
both sides share, with containers among the order keys. Absolute rule indexes
are a different coordinate and the two disagree, so an index cannot always
express a move that the matching found. A renderer meeting a shape it cannot
spell reports less. It never asserts: the report is buffered, so raising there
costs the whole report rather than the one entry.
