---
paths:
  - "lib/pp.ml"
  - "lib/pp.mli"
---

# The printer is a pure serializer

`Pp` serializes the AST it is given. It may only pick a different spelling of
the same node: float and whitespace formatting, separators, the shortest
equivalent lexical form. It may not change the node.

The litmus test is `parse a = parse b`. If the two spellings parse to different
ASTs, the rewrite is not a printing choice and belongs in `Optimize`, which
maps AST to AST. Colour cross-folds, `calc` folding, unit and keyword folds,
and `scale(x,x)` to `scale(x)` are all node-changing and all live there.

Keeping this line is what makes factoring deterministic and idempotent: two
declarations that minify to the same text have to hash the same, and a fold
smuggled into the printer breaks that without failing any printer test.

Test both forms per scenario, minify alone and minify with `--optimize`, and derive
the held form from the spec rather than from what the code currently prints.
