CLI: what a refuted @supports block takes with it, and what entailment leaves
alone.

Simplifying a nested [@supports] against the conditions enclosing it deletes a
block whose guard those conditions refute. CSS Conditional 3 sec. 2 says of a
false condition that processors "must not apply any of rules inside the group
rule", so the block has no effect left to preserve. Its cascade layers are the
case worth pinning, because a layer declaration reaches past the block it sits
in: CSS Cascade 5 sec. 6.4.1 answers it with "Layers that are defined inside of
a conditional group rule do not contribute to the layer order unless the
condition is true or unless the conditional group rule can evaluate differently
for different elements in the document." A feature query is answered once for
the whole document rather than per element, so a layer a refuted block declares
never entered the order and deleting the block cannot move it.


# A refuted block's layer order goes with it


The dead block declares [b] before [a] is declared anywhere. Since it applies
on no UA, the order the sheet establishes is [a] then [b], which is the order
left standing.

  $ cat > dead-layer.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @supports (not (nonsense-a: 1px)) { @layer b; }
  >   @layer a { .x { top: 0 } }
  >   @layer b { .x { top: 1px } }
  > }
  > EOF
  $ cascade --minify dead-layer.css
  @supports(nonsense-a:1px){@layer a{.x{top:0}}@layer b{.x{top:1px}}}

The control: nothing refutes this guard, so its block applies on the UAs that
answer yes and [@layer b;] keeps its place at the head of the order.

  $ cat > live-layer.css <<EOF
  > @supports (nonsense-b: 2px) { @layer b; }
  > @layer a { .x { top: 0 } }
  > @layer b { .x { top: 1px } }
  > EOF
  $ cascade --minify live-layer.css
  @supports(nonsense-b:2px){@layer b;}@layer a{.x{top:0}}@layer b{.x{top:1px}}


# The supports() clause of an @import is nested in nothing


An [@import] sits in the sheet's prelude with no conditional group rule around
it, so there is no context to decide its [supports()] clause against. It is
left as the author wrote it even when the clause decides itself, while the
[@supports] rule below it simplifies against its own context.

  $ cat > import.css <<EOF
  > @import url("x.css") supports((nonsense-a: 1px) or (not (nonsense-a: 1px)));
  > @supports (nonsense-a: 1px) {
  >   @supports (nonsense-a: 1px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify import.css
  @import"x.css"supports((nonsense-a:1px)or (not (nonsense-a:1px)));@supports(nonsense-a:1px){.r{top:0}}
