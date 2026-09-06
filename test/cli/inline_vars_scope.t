CLI: --inline-vars - scoping rules and cycles.

A variable declared at :root applies wherever the inliner can prove it
matches; defined on an arbitrary selector, the inliner conservatively
preserves the var() reference for non-descendant uses.

  $ cat > scoped-decl.css <<EOF
  > .theme { --c: red }
  > .theme .descendant { color: var(--c) }
  > .other { color: var(--c) }
  > EOF
  $ cascade --minify --inline-vars scoped-decl.css
  .theme{--c:red;.descendant{color:red}}.other{color:var(--c)}

A variable declared inside @media is folded only into consumers within
that @media block. A consumer outside keeps its var() reference, and the
definition goes to the output with it: the browser answers the reference
from its own cascade whenever the condition holds.

  $ cat > media.css <<EOF
  > @media (min-width: 30em) {
  >   :root { --brand: red }
  >   .a { color: var(--brand) }
  > }
  > .b { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-vars media.css
  @media(width>=30em){:root{--brand:red}.a{color:red}}.b{color:var(--brand)}

A cascade layer only orders competing declarations, it does not scope
custom-property visibility (unlike the conditional @media / @container),
so a variable declared inside @layer resolves for consumers inside and
outside the layer alike, exactly as without the layer wrapper.

  $ cat > layer.css <<EOF
  > @layer theme { :root { --brand: red } .a { color: var(--brand) } }
  > .b { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-vars layer.css
  .a,.b{color:red}

A variable redefined on the same element across layers has a statically
decidable winner (CSS Cascade 5 6.4.3), so it folds to that winner rather
than staying a live var(). For normal declarations the later layer wins.

  $ cat > layer-order.css <<EOF
  > @layer a { :root { --x: 1px } }
  > @layer b { :root { --x: 2px } }
  > .z { width: var(--x) }
  > EOF
  $ cascade --minify --inline-vars layer-order.css
  .z{width:2px}

An unlayered definition wins over a layered one, whatever the document
order.

  $ cat > layer-unlayered.css <<EOF
  > :root { --x: 2px }
  > @layer a { :root { --x: 1px } }
  > .z { width: var(--x) }
  > EOF
  $ cascade --minify --inline-vars layer-unlayered.css
  .z{width:2px}

A [revert-layer] winner rolls back to the value from the layer below it.

  $ cat > layer-revert.css <<EOF
  > @layer a { :root { --x: 1px } }
  > @layer b { :root { --x: revert-layer } }
  > .z { width: var(--x) }
  > EOF
  $ cascade --minify --inline-vars layer-revert.css
  .z{width:1px}

A variable used in a @container query value is preserved (container
queries evaluate at layout time, not at the syntax layer).

  $ cat > container.css <<EOF
  > :root { --bp: 30em }
  > @container (min-width: var(--bp)) { .x { color: red } }
  > EOF
  $ cascade --minify --inline-vars container.css 2>&1 | grep -v "warning"
  :root{--bp:30em}@container(width>=var(--bp)){.x{color:red}}

A variable used in a @media query value is preserved. Custom property
substitution only happens in property values, not media query syntax.

  $ cat > media-query-var.css <<EOF
  > :root { --bp: 30em }
  > @media (min-width: var(--bp)) { .x { color: red } }
  > EOF
  $ cascade --minify --inline-vars media-query-var.css 2>&1 | grep -v "warning"
  :root{--bp:30em}@media(width>=var(--bp)){.x{color:red}}

A variable used in an @supports condition is preserved for the same
reason.

  $ cat > supports-var.css <<EOF
  > :root { --display: grid }
  > @supports (display: var(--display)) { .x { color: red } }
  > EOF
  $ cascade --minify --inline-vars supports-var.css 2>&1 | grep -v "warning"
  :root{--display:grid}@supports(display:var(--display)){.x{color:red}}

A self-referential variable [--x: var(--x)] is invalid at computed
time per CSS Custom Properties L1 §5; consumers use their fallback, and
the now-unreferenced custom property is dead-stripped.

  $ cat > self-cycle.css <<EOF
  > :root { --x: var(--x) }
  > .a { color: var(--x, red) }
  > EOF
  $ cascade --minify --inline-vars self-cycle.css 2>&1 | grep -v "warning"
  .a{color:red}

A two-cycle [--a -> --b -> --a] is detected and both definitions stay
verbatim. The consumer's fallback is no colour, so its declaration is
invalid at computed-value time and the reference stays for the browser
to answer.

  $ cat > two-cycle.css <<EOF
  > :root { --a: var(--b); --b: var(--a) }
  > .x { color: var(--a, fallback) }
  > EOF
  $ cascade --minify --inline-vars two-cycle.css 2>&1 | grep -v "warning"
  :root{--a:var(--b);--b:var(--a)}.x{color:var(--a,fallback)}

A three-step indirect cycle [--a -> --b -> --c -> --a] is detected the
same way.

  $ cat > three-cycle.css <<EOF
  > :root {
  >   --a: var(--b);
  >   --b: var(--c);
  >   --c: var(--a);
  > }
  > .x { color: var(--a, fallback) }
  > EOF
  $ cascade --minify --inline-vars three-cycle.css 2>&1 | grep -v "warning"
  :root{--a:var(--b);--b:var(--c);--c:var(--a)}.x{color:var(--a,fallback)}

A var() inside a string token is NOT substituted - per CSS Custom
Properties L1 §2 substitution does not look inside [<string>] tokens.
The declaration that only feeds that string is therefore unused and
dead-stripped.

  $ cat > string.css <<EOF
  > :root { --label: "brand" }
  > .a:before { content: "var(--label)" }
  > EOF
  $ cascade --minify --inline-vars string.css
  .a:before{content:"var(--label)"}
