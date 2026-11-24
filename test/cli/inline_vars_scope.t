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
  .theme{--c:red}.theme .descendant{color:red}.other{color:var(--c)}

A nested rule's selector is its parent's composed as CSS Nesting 1 sec. 3
composes it. A type selector parent goes inside [:is()] (Selectors 4 sec.
3.5 keeps it leading its compound), so [.x&] under [a] is [.x:is(a)], an [a]
element with class [x], and not the class [.xa], which a definition there
never reaches.

  $ cat > nested-type.css <<EOF
  > .xa { --c: red }
  > a { .x& { color: var(--c) } }
  > EOF
  $ cascade --minify --inline-vars nested-type.css
  .xa{--c:red}.x:is(a){color:var(--c)}

A definition on a selector list reaches what each branch matches, so it
covers a consumer nested under that list, and a consumer under one of its
branches.

  $ cat > list-scope.css <<EOF
  > .a, .b { --c: red; .x { color: var(--c) } }
  > EOF
  $ cascade --minify --inline-vars list-scope.css
  :is(.a,.b) .x{color:red}
  $ cat > list-branch.css <<EOF
  > .a, .b { --c: red }
  > .a .x { color: var(--c) }
  > EOF
  $ cascade --minify --inline-vars list-branch.css
  .a .x{color:red}

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

The winner is only statically decidable when every definition sits on the
same element. Definitions on different selectors leave it to which element
is being styled, so the variable stays live instead of folding to whichever
one the layer order picks: [.x] computes green and [.y] computes blue, and no
single value stands for both. The raw objective merges what is left by its
raw bytes, here and below.

  $ cat > layer-selectors.css <<EOF
  > @layer a, b;
  > @layer a { .x { --c: red } }
  > @layer b { .x { --c: green } }
  > @layer b { .y { --c: blue } }
  > .x { color: var(--c) }
  > .y { color: var(--c) }
  > EOF
  $ cascade --minify --objective=raw --inline-vars layer-selectors.css 2>&1
  cascade: warning: --c takes different values in different scopes; kept live (cannot inline safely)
  .x{--c:green}.y{--c:blue}.x,.y{color:var(--c)}

Rules that define a variable with one value give it one binding, wherever
they sit: an element any of them reaches computes that value. It folds into
each consumer a definition covers, as it does once --minify has merged the
definitions into one rule, so inlining the output again changes nothing.

  $ cat > same-value.css <<EOF
  > .a { --s: 1px; width: var(--s) }
  > .b { --s: 1px; height: var(--s) }
  > EOF
  $ cascade --minify --inline-vars same-value.css > same-value.min.css
  $ cat same-value.min.css
  .a{width:1px}.b{height:1px}
  $ cascade --minify --inline-vars same-value.min.css
  .a{width:1px}.b{height:1px}

A consumer no definition covers keeps its reference, and the definitions stay
for it; the covered ones still fold, on the first pass as on the second.

  $ cat > same-value-partial.css <<EOF
  > .a { --s: 1px; --g: f(var(--s)); width: var(--g) }
  > .b { --s: 1px; --g: f(var(--s)); height: var(--g) }
  > .c { --g: f(var(--s, 2px)) }
  > EOF
  $ cascade --minify --objective=raw --inline-vars same-value-partial.css 2>&1 > same-value-partial.min.css
  cascade: warning: --g takes different values in different scopes; kept live (cannot inline safely)
  $ cat same-value-partial.min.css
  .a,.b{--s:1px;--g:f(1px)}.a{width:var(--g)}.b{height:var(--g)}.c{--g:f(var(--s,2px))}
  $ cascade --minify --objective=raw --inline-vars same-value-partial.min.css 2>&1
  cascade: warning: --g takes different values in different scopes; kept live (cannot inline safely)
  .a,.b{--s:1px;--g:f(1px)}.a{width:var(--g)}.b{height:var(--g)}.c{--g:f(var(--s,2px))}

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
  $ cascade --minify --inline-vars container.css 2>&1
  :root{--bp:30em}@container(width>=var(--bp)){.x{color:red}}

A variable used in a @media query value is preserved. Custom property
substitution only happens in property values, not media query syntax.

  $ cat > media-query-var.css <<EOF
  > :root { --bp: 30em }
  > @media (min-width: var(--bp)) { .x { color: red } }
  > EOF
  $ cascade --minify --inline-vars media-query-var.css 2>&1
  :root{--bp:30em}@media(width>=var(--bp)){.x{color:red}}

A variable used in an @supports condition is preserved for the same
reason.

  $ cat > supports-var.css <<EOF
  > :root { --display: grid }
  > @supports (display: var(--display)) { .x { color: red } }
  > EOF
  $ cascade --minify --inline-vars supports-var.css 2>&1
  :root{--display:grid}@supports(display:var(--display)){.x{color:red}}

A self-referential variable [--x: var(--x)] is invalid at computed
time per CSS Custom Properties L1 §5; consumers use their fallback, and
the now-unreferenced custom property is dead-stripped.

  $ cat > self-cycle.css <<EOF
  > :root { --x: var(--x) }
  > .a { color: var(--x, red) }
  > EOF
  $ cascade --minify --inline-vars self-cycle.css 2>&1
  .a{color:red}

A two-cycle [--a -> --b -> --a] is detected and both definitions stay
verbatim. The consumer's fallback is no colour, so its declaration is
invalid at computed-value time and the reference stays for the browser
to answer.

  $ cat > two-cycle.css <<EOF
  > :root { --a: var(--b); --b: var(--a) }
  > .x { color: var(--a, fallback) }
  > EOF
  $ cascade --minify --inline-vars two-cycle.css 2>&1
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
  $ cascade --minify --inline-vars three-cycle.css 2>&1
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
