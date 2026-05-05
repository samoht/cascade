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

A variable declared inside @media is in scope only for consumers within
that @media block.

  $ cat > media.css <<EOF
  > @media (min-width: 30em) {
  >   :root { --brand: red }
  >   .a { color: var(--brand) }
  > }
  > .b { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-vars media.css
  @media (min-width:30em){.a{color:red}}.b{color:var(--brand)}

A variable declared inside @layer applies to consumers within the same
layer; outside-layer use stays as var().

  $ cat > layer.css <<EOF
  > @layer theme { :root { --brand: red } .a { color: var(--brand) } }
  > .b { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-vars layer.css
  @layer theme{.a{color:red}}.b{color:var(--brand)}

A variable used in a @container query value is preserved (container
queries evaluate at layout time, not at the syntax layer).

  $ cat > container.css <<EOF
  > :root { --bp: 30em }
  > @container (min-width: var(--bp)) { .x { color: red } }
  > EOF
  $ cascade --minify --inline-vars container.css 2>&1 | grep -v "warning"
  :root{--bp:30em}@container (min-width:var(--bp)){.x{color:red}}

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
verbatim with consumers falling back.

  $ cat > two-cycle.css <<EOF
  > :root { --a: var(--b); --b: var(--a) }
  > .x { color: var(--a, fallback) }
  > EOF
  $ cascade --minify --inline-vars two-cycle.css 2>&1 | grep -v "warning"
  :root{--a:var(--b);--b:var(--a)}.x{color:fallback}

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
  :root{--a:var(--b);--b:var(--c);--c:var(--a)}.x{color:fallback}

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
