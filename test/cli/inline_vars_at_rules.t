CLI: --inline-vars across at-rule contexts.

A variable used inside @keyframes resolves the same as in a normal rule.

  $ cat > keyframes.css <<EOF
  > :root { --brand: red }
  > @keyframes pulse { from { color: var(--brand) } to { color: blue } }
  > EOF
  $ cascade --minify --inline-vars keyframes.css
  @keyframes pulse{0%{color:red}to{color:#00f}}

A variable used in an @font-face descriptor inlines.

  $ cat > font-face.css <<EOF
  > :root { --font-url: url("font.woff2"); --range: U+0025-00FF }
  > @font-face {
  >   font-family: Brand;
  >   src: var(--font-url);
  >   unicode-range: var(--range);
  > }
  > EOF
  $ cascade --minify --inline-vars font-face.css
  @font-face{font-family:Brand;src:url(font.woff2);unicode-range:U+25-FF}

A variable used in an @page margin descriptor inlines.

  $ cat > page.css <<EOF
  > :root { --margin-top: 1cm }
  > @page { margin-top: var(--margin-top) }
  > EOF
  $ cascade --minify --inline-vars page.css
  @page{margin-top:1cm}

A registered custom property declared via @property with an
[initial-value] provides the inlining source when no explicit
declaration exists for the variable.

  $ cat > prop.css <<EOF
  > @property --gap {
  >   syntax: "<length>";
  >   inherits: false;
  >   initial-value: 16px;
  > }
  > .a { padding: var(--gap) }
  > EOF
  $ cascade --minify --inline-vars prop.css
  .a{padding:16px}

A variable used inside @starting-style inlines.

  $ cat > starting.css <<EOF
  > :root { --c: red }
  > @starting-style { .a { color: var(--c) } }
  > EOF
  $ cascade --minify --inline-vars starting.css
  @starting-style{.a{color:red}}

A variable used inside @scope inlines.

  $ cat > scope.css <<EOF
  > :root { --c: red }
  > @scope (.card) to (.boundary) { .item { color: var(--c) } }
  > EOF
  $ cascade --minify --inline-vars scope.css
  @scope(.card)to (.boundary){.item{color:red}}
