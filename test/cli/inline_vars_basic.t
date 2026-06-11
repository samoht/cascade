CLI: --inline-vars - basic substitution.

Default --minify keeps var() references unchanged (open-world parity with
cssnano / Lightning CSS).

  $ cat > basic.css <<EOF
  > :root { --brand: red }
  > .a { color: var(--brand) }
  > EOF
  $ cascade --minify basic.css
  :root{--brand:red}.a{color:var(--brand)}

--inline-vars resolves the reference and drops the now-dead --brand
declaration.

  $ cascade --minify --inline-vars basic.css
  .a{color:red}

A variable used in the same rule that declares it resolves locally.

  $ cat > local.css <<EOF
  > .x { --pad: 10px; padding: var(--pad) }
  > EOF
  $ cascade --minify --inline-vars local.css
  .x{padding:10px}

Multiple var() references in a single value all inline.

  $ cat > multi.css <<EOF
  > :root { --top: 10px; --right: 20px }
  > .a { padding: var(--top) var(--right) var(--top) var(--right) }
  > EOF
  $ cascade --minify --inline-vars multi.css
  .a{padding:10px 20px}

A custom property whose value contains another var() resolves
transitively.

  $ cat > chain.css <<EOF
  > :root {
  >   --base: 8px;
  >   --double: calc(var(--base) * 2);
  >   --quad: calc(var(--double) * 2);
  > }
  > .a { padding: var(--quad) }
  > EOF
  $ cascade --minify --inline-vars chain.css
  .a{padding:32px}

A multi-token custom property value inlines as a sequence.

  $ cat > tokens.css <<EOF
  > :root { --shadow: 0 1px 2px black }
  > .a { box-shadow: var(--shadow) }
  > EOF
  $ cascade --minify --inline-vars tokens.css
  .a{box-shadow:0 1px 2px #000}

Custom property names with mixed dash/underscore/digits round-trip
preserved.

  $ cat > names.css <<EOF
  > :root {
  >   --my-Var-1: red;
  >   --color_2: blue;
  >   --foo-bar-baz-qux: green;
  > }
  > .a { color: var(--my-Var-1) }
  > .b { color: var(--color_2) }
  > .c { color: var(--foo-bar-baz-qux) }
  > EOF
  $ cascade --minify --inline-vars names.css
  .a{color:red}.b{color:#00f}.c{color:green}

A redeclared variable keeps the later definition (cascade rule).

  $ cat > shadowed.css <<EOF
  > :root { --brand: red }
  > :root { --brand: blue }
  > .a { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-vars shadowed.css
  .a{color:#00f}
