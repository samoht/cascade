CLI: --inline-vars composition with other canonicalizations.

Inlined values participate in normal canonicalization. A hex color in a
declaration shortens to its named equivalent after substitution.

  $ cat > color.css <<EOF
  > :root { --brand: #ff0000 }
  > .a { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-vars color.css
  .a{color:red}

A calc inside the substituted value reduces.

  $ cat > calc.css <<EOF
  > :root { --gap: 5px }
  > .a { width: calc(var(--gap) + 10px) }
  > EOF
  $ cascade --minify --inline-vars calc.css
  .a{width:15px}

A shorthand-valued variable inlines and the printer canonicalizes.

  $ cat > shorthand.css <<EOF
  > :root { --m: 1px 1px 1px 1px }
  > .a { margin: var(--m) }
  > EOF
  $ cascade --minify --inline-vars shorthand.css
  .a{margin:1px}

The Tailwind opacity-guard pattern: alpha component reads from a guard
variable. With both vars defined, the rgba() collapses to a fully-opaque
form per CSS Color L4 §1.3, then minifies to the shortest equivalent
hex spelling.

  $ cat > opacity-guard.css <<EOF
  > :root { --tw-bg-opacity: 1; --tw-color: 248 113 113 }
  > .bg-red { background-color: rgba(var(--tw-color) / var(--tw-bg-opacity)) }
  > EOF
  $ cascade --minify --inline-vars opacity-guard.css
  .bg-red{background-color:#f87171}

A var() consumer with !important keeps the importance after substitution.

  $ cat > important.css <<EOF
  > :root { --brand: red }
  > .a { color: var(--brand) !important }
  > EOF
  $ cascade --minify --inline-vars important.css
  .a{color:red!important}

A custom property declared with !important overrides a non-important
redeclaration (per Cascade L6 §6.3 importance applies to custom
properties).

  $ cat > important-decl.css <<EOF
  > :root { --brand: red !important }
  > :root { --brand: blue }
  > .a { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-vars important-decl.css
  .a{color:red}

A var() inside a function (rgb, calc, gradient, filter, attr) inlines and
the surrounding function canonicalizes.

  $ cat > functions.css <<EOF
  > :root { --r: 255; --g: 0; --b: 0; --blur: 5px }
  > .a { color: rgb(var(--r) var(--g) var(--b)) }
  > .b { filter: blur(var(--blur)) }
  > .c { background: linear-gradient(red, var(--r)) }
  > EOF
  $ cascade --minify --inline-vars functions.css 2>&1 | grep -v "warning"
  .a{color:red}.b{filter:blur(5px)}.c{background:linear-gradient(red,255)}

A var() used in transition / animation shorthand resolves the
appropriate component.

  $ cat > shorthand-anim.css <<EOF
  > :root { --prop: opacity; --anim: slide }
  > .a { transition: var(--prop) 0.3s ease }
  > .b { animation: var(--anim) 1s ease infinite }
  > EOF
  $ cascade --minify --inline-vars shorthand-anim.css
  .a{transition:opacity .3s}.b{animation:slide 1s infinite}

A custom property holding a CSS-wide keyword (initial / inherit / unset
/ revert) is not inlined: CSS Variables L1 sec. 2.1 gives the keyword its
usual meaning, so the binding is what the cascade makes of it against the
element the sheet is read for, and never the keyword's own tokens.
Writing [color: inherit] instead inherits the property where the binding
resolves to the guaranteed-invalid value and the declaration does not.

  $ cat > keywords.css <<EOF
  > :root { --c: inherit }
  > .a { color: var(--c) }
  > EOF
  $ cascade --minify --inline-vars keywords.css
  :root{--c:inherit}.a{color:var(--c)}

A custom property holding a comma-separated list inlines as the literal
sequence, preserving the list separator.

  $ cat > list.css <<EOF
  > :root { --colors: red, blue, green }
  > .a { background: linear-gradient(var(--colors)) }
  > EOF
  $ cascade --minify --inline-vars list.css
  .a{background:linear-gradient(red,#00f,green)}
