CLI: --inline-vars + --keep-vars.

--keep-vars protects names from inlining. Useful for production builds
that want to inline static design tokens but keep user-themable
variables runtime-mutable.

  $ cat > theme.css <<EOF
  > :root { --brand: red; --spacing-3: 12px }
  > .btn { color: var(--brand); padding: var(--spacing-3) }
  > EOF
  $ cascade --minify --inline-vars --keep-vars=brand theme.css
  :root{--brand:red}.btn{color:var(--brand);padding:12px}

Multiple --keep-vars names accepted as comma-separated list.

  $ cascade --minify --inline-vars --keep-vars=brand,spacing-3 theme.css
  :root{--brand:red;--spacing-3:12px}.btn{color:var(--brand);padding:var(--spacing-3)}

--keep-vars accepts the leading [--] (forgives a common mistake).

  $ cascade --minify --inline-vars --keep-vars=--brand theme.css
  :root{--brand:red}.btn{color:var(--brand);padding:12px}

Mixed forms with and without [--] accepted.

  $ cascade --minify --inline-vars --keep-vars=--brand,spacing-3 theme.css
  :root{--brand:red;--spacing-3:12px}.btn{color:var(--brand);padding:var(--spacing-3)}

Keeping a variable also keeps the runtime dependencies that its value
references. Otherwise the kept variable would compute differently after
dead-stripping.

  $ cat > transitive.css <<EOF
  > :root { --brand: var(--palette-red); --palette-red: red; --gap: 8px }
  > .btn { color: var(--brand); padding: var(--gap) }
  > EOF
  $ cascade --minify --inline-vars --keep-vars=brand transitive.css
  :root{--brand:var(--palette-red);--palette-red:red}.btn{color:var(--brand);padding:8px}

Custom property names are case-sensitive. Keeping [--brand] must not
also keep [--Brand].

  $ cat > case.css <<EOF
  > :root { --brand: red; --Brand: blue }
  > .a { color: var(--brand) }
  > .b { color: var(--Brand) }
  > EOF
  $ cascade --minify --inline-vars --keep-vars=brand case.css
  :root{--brand:red}.a{color:var(--brand)}.b{color:#00f}

Whitespace around list entries is tolerated.

  $ cascade --minify --inline-vars --keep-vars=" brand , spacing-3 " theme.css
  :root{--brand:red;--spacing-3:12px}.btn{color:var(--brand);padding:var(--spacing-3)}

--keep-vars without --inline-vars emits a warning that the flag has no
effect on its own.

  $ cascade --minify --keep-vars=brand theme.css 2>&1 | head -2
  Warning: --keep-vars has no effect without --inline-vars
  :root{--brand:red;--spacing-3:12px}.btn{color:var(--brand);padding:var(--spacing-3)}

A wildcard [*] is rejected with a clear error - names must be listed
explicitly.

  $ cascade --minify --inline-vars --keep-vars="*" theme.css 2>&1 | head -3
  Error: --keep-vars does not accept the wildcard "*"; list names explicitly.
  [1]
