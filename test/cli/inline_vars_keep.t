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
