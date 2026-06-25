CLI: --inline-imports + --inline-vars combined.

The canonical "production single-file build" flow: import a tokens file,
inline the imports, then resolve the variables defined in the imported
file at use sites in the entry.

  $ cat > tokens.css <<EOF
  > :root { --brand: #ff0000; --gap: 8px }
  > EOF
  $ cat > app.css <<EOF
  > @import url("tokens.css");
  > .btn { color: var(--brand); padding: var(--gap) }
  > EOF
  $ cascade --minify --inline-imports --inline-vars app.css
  .btn{color:red;padding:8px}

A calc() referencing imported tokens reduces after both passes.

  $ cat > tokens-calc.css <<EOF
  > :root { --brand: #ff0000; --gap: calc(4px * 2) }
  > EOF
  $ cat > app-calc.css <<EOF
  > @import url("tokens-calc.css");
  > .btn { color: var(--brand); padding: var(--gap) }
  > EOF
  $ cascade --minify --inline-imports --inline-vars app-calc.css
  .btn{color:red;padding:8px}

--keep-vars still applies to imported variable definitions: only the
listed names retain their var() reference; everything else inlines.

  $ cat > app-keep.css <<EOF
  > @import url("tokens.css");
  > .btn { color: var(--brand); padding: var(--gap) }
  > EOF
  $ cascade --minify --inline-imports --inline-vars --keep-vars=brand app-keep.css
  :root{--brand:#f00}.btn{color:var(--brand);padding:8px}

A multi-file design system: tokens in one file, components in another,
entry pulls both. Variables defined in either imported file resolve
into the entry's rules.

  $ cat > tokens-ds.css <<EOF
  > :root { --brand: #ff0000; --radius: 4px }
  > EOF
  $ cat > components.css <<EOF
  > .btn { color: var(--brand); border-radius: var(--radius) }
  > .card { background: var(--brand); padding: var(--radius) }
  > EOF
  $ cat > app-ds.css <<EOF
  > @import url("tokens-ds.css");
  > @import url("components.css");
  > .extra { color: var(--brand) }
  > EOF
  $ cascade --minify --inline-imports --inline-vars app-ds.css
  .btn,.extra{color:red}.btn{border-radius:4px}.card{background:red;padding:4px}

A conditional import combined with --inline-vars: the variables are
resolved within the condition's wrapped scope.

  $ cat > print-tokens.css <<EOF
  > :root { --print-color: black }
  > .heading { color: var(--print-color) }
  > EOF
  $ cat > app-cond.css <<EOF
  > @import url("print-tokens.css") print;
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports --inline-vars app-cond.css
  @media print{.heading{color:#000}}.e{padding:0}

A @layer-scoped import combined with --inline-vars: variables defined
inside the layer scope to layer consumers.

  $ cat > layer-tokens.css <<EOF
  > :root { --c: red }
  > .x { color: var(--c) }
  > EOF
  $ cat > app-layer.css <<EOF
  > @import url("layer-tokens.css") layer(theme);
  > .e { color: blue }
  > EOF
  $ cascade --minify --inline-imports --inline-vars app-layer.css
  .x{color:red}.e{color:#00f}

An imported variable that a kept variable references is folded into the kept
variable and deleted, the same as a local one.

  $ cat > tokens-transitive.css <<EOF
  > :root { --brand: var(--palette-red); --palette-red: red; --gap: 8px }
  > EOF
  $ cat > app-transitive.css <<EOF
  > @import url("tokens-transitive.css");
  > .btn { color: var(--brand); padding: var(--gap) }
  > EOF
  $ cascade --minify --inline-imports --inline-vars --keep-vars=brand app-transitive.css
  :root{--brand:red}.btn{color:var(--brand);padding:8px}
