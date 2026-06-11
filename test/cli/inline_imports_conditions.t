CLI: --inline-imports with conditional clauses.

A conditional @import wraps the inlined content in @media per CSS
Cascade Module Level 6 §2.

  $ cat > print.css <<EOF
  > .heading { font-size: 24pt }
  > EOF
  $ cat > conditional.css <<EOF
  > @import url("print.css") print;
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports conditional.css
  @media print{.heading{font-size:24pt}}.e{padding:0}

A layered @import wraps the inlined content in @layer.

  $ cat > theme.css <<EOF
  > .btn { background: blue }
  > EOF
  $ cat > layered.css <<EOF
  > @import url("theme.css") layer(theme);
  > EOF
  $ cascade --minify --inline-imports layered.css
  @layer theme{.btn{background:#00f}}

A @supports condition on @import wraps the inlined content in @supports.

  $ cat > grid.css <<EOF
  > .grid { display: grid }
  > EOF
  $ cat > supports.css <<EOF
  > @import url("grid.css") supports(display: grid);
  > .e { padding: 10px }
  > EOF
  $ cascade --minify --inline-imports supports.css
  .grid{display:grid}.e{padding:10px}

A combined @import with both layer and media wraps in nested at-rules.

  $ cat > theme-print.css <<EOF
  > .heading { font-size: 24pt }
  > EOF
  $ cat > combined.css <<EOF
  > @import url("theme-print.css") layer(theme) print;
  > .e { padding: 10px }
  > EOF
  $ cascade --minify --inline-imports combined.css
  @layer theme{@media print{.heading{font-size:24pt}}}.e{padding:10px}

A comma-separated media list wraps the inlined content in a single
@media that lists both queries.

  $ cat > shared.css <<EOF
  > .shared { color: red }
  > EOF
  $ cat > list.css <<EOF
  > @import url("shared.css") screen, print;
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports list.css
  @media screen,print{.shared{color:red}}.e{padding:0}

The same file imported twice with different conditions produces two
distinct wrapped blocks.

  $ cat > entry-cond.css <<EOF
  > @import url("shared.css") screen;
  > @import url("shared.css") print;
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-cond.css
  @media screen{.shared{color:red}}@media print{.shared{color:red}}.e{padding:0}

@import url(...) layer(name) with a dotted layer name creates the
dotted-name layer block.

  $ cat > nested-layer.css <<EOF
  > @layer base;
  > @import url("theme.css") layer(base.theme);
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports nested-layer.css
  @layer base;@layer base.theme{.btn{background:#00f}}.e{padding:0}

@import url(...) layer() with empty parens wraps in anonymous @layer.

  $ cat > anon.css <<EOF
  > .anon { color: red }
  > EOF
  $ cat > entry-anon.css <<EOF
  > @import url("anon.css") layer();
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-anon.css
  @layer{.anon{color:red}}.e{padding:0}

@import url(...) layer (bare keyword, no parens) also creates an
anonymous layer.

  $ cat > entry-anon-bare.css <<EOF
  > @import url("anon.css") layer;
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-anon-bare.css
  @layer{.anon{color:red}}.e{padding:0}

Nested conditions: an imported file that itself contains conditional
imports wraps the inner condition inside the outer condition.

  $ cat > deeply.css <<EOF
  > .deep { color: red }
  > EOF
  $ cat > middle-cond.css <<EOF
  > @import url("deeply.css") (min-width: 30em);
  > .middle { color: blue }
  > EOF
  $ cat > entry-deep-cond.css <<EOF
  > @import url("middle-cond.css") screen;
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-deep-cond.css
  @media screen{@media(width>=30em){.deep{color:red}}.middle{color:#00f}}.e{padding:0}
