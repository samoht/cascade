CLI: --inline-imports error handling.

A missing local file is reported as a warning and the @import is
preserved.

  $ cat > missing.css <<EOF
  > @import url("does-not-exist.css");
  > .a { color: red }
  > EOF
  $ cascade --minify --inline-imports missing.css 2>&1 | grep -v "warning"
  @import"does-not-exist.css";.a{color:red}

An imported file with a parse error: the error is reported via warning
and the broken rule is dropped; surviving rules pass through and the
entry's own rules are preserved.

  $ cat > broken.css <<EOF
  > .b { color: notacolor }
  > .ok { color: blue }
  > EOF
  $ cat > entry-broken.css <<EOF
  > @import url("broken.css");
  > .e { color: green }
  > EOF
  $ cascade --minify --inline-imports entry-broken.css 2>&1 | grep -v "warning"
  .ok{color:#00f}.e{color:green}

A non-CSS binary file referenced by @import: parse errors yield warnings
and no content is inlined.

  $ printf '\xff\xfe\x00\x00binary' > binary.dat
  $ cat > entry-bin.css <<EOF
  > @import url("binary.dat");
  > .e { color: red }
  > EOF
  $ cascade --minify --inline-imports entry-bin.css 2>&1 | grep -v "warning"
  .e{color:red}

@import inside a nested at-rule is invalid - @import must be at the top
of the stylesheet (after @charset/@layer statement-list rules per CSS
Cascade Module Level 6 §2). The misplaced import is dropped during
parsing.

  $ cat > misplaced.css <<EOF
  > @media print { @import url("missing.css"); }
  > .e { color: red }
  > EOF
  $ cascade --minify --inline-imports misplaced.css 2>&1 | grep -v "warning"
  .e{color:red}

@import after a rule statement is invalid - the import is rejected
during parsing (per CSS Cascade L6 §2 imports must precede all rule
statements).

  $ cat > late.css <<EOF
  > .e { padding: 0 }
  > @import url("does-not-matter.css");
  > .other { color: red }
  > EOF
  $ cascade --minify --inline-imports late.css 2>&1 | grep -v "warning"
  .e{padding:0}.other{color:red}

@import deeply nested inside @media+@media is also invalid; dropped.

  $ cat > inner.css <<EOF
  > .inner { color: red }
  > EOF
  $ cat > deep-nested.css <<EOF
  > @media print {
  >   @media (min-width: 30em) {
  >     @import url("inner.css");
  >     .x { color: blue }
  >   }
  > }
  > EOF
  $ cascade --minify --inline-imports deep-nested.css 2>&1 | grep -v "warning"
  @media print{@media(width>=30em){.x{color:#00f}}}

@import with a [var()]-driven URL is invalid - the URL must be a
parse-time literal, not a computed reference.

  $ cat > theme-vars.css <<EOF
  > :root { --import-url: url("base.css") }
  > @import var(--import-url);
  > .e { color: red }
  > EOF
  $ cascade --minify --inline-imports theme-vars.css 2>&1 | grep -v "warning"
  :root{--import-url:url(base.css)}.e{color:red}

--inline-imports from stdin is rejected because relative paths cannot be
resolved without a base directory.

  $ echo "@import url('x.css');" | cascade --minify --inline-imports - 2>&1
  Error: --inline-imports requires a file path (cannot resolve relative URLs from stdin)
  [1]
