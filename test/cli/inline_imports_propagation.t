CLI: --inline-imports - what propagates and what is dropped.

@charset in an imported file is ignored - per CSS Syntax L3 §3.2 only
the entry stylesheet's @charset has effect. Cascade receives already-decoded
UTF-8 text, so redundant UTF-8 @charset syntax is not emitted under minify.

  $ cat > inner-charset.css <<EOF
  > @charset "UTF-8";
  > .x { color: red }
  > EOF
  $ cat > entry-charset.css <<EOF
  > @charset "UTF-8";
  > @import url("inner-charset.css");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-charset.css
  .x{color:red}.e{padding:0}

@namespace from an imported file propagates into the bundle - per CSS
Namespaces L3 §2 namespaces are stylesheet-scoped, so when the file is
merged the namespace must travel with its rules.

  $ cat > svg.css <<EOF
  > @namespace svg url(http://www.w3.org/2000/svg);
  > svg|circle { fill: red }
  > EOF
  $ cat > entry-ns.css <<EOF
  > @import url("svg.css");
  > .e { color: blue }
  > EOF
  $ cascade --minify --inline-imports entry-ns.css
  @namespace svg"http://www.w3.org/2000/svg";svg|circle{fill:red}.e{color:#00f}

A UTF-8 BOM at the start of an imported file is stripped during parsing
and not propagated.

  $ printf '\357\273\277.bom { color: red }\n' > bom.css
  $ cat > entry-bom.css <<EOF
  > @import url("bom.css");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-bom.css
  .bom{color:red}.e{padding:0}

Comments in an imported file are stripped (cascade always strips
comments at parse time per CSS Syntax L3 §4.3.2).

  $ cat > with-comments.css <<EOF
  > /* this is a comment */
  > .a { color: red /* inline comment */ }
  > EOF
  $ cat > entry-comments.css <<EOF
  > @import url("with-comments.css");
  > EOF
  $ cascade --minify --inline-imports entry-comments.css
  .a{color:red}

Remote URLs (http, https, data:) are NOT followed - the @import is
preserved as-is.

  $ cat > remote.css <<EOF
  > @import url("https://example.test/reset.css");
  > @import url("data:text/css;base64,LmRhdGEgeyBjb2xvcjogcmVkIH0=");
  > .a { color: red }
  > EOF
  $ cascade --minify --inline-imports remote.css
  @import"https://example.test/reset.css";@import"data:text/css;base64,LmRhdGEgeyBjb2xvcjogcmVkIH0=";.a{color:red}
