CLI: `apply` must not report success when it lost CSS.

`cascade apply page.html > out.html` is a build step. CSS it cannot parse
is dropped and the command exits 0, so the redirect writes a page with no
styling and the build sees a green status. The warnings on stderr are easy
to miss and impossible to gate on; the exit status is not.

A `<style>` block the parser cannot use is kept exactly as it was. Deleting
it would ship a page with neither the inline styles it should have had nor
the CSS text a browser might still make something of.

  $ cat > broken.html <<EOF
  > <html><head><style>@@@@ }}} {{{ !!! ;;;</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply broken.html > out.html 2> err.txt
  [1]
  $ cat out.html
  <html><head><style>@@@@ }}} {{{ !!! ;;;</style></head><body><p>hi</p>
  </body></html>
  $ grep '^Error' err.txt
  Error: broken.html:<style>#1: parse dropped every rule; keeping the block verbatim

The warnings that explain the drop are still reported.

  $ grep -c '^warning' err.txt
  6

The control: a page whose CSS parses is projected exactly as before, and the
exit status stays 0.

  $ cat > ok.html <<EOF
  > <html><head><style>p{color:red}p:hover{margin:0}</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply ok.html 2> /dev/null
  <html><head><style></style><style>p:hover{margin:0}</style></head><body><p style="color:red">hi</p>
  </body></html>

A supplementary stylesheet that does not parse is an error too. There is no
block to keep here, so the page is written with whatever its own `<style>`
blocks gave it and the exit status says the extra sheet did not apply.

  $ cat > page.html <<EOF
  > <html><head></head><body><p>hi</p></body></html>
  > EOF
  $ cat > bad.css <<EOF
  > @@@@ }}} {{{ !!! ;;;
  > EOF
  $ cascade apply page.html bad.css > out2.html 2> err2.txt
  [1]
  $ grep '^Error' err2.txt
  Error: bad.css: parse dropped every rule

A supplementary stylesheet that cannot be read at all never reaches the
parser: cmdliner's `file` conv only checks that the path exists, so a
directory gets through. Reading it fails, and an unreadable stylesheet is
not the same thing as no stylesheet.

  $ mkdir extra.d
  $ cascade apply page.html extra.d > out3.html 2> err3.txt
  [124]
  $ grep -c '^cascade: Error reading extra.d' err3.txt
  1
  $ wc -c < out3.html | tr -d ' '
  0

The control: a supplementary stylesheet that reads and parses still applies.

  $ cat > good.css <<EOF
  > p{margin:0}
  > EOF
  $ cascade apply page.html good.css 2> /dev/null
  <html><head></head><body><p style="margin:0">hi</p>
  </body></html>

The gate asks a question about the parse: did it produce any statement? It
is not the question "does this sheet serialise to anything", which a parse
that kept everything can still answer with nothing. A statement can be
parsed, held, and still print as the empty string: CSS Syntax 3 §8.3 makes
`@charset` a decoder hint rather than a rule, and UTF-8 is what the
serialiser emits anyway, so a redundant one prints nothing.

  $ cat > charset.html <<EOF
  > <html><head><style>@charset "UTF-8";@charset "UTF-8";</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply charset.html > out4.html 2> err4.txt
  $ grep '^Error' err4.txt
  [1]
  $ cat out4.html
  <html><head><style></style></head><body><p>hi</p>
  </body></html>

The second `@charset` is out of place, which is reported, and both are
kept: nothing about that parse dropped every rule.

  $ grep -c '^warning' err4.txt
  1

A `@font-face` with no `src` is the same shape. CSS Fonts 4 §4.3 gives it
no font to load, so it prints nothing, but the rule itself parsed and the
statement is there for the projection to read.

  $ cat > face.html <<EOF
  > <html><head><style>@font-face{font-family:x}</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply face.html > out5.html 2> err5.txt
  $ grep '^Error' err5.txt
  [1]
  $ cat out5.html
  <html><head><style></style></head><body><p>hi</p>
  </body></html>

An at-rule written among the descriptors is no loss either. CSS Syntax 3
(ED) §5.5.5 gives it to "consume an at-rule", which ends at its block or at
its `;`, so it costs itself and the `@font-face` around it still parses.

  $ cat > face2.html <<EOF
  > <html><head><style>@font-face{font-family:x;@bogus w;}</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply face2.html > out8.html 2> err8.txt
  $ grep '^Error' err8.txt
  [1]
  $ cat out8.html
  <html><head><style></style></head><body><p>hi</p>
  </body></html>

An at-rule this library does not know parses to a statement of its own and
is kept, so a block holding nothing else is not a loss either.

  $ cat > unknown.html <<EOF
  > <html><head><style>@foo bar;</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply unknown.html > out6.html 2> err6.txt
  $ grep '^Error' err6.txt
  [1]
  $ cat out6.html
  <html><head><style></style><style>@foo bar;</style></head><body><p>hi</p>
  </body></html>

The true positive keeps its exit status. `@charset` is recognised only in
the exact form CSS Syntax 3 §8.2 reserves, so a lowercase label is not a
statement this parser produces: the block holds one construct, it is
dropped, and nothing is left to project.

  $ cat > lower.html <<EOF
  > <html><head><style>@charset "utf-8";</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply lower.html > out7.html 2> err7.txt
  [1]
  $ grep '^Error' err7.txt
  Error: lower.html:<style>#1: parse dropped every rule; keeping the block verbatim
  $ cat out7.html
  <html><head><style>@charset "utf-8";</style></head><body><p>hi</p>
  </body></html>
