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
  3

The control: a page whose CSS parses is projected exactly as before, and the
exit status stays 0.

  $ cat > ok.html <<EOF
  > <html><head><style>p{color:red}p:hover{margin:0}</style></head><body><p>hi</p></body></html>
  > EOF
  $ cascade apply ok.html 2> /dev/null
  <html><head><style>p:hover{margin:0}</style></head><body><p style="color:red">hi</p>
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
