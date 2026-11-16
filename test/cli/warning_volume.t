CLI: a noisy sheet's warnings stay readable, and -q and -v set how many.

A real sheet trips the same unsupported construct many times: an old
`display:box` or an IE `filter:alpha()` per component. Printing each one
in full buries the rest, so a warning whose message was already printed
for the same file is counted under the first, with where the next few
are, and a last line says how many there were and how to see them.

  $ cat > noisy.css <<EOF
  > .a { display: box; color: red }
  > .b { display: box; color: red }
  > .c { display: box; color: red }
  > .d { display: box; color: red }
  > .e { display: box; float: center; color: red }
  > .f { display: box; color: red }
  > EOF
  $ cascade --minify noisy.css 2> err.txt
  .a,.b,.c,.d,.e,.f{color:red}
  $ cat err.txt
  noisy.css:1:15: warning: bad value for display: unknown display: box
    .a { display: box; color: red }
                  ^^^
    (5 more like this, at 2:15, 3:15, 4:15, ...)
  noisy.css:5:27: warning: bad value for float: unknown float-side: center
    .e { display: box; float: center; color: red }
                              ^^^^^^
  noisy.css: 7 warnings, 2 shown; -v prints every one, -q none

`-v` prints every warning in full.

  $ cascade -v --minify noisy.css 2>&1 >/dev/null | grep -c ': warning: '
  7

`-q` prints none: the output is the same and standard error is empty.

  $ cascade -q --minify noisy.css 2> quiet.txt
  .a,.b,.c,.d,.e,.f{color:red}
  $ wc -c < quiet.txt | tr -d ' '
  0

A sheet whose warnings are all different prints each one, with no count.

  $ cat > varied.css <<EOF
  > .a { display: box; color: red }
  > .b { float: center }
  > EOF
  $ cascade --minify varied.css 2>&1 >/dev/null | grep -v '^  '
  varied.css:1:15: warning: bad value for display: unknown display: box
  varied.css:2:13: warning: bad value for float: unknown float-side: center

`-q` is errors only, not silence: a sheet the parser could use none of
still says so, and fails.

  $ printf '@@@@ }}} {{{' > broken.css
  $ cascade -q broken.css
  Error: broken.css: parse dropped every rule; refusing to write an empty stylesheet
  [1]

The other commands take the same flags. `apply` groups and quiets the
warnings of each `<style>` block, and `prune` those of its stylesheet.

  $ cat > page.html <<EOF
  > <html><head><style>.a { display: box } .b { display: box }</style></head>
  > <body><p class="a">x</p><p class="b">y</p></body></html>
  > EOF
  $ cascade apply page.html 2>&1 >/dev/null
  page.html:<style>#1:1:15: warning: bad value for display: unknown display: box
    .a { display: box } .b { display: box }
                  ^^^
    (1 more like this, at 1:35)
  page.html:<style>#1: 2 warnings, 1 shown; -v prints every one, -q none
  $ cascade apply -q page.html 2>&1 >/dev/null
  $ cascade prune -q page.html noisy.css 2>&1 >/dev/null

`diff` reports a few warnings in its report and counts the rest; `-q`
leaves them out and `-v` prints every one.

  $ cp noisy.css noisy2.css
  $ echo '.g { color: red }' >> noisy2.css
  $ cascade diff -q noisy.css noisy2.css
  CSS: 207 chars vs 225 chars (8.7% diff)
  Changes: 1 added rule
  
  --- noisy.css
  +++ noisy2.css
  └─ .g
        + color: red
  
  [1]
  $ cascade diff -v noisy.css noisy2.css | grep -c ': warning: '
  7
