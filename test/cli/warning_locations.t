CLI: every parse warning names the line and column it is about.

A warning an editor can jump to says `file:line:column`. A byte range
such as `[14-29]` names a place only a reader holding the text can count
to, so no warning is built without the text it is about. Each case below
is one statement.

An at-rule cascade has no grammar for keeps its text and is reported where
it starts, at the top level and inside an @font-face body.

  $ cat > unknown.css <<EOF
  > .a { color: red }
  > @tailwind base;
  > @font-face { font-family: z; @foo bar; src: url(a.woff) }
  > EOF
  $ cascade --minify unknown.css 2>&1 >/dev/null | grep -o 'unknown at-rule @[a-z]* at [^ ]*'
  unknown at-rule @tailwind at unknown.css:2:1
  unknown at-rule @foo at unknown.css:3:30

An @charset that is not the first bytes of the sheet is dropped and
reported where it stands.

  $ cat > charset.css <<EOF
  > .a { color: red }
  > @charset "utf-8";
  > EOF
  $ cascade --minify charset.css 2>&1 >/dev/null | grep -o 'at charset.css:[0-9:]*'
  at charset.css:2:1

An @font-face descriptor whose value does not read is reported at that
value in the sheet, under its own name, rather than at the start of a
copy of the value.

  $ cat > descriptor.css <<EOF
  > @font-face {
  >   font-family: y;
  >   font-weight: banana;
  >   src: url(a.woff)
  > }
  > EOF
  $ cascade --minify descriptor.css 2>&1 >/dev/null | grep -o 'bad value for [^:]*: .* at descriptor.css:[0-9:]*'
  bad value for font-weight: expected number at descriptor.css:3:16

The checks made on a statement the reader kept, or dropped whole, point
at that statement: an @font-face missing its source, an @import after a
style rule or inside a block, and an @else with no @when before it.

  $ cat > statements.css <<EOF
  > @font-face { font-family: x }
  > .d { color: red }
  > @import "late.css";
  > @media screen { .c { @import "x.css"; color: red } }
  > @media print { @import "y.css"; }
  > @else { .e { color: red } }
  > EOF
  $ cascade --minify statements.css 2>&1 >/dev/null | grep -oE ' at ([^ ]+:[0-9]+:[0-9]+|\[[0-9-]+\])'
   at statements.css:1:1
   at statements.css:3:1
   at statements.css:4:22
   at statements.css:5:16
   at statements.css:6:1
