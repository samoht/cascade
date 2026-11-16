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
  $ cascade --minify unknown.css 2>&1 >/dev/null
  unknown.css:2:1: warning: unknown at-rule @tailwind
    @tailwind base;
    ^^^^^^^^^^^^^^^
  unknown.css:3:30: warning: unknown at-rule @foo
    @font-face { font-family: z; @foo bar; src: url(a.woff) }
                                 ^^^^

An @charset that is not the first bytes of the sheet is dropped and
reported where it stands.

  $ cat > charset.css <<EOF
  > .a { color: red }
  > @charset "utf-8";
  > EOF
  $ cascade --minify charset.css 2>&1 >/dev/null
  charset.css:2:1: warning: @charset must be the first bytes of the stylesheet
    @charset "utf-8";
    ^^^^^^^^^^^^^^^^^

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
  $ cascade --minify descriptor.css 2>&1 >/dev/null
  descriptor.css:3:16: warning: bad value for font-weight: expected number
      font-weight: banana;
                   ^^^^^^

The checks made on a statement the reader kept, or dropped whole, point
at that statement: an @font-face missing its source, an @import after a
style rule or inside a block, and an @else with no @when before it. `-v`
prints the two misplaced imports apart, though their message is the same.

  $ cat > statements.css <<EOF
  > @font-face { font-family: x }
  > .d { color: red }
  > @import "late.css";
  > @media screen { .c { @import "x.css"; color: red } }
  > @media print { @import "y.css"; }
  > @else { .e { color: red } }
  > EOF
  $ cascade -v --minify statements.css 2>&1 >/dev/null
  statements.css:1:1: warning: @font-face without a font-family or src descriptor
    @font-face { font-family: x }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  statements.css:3:1: warning: @import must precede style rules
    @import "late.css";
    ^^^^^^^^^^^^^^^^^^^
  statements.css:4:22: warning: @import is only valid at the top of a stylesheet
    @media screen { .c { @import "x.css"; color: red } }
                         ^^^^^^^
  statements.css:5:16: warning: @import is only valid at the top of a stylesheet
    @media print { @import "y.css"; }
                   ^^^^^^^
  statements.css:6:1: warning: @else without preceding @when
    @else { .e { color: red } }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
