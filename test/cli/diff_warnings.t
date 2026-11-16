CLI: cascade diff - a parse warning both inputs raise is reported once.

The two inputs of a diff are usually variants of one source, so a
declaration the parser rejects is normally rejected on both sides. That is
one fact about the input, not two findings about the difference: it prints
once, under a label naming both files, with the first file's snippet. The
byte offset each side puts it at is no part of its identity.

  $ cat > a.css <<EOF
  > .x { color: red; float: center; margin: 0 }
  > EOF
  $ cat > b.css <<EOF
  > .x { color: red; width: 0; float: center; margin: 1px }
  > EOF
  $ NO_COLOR=1 cascade diff a.css b.css
  CSS: 44 chars vs 56 chars (27.3% diff)
  Changes: 1 modified rule
  
  a.css:1:25: warning: bad value for float: unknown float-side: center (also in b.css)
    .x { color: red; float: center; margin: 0 }
                            ^^^^^^
  
  --- a.css
  +++ b.css
  └─ .x
        + width: 0
        * margin: 0 -> 1px
  
  [1]

A declaration rejected on one side only is what qualifies the difference
reported below it, so one-sided warnings lead: the first file's, then the
second file's, then the ones both files raise.

  $ cat > c.css <<EOF
  > .x { float: center; clear: nope; margin: 0 }
  > EOF
  $ cat > d.css <<EOF
  > .x { float: center; position: nope; margin: 1px }
  > EOF
  $ NO_COLOR=1 cascade diff c.css d.css
  CSS: 45 chars vs 50 chars (11.1% diff)
  Changes: 1 modified rule
  
  c.css:1:28: warning: bad value for clear: unknown clear: nope
    .x { float: center; clear: nope; margin: 0 }
                               ^^^^
  d.css:1:31: warning: bad value for position: unknown position: nope
    .x { float: center; position: nope; margin: 1px }
                                  ^^^^
  c.css:1:13: warning: bad value for float: unknown float-side: center (also in d.css)
    .x { float: center; clear: nope; margin: 0 }
                ^^^^^^
  
  --- c.css
  +++ d.css
  └─ .x
        * margin: 0 -> 1px
  
  [1]

The budget is a total over the report and the one-sided warnings claim it
first. The three warnings both files raise cost one slot between them, so
the two the budget leaves out are counted once rather than once per side.

  $ cat > e.css <<EOF
  > .x { float: center; clear: nope; visibility: nope; z-index: nope; margin: 0 }
  > EOF
  $ cat > f.css <<EOF
  > .x { float: center; clear: nope; visibility: nope; position: nope; margin: 1px }
  > EOF
  $ NO_COLOR=1 cascade diff e.css f.css
  CSS: 78 chars vs 81 chars (3.8% diff)
  Changes: 1 modified rule
  
  e.css:1:61: warning: bad value for z-index: expected integer
    clear: nope; visibility: nope; z-index: nope; margin: 0 }
                                            ^^^^
  f.css:1:62: warning: bad value for position: unknown position: nope
    lear: nope; visibility: nope; position: nope; margin: 1px }
                                            ^^^^
  e.css:1:13: warning: bad value for float: unknown float-side: center (also in f.css)
    .x { float: center; clear: nope; visibility: nope; z-index
                ^^^^^^
  e.css and f.css: 2 more parse warnings
  
  --- e.css
  +++ f.css
  └─ .x
        * margin: 0 -> 1px
  
  [1]

One warning past the budget keeps the singular form.

  $ cat > g.css <<EOF
  > .x { float: center; clear: nope; visibility: nope; z-index: nope; margin: 0 }
  > EOF
  $ cat > h.css <<EOF
  > .x { float: center; clear: nope; visibility: nope; z-index: nope; margin: 1px }
  > EOF
  $ NO_COLOR=1 cascade diff g.css h.css
  CSS: 78 chars vs 80 chars (2.6% diff)
  Changes: 1 modified rule
  
  g.css:1:13: warning: bad value for float: unknown float-side: center (also in h.css)
    .x { float: center; clear: nope; visibility: nope; z-index
                ^^^^^^
  g.css:1:28: warning: bad value for clear: unknown clear: nope (also in h.css)
    .x { float: center; clear: nope; visibility: nope; z-index: nope; margi
                               ^^^^
  g.css:1:46: warning: bad value for visibility: unknown visibility: nope (also in h.css)
    float: center; clear: nope; visibility: nope; z-index: nope; margin: 0 }
                                            ^^^^
  g.css and h.css: 1 more parse warning
  
  --- g.css
  +++ h.css
  └─ .x
        * margin: 0 -> 1px
  
  [1]
