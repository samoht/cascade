CLI: cascade diff - report shaping (depth, warnings, block runs).

A small diff prints in full: --depth=auto only steps back once the
report stops fitting.

  $ cat > a.css <<EOF
  > .x { color: red; margin: 0 }
  > EOF
  $ cat > b.css <<EOF
  > .x { color: blue; margin: 1px }
  > EOF
  $ NO_COLOR=1 cascade diff a.css b.css
  CSS: 29 chars vs 32 chars (10.3% diff)
  Changes: 1 modified rule
  
  --- a.css
  +++ b.css
  └─ .x
        * color: red -> blue
        * margin: 0 -> 1px
  
  [1]



Pinning a depth cuts the tree there and records what it hid, so an
elided subtree never reads as an empty one.

  $ NO_COLOR=1 cascade diff --depth=1 a.css b.css
  CSS: 29 chars vs 32 chars (10.3% diff)
  Changes: 1 modified rule
  
  --- a.css
  +++ b.css
  └─ .x
        ...2 more lines
  
  [1]



Parse warnings lead the report: a declaration the parser dropped
qualifies every difference below it.

  $ cat > warn.css <<EOF
  > .x { color: red; width: <value> }
  > EOF
  $ NO_COLOR=1 cascade diff a.css warn.css
  CSS: 29 chars vs 34 chars (17.2% diff)
  Changes: 1 modified rule
  
  warn.css parse warning: <string>: read_declaration/width: bad value for width: expected one of at [24-25] (in component)
  .x { color: red; width: <value> }
                          ^
  
  --- a.css
  +++ warn.css
  └─ .x
        - margin
  
  [1]



