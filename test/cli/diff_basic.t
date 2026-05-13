CLI: cascade diff - basic structural comparison.

Identical files report identity and exit 0.

  $ cat > a.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > b.css <<EOF
  > .x { color: red }
  > EOF
  $ cascade diff a.css b.css
  CSS files are identical

A property-value change is reported as a modified rule, not a string
edit.

  $ cat > c.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > d.css <<EOF
  > .x { color: blue }
  > EOF
  $ NO_COLOR=1 cascade diff c.css d.css
  cascade: CSS files differ
  CSS: 19 chars vs 18 chars (5.6% diff)
  Changes: 1 modified rule
  
  --- c.css
  +++ d.css
  └─ .x
        * color: red -> blue
  
  [124]



Equivalent colours under different spellings still surface as a
modified rule under --diff=tree (cascade does not minify before
diffing).

  $ cat > e.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > f.css <<EOF
  > .x { color: #f00 }
  > EOF
  $ NO_COLOR=1 cascade diff --diff=tree e.css f.css
  cascade: CSS files differ
  CSS: 19 chars vs 18 chars (5.6% diff)
  Changes: 1 modified rule
  
  --- e.css
  +++ f.css
  └─ .x
        * color: red -> #f00
  
  [124]


The semantic diff mode compares canonical minified CSS and accepts
equivalent spellings.

  $ cat > i.css <<EOF
  > .x { color: color-mix(in oklab, currentcolor 50%, transparent) }
  > EOF
  $ cat > j.css <<EOF
  > .x { color: color-mix(in oklab, currentcolor 50%, #0000) }
  > EOF
  $ cascade diff --diff=semantic i.css j.css
  CSS files are identical



The --diff=string mode falls back to character-level diffing.

  $ cat > g.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > h.css <<EOF
  > .x { color: blue }
  > EOF
  $ NO_COLOR=1 cascade diff --diff=string g.css h.css
  cascade: CSS files differ (string diff)
  CSS: 19 chars vs 18 chars (5.6% diff)
  No structural differences
  
  Strings differ at position 12 (line 0, col 12)
  
  --- Expected
  +++ Actual
  @@ position 12 @@
  -.x { color: red }
  +.x { color: blue }
               ^
  
  [124]


