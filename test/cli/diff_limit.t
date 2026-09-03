CLI: cascade diff - report shaping (breadth).

A report too wide for the line budget shows fewer differences in full
rather than every difference with its body cut: three complete entries
and a count of the rest can be acted on, a column of bare selectors
cannot.

  $ for i in $(seq 1 20); do
  >   echo ".r$i { color: red; margin: 0; padding: 0; width: 1px }" >> a.css
  >   echo ".r$i { color: blue; margin: 1px; padding: 2px; width: 2px }" >> b.css
  > done
  $ NO_COLOR=1 cascade diff a.css b.css
  CSS: 1091 chars vs 1191 chars (9.2% diff)
  Changes: 20 modified rules
  
  --- a.css
  +++ b.css
  ├─ .r1
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  ├─ .r2
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  ├─ .r3
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  ├─ .r4
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  ├─ .r5
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  ├─ .r6
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  ├─ .r7
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  └─ ...13 more differences
  
  (limit 7; use --limit=none for the full report)
  [1]



An explicit count overrides the budget.

  $ NO_COLOR=1 cascade diff --limit=2 a.css b.css
  CSS: 1091 chars vs 1191 chars (9.2% diff)
  Changes: 20 modified rules
  
  --- a.css
  +++ b.css
  ├─ .r1
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  ├─ .r2
  │     * color: red -> blue
  │     * margin: 0 -> 1px
  │     * padding: 0 -> 2px
  │     * width: 1px -> 2px
  └─ ...18 more differences
  
  [1]



--limit=none bounds nothing, so every difference is named and only the
depth fallback shapes the report.

  $ NO_COLOR=1 cascade diff --limit=none a.css b.css | grep -c '\.r[0-9]'
  20
  $ NO_COLOR=1 cascade diff --limit=none a.css b.css | grep 'more differences'
  [1]


A report that already fits is printed whole, entry limit or not.

  $ cat > small-a.css <<EOF
  > .x { color: red; margin: 0 }
  > EOF
  $ cat > small-b.css <<EOF
  > .x { color: blue; margin: 1px }
  > EOF
  $ NO_COLOR=1 cascade diff small-a.css small-b.css
  CSS: 29 chars vs 32 chars (10.3% diff)
  Changes: 1 modified rule
  
  --- small-a.css
  +++ small-b.css
  └─ .x
        * color: red -> blue
        * margin: 0 -> 1px
  
  [1]


Truncating a section moves the closing connector onto the count line, so a
bounded report never ends on a branch with nothing under it.

  $ cat > m-a.css <<EOF
  > @media print { .a { color: red } }
  > @media screen { .a { color: red } }
  > @media (min-width: 10px) { .a { color: red } }
  > EOF
  $ cat > m-b.css <<EOF
  > @media print { .a { color: blue } }
  > @media screen { .a { color: blue } }
  > @media (min-width: 10px) { .a { color: blue } }
  > EOF
  $ NO_COLOR=1 cascade diff --limit=2 m-a.css m-b.css | grep -cE '^├─'
  2
  $ NO_COLOR=1 cascade diff --limit=2 m-a.css m-b.css | grep -E '^└─'
  └─ ...1 more difference


One difference that overflows the budget on its own is still printed whole:
a difference a reader can act on and a count of the rest is the floor, not
every difference with its body cut.

  $ for i in 1 2 3; do
  >   printf '@media (min-width: %spx) {' "$i" >> wide-a.css
  >   printf '@media (min-width: %spx) {' "$i" >> wide-b.css
  >   for j in $(seq 1 15); do
  >     printf ' .r%s { color: red; margin: 0 }' "$j" >> wide-a.css
  >     printf ' .r%s { color: blue; margin: 1px }' "$j" >> wide-b.css
  >   done
  >   printf ' }\n' >> wide-a.css
  >   printf ' }\n' >> wide-b.css
  > done
  $ NO_COLOR=1 cascade diff wide-a.css wide-b.css | grep -cE '^(├|└)─ @media'
  1
  $ NO_COLOR=1 cascade diff wide-a.css wide-b.css | grep -c ' -> '
  30
  $ NO_COLOR=1 cascade diff wide-a.css wide-b.css | grep 'more lines'
  [1]
  $ NO_COLOR=1 cascade diff wide-a.css wide-b.css | tail -3
  └─ ...2 more differences
  
  (limit 1; use --limit=none for the full report)
