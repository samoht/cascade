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
  CSS: 18 chars vs 19 chars (5.6% diff)
  Changes: 1 modified rule
  
  --- c.css
  +++ d.css
  └─ .x
        * color: red -> blue
  
  [1]

Forcing colour on emits ANSI markers even into a pipe; the default
resolves to plain off-tty, as the NO_COLOR run above shows.

  $ cascade diff --color=always c.css d.css | cat -v | head -5
  CSS: 18 chars vs 19 chars (5.6% diff)
  Changes: 1 modified rule
  
  ^[[33m---^[[0m ^[[33mc.css^[[0m
  ^[[33m+++^[[0m ^[[33md.css^[[0m

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
  CSS: 18 chars vs 19 chars (5.6% diff)
  Changes: 1 modified rule
  
  --- e.css
  +++ f.css
  └─ .x
        * color: red -> #f00
  
  [1]


The canonical diff mode compares canonical minified CSS and accepts
equivalent spellings.

  $ cat > i.css <<EOF
  > .x { color: color-mix(in oklab, currentcolor 50%, transparent) }
  > EOF
  $ cat > j.css <<EOF
  > .x { color: color-mix(in oklab, currentcolor 50%, #0000) }
  > EOF
  $ cascade diff --diff=canonical i.css j.css
  CSS files are identical

Canonical mode also accepts moving a rule past a conditional block when their
contents cannot conflict (here [display] on one selector vs [flex-grow] on
others), including when a same-condition block was split around the rule.

  $ cat > grouped.css <<EOF
  > @layer utilities{.md\:block{display:block}@media (min-width:48rem){.md\:flex-grow,.md\:grow{flex-grow:1}}}
  > EOF
  $ cat > split.css <<EOF
  > @layer utilities{@media (min-width:48rem){.md\:flex-grow{flex-grow:1}}.md\:block{display:block}@media (min-width:48rem){.md\:grow{flex-grow:1}}}
  > EOF
  $ cascade diff --diff=canonical grouped.css split.css
  CSS files are identical

Canonical mode reorders declarations that write disjoint cascade slots, so two
rules holding the same declarations in a different commuting order compare
identical, while a shorthand and its longhand (which overlap) stay ordered.

  $ cat > order-a.css <<EOF
  > .sr-only{position:absolute;clip-path:inset(50%);width:1px;overflow:hidden}
  > EOF
  $ cat > order-b.css <<EOF
  > .sr-only{width:1px;overflow:hidden;position:absolute;clip-path:inset(50%)}
  > EOF
  $ cascade diff --diff=canonical order-a.css order-b.css
  CSS files are identical

  $ cat > over-a.css <<EOF
  > .x{margin:0;margin-top:5px}
  > EOF
  $ cat > over-b.css <<EOF
  > .x{margin-top:5px;margin:0}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=canonical over-a.css over-b.css > /dev/null; echo $?
  1

Canonical mode also equates different factorings of the same content: a
declaration hoisted into a shared selector-list group is the same declaration
written inline.

  $ cat > hoisted.css <<EOF
  > .absolute,.sr-only{position:absolute}.sr-only{white-space:nowrap}
  > EOF
  $ cat > inline.css <<EOF
  > .absolute{position:absolute}.sr-only{white-space:nowrap;position:absolute}
  > EOF
  $ cascade diff --diff=canonical hoisted.css inline.css
  CSS files are identical

A conditional block whose rules write the same property on the same selector
stays ordered: swapping it with the rule is a real difference.

  $ cat > before.css <<EOF
  > .a{display:block}@media print{.a{display:flex}}
  > EOF
  $ cat > after.css <<EOF
  > @media print{.a{display:flex}}.a{display:block}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=canonical before.css after.css
  CSS: 48 chars vs 48 chars (0.0% diff)
  Changes: 1 reordered rule
  
  --- before.css
  +++ after.css
  Rules reordered (1 rules):
  └─ .a ↔  @media print
  
  [1]

The same swap the other way round, with the block ahead of the rule. Which
of the two the walk names is its own choice; what may not happen is calling
the pair identical.

  $ NO_COLOR=1 cascade diff --diff=tree before.css after.css > /dev/null; echo $?
  1
  $ NO_COLOR=1 cascade diff --diff=tree after.css before.css > /dev/null; echo $?
  1

  $ cat > swap-a.css <<EOF
  > @media (min-width:10px){a{color:red}}a{color:blue}
  > EOF
  $ cat > swap-b.css <<EOF
  > a{color:blue}@media (min-width:10px){a{color:red}}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=tree swap-a.css swap-b.css > /dev/null; echo $?
  1
  $ NO_COLOR=1 cascade diff --diff=canonical swap-a.css swap-b.css > /dev/null; echo $?
  1



The --diff=string mode falls back to character-level diffing.

  $ cat > g.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > h.css <<EOF
  > .x { color: blue }
  > EOF
  $ NO_COLOR=1 cascade diff --diff=string g.css h.css
  CSS: 18 chars vs 19 chars (5.6% diff)
  Changes: none classified structurally (see report below)
  
  Strings differ at position 12 (line 0, col 12)
  
  --- g.css
  +++ h.css
  @@ position 12 @@
  -.x { color: red }
  +.x { color: blue }
               ^
  
  [1]



The size summary lists the two files in the order of the headers below it:
the first file given, then the second.

  $ cat > one.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > many.css <<EOF
  > .x { color: red } .y { top: 0 } .z { left: 0 }
  > EOF
  $ NO_COLOR=1 cascade diff one.css many.css
  CSS: 18 chars vs 47 chars (161.1% diff)
  Changes: 2 added rules
  
  --- one.css
  +++ many.css
  ├─ .y
  │     + top 0
  └─ .z
        + left 0
  
  [1]
