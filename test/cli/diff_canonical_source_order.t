CLI: canonical diff reports source-order changes, not projection churn.

Canonical rule ordering is computed from each sheet's cascade dependencies.
Changing one declaration can therefore change the generated order of rules
that stayed at the same source position. That projection churn is not an
authored reorder and must not be reported as one.

Here `.a` is second in both inputs. Changing `.c` removes its conflict with
`.a`, which changes their order in the independently canonicalized trees, but
the report contains only the declaration change the author made.

  $ cat > before.css <<'CSS'
  > .c{color:blue}.a{color:red}.b{margin-top:1px}
  > CSS
  $ cat > after.css <<'CSS'
  > .c{background-color:blue}.a{color:red}.b{margin-top:1px}
  > CSS
  $ cascade diff --diff=canonical --limit=none before.css after.css
  CSS: 46 chars vs 57 chars (23.9% diff)
  Changes: 1 modified rule
  
  --- before.css
  +++ after.css
  └─ .c
        - color: #00f
        + background-color: #00f
  
  [1]

A real source-order change remains visible. These selectors can match the same
element and both write `color`, so swapping the rules changes the winner.

  $ cat > move-before.css <<'CSS'
  > .a{color:red}.b{color:blue}
  > CSS
  $ cat > move-after.css <<'CSS'
  > .b{color:blue}.a{color:red}
  > CSS
  $ cascade diff --diff=canonical --limit=none move-before.css move-after.css
  CSS: 28 chars vs 28 chars (0.0% diff)
  Changes: 1 reordered rule
  
  --- move-before.css
  +++ move-after.css
  Rules reordered (1 rules):
  └─ .a (moved)
  
  [1]

A reordering the projection normalises is not an authored reorder either, and
one real difference elsewhere does not make it one. These two sheets differ
only in the order of two rules writing different properties, so no element
resolves differently either way.

  $ cat > neutral.css <<'CSS'
  > .a{color:red}.b{margin:0}
  > CSS
  $ cat > neutral-swapped.css <<'CSS'
  > .b{margin:0}.a{color:red}
  > CSS
  $ cascade diff --diff=canonical --limit=none neutral.css neutral-swapped.css
  CSS files are identical

Adding a rule to the second sheet says nothing about that swap, so the report
names the added rule and stops.

  $ cat > neutral-extra.css <<'CSS'
  > .b{margin:0}.a{color:red}.y{outline:0}
  > CSS
  $ cascade diff --diff=canonical --limit=none neutral.css neutral-extra.css
  CSS: 26 chars vs 39 chars (50.0% diff)
  Changes: 1 added rule
  
  --- neutral.css
  +++ neutral-extra.css
  └─ .y
        + outline: 0
  
  [1]

A whole block reads the same way. CSS Properties and Values API 1 sec. 2 makes
registrations for different names order-independent, so the order of a run of
`@property` rules is not a difference, with or without the added rule.

  $ cat > props.css <<'CSS'
  > @property --a{syntax:"*";inherits:false}@property --b{syntax:"*";inherits:false}@property --c{syntax:"*";inherits:false}.x{color:red}
  > CSS
  $ cat > props-shuffled.css <<'CSS'
  > @property --c{syntax:"*";inherits:false}@property --a{syntax:"*";inherits:false}@property --b{syntax:"*";inherits:false}.x{color:red}
  > CSS
  $ cascade diff --diff=canonical --limit=none props.css props-shuffled.css
  CSS files are identical

  $ cat > props-extra.css <<'CSS'
  > @property --c{syntax:"*";inherits:false}@property --a{syntax:"*";inherits:false}@property --b{syntax:"*";inherits:false}.x{color:red}.y{outline:0}
  > CSS
  $ cascade diff --diff=canonical --limit=none props.css props-extra.css
  CSS: 134 chars vs 147 chars (9.7% diff)
  Changes: 1 added rule
  
  --- props.css
  +++ props-extra.css
  └─ .y
        + outline: 0
  
  [1]

A move the projection does keep is reported however the sheet spells the
prelude it stands on. Media Queries 4 sec. 4.2 gives `min-width` and the range
form one meaning, so the projection names the block the second way, and both
blocks write `color` for `.a`, which an element 25px wide resolves by taking
whichever comes last.

  $ cat > mq.css <<'CSS'
  > @media (min-width:10px){.a{color:red}}@media (min-width:20px){.a{color:#00f}}.z{margin:0}
  > CSS
  $ cat > mq-swapped.css <<'CSS'
  > @media (min-width:20px){.a{color:#00f}}@media (min-width:10px){.a{color:red}}.z{margin:1px}
  > CSS
  $ cascade diff --diff=canonical --limit=none mq.css mq-swapped.css
  CSS: 90 chars vs 92 chars (2.2% diff)
  Changes: 1 modified rule, 1 changed container
  
  --- mq.css
  +++ mq-swapped.css
  ├─ .z
  │     * margin: 0 -> 1px
  └─ @media (width >= 10px) (moved)
  
  [1]

Whether a move is a difference is settled by what the two statements write, not
by whether the sheets also differ somewhere else. Here an `@container` block
and a bare rule both write `color` for `.a`, so an element inside a 28rem
container takes whichever comes last, and the move is a difference on its own.

  $ cat > ctr.css <<'CSS'
  > @container (width>=28rem){.a{color:red}}.a{color:blue}.z{top:0}
  > CSS
  $ cat > ctr-moved.css <<'CSS'
  > .a{color:blue}@container (width>=28rem){.a{color:red}}.z{top:0}
  > CSS
  $ cascade diff --diff=canonical --limit=none ctr.css ctr-moved.css
  CSS: 64 chars vs 64 chars (0.0% diff)
  Changes: 1 changed container
  
  --- ctr.css
  +++ ctr-moved.css
  └─ @container (width >= 28rem) (moved)
  
  [1]

Changing `.z` as well says nothing about that move, so both are reported.

  $ cat > ctr-moved-edited.css <<'CSS'
  > .a{color:blue}@container (width>=28rem){.a{color:red}}.z{top:9px}
  > CSS
  $ cascade diff --diff=canonical --limit=none ctr.css ctr-moved-edited.css
  CSS: 64 chars vs 66 chars (3.1% diff)
  Changes: 1 modified rule, 1 changed container
  
  --- ctr.css
  +++ ctr-moved-edited.css
  ├─ .z
  │     * top: 0 -> 9px
  └─ @container (width >= 28rem) (moved)
  
  [1]

The same block over a selector nothing outside it writes `left` for reaches no
element the rules around it reach, so that move is no difference at all, with
or without the change to `.z`.

  $ cat > free.css <<'CSS'
  > @container (width>=28rem){.b{left:0}}.a{color:blue}.z{top:0}
  > CSS
  $ cat > free-moved.css <<'CSS'
  > .a{color:blue}@container (width>=28rem){.b{left:0}}.z{top:0}
  > CSS
  $ cascade diff --diff=canonical --limit=none free.css free-moved.css
  CSS files are identical

  $ cat > free-moved-edited.css <<'CSS'
  > .a{color:blue}@container (width>=28rem){.b{left:0}}.z{top:9px}
  > CSS
  $ cascade diff --diff=canonical --limit=none free.css free-moved-edited.css
  CSS: 61 chars vs 63 chars (3.3% diff)
  Changes: 1 modified rule
  
  --- free.css
  +++ free-moved-edited.css
  └─ .z
        * top: 0 -> 9px
  
  [1]

An `@media` block is read the same way, so the one whose rules reach nothing
around it moves freely beside a changed declaration.

  $ cat > mfree.css <<'CSS'
  > @media (min-width:10px){.b{left:0}}.a{color:blue}.z{top:0}
  > CSS
  $ cat > mfree-moved.css <<'CSS'
  > .a{color:blue}@media (min-width:10px){.b{left:0}}.z{top:0}
  > CSS
  $ cascade diff --diff=canonical --limit=none mfree.css mfree-moved.css
  CSS files are identical

  $ cat > mfree-moved-edited.css <<'CSS'
  > .a{color:blue}@media (min-width:10px){.b{left:0}}.z{top:9px}
  > CSS
  $ cascade diff --diff=canonical --limit=none mfree.css mfree-moved-edited.css
  CSS: 59 chars vs 61 chars (3.4% diff)
  Changes: 1 modified rule
  
  --- mfree.css
  +++ mfree-moved-edited.css
  └─ .z
        * top: 0 -> 9px
  
  [1]

A move inside a block reads the block's own answer, and finds it through the
prelude the projection keys the block by rather than the one the sheet writes.
Both rules here write `color` for an element carrying either class.

  $ cat > inner.css <<'CSS'
  > @media (min-width:10px){.a{color:red}.b{color:blue}}.z{top:0}
  > CSS
  $ cat > inner-moved.css <<'CSS'
  > @media (min-width:10px){.b{color:blue}.a{color:red}}.z{top:9px}
  > CSS
  $ cascade diff --diff=canonical --limit=none inner.css inner-moved.css
  CSS: 62 chars vs 64 chars (3.2% diff)
  Changes: 1 modified rule, 1 changed container
  
  --- inner.css
  +++ inner-moved.css
  ├─ .z
  │     * top: 0 -> 9px
  └─ @media (width >= 10px) (1 reordered)
     └─ .a (moved)
  
  [1]
