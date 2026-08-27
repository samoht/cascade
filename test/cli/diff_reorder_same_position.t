CLI: cascade diff - a move the rule index cannot express.

A rule's position in these entries is its index among the rules of its
container, but whether the selector moved is judged against the selectors
both sides share. The two disagree: a selector can hold the same index on
both sides and still have moved, so an entry pairing that index with itself
carries no information and the report has to say so and keep printing.

Every pair of these three inverts, so each selector moved, and the middle
one keeps its index while doing it. The report names the move without a
coordinate rather than pairing position 1 with position 1.

  $ cat > reversed_ref.css <<'CSS'
  > .b{--c:1}.a{--c:2}.c{--c:3}.z{--e:9}
  > CSS
  $ cat > reversed_tw.css <<'CSS'
  > .c{--c:3}.a{--c:2}.b{--c:1}
  > CSS
  $ cascade diff --diff=tree --depth=max reversed_ref.css reversed_tw.css
  CSS: 37 chars vs 28 chars (24.3% diff)
  Changes: 1 removed rule, 2 reordered rules
  
  --- reversed_ref.css
  +++ reversed_tw.css
  └─ .z
        - --e 9
  Rules reordered (2 rules):
  ├─ .b (position 2) ↔  .c (position 0)
  └─ .a (moved)
  
  [1]

The same pair inside a container reports through the container renderer.

  $ cat > layer_ref.css <<'CSS'
  > @layer u{.b{--c:1}.a{--c:2}.c{--c:3}.z{--e:9}}
  > CSS
  $ cat > layer_tw.css <<'CSS'
  > @layer u{.c{--c:3}.a{--c:2}.b{--c:1}}
  > CSS
  $ cascade diff --diff=tree --depth=max layer_ref.css layer_tw.css
  CSS: 47 chars vs 38 chars (19.1% diff)
  Changes: 1 changed container
  
  --- layer_ref.css
  +++ layer_tw.css
  └─ @layer u (1 removed, 2 reordered)
     ├─ .z
     │     - --e 9
     ├─ .b (position 2) ↔  .c (position 0)
     └─ .a (moved)
  
  [1]

A dropped rule ahead of the selector lands the move on the same index too.
Here `.a` really does cross `.b`, and the index is 1 on both sides only
because `.z` left.

  $ cat > shifted_ref.css <<'CSS'
  > .z{--e:9}.a{--c:2}.b{--c:1}
  > CSS
  $ cat > shifted_tw.css <<'CSS'
  > .b{--c:1}.a{--c:2}
  > CSS
  $ cascade diff --diff=tree --depth=max shifted_ref.css shifted_tw.css
  CSS: 28 chars vs 19 chars (32.1% diff)
  Changes: 1 removed rule, 1 reordered rule
  
  --- shifted_ref.css
  +++ shifted_tw.css
  └─ .z
        - --e 9
  Rules reordered (1 rules):
  └─ .a (moved)
  
  [1]
