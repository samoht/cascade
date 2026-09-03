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
