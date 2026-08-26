CLI: cascade diff - one node per selector.

A selector written by more than one rule reports once. When every declaration
it writes survives on both sides, spread differently over those rules, the
entry names the move, where the selector was named twice, once losing a
declaration and once gaining another.

These two are a reduced Tailwind utilities layer: each utility is a bare
rule, an @supports rule and another bare rule, and the two sides emit the
blue group and the indigo group in the opposite order. That order is
cascade-significant for an element carrying both classes, so the difference
is real and must still be reported.

  $ cat > ref.css <<'EOF'
  > @layer utilities{.drop-shadow-sm{--tw-drop-shadow-size:drop-shadow(0 1px 2px var(--tw-drop-shadow-color,#00000026));--tw-drop-shadow:drop-shadow(var(--drop-shadow-sm));filter:var(--tw-blur,) var(--tw-brightness,) var(--tw-contrast,) var(--tw-grayscale,) var(--tw-hue-rotate,) var(--tw-invert,) var(--tw-saturate,) var(--tw-sepia,) var(--tw-drop-shadow,)}.drop-shadow-blue-500\/50{--tw-drop-shadow-color:#3080ff80}@supports (color:color-mix(in lab, red, red)){.drop-shadow-blue-500\/50{--tw-drop-shadow-color:color-mix(in oklab, color-mix(in oklab, var(--color-blue-500) 50%, transparent) var(--tw-drop-shadow-alpha), transparent)}}.drop-shadow-blue-500\/50{--tw-drop-shadow:var(--tw-drop-shadow-size)}.drop-shadow-indigo-500{--tw-drop-shadow-color:oklch(58.5% .233 277.117)}@supports (color:color-mix(in lab, red, red)){.drop-shadow-indigo-500{--tw-drop-shadow-color:color-mix(in oklab, var(--color-indigo-500) var(--tw-drop-shadow-alpha), transparent)}}.drop-shadow-indigo-500{--tw-drop-shadow:var(--tw-drop-shadow-size)}}
  > EOF
  $ cat > tw.css <<'EOF'
  > @layer utilities{.drop-shadow-sm{--tw-drop-shadow-size:drop-shadow(0 1px 2px var(--tw-drop-shadow-color,#00000026));--tw-drop-shadow:drop-shadow(var(--drop-shadow-sm));filter:var(--tw-blur,)var(--tw-brightness,)var(--tw-contrast,)var(--tw-grayscale,)var(--tw-hue-rotate,)var(--tw-invert,)var(--tw-saturate,)var(--tw-sepia,)var(--tw-drop-shadow,)}.drop-shadow-indigo-500{--tw-drop-shadow-color:oklch(58.5%.233 277.117)}@supports(color:color-mix(in lab,red,red)){.drop-shadow-indigo-500{--tw-drop-shadow-color:color-mix(in oklab,var(--color-indigo-500) var(--tw-drop-shadow-alpha),transparent)}}.drop-shadow-indigo-500{--tw-drop-shadow:var(--tw-drop-shadow-size)}.drop-shadow-blue-500\/50{--tw-drop-shadow-color:#3080ff80}@supports(color:color-mix(in lab,red,red)){.drop-shadow-blue-500\/50{--tw-drop-shadow-color:color-mix(in oklab,color-mix(in oklab,var(--color-blue-500) 50%,transparent) var(--tw-drop-shadow-alpha),transparent)}}.drop-shadow-blue-500\/50{--tw-drop-shadow:var(--tw-drop-shadow-size)}}
  > EOF
  $ cascade diff --diff=canonical --prune-unused-custom-props --depth=max ref.css tw.css
  CSS: 1024 chars vs 1003 chars (2.1% diff)
  Changes: 1 changed container
  
  --- ref.css
  +++ tw.css
  └─ @layer utilities (1 rearranged)
     └─ .drop-shadow-indigo-500 (moved between rules)
             --tw-drop-shadow-color color-mix(in oklab,var(--col...tw-drop-shadow-alpha),#0000)
             --tw-drop-shadow var(--tw-drop-shadow-size)
  
  [1]



A top-level group reports the same way as one inside a container.

  $ cat > split.css <<'EOF'
  > .a{color:red}.b{color:blue}.a{margin:0}
  > EOF
  $ cat > joined.css <<'EOF'
  > .a{color:red;margin:0}.b{color:blue}
  > EOF
  $ cascade diff --depth=max split.css joined.css
  CSS: 40 chars vs 37 chars (7.5% diff)
  Changes: 1 rearranged rule
  
  --- split.css
  +++ joined.css
  └─ .a (moved between rules)
          color red
          margin 0
  
  [1]



  $ cat > split_layer.css <<'EOF'
  > @layer u{.a{color:red}.b{color:blue}.a{margin:0}}
  > EOF
  $ cat > joined_layer.css <<'EOF'
  > @layer u{.a{color:red;margin:0}.b{color:blue}}
  > EOF
  $ cascade diff --depth=max split_layer.css joined_layer.css
  CSS: 50 chars vs 47 chars (6.0% diff)
  Changes: 1 changed container
  
  --- split_layer.css
  +++ joined_layer.css
  └─ @layer u (1 rearranged)
     └─ .a (moved between rules)
             color red
             margin 0
  
  [1]



A declaration that does not survive is reported as a loss.

  $ cat > lost.css <<'EOF'
  > .a{color:red}.b{color:blue}
  > EOF
  $ cascade diff --depth=max split.css lost.css
  CSS: 40 chars vs 28 chars (30.0% diff)
  Changes: 1 removed rule
  
  --- split.css
  +++ lost.css
  └─ .a
        - margin 0
  
  [1]



The property name appearing on both sides is not enough: a changed value or
an added !important changes what the selector writes.

  $ cat > weighted.css <<'EOF'
  > .a{color:red;margin:0 !important}.b{color:blue}
  > EOF
  $ cascade diff --depth=max split.css weighted.css
  CSS: 40 chars vs 48 chars (20.0% diff)
  Changes: 1 modified rule
  
  --- split.css
  +++ weighted.css
  └─ .a
        * margin: 0 -> 0 !important
  
  [1]





The guard does not have to be one the optimizer can drop. Under a feature
query no browser satisfies, each utility keeps all three of its rules and
the two sides still differ only in which group comes first. Neither selector
gains or loses a declaration, so neither may be reported as modified.

  $ cat > guarded_ref.css <<'EOF'
  > @layer utilities{.x{--c:1}@supports (zoo:bar){.x{--c:2}}.x{--d:3}.y{--c:4}@supports (zoo:bar){.y{--c:5}}.y{--d:6}}
  > EOF
  $ cat > guarded_tw.css <<'EOF'
  > @layer utilities{.y{--c:4}@supports (zoo:bar){.y{--c:5}}.y{--d:6}.x{--c:1}@supports (zoo:bar){.x{--c:2}}.x{--d:3}}
  > EOF
  $ cascade diff --diff=canonical --depth=max guarded_ref.css guarded_tw.css
  CSS: 115 chars vs 115 chars (0.0% diff)
  Changes: 1 changed container
  
  --- guarded_ref.css
  +++ guarded_tw.css
  └─ @layer utilities (1 reordered, 2 rearranged)
     ├─ .x (position 2) ↔  .y (position 0)
     ├─ .x (moved between rules)
     │       --c 1
     │       --d 3
     ├─ .y (moved between rules)
     │       --c 4
     │       --d 6
     └─ @supports (zoo: bar) (1 reordered)
        └─ .x ↔  .y
  
  [1]



The tree view of the same pair names the move once. A [Reordered] entry
carries the selector and the position that selector holds on each side, not
the rule that carried it, so the two rules of `.x` crossing `.y` together
are one fact stated once, and the summary counts what the node reports.

  $ cascade diff --diff=tree --depth=max guarded_ref.css guarded_tw.css
  CSS: 115 chars vs 115 chars (0.0% diff)
  Changes: 1 changed container
  
  --- guarded_ref.css
  +++ guarded_tw.css
  └─ @layer utilities (1 reordered)
     ├─ .x (position 3) ↔  .y (position 0)
     └─ @supports (zoo: bar) (1 reordered)
        └─ .x ↔  .y
  
  [1]



Nothing about the walk that pairs the two sides position by position: a
structural change elsewhere in the container sends the same selector down
the exact-match path, which names the move once as well.

  $ cat > swap_ref.css <<'EOF'
  > @layer u{.x{--c:1}.x{--d:3}.y{--c:4}.z{--e:9}}
  > EOF
  $ cat > swap_tw.css <<'EOF'
  > @layer u{.y{--c:4}.x{--c:1}.x{--d:3}}
  > EOF
  $ cascade diff --diff=tree --depth=max swap_ref.css swap_tw.css
  CSS: 47 chars vs 38 chars (19.1% diff)
  Changes: 1 changed container
  
  --- swap_ref.css
  +++ swap_tw.css
  └─ @layer u (1 removed, 1 reordered)
     ├─ .z
     │     - --e 9
     └─ .x ↔  .y
  
  [1]
