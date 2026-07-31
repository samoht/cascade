CLI: cascade diff - one node per selector.

A selector written by more than one rule in a container used to produce a
node per rule, each carrying the same label: one saying a declaration was
added and another that a different one was removed, which reads as a
contradiction rather than as the declarations sitting on different rules.
The group is reported once.

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
  CSS: 1003 chars vs 1024 chars (2.1% diff)
  Changes: 1 changed container
  
  --- ref.css
  +++ tw.css
  └─ @layer utilities (1 modified)
     └─ .drop-shadow-indigo-500
           - --tw-drop-shadow-color
  
  [1]
