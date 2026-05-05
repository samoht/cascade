CLI: --minify dead-code elimination across the CSS AST.

In CSS, "dead code" is a declaration, rule, or at-rule whose removal
does not change the cascaded value for any element. The cascade
resolves declarations by origin, importance, layer, scope, specificity,
and source order; a node is dead when an unconditionally-later node of
the same property/condition at the same or higher cascade weight
guarantees it can never win.

Each section below covers a distinct AST level where elimination
applies under --minify, parity with cssnano and Lightning CSS.


# Declaration-level dead code


Exact-duplicate declarations within a rule collapse to one.

  $ cat > dup.css <<EOF
  > .x { color: red; color: red; padding: 10px; padding: 10px }
  > EOF
  $ cascade --minify dup.css
  .x{color:red;padding:10px}

A shadowed shorthand declaration is dead - the later shorthand sets all
longhands the earlier one set.

  $ cat > shadow-shorthand.css <<EOF
  > .x { margin: 10px; margin: 5px }
  > .y { background: red; background: blue }
  > EOF
  $ cascade --minify shadow-shorthand.css
  .x{margin:5px}.y{background:#00f}

A shorthand followed by a longhand merges - the shorthand expands to
its longhands and the later longhand wins for its slot.

  $ cat > shorthand-longhand.css <<EOF
  > .x { margin: 10px; margin-top: 20px }
  > EOF
  $ cascade --minify shorthand-longhand.css
  .x{margin:20px 10px 10px}

A longhand followed by a shorthand discards the earlier longhand - the
shorthand sets all four sides, including the slot the longhand was
targeting.

  $ cat > longhand-shorthand.css <<EOF
  > .x { margin-top: 20px; margin: 10px }
  > EOF
  $ cascade --minify longhand-shorthand.css
  .x{margin:10px}

A non-important declaration shadowed by a same-property [!important]
in the same rule is dead (the !important always wins per Cascade L6
§6.3).

  $ cat > imp-shadows.css <<EOF
  > .x { color: red; color: blue !important }
  > .y { color: red !important; color: blue }
  > EOF
  $ cascade --minify imp-shadows.css
  .x{color:#00f!important}.y{color:red!important}

A later !important shadows an earlier !important of the same property.

  $ cat > both-imp.css <<EOF
  > .x { color: red !important; color: blue !important }
  > EOF
  $ cascade --minify both-imp.css
  .x{color:#00f!important}

A vendor-prefixed property followed by the unprefixed equivalent is
NOT dead - the cascade picks whichever the browser understands. Both
must round-trip.

  $ cat > prefix.css <<EOF
  > .x { -webkit-mask-image: url(a.png); mask-image: url(a.png) }
  > EOF
  $ cascade --minify prefix.css
  .x{-webkit-mask-image:url(a.png);mask-image:url(a.png)}


# Rule-level dead code


Empty rules are removed.

  $ cat > empty-rule.css <<EOF
  > .x { }
  > .y { color: red }
  > .z { }
  > EOF
  $ cascade --minify empty-rule.css
  .y{color:red}

A rule that becomes empty after declaration-level elimination is also
removed (cascading dead code: dead declarations -> dead rule).

  $ cat > rule-becomes-empty.css <<EOF
  > .x { color: red; color: red }
  > .y { }
  > EOF
  $ cascade --minify rule-becomes-empty.css
  .x{color:red}

Adjacent rules with the same selector merge.

  $ cat > adjacent.css <<EOF
  > .x { color: red }
  > .x { padding: 10px }
  > .x { margin: 5px }
  > EOF
  $ cascade --minify adjacent.css
  .x{color:red;padding:10px;margin:5px}

Non-adjacent rules with the same selector do NOT merge - an
intervening rule means cascade order matters and merging would change
which declaration wins.

  $ cat > non-adjacent.css <<EOF
  > .x { padding: 10px }
  > .y { color: red }
  > .x { margin: 5px }
  > EOF
  $ cascade --minify non-adjacent.css
  .x{padding:10px}.y{color:red}.x{margin:5px}

Different selectors with the same declaration block group into a
single rule with a selector list.

  $ cat > group.css <<EOF
  > .a { color: red }
  > .b { color: red }
  > .c { color: red }
  > EOF
  $ cascade --minify group.css
  .a,.b,.c{color:red}

Duplicate selectors within a single selector list dedup.

  $ cat > selector-dup.css <<EOF
  > .a, .a, .b { color: red }
  > EOF
  $ cascade --minify selector-dup.css
  .a,.b{color:red}


# At-rule-level dead code


Empty [@media] blocks are removed.

  $ cat > empty-media.css <<EOF
  > @media screen { }
  > .x { color: red }
  > EOF
  $ cascade --minify empty-media.css
  .x{color:red}

Empty [@supports] blocks are removed.

  $ cat > empty-supports.css <<EOF
  > @supports (display: grid) { }
  > .x { color: red }
  > EOF
  $ cascade --minify empty-supports.css
  .x{color:red}

Empty [@container] blocks are removed.

  $ cat > empty-container.css <<EOF
  > @container (min-width: 30em) { }
  > .x { color: red }
  > EOF
  $ cascade --minify empty-container.css
  .x{color:red}

Empty named [@layer] blocks collapse to the statement form, which
still contributes the layer to the layer order per CSS Cascade L6
§6.4.4.2.

  $ cat > empty-layer.css <<EOF
  > @layer base { }
  > @layer theme { }
  > .x { color: red }
  > EOF
  $ cascade --minify empty-layer.css
  @layer base,theme;.x{color:red}

Empty anonymous [@layer] blocks are removed (anonymous layers cannot
be referenced again, so an empty one contributes nothing).

  $ cat > empty-anon-layer.css <<EOF
  > @layer { }
  > .x { color: red }
  > EOF
  $ cascade --minify empty-anon-layer.css
  .x{color:red}

Empty [@scope] blocks are removed.

  $ cat > empty-scope.css <<EOF
  > @scope (.card) to (.boundary) { }
  > .x { color: red }
  > EOF
  $ cascade --minify empty-scope.css
  .x{color:red}

Empty [@starting-style] blocks are removed.

  $ cat > empty-starting.css <<EOF
  > @starting-style { }
  > .x { color: red }
  > EOF
  $ cascade --minify empty-starting.css
  .x{color:red}

Adjacent same-condition [@media] blocks merge.

  $ cat > adj-media.css <<EOF
  > @media screen { .a { color: red } }
  > @media screen { .b { color: blue } }
  > EOF
  $ cascade --minify adj-media.css
  @media screen{.a{color:red}.b{color:#00f}}

Adjacent same-name [@layer] blocks merge.

  $ cat > adj-layer.css <<EOF
  > @layer base { .a { color: red } }
  > @layer base { .b { color: blue } }
  > EOF
  $ cascade --minify adj-layer.css
  @layer base{.a{color:red}.b{color:#00f}}

Adjacent same-condition [@supports] blocks merge.

  $ cat > adj-supports.css <<EOF
  > @supports (display: grid) { .a { color: red } }
  > @supports (display: grid) { .b { color: blue } }
  > EOF
  $ cascade --minify adj-supports.css
  @supports (display:grid){.a{color:red}.b{color:#00f}}

Adjacent same-condition [@container] blocks merge.

  $ cat > adj-container.css <<EOF
  > @container (min-width: 30em) { .a { color: red } }
  > @container (min-width: 30em) { .b { color: blue } }
  > EOF
  $ cascade --minify adj-container.css
  @container (min-width:30em){.a{color:red}.b{color:#00f}}

Non-adjacent same-condition at-rules do NOT merge - an intervening
rule could affect cascade order.

  $ cat > non-adj-media.css <<EOF
  > @media screen { .a { color: red } }
  > .x { color: blue }
  > @media screen { .b { color: green } }
  > EOF
  $ cascade --minify non-adj-media.css
  @media screen{.a{color:red}}.x{color:#00f}@media screen{.b{color:green}}

A misplaced [@import] (after a rule statement) is invalid per CSS
Cascade L6 §2 and is dropped during parsing.

  $ cat > misplaced-import.css <<EOF
  > .x { color: red }
  > @import url("late.css");
  > .y { color: blue }
  > EOF
  $ cascade --minify misplaced-import.css 2>&1 | grep -v "warning"
  .x{color:red}.y{color:#00f}


# Selector-level dead code


Universal selector in a non-solitary compound is redundant per CSS
Selectors L4 §3.5; the [*] is dropped.

  $ cat > universal.css <<EOF
  > *.foo { color: red }
  > *#main { color: blue }
  > *[data-x] { padding: 10px }
  > EOF
  $ cascade --minify universal.css
  .foo{color:red}#main{color:#00f}[data-x]{padding:10px}

Single-argument [:is()] is spec-equivalent to the bare selector and
unwraps under shortest-wins.

  $ cat > is-unwrap.css <<EOF
  > :is(.foo) { color: red }
  > :is(:is(.bar)) { color: blue }
  > EOF
  $ cascade --minify is-unwrap.css
  .foo{color:red}.bar{color:#00f}

[:is()] with all-invalid forgiving-parse arguments matches nothing -
the rule is dead and removed.

  $ cat > is-empty.css <<EOF
  > :is(:future-pseudo) { color: red }
  > .x { color: blue }
  > EOF
  $ cascade --minify is-empty.css
  .x{color:#00f}


# Value-level dead code (default elision)


Default keyword in a shorthand is dropped (Cascade L6 §3 - omitted
sub-properties take their initial values; the printer drops them
when the source spelled them out explicitly).

  $ cat > defaults.css <<EOF
  > .a { background: red 0% 0% }
  > .b { background-position: 50% 50% }
  > .c { border-radius: 0 0 0 0 }
  > EOF
  $ cascade --minify defaults.css
  .a{background:red}.b{background-position:50%}.c{border-radius:0}

Trailing zeros in numeric tokens are dropped per CSS Values L4 §8.

  $ cat > zeros.css <<EOF
  > .a { width: 10.0px }
  > .b { width: 10.50px }
  > .c { line-height: 1.0 }
  > EOF
  $ cascade --minify zeros.css
  .a{width:10px}.b{width:10.5px}.c{line-height:1}

Zero-length unit dropped where unambiguous.

  $ cat > zero-unit.css <<EOF
  > .a { width: 0px }
  > .b { width: 0em }
  > .c { width: 0vh }
  > EOF
  $ cascade --minify zero-unit.css
  .a,.b,.c{width:0}


# Cascading dead code (multi-pass)


A combination produces cascading elimination across all levels.

  $ cat > multi.css <<EOF
  > @media screen { }
  > .a { color: red; color: red }
  > .b { }
  > *.c { padding: 10px }
  > .d { color: red } .d { padding: 10px }
  > .e { color: blue } .f { color: blue }
  > @media screen { .g { color: red } } @media screen { .h { color: blue } }
  > EOF
  $ cascade --minify multi.css
  .a{color:red}.c{padding:10px}.d{color:red;padding:10px}.e,.f{color:#00f}@media screen{.g{color:red}.h{color:#00f}}


# What is NOT dead code


Cross-rule shadowing is NOT removed - the cascade may apply each rule
to a different element set.

  $ cat > cross.css <<EOF
  > .btn { color: red }
  > .btn:hover { color: blue }
  > .btn { padding: 10px }
  > EOF
  $ cascade --minify cross.css
  .btn{color:red}.btn:hover{color:#00f}.btn{padding:10px}

Different-value duplicates of the same property in the same rule are
preserved when the earlier value is a different format the cascade
might fall back on (legacy syntax pattern).

  $ cat > legacy.css <<EOF
  > .x { display: -webkit-box; display: flex }
  > EOF
  $ cascade --minify legacy.css
  .x{display:-webkit-box;display:flex}
