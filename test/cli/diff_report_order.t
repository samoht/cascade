CLI: cascade diff - the order rule differences are printed in.

The entries follow the expected side down its source, so a reader compares
the report against the sheet in one pass and a truncated report names the
differences nearest the top rather than an arbitrary sample of them.

Each selector below holds two rules the other side swaps. That is a
cascade-visible move no order key distinguishes, so every selector in the
sheet carries a difference and nothing but the ordering is under test.

  $ cat > order_ref.css <<'CSS'
  > .a{margin:0;margin-top:1px}.a{margin-top:1px;margin:0}
  > .b{margin:0;margin-top:1px}.b{margin-top:1px;margin:0}
  > .c{margin:0;margin-top:1px}.c{margin-top:1px;margin:0}
  > .d{margin:0;margin-top:1px}.d{margin-top:1px;margin:0}
  > CSS
  $ cat > order_out.css <<'CSS'
  > .a{margin-top:1px;margin:0}.a{margin:0;margin-top:1px}
  > .b{margin-top:1px;margin:0}.b{margin:0;margin-top:1px}
  > .c{margin-top:1px;margin:0}.c{margin:0;margin-top:1px}
  > .d{margin-top:1px;margin:0}.d{margin:0;margin-top:1px}
  > CSS
  $ NO_COLOR=1 cascade diff --limit=none order_ref.css order_out.css
  CSS: 220 chars vs 220 chars (0.0% diff)
  Changes: 8 reordered rules
  
  --- order_ref.css
  +++ order_out.css
  Rules reordered (8 rules):
  ├─ .a
  │     * margin ↔ margin-top
  ├─ .a
  │     * margin-top ↔ margin
  ├─ .b
  │     * margin ↔ margin-top
  ├─ .b
  │     * margin-top ↔ margin
  ├─ .c
  │     * margin ↔ margin-top
  ├─ .c
  │     * margin-top ↔ margin
  ├─ .d
  │     * margin ↔ margin-top
  └─ .d
        * margin-top ↔ margin
  
  [1]

A bounded report is the head of that same order, not a sample of it.

  $ NO_COLOR=1 cascade diff --limit=2 order_ref.css order_out.css
  CSS: 220 chars vs 220 chars (0.0% diff)
  Changes: 8 reordered rules
  
  --- order_ref.css
  +++ order_out.css
  Rules reordered (2 rules):
  ├─ .a
  │     * margin ↔ margin-top
  ├─ .a
  │     * margin-top ↔ margin
  └─ ...6 more differences
  
  [1]

Renaming the selectors gives them different hashes and lands them in
different buckets. The report is unmoved: it reads down the sheet, and the
sheet says .zq, .h, .m3, .r.

  $ cat > wide_ref.css <<'CSS'
  > .zq{margin:0;margin-top:1px}.zq{margin-top:1px;margin:0}
  > .h{margin:0;margin-top:1px}.h{margin-top:1px;margin:0}
  > .m3{margin:0;margin-top:1px}.m3{margin-top:1px;margin:0}
  > .r{margin:0;margin-top:1px}.r{margin-top:1px;margin:0}
  > CSS
  $ cat > wide_out.css <<'CSS'
  > .zq{margin-top:1px;margin:0}.zq{margin:0;margin-top:1px}
  > .h{margin-top:1px;margin:0}.h{margin:0;margin-top:1px}
  > .m3{margin-top:1px;margin:0}.m3{margin:0;margin-top:1px}
  > .r{margin-top:1px;margin:0}.r{margin:0;margin-top:1px}
  > CSS
  $ NO_COLOR=1 cascade diff --limit=none wide_ref.css wide_out.css | grep -E '^(├|└)─'
  ├─ .zq
  ├─ .zq
  ├─ .h
  ├─ .h
  ├─ .m3
  ├─ .m3
  ├─ .r
  └─ .r
