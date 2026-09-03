CLI: cascade diff - the order container differences are printed in.

Container entries follow the expected side down its source, the same
coordinate rule differences already use, so a reader checks the report
against the sheet in one pass instead of against a hash table.

The preamble names byte counts and file names, which are not what these
cases are about, so each run is projected to the report body.

Conditions that hash into one order and are written in another. The
sheet says print, screen, 10px, 20px, speech.

  $ cat > many_ref.css <<'CSS'
  > @media print{.a{color:red}}
  > @media screen{.b{color:red}}
  > @media (min-width:10px){.c{color:red}}
  > @media (min-width:20px){.d{color:red}}
  > @media speech{.e{color:red}}
  > CSS
  $ cat > many_out.css <<'CSS'
  > @media print{.a{color:blue}}
  > @media screen{.b{color:blue}}
  > @media (min-width:10px){.c{color:blue}}
  > @media (min-width:20px){.d{color:blue}}
  > @media speech{.e{color:blue}}
  > CSS
  $ NO_COLOR=1 cascade diff --diff=tree --limit=none many_ref.css many_out.css | sed -n '6,$p'
  ├─ @media print (1 modified)
  │  └─ .a
  │        * color: red -> blue
  ├─ @media screen (1 modified)
  │  └─ .b
  │        * color: red -> blue
  ├─ @media (min-width: 10px) (1 modified)
  │  └─ .c
  │        * color: red -> blue
  ├─ @media (min-width: 20px) (1 modified)
  │  └─ .d
  │        * color: red -> blue
  └─ @media speech (1 modified)
     └─ .e
           * color: red -> blue
  

A stylesheet may write one condition several times, and two blocks under
the same condition can sit either side of another block. Each keeps the
place the sheet gives it, so the two `@media print` entries below are
not drawn together and the entries between them are not displaced.

  $ cat > twice_ref.css <<'CSS'
  > @media print{.a{color:red}}
  > @media screen{.b{color:red}}
  > @media speech{.c{color:red}}
  > @media print{.d{color:red}}
  > CSS
  $ cat > twice_out.css <<'CSS'
  > @media screen{.b{color:blue}}
  > @media speech{.c{color:blue}}
  > CSS
  $ NO_COLOR=1 cascade diff --diff=tree --limit=none twice_ref.css twice_out.css | sed -n '6,$p'
  ├─ @media print (removed)
  │  └─ .a (removed)
  ├─ @media screen (1 modified)
  │  └─ .b
  │        * color: red -> blue
  ├─ @media speech (1 modified)
  │  └─ .c
  │        * color: red -> blue
  └─ @media print (removed)
     └─ .d (removed)
  

A container only the actual side holds names no place in the expected
sheet, so it prints after every entry that names one, in the order the
actual side writes it: @media tv is written before @media all.

  $ cat > add_ref.css <<'CSS'
  > @media print{.a{color:red}}
  > @media screen{.b{color:red}}
  > CSS
  $ cat > add_out.css <<'CSS'
  > @media tv{.z{color:red}}
  > @media print{.a{color:blue}}
  > @media all{.y{color:red}}
  > @media screen{.b{color:blue}}
  > CSS
  $ NO_COLOR=1 cascade diff --diff=tree --limit=none add_ref.css add_out.css | sed -n '6,$p'
  ├─ @media print (1 modified)
  │  └─ .a
  │        * color: red -> blue
  ├─ @media screen (1 modified)
  │  └─ .b
  │        * color: red -> blue
  ├─ @media tv (added)
  │  └─ .z (added)
  └─ @media all (added)
     └─ .y (added)
  

A condition whose two sides hold a different number of blocks reports
that split or merge, and takes the same place as any other entry.

  $ cat > split_ref.css <<'CSS'
  > @media print{.a{color:red}}
  > @media print{.b{color:red}}
  > @media screen{.c{color:red}}
  > @media screen{.d{color:red}}
  > @media speech{.e{color:red}}
  > @media speech{.f{color:red}}
  > @media tv{.g{color:red}}
  > @media tv{.h{color:red}}
  > CSS
  $ cat > split_out.css <<'CSS'
  > @media print{.a{color:red}.b{color:red}}
  > @media screen{.c{color:red}.d{color:red}}
  > @media speech{.e{color:red}.f{color:red}}
  > @media tv{.g{color:red}.h{color:red}}
  > CSS
  $ NO_COLOR=1 cascade diff --diff=tree --limit=none split_ref.css split_out.css | sed -n '6,$p'
  ├─ @media print (2 blocks merged into 1)
  │     - Block at position 0: .a
  │     - Block at position 1: .b
  │     + Block at position 0: .a, .b
  ├─ @media screen (2 blocks merged into 1)
  │     - Block at position 2: .c
  │     - Block at position 3: .d
  │     + Block at position 1: .c, .d
  ├─ @media speech (2 blocks merged into 1)
  │     - Block at position 4: .e
  │     - Block at position 5: .f
  │     + Block at position 2: .e, .f
  └─ @media tv (2 blocks merged into 1)
        - Block at position 6: .g
        - Block at position 7: .h
        + Block at position 3: .g, .h
  
