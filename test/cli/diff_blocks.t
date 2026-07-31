CLI: cascade diff - conditional block structure.

Merging two same-condition blocks renumbers every block after them.
The blocks that only moved are reported as a shift run, so the one
real change stays legible instead of being repeated once per block on
each side.

  $ cat > blocks-split.css <<EOF
  > @media print{.a{color:red}}
  > .x{display:block}
  > @media print{.b{color:blue}}
  > .s1{order:1}
  > @media print{.t1{color:#0f0}}
  > .s2{order:2}
  > @media print{.t2{color:#0f0}}
  > .s3{order:3}
  > @media print{.t3{color:#0f0}}
  > EOF
  $ cat > blocks-merged.css <<EOF
  > @media print{.a{color:red}.b{color:blue}}
  > .x{display:block}
  > .s1{order:1}
  > @media print{.t1{color:#0f0}}
  > .s2{order:2}
  > @media print{.t2{color:#0f0}}
  > .s3{order:3}
  > @media print{.t3{color:#0f0}}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=tree --depth=max blocks-split.css blocks-merged.css
  CSS: 204 chars vs 189 chars (7.4% diff)
  Changes: 1 changed container
  
  --- blocks-split.css
  +++ blocks-merged.css
  └─ @media print (5 blocks merged into 4)
        - Block at position 0: .a
        - Block at position 2: .b
        + Block at position 0: .a, .b
        3 blocks shifted by -1
  
  [1]
