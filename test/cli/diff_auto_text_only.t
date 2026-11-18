CLI: cascade diff - the default mode over two sheets that parse alike.

The default mode takes its verdict from the text and lets the tree explain
it. When the parsed stylesheets are equal there is nothing for the tree to
explain: the report says only the text differs, prints the string diff, and
exits 1 as for any difference.

A colour written two ways is one value. No rule is modified.

  $ cat > long.css <<EOF
  > .a{color:#ffffff;top:0}
  > EOF
  $ cat > short.css <<EOF
  > .a{color:#fff;top:0}
  > EOF
  $ NO_COLOR=1 cascade diff long.css short.css
  CSS: 24 chars vs 21 chars (12.5% diff)
  Changes: no structural change; the text differs
  
  Strings differ at line 1, column 14 (byte 13)
  
  --- long.css
  +++ short.css
  @@ line 1 @@
  -.a{color:#ffffff;top:0}
  +.a{color:#fff;top:0}
                ^
  
  [1]

A rule that writes a property twice, as a fallback does, keeps its order on
both sides, so the respelling is not a reorder either.

  $ cat > fallback-long.css <<EOF
  > .a{display:-webkit-box;display:flex;color:#ffffff}
  > EOF
  $ cat > fallback-short.css <<EOF
  > .a{display:-webkit-box;display:flex;color:#fff}
  > EOF
  $ NO_COLOR=1 cascade diff fallback-long.css fallback-short.css | sed -n 2p
  Changes: no structural change; the text differs

An attribute value quoted on one side and escaped on the other is the same
string.

  $ cat > quoted.css <<EOF
  > .aria-\[x\=a_b\]\:flex[aria-x="a b"]{display:flex}
  > EOF
  $ cat > escaped.css <<EOF
  > .aria-\[x\=a_b\]\:flex[aria-x=a\ b]{display:flex}
  > EOF
  $ NO_COLOR=1 cascade diff quoted.css escaped.css > out.txt; echo $?
  1
  $ sed -n 2p out.txt
  Changes: no structural change; the text differs
  $ cascade diff --diff=tree quoted.css escaped.css
  CSS files are identical

Both sides refusing the same declaration hide nothing from the comparison,
so the quoted family name still reads as a text-only difference.

  $ cat > family-quoted.css <<EOF
  > .d{display:compact}.f{font-family:"Helvetica Neue",Arial}
  > EOF
  $ cat > family-bare.css <<EOF
  > .d{display:compact}.f{font-family:Helvetica Neue,Arial}
  > EOF
  $ NO_COLOR=1 cascade diff family-quoted.css family-bare.css 2>&1 | grep -e '^Changes' -e 'warning'
  Changes: no structural change; the text differs
  family-quoted.css:1:12: warning: bad value for display: unknown display: compact (also in family-bare.css)

A declaration order the cascade can see is still a reorder, and a changed
value is still a modified rule.

  $ cat > order-a.css <<EOF
  > .a{margin:0;margin-top:5px}
  > EOF
  $ cat > order-b.css <<EOF
  > .a{margin-top:5px;margin:0}
  > EOF
  $ NO_COLOR=1 cascade diff order-a.css order-b.css | sed -n 2p
  Changes: 1 reordered rule
  $ cat > value.css <<EOF
  > .a{color:#eee;top:0}
  > EOF
  $ NO_COLOR=1 cascade diff long.css value.css | sed -n 2p
  Changes: 1 modified rule
