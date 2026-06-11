CLI: --inline-imports - basic resolution.

Default --minify keeps @import references unchanged (open-world default).

  $ cat > base.css <<EOF
  > .base { color: red }
  > EOF
  $ cat > entry.css <<EOF
  > @import url("base.css");
  > .a { padding: 10px }
  > EOF
  $ cascade --minify entry.css
  @import"base.css";.a{padding:10px}

--inline-imports resolves the reference and merges the content in source
order.

  $ cascade --minify --inline-imports entry.css
  .base{color:red}.a{padding:10px}

Quoted-string @import (no url() wrapper) works equivalently.

  $ cat > entry-string.css <<EOF
  > @import "base.css";
  > .a { padding: 10px }
  > EOF
  $ cascade --minify --inline-imports entry-string.css
  .base{color:red}.a{padding:10px}

A transitive import (A imports B which imports C) is fully resolved.

  $ cat > c.css <<EOF
  > .c { color: green }
  > EOF
  $ cat > b.css <<EOF
  > @import url("c.css");
  > .b { color: blue }
  > EOF
  $ cat > a.css <<EOF
  > @import url("b.css");
  > .a { color: red }
  > EOF
  $ cascade --minify --inline-imports a.css
  .c{color:green}.b{color:#00f}.a{color:red}

Source order is preserved across imports - rules from an earlier @import
precede rules from a later @import or the entry's own rules.

  $ cat > first.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > second.css <<EOF
  > .x { color: blue }
  > EOF
  $ cat > order.css <<EOF
  > @import url("first.css");
  > @import url("second.css");
  > .e { color: green }
  > EOF
  $ cascade --minify --inline-imports order.css
  .x{color:#00f}.e{color:green}

Empty imported file leaves no trace.

  $ cat > empty.css <<EOF
  > /* nothing */
  > EOF
  $ cat > entry-empty.css <<EOF
  > @import url("empty.css");
  > .e { color: red }
  > EOF
  $ cascade --minify --inline-imports entry-empty.css
  .e{color:red}

Same file imported twice via different paths inlines twice (no automatic
file-identity dedup; cascade order resolves any conflicts).

  $ mkdir -p sub
  $ cat > sub/dep.css <<EOF
  > .d { color: red }
  > EOF
  $ cat > entry-dup.css <<EOF
  > @import url("sub/dep.css");
  > @import url("./sub/dep.css");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-dup.css
  .d{color:red}.e{padding:0}

A long chain of imports (depth ~ 9) flattens to a single sequence of
rules in deepest-first source order.

  $ for i in 1 2 3 4 5 6 7 8 9; do
  >   prev=$((i-1))
  >   if [ "$i" = "1" ]; then
  >     echo ".d1 { color: red }" > "depth-1.css"
  >   else
  >     printf '@import url("depth-%d.css");\n.d%d { padding: %dpx }\n' "$prev" "$i" "$i" > "depth-$i.css"
  >   fi
  > done
  $ cat > entry-chain.css <<EOF
  > @import url("depth-9.css");
  > EOF
  $ cascade --minify --inline-imports entry-chain.css
  .d1{color:red}.d2{padding:2px}.d3{padding:3px}.d4{padding:4px}.d5{padding:5px}.d6{padding:6px}.d7{padding:7px}.d8{padding:8px}.d9{padding:9px}
