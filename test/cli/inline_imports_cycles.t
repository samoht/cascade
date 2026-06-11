CLI: --inline-imports cycle detection.

A self-import (file imports itself) is broken at the cycle. The first
visit's content stays; the second visit drops the @import line.

  $ cat > self.css <<EOF
  > @import url("self.css");
  > .self { color: red }
  > EOF
  $ cascade --minify --inline-imports self.css 2>&1 | grep -v "warning"
  .self{color:red}

A two-cycle (A imports B which imports A) is detected and broken at
the second visit.

  $ cat > a.css <<EOF
  > @import url("b.css");
  > .a { color: red }
  > EOF
  $ cat > b.css <<EOF
  > @import url("a.css");
  > .b { color: blue }
  > EOF
  $ cascade --minify --inline-imports a.css 2>&1 | grep -v "warning"
  .b{color:#00f}.a{color:red}

A three-level cycle (A -> B -> C -> A) is detected and broken.

  $ cat > cyc-a.css <<EOF
  > @import url("cyc-b.css");
  > .a { color: red }
  > EOF
  $ cat > cyc-b.css <<EOF
  > @import url("cyc-c.css");
  > .b { color: blue }
  > EOF
  $ cat > cyc-c.css <<EOF
  > @import url("cyc-a.css");
  > .c { color: green }
  > EOF
  $ cascade --minify --inline-imports cyc-a.css 2>&1 | grep -v "warning"
  .c{color:green}.b{color:#00f}.a{color:red}
