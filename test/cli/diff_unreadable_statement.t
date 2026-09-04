CLI: `cascade diff` does not call two files identical over a statement it
dropped whole.

A statement the reader refuses goes the way of a refused declaration or
rule: it reaches neither side of the comparison. Two warnings share the
shape `bad value ... (in property-value)` and mean opposite things. One
names a `@charset` the reader dropped, the other an `@font-face` cascade
kept and reported on. The verdict follows what the reader did with the
statement, not the shape of the warning it left behind.

An `@charset` argument the reader refuses costs the whole statement. These
two sheets differ only in that argument, so both sides lose it and the
comparison sees the same thing twice.

  $ cat > charset-a.css <<EOF
  > .b{color:red}
  > @charset "a";
  > EOF
  $ cat > charset-b.css <<EOF
  > .b { color: red }
  > @charset "b";
  > EOF
  $ cascade diff --diff=canonical charset-a.css charset-b.css
  charset-a.css and charset-b.css parse warning: <string>: bad value for @charset: @charset only recognises "UTF-8" at [14-27] (in property-value)
  
  Unreadable rules: charset-a.css 1, charset-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

An `@font-face` missing `src` reaches the AST whole, so its warning reports
on material the parse kept and the verdict stays proven.

  $ cat > face-a.css <<EOF
  > @font-face{font-family:x}
  > .b{color:red}
  > EOF
  $ cat > face-b.css <<EOF
  > @font-face{font-family:x}
  > .b { color: red }
  > EOF
  $ cascade diff --diff=canonical face-a.css face-b.css
  face-a.css and face-b.css parse warning: <string>: bad value for @font-face: missing font-family or src descriptor at [0-25] (in property-value)
  
  CSS files are identical

The same verdict when the descriptor differs between the two files.
`--diff=canonical` compares minified forms, and a `src`-less `@font-face`
names no font to load, so minification drops it from both sides.

  $ cat > face-c.css <<EOF
  > @font-face{font-family:y}
  > .b { color: red }
  > EOF
  $ cascade diff --diff=canonical face-a.css face-c.css
  face-a.css and face-c.css parse warning: <string>: bad value for @font-face: missing font-family or src descriptor at [0-25] (in property-value)
  
  CSS files are identical

`@tailwind base;` opens every Tailwind entry stylesheet. Cascade keeps the
at-rule and its prelude, so it costs the verdict nothing.

  $ cat > tw-a.css <<EOF
  > @tailwind base;
  > .b{color:red}
  > EOF
  $ cat > tw-b.css <<EOF
  > @tailwind base;
  > .b { color: red }
  > EOF
  $ cascade diff --diff=canonical tw-a.css tw-b.css
  tw-a.css and tw-b.css parse warning: <string>: unknown at-rule @tailwind at [0-15] (in at-rule)
  
  CSS files are identical

`--json` counts the dropped `@charset` among the rules, and the status
agrees with the document.

  $ cascade diff --json --diff=canonical charset-a.css charset-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"], d["unreadable_rules"])'
  False {'expected': 0, 'actual': 0} {'expected': 1, 'actual': 1}
  $ cascade diff --json --diff=canonical tw-a.css tw-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"], d["unreadable_rules"])'
  True {'expected': 0, 'actual': 0} {'expected': 0, 'actual': 0}
  $ cascade diff --json --diff=canonical charset-a.css charset-b.css > /dev/null
  [2]
