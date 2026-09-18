CLI: `cascade diff` calls two files identical over a declaration it could
not read, and withholds the verdict over a rule it could not.

A declaration the reader refuses is one the browser refuses: the reader is
held to the browser's accept set by `test/spec/browser/accept_set`. The
browser drops that declaration from whichever sheet holds it and paints the
same, so it separates nothing, and the pair compares by what remains. The
warning is kept as information, since the reader can lag the browser and the
warning says where to look. A rule the reader refuses is another matter:
nothing holds the rule reader to the browser, and the rule takes everything
it held with it, so `cascade diff` has a third verdict for it and exits 2
(see `diff_unreadable_rule.t`).

Both files read whole and compare equal. That verdict is proven, so it stays
`identical` at exit 0.

  $ cat > plain-a.css <<EOF
  > .g { color: red }
  > EOF
  $ cat > plain-b.css <<EOF
  > .g{color:#f00}
  > EOF
  $ cascade diff --diff=canonical plain-a.css plain-b.css
  CSS files are identical

The same unreadable declaration on both sides, spelled differently. Every
browser drops both copies, so the two files render alike, and the verdict
says so. The warning names the declaration each side lost.

  $ cat > same-a.css <<EOF
  > .g { grid-template-columns: calc(1 + 2); color: red }
  > EOF
  $ cat > same-b.css <<EOF
  > .g{grid-template-columns:calc(1 + 2);color:red}
  > EOF
  $ cascade diff --diff=canonical same-a.css same-b.css
  same-a.css and same-b.css parse warning: <string>: read_declaration/grid-template-columns: bad value for grid-template-columns: expected at least 1 items (got 0) at [28-39] (in component)
  .g { grid-template-columns: calc(1 + 2); color: red }
                              ^^^^^^^^^^^
  
  CSS files are identical

The same verdict when the two unreadable declarations carry different text:
the browser reads neither.

  $ cat > other-b.css <<EOF
  > .g{grid-template-columns:calc(9 + 9);color:red}
  > EOF
  $ cascade diff --diff=canonical same-a.css other-b.css
  same-a.css and other-b.css parse warning: <string>: read_declaration/grid-template-columns: bad value for grid-template-columns: expected at least 1 items (got 0) at [28-39] (in component)
  .g { grid-template-columns: calc(1 + 2); color: red }
                              ^^^^^^^^^^^
  
  CSS files are identical

And when the two spell the unreadable declaration alike and differ only where
the comparison could read both sides.

  $ cat > text-a.css <<EOF
  > .g{grid-template-columns:calc(1 + 2);color:red}
  > EOF
  $ cat > text-b.css <<EOF
  > .g{grid-template-columns:calc(1 + 2);color:#f00}
  > EOF
  $ cascade diff --diff=canonical text-a.css text-b.css
  text-a.css and text-b.css parse warning: <string>: read_declaration/grid-template-columns: bad value for grid-template-columns: expected at least 1 items (got 0) at [25-36] (in component)
  .g{grid-template-columns:calc(1 + 2);color:red}
                           ^^^^^^^^^^^
  
  CSS files are identical

`--json` still counts the declaration each side lost, and reports the pair
identical.

  $ cascade diff --json --diff=canonical text-a.css text-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"])'
  True {'expected': 1, 'actual': 1}

Two refused declarations that name different properties are two
declarations the browser drops, and the status is the same.

  $ cat > prop-a.css <<EOF
  > .g{grid-auto-flow:calc(1 + 2);color:red}
  > EOF
  $ cat > prop-b.css <<EOF
  > .g{grid-auto-rows:calc(1 + 2);color:#f00}
  > EOF
  $ cascade diff --diff=canonical prop-a.css prop-b.css > /dev/null

A difference the comparison did reach is a difference: the report names it
and the status is 1, warning or no warning.

  $ cat > differ-b.css <<EOF
  > .g{grid-template-columns:calc(9 + 9);color:blue}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=canonical same-a.css differ-b.css
  CSS: 54 chars vs 49 chars (9.3% diff)
  Changes: 1 modified rule
  
  same-a.css and differ-b.css parse warning: <string>: read_declaration/grid-template-columns: bad value for grid-template-columns: expected at least 1 items (got 0) at [28-39] (in component)
  .g { grid-template-columns: calc(1 + 2); color: red }
                              ^^^^^^^^^^^
  
  --- same-a.css
  +++ differ-b.css
  └─ .g
        * color: red -> #00f
  
  [1]

A declaration only one side holds is the case Tailwind's compiled sheet
reaches against tw's: Tailwind writes `filter: blur(<value>)` for a docs
placeholder class and tw writes nothing. The browser drops the declaration,
the two paint the same, and the verdict is 0.

  $ cat > one-a.css <<EOF
  > .g{color:red}
  > EOF
  $ cat > one-b.css <<EOF
  > .g{grid-template-columns:calc(1 + 2);color:red}
  > EOF
  $ cascade diff --diff=canonical one-a.css one-b.css
  one-b.css parse warning: <string>: read_declaration/grid-template-columns: bad value for grid-template-columns: expected at least 1 items (got 0) at [25-36] (in component)
  .g{grid-template-columns:calc(1 + 2);color:red}
                           ^^^^^^^^^^^
  
  CSS files are identical
  $ cat > blur-a.css <<EOF
  > .a{filter:blur(<value>)}
  > EOF
  $ cat > blur-b.css <<EOF
  > EOF
  $ cascade diff --diff=canonical blur-a.css blur-b.css
  blur-a.css parse warning: <string>: read_declaration/filter: bad value for filter: invalid filter value: expected filter function(s) at [10-23] (in component)
  .a{filter:blur(<value>)}
            ^^^^^^^^^^^^^
  
  CSS files are identical

The same pair with a real difference beside the refused declaration still
exits 1.

  $ cat > blur-c.css <<EOF
  > .a{filter:blur(<value>)}.b{color:red}
  > EOF
  $ cat > blur-d.css <<EOF
  > .b{color:blue}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=canonical blur-c.css blur-d.css
  CSS: 38 chars vs 15 chars (60.5% diff)
  Changes: 1 modified rule
  
  blur-c.css parse warning: <string>: read_declaration/filter: bad value for filter: invalid filter value: expected filter function(s) at [10-23] (in component)
  .a{filter:blur(<value>)}.b{color:red}
            ^^^^^^^^^^^^^
  
  --- blur-c.css
  +++ blur-d.css
  └─ .b
        * color: red -> #00f
  
  [1]

An unknown at-rule is not an unreadable declaration. Cascade keeps the rule
and its block, both sides hold the same text, and the verdict is proven.

  $ cat > at-a.css <<EOF
  > @tailwind base; .g{color:red}
  > EOF
  $ cat > at-b.css <<EOF
  > @tailwind base;
  > .g { color: red }
  > EOF
  $ cascade diff --diff=canonical at-a.css at-b.css
  at-a.css and at-b.css parse warning: <string>: unknown at-rule @tailwind at [0-15] (in at-rule)
  
  CSS files are identical

`--json` carries the count per side, so a harness asserts "nothing unread on
either side" without reading the report, and reads `identical` for the
verdict.

  $ cascade diff --json --diff=canonical same-a.css same-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"])'
  True {'expected': 1, 'actual': 1}
  $ cascade diff --json --diff=canonical one-a.css one-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"])'
  True {'expected': 0, 'actual': 1}
  $ cascade diff --json --diff=canonical plain-a.css plain-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"])'
  True {'expected': 0, 'actual': 0}

The document and the status agree: `--json` reports the same verdicts.

  $ cascade diff --json --diff=canonical same-a.css same-b.css > /dev/null
  $ cascade diff --json --diff=canonical plain-a.css plain-b.css > /dev/null
  $ cascade diff --json --diff=canonical text-a.css text-b.css > /dev/null
  $ cascade diff --json --diff=canonical one-a.css one-b.css > /dev/null
  $ cascade diff --json --diff=canonical same-a.css differ-b.css > /dev/null
  [1]

`--help` documents the status.

  $ cascade diff --help=plain | sed -n '/^EXIT STATUS/,/^ENVIRONMENT/p' | grep -c '^       2   '
  1
