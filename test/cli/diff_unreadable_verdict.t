CLI: `cascade diff` does not call two files identical over a declaration it
could not read.

A declaration the reader refuses is dropped from both sides before the
comparison, so nothing it says can reach the verdict. Reporting the pair as
identical there claims an equivalence the tool never checked. `cascade diff`
has a third verdict for it and exits 2, so a harness that gates on the status
is told the answer is unknown rather than given the wrong one.

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
browser drops both copies, so the two files may well render alike - but the
run of source text each sheet lost is a different run, so neither loss
accounts for the other and the count reports both.

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
  
  Unreadable declarations: same-a.css 1, same-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

The same verdict when the two unreadable declarations carry different text.
The comparison never saw either one, so it has no more to say here than it
had above.

  $ cat > other-b.css <<EOF
  > .g{grid-template-columns:calc(9 + 9);color:red}
  > EOF
  $ cascade diff --diff=canonical same-a.css other-b.css
  same-a.css and other-b.css parse warning: <string>: read_declaration/grid-template-columns: bad value for grid-template-columns: expected at least 1 items (got 0) at [28-39] (in component)
  .g { grid-template-columns: calc(1 + 2); color: red }
                              ^^^^^^^^^^^
  
  Unreadable declarations: same-a.css 1, other-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

Both sides losing the same run of source text is the exception. There the
comparison did see the same thing twice, so the equality it found holds and
the status is 0. These two spell the unreadable declaration alike and differ
only where the comparison could read both sides.

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

A declaration the two sides spell differently is not the same loss, however
alike the two failures look. These two name a different property of the same
length, so the reader gives up at the same offset on both sides and the
message it prints is no evidence they lost the same declaration.

  $ cat > prop-a.css <<EOF
  > .g{grid-auto-flow:calc(1 + 2);color:red}
  > EOF
  $ cat > prop-b.css <<EOF
  > .g{grid-auto-rows:calc(1 + 2);color:#f00}
  > EOF
  $ cascade diff --diff=canonical prop-a.css prop-b.css > /dev/null
  [2]

A difference the comparison did reach outranks one it could not: the report
names the difference and the status stays 1.

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

A declaration only one side holds reaches the same verdict: the side that
kept it has nothing to compare against the side that dropped it.

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
  
  Unreadable declarations: one-a.css 0, one-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

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
either side, and identical" without reading the report.

  $ cascade diff --json --diff=canonical same-a.css same-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"])'
  False {'expected': 1, 'actual': 1}
  $ cascade diff --json --diff=canonical plain-a.css plain-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"])'
  True {'expected': 0, 'actual': 0}

The document and the status agree: `--json` reports the same three verdicts.

  $ cascade diff --json --diff=canonical same-a.css same-b.css > /dev/null
  [2]
  $ cascade diff --json --diff=canonical plain-a.css plain-b.css > /dev/null
  $ cascade diff --json --diff=canonical text-a.css text-b.css > /dev/null
  $ cascade diff --json --diff=canonical same-a.css differ-b.css > /dev/null
  [1]

`--help` documents the status.

  $ cascade diff --help=plain | sed -n '/^EXIT STATUS/,/^ENVIRONMENT/p' | grep -c '^       2   '
  1
