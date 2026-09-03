CLI: `cascade diff` does not call two files identical over a rule it could
not read.

A rule the reader refuses is dropped from both sides before the comparison,
exactly as a refused declaration is, and it takes everything it holds with
it. Reporting the pair as identical there claims an equivalence the tool
never checked, so `cascade diff` gives the same third verdict and exits 2.
The count is kept apart from the declaration count: a harness reads the two
to tell what its sheet lost.

A prelude the reader cannot close takes the whole rule. These two sheets
differ only inside one, so both sides drop it and the comparison sees the
same thing twice.

  $ cat > rule-a.css <<EOF
  > .b{color:red}
  > .a[[ {color:red}
  > EOF
  $ cat > rule-b.css <<EOF
  > .b{color:red}
  > .a[[ {color:blue}
  > EOF
  $ cascade diff --diff=canonical rule-a.css rule-b.css
  rule-a.css and rule-b.css parse warning: <string>: unterminated qualified-rule at [14-30] (in qualified-rule)
  .b{color:red}
  .a[[ {color:red}
  ^^^^^^^^^^^^^^^^
  
  Unreadable rules: rule-a.css 1, rule-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

The same dropped rule text on both sides reaches the same verdict. The
comparison has no more to say here than it had above.

  $ cat > same-a.css <<EOF
  > .b { color: red }
  > .a[[ {color:red}
  > EOF
  $ cat > same-b.css <<EOF
  > .b{color:red}
  > .a[[ {color:red}
  > EOF
  $ cascade diff --diff=canonical same-a.css same-b.css
  same-a.css and same-b.css parse warning: <string>: unterminated qualified-rule at [18-34] (in qualified-rule)
  .b { color: red }
  .a[[ {color:red}
  ^^^^^^^^^^^^^^^^
  
  Unreadable rules: same-a.css 1, same-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

An at-rule condition the grammar refuses loses more: the at-rule goes and
every rule nested inside it goes with it.

  $ cat > media-a.css <<EOF
  > .b{color:red}
  > @media (bogus^^^){.x{color:red}}
  > EOF
  $ cat > media-b.css <<EOF
  > .b{color:red}
  > @media (bogus^^^){.x{color:blue}}
  > EOF
  $ cascade diff --diff=canonical media-a.css media-b.css
  media-a.css and media-b.css parse warning: <string>: bad condition for @media: expected media-in-parens at [22-27] (in at-rule)
  .b{color:red}
  @media (bogus^^^){.x{color:red}}
          ^^^^^
  
  Unreadable rules: media-a.css 1, media-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

A prelude the parser reads as a declaration drops the rule that carried it,
and that is the same loss.

  $ cat > decl-a.css <<EOF
  > .b{color:red}
  > --foo:{color:red}
  > EOF
  $ cat > decl-b.css <<EOF
  > .b{color:red}
  > --foo:{color:blue}
  > EOF
  $ cascade diff --diff=canonical decl-a.css decl-b.css
  decl-a.css and decl-b.css parse warning: <string>: expected selector but found declaration at [14-31] (in qualified-rule)
  .b{color:red}
  --foo:{color:red}
  ^^^^^^^^^^^^^^^^^
  
  Unreadable rules: decl-a.css 1, decl-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

A difference the comparison did reach outranks one it could not: the report
names the difference and the status stays 1.

  $ cat > differ-b.css <<EOF
  > .b{color:lime}
  > .a[[ {color:blue}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=canonical rule-a.css differ-b.css
  CSS: 31 chars vs 33 chars (6.5% diff)
  Changes: 1 modified rule
  
  rule-a.css and differ-b.css parse warning: <string>: unterminated qualified-rule at [14-30] (in qualified-rule)
  .b{color:red}
  .a[[ {color:red}
  ^^^^^^^^^^^^^^^^
  
  --- rule-a.css
  +++ differ-b.css
  └─ .b
        * color: red -> #0f0
  
  [1]

An at-rule cascade does not recognise keeps its prelude and its block, so
both sides still hold the text and the verdict is proven. `@tailwind base;`
is the case that reaches cascade from every Tailwind entry stylesheet.

  $ cat > at-a.css <<EOF
  > @tailwind base; .b{color:red}
  > EOF
  $ cat > at-b.css <<EOF
  > @tailwind base;
  > .b { color: red }
  > EOF
  $ cascade diff --diff=canonical at-a.css at-b.css
  at-a.css and at-b.css parse warning: <string>: unknown at-rule @tailwind at [0-15] (in at-rule)
  
  CSS files are identical

A dropped declaration and a dropped rule are counted apart, so a harness
reading the report is told which of the two its sheet lost.

  $ cat > mix-a.css <<EOF
  > .b{grid-template-columns:calc(1 + 2);color:red}
  > .a[[ {color:red}
  > EOF
  $ cat > mix-b.css <<EOF
  > .b{grid-template-columns:calc(1 + 2);color:red}
  > .a[[ {color:blue}
  > EOF
  $ cascade diff --diff=canonical mix-a.css mix-b.css
  mix-a.css and mix-b.css parse warning: <string>: unterminated qualified-rule at [48-64] (in qualified-rule)
  template-columns:calc(1 + 2);color:red}
  .a[[ {color:red}
  ^^^^^^^^^^^^^^^^
  mix-a.css and mix-b.css parse warning: <string>: read_declaration/grid-template-columns: bad value for grid-template-columns: expected at least 1 items (got 0) at [25-36] (in component)
  .b{grid-template-columns:calc(1 + 2);color:red}
                           ^^^^^^^^^^^
  .a[[ {color:red}
  
  Unreadable declarations: mix-a.css 1, mix-b.css 1
  Unreadable rules: mix-a.css 1, mix-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

`--json` carries the rule count beside the declaration count, so a harness
asserts on either without reading the report.

  $ cascade diff --json --diff=canonical mix-a.css mix-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"], d["unreadable_rules"])'
  False {'expected': 1, 'actual': 1} {'expected': 1, 'actual': 1}
  $ cascade diff --json --diff=canonical at-a.css at-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_declarations"], d["unreadable_rules"])'
  True {'expected': 0, 'actual': 0} {'expected': 0, 'actual': 0}

The document and the status agree.

  $ cascade diff --json --diff=canonical rule-a.css rule-b.css > /dev/null
  [2]
  $ cascade diff --json --diff=canonical at-a.css at-b.css > /dev/null
  $ cascade diff --json --diff=canonical rule-a.css differ-b.css > /dev/null
  [1]
