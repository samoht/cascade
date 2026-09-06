CLI: `cascade diff` does not call two files identical over a rule it could
not read.

A rule the reader refuses is dropped from both sides before the comparison,
exactly as a refused declaration is, and it takes everything it holds with
it. Reporting the pair as identical there claims an equivalence the tool
never checked, so `cascade diff` gives the same third verdict and exits 2.
The count is kept apart from the declaration count: a harness reads the two
to tell what its sheet lost.

Two sides that lost the same source text are the exception. There the
comparison did see the same thing twice, so the equality it found holds and
the status is 0. The counts still report what each side lost.

A prelude the reader cannot close takes the whole rule. These two sheets
differ inside one, so each side drops a different rule and neither loss is
accounted for by the other.

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

The same dropped rule text on both sides costs the verdict nothing. What the
two sheets still hold compares equal, and the rule each of them lost was the
same run of bytes, so the status is 0. The two sides carry it at different
offsets, which the text comparison does not care about.

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
  
  CSS files are identical

`--json` still counts the rule each side lost. The counts say what the parse
dropped; they no longer withhold the verdict on their own.

  $ cascade diff --json --diff=canonical same-a.css same-b.css | python3 -c 'import json,sys
  > d = json.load(sys.stdin)
  > print(d["identical"], d["unreadable_rules"])'
  True {'expected': 1, 'actual': 1}

A rule only one side dropped leaves the other side holding text the
comparison never saw, so that verdict stays unproven.

  $ cat > kept-a.css <<EOF
  > .b{color:red}
  > EOF
  $ cascade diff --diff=canonical kept-a.css same-b.css
  same-b.css parse warning: <string>: unterminated qualified-rule at [14-30] (in qualified-rule)
  .b{color:red}
  .a[[ {color:red}
  ^^^^^^^^^^^^^^^^
  
  Unreadable rules: kept-a.css 0, same-b.css 1
  Cannot determine whether the CSS files are identical
  [2]

A media condition the grammar refuses loses nothing: Media Queries 5 sec. 3.2
replaces it with `not all`, so both sides keep the block and the comparison
reaches the rules inside it. The condition is still reported as a warning, on
both sides, at the offset where it gave up.

  $ cat > media-a.css <<EOF
  > .b{color:red}
  > @media ^^^{.x{color:red}}
  > EOF
  $ cat > media-b.css <<EOF
  > .b{color:red}
  > @media ^^^{.x{color:blue}}
  > EOF
  $ cascade diff --diff=canonical media-a.css media-b.css
  CSS: 40 chars vs 41 chars (2.5% diff)
  Changes: 1 changed container
  
  media-a.css and media-b.css parse warning: <string>: bad condition for @media: expected media type or condition at [21-22] (in at-rule)
  .b{color:red}
  @media ^^^{.x{color:red}}
         ^
  
  --- media-a.css
  +++ media-b.css
  └─ @media not all (1 modified)
     └─ .x
           * color: red -> #00f
  
  [1]

An at-rule condition with no such rule behind it does lose the block, and
every rule nested inside it goes with it. The rules thrown away differ, and
the verdict follows those.

  $ cat > sup-a.css <<EOF
  > .b{color:red}
  > @supports ^^^{.x{color:red}}
  > EOF
  $ cat > sup-b.css <<EOF
  > .b{color:red}
  > @supports ^^^{.x{color:blue}}
  > EOF
  $ cascade diff --diff=canonical sup-a.css sup-b.css
  sup-a.css and sup-b.css parse warning: <string>: bad condition for @supports: Expected supports feature at [24-25] (in at-rule)
  .b{color:red}
  @supports ^^^{.x{color:red}}
            ^
  
  Unreadable rules: sup-a.css 1, sup-b.css 1
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
  $ cascade diff --json --diff=canonical same-a.css same-b.css > /dev/null
  $ cascade diff --json --diff=canonical kept-a.css same-b.css > /dev/null
  [2]
  $ cascade diff --json --diff=canonical at-a.css at-b.css > /dev/null
  $ cascade diff --json --diff=canonical rule-a.css differ-b.css > /dev/null
  [1]
