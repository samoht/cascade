CLI: a unitless zero in a media feature keeps the at-rule.

CSS Values 4 sec. 5 spells a zero `<length>` as the bare number `0`, and
Media Queries 4 sec. 1.3 takes its units from CSS Values. Chrome matches
`@media (min-width: 0)` against a live document, so the whole at-rule and
every rule inside it have to survive the parse.

  $ cat > zero.css <<EOF
  > .keep { color: blue }
  > @media (min-width: 0) { .a { color: red } }
  > EOF
  $ cascade fmt --minify zero.css
  .keep{color:#00f}@media(width>=0px){.a{color:red}}

The unit is the only difference: spelling the same query with `0px` has
always worked.

  $ cat > unit.css <<EOF
  > .keep { color: blue }
  > @media (min-width: 0px) { .a { color: red } }
  > EOF
  $ cascade fmt --minify unit.css
  .keep{color:#00f}@media(width>=0px){.a{color:red}}

A non-zero unitless number is not a length, but the enclosed condition
remains valid general-enclosed syntax and must retain its guard.

  $ cat > one.css <<EOF
  > .keep { color: blue }
  > @media (min-width: 1) { .a { color: red } }
  > EOF
  $ cascade fmt --minify one.css 2> /dev/null
  .keep{color:#00f}@media(min-width: 1){.a{color:red}}

The range and container spellings take the zero too.

  $ cat > range.css <<EOF
  > @media (width >= 0) { .a { color: red } }
  > @media (0 <= width) { .b { color: red } }
  > @container (min-width: 0) { .c { color: red } }
  > EOF
  $ cascade fmt --minify range.css
  @media(0px<=width){.a,.b{color:red}}@container(width>=0px){.c{color:red}}
