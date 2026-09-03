CLI: cascade diff - a prelude statement that changed is a difference.

A type selector matches in the namespace its sheet declares, so two sheets
naming different namespace URLs style different elements. The structural
mode must not call them identical, and the report must name the at-rule
that changed.

  $ cat > a.css <<EOF
  > @namespace url(http://a.example);
  > .x { color: red }
  > EOF
  $ cat > b.css <<EOF
  > @namespace url(http://b.example);
  > .x { color: red }
  > EOF
  $ NO_COLOR=1 cascade diff --diff=tree a.css b.css > report.txt
  [1]
  $ grep -q '@namespace' report.txt && echo named
  named
