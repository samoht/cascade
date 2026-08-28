CLI: `:dir()` takes a keyword, so its argument is ASCII case-insensitive.

CSS Values 4 sec. 4.1: "Keywords are identifiers and are interpreted ASCII
case-insensitively (i.e., [a-z] and [A-Z] are equivalent)." CSS Selectors 4
sec. 7.1 names `ltr` and `rtl` as the two directionalities `:dir()` matches, so
`:dir(LTR)` is that keyword written in another case and means `:dir(ltr)`.

The oracle for every case below is the all-lower-case spelling: case cannot
change what cascade emits.

  $ cat > lower.css <<EOF
  > .a:not(:dir(ltr)) { color: red }
  > EOF
  $ cascade fmt --minify lower.css
  .a:dir(rtl){color:red}

  $ cat > upper.css <<EOF
  > .a:not(:dir(LTR)) { color: red }
  > EOF
  $ cascade fmt --minify upper.css
  .a:dir(rtl){color:red}

  $ cat > mixed.css <<EOF
  > .a:not(:dir(Rtl)) { color: red }
  > EOF
  $ cascade fmt --minify mixed.css
  .a:dir(ltr){color:red}

A positive `:dir()` carries the same keyword.

  $ cat > plain.css <<EOF
  > .a:dir(RTL) { color: red }
  > EOF
  $ cascade fmt --minify plain.css
  .a:dir(rtl){color:red}

One keyword is one node, so two rules that differ only in its case are the same
rule and merge.

  $ cat > merge.css <<EOF
  > .a:dir(LTR) { color: red }
  > .b:dir(ltr) { color: red }
  > EOF
  $ cascade fmt --minify merge.css
  .a:dir(ltr),.b:dir(ltr){color:red}

  $ cat > same.css <<EOF
  > .a:dir(ltr) { color: red }
  > .a:dir(LTR) { color: red }
  > EOF
  $ cascade fmt --minify same.css
  .a:dir(ltr){color:red}

Sec. 7.1 leaves an identifier that is neither keyword valid but non-matching.
That is no keyword, so it keeps the case the author wrote and pairs with no
directionality.

  $ cat > other.css <<EOF
  > .a:not(:dir(Auto)) { color: red }
  > EOF
  $ cascade fmt --minify other.css
  .a:not(:dir(Auto)){color:red}

Case is a fact of the CSS text, so `--enforce-spec` reads the keyword the same
way. What it drops is the host document's partition of sec. 7.1, which is what
keeps the `:not()` here.

  $ cascade fmt --minify --enforce-spec upper.css
  .a:not(:dir(ltr)){color:red}
