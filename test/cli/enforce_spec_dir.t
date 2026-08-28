CLI: --enforce-spec keeps the author's :not(:dir()).

CSS Selectors 4 sec. 7.1 defers directionality to the document language, and
an element the language gives no directionality matches neither `:dir(ltr)`
nor `:dir(rtl)`. What makes the two a partition is the host document: HTML
gives every element, not just an HTML one, a directionality of either 'ltr' or
'rtl'. That is the fact `--minify` takes and `--enforce-spec` drops.

  $ cat > ltr.css <<EOF
  > .a:not(:dir(ltr)) { color: red }
  > EOF
  $ cascade fmt --minify ltr.css
  .a:dir(rtl){color:red}
  $ cascade fmt --minify --enforce-spec ltr.css
  .a:not(:dir(ltr)){color:red}

The other direction is the same fact.

  $ cat > rtl.css <<EOF
  > .a:not(:dir(rtl)) { color: red }
  > EOF
  $ cascade fmt --minify rtl.css
  .a:dir(ltr){color:red}
  $ cascade fmt --minify --enforce-spec rtl.css
  .a:not(:dir(rtl)){color:red}

The author's own `:dir()` is not a rewrite either way.

  $ cat > plain.css <<EOF
  > .a:dir(rtl) { color: red }
  > EOF
  $ cascade fmt --minify plain.css
  .a:dir(rtl){color:red}
  $ cascade fmt --minify --enforce-spec plain.css
  .a:dir(rtl){color:red}
