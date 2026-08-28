CLI: :dir() takes any single identifier.

CSS Selectors 4 sec. 7.1: "Values other than ltr and rtl are not invalid, but
do not match anything." `:dir(auto)` matches no element, so `:not(:dir(auto))`
matches every element, and its rule must survive. The colour is folded to its
shortest spelling on the way out, which is a separate pass.

  $ cat > dir.css <<CSS
  > .i:not(:dir(auto)) { color: lime }
  > .j { color: red }
  > CSS
  $ cascade fmt --minify dir.css
  .i:not(:dir(auto)){color:#0f0}.j{color:red}
