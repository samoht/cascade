CLI: --minify merges two rules that declare the same NaN.

CSS Values 4 sec. 10.7.2 defines NaN as a keyword of the <number> grammar,
resolved at parse time, and sec. 10.13 serialises every NaN-valued calculation
as calc(NaN). One value, one spelling: two rules that declare it hold the same
declaration and merge, exactly as the ordinary-float pair below does.

  $ cat > nan.css <<CSS
  > .a { opacity: calc(infinity - infinity) }
  > .b { opacity: calc(infinity - infinity) }
  > .c { opacity: .5 }
  > .d { opacity: .5 }
  > CSS
  $ cascade --minify nan.css
  .a,.b{opacity:calc(NaN)}.c,.d{opacity:.5}
