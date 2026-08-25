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

The keyword and a calculation that lands on NaN are the same value, so they
merge too. Sec. 10.7.2 resolves NaN at parse time and sec. 10.13 gives it one
serialisation, which leaves the source spelling nothing to carry.

  $ cat > spellings.css <<CSS
  > .a { opacity: calc(NaN) }
  > .b { opacity: calc(infinity - infinity) }
  > CSS
  $ cascade --minify spellings.css
  .a,.b{opacity:calc(NaN)}

infinity and -infinity are constants of their own in that same list, each with
its own serialisation, so the three stay three values.

  $ cat > constants.css <<CSS
  > .a { opacity: calc(NaN) }
  > .b { opacity: calc(infinity) }
  > .c { opacity: calc(infinity) }
  > .d { opacity: calc(-infinity) }
  > CSS
  $ cascade --minify constants.css
  .a{opacity:calc(NaN)}.b,.c{opacity:calc(infinity)}.d{opacity:calc(-infinity)}
