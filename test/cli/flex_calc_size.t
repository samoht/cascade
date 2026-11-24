A `calc-size()` basis in the `flex` shorthand. CSS Flexbox 1 (ED) sec.
7.1 gives `flex` the `<'flex-basis'>` grammar, and CSS Sizing 4 (ED)
sec. 7 puts `calc-size()` in it, with any sizing keyword the context
allows as its basis, `content` among them for `flex-basis`. Chromium
153 drops `flex:1 calc-size(auto,size)` and `flex:calc-size(content,size)`
and no shipping engine paints either, so the default mode refuses them
and keeps the `flex` before as the declaration every engine paints.
`--enforce-spec` relies only on the specifications, so it keeps both
shorthands; the `flex` before stays as the fallback it is beside every
value a browser may not read (`width:2px;width:calc-size(auto,size)`
keeps its first declaration in either mode).

  $ printf '.a{flex:2;flex:1 calc-size(auto,size)}.b{flex:calc-size(content,size)}' > in.css
  $ cascade fmt --minify in.css 2>/dev/null
  .a{flex:2}
  $ cascade fmt --minify --enforce-spec in.css 2>/dev/null
  .a{flex:2;flex:1 calc-size(auto,size)}.b{flex:calc-size(content,size)}
