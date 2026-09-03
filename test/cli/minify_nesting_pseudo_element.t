CLI: nesting under a pseudo-element parent, and what --minify reads back.

CSS Selectors 4 sec. 3.6.5 (ED) makes a combinator after a pseudo-element
invalid, and no engine matches one. Nesting composes exactly that selector out
of a valid parent and a valid child, so merging a lone wrapper into its sole
nested rule drops the branches that follow the pseudo-element, as flattening
already does.

  $ cat > nested.css <<CSS
  > .a::before { .b { color: red } }
  > CSS
  $ cascade --minify nested.css
  $ cascade --minify --flatten-nesting nested.css

Whatever --minify writes, --minify reads back. Composing a selector the reader
refuses broke that round trip: the second pass dropped every rule.

  $ cascade --minify nested.css | cascade --minify -

A branch that leaves the pseudo-element last stays, and reads back.

  $ cat > branches.css <<CSS
  > .a::before { .b, .c & { color: red } }
  > CSS
  $ cascade --minify branches.css
  .c .a:before{color:red}
  $ cascade --minify branches.css | cascade --minify -
  .c .a:before{color:red}

The parent keeps its own declarations, and the dead nested rule goes with the
one the lone-wrapper merge already took: it matches nothing either way.

  $ cat > kept.css <<CSS
  > .a::before { color: red; .b { color: blue } }
  > CSS
  $ cascade --minify kept.css
  .a:before{color:red}
  $ cascade --minify --flatten-nesting kept.css
  .a:before{color:red}
  $ cascade --minify kept.css | cascade --minify -
  .a:before{color:red}

A conditional block keeps its parent's selector for what it wraps, so what it
wraps is as dead.

  $ cat > media.css <<CSS
  > .a::before { @media print { .b { color: blue } } }
  > CSS
  $ cascade --minify media.css
  $ cascade --minify --flatten-nesting media.css

The same at a remove: [&] names the pseudo-element parent, so a rule nested
under it follows the pseudo-element too.

  $ cat > deep.css <<CSS
  > .a::before { & { color: red; .b { color: blue } } }
  > CSS
  $ cascade --minify deep.css
  .a:before{color:red}
  $ cascade --minify --flatten-nesting deep.css
  .a:before{color:red}

A parent with no pseudo-element still merges, and reads back the same way.

  $ cat > plain.css <<CSS
  > .a { .b { color: red } }
  > CSS
  $ cascade --minify plain.css
  .a .b{color:red}
  $ cascade --minify plain.css | cascade --minify -
  .a .b{color:red}

  $ cat > plain_nested.css <<CSS
  > .a { color: red; .b { color: blue } }
  > CSS
  $ cascade --minify plain_nested.css
  .a{color:red;.b{color:#00f}}
