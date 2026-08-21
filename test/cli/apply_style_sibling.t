CLI: [cascade apply] and the <style> element as a sibling.

A <style> element is a sibling like any other, and Selectors 4 §15 lets
a rule reach across it: [.navbox + style + .portal-bar] is a rule on
Wikipedia today. Projecting a block takes away its CSS, not its place
in the tree, or a rule kept in the sheet stops matching in the output
page what it matches in the input one.

  $ cat > sibling.html <<EOF
  > <html><head><style>
  > @media (min-width: 1px) { .a + style + .b { color: red } }
  > </style></head><body>
  > <p class="a">a</p><style>.mid{padding:1px}</style><p class="b">b</p><p class="mid">m</p>
  > </body></html>
  > EOF

The middle block's rule projects onto the element it matches, so no CSS
is left in it, and the emptied element still separates the two
paragraphs the kept @media rule reaches across.

  $ cascade apply sibling.html 2> /dev/null
  <html><head><style></style><style>@media(min-width:1px){.a+style+.b{color:red}}</style></head><body>
  <p class="a">a</p><style></style><p class="b">b</p><p style="padding:1px" class="mid">m</p>
  
  </body></html>
