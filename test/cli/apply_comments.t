CLI: [cascade apply] and HTML comments.

A comment is content. A licence header has to reach the output page,
and React writes an empty comment between two adjacent text nodes on
purpose: it keeps them two nodes, and a browser measures and rounds
each run on its own, so merging them moves the element's width.

  $ cat > page.html <<EOF
  > <!-- Copyright 2026. -->
  > <html><head><style>p{color:red}</style></head><body>
  > <p>v<!-- -->4.3</p>
  > <!--[if IE]><p>legacy</p><![endif]-->
  > <p><!-- empty but for this --></p>
  > </body></html>
  > EOF

  $ cascade apply page.html 2> /dev/null
  <!-- Copyright 2026. --><html><head><style></style></head><body>
  <p style="color:red">v<!-- -->4.3</p>
  <!--[if IE]><p>legacy</p><![endif]-->
  <p style="color:red"><!-- empty but for this --></p>
  
  </body></html>
