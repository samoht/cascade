CLI: [cascade apply] and the :empty pseudo-class.

Selectors 4 §13.2 says :empty represents "an element that has no
children except, optionally, document white space characters", and
that "only element nodes and content nodes (such as [DOM] text nodes,
and entity references) whose data has a non-zero length must be
considered as affecting emptiness". Document white space characters
are spaces (U+0020), tabs (U+0009) and segment breaks (CSS Text 4
§4.3), so U+00A0 is not one of them.

  $ cat > empty.html <<EOF
  > <html><head><style>p:empty{color:red}</style></head><body>
  > <p id="void"></p>
  > <p id="spaces">  </p>
  > <p id="text">text</p>
  > <p id="child"><span></span></p>
  > <p id="nbsp">&nbsp;</p>
  > </body></html>
  > EOF

An element with a text child is not :empty, so it must not be painted;
one holding nothing but white space is. The entity reference resolves
to U+00A0, which is not document white space, so that paragraph is not
empty either.

  $ cascade apply empty.html
  <html><head></head><body>
  <p style="color:red" id="void"></p>
  <p style="color:red" id="spaces">  </p>
  <p id="text">text</p>
  <p id="child"><span></span></p>
  <p id="nbsp">&nbsp;</p>
  
  </body></html>
