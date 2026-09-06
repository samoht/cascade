CLI: [cascade apply] and the :empty pseudo-class.

Selectors 4 §13.2 represents with :empty "an element that has no
children except, optionally, document white space characters", and its
own note records that Level 2 and Level 3 did not match an element
holding nothing but white space. No engine has taken that change, so
an answer here states what the specification says rather than what the
page does.

  $ cat > empty.html <<EOF
  > <html><head><style>p:empty{color:red}</style></head><body>
  > <p id="void"></p>
  > <p id="spaces">  </p>
  > <p id="text">text</p>
  > <p id="child"><span></span></p>
  > <p id="nbsp">&nbsp;</p>
  > </body></html>
  > EOF

Inlining takes the rule out of the stylesheet, so painting an element
on the strength of that answer would write a page that renders
differently from the one it was read from. The rule stays where the
browser reads it and no paragraph is painted here.

  $ cascade apply empty.html
  Kept 1 rule(s) with no inline form in a <style> block.
  <html><head><style></style><style>p:empty{color:red}</style></head><body>
  <p id="void"></p>
  <p id="spaces">  </p>
  <p id="text">text</p>
  <p id="child"><span></span></p>
  <p id="nbsp">&nbsp;</p>
  
  </body></html>

