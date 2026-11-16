CLI: a selector the reader refuses is reported as a selector error.

The selector readers share the value readers' error helpers, which know
no property, so a bad selector used to read "bad value: ..." with
nothing for the value to belong to. A rule's prelude and a selector read
on its own say "bad selector:" instead, at the same place.

  $ cat > sel.css <<EOF
  > .a > { color: red }
  > a..b { color: red }
  > :nth-child(2n+) { color: red }
  > , .c { color: red }
  > .ok { color: red }
  > EOF
  $ cascade -v --minify sel.css 2>&1 | grep -e ': warning: ' -e '^\.'
  sel.css:1:6: warning: bad selector: expected at least one selector
  sel.css:2:3: warning: bad selector: expected identifier
  sel.css:3:15: warning: bad selector: expected signless integer
  sel.css:4:1: warning: bad selector: expected at least one selector
  .ok{color:red}

A value is still a value: the same helpers under a declaration name the
property.

  $ printf '.a { width: nope }' | cascade -v - 2>&1 | grep ': warning: '
  <stdin>:1:13: warning: bad value for width: no accepted form
