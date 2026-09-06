CLI: parse-error warnings carry a precise location, not Loc.dummy.

The `read` parsers in lib/{supports,container,media,...}.ml take a cursor
over the components the prelude was lexed into, so the typed
`Cursor.Parse_error` they raise is anchored on the slice of the condition
that failed. The `[start-end]` locator in the warning points at that slice
of the source file, never `[0-0]`, and the caret underlines it.

Invalid `@supports` condition: trailing junk after a valid feature
query lands the warning on the junk.

  $ cat > bad-supports.css <<EOF
  > .ok { color: red }
  > @supports (display: grid) extra-junk { .a { color: blue } }
  > .also-ok { color: green }
  > EOF
  $ cascade --minify bad-supports.css 2>&1
  warning: bad-supports.css: bad condition for @supports: trailing content at [45-55] (in at-rule)
  warning:  color: red }
  warning: @supports (display: grid) extra-junk { .a { color: blue } }
  warning:                           ^^^^^^^^^^
  warning: .also-ok { color
  .ok{color:red}.also-ok{color:green}

Invalid `@container` query: malformed `style()` reports at the query
slice, not at the start of the file.

  $ cat > bad-container.css <<EOF
  > .ok { color: red }
  > @container style() { .a { color: blue } }
  > EOF
  $ cascade --minify bad-container.css 2>&1
  warning: bad-container.css: bad condition for @container: empty style() container query at [30-37] (in at-rule)
  warning: .ok { color: red }
  warning: @container style() { .a { color: blue } }
  warning:            ^^^^^^^
  warning: 
  .ok{color:red}

Invalid `@media` query inside `@import`: the media query list of the
prelude is read the same way as an `@media` rule's.

  $ cat > bad-media.css <<EOF
  > @import url("a.css") !!!;
  > .ok { color: red }
  > EOF
  $ cascade --minify bad-media.css 2>&1 | grep -E "warning|color" | head
  warning: bad-media.css: bad condition for @media: expected media type or condition at [21-22] (in at-rule)
  warning: @import url("a.css") !!!;
  warning:                      ^
  warning: .ok { color: red }
  warning: 
  .ok{color:red}
