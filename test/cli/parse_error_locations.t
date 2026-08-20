CLI: parse-error warnings carry a precise location, not Loc.dummy.

The `of_string` parsers in lib/{supports,container,media,...}.ml raise
typed `Cursor.Parse_error` anchored at the failing cursor position.
The stylesheet readers re-anchor those errors at the surrounding rule's
condition span so the `[start-end]` locator in the warning points at
the offending region of the source file, never `[0-0]`.

Invalid `@supports` condition: trailing junk after a valid feature
query lands the warning at the at-rule's prelude span.

  $ cat > bad-supports.css <<EOF
  > .ok { color: red }
  > @supports (display: grid) extra-junk { .a { color: blue } }
  > .also-ok { color: green }
  > EOF
  $ cascade --minify bad-supports.css 2>&1
  warning: bad-supports.css: bad condition for @supports: trailing content at [29-44] (in at-rule)
  .ok{color:red}.also-ok{color:green}

Invalid `@container` query: malformed `style()` reports at the query
slice, not at the start of the file.

  $ cat > bad-container.css <<EOF
  > .ok { color: red }
  > @container style() { .a { color: blue } }
  > EOF
  $ cascade --minify bad-container.css 2>&1
  warning: bad-container.css: bad value for : invalid: empty style() container query at [61-61] (in component)
  warning: ontainer style() { .a { color: blue } }
  warning: 
  warning:                                         ^
  .ok{color:red}

Invalid `@media` query inside `@import`: the warning points at the
import URL's span.

  $ cat > bad-media.css <<EOF
  > @import url("a.css") (bogus !!!);
  > .ok { color: red }
  > EOF
  $ cascade --minify bad-media.css 2>&1 | grep -E "warning|color" | head
  warning: bad-media.css: bad condition for @media: expected media-in-parens at [21-32] (in at-rule)
  .ok{color:red}
