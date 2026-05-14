# Unquoted url() to string form in @import

`@import url(foo.css)` shortens to `@import"foo.css"`. Even when the url()
value is already unquoted, the string form is shorter because it removes the
`url(` and `)` wrapper (5 chars) and adds only 2 quote chars.
