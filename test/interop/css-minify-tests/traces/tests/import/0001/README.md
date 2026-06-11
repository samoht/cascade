# url() to string form in @import

`@import url("foo.css")` can be shortened to `@import"foo.css"`. The CSS spec
allows both forms and the string form is always shorter. No space needed between
`@import` and the string token.
