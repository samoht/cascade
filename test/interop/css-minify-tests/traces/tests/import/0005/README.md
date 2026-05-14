# @import with layer() and supports()

`@import url("foo.css") layer(base) supports(foo: bar)` removes url()
wrapper, removes spaces between the closing `)` and next keyword/function, and
removes whitespace inside the supports() condition. Uses an unknown property
so minifiers cannot elide the supports() condition.
