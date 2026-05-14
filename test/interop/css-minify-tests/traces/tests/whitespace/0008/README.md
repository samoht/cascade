# @media -- space around `and` keyword required

`@media screen and (color)` requires spaces around `and`. Without a space before
`and`, `screenand` becomes a single ident token (unknown media type). Without a
space after `and`, `and(` becomes a function token instead of the `and` keyword
followed by `(`, making the query invalid.
