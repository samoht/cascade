# @supports -- space after `and` combinator required

`@supports (X) and (Y)` can be minified to `@supports(X)and (Y)`. Spaces after
`and` are required to disambiguate between a Function token and an Ident, but
the space before `and` can be dropped, as `)and` & `) and` tokenise identically.
Additionally `(`s are not allowed in identifiers, so `@supports(` tokenises
identically to `@supports (`.
