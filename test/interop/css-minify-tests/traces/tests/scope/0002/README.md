# @scope with proximity limit

`@scope (.card) to (.card .content)` uses a scope limit. Whitespace around
the parentheses can be removed but the space between `to` and `(` must be
preserved to avoid `to(` being tokenized as a function token.
