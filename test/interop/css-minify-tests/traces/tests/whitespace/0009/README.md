# @supports not -- space before ( required

`@supports not (display: flex)` requires a space between `not` and `(`. The CSS
tokenizer treats `not(` as a function token, not the `not` keyword followed by
`(`. This changes the grammar from a negation condition to an invalid/unknown
function call, causing the condition to fail.
