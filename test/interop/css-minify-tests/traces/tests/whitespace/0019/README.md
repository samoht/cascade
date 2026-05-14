# Space preservation around + inside clamp()

Spaces around `+` and `-` operators inside `clamp()` are required just like in
`calc()`. Stripping them silently breaks the declaration.
