# Space before !important removable

`color: red !important` can drop the space before `!`. The `!` delimiter token
is not an ident character so no ambiguity arises when adjacent to the value.
