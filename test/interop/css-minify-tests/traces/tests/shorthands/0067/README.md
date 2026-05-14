# border-block-start before border-block is dead code

`border-block-start: 1px solid blue` followed by `border-block: 2px solid red`
is dead code because `border-block` resets both `border-block-start` and
`border-block-end`.
