# inset-block-start before inset-block is dead code

`inset-block-start: 10px` followed by `inset-block: 20px` is dead code because
`inset-block` resets both `inset-block-start` and `inset-block-end`.
