# margin-top before margin is dead code

`margin-top: 10px` followed by `margin: 20px` is dead code because the
`margin` shorthand resets all four physical sides.
