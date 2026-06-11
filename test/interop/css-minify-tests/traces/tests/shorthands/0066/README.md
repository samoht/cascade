# border-top before border is dead code

`border-top: 1px solid blue` followed by `border: 2px solid red` is dead code
because the `border` shorthand resets all sides including `border-top`.
