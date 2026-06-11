# Flatten &:pseudo from empty parent

`a { &:hover { color: red } }` can be flattened to `a:hover{color:red}` when
the parent has no own declarations. The `&` resolves to the parent selector,
producing the compound `a:hover`.
