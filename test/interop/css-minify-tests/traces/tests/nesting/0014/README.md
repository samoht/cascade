# Flatten child combinator nesting with empty parent

`a { > b { color: red } }` can be flattened to `a>b{color:red}` when the
parent has no own declarations. The relative child selector resolves to
a direct child combinator.
