# Flatten subsequent-sibling combinator nesting with empty parent

`a { ~ b { color: red } }` can be flattened to `a~b{color:red}`. The relative
subsequent-sibling selector resolves to the general sibling combinator when
the parent has no own declarations.
