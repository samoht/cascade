# Flatten next-sibling combinator nesting with empty parent

`a { + b { color: red } }` can be flattened to `a+b{color:red}`. The relative
next-sibling selector resolves to the adjacent sibling combinator when the
parent wrapper has no declarations.
