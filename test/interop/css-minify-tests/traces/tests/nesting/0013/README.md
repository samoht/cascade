# Flatten descendant nesting with empty parent

When a parent rule has no declarations of its own, the nested child can be
flattened into a single descendant selector. `a { b { color: red } }` becomes
`a b{color:red}`, eliminating the empty wrapper.
