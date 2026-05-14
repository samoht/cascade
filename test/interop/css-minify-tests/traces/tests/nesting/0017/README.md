# Nested rules with mixed combinators stay nested

When a parent has no own declarations but multiple nested children, keeping the
nesting is shorter than flattening because the parent selector only appears once
instead of being repeated for each child rule.
