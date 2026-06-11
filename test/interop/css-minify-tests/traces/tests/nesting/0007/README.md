# Redundant & removal

`& .bar` is equivalent to `.bar` in a nesting context because a nested selector without `&` implies a descendant relationship. The explicit `&` is redundant and can be removed.
