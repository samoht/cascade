# Flat and nested equivalent rules should dedup

`.foo .bar{...}` and `.foo { .bar{...} }` resolve to the same selector. When
the declarations are identical, the duplicate should be removed.
