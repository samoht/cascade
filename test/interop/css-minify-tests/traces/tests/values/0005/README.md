# Redundant positive sign removal

`+1.5px` should be normalized to `1.5px`. The explicit `+` sign on positive
numeric values is redundant outside of contexts that require it (e.g. `calc()`).
