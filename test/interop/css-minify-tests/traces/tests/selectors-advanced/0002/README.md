# :is() must not decompose with mixed specificity

`:is(#a, .b)` cannot be decomposed to `#a,.b` because `:is()` takes the highest
specificity of its arguments. Both `.b` and `#a` match with `(1,0,0)` inside
`:is()`, but decomposed `.b` would only have `(0,1,0)`.
