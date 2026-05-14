# :is() decomposition with equal-specificity selectors

`:is(a, b)` can be decomposed to `a,b` when all selectors are widely supported
and share the same specificity. This is safe because `:is()` takes the highest
specificity of its arguments, which equals each individual selector here.
