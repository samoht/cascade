# UNSAFE: inset-block-start before top must not be removed

`inset-block-start` is logical and `top` is physical. In `horizontal-tb` they
resolve to the same edge, but in vertical writing modes `inset-block-start`
maps to `left` or `right`. Removing either changes rendering in some writing
modes.
