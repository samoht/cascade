# UNSAFE: border-top before border-block must not be removed

`border-top` is physical and `border-block` is logical. In `horizontal-tb`
writing mode `border-block` maps to top/bottom, but in vertical writing modes
it maps to left/right. Removing `border-top` would change rendering in some
writing modes.
