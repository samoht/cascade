# UNSAFE: padding-top before padding-block-start must not be removed

`padding-top` is physical and `padding-block-start` is logical. In
`horizontal-tb` they resolve to the same edge, but in vertical writing modes
`padding-block-start` maps to `padding-left` or `padding-right`. Removing
either declaration changes rendering in some writing modes.
