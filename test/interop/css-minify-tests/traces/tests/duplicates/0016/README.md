# UNSAFE: margin-left before margin-inline-start must not be removed

`margin-left` is physical and `margin-inline-start` is logical. In LTR
`margin-inline-start` maps to `margin-left`, but in RTL it maps to
`margin-right`. Both declarations interact via cascade order and writing mode,
so removing either changes rendering in some configurations.
