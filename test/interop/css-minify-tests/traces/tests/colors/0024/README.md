# color-mix with none channel uses the other color's value

In interpolation contexts like `color-mix`, a `none` (missing) channel is
replaced by the corresponding channel from the other color before mixing.
`rgb(none 0 0)` mixed 50/50 with `rgb(200 0 0)` yields `rgb(200 0 0)` =
`#c80000` because the missing R channel adopts 200 from the other side.
