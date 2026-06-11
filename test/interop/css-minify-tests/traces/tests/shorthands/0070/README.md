# border-top-width before border-width is dead code

`border-top-width: 1px` followed by `border-width: 2px` is dead code because
`border-width` resets all four physical border widths.
