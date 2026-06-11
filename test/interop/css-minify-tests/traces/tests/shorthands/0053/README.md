# Padding longhand merge unsafe with var()

Merging padding-top/right/bottom/left into `padding` shorthand is unsafe when
values use `var()`. If any variable is undefined or empty, shorthand fallback
behavior differs from individual longhands.
