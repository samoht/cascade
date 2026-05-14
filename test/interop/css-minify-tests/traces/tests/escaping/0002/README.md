# Escape sequence resolution in strings

`'\66 oo'` should resolve the escape to `"foo"`. Tests both escape resolution
within string values and normalization from single to double quotes.
