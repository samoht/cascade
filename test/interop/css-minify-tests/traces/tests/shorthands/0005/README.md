# Color shortening and space removal before hash

`border: 1px solid #000000` shortens the hex color to `#000` and removes the
space before `#` since `#` unambiguously starts a hash token -- no whitespace
is needed to separate it from the preceding keyword.
