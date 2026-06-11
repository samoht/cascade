# Convert max-width to range syntax in @container

`@container (max-width: 500px)` can be shortened to `@container(width<=500px)`
using range syntax. In addition the white space can be elided as an at-keyword
cannot contain `(`.
