# Convert min-width to range syntax in @container

`@container (min-width: 700px)` can be shortened to `@container(width>=700px)`
using range syntax. Container queries support the same range forms as media
queries. In addition the white space can be elided as an at-keyword cannot
contain `(`.
