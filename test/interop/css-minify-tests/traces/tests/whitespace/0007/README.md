# @media not -- space before media type required

`@media not print` requires a space between `not` and `print`. Without it,
`notprint` becomes a single ident token parsed as an unknown media type, changing
the query from "not print media" to "unknown media type" which never matches.
