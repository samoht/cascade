# Hex escape in standard property name

`\63 olor` encodes the `c` in `color` as a hex escape. Minifiers should resolve
escape sequences in property names to their plain equivalents.
