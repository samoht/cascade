# Comment in custom property value must be preserved

`--bar: a/**/b` tokenizes as `ident:a` `ident:b` with no whitespace between
them. Replacing the comment with a space produces `a b` which is a different
token sequence. This matters for `@container style()` queries matching against
custom property values.
