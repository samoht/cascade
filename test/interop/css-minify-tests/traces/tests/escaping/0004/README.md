# Hex escape for hyphen-minus in element name

`\2d foo` encodes a hyphen-minus followed by `foo`, producing `-foo`. The hyphen
is valid mid-identifier so the escape can be resolved to a literal `-`.
