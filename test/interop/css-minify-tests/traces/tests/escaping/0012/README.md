# Hex escape in custom property name

`--\66 oo` encodes `f` as a hex escape in a custom property name, resolving to
`--foo`. The same escape appears in `var(--\66 oo)` and must resolve consistently.
