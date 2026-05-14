# Padding longhand merge safe with @property constraint

When each custom property has an `@property` rule constraining its syntax to
`<length>`, the value is guaranteed valid at parse time. Invalid values are
rejected and fall back to `initial-value`, so shorthand merge is safe.
