# Border longhand merge safe with @property constraint

When each custom property has an `@property` rule constraining its syntax,
invalid values are rejected at parse time and fall back to `initial-value`.
The variable is guaranteed to produce a single valid component, so shorthand
merge is safe.
