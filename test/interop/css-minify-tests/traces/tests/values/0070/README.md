# Custom property value can be minified with @property constraint

When `@property` declares `syntax: "<color>"`, the value is parsed as a color
at parse time. The serialization is well-defined, so `rgb(0 0 0)` can be safely
minified to `#000`.
