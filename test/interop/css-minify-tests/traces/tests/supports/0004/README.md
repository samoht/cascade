# Elide @supports with multiple inner rules

`display: flex` is universally supported. The `@supports` wrapper can be
removed and all inner rules unwrapped to the parent context.
