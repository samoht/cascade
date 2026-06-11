# Elide @supports for universally supported property

`display: grid` is supported in all current browsers. The `@supports` wrapper
always evaluates to true and can be removed, unwrapping the inner rules.
