# scale() percentage to number

`scale(50%)` is equivalent to `scale(.5)`. Per the CSS Transforms spec, scale
functions accept `<number>` or `<percentage>`, where 100% = 1. Converting
percentages to numbers is shorter and avoids the `%` character.
