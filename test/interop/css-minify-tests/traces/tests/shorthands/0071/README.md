# border-top-color before border-top is dead code

`border-top-color: blue` followed by `border-top: 2px solid red` is dead code
because `border-top` resets `border-top-color`, `border-top-style`, and
`border-top-width`.
