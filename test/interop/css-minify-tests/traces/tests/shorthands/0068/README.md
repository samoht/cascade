# padding-inline-start before padding-inline is dead code

`padding-inline-start: 10px` followed by `padding-inline: 20px` is dead code
because `padding-inline` resets both `padding-inline-start` and
`padding-inline-end`.
