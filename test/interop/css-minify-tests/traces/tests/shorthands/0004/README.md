# background:none to background:0 0

`background: none` and `background: 0 0` are equivalent. Both reset all
background longhands to initial values -- `0 0` is parsed as
`background-position` and all omitted longhands (including `background-image`)
reset to their initial values. `0 0` is one byte shorter.
