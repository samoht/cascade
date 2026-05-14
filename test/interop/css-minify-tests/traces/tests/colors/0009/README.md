# rgba() zero alpha to short hex

`rgba(0, 0, 0, 0)` is fully transparent. The 4-digit hex `#0000` is the shortest
equivalent representation (5 chars vs 11 for `transparent`). Note that 4-digit
hex (CSS Color Level 4) may not be emitted by all minifiers -- some produce
`transparent` instead, which is correct but not minimal.
