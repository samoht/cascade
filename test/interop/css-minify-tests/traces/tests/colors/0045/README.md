# Fractional sRGB channels round to integers

`rgb(127.6 64.4 191.5)` has fractional channel values that are meaningless in
8-bit sRGB. Browsers round to the nearest integer (128, 64, 192) before
display. A minifier should round-then-convert to hex `#8040c0`, not truncate
(which would give `#7f40bf`).
