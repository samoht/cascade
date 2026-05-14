# color-mix() percentages below 100% produce semi-transparent result

`color-mix(in srgb, red 30%, blue 30%)` — the percentages sum to 60%, not 100%.
Per CSS Color 5, the mix is normalized to 50%/50% but the alpha of the result
is multiplied by `sum / 100 = 0.6`. The result is rgba(128, 0, 128, 0.6),
i.e. a semi-transparent purple. A minifier that resolves this to opaque `purple`
would be incorrect: the alpha multiplier would be lost.
