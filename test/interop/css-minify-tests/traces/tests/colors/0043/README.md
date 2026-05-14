# color-mix() with percentages summing over 100% must normalize

`color-mix(in srgb, red 80%, blue 40%)` — the percentages sum to 120%.
Per CSS Color 5 percentage normalization, they scale to 66.67%/33.33%.
The result is rgb(170, 0, 85) = `#a05`. A minifier that treats the percentages
at face value without normalizing would produce an incorrect color.
