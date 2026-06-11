# Out-of-gamut display-p3 color must not be clamped

`color(display-p3 1.2 -0.3 0.5)` has channels outside [0,1]. These are valid
per CSS Color 4 and represent colors outside the display-p3 gamut. A minifier
must not clamp these values or convert to sRGB, as that would destroy the
author's intent. Only numeric minification (leading zero removal) is safe.

Additionally `1.2 -0.3` value can have whitespace elided as the leading dash
is unambiguous: `1.2-0.3` is two number tokens.
