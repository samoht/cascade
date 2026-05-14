# In-gamut display-p3 colour is minified to shortest sRGB form

`color(display-p3 0.5 0.5 0.5)` is a neutral grey that is within the sRGB
gamut. A minifier should convert it to the shortest sRGB representation
(the named colour `gray`) rather than keeping the longer display-p3 form.
