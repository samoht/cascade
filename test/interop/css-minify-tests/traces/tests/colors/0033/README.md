# color-mix: elide default oklab interpolation space

Per CSS Color 5, `oklab` is the default interpolation method for `color-mix()`
when none is specified. `in oklab` can be elided, saving 9 bytes. Browsers
already support the omitted form.
