# Custom property values must not be color-minified

Custom property values may be read by JavaScript via `getComputedStyle`, where
the serialized form matters. Converting `rgb(0 0 0)` to `#000` inside a custom
property changes JS-visible serialization.
