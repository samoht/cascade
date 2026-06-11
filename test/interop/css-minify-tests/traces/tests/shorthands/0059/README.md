# Border longhand merge unsafe even with var() fallback

A valid fallback does not make merging safe. The fallback only triggers when the
property is undefined. If the property is defined with an invalid value, the
fallback is ignored and the entire shorthand declaration fails at computed value
time.
