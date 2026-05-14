# clamp() with identical arguments resolves to single value

When min, preferred, and max are all the same known value, `clamp()` can be
replaced with that value.
