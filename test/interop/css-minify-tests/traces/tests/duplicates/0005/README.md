# Duplicate properties with env() fallback must be preserved

The plain `1rem` fallback and `env()` declaration are progressive enhancement.
Removing the "duplicate" breaks browsers that don't support `env()`.
