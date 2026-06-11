# var() custom property preservation

`var(--main-color)` must be preserved as-is. Custom property references cannot
be resolved or optimized at minification time since their values are defined at
runtime.
