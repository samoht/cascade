# currentColor must not be resolved

`currentColor` is a runtime keyword that inherits the computed `color` value.
It cannot be replaced with a static color at build time because the inherited
value depends on the DOM context.
