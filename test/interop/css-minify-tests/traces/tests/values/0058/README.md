# Empty custom property declaration must not be discarded

`--foo: ;` is valid CSS that sets a custom property to an empty value (a single
space token). This is used to "reset" inherited custom properties. Discarding
this declaration as "empty" changes behavior because `var(--foo, red)` would
then use the fallback instead.
