# @property descriptor preservation

All three descriptors (`syntax`, `inherits`, `initial-value`) are required and
must not be removed or altered. The `syntax` string quotes are required.
Changing `inherits` silently breaks inheritance behavior.
