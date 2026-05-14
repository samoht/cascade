# ::view-transition-group must keep double colon

`::view-transition-group()` is a modern pseudo-element and must NOT be
shortened to a single colon. The `::` -> `:` shortening is only valid for
CSS2.1 pseudo-elements (`::before`, `::after`, etc.).
