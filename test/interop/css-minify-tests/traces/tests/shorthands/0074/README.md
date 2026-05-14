# Zero flex-basis can be omitted

`flex: 0 0 0px` shortens to `flex: 0 0`. When flex-grow and flex-shrink are
specified as numbers, the omitted flex-basis defaults to `0%`, which is
equivalent to `0px` (both resolve to zero length).
