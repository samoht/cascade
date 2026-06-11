# Keep unit on zero time

`0s` must NOT be simplified to `0`. Unitless zero is invalid for `<time>` values.
Stripping the unit breaks transitions and animations.
