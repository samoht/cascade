# 0% in keyframes must keep the percent sign

In keyframe selectors, `0%` must not be stripped to bare `0`. A `0` without `%`
is not valid as a keyframe selector. The `%` unit must be preserved.
