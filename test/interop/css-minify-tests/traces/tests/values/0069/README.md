# calc() zero with different unit must not be dropped

`calc(100px + 0%)` must not simplify to `100px`. Although `0%` is numerically
zero, it has a different unit type. The browser resolves `%` against the
containing block at computed value time, and future animations or transitions
could interpolate through non-zero percentage values.
