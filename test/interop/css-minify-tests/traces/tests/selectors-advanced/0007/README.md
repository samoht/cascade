# Double :not() cancellation

`:not(:not(X))` is logically equivalent to `X`. The double negation can be
removed, producing a shorter selector.
