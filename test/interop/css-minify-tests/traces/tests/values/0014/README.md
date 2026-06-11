# Nested calc() flattening and reduction

`calc(calc(100% - 20px) + 10px)` should flatten the inner `calc()` and reduce compatible units: `-20px + 10px` = `-10px`, yielding `calc(100% - 10px)`.
