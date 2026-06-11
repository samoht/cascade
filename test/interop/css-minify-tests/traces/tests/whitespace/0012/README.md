# calc() spaces around \* and / are removable

Unlike `+` and `-`, the `*` and `/` operators in `calc()` do not require
surrounding spaces. They are delim tokens that cannot be confused with number
signs, so `calc(100%*2)` and `calc(10px/2)` are valid.
