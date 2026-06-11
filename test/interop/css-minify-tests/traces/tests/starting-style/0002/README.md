# @starting-style with discrete animation must not be removed

`@starting-style` with `display: none` and `opacity: 0` defines the entry
state for a discrete transition. A minifier must not remove this block even
though the values appear to match the element's hidden state -- without it the
entry animation does not play.
