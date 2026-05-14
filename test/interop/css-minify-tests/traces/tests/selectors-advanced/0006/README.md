# :not(:dir(ltr)) to :dir(rtl)

Every element has exactly one directionality -- `ltr` or `rtl`. The two values
are a complete partition, so `:not(:dir(ltr))` is equivalent to `:dir(rtl)`.
