# Vendor-prefixed duplicate can be dropped when targeting modern browsers

With a modern browserslist target, `-webkit-transform` is unnecessary because
all target browsers support unprefixed `transform`. The prefix is dead code.
