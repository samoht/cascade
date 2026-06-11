# Duplicate declaration removal with !important

When a property is declared with `!important` and then redeclared without it, the non-important declaration loses and should be removed. The `!important` declaration always wins regardless of order.
