# @supports not with unsupported property must be preserved

`hanging-punctuation` lacks universal support, so `@supports not` cannot be
statically resolved. The wrapper must remain even though the negation might
suggest removal.
