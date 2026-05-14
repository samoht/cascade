# Quote normalization

`'hello world'` should be normalized to `"hello world"`. CSS allows both single
and double quotes for strings; normalizing to one form is valid. Uses `content`
to ensure quotes cannot be removed entirely.
