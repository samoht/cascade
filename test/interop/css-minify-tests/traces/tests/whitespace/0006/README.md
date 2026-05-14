# Consecutive semicolon removal

`color: red;;` should collapse redundant semicolons. Multiple consecutive
semicolons are syntactically valid but wasteful; only one is needed between
declarations, and the final one before `}` can also be dropped.
