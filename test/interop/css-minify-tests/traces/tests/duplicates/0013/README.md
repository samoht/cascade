# !important wins on custom properties

When a custom property has both a normal and `!important` declaration, the
`!important` one wins regardless of order. The normal declaration is dead code.
Note the space before `!important` is part of the syntax, not the value.
