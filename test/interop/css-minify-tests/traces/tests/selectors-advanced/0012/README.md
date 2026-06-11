# :is() scoped :not(:required) to :optional

When `:is()` restricts to form-associated elements, `:required` and `:optional`
are a complete partition. `:is(textarea, input):not(:required)` can be replaced
with `:is(textarea, input):optional`.
