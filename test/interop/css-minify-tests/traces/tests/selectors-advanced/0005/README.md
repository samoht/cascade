# input:not(:invalid) to input:valid

On form elements `:valid` and `:invalid` are a complete partition -- every input
is one or the other. So `input:not(:invalid)` can be replaced with the shorter
`input:valid`. This only holds when the type selector restricts to elements that
participate in constraint validation.
