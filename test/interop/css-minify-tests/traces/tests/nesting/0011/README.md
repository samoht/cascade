# Introduce nesting for pseudo-class

`a { ... } a:hover { ... }` is shorter when nested as `a{...;&:hover{...}}`, eliminating the repeated `a` selector.
