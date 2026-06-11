# Non-adjacent @media merge is unsafe due to cascade

Merging two identical `@media (width >= 1px)` blocks that have intervening
rules between them changes the cascade order. The intervening `a{color:green}`
must remain between the two @media blocks so it correctly overrides the first
block's `color:red`.
