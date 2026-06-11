# border longhands then border-image (safe ordering)

Border longhands come before border-image. Collapsing longhands to `border`
shorthand is safe since border-image already follows and overrides the reset.
