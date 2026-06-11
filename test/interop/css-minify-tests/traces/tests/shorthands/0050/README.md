# mask longhands before mask-border (safe ordering)

Mask longhands come before mask-border. Collapsing longhands to `mask` shorthand
is safe since mask-border already follows and overrides the implicit reset.
