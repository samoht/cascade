# font longhands before font-feature-settings (safe ordering)

Font longhands come before font-feature-settings. Collapsing longhands to `font`
shorthand is safe since font-feature-settings already follows and overrides the
implicit reset.
