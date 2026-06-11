# border shorthand intentionally resets border-image

Declaring border-image then `border` resets border-image to none. The
border-image declaration is dead code and can be removed.
