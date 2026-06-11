# font longhands with font-kerning longhand (collapse, preserve order)

Font longhands and font-kerning are both expressed as longhands. Minifier should
collapse the font longhands into `font` shorthand, keeping `font` before
`font-kerning` since `font` resets font-kerning to `auto`.
