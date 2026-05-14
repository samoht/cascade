# Static relative color syntax resolves to plain color

`color(from red srgb r g b / 0.5)` passes all channels through from `red`
unchanged, so it resolves to `srgb(1 0 0 / 0.5)` which is `#ff000080`.
A minifier that can evaluate static relative color expressions should fold this.
