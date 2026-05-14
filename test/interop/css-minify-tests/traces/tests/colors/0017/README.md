# Modern hsl() with alpha to 8-digit hex

`hsl(120 100% 50% / 0.5)` resolves to fully saturated green at 50% opacity.
This can be converted to the shorter 8-digit hex `#00ff0080`. The alpha 0.5
maps to byte 0x80 (128) under standard 8-bit quantization.
