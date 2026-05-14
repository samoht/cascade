# Modern rgb() with alpha to 8-digit hex

`rgb(143 101 98 / 43%)` can be converted to the shorter 8-digit hex `#8f65626e`.
The alpha 43% maps to byte 0x6E (110). This is the standard 8-bit quantization
browsers apply internally, making the conversion safe.
