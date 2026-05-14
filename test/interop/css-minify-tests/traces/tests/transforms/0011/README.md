# scale property percentage to number

The individual `scale` property accepts `<number>` or `<percentage>`, where
100% = 1. `scale: 200%` is equivalent to `scale: 2`. Converting to a number
saves 2 bytes (drops `%` and avoids the longer percentage representation).
