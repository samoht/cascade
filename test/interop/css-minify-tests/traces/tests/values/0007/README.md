# URL whitespace trimming

`url(  image.png  )` should be trimmed to `url(image.png)`. Leading and
trailing whitespace inside unquoted `url()` tokens is insignificant.
