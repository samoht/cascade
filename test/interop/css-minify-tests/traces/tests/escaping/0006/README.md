# Digit-start identifier must keep escape

`.\31 23` is the class selector `.123`. The leading digit must remain escaped
because bare `123` is not a valid CSS identifier start. A minifier that resolves
this to `.123` would produce an invalid selector.
