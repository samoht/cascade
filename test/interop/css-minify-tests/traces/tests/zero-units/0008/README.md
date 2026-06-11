# Keep unit on zero angle

`0deg` must NOT be simplified to `0` in the `rotate` property. Unlike legacy
transform functions (where browsers accept unitless zero for compatibility), the
individual transform properties reject unitless zero per spec. Stripping the
unit produces an invalid declaration.
