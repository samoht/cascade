# Multiple consecutive hex escapes in class selector

`.\66\6f\6f` encodes each letter of `foo` as a separate hex escape. All three
can be resolved to plain ASCII, producing `.foo`.
