# Cascade -- CSS Library for OCaml

## What is this?

A standalone CSS generation and manipulation library extracted from the `tw`
(Tailwind CSS v4 in OCaml) project. It provides a typed CSS AST, parser,
pretty-printer, and optimizer with no Tailwind-specific code.

## Build & Test

```bash
dune build
dune test
```

## Project Structure

```
lib/           CSS library (public_name: cascade)
  css.ml       Main entry point: of_string, to_string, rule, etc.
  values.ml    CSS values: lengths, colors, angles, durations
  properties.ml CSS properties: typed property/value pairs
  selector.ml  CSS selectors: class, id, pseudo, combinators
  declaration.ml Declarations and custom properties
  stylesheet.ml Statements: rules, @media, @layer, @supports, etc.
  reader.ml    CSS parser
  pp.ml        CSS pretty-printer (minification support)
  optimize.ml  CSS optimizer (dedup, merge, combine)
  variables.ml CSS custom properties and @property
  media.ml     Media queries
  supports.ml  @supports conditions
  container.ml @container queries
  font_face.ml @font-face
  keyframe.ml  @keyframes
  tools/       CSS comparison tools
    css_compare.ml  Structural CSS diff
    tree_diff.ml    Tree-based diff algorithm
    string_diff.ml  String-level diff
test/          CSS parser and printer tests
bin/           CLI tools
  cascade.ml   CSS formatting tool
  cssdiff.ml   CSS structural diff tool
```

## TODO

- ~~Rename internal module from `Css` to `Cascade`~~ (done: `open Cascade` then use `Css.*`)
- Remove dune-build-info dependency (make version banner optional)
- Add CSS nesting support for round-trip parse/print
- Improve odoc documentation
