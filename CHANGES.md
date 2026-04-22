## 1.1.0

### Fixed

- Accept custom property names starting with a digit after `--` in `var()`
  references (e.g. `var(--1A202C)`). Per CSS Syntax Level 3 §4.3.11, `--`
  itself is a valid ident-start, and any ident code point (including digits)
  can follow.

## 1.0.0

### Added

- Initial release of Cascade as a standalone CSS library
- Typed CSS AST with selectors, properties, values, declarations, and stylesheets
- CSS parser with error recovery (`Css.of_string`)
- CSS pretty-printer with minification support (`Css.to_string`)
- CSS optimizer: deduplication, rule merging, selector combining (`Css.optimize`)
- CSS custom properties with `@property` registration and typed syntax
- At-rules: `@media`, `@supports`, `@layer`, `@keyframes`, `@font-face`,
  `@container`, `@property`, `@starting-style`
- Modern color spaces: oklch, oklab, lch, hwb, color-mix, system colors
- Logical properties (inline/block variants)
- `cascade` CLI tool for formatting, minifying, and optimizing CSS
- `cssdiff` CLI tool for structural CSS comparison
- `cascade.tools` sub-library for programmatic CSS diffing
