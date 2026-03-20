## unreleased

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
