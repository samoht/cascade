# Invariants

These hold across the printer, the optimizer and the canonical comparison. A
change that breaks one is a defect even when every test passes. Each bullet
names the test or type that holds it, or says it is a convention only.

- **pp is a serializer.** `to_string`, pretty or minified, prints the node it
  is given and picks only among spellings that parse to the same node (`.5`,
  `#fff`, `:before`, whitespace). Anything that changes the node (`#f00` to
  `red`, `scale(x,x)` to `scale(x)`, `calc` folding, `0deg` to `0`) is an
  AST rewrite in the optimizer's normalize pass. The litmus is
  `parse(A) = parse(B)`, not whether the code is gated on minify. Dropping a
  `url()`'s quotes, `center center` to `center`, `flex:1 1 0%` to `flex:1`
  and a computed float to the eight decimals the printer writes are such
  rewrites too. The minified printer writes a `<string>` or a url from the
  string it names, through `Pp.quoted_string`, `Pp.url` or, for an image url
  held bare, `Pp.url_token` (CSSOM 1 sec. 2.1, serialize a string and
  serialize a URL), so it reads back as that string whatever it holds. The
  pretty printer does the same unless the node keeps the author's spelling
  (see "Authored spellings stop at normalize"). Held by `pp` "string printers
  round-trip", fuzz `pp` "string printers read back" and `test/fixpoint`
  "values".
- **Normalize reaches a normal form.** For a typed declaration `d` with
  `n = normalize d`, `normalize n = n`, and reading `n`'s print back,
  minified or pretty, and normalising gives `n` again. Two normal forms with
  one minified text are then one node with one `Declaration.hash`, which
  factoring buckets on, so the first `--minify` pass merges what a second
  would. `test/fixpoint` holds this over every typed declaration its corpora
  read ("normal form"), and its `spellings` corpus holds the second pass
  still.
- **Type-changing rewrites fire only at typed leaves.** The number/percentage
  swap and zero-unit stripping are normalize rewrites at an operand whose whole
  type is known: a bare value, or a math function with no `var()` in it. CSS
  Values 4 sec. 10.9 types a math function by its operands, and a `var()`
  supplies its type only at substitution. A math function holding one keeps
  every operand's spelling, and so does a `var()` fallback; lexical number
  formatting still applies there. The readers keep a percentage a
  percentage until then (`v4109_percentage_beside_var` in
  `test/test_stylesheet_calc.ml`). Zero-unit stripping also stops where the
  grammar reads a bare `0` as another type. CSS Flexbox 1 sec. 7.2 reads a
  lone number in `flex` as the grow factor, so normalize leaves `flex:0px` and
  never builds a lone `Zero` basis. That node has no spelling of its own, and
  pp prints it as `0px` (`properties_layout` "flex", `optimize_declarations`
  "target minify and enforce-spec split"). A property's range binds only a
  literal: CSS Values 4 sec. 10.12 clamps a math function past it at
  computed-value time, so a reader takes every operand and result of either
  sign, a comparison folded at read time stays a calculation, and a fold keeps
  the wrapper around a result the bare literal could not spell (`values_math`
  "spec math range every family").
- **A custom property is single-valued only by `@property` registration**,
  never by inference from a definition's value. A `calc()` identity that drops
  an operand (`x * 0`, `0 * x`) folds only over an operand with no `var()`, or
  over a `<number>` or `<integer>` registration, which substitutes one number
  (`v41011_zero_product_over_var` in `test/test_stylesheet_calc.ml`,
  `calc_zero_product_registered_number` in
  `test/test_optimize_boundaries.ml`). `x * 1` and `x / 1` keep `x` whatever it
  substitutes, but not the `calc()` or parentheses around it. `Css.Context` and
  `--inline-vars` keep those around an unresolved `var()`
  (`runtime_var_keeps_its_calc` in `test/test_context_eval.ml`), and the
  optimizer drops a nested one only around a single-valued registration.
- **Authored spellings stop at normalize.** A few parser nodes keep the text
  the author wrote beside the value: `Authored_hex` for a hex colour, the
  number text of a `Dimension`, and the quote and source text of a quoted
  `content` string. The pretty printer writes that text back (`0.90rem`,
  `#0000FF` and `"\66oo"` print as read), the minified printer writes the
  value, and normalize drops the text, so two spellings of one value are one
  normal form with one hash. Equality before normalize still tells them apart.
  Held by `test/test_declaration_identity.ml` ("hex spellings have one node",
  "number spellings have one node", "quoted content is one node") and
  `test/fixpoint` "normal form". `Css.of_string ~preserve_source:true` keeps
  the byte-exact input in a separate `source` snapshot, which optimising and
  the other transforms leave untouched. Byte-identical round trips are not a
  contract.
- **A pass returns its input physically when it changes nothing, and equal
  declarations built in one domain are one object.** `Declaration.v` and
  `Declaration.theme_guarded` intern each declaration in a per-domain weak
  table (`lib/declaration_core.ml`), so `equal_declaration` answers most pairs
  with `==`. Passes signal "no change" by returning their input
  (`map_preserve`, `x == y` checks). `List.memq` over declarations is a
  semantic test in `lib/rule_trace.ml`, `lib/rule_order.ml` and `lib/rule.ml`.
  It asks whether a pass kept a declaration equal to the one given, which
  holds only under both properties. Held by "optimize preserves physical
  identity on a fixed point" in `test/test_optimize.ml` and in
  `fuzz/fuzz_optimize.ml`, and by "equal_declaration agrees with compare" in
  `fuzz/fuzz_declaration.ml`.
- **A cost model weighs the bytes the run prints.** Factoring, its preflight
  and the rule graph weigh a declaration by the length of its minified print
  under the run's own `--lossless` and `--enforce-spec`, which choose some
  spellings (an oklch chroma's `76%` or `.304`, a family name's quotes).
- **The canonical diff is the optimizer plus a projection.** Each side runs
  `Css.optimize ~regroup:false`, judged for the default targets unless
  `--enforce-spec` (`?judge`). Then come `Css.flatten_nesting`, the optimizer
  again and `Css.canonicalize_rule_order`, and the two minified prints are
  compared byte for byte (`canonical_of_stylesheet` in
  `lib/diff/css_compare.ml`). The projection (`lib/rule_order.mli`) adds
  normalizations emission does not make:
  - `@media not all and (X)` against `not (X)`, and `min-X`/`max-X` against
    the range form (Media Queries 4 sec. 2.3 and 4.2), which a Level 3 parser
    rejects, so `--enforce-spec` emission keeps the long forms;
  - in a custom property's tokens, a quoted font family against its ident
    sequence where a bare generic family proves a font stack, a time
    (`500ms` against `.5s`) and, bar `--lossless`, an angle; the tokens are
    what the CSSOM reports;
  - `oklab(0% none none/.5)` against `oklab(0% 0 0/.5)` for a colour standing
    as a whole colour value outside `@keyframes`, `@starting-style` and a rule
    that transitions it (CSS Color 4 sec. 4.4 and 13.3); emission keeps
    `none`, which interpolates differently;
  - under `--lossless`, `color(srgb ...)` on whole bytes against its `rgb()`,
    hex or named spelling; `--lossless` emission keeps the authored function;
  - a run of `@property` rules sorted by name, keeping the last registration
    of each, and a top-level `@layer a,b;` name the following blocks already
    declare in that order (CSS Properties and Values API 1 sec. 2, CSS Cascade
    5 sec. 6.4.3); both are visible through the CSSOM, so emission keeps them;
  - a WHATWG Compatibility sec. 3.4.1 legacy alias read as its property,
    nested `@media` conditions sorted and deduplicated, and a keyframe's
    declarations sorted as a rule's are; emission keeps the authored structure
    the CSSOM lists, and `--enforce-spec` keeps an alias for an engine that
    reads only the prefix.

  Each equates two values the specs make equal, and emission declines each for
  the reason its item gives. Beyond the optimizer's own deduplication, the
  projection deletes only what no browser reads differently: an `@supports`
  guard that never holds (CSS Conditional 3 sec. 6.1), a superseded
  registration or layer name, and, only when judging, a prefix no target
  reads. The `Tree_diff` comparison key
  (`Css.declaration_value_for_equivalence`) may likewise equate values the
  optimizer must not fold on emission, provided their type is proven. A
  phantom canonical difference on equal content is a projection gap in
  `rule_order.ml` or `rule_trace.ml`, not a renderer bug. Held by
  `test/test_css_compare_structure.ml` (for one, "not all and (X) is not a
  difference against not (X)") and by `test/render` "canonical_agree" (a pair
  the projection calls identical paints one page in Chromium).
- **The canonical projection is confluent.** It reads a run of style rules as
  the trace of its declarations: an entry per declaration under its selector
  and conditions, two entries ordered only where swapping them side by side
  could change a computed value, and the form printed is a function of that
  partial order and the entries' text. Where the run's rules begin and end,
  which occurrences of a selector were folded and in which order, never reach
  it. A fact that frees a pair by reading a third entry (a slot a later write
  of the group rewrites, a later twin of one in the other's group) never rests
  on an entry the projection drops: an entry is dropped only when it is
  redundant in the run's own order and no such fact about another entry of its
  group is read off it, so the order of what is left is that order less the
  entry. A legacy name alias (WHATWG Compatibility Standard sec. 3.4.1) reads
  as its property before the trace, so `-webkit-transform` and `transform` are
  two writes of one property wherever they stand. The projection compares
  declarations by node and reads them in normal form: the optimizer hands it
  normal forms, and the declaration it builds for an alias is normalised
  before it is compared. Held by `fuzz/fuzz_rule_order.ml` ("independent rules
  commute", "adjacent occurrences fold", "independent moves converge").
- **Judge may delete only what every browser at or above the floors
  ignores.** A target version is a floor: that release and every later one.
  The canonical comparison judges for the default targets (`?judge`), and a
  deletion it makes on that ground holds for every browser at or above them,
  not only for the floor versions. A floor that implements a production
  answers for the releases after it, so a `@supports` guard every floor
  satisfies is unwrapped. A floor that lacks one says nothing about them, so
  a guard no floor satisfies stays unless the data shows the feature never
  ships. The web-features dataset never shows that. `Supports.implemented_by`
  answers the question; `equal_canonical_targets` in
  `test/test_css_compare_structure.ml` holds it (`field-sizing` and
  `anchor-name` guards separate, `not (display:grid)` does not).
- **A fallback is dead only when every browser at or above the floors that
  reads it reads the declaration after it.** An earlier declaration that a
  later one of the same selector covers is what a browser paints when it reads
  it and drops the later one at parse time. The minifier and the judge decide
  that through one function, `Declaration.fallback_for`, asked of the same
  floors (`Ctx.floors`) by in-rule deduplication
  (`Shorthand_dedup.deduplicate_step`), by same-selector shadowing across
  rules (`Cover.covered`, `Rule.drop_shadowed`) and by the projection's
  shadowing (`Rule_order`). So whatever the minifier deletes the judge also
  counts dead, which the release criterion assumes. The minifier keeps more:
  without a judge it also keeps the fallback before a CSS Color 4 colour or a
  runtime substitution. `floor_fallbacks_survive` in
  `test/test_optimize_declarations.ml` and `equal_canonical_targets` hold it.
- **Two selectors share a list only when every browser at or above the floors
  drops both or neither.** A browser drops a whole list over a selector it
  does not parse (Selectors 4 sec. 3.9). A selector a floor reads is read
  from then on; one a floor drops may be read from a later release, so
  `Merge.compatible` lists two such selectors only when every engine ships
  them in the same release, and one whose production no release ships lists
  only with selectors waiting on the same productions.
  `test_compatible_past_the_floors` in `test/test_merge.ml` holds it.
- **Rule rewrites that move source order reason about overlap.** Factoring a
  default value into a shared block keeps a rule's own declaration when an
  earlier overlapping rule writes a different value. Nesting synthesis is
  adjacent-only, within one cascade context, with an exact selector extension
  under a non-list parent, and only when it shortens the output. Byte savings
  never justify a rewrite that is not cascade-safe. Held by `test/render`
  "render_diff" (the optimized sheet paints the source's page in Chromium) and
  by the release criterion ([reference](reference.md#release-criteria)).
- **`--enforce-spec` never emits output every browser renders differently.**
  It drops browser-target facts; it does not license spec literalism
  (`flex:1 1 0` stays in both modes). Held by `optimize_declarations` "target
  minify and enforce-spec split".
- **`` `Stylesheet `` scope closes over the CSS text, not the runtime.**
  Rewrites needing writing mode, direction or runtime custom-property values
  stay forbidden in every scope. Convention only; `optimize_declarations`
  "lossless keeps logical physical order" pins the writing-mode case.
- **Speed on real stylesheets is a requirement.** A slow path on realistic
  input is a defect. Convention only: tests such as `optimize` "nested media
  merges take linear work" pin single paths, and `bench/corpus_sweep.sh`
  measures the SatCSS corpus by hand.

These hold for the source tree:

- **Sources are 7-bit ASCII.** `.ml`, `.mli`, `.mll`, `.mly`, `dune` and
  `dune-project` files carry no other byte. `scripts/check_ascii.sh` enforces
  it, from the pre-commit hook on the staged content and from CI's "ASCII
  source" job on every tracked file.
- **`lib/` holds no module-level mutable state** but the per-domain
  declaration table above. Configuration is threaded as arguments. Convention
  only; no lint checks it.
- **The optimizer matches the typed property GADT, never property-name
  strings.** A longhand that parses as `Unknown_property` is typed first.
  Convention only; no lint checks it.
