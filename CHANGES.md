## 1.3.0 (unreleased)

1.2.1 could minify a sheet into one that renders differently from its source,
and `cascade diff --diff=canonical` could not be relied on to tell a change in
rendering from a change in spelling. This release fixes both, and makes cascade
two to three times cheaper to run.

- **Minified output that renders as its source does.** `--minify` read
  `-webkit-linear-gradient(top,...)` as the unprefixed syntax reads it, so the
  gradient changed direction. It listed a rule over a vendor selector with
  other rules, and a browser that does not read that selector then dropped them
  all. It could drop a border side, move a rule past one that styles the same
  element, and print `p:not(:not(.a .b))` as another selector. Run on 1.2.1's
  `--minify` output, 1.3's canonical diff finds a difference from the source
  in 190 of the 504 sheets of the SatCSS corpus, most of them over a prefixed
  gradient. If you ship CSS minified by 1.2.1, upgrade and minify again.
- **A canonical diff you can gate a build on.** `cascade diff --diff=canonical
  style.css style.min.css` exits 0 for each of the 504 SatCSS sheets and for
  Tailwind's output against its own `--minify` output. With 1.2.1, 139 of the
  504 failed that check: 94 reported a difference and 45 exited 2. 1.2.1 also
  called two sheets identical when one had lost a rule inside `@media` to a
  parse error.
- **Speed.** `--minify` takes less than half the CPU of 1.2.1 over the SatCSS
  corpus, and about a third on Tailwind's output. `cascade diff
  --diff=canonical` of Tailwind's stylesheet against tw's takes less than a
  third of the CPU.
- **Stable output.** Minifying `--minify` output again returns it byte for
  byte, on all 504 SatCSS sheets and on Tailwind's output. With 1.2.1, 69 of
  those sheets and both Tailwind stylesheets shrank again. Output no longer
  depends on hash order, and numbers print the same on Windows as elsewhere.
- **Warnings you can read.** A warning reads `file:line:column: warning:
  message` over the source line and a caret. Repeats are counted under the
  first, so netflix.com's sheet prints about 40 lines where it printed nearly
  500, and `-q` silences them.
- **Smaller output.** Over the SatCSS corpus and Tailwind's output, `--minify`
  output is about 1.7% smaller raw than with 1.2.1, and about the same size
  compressed: half a percent smaller under gzip, a little less under brotli.
  One sheet can still come out a little larger compressed; [Which rules
  merge](#which-rules-merge) says why.

The price is under [Breaking](#breaking). On the command line, a script that
reads cascade's diagnostics needs a new pattern. In the library, parsers return
a `result` instead of raising, and two spellings of one selector are one value.
`Css.to_string ~minify:true` prints the sheet it is given, and `Css.minify`
does what it used to. Each removed function is listed with what to call
instead, and most migrations are a rename or a deleted call.

### Breaking

#### Command line

- **Warnings and errors have a new form.** They were hard to locate: a warning
  gave a byte range, a trace of the reader's own functions
  (`read_declaration/float:`) and an `(in component)` tag, and cascade's own
  messages took another form (`Error: ...`). Every diagnostic now reads
  `file:line:column: warning: message` or `file: error: message`, as a
  compiler's does, and a parse diagnostic is followed by the source line with a
  caret under the span. A warning from `cascade diff` names the input it is
  about, and a sheet parsed from a string without a name prints as `<CSS
  input>`. This affects scripts that match cascade's standard error. Match the
  new form, or read `cascade diff --json` (#1299, #1330, #1331, #1337).
- **`cascade diff` gives positions as members, counted from 1.** `--json` made
  a consumer parse the location out of each message. The text report counted
  lines and columns from 0 while warnings counted from 1, so a difference at
  the start of the second line read `line 1, col 0`. Each warning and error of
  the `--json` document now has `file`, `line` and `column` members, and its
  `message` is the reason alone. The report and the document count from 1, and
  the report's `@@` header names the line instead of a byte offset. This
  affects consumers of `--json` and of the text report. Read the location from
  the members, and stop adding 1 (#1331, #1334).
- **A missing input file exits 124.** `cascade fmt missing.css` exited 1, the
  status of a file that failed to parse, so a build could not tell the two
  apart. It now exits 124, as the other commands do. A script that tests for
  status 1 on a missing file tests for 124 (#1338).

#### OCaml library

cascade keeps no deprecated aliases, so a removed function fails to compile
where it is called. Each entry says what to write there.

- **Parsers return a `result`.** Parsers reported failure in three ways,
  `Error.Parse_error`, `Failure` and `Invalid_argument`, so no single handler
  caught a malformed input. `Selector.of_string`, `Container.of_string`,
  `Supports.of_string`, `Declaration.of_string`, `Declaration.with_value`,
  `Media.of_string_strict`, `Media.value_of_string`,
  `Keyframe.selector_of_string` and `Font_face.*_of_string` now return `(_,
  Error.t) result`, and `Aria.of_string` an option. Every caller is affected.
  Match on the result, and print an error with `Error.to_string`. For CSS
  written in the program, use `Selector.v`, `Container.v` or `Css.stylesheet`,
  which raise `Invalid_argument` (#1299, #1367).
- **`Css.of_string_exn` is gone.** It raised `Error.Parse_error`, where every
  other function for CSS written in the program raises `Invalid_argument`. Use
  `Css.stylesheet`, which takes the same arguments. A handler for
  `Error.Parse_error` around the call moves to `Css.of_string` and its `Error`
  case (#1299, #1367, #1391).
- **Misuse raises `Invalid_argument`.** `Declaration.custom_property`,
  `Supports.property`, `Supports.func` and `Supports.property_name` raised
  `Failure` on a bad argument. A `try ... with Failure _` around them still
  compiles and stops catching, so search for it and catch `Invalid_argument`
  instead (#1299).
- **`Css.to_string ~minify:true` prints the sheet it is given.** The minified
  printer also rewrote the sheet, so the printed text did not read back as the
  value printed. It folded `:nth-child(1)` and `:is(:link,:visited)`, dropped
  an `@charset` and an `@font-face` without `font-family` or `src`, and moved a
  registered `<color>` variable in a `box-shadow`. It also rounded a colour's
  alpha, dropped `0%` and `100%` from a gradient's end stops and folded the
  rows of `grid-template-areas`. This affects code that prints with
  `~minify:true` without optimising first: it compiles, and its output gets
  longer. Call `Css.minify`. It optimises and prints in one call, as `cascade
  --minify` does. To take one step alone, call `Selector.canonicalize`,
  `Stylesheet.drop_inert_at_rules` or `Stylesheet.normalize_shadow_color_vars`
  (#1350, #1351, #1354, #1367, #1370).
- **`Selector`: one value whatever the spelling.** `:before` and `::before`,
  `[x=a]` and `[x="a"]`, `odd` and `2n+1` were different values, so equality,
  hashing and deduplication missed them, and parsing a printed selector could
  give back another value. This affects code that builds or matches these
  constructors. `Before`, `After`, `First_letter` and `First_line` take no
  argument and `colon_form` is gone. `Exact_quoted (v, _)` and the other
  `*_quoted` attribute matches are `Exact v` and so on, with `v` decoded. `Odd`
  is `An_plus_b (2, 1)` and `Even` is `An_plus_b (2, 0)`. The printer picks the
  spelling. `Selector.t` also has `Webkit_full_screen`,
  `Webkit_full_screen_ancestor` and `Moz_full_screen` cases, which an
  exhaustive match needs (#1357, #1408).
- **`Error` has a shared source and three new kinds.** Each error located
  itself by counting lines from the start of its source, so printing the
  warnings of a sheet cost its size per warning. The errors of one parse now
  share one index, and the `source` field of `Error.t` is an `Error.Source.t`.
  This affects code that reads or builds that field, or passes `?source` to
  `Cursor.of_components` or `Stylesheet.read_stylesheet_of_rules`. Wrap the
  text with `Error.Source.v` and read it back with `Error.Source.text`;
  `Error.position` gives the line and column. `Error.kind` has three new cases
  that an exhaustive match needs. `Invalid_statement` is a statement the rules
  around it refuse, such as an `@import` after a style rule, which was a
  `Bad_value` for a property called `stylesheet`. `Untargeted_selector` is a
  selector no target browser reads, and `Too_deep` is input nested past the
  reader's bound. The eight `Error.fail_*` functions each spelled `Error.fail`
  on one constructor. Call `Error.fail` on the constructor's value, as in
  `Error.fail (Error.bad_value loc ~property ~reason)` (#1296, #1299, #1310,
  #1331, #1371, #1391, #1429).
- **`Reader` has no error type of its own.** `Reader.Parse_error`, its
  `Reader.parse_error` record, `Reader.pp_parse_error`, `Reader.with_filename`,
  the `err` functions (`err`, `err_eof`, `err_expected`,
  `err_expected_but_eof`, `err_invalid`) and the context stack (`push_context`,
  `pop_context`, `with_context`, `callstack`) are gone. They kept a second
  error type beside `Error.t` that no parser raised and no `of_string`
  returned. This affects code that writes its own readers on `Reader`. Build an
  `Error.t` and raise it with `Error.fail`: `Error.unterminated` for `err_eof`,
  `Error.missing_token` for `err_expected` and `err_expected_but_eof`,
  `Error.bad_value` for `err` and `err_invalid`. Catch `Error.Parse_error`,
  print with `Error.to_string`, name the source with `Error.with_filename` and
  label a sub-reader with `Error.with_context`. `Reader.skip` at the end of
  input raises `Invalid_argument`, since reading past the end is a bug in the
  caller. Test `Reader.is_done` first (#1299).
- **`Cursor.try_enum` and `Cursor.err_eof` are gone.** Each repeated another
  function. Use `Cursor.option (Cursor.enum label table)`, which also skips
  leading whitespace, and `Error.fail (Error.unterminated loc sort)` (#1371,
  #1391).
- **`Component.t` has a `Flat` case.** Deeply nested input overflowed the stack
  (see [Parsing](#parsing)). The contents of a block or function nested past
  `Parser.max_nesting_depth` (128) are now kept as the tokens read, not as a
  tree. An exhaustive match over `Component.t` needs a `Flat` case. Code that
  only looks for a `Block` or a `Func` can ignore it (#1296).
- **A custom property's tokens carry no source position.** Two declarations of
  one custom property with one value compared and hashed differently depending
  on where they were read, so a table keyed on declarations kept both. The
  positions were offsets into the value's own text and located nothing in the
  sheet. `Component.source_loc` now gives `Loc.dummy` for these tokens.
  `Component.as_values` drops the positions from a token list you build (#1353,
  #1386).
- **`Properties.property` has new cases.** `text-rendering` and
  `-webkit-touch-callout` were read as unknown, which kept rules apart for no
  reason (see [Comparing stylesheets](#comparing-stylesheets)), and thirteen
  `-webkit-` legacy aliases had no type (see [Smaller
  output](#smaller-output)). `Text_rendering`, `Webkit_touch_callout` and a
  constructor for each alias, `Webkit_flex` and `Webkit_transform_origin` among
  them, are new, and an alias carries its unprefixed property's value type. An
  exhaustive match over `property` needs the new cases, and code that found one
  of these among the unknown properties finds it typed (#1440, #1442, #1518).
- **`Properties.opacity` and `Properties.shape_image_threshold` keep a
  percentage.** Both read `50%` as the number `.5`, so a percentage beside a
  `var()` lost its type (see [Rendering fixes](#rendering-fixes)). They have an
  `Opacity_percentage` and a `Percentage` case, holding the percentage as
  written (`50.` for `50%`). An exhaustive match over either type needs the
  new case; `Css.optimize` turns it into the number wherever no `var()` shares
  the calculation (#1456).
- **`Css.white_space` holds the longhand pair.** `Collapse` and
  `Preserve_nowrap` are gone, and `Longhands (collapse, mode)` spells the pair.
  A match on `Collapse` moves to `Longhands (Some Collapse, None)`, and one on
  `Preserve_nowrap` to `Longhands (Some Preserve, Some No_wrap)` (#1539).
- **Value types have new cases.** `--minify` deleted values these types had no
  room for (see [Rendering fixes](#rendering-fixes)), and an exhaustive match
  over one of them needs the new cases. `Css.length` has `Rex`, `Rcap`, `Rch`,
  `Svi`, `Svb`, `Lvi`, `Lvb`, `Dvi` and `Dvb`. `system_color`, `cursor`,
  `display`, `overflow`, `text_align`, `unicode_bidi`, `writing_mode`,
  `image_rendering`, `appearance`, `dominant_baseline`, `content` and `length`
  have the prefixed, legacy and newer keywords. `Properties.inset` has
  `Trailing`, for an `inset` written after a body holding a `var()`: treat
  `Trailing body` as `Body body` with the keyword printed last (#1511, #1536,
  #1539).
- **`Stats` reports only what the optimizer did.** A factoring run records one
  row, so `Stats.iteration_stat`'s `local_iteration` and `active_passes` were
  always 1, and its `iteration` and `Stats.counters`'s `iterations` repeated
  the run number. `Stats.revert_fixpoint` and `factor_transfer_reverts` counted
  runs thrown away by a gate that no longer exists. All are gone,
  `changed_passes` is `changed : bool`, and `Stats.record_iteration` takes the
  remaining fields. `elapsed` is 0 unless the recorder was made with
  `~profile:true`, as the byte columns already were. This affects code that
  reads `Stats` records. Read the run number from `fixpoint`, test `changed`
  where you tested `changed_passes > 0`, and delete the reads of the removed
  fields (#1372, #1399).
- **Three `Optimize` functions are gone.**
  `Optimize.duplicate_buggy_properties` and
  `Optimize.apply_property_duplication` were documented as duplicating
  properties for buggy browsers, and returned their input unchanged. Delete the
  call; `Optimize.add_compatibility_prefixes` writes the WebKit fallbacks they
  were documented to add. `Optimize.edges_of_rule`, with its `edge` and
  `packed_property` types, was read by nothing in cascade. Build the pairs from
  `Selector_summary.of_selector` and `Stylesheet.declarations`, keyed by
  `Declaration.property_key` (#1345, #1367, #1371, #1391).
- **An `@import` rule's `url` holds the url, not its source text.** The field
  held the source spelling (`url("theme.css")`, `'theme.css'`) when the rule
  was read from a sheet and the decoded url on other paths, so a printer could
  not tell `@import '"x".css'` from a spelling and printed it unescaped. It now
  holds the decoded url on every path: `theme.css` for each of `url(theme.css)`,
  `url("theme.css")` and `"theme.css"`. Code that reads `url` gets the url
  directly. Code that builds an `import_rule` writes the url, `url =
  "theme.css"`; a record built with `url = "url(theme.css)"` now prints as
  `@import "url(theme.css)"`, an import of the file named `url(theme.css)`
  (#1460).
- **`Css.decode_import_url` is gone.** It recovered the url from the source
  spelling the `url` field held, which the field no longer holds, and applied to
  a decoded url it read `"url(a.css)"` as `a.css`. Replace
  `Css.decode_import_url ir.url` with `ir.url` (#1460).
- **A `@counter-style` symbol says which kind it is.** `Symbols` held a
  `string list` and `Prefix` and `Suffix` a `string`, so an image symbol was
  kept as the text it was written with and printed back as a string. They now
  hold `Stylesheet.counter_symbol` values: `String`, `Image` (a
  `Properties.background_image`) or `Ident`. Code that matches or builds these
  descriptors wraps each string in `String`, or in `Ident` for a name written
  without quotes (#1461).
- **`counter()` and `counters()` content items carry a counter style.**
  `Counter of string` and `Counters of string * string` had no room for the
  style CSS Lists 3 sec. 4.6 lets them name. They are now `Counter { name;
  style }` and `Counters { name; separator; style }`, the style a
  `list_style_type option`. A pattern or a constructor names the fields, with
  `style = None` for the default; `Css.content_counter` and
  `Css.content_counters` take an optional `?style` (#1462).
- **`content` has a `With_alt` case.** CSS Generated Content 3 sec. 1.1 lets
  `content` end with `/` and an alternative text, which the type had no room
  for. A match over `Properties.content` needs a `With_alt { content; alt }`
  arm; `alt` is the list of strings, `counter()`, `counters()` and `attr()`
  items after the slash (#1474).
- **`Values.math_fn` has `Min_n`, `Max_n` and `Clamp_n`.** A `min()`, `max()`
  or `clamp()` over a time, an angle or a percentage had no node, which is why
  the slots of those types refused it. A match over `math_fn` needs the three
  arms; each holds the `math_arg`s as written (#1475).
- **`Pp.quoted` is gone.** It wrote a string between double quotes without
  escaping it, so a quote or a backslash inside ended the string or started an
  escape. Use `Pp.quoted_string`, which escapes what CSSOM 1 sec. 2.1 escapes
  and takes an optional `~quote` (#1460).
- and number slots may hold a calculation.** `counter_item.value` is a `number
  option` (was `int option`), `font_feature_value`'s `Index` carries a
  `number` (was `int`), `font_variation_setting.value` is a `number` (was
  `float`), and `rotate_value`'s `Axis` carries three `number`s (were
  `float`s); match `Num n` for a literal. `Css.counter_item ?value` still
  takes an `int` (#1584).
- carries an `untyped_name` record** (`written`, `vendor`, `unprefixed`,
  `spells`, `own`) instead of a string: build one with `Properties.untyped
  name` and read the name from `written`. `Properties.vendor` gives a
  property's vendor prefix, and the optimizer reads these fields rather than
  the name (#1588).
- holds a typed value.** Its value is the new `Properties.shape_outside`
  (`Shape`, `Image` or `Var`) instead of a string, and `Css.shape_outside`
  takes one (#1591).
- loses its `property_name` field.** `revert-layer` already matches a rule by
  its typed `declaration`, so the name was a redundant copy; read it with
  `Declaration.property_name rule.declaration` (#1594).

### Minified output

#### Rendering fixes

Each entry here is a sheet that 1.2.1 minified into one a browser renders
differently. They are the reason to minify again.

- **Prefixed gradients changed direction.** `--minify` read
  `-webkit-linear-gradient(top,...)` as the unprefixed `to top`, and took an
  omitted direction for `bottom`, so it printed
  `-webkit-linear-gradient(0deg,...)` for both. A prefixed gradient's side
  keyword names the side it starts from (CSS Image Values 3, Working Draft of
  17 February 2011, sec. 5.1.1), and is now read that way. This is the
  difference 1.3 finds most often in 1.2.1's output of the SatCSS corpus
  (#1418).
- **Vendor selectors took other rules down with them.** A browser that cannot
  read one selector of a list drops the rule for the whole list (Selectors 4
  sec. 3.9). `--minify` merged `.a:-moz-focusring{color:red}` and
  `.b{color:red}` into one list, so Chromium lost `.b`'s colour, and merged
  `a::-moz-placeholder` with `a::-webkit-input-placeholder`, so Chromium
  dropped both. It also did the reverse, and split an author's list such as
  `input::file-selector-button,b::-moz-selection{background:red}`, so Chromium
  painted a button the source left alone. Which targets drop a selector is now
  read from MDN's compatibility data, Firefox's selector parser and browser
  probes. Two selectors share a list only when the same targets drop each, and
  an author's list stays as written (#1407, #1428, #1437).
- **Rules over vendor pseudo-classes were dropped or revived.** `--minify`
  dropped rules over `:-webkit-full-screen`, `:-moz-full-screen`,
  `:-webkit-any-link` or `:-moz-only-whitespace` as unknown, though a target
  browser reads each. Such a rule now stays as written. In the other direction
  it printed `input:-webkit-input-placeholder`, a rule every browser drops, as
  `input::-webkit-input-placeholder`, which they read, so the placeholder took
  a colour the source never gave it. Only `:before`, `:after`, `:first-line`
  and `:first-letter` are pseudo-elements after one colon (Selectors 4 sec.
  3.6.1). A rule is now dropped only when no target reads its selector, with a
  warning that says so (#1408, #1427, #1449).
- **A rule could move past another that styles the same element.** A later
  `@media` or `@container` block merged into an earlier one across a list like
  `.a:after,.a:before` that styles the same pseudo-element. Two rules could
  also swap when they differ only in the case of a type selector (`DIV`,
  `div`), of an ID (quirks mode) or of an attribute value read through an `i`
  or `s` flag. Both orders are now kept (#1295).
- **A rule with nested content moved past one that styles the same
  element.** `--minify` read a rule as the declarations before its first
  nested statement, so what a nested rule, a nested `@media` or a later
  declaration wrote did not stop a move. `.a{top:1px}.d{color:red}
  .a{@media (min-width:1px){color:blue}}` became `.a{top:1px;@media
  (width>=1px){color:#00f}}.d{color:red}`, and an element with both classes
  turned red. A rule now counts everything its body writes, under each
  nested rule's composed selector, and a merge or move that would cross a
  rule competing with any of it stays in source order. Of 9,000 random
  sheets with nesting, the number Chromium renders differently after
  `--minify` falls from 28 to 17. The rest come from writing or removing
  nesting, which this fix does not touch (#1479).
- **A border side was lost.** When `--minify` moved declarations two rules
  share into one rule, it could drop a longhand a rule wrote after its own
  shorthand: `.b{background:#fff;border:1px solid #ddd}` beside
  `.a{background:#fff;border:1px solid #ccc;border-top:3px solid red}` became
  `.a,.b{background:#fff;border:1px solid#ddd}.a{border:1px solid#ccc}`, and
  `.a` lost its red top border. 1.2.1's output of ebay, google-1 and sina in
  the SatCSS corpus loses a border side this way. What a rule keeps now stays
  in the order it wrote it (#1404).
- **Selectors came out as other selectors.** `p:not(:not(.a .b))` was printed
  as `p.a .b`; a double negation holding a type or a combinator now prints as
  `:is(X)`. Attribute values were unquoted into selectors browsers drop
  (`[x="-2"]`, `[x="-"]`) or into another value (`[x="a\\b"]`). The printer now
  takes the shorter of an escaped ident and a string that reads back as
  written, so `[x="a b"]` prints as `[x=a\ b]` (#1292, #1350, #1357).
- **Values changed.** `animation:"none" 1s`, which runs the `@keyframes` named
  `"none"`, became `animation:1s`. `env(--branding-padding 2)` became
  `env(--branding-padding2)`, another variable. `width:4611686018427387904px`
  came out negative and `z-index:calc(1e30)` as `-1`. `@media
  (aspect-ratio:1.5/1)` was rounded to `1`, and `123456789.12345679` to
  `123456789.1234568`. A quote inside `local("...")` ended the string early.
  Each now prints a value that reads back as the one written (#1280, #1300,
  #1351, #1368, #1369).
- **A lone zero flex basis became a grow factor.** `flex:0px`, `flex:0em` and
  `flex:calc(0px)` became `flex:0`, with or without `--enforce-spec`.
  Chromium reads `flex:0` as grow 0 over a `0%` basis, where the source is grow
  1 over a `0px` basis. A lone unitless number in `flex` is the grow factor
  (CSS Flexbox 1 sec. 7.2), so a zero basis on its own now keeps its unit
  (#1459).
- **Strings and urls came out unescaped.** `quotes:'"' '"'` and
  `quotes:'""' ''` both printed `quotes:""""""`, which Chromium drops, and
  a backslash or a newline in a `quotes` string was written raw. The same held
  for `image-set()` strings and `type()`, `clip-path:url()`, `@font-face`
  `src` urls and `format()`, `@namespace` urls, and a custom property's
  `url()` holding a tab, which came out bare; `@import '"x".css'` printed
  `@import "x".css` unminified. A registered `<custom-ident>` lost its escapes,
  so `initial-value:a\ b` became `a b`, and Chromium dropped the `@property`
  rule. Every string, url and registered ident is now written escaped, so it
  reads back as the one written (#1460).
- **A `@counter-style` image symbol became text.**
  `@counter-style c{system:cyclic;symbols:url(a.png)}` minified to
  `symbols:"url(a.png)"`. Chromium supports no image symbol and drops the
  source's descriptor, so its list falls back to decimal, but it keeps the
  output's string and draws the text `url(a.png)` as the marker. A symbol is a
  string, an image or an ident (CSS Counter Styles 3 sec. 3.1.7), and each now
  prints as the kind it was written as (#1461).
- **`counter()` and `counters()` with a counter style were dropped.**
  `content:counter(n,upper-roman)`, `counter(n,c)`, `counter(n,disc)`,
  `counter(n,none)` and `counters(n,".",lower-alpha)` were refused as a bad
  value, so the generated content was lost, where Chromium draws each one. CSS
  Lists 3 sec. 4.6 gives both functions an optional `<counter-style>`, a
  counter-style name or `symbols()`; it is now read and printed, and the
  default `decimal` is left out (#1462).
- **A fallback a browser needs was dropped.** `--minify` turned
  `a{color:red;color:light-dark(red,blue)}` into
  `a{color:light-dark(red,#00f)}`, so Chrome 111 to 122 and Safari 16.4 to
  17.4, which drop `light-dark()`, lost the colour. It did the same before
  `contrast-color()`, `anchor()`, `anchor-size()`, `stretch` and `image-set()`,
  also when the two declarations sat in two rules of one selector, and before
  `width:fit-content(10px)`, which no browser reads, so Chromium 153 painted a
  different width. It also turned `a{width:anchor-size(width);width:stretch}`
  into `a{width:stretch}`, since Chrome 111 reads neither, though Chrome 125 to
  137, and Firefox from 147, read the first alone and lost the width. The
  targets are floors: each version and every later one. An earlier declaration
  is now dropped only when every release from the targets to the newest the
  compatibility data records that reads it reads the later one too, and
  canonical diff counts it dead on the same terms (#1464, #1486).
- **A `color-mix()` of one colour or of three was mishandled.** CSS Color 5
  mixes any number of colours, and Firefox 150 reads them. `--minify` read the
  first two of `color-mix(in srgb,red,blue,green)` and folded them to `purple`.
  It refused `color-mix(in srgb,blue)`, so `a{color:red;color:color-mix(in
  srgb,blue)}` became `a{color:red}`. Both mixes now stay as written, and the
  `color:red` before the second stays for the browsers that drop it, Chromium
  153 among them (#1465, #1483).
- **A reversed counter was deleted.** `--minify` refused
  `counter-reset:reversed(x)`, which CSS Lists 3 grants and Firefox reads, so
  it dropped the declaration, and after `counter-reset:x` it kept only the
  earlier write. It now reads `reversed()` in `counter-reset`, and keeps the
  earlier write as the fallback for Chrome and Safari, which drop it (#1466).
- **`break-before:always` became `page`.** `--minify` read `always` as
  `page`, though in a multicol container `always` breaks the column and `page`
  the page (CSS Fragmentation 3). Chrome drops `always`, so it broke a page
  the source never broke. Both values now print as written, the legacy
  `page-break-before:always` still maps to `page`, and an earlier
  `break-before:auto` stays as the fallback for browsers that drop `always`
  (#1467).
- **Two selectors a later browser reads apart shared a list.** `--minify`
  listed `.a:popover-open` and `.b:user-valid`, which Chrome 111 and Safari
  16.4 both drop, though Chrome 114 to 118 reads the first alone and Safari
  16.5 and 16.6 the second alone, and each dropped the rule. The targets are
  floors, so two selectors now list only when every release at or past them
  reads both or neither (#1468).
- **Math functions outside a property's range were dropped or broken.**
  `font-size:calc(-1px)`, `transition-duration:calc(-1s)`,
  `flex-basis:calc(-1%)` and `font-stretch:min(-1%,2%)` were dropped, and
  `scroll-padding-inline:calc(2px - 3px)`, `gap:calc(-1px)`,
  `width:calc(1% - 2%)`, `border-width:min(-1px,2px)` and
  `zoom:min(-1,2)` became a negative literal every browser drops. A math
  function past the range is valid and clamped (CSS Values 4 sec. 10.12), so
  each is now kept, folded where it can be, inside `calc()` (#1473).
- **`content` with an alternative text was dropped.** `content:"a" / "b"`,
  `content:url(a.png) / "b"` and `content:counter(n) / "b"` were refused as a
  bad value, so the generated content was lost where Chromium draws it. The
  alternative text after the slash (CSS Generated Content 3 sec. 1.1), with
  `attr()`, `counter()` and `counters()` in it, is now read and printed, and a
  value holding one keeps the `content` written before it for browsers that
  predate it (#1474).
- **Math functions over times, angles and percentages were dropped.**
  `transition-duration:min(1s,2s)`, `transition-delay:abs(-1s)`,
  `rotate:min(10deg,20deg)`, `opacity:min(10%,20%)`, `zoom:max(10%,20%)`,
  `border-image-slice:clamp(10%,20%,30%)` and the same calls in the
  transition and animation shorthands, `offset-rotate`, the SVG opacities,
  `shape-image-threshold` and `scale` were refused, so the declaration was
  lost. Every math function now reads wherever its type stands (CSS Values 4
  sec. 10.1) and folds to the literal of its value (#1475).
- **An `@import` after a `/*! */` comment was dropped.** So was one after an
  unrecognised at-rule, though browsers apply both. They are kept. An `@import`
  after an `@namespace`, and an `@namespace` after a rule, go, as browsers
  ignore them (#1294).
- **`--scope=stylesheet` dropped declarations a page needs.** A declaration
  written before longhands of its family was reset by the shorthand built from
  them: `background:#fff url(a.png) no-repeat center` lost its image and colour
  to a later `background-position` and `background-size`. It is now kept.
  Default output is unchanged (#1348).
- **A percentage beside a `var()` became a number.**
  `opacity:calc(100% - var(--x))` was minified to `calc(1 - var(--x))`, which
  a browser drops at computed-value time: with `--x:30%` Chromium shows the
  source at .7 opacity and the output at full opacity. The same held for the
  SVG opacities and `shape-image-threshold`, and `cascade diff
  --diff=canonical` called the two spellings identical. CSS Values 4 sec. 10.9
  types a calculation by its operands, so a percentage now becomes its number
  only where no `var()` shares the calculation (#1456).
- **A product with zero dropped a `var()`.** `width:calc(var(--u) * 0px)`
  was minified to `width:0`, though with `--u` undefined the source is invalid
  at computed-value time and Chromium lays the element out at `auto`. A
  `var()` substitutes tokens, so `calc(0 * var(--x))` under `--x:1px + 2px`
  is 2px, not 0, and `--y:calc(var(--x) * 0)` became `--y:0`. `cascade diff
  --diff=canonical`, `--inline-vars` with a kept variable and `Css.Context`
  runtime variables made the same fold. A zero product now drops an operand
  only when it holds no `var()`, or is an `@property` registered as
  `<number>` or `<integer>` (#1457).
- **A scroll-driven animation ended at the wrong point.**
  `animation-range:entry 10% normal` was minified to
  `animation-range:entry 10%`, which ends the animation at `entry 100%`
  rather than at `normal`: an omitted end takes the start's range name (CSS
  Scroll Animations 1 sec. 6.2). The end now goes only where it says what
  leaving it out says (#1470).
- **A zero blur or spread went before a `var()`.** `text-shadow:0px 5px 0px
  var(--c)` was minified to `text-shadow:0 5px var(--c)`, where `var(--c)` can
  be the blur: with `--c:3px` the source draws no shadow and the output a
  blurred one. `box-shadow:0 1px 0 0 var(--c)` lost its zero spread the same
  way, and drew a spread shadow. Each zero now stays before a `var()` colour,
  as a `box-shadow` blur's already did (#1471, #1472).
- **A margin longhand jumped a logical one.** `--minify` folded
  `margin:0;margin-block-start:5px;margin-top:1px` into
  `margin:1px 0 0;margin-block-start:5px`, which moves `margin-top` before the
  logical write of the same side, so Chromium sets the top margin to 5px
  instead of 1px. The same happened to `padding` and to
  `-webkit-margin-before` and `-webkit-padding-before`. A longhand now folds
  into the shorthand before it only past declarations that write none of its
  sides, and `cascade diff --diff=canonical` no longer prints such a rule in
  an order it would read differently the next time (#1478).
- **An `@property` registration was trusted where another can replace it.**
  `--minify` read a custom property as the type its `@property` gave it, so
  `@property --n{syntax:"<number>";...}` let `calc(var(--n) * 0px)` fold to
  `0` and `--c:rgb(255 0 0)` respell as `red`. A script
  (`CSS.registerProperty()`) or a later sheet registers the name again, and
  then the source and the output differ: Chromium computes 1px against 0px
  for the first under a later universal registration. A registration now
  types its property only under `--scope=stylesheet`, when it is the last for
  its name outside any conditional rule. Under `--enforce-spec`, an
  `@property` missing a descriptor is kept rather than dropped, since the
  current specification reads it as a registration (#1481).
- **`--scope=stylesheet` built shorthands that reset what another rule set.**
  `h1{font-size:20px;font-family:serif}` became `h1{font:20px serif}`, which
  resets the browser's bold heading weight (700 to 400 in Chromium), and an
  `em` lost its italic the same way. Next to `.y{font-weight:bold}` or
  `.y{background-repeat:no-repeat}`, `.x` lost the weight or the repeat on an
  element of both classes, and a `transition-delay` inside `@media` was reset
  by a `transition` built outside it. The scope rules out CSS outside the
  sheet. The sheet's other rules and the browser's own still apply, so a
  shorthand is now built from part of its longhands only when none of them
  sets the others (#1482).
- **Gradients over a `var()` changed.**
  `linear-gradient(to top,var(--a),var(--b))` was minified to
  `linear-gradient(var(--b),var(--a))`, which with `--a:red 20%` puts the red
  at the other end, since a variable can hold a position or several stops.
  `linear-gradient(red,var(--a) 100%)` lost its `100%`, which with
  `--a:red 10% 20%` turned a gradient Chromium drops into one it paints. And
  `radial-gradient(circle var(--r),red,blue)` became
  `radial-gradient(circle,var(--r),red,#00f)`, a gradient over three stops
  where the source sizes the circle. A stop whose colour is a `var()` is no
  longer reversed or stripped of a position. A radial prelude the reader
  cannot finish now keeps the whole value as written, since the comma after a
  prelude is omitted only with the prelude (CSS Values 4 sec. 2.6) (#1490).
- **Conic gradients placed by angles were deleted.** `conic-gradient(red
  0deg,blue 360deg)`, `conic-gradient(red 0,blue 1turn)`, the same after `from
  90deg at 0 0` and `conic-gradient(red calc(10deg + 5%),blue)` were refused as
  a bad value, so `--minify` dropped the declaration where Chromium paints the
  gradient. It kept `conic-gradient(red 10px,blue)`, which Chromium drops. A
  conic stop is now placed by one or two angles or percentages, `0` or a math
  function of both, never by a length, and a conic hint by one (CSS Images 4
  sec. 3.3.2 and 3.5.1). A first stop at `0deg` and a last at `360deg` lose the
  position they take by default, and an angle that is a whole percentage of the
  turn takes the shorter spelling: `90deg` prints as `25%` (#1491, #1534).
- **`mask` boxes moved, and layer longhands merged into the wrong layers.**
  One box in a `mask` layer sets both `mask-origin` and `mask-clip`, and two
  set them in order (CSS Masking 1 sec. 7.9), but `--minify` dropped each
  `border-box` on its own. So `mask:url(a.png) border-box content-box` became
  `mask:url(a.png)content-box`, which moves the origin, and `padding-box
  border-box` became `padding-box`, which moves the clip. `mask:url(a.png)
  no-clip` was refused and deleted. Mask longhands with two images and one
  `mask-repeat:no-repeat` became `mask:url(a.png),url(b.png)no-repeat`,
  which repeats the first image, and `background` did the same with one image
  and two repeats, or with a `var()` in a longhand, which can stand for
  another slot. Two boxes now print as one only when they are one box,
  `no-clip` is read, and longhands merge into a shorthand only when they spell
  one layer with no `var()` (#1492).
- **An `ellipse()` lost a radius.** `clip-path:ellipse(10px closest-side)`
  was minified to `ellipse(10px)`, which Chromium rejects, so the clip
  disappeared, and `ellipse(closest-side farthest-side)` and
  `ellipse(var(--v) closest-side)` broke the same way. An omitted size is
  `closest-side` on both axes (CSS Shapes 1 sec. 3.1), so the radii now drop
  only as that pair (#1493).
- **Anchor fallback tactics were reordered.**
  `position-try-fallbacks:flip-start flip-block` was minified to
  `flip-block flip-start`, which places the box elsewhere, since the tactics
  compose in the order written (CSS Anchor Positioning 1 sec. 6.1). A name
  between two tactics, which Chromium rejects, was moved out to make the entry
  valid. The tactics now keep their order and a split entry is refused (#1494).
- **Two longhands composed past a shorthand that resets one.**
  `position-try-order:most-height;position-try:flip-inline;position-try-fallbacks:top`
  was minified to `position-try:most-height top;position-try:flip-inline`,
  which moves the fallbacks before the shorthand that resets them, so the box
  falls back to `flip-inline` instead of `top`. `columns` did the same past
  `-webkit-columns`. Two longhands now compose only past declarations that do
  not write the later one (#1495).
- **`animation-range` lists were merged side by side.**
  `animation-range-start:exit 100%;animation-range-end:cover,entry` was
  minified to `animation-range:exit 100%cover,entry`, which gives the second
  animation another start, and `cover,entry` beside `exit,contain` became
  three ranges. The shorthand pairs each start with its end (CSS Animations 2
  sec. 6.3), so lists of one length now compose range by range and lists of
  different lengths stay longhands (#1496).
- **A transition of a property named like an easing keyword changed.**
  `transition:ease ease 1s` transitions a property named `ease`, and was
  minified to `transition:ease 1s`, which transitions `all`;
  `ease-in linear 1s` became `linear 1s ease-in`, which swaps the easing and
  the property. A browser reads the first easing keyword as the easing and a
  second as the property, so such a property now prints after its easing,
  `ease` included (#1497).
- **Declarations Chromium reads were deleted.** `--minify` refused each value
  below and dropped its declaration. Each is now read (#1498, #1499, #1500,
  #1514, #1521, #1522, #1523, #1535).
  - `position-try:flip-x` and `position-try-fallbacks:flip-y flip-block`, the
    physical tactics beside the logical ones (CSS Anchor Positioning 1 sec.
    6.1).
  - `columns:10px 2/5px` and `columns:0/0`, which give a column height (CSS
    Multicol 2 sec. 4.5). A `columns` before one stays as the fallback Firefox
    and Safari paint.
  - `animation-range:scroll`, `animation-range-start:scroll 10%` and a `scroll
    50%` keyframe, the `scroll` timeline range (Scroll-driven Animations 1 sec.
    3.1).
  - `scrollbar-gutter:both-edges stable`, in the other order CSS Overflow 3
    sec. 5.2 grants, and `offset:content-box auto`, a reference box with no
    shape before an offset distance or rotation (CSS Motion Path 1 sec. 2.1 and
    2.6).
  - `contain-intrinsic-size:0 auto`, which Chromium reads as `0`. No
    specification grants the form and Safari drops it, so it is kept as
    written, and a declaration before it stays as Safari's fallback.
  - `line-height:min(calc(1em + 2px),1px)` and `max(1em + 2px,1px)`. Each
    argument of `min()`, `max()` and `clamp()` is read as the sum CSS Values 4
    sec. 10.2 grants, and arguments of two types are still refused.
  - `stroke-dasharray:1 2 calc(3%)` and every dash pattern with a math function
    after its first dash, `object-view-box:inset(0 round 4px)` and every
    rounded `inset()`, and `baseline-shift:1` and `baseline-shift:baseline`.
    Chromium reads the number as px, so an earlier `baseline-shift` won.
  - `animation:foo 1s -1s`, a form `--minify` printed itself from `calc(-1s)`.
    The delay, the second time, may be negative; only the duration must not be
    (CSS Animations 1 sec. 3.6).
- **Anchor-positioned boxes lost their position.** `--minify` deleted
  `top:anchor(50%)`, `top:anchor(top --a)`, `width:anchor-size()` and every
  other `anchor()` or `anchor-size()` with a percentage side, the name after
  the keyword, or no argument, though Chromium places the box by each (CSS
  Anchor Positioning 1 sec. 3.2 and 5.1). It kept `anchor(width)` and
  `anchor-size(top)`, which Chromium drops. Both functions now read the
  grammar the specification gives, and print the name first (#1532).
- **Legacy `-webkit-` images were deleted or rewritten.** `--minify` deleted
  `-webkit-radial-gradient(center, ellipse cover, red, blue)`, the prefixed
  syntax with its position first, and every `-webkit-cross-fade(a, b, 50%)`,
  in each property that takes an image. Chromium paints both. It also read
  `-webkit-radial-gradient(circle at center, red, blue)`, which Chromium
  drops, as the unprefixed syntax, and printed a gradient Chromium paints. The
  prefixed radial gradients now read the syntax the Compatibility Standard
  gives them (sec. 3.3.2), and `-webkit-cross-fade()` reads as Chromium reads
  it (#1533).
- **An inverse trigonometric function counted as a number.** `--minify`
  turned `opacity:asin(0)` into `opacity:0`, and did the same at `z-index`,
  `line-height`, `flex-grow`, `scale` and inside `calc(asin(0)*1px)`, though
  `asin()`, `acos()`, `atan()` and `atan2()` answer an angle (CSS Values 4
  sec. 10.4) and browsers drop those declarations. It also folded
  `sin(asin(.5))` to `-.98803162`, reading the angle's degrees as radians.
  Such a declaration is now dropped, and `sin(asin(.5))` is `.5` (#1516).
- **`&` under a pseudo-element styled the pseudo-element.** CSS Nesting 1
  sec. 4 says `&` cannot represent a pseudo-element, so
  `p::before{&{color:blue}}` styles nothing, and Chromium leaves the
  `::before` alone. `--minify` printed it as `p:before{color:#00f}`,
  `--flatten-nesting` turned `.x &` under `p::before` into `.x p:before` and
  `&:hover` under `::part(p)` into `::part(p):hover`, and `cascade diff
  --diff=canonical` called `p::before{color:red;&{color:blue}}` equal to
  `p::before{color:blue}`. Such a rule now matches nothing and goes, a list
  parent's pseudo-element branch gives `&` nothing, and no rule is nested
  under a pseudo-element (#1503).
- **Nesting inside `@scope` lost the scoping root.** A rule in an `@scope`
  block is relative to the scoping root (CSS Cascade 6 sec. 3.5.3).
  `--minify` merged `@scope (.c){.a{.x &{...}}}` into `.x .a` and
  `--flatten-nesting` turned `@scope (.c){:is(.a,.b){.a &{...}}}` into
  `.a :is(.a,.b)`, so an `.x` or `.a` outside the scope stopped counting.
  Flattening also turned `.a{@scope (.b){color:red}}` into
  `@scope(.b){.a{color:red}}`, which styles an `.a` inside a `.b` rather than
  a `.b` inside an `.a`. Flattening now spells the root as `&` where a
  selector no longer starts with its parent, `--minify` keeps such a rule
  nested, and a nested `@scope` roots under its parent (#1504).
- **Nesting the default targets do not read.** `--minify` turned
  `.a{color:red}.a .b{color:blue}` into `.a{color:red;.b{color:#00f}}`, kept
  an author's nesting, and turned `& div` into `div`. Chrome 111 and Safari
  16.4 read no nested rule, and Chrome 112 to 119 and Safari 16.5 to 17.1
  refuse one that starts with an identifier, so those releases lost the rule.
  For the default targets and under `--enforce-spec` nesting is now flattened
  and none is synthesised, which makes the output larger on sheets that
  nested. Library callers whose targets parse nesting keep it, apart from a
  rule holding a declaration after a nested rule or in a nested `@media`
  (CSS Nesting 1 sec. 5), which is flattened under any targets (#1505).
- **Nesting gave `&` another parent.** Where nesting was synthesised,
  `.c{.a{color:red}.a &{color:blue}}` became `.c .a{color:red;&{color:#00f}}`
  and `div{X}div>&{Y}` became `div{X;>&{Y}}`, so `&` named the new parent
  rather than the old one. A rule whose selector holds `&` past its parent's
  prefix now stays where it is (#1506).
- **A math function at a counter, feature or axis number was deleted.**
  `--minify` deleted `counter-increment:a calc(1)`, `counter-reset:a
  calc(2*3)`, `font-feature-settings:"liga" calc(1)`,
  `font-variation-settings:"wght" calc(400)` and `rotate:1 calc(1) 0 45deg`,
  and an `@property` rule whose `<number>` initial value was `calc(1.5)`. CSS
  Values 4 sec. 10.1 puts a math function at any slot of its type, and sec.
  10.9 rounds one at an integer. A call that folds to a constant is now read as
  that number, rounded at an integer (`calc(1.5)` is `2`). (#1515).
- **A math function over a `var()` could delete its declaration.**
  `--minify` deleted `rotate:atan2(var(--y),var(--x))`, the same over
  `acos()`, `asin()` and `atan()`, `rotate:x acos(var(--v))` and
  `clip-path:circle(var(--v) 0% 0%)` as invalid, and printed the first from
  `rotate:calc(atan2(...))`, so a second pass deleted what the first kept. A
  declaration holding a `var()` is valid until substitution (CSS Variables 1
  sec. 3), and such a value is now kept as written (#1513).
- **An infinite negative size became an invalid literal.** `--minify` turned
  `min-height:calc(-infinity*1px)` into `-3.40282e38px`, which the twelve
  sizing properties refuse, so the declaration was dropped where the source
  clamps to `0px` (CSS Values 4 sec. 10.12). The call now stays around the
  folded value, as for any other negative result (#1512).
- **A value moved or dropped components around a `var()`.** `--minify` treated
  a `var()` beside other components as one of the property's slots, then filled
  in, dropped, merged or reordered the slots around it: `flex:var(--g) 0px`
  became `var(--g) 1 0`, `flex:1 1 var(--b)` became `1 var(--b)`, `gap:var(--a)
  var(--a)` became `var(--a)`, `list-style:var(--a) none` became `none var(--a)
  none`, `grid-area:var(--a) / auto / auto`, `border:var(--a) none` and
  `transition:var(--a) 0s` lost their trailing slots,
  `transform:translate(var(--a),0px)` lost its second argument, `background:red
  var(--x)` moved the colour last, `rgb(var(--r),var(--g),var(--b))` lost its
  commas, and `flex:var(--g) 1 0px` became `var(--g) 1 0`. A `var()` may supply
  any number of components, commas included (CSS Variables 1 sec. 3), and
  Chromium renders each pair differently for some value of the custom property.
  Such a value is now written back with every component in place, each still
  respelled where it stands; a `var()` that is the whole value or sits inside a
  math function is unchanged. `cascade diff --diff=canonical` no longer calls
  such a pair equal. (#1511).
- **The space beside a `var()` in a math function was dropped.**
  `--minify` turned `calc(100% var(--op) 10px)` into
  `calc(100%var(--op) 10px)`, in a typed declaration and in a custom property.
  The `var()` may substitute a `+` or `-`, which CSS Values 4 sec. 10.8 needs
  whitespace around, so with `--op: -` the first is `100% - 10px` and the
  second is invalid. The whitespace beside a `var()`, `env()` or `attr()` in a
  math function now stays, except next to `*`, `/` or a comma (#1510).
- **A comparison over a position percentage was folded.**
  `background-position:min(10%,20%) 0` became `10% 0`, and
  `calc(min(10%,20%) + 1px)` became `calc(10% + 1px)`. A percentage in
  `background-position`, `mask-position` or `object-position` resolves against
  the box less the image, negative when the image is the larger, so
  `min(10%,20%)` there is `20%` of it. A comparison, `abs()` or a stepped
  function over such a percentage is now kept as written; one whose basis
  cannot be negative, as in `transform-origin`, still folds (#1509).
- **A zero lost its unit inside a line-width comparison.** `--minify` turned
  `outline-width:max(0px,var(--v))` into `max(0,var(--v))`, which mixes a
  number with a length and is invalid at computed-value time, across the 27
  line-width properties; it folded `round(1px,0px)` to `1px` and
  `mod(1px,0px)` to `1px`, where CSS Values 4 sec. 10.9 makes both NaN and
  Chromium computes `0px`. A zero operand now keeps its unit, and a zero step
  or divisor keeps the call (#1508).
- **`border` reset a `border-image` set elsewhere.** `--minify` contracted
  `border-width`, `border-style` and `border-color` into `border`, which also
  resets `border-image` (CSS Backgrounds 3 sec. 4.4). Where another sheet of
  the page, or a rule of the same sheet inside `@media`, set a `border-image`
  on the element, Chromium drew none. The three now stay longhands. Under
  `--scope=stylesheet` they contract when no rule of the sheet, in any block,
  holds a `border-image` value, and so do the four sides `border-top`,
  `-right`, `-bottom` and `-left` holding one value. `cascade diff
  --diff=canonical` called the two spellings identical, and now calls them
  different (#1537).
- **Declarations in newer CSS were deleted.** `--minify` dropped, with a
  warning, a length in a root font unit (`rex`, `rcap`, `rch`) or a logical
  viewport unit (`svi`, `lvb`, `dvi` and the rest), a container unit in
  `flex-basis` or `flex`, and any value holding `if()` (CSS Values 5 sec. 8.3)
  or a custom function call such as `--f()` (CSS Mixins 1). Chromium 153 reads
  each, so `font-size:1rcap` or `width:if(media(print):1px;else:2px)` lost its
  effect. Each length is now read with its unit, and a value holding `if()` or
  `--f()` is kept as written, as one holding `var()` is, when the call matches
  its own grammar. (#1536).
- **A fallback before a prefixed value was deleted across rules.**
  `.a{position:sticky}` before `.a{position:-webkit-sticky}` lost
  `position:sticky`, and `display:flex` before `display:-webkit-box` lost
  `flex`, though a browser that reads only the unprefixed value then paints
  neither. The compatibility data says nothing of which browser reads a
  prefixed keyword, so an earlier value of the property now stays before one
  that uses such a keyword, across rules as it did inside one. Two values
  with the same prefixed words, such as two `-webkit-linear-gradient()`s, are
  read by the same browsers, and the earlier one now goes inside one rule
  too (#1538).
- **Prefixed, legacy and newer keywords were deleted.** `--minify` dropped,
  with a warning, values Chromium 153 reads: `cursor:-webkit-grab`,
  `display:-webkit-inline-box`, `width:-webkit-fill-available`,
  `overflow:overlay`, `writing-mode:tb-rl`, the deprecated system colours such
  as `Window` and `ThreeDFace`, `place-content:flex-start`,
  `content:no-open-quote`, `font:oblique 10deg 12px a`, `image-set("a.png")`, a
  grid area named `1a`, `perspective(none)`, `white-space:preserve` and
  `border-image:url(a.png) 30 // 5px`, among others. Each is now read, and a
  prefixed or legacy keyword keeps an earlier value of its property as its
  fallback, as `-webkit-box` does. (#1539).
- **An invalid `mask-position` came out valid, and a mask some browsers
  read was deleted.** `mask-position:left 10px top` uses the three-value
  form only `background-position` has, so Chromium, Firefox and Safari drop
  it, and `--minify` printed `mask-position:10px 0`, which they read.
  `mask-position` now reads the `<position>` of CSS Values 4, while
  `-webkit-mask-position` keeps the legacy form browsers read there. The
  same position in the `mask` shorthand, `mask:url(a.png) left 10px top`,
  is read by Chromium and Safari and not by Firefox, and `--minify` dropped
  it. It is now kept as written, so each browser reads or drops it as it
  did (#1542).
- **An invalid `image-set()` came out valid.** `image-set(url(a.png) 1x 2x)`
  repeats its resolution, so browsers drop the declaration, and `--minify`
  printed `image-set(url(a.png)1x)`, which they read. The repeat now makes
  the declaration invalid, as a repeated `type()` does (#1540).
- **A colour changed the way it blends.** `--minify` turned
  `linear-gradient(color(srgb 1 0 0),color(srgb 0 0 1))` into
  `linear-gradient(red,#00f)`, and Chromium paints the middle of the first
  (139,83,162) and of the second (127,0,128): CSS Color 4 interpolates a hex,
  a name, `rgb()`, `hsl()` and `hwb()` in sRGB and every other colour in
  Oklab. The same fold to hex changed transitions, animations and
  `color-mix()` over `currentcolor`, and reading `none` as zero changed them
  too, since an interpolation gives a missing component the other colour's.
  An `oklch()`, `lab()` or `color()` colour now stays in its notation,
  rounded within the same budget, a static `color-mix()` folds to a colour of
  its interpolation space (`color(srgb .5 0 .5)` for `color-mix(in
  srgb,red,blue)`), and `none` is kept, in `hsl()` and `hwb()` too.
  `cascade diff --diff=canonical` no longer calls those spellings identical.
  The SatCSS sheets minify as before; Tailwind's output keeps its `oklch()`
  palette, about 3 KB larger raw and 0.7 KB under gzip (#1549).
- **Huge numbers lost digits.** `--minify` printed a number from 2^62 up, or
  a fraction from about 4.6e10 up, with 12 significant digits:
  `46116860184.27387904px` became `46116860184.3px` and `123456789012.5px`
  became `123456789012px`. A browser clamps such a length, but it adds a
  `calc()`'s operands first, so `calc(123456789012.5px - var(--b))` with
  `--b:123456789012px` rendered 0px wide instead of half a pixel. Such a
  number now prints the shortest digits that read back as its value (#1550).
- **Names with a control character broke their rule.** A control character
  written as an escape (`\1 `) in an attribute name, in `::part()`,
  `:state()`, `:dir()`, `::highlight()` or the view transition selectors, in
  an `@namespace` prefix or `@supports` function, or in the name `attr()`,
  `env()` or `anchor()` reads, was printed back raw. A browser reads the raw
  character as punctuation and drops the rule or declaration (Chromium drops
  `[\x01]{}` and keeps `[\1 ]{}`). Every name is now written as CSSOM sec.
  2.1 serialises an identifier, selectors included, so a class holding a
  letter from U+0080 up also prints the letter instead of a hex escape (#1551).
- **Unwrapping `:is()` took other selectors down.** `--minify` printed
  `.c,:is([x=a s])` as `.c,[x=a s]`. `:is()` drops an argument a browser
  cannot read on its own, but a plain list is dropped whole, and Chromium
  reads no `s` flag, so `.c` lost its colour there. The same happened to
  `:is(:-moz-focusring)` and `:not(:not(...))`, and `cascade diff
  --diff=canonical` called the two spellings equal. An argument some target
  browser does not read now keeps its `:is()` (#1552).
- percentage media feature was read as a width bound.** `--minify` respelled
  `@media (min-width:50%)` as `(width>=50%)`, though Media Queries 4 grants no
  percentage there and Chromium never matches it. It is now kept as written
  (#1566).
- math function in a custom property was folded to its number.**
  `--lh:calc(2.25/1.875)` became `--lh:1.2`, but Chromium computes
  `z-index:var(--lh)` as `1` under the first and `auto` under the second,
  since an unregistered custom property substitutes its tokens. A number
  result is now kept as written, and a time folds only when it is not negative
  (#1567).
- and a one-argument `round()` at an integer were deleted.**
  `counter-increment:a sign(1)` and `z-index:round(1.5)` are now read as
  Chromium reads them (CSS Values 4 sec. 10.3 lets `round()` omit its step)
  (#1568).
- and a `none` bound of `clamp()` were dropped.** `--minify` deleted
  `width:revert-rule` and `width:clamp(none,50%,80px)` with a warning, though
  Chromium and WebKit read both, and merged `.c{--x:1}.c{--x:revert-rule}`
  into one rule, which changes what the keyword rolls back to (CSS Cascade 5
  sec. 7.3.6). Both are now kept, `clamp()` with a `none` bound is never
  folded, and a rule holding `revert-rule` is never merged, split, or let
  shadow an earlier rule (#1569).
- math function that cannot fold was deleted at `opacity`.**
  `opacity:sign(1em)`, `shape-image-threshold:sign(1em)` and `zoom:sign(1em)`
  were dropped, though Chromium computes each as 1; they are now kept (#1571).
- distant `@media` merge changed which `@keyframes` ran.** Joining two `@media
  print` blocks across a `@keyframes k` moved the later `@keyframes k` above
  it, so the earlier one won; `@font-face` too. A merged block now carries
  style rules only, and a distant merge may cross comments, `@keyframes`,
  `@font-face`, `@property` and a top-level `@scope` block (#1576).
- negative zero in a calculation lost its sign.** `opacity:calc(1/-0)` printed
  as `calc(1/0)` and `z-index:calc(1/sign(-0))` as `calc(1/sign(0))`, which
  flips the infinity: Chromium computes `-2147483648` against `2147483647`. A
  zero keeps its sign now (#1582).
- number math function other than `calc()` was deleted at a number slot.**
  `aspect-ratio:cos(0)`, which Chromium computes as `1 / 1`, was dropped, as
  were `tan()`, `exp()`, `log()` and `sin()` of a number there; they are now
  read (#1583).
- math function that cannot fold was deleted at a counter, font setting or
  rotate axis.** `counter-increment:a sign(1em)`,
  `font-feature-settings:"liga" sign(1em)`, `font-variation-settings:"wght"
  calc(400*sign(1em))` and `rotate:1 sign(1em) 0 45deg` were dropped, though
  Chromium keeps each; they are now kept, and a call that folds still becomes
  its number (#1584).
- `background-blend-mode` followed by `background` is kept.** Compositing 1
  sec. 3.4.3 has the shorthand reset it, but Chrome and WebKit keep it, so
  dropping it stopped the layers blending; `--diff=canonical` no longer calls
  the two equal (#1599).
- `-moz-` radial gradients were deleted or rewritten.** `--minify` deleted
  `-moz-radial-gradient(center, ellipse cover, red, blue)`, which Firefox 160
  paints, and kept `-moz-radial-gradient(circle at center, red, blue)`, which
  it drops; the `-moz-` and `-o-` radial gradients now read the 2011 syntax
  (CSS Image Values 3 WD sec. 5.2.1) as the `-webkit-` ones do, with Firefox's
  interpolation hints for `-moz-` (#1604).
- gradients with an interpolation method or a `to` direction are kept**:
  `--minify` deleted `-moz-radial-gradient(in oklab,red,blue)` and
  `-moz-linear-gradient(to bottom,...)`, which Firefox paints; a `to`
  direction keeps the unprefixed meaning Firefox gives it.
  `-webkit-radial-gradient(10px 20px red,blue)`, which no browser reads, is no
  longer rewritten into one they paint (#1605).
- gradients only one engine reads are kept**: Firefox's interpolation hints
  and methods in `-webkit-radial-gradient()`, Chromium's missing comma after a
  radial shape and size (kept missing, so Firefox still drops it), and
  `-webkit-linear-gradient()` with an interpolation method, which both paint
  (#1606).
- gradient whose interpolation hint is not between two colour stops is
  invalid**, as CSS Images 4 sec. 3.5.1, Chromium and Firefox say, so
  `--minify` drops it instead of keeping it (#1607).
- `none` alpha is kept**: `rgb(0 0 0 / none)`, transparent at rest (CSS Color
  4 sec. 4.4), no longer minifies to the opaque `#000`, and `--diff=canonical`
  tells it from an opaque or a zero-alpha colour. A negative `lch()` or
  `oklch()` chroma clamps to zero at parse (sec. 9.3) (#1612).
- legacy comma-separated `rgb()` and `hsl()` syntax is read by its own
  grammar** (CSS Color 4 sec. 5.1 and 7.1): it takes no `none`, `rgb()` takes
  three numbers or three percentages, and `hsl()` takes percentages and a
  comma alpha, so `color:blue;color:rgb(255,0%,0)` no longer becomes
  `color:red` (#1613).

#### Stable output

- **Minifying twice gave other bytes than minifying once.** With 1.2.1, 69 of
  the 504 SatCSS sheets and both Tailwind sheets shrank when their `--minify`
  output was minified again, by up to 434 bytes. One run left work for the
  next. A merged rule was nested under its neighbour only on the second run,
  and `linear-gradient(0deg,red 0%,blue 100%)` took two runs to reach
  `linear-gradient(#00f,red)`. A sheet of more than 128 rules got one pass
  where a smaller one got up to five. The optimiser now finishes in one pass at
  every size, and a second `--minify` changes none of the 504 sheets nor
  Tailwind's output. A pipeline that minifies at two stages gets the bytes of
  one that minifies once (#1342, #1370, #1395, #1401, #1402, #1403, #1424).
- **Two spellings of one value merged only on a second run.**
  `a{background:url("i.png")}b{background:url(i.png)}` minified to two rules,
  and minifying that again gave `a,b{...}`. The same held for `center center`
  and `center` in `position-area` and `place-items`, `flex:1 1 0%` and
  `flex:1`, a quoted `url()` in `list-style`, `content` and `mask`, `rotate:1 0
  0 10deg` and `rotate:x 10deg`, two spellings of a colour, a `calc()` or an
  unknown property's value, and more. `aspect-ratio:calc(2*16/9)` beside
  `calc(16/9*2)`, and `contain-intrinsic-inline-size:100E0px` beside
  `calc(1e2px)`, did the same. One run now merges each pair. An `aspect-ratio`
  calculation folds to six significant figures, as `line-height` does, and
  `--lossless` keeps it. `Css.to_string`, which prints without optimising,
  writes a value as it is held: `url("i.png")`, `flex:1 1 0%` and `hsl(120deg
  50% 50%)` stay so, and `Css.minify` shortens them. A prefixed copy that
  spells its twin's value another way, such as `-ms-flex:0 0 auto` beside
  `flex:none`, now goes as one spelled alike does, which takes about 28 kB off
  the SatCSS output (#1469, #1527, #1528).
- **A number past eight decimals merged only on a second run.**
  `a{width:33.33333333333333%}b{width:33.33333333%}` minified to two rules
  printed alike, and minifying that again merged them. The printer writes a
  number to eight decimals while the reader kept every digit. A number read
  past eight decimals is now the eight-decimal value, so one run merges them;
  the SatCSS sheets held 252 such declarations. A colour channel that the
  digits past eight decimals move across a byte's half is kept as written
  instead: `rgba(0,0,0,0.09999999999999998)` paints alpha 25 in Chromium, its
  space-syntax spelling 26, and 1.2.1 folded both to `#00000019` and printed
  `rgb(0 0 0/.1)` under `--lossless`; beside a `var()`, and through
  `--inline-vars`, the same alpha became `.1`. An alpha the fold rounds now
  keeps the byte it paints, so `rgb(0 0 var(--b)/.0996)` no longer becomes
  `/.1`. For the library, `Values.color` gains `Authored_color { color;
  minified }`, the colour kept as written; a match over `Values.color` needs
  the arm (#1548).
- **Output shifted for unrelated reasons.** Merges that save the same number of
  bytes were ordered by hash, so a change inside cascade could move the output
  of a sheet nobody touched. A tie now breaks by the shortest result, then by
  source order (#1355).
- nested value prints under the sheet's flags**: `--enforce-spec` printed
  `oklch(.7 76%20)` in a border, a gradient stop and `override-colors` where
  `color` kept `.304`, and `Css.minify ~rename_custom_property` now renames a
  `var()` inside a border colour (#1614).

#### Which rules merge

- **The default objective bounds merges by distance.** With 1.2.1, `--minify`
  merged rules at any distance, then threw a whole run of merges away when an
  estimate of the gzip size grew. Near merges were lost with far ones, and a
  short sheet was searched differently from a long one. The default
  `--objective=transfer` now minimises output bytes, and merges rules with
  identical bodies only within 128 printed rules of each other. A compressor
  encodes a repeated declaration in two or three bytes wherever it sits, but a
  selector moved away from its neighbours stops compressing against them.
  `--objective=raw` merges at any distance, for output that ships uncompressed.
  Against 1.2.1 the output is smaller in total, but not on every sheet. 182
  sheets shrink by more than 1% raw. About twenty grow by more than 1% under
  gzip, at most by about 3%, and 27 under brotli, one cnn sheet by about 7%
  (#1399, #1422, #1423, #1425).
- rules holding only nested rules merge** under targets that read nesting
  (#1575).
- `@supports` blocks of one condition merge,** and a distant `@media` or
  `@container` merge moves the earlier block down to the later one when the
  later cannot rise (#1577).
- box shorthand and a side of the same value no longer order each other:**
  `padding:1px` and `padding-top:1px`, likewise `margin` and `inset`, in the
  minifier and in `cascade diff --diff=canonical` (#1578).
- and `[x=A]` are disjoint** unless HTML compares `x` without case (HTML sec.
  4.16.2), so rules on them merge past each other (#1579).
- merges cross the remaining definition rules:** `@counter-style`, `@page`,
  `@font-palette-values`, `@font-feature-values`, `@position-try` and
  `@view-transition`, none of which takes part in the order of style rules. A
  block holding one never moves, so a later definition of a name is never
  hoisted above an earlier one (#1590).

#### Smaller output

- **Math functions over two units of one type fold.** `min(1s,1000ms)`,
  `calc(1s + 500ms)`, `min(1turn,10deg)` and `max(1cm,10mm)` were kept as
  written, since the fold compared and summed only equal units. Units with a
  fixed ratio now convert first (CSS Values 4 sec. 10.7), so they fold to `1s`,
  `1.5s`, `10deg` and `1cm`, and `line-height:abs(-1px)` folds to `1px`. A
  call that still cannot fold, such as `min(1s,var(--x))`, prints bare rather
  than inside `calc()` (#1507).
- **Prefixed copies of widely available properties go.** `--minify` dropped
  `-webkit-animation-delay:1s` beside `animation-delay:1s`, but kept
  `-webkit-flex-grow:1` beside `flex-grow:1`, a prefixed property cascade has
  no type for. Such a copy is now dropped too when the two values agree, which
  shrinks the SatCSS output by about 0.7% raw and 0.3% under gzip.
  `--enforce-spec` keeps every copy (#1439).
- **Thirteen more prefixed aliases are shortened and checked.** `--minify`
  printed `-webkit-flex:0 0 auto` and `-webkit-transform-origin:top left` as
  written, and kept `-webkit-flex:0 1 main-size`, which no target browser
  parses. `-webkit-flex` and its three longhands, `-webkit-order`, the four
  `-webkit-border-*-radius` corners, `-webkit-transform-origin`,
  `-webkit-transform-style`, `-webkit-perspective-origin` and
  `-webkit-backface-visibility` are WHATWG Compatibility legacy aliases, which
  every target engine parses under the unprefixed property's grammar. They now
  print as `flex:none` and `transform-origin:0 0` do, and a value that grammar
  refuses is dropped with a warning. A few SatCSS sheets shrink, most of them
  under `--enforce-spec`, which keeps every prefixed copy (#1518).
- **An overridden `content` goes.** `--minify` kept both declarations of
  `.a{content:"a";content:"b"}`, since a later `content` can be a value some
  browser rejects and the earlier its fallback. After a string, a quote
  keyword, a counter, `none` or a CSS-wide keyword, which every browser reads,
  nothing is left to fall back to, and the earlier declaration is dropped. So
  is a normal `content` beside an important one, in either order (#1417,
  #1419).
- **An overridden `text-rendering` goes,** now that the property is typed
  (#1440).
- **`--enforce-spec` and `--lossless` left merges unmade.** A shared
  `font-family:"A B"` stayed in each rule where lifting it into one saved
  bytes. `--minify` weighed each declaration at its default spelling, which is
  shorter than the one either flag prints: `--enforce-spec` keeps a family
  name's quotes, and both keep an oklch chroma's number where the default
  writes a percentage. It now weighs the bytes the run prints, which changes 21
  of the 505 SatCSS and Tailwind sheets under `--enforce-spec` and none under
  `--lossless`; default output is unchanged (#1477).
- descriptors drop needless whitespace:** `additive-symbols:2 "a",1 "b"` and
  `range:1 2,4 5` (#1570).
- unfolded math function at a number slot prints without `calc()`:**
  `z-index:calc(sign(1em))` is now `z-index:sign(1em)`, and so at
  `line-height`, `font-weight`, `order`, flex factors, `column-count` and
  `scale` (#1572).
- basic shape drops a zero `round`:** `inset(0 round 0)` prints `inset(0)` in
  `clip-path` and `object-view-box` (#1573).
- registered length times zero folds under `--scope=stylesheet`:**
  `calc(var(--l)*0)` becomes `0px` for a `<length>` registration where the
  calculation needs a length, as it already did for `<number>` and `<integer>`
  (#1574).
- is scored by compressed size.** The default transfer objective weighed
  identical-body merges by an estimate of their compressed size but factoring
  by raw bytes; every merge is now scored by the estimate, so a factoring gzip
  would undo is no longer taken. The SatCSS sheets come out 0.12% smaller
  under gzip and 0.14% under brotli, 0.26% larger raw; `--objective=raw`
  output is unchanged (#1581).
- conic gradient drops its default prelude:** `conic-gradient(from 0deg at
  center,blue,red)` prints `conic-gradient(blue,red)`, and `from 45deg at 50%`
  prints `from 45deg`. Both stay when a `var()` leads the stops (#1585).
- output drops the space beside a string:** `grid-template-areas:"a""b"`,
  `"head head"auto`, `quotes:"a""b"`, which Chromium reads as the spaced forms
  (CSS Syntax 3 sec. 9.1) (#1586).
- vendor fallback every target outgrew is dropped:**
  `display:-webkit-box;display:flex` and a `-webkit-linear-gradient()` before
  `linear-gradient()` keep only the standard value when every target browser
  parses it. A prefix some target still needs, such as
  `width:-webkit-fill-available` before `stretch`, stays (#1587).
- declarations stay nested where every target reads them.** Under targets from
  Chrome 130, Firefox 132 and Safari 18.2 (`api.CSSNestedDeclarations`), a
  declaration after a nested rule or directly in a nested group rule is kept
  in place instead of being flattened into a copy of its parent, and a bare
  `&{...}` rule under a single selector becomes declarations:
  `.a{b{color:red}color:#00f}` rather than `.a b{color:red}.a{color:#00f}`
  (#1589).
- is normalised** as `clip-path` and `background-image` are: `inset(0 round
  0)` prints `inset(0)`, a `margin-box` beside a shape is dropped as the
  default (CSS Shapes 1 sec. 6.1), and an image gradient's colours are
  shortened (#1591).
- longhand restating a shorthand's omitted initial is dropped**, as in
  `background:0 0;background-image:none` (1975 bytes over 62 of 504 SatCSS
  sheets) (#1596).
- 1 <basis>` is written as the lone basis** (`flex:1 10px` is `flex:10px`,
  Flexbox 1 sec. 7.1), and a background colour that is transparent black in
  any spelling (`#0000`, `rgb(0 0 0/0)`) is dropped: 208 bytes over 47 of 504
  SatCSS sheets (#1599).
- folds a `color-mix()` of one colour**: CSS Color 5 sec. 3.3 makes
  `color-mix(in srgb,blue 40%)` the colour with its alpha scaled, `color(srgb
  0 0 1/.4)`. The default targets keep it as written, since Chrome drops it;
  `--diff=canonical --enforce-spec` reads the two as one value (#1600).
- `--enforce-spec`, `background` resets `background-blend-mode`** as
  Compositing 1 sec. 3.4.3 says, so a blend mode a later `background` makes
  dead is dropped and `--diff=canonical --enforce-spec` calls the two equal;
  the default mode keeps it for Chrome and WebKit (#1602).
- legacy alias restating what a shorthand set is dropped**:
  `-webkit-flex-basis:0%` or `-webkit-flex-grow:1` after `flex:1` goes like
  its unprefixed spelling (Compatibility sec. 3.4.1, Cascade 5 sec. 3.1), 162
  bytes over 7 of 504 SatCSS sheets (#1603).

### Formatted output

- `cascade fmt` kept whichever spelling the input used, so one selector printed
  two ways. It now prints `::before` for `:before` and `odd` for `2n+1`
  (#1357).
- `lab()` and `oklab()` printed a percentage alpha as a number: `lab(50% 20 30
  / 40%)` came out as `lab(50% 20 30 / .4)`. It prints as written, as in the
  other colour functions (#1370).

### Comparing stylesheets

#### `cascade diff --diff=canonical`

- **Two sheets were called identical when they are not.** One sheet could lose
  a rule or a declaration inside an `@media` block to a parse error and still
  compare identical: `@media screen{a!{color:red}b{color:red}}` against `@media
  screen{b{color:red}}` exited 0. That loss now counts as unread, as one inside
  `@supports` does, and the command exits 2. The comparison also called
  `.o{width:76px}.g,.o{display:none}.p{display:block}.g{width:24px}` identical
  to a sheet that gives an element of both classes a width of 76px instead of
  24px, and called `.a{color:red}@media screen{.b{color:blue}.a{color:red}}`
  identical to the same sheet without its last `.a`, which on screen paints
  an element of both classes blue instead of red. A rule inside a condition
  that repeats an earlier write of its selector is now dropped only when
  nothing, `all` included, wrote the same property in between, inside the
  block or out. Each pair now compares different (#1312, #1406, #1451).
- **A guard a newer browser reads was no difference.**
  `@supports (field-sizing:content){a{color:red}}` compared identical to an
  empty sheet, and so did a guard on `anchor-name`, since no target version
  reads either. Chrome ships them from 123 and 125, and Chromium 153 colours
  the link. A target now stands for its version and every later one, so a
  guard is dropped only when every browser at or past the targets fails it,
  and the pair compares different (#1463).
- **A double negation in `@supports` was printed invalid.**
  `@supports not (not (display:grid))` came out as
  `@supports not not (display:grid)`, which Chromium drops with its block and
  cascade cannot read back, and `((display:grid) and ((color:red) or
  (gap:1px))) or (top:0)` lost the parentheses around its inner `or`. Every
  operand now keeps the parentheses the grammar asks of it (#1485).
- **A list a browser drops whole compared equal to its rules.**
  `a,a:has-slotted{color:red}` compared identical to
  `a{color:red}a:has-slotted{color:red}`, though Chromium 153 drops the first
  rule whole and leaves `a` uncoloured. A list whose selectors some target
  release reads apart now compares as one rule, and one no target parses,
  such as a list holding a `-ms-` pseudo-element, as no rule (#1487).
- **A prefixed property was taken for one no browser reads.** The WHATWG
  Compatibility Standard lists `-webkit-mask-box-image` and its longhands as
  aliases of `mask-border` and its longhands, and the comparison read every
  listed alias as its property, so it called
  `.a{-webkit-mask-box-image-width:1px}` identical to
  `.a{mask-border-width:1px}`. No Chromium or Firefox release reads
  `mask-border`, and Chromium leaves the prefixed width at `auto` under the
  second sheet. The set is now a property of its own, and the pair compares
  different, as it did in 1.2.1 (#1517).
- **A prefixed value was taken for one a browser reads later.** The
  comparison read every legacy alias as its property whatever the value, so it
  called `.a{-webkit-background-clip:text}` identical to
  `.a{background-clip:text}`. Chrome 111 to 119 parse `text` only under the
  prefixed name and paint the second sheet's box. An alias now reads as its
  property only where every target release that parses one spelling of the
  value parses the other, so the pair compares different; `-webkit-mask-image`
  against `mask-image`, which Chrome reads unprefixed from 120, does too
  (#1519).
- **A guard was decided by its property alone.**
  `@supports (break-before:always){a{color:red}}` compared identical to
  `a{color:red}`, though Chromium 153 drops `break-before:always` and leaves
  the rule unapplied, and so did a guard on `width:stretch`, which Chrome
  reads only from 138. A guard is now true from the first release that parses
  every production its declaration uses, value included, and is unwrapped only
  when true, and dropped only when false, in every release from the targets to
  the newest the compatibility data records (#1484).
- **A sheet differed from its own minified output.** This is the check the
  comparison exists for, and with 1.2.1 it failed on 139 of the 504 SatCSS
  sheets and on Tailwind's output. The comparison now treats as equal each
  rewrite below, none of which can change a computed value. All 504 sheets and
  Tailwind's output compare identical to their `--minify` output under both
  objectives (#1394, #1405, #1409, #1410, #1411, #1412, #1413, #1414, #1415,
  #1416, #1430, #1431, #1438, #1440, #1441, #1442, #1443, #1450, #1454).
  - The split of a selector's declarations across rules.
    `.a{width:1px}.b{width:2px}.c{height:3px}.a{height:4px}` equals
    `.c{height:3px}.a{height:4px;width:1px}.b{width:2px}`, and
    `.a,.b{speak:none}.a,.b{top:7px}` equals `.a,.b{speak:none;top:7px}`. Two
    declarations are now ordered only where swapping them could change a
    value.
  - Longhands against their shorthand. `top`, `right`, `bottom` and `left`
    compare equal to `inset` also when another declaration sits between them.
  - An order no element can observe. Two rules that write the same declarations
    compare equal in either order. So do a normal and an important declaration
    of one rule (CSS Cascade 5 sec. 6.1), and two rules that write one value
    through two spellings, as `-moz-transform:rotate(180deg)` and
    `-webkit-transform:rotate(180deg)` do.
  - A legacy alias against its property. The WHATWG Compatibility Standard
    makes `-webkit-flex-grow` the same property as `flex-grow`, so in
    `-webkit-flex-grow:1;flex-grow:2` the later write wins whatever the two
    values and wherever they stand, and a later `flex` writes over both. The
    comparison equated an alias only with an identical twin in the same rule,
    so it called instagram's sheet different from 1.2.1's output of it, which
    keeps `-webkit-flex-basis`, `-grow` and `-shrink` before `flex:1 150px`.
  - A declaration no target reads. A `-ms-`, `-o-` or `-khtml-` declaration,
    `font-smooth` and `font-smoothing` are dropped from both sides, and a
    prefixed copy a target needs, such as the `-webkit-hyphens` of Safari 16.4,
    is kept on both. `--enforce-spec` turns this off.
  - A property cascade did not type. `text-rendering`, `-webkit-touch-callout`
    and an untyped prefixed spelling such as `-moz-transform-origin` were taken
    to write every property, so they held apart rules that write nothing in
    common. Each now writes its own property alone.
- **A rule no browser reads withheld the verdict.** The comparison exited 2 on
  a sheet against its `--minify` output when the sheet held a rule over
  `:-o-prefocus`, `:-moz-submit-invalid`, `:selected`, a misspelt pseudo-class
  such as `:h`, or a keyframe selector such as `0` for `0%`. No target reads
  any of them, so every target drops the rule. The warning now says so. The
  comparison judges for the targets, so it does not count the loss and no
  longer exits 2 for it. Under `--enforce-spec` the reader keeps such a rule
  and the comparison reports a sheet that lost it (#1429, #1446).
- **Three respellings were reported as differences.** The comparison exited 1
  on `.a.b{color:red}` against `.b.a{color:red}`, on an `@keyframes` or
  `@font-face` rule moved among style rules, and on `@media screen{@media
  (min-width:1px){...}}` against `@media screen and (min-width:1px){...}`,
  though Chromium renders both sides alike. Each now compares equal: a
  compound's simple selectors print in one order, a name-defining rule moves
  freely among style rules while two of one name keep their order, and nested
  `@media` blocks read as the conjunction of their queries. `@media
  print{@media screen{...}}` and a negated query still compare different
  (#1537).
- equates `list-style-image: initial` with `none` and `list-style-position:
  initial` with `outside` (CSS Lists 3 secs. 3.3 and 3.5), so `list-style:
  none` matches the longhands a browser expands it to, and `--minify` writes
  `list-style-image: initial` as `none` (#1593).

#### `cascade diff` without `--diff`

- **A colour written two ways was reported as a modified rule.**
  `color:#ffffff` against `color:#fff` showed as a rule annotated
  "(declarations differ in subtle ways)". The default mode now compares
  declarations the way `--diff=tree` does. When the two files parse to the same
  stylesheet, the report says "no structural change; the text differs" above
  the text diff. It still exits 1, because the default mode counts any
  difference in the text (#1400).
- **A changed rule could go unreported.** When the other file held two
  identical copies of its counterpart, pairing one copy claimed both and the
  change was dropped. It is reported (#1297).
- shorthand against its longhands is reported per longhand.** `.a{border:1px
  solid red}` against
  `.a{border-color:blue;border-style:solid;border-width:2px}` printed `-
  border` and three `+` lines; it now prints `- border-image: none`, `*
  border-color: red -> blue` and `* border-width: 1px -> 2px` (#1555).
- shorthand against the longhands it expands to is no change.** A slot the
  shorthand omits reads as its spec initial value for `border`, a single-layer
  `background`, `flex` and a single `transition`, so `border:1px solid`
  matches
  `border-width:1px;border-style:solid;border-color:currentcolor;border-image:none`:
  `--diff=tree` exits 0 and the default mode says only that the text differs
  (#1596).
- basis `flex:1` omits reads by the mode.** By default it is `0%`, as Chrome
  computes, so `flex:1` against `flex-grow:1;flex-shrink:1;flex-basis:0%` is
  no structural change; under `--enforce-spec` it is Flexbox 1 sec. 7.1's `0`,
  so `flex:1 1 0` minifies to `flex:1` and a `0%` basis is kept as `flex:1 0%`
  (#1601).

### Warnings, errors and help

The new form of a diagnostic is under [Breaking](#command-line).

- **A noisy sheet buried its warnings.** A warning already printed for the same
  file is now counted under the first, and a last line gives the total and
  points to `-v`. netflix.com's sheet prints about 40 lines of warnings where
  it printed nearly 500. `-q` was documented and silenced nothing; it now
  prints no warning, and `-v` prints every one, in `fmt`, `apply`, `prune` and
  the `diff` report (#1332, #1341).
- **Some messages pointed at the wrong thing.** A selector the reader refuses
  was a `bad value: ...` naming no property, and is a `bad selector: ...`. A
  declaration with no value, such as `.b { color: }`, was reported one column
  past the `}` that ends it, and is reported at the `}`. A bad `@font-face`
  descriptor value pointed at line 1 of a copy of the value, and is located in
  the sheet (#1330, #1339, #1340).
- **A mistyped command was taken for a file.** `cascade fmtt a.css` complained
  about `a.css`. It now says `fmtt` is an unknown command and suggests `fmt`.
  `--flatten-nesting` without `--minify` did nothing and said nothing, and is
  now noted (#1333).
- **The help pages are written for a first reading.** They group the options by
  what they act on, give every command examples and describe the `diff` report
  and its `--json` members (#1333, #1335).
- **`diff --browser` promised more than it checks.** Its help said it reports
  every computed-style value the two files disagree on. It compares renders
  pixel for pixel and lists computed values only under pixels that differ, so
  a value no pixel shows, such as the duration of an animation that never
  runs, goes unreported. The help now says so (#1455).

### Parsing

- **A custom property's value could change on the way through.** It was printed
  and lexed again, so `--x:\!important` became a lone backslash marked
  important, and `--x:a/**/b` became `--x:a b`, another token sequence. The
  value is now the tokens the parser read. `'a'` and `"a"` stay one value
  (#1386).
- **Deep nesting crashed cascade.** Input nesting blocks or functions more than
  128 deep overflowed the stack. A value nested deeper is now kept and printed
  back as written, and a rule nested deeper loses its contents with a warning.
  A selector nesting `:is()` more than 100 deep read as an `:is()` that matches
  nothing; its rule is dropped with a warning (#1296).
- **Rules and declarations a browser drops were kept.** `[data-x=2]`, an
  attribute selector with a number as its value, was kept as `[data-x="2"]` and
  so matched elements no browser matches. In an `@keyframes` frame, an
  `!important` declaration or an `animation-*` property other than
  `animation-timing-function` stayed in the output. A `text-rendering` value
  outside `auto | optimizeSpeed | optimizeLegibility | geometricPrecision` was
  printed back. Each now goes with a warning, as browsers drop it (#1292,
  #1294, #1440).
- **A math function of the wrong type inside `calc()` was kept.**
  `opacity:calc(min(10px,1px) + max(1px,2px))` and the same length comparison
  at a time, a percentage or an angle were kept, and
  `width:calc(min(1s,2s) + max(1s,2s))` was printed as `width:3s`; browsers
  drop each one. A comparison, `abs()`, `hypot()` or a stepped-value call in a
  calculation now has to carry a unit the property takes (CSS Values 4 sec.
  10.1 and 10.9), and the declaration goes with a warning otherwise (#1524).
- **A percentage Z offset in `transform-origin` was kept.**
  `transform-origin:50% center 50%` was kept and minified into `50% 50% 50%`,
  though CSS Transforms 1 sec. 4 makes the third value a `<length>` and
  browsers drop it. It now goes with a warning (#1525).
- **A list of images in a counter-style symbol was kept.**
  `@counter-style` read `prefix:url(a.png),none` as a `background-image` list
  and kept it, though a `<symbol>` is one image (CSS Counter Styles 3 sec.
  3.1.7) and browsers drop it; the same reading dropped
  `additive-symbols:2 url(a.png),1 url(b.png)`. A symbol is now one image
  (#1526).
- **A reference box beside `none` was kept.** `clip-path:content-box none`
  and `offset-path:content-box none` were kept, though a box pairs only with a
  shape (CSS Masking 1 sec. 5.1, CSS Motion Path 1 sec. 2.1) and browsers drop
  both. Each now goes with a warning (#1529).
- **A counter-style range with its bounds reversed was kept.**
  `@counter-style` kept `range:4 2`, though CSS Counter Styles 3 sec. 3.5
  makes a range whose lower bound is above its upper one invalid and browsers
  drop the descriptor. It now goes with a warning (#1530).
- **`@charset` is read as browsers read it.** It warns and goes unless the
  sheet opens with exactly `@charset "UTF-8";`, so not after whitespace, a
  comment or an `@layer` statement (#1294).

### `--inline-vars`

- **A value could land in the wrong rule.** `--inline-vars` read `.x&` under
  `a` as the class `.xa` and folded a value defined there into a rule styling
  `a.x`. It now composes a nested rule's selector as CSS Nesting does, and a
  definition on a selector list reaches a consumer under one of its branches
  (#1352).
- **Inlining its own output folded more.** A variable that several rules define
  with one value was kept, then folded on a second run. It is folded the first
  time. One whose definitions disagree is named: `--x takes different values in
  different scopes` (#1343).
- **A `calc()` around a kept variable was dropped.** With `--keep-vars`,
  `padding:calc(var(--s) * 1)` and `width:calc(var(--s))` came out as a bare
  `var(--s)`, and `calc(calc(var(--s)) * 2)` as `calc(var(--s)*2)`. A variable
  substitutes tokens, and `calc()` is what reads them as one sum: under
  `--s:1px + 2px`, Chromium computes `width:calc(var(--s))` to 3px and
  `width:var(--s)` to `auto`, and under `--s:1px 2px` the padding goes the
  other way. `Css.Context` runtime variables did the same. The `calc()` now
  stays, as it does under `--minify`, which drops a nested one only where an
  `@property` registration makes the variable one value (#1458).

### Windows

- **A build gave other bytes on Windows.** Numbers printed differently:
  `3.40282e38px` came out as `3.40282e038px`, and `100000000000.5` as
  `100000000001`. `cascade` also wrote CRLF line endings in its output and help
  pages. Numbers print as elsewhere and lines end in LF (#1286, #1291, #1292,
  #1300).
- **Local paths were taken for remote URLs.** `--inline-imports` left Windows
  drive paths (`C:/a.css`, `C:\a.css`, `C:a.css`) and `file:` URLs alone, and
  looked a UNC path up under the importing file's directory. It inlines them
  and checks them against `--import-root`. `cascade diff --browser` starts node
  without a POSIX shell and finds it on `PATH` under `PATHEXT` (#1291).
- browser checks find Chrome on Windows:** under `USERPROFILE` when `HOME` is
  unset, in playwright's `LOCALAPPDATA` cache, and installed under
  `PROGRAMFILES` or `LOCALAPPDATA` (#1557).
- browser harnesses start node without a shell,** so they run under `cmd.exe`
  (#1558).
- fuzz tests run on Windows:** they require alcobar 0.5.0, whose timeout no
  longer needs `SIGALRM` (#1559).

### Speed

- **`--minify` takes less than half the CPU of 1.2.1** over the SatCSS corpus,
  and about a third on Tailwind's stylesheet and on tw's. Each step left the
  output byte for byte as it was (#1306, #1307, #1313, #1322, #1323, #1324,
  #1325, #1329, #1344, #1347, #1356, #1361, #1362, #1363, #1364, #1365, #1374,
  #1375, #1376, #1377, #1378, #1379, #1380, #1384, #1385, #1420, #1421, #1434,
  #1435, #1436).
- **`cascade diff --diff=canonical` takes less than a third of the CPU** on
  Tailwind's stylesheet against tw's, and less than half the memory (#1304,
  #1305, #1308, #1309, #1326, #1327, #1328, #1426, #1447, #1450).
- **`cascade diff` no longer slows with the square of the rules that moved.**
  netflix.com's stylesheet against its own `--minify` output takes 0.2 s of CPU
  instead of 1.2 s (#1297).
- **Parsing is faster,** most of all on Tailwind's output, which sits in one
  `@layer`. A sheet of `var()` fallbacks that do not match their property's
  grammar cost its size squared, and no longer does (#1311, #1314, #1315,
  #1381, #1382, #1383, #1387).
- **Printing warnings no longer costs the sheet's size per warning.** A 2 MB
  sheet prints its warnings about twenty times faster (#1310).
- **`--profile` says where a run goes.** `cascade fmt --minify --profile` opens
  with the time spent reading, optimising and printing, names its columns in
  words and prints one row per factoring run. It counts `nested-merge probes`,
  also read as `Stats.counters.nested_merge_probes` (#1301, #1336, #1372).
- diff --diff=canonical` is faster on large sheets.** Instagram's SatCSS
  sheets project in 0.11 s instead of 0.48 s and Tailwind's full build in 0.29
  s instead of 0.74 s, with the same canonical output on all 504 SatCSS sheets
  (#1554).
- composes shorthands only where two declarations share a family**, skipping
  63% of composition calls on the SatCSS corpus: about 2% less CPU and 2-5%
  fewer minor words, with byte-identical output (#1598).

### Library

- **`Css.minify`** optimises and prints in one call, as `cascade --minify`
  does. `Selector.v`, `Container.v` and `Css.stylesheet` read CSS written in
  the program (#1299, #1367).
- **`Source_index` offsets drifted by a byte per CRLF,** so a tool locating
  `@import` in a CRLF file cut statements in the wrong place, and a NUL could
  make `Source_index.v` raise. Offsets and slices index the string you passed,
  also when it starts with a BOM. `Reader.origin` and `Reader.original_offset`
  map a position in the preprocessed input back to a byte offset in that string
  (#1285).
- **`Selector.specificity` counted a prefixed pseudo-element as a class.**
  `input::-webkit-input-placeholder` weighed (0,1,1) where Selectors 4 sec. 17
  and the browsers give (0,0,2). `::-webkit-scrollbar` and `::details-content`
  were counted the same way. Each now counts with the type selectors (#1444).
- **A selector built in the program is the one the parser reads.**
  `Selector.compound`, `combine` and `reshape` build a selector in the parser's
  shape, so it equals the selector read back from its printed text (#1342).
- **`Css.Context.resolve_url`** resolves `1:/x` against the base as a relative
  path, as WHATWG URL does, since a digit cannot open a scheme (#1291).
- **`Properties.is_vendor_prefixed` and `Properties.legacy_alias`** say whether
  a property is a vendor's own extension and which property a WHATWG
  Compatibility legacy alias such as `-webkit-transform` names (#1480).
- **`Values.read_constant_number` and `Values.read_constant_integer`** read a
  literal number, or a math function that folds to a constant, rounded at an
  integer: `calc(1.5)` is `2` (#1515).
- legacy name alias is its property during evaluation.**
  `transform:revert-layer` now rolls back to a `-webkit-transform` in a lower
  layer, `transform:inherit` reads an inherited `-webkit-transform`, and
  `Context.inherited_value`, `Context.initial_value` and
  `Stylesheet.declared_values` find either spelling (CSS Cascade 5 sec. 3.1).
  Property names are read in any ASCII case (#1561).

## 1.2.1

### Parsing

- A `/*! */` comment inside a rule is discarded with the rule's other comments
  instead of being hoisted in front of it, so `cascade diff` no longer reports
  removed rules on `@tailwindcss/forms` output (#1276).

## 1.2.0

Many fixes in this release change the CSS cascade emits for input 1.1.0 already
accepted, and some are miscompiles that can change how a page renders. If you
shipped minified output built with 1.1.0, rebuild it and run `cascade diff
--diff=canonical old.css new.css`, which exits 1 and prints the difference
wherever the two are not equivalent. Library callers should start at `###
Breaking`, where the public module set and the parser entry points both moved.

### Breaking

- A calculation whose sum has no consistent type is dropped, as in `width:
  calc(sqrt(4) - 1px)` and `border-width: abs(-1)`. It was written back as text
  cascade's own reader refuses (#1151).
- A math function takes only the operands CSS grants, so `height: calc(auto)`,
  `width: min(0, 1px)` and `scale: abs(-1px)` are dropped. `width:
  calc(inherit)` printed a live `inherit` (#1139, #1144, #1153).
- `Cascade.Properties.font_weight`, `webkit_line_clamp`, `zoom`,
  `border_image_slice_item` and `shape_image_threshold` gain `Calc`, as those
  slots read a math function (#1072, #1086, #1124, #1125, #1145, #1148).
- `Cascade.Properties.line_height` gains `Min`, `Max` and `Clamp`, so
  `line-height: min(120%, 1px)` reads, and `grid_line` gains `Calc_name` (#1126,
  #1161, #1174).
- Column rules and `interest-delay` carry a comma-separated list, so
  `column-rule: 1px solid red, 2px dashed blue` reads. A caller writing one
  entry passes a one-element list (#1024, #1053, #1083).
- `animation_range`, `animation_timeline`, `timeline_axis` and `timeline_inset`
  gain list arms, the timeline shorthands lose `None`, and `timeline-scope`
  moves to `timeline_scope` (#1050, #1051, #1052, #1074).
- `Cascade.Stylesheet.font_face_descriptor` gains `_auto` arms for `font-style`,
  `font-weight` and `font-stretch: auto`. `font-weight: lighter` and the
  `font-tech` descriptor are dropped (#1116, #1117, #1118).
- `font_style_range`, `font_tech_descriptor` and `read_font_tech_descriptor` are
  gone. `tech()` inside `src` and `font-tech()` in `@supports` still read
  (#1116, #1117, #1118).
- `Cascade.Properties.tab_size` holds `Number of number` where it held `Int`, so
  `tab-size: 10.5` reads. A caller building a literal writes `Number (Num 4.)`
  (#1054).
- `dash_length`, `animation_iteration_count`, `font_weight.Weight` and the three
  `border_image_*` items carry a `number` where they carried a `float` (#1021,
  #1048).
- `flex_basis` and `border_width` gain `Dimension` for a spelling no typed
  constructor carries, as in `flex-basis: 1e3px` and `border-width: 3dvh` (#612,
  #1023).
- `vertical_align` carries one `Length of length_percentage` and
  `position_value.Axis_edge_offset` a `length_percentage`, so `vertical-align:
  1cap` reads (#673, #1070).
- `Cascade.Properties.page_size`, `page_break_value`, `page_break_inside_value`
  and `border_image_slice` carry the CSS-wide keywords, as their siblings did
  (#993, #994, #1004).
- Value types gain arms for the rest of their grammar: `content: url(a.png)`,
  `flex-wrap: balance` and `ruby-overhang: spaces` read. Exhaustive visitors
  must handle the new leaves (#995, #1013, #1018, #1019, #1020, #1049, #1088).
- `ray_size` names only the sizes `ray()` accepts, `position_try_fallbacks`
  holds typed entries and `place_items` gains the modifier-first baselines
  (#549, #725, #1011, #1012).
- The `view-timeline`, `text-box` and `contain-intrinsic-*` types carry their
  optional slots, so `text-box: normal` and `contain-intrinsic-width: auto none`
  read (#668, #669, #670, #671, #1071).
- `-webkit-mask-origin` and `-webkit-mask-clip` carry WebKit's own box
  vocabulary in place of the `mask_box` of their unprefixed namesakes (#1087).
- `Cascade.Properties.color_interpolation` carries the colour space. Its six
  `In_*` constructors are one `In` of a `color_space` and an optional hue
  method, and a gradient takes every space `color-mix()` takes (#1177).
- `flex_basis.From_font` is gone, and so are `Left`, `Right` and their `Safe_`
  and `Unsafe_` spellings on `align_content`, which printed CSS no grammar
  grants (#658, #1014).
- `Css.filter` gains `Omitted of filter_function` and `Css.Supports.t` gains
  `General_enclosed`, which keep what the reader used to discard (#869, #870).
- `Css.kind` gains `Radial_shape`, `Radial_size` and `Position_value`, and
  `Css.Stylesheet.moz_document_condition` gains `Url_exact`, `Domain`,
  `Media_document` and `Regexp` (#461, #508).
- `Css.color` keeps the origin of a relative colour as a colour. `Relative_rgb`
  carries `color * string` and `Relative_color_mix` a pair of them (#313).
- Values every browser drops are dropped with a warning, such as a colour
  outside the final background layer and `gap: -10%`.
  `Cascade.Values.read_margin_length` is new (#1096, #1097, #1101, #1108).
- A `var()` whose custom property resolves through another keeps its reference
  instead of taking its fallback, so `color: var(--x, lime)` computes the
  inherited colour as browsers do (#1100, #1102).
- `border-image` and `mask-border` fill their slots in any order, so
  `border-image: 50% none` and `round none 30` read (#1089).
- Browser support comes from the web-features dataset, so a `backdrop-filter`
  for Safari 17.0 to 17.6 keeps its `-webkit-` twin. `Cascade.Support` is new
  and `Cascade.Optimize.targets` aliases its `targets` (#1090, #1110, #1111).
- An `@supports` condition keeps the property name and value the author wrote,
  since rewriting them changed what it tested. `--minify` keeps the guard and
  does not decide it (#584, #587, #1092).
- `Css.Supports.simplify_baseline` is gone, `Cascade.Supports.pp` and
  `to_string` gain `?verbatim`, and `Cascade.Supports.Declaration` carries the
  name beside the declaration (#584, #1092, #1095).
- Implementation modules are not reachable through accidental `Cascade.*`
  aliases any more. `Cascade.Reader` is the character cursor `Lexer` drives and
  does not read CSS (#496, #497, #499, #501, #514, #539).
- `Declaration.pp` and `Declaration.to_string` replace `pp_declaration` and
  `string_of_declaration`, and `Cascade.Parser.string_of_components` replaces
  `Parser.to_string_custom` (#544, #806).
- `Container.read` and `Media.read` over a cursor replace `of_components`,
  `Css.Stylesheet.statement_declarations` replaces `Css.statement_declarations`,
  and `Css.vars_of_stylesheet` replaces `Css.vars_of_rules` (#348, #382, #509).
- The `Css.Transform`, `Transform_origin`, `Perspective_origin` and `Animation`
  string parsers are replaced by the main readers, and
  `Css.Selector_summary.clear_memo` is gone (#504, #548).
- `Css.Stylesheet.layer_name` is the identifiers of a `<layer-name>`, and
  `Css.Stylesheet.edit_statements` callbacks return `Keep`, `Replace` or `Drop`
  of the new `Css.Stylesheet.edit` type (#442).
- `Declaration.declaration` is a private variant. Its constructors still match,
  but values are built with `Declaration.v` (#527).
- `Css.Supports.property` and `Css.Declaration.custom_property` raise `Failure`
  on text that does not write back as the one declaration they name (#421, #428,
  #459).
- A helper whose CSS value needs a non-empty list raises `Invalid_argument` on
  `[]`, `Declaration.of_string` raises `Cursor.Parse_error` on every refusal,
  and `Css.Selector.of_string ""` raises `Error.Parse_error` (#528, #535, #691).
- `Cursor.pair` and `triple` rewind the cursor when the separator or a later
  parser fails, so code that caught `Parse_error` and read on sees a different
  position (#509).
- New record fields must be set in expressions and bound or skipped with `; _`
  in patterns: `recovery` on `Cascade.Error.t`, `source` on `Css.parse` and
  `in_style_rule` on `Css.Pp.ctx` (#374, #747, #834).
- `Cascade.Reader.parse_error` gains `line` and `col`, and its `filename` holds
  the source name where it packed `"<CSS input>:L:C"` (#491).
- `Css.Context.query` gains `media_inapplicable`, and
  `Tree_diff.Content_changed` gains `added_properties` and `removed_properties`
  (#723, #868).
- `Css.Media.equal` compares normalised query structure, so it can disagree with
  `Css.Media.compare a b = 0`, and `Css.Media.kind` sorts `not (min-width:
  640px)` with the upper bounds (#328, #516).
- `cascade diff` exits 2 instead of 0 when it finds no difference but dropped a
  rule it could not read. A refused declaration, which browsers drop too, does
  not withhold the verdict (#832, #833, #834, #835, #836, #1270).
- `cascade diff --depth` is gone, since a report is bounded by whole
  differences. Pass `--limit=none` where you passed `--depth=max` (#792).
- `cascade` requires `cmdliner >= 2.0.0`, the release that lets either side name
  standard input as `-` (#796).

### Parsing

- A math function reads wherever the grammar allows its type, with no `calc()`
  wrapper, so `font-weight: calc(400)`, `width: abs(-1px)` and the `@font-face`
  override descriptors read (#1145, #1148, #1164).
- A value a browser rejects is rejected whole, such as `translateX(10px red)`.
  The prefix cascade recognised used to survive (#417, #617, #627, #629, #631,
  #673, #680, #688, #699, #701, #878, #882, #890, #891).
- A property refuses what its grammar refuses, so `zoom: -1`, `font-style:
  oblique 0` and a percentage `@page size` are dropped (#982, #1001, #1002,
  #1003, #1004, #1005, #1006, #1007, #1010, #1015, #1077, #1163, #1165, #1166).
- An empty value is a declaration only for a custom property, and `color-scheme:
  light only dark` is dropped. `Cascade.Properties.read_border_image_source` is
  new (#640, #1000, #1009, #1078, #1169).
- A `<custom-ident>` refuses the reserved names in any case. `animation-name:
  default` is dropped with a warning and `animation-name: "default"` still reads
  (#1143).
- A value list takes the items its grammar grants: `mask: none, none` and
  `font-family: Cambria Math` read, and `box-shadow: none, none` and
  `font-family: serif serif` are dropped (#1147, #1167, #1171, #1173, #1176).
- `@keyframes "default"` keeps its quotes, where it printed as a `@keyframes
  default` browsers drop, and `@keyframes "none"` reads. `color-scheme: default`
  is dropped with a warning (#1146).
- A `<length>` slot refuses a percentage and a sizing keyword, and a `<time>` or
  `<angle>` needs its unit, so `border-width: 50%` and `rotate: 0` are dropped
  (#871, #879, #880, #951, #952, #953, #954, #1056, #1058, #1059).
- Selectors and at-rule conditions browsers accept are read where they were
  dropped, such as the `svg||td` column combinator, `:dir(auto)` and a `style()`
  range query (#335, #427, #461, #551, #572, #594, #805).
- `white-space: collapse`, `animation-duration: auto` and other text, list and
  animation values browsers accept are read (#646, #665, #666, #682, #874, #877,
  #885, #886, #889, #995, #998, #1022).
- Layout values browsers accept are read too, such as `grid: none / 200px`,
  `place-items: flex-start baseline`, `row-gap: normal` and `outline: thin solid
  red` (#456, #633, #739, #881, #884, #909, #1055, #1122).
- An empty `blur()` reads, and so does a CSS-wide keyword at the `page-break-*`
  aliases and `border-image-slice` (#334, #579, #644, #667, #827, #870, #887,
  #993, #994, #1060, #1085).
- A value is checked against the range its specification gives:
  `offset-distance: -10px` and `z-index: calc(.5)` read, and a math call keeps
  its wrapper under `--minify` (#996, #999, #1026, #1047, #1080, #1082, #1123).
- Each grid property reads its own grammar. `grid-template-columns: [a]` and
  `grid-column-start: 0` are dropped, and a track size accepts math resolving to
  `<flex>` (#708, #711, #712, #714, #717, #718, #724, #749, #1016, #1076).
- `font-variant` is typed, the `font` shorthand reads all nine width keywords,
  and a bad `src` item no longer costs the whole `@font-face` (#695, #963, #964,
  #965, #972, #1115).
- `@counter-style` descriptors are checked against their grammars. `range:
  bogus` and `pad: 3` are dropped, and a counter symbol may be an image (#1130).
- An `@property` rule is dropped when its `syntax` cannot be honoured or its
  `initial-value` holds a `var()`, `attr()` or `env()`. Two syntaxes that hung
  the parser read (#398, #707, #710, #713, #1142).
- An `@property` `initial-value` refuses a length that needs an element, such as
  `3em`, under any syntax but the universal one, which stores the value as
  written (#1179).
- A bad nested rule, a stray `;` or leftover tokens after a value are dropped on
  their own and the style rule around them survives (#374, #380, #384, #388,
  #392, #727, #895, #897, #974).
- A bad descriptor, margin at-rule or `@scope` bound is dropped on its own, and
  the at-rule around it keeps the rest of its block (#399, #402, #403, #404,
  #405, #419, #420, #946, #948, #949).
- An unrecognised at-rule keeps its block, an unknown `@supports` or media
  condition keeps its guard, and `Optimize.drop_unknown_at_rules` drops such
  rules on request (#469, #483, #867, #869, #947, #977).
- A selector no browser can match is dropped: `. x a` used to read as `.x a`.
  `Css.Selector.of_string` refuses a string that is not a selector (#418, #426,
  #430, #441, #552, #553, #556, #559, #950, #976, #982).
- A `var()`, `env()` or `attr()` is kept inside another function as it is at the
  top level. A value with an unmatched `)`, `]` or `}` is dropped with a warning
  (#511, #518, #726, #729, #734, #735, #787, #813, #978, #997).
- A `var()` the evaluator cannot resolve keeps its reference instead of taking
  its fallback, and `--inline-vars` keeps it live unless its definition provably
  applies (#985, #986, #987, #988, #989, #990, #991, #992).
- Keyword, at-rule and function names match in any case, so `@MEDIA` and `RGB()`
  read. An escape reads as what it spells, and `@layer a\2e b` names `a.b`
  (#437, #442, #602, #603, #604, #620, #622, #767, #1141, #1162).
- Cascade reads back what it writes. Minified `@scope to (...)`, a negative
  `rotate` axis, a relative colour's channels and a `-webkit-gradient` survive
  the round trip (#656, #898, #899, #900, #902, #903).
- An unterminated string, a keyword-shaped keyframe name, an at-rule body ending
  on a backslash and a `url(` at end of input survive the round trip too (#558,
  #875, #876, #894, #896, #901, #910, #911, #912, #913, #1155).
- A relative colour's channel expressions are type-checked, so `rgb(from red
  calc(r + 10%) g b)` is dropped as browsers drop it, and a percentage `oklab()`
  lightness clamps (#904, #905, #1061).
- The `offset` and `position-*` properties and `white-space` read their own
  grammars, so a built and a parsed value compare equal (#478, #485, #495, #651,
  #652, #653, #654, #657, #674, #683, #888, #935).
- An input that ends mid-value keeps its declaration, where it swallowed the
  rest of the sheet, and `--x: 1 ! important` is important with the value `1`
  (#972, #973, #979).
- Everything the parser repairs or drops is reported, so strict mode rejects it.
  `@media screen {` swallowed the rest of the file and returned `Ok` with no
  warning (#466, #472, #473, #477, #484, #538, #789, #793, #801).
- A parse error points at the value the author wrote, and `Cascade.Reader.int`
  raises `Parse_error` on a fractional or out-of-range number instead of
  truncating (#496, #497, #499, #501).

### Printing

- `text-decoration`, `mask-border`, `animation` and `mask` never minify to an
  empty value such as `text-decoration:`, which no parser reads back (#682,
  #955).
- An identifier prints with the escapes it needs to read back. `@layer a\3b b`
  printed two statements and `--x\3b y` two declarations (#435, #436, #526,
  #598).
- A compound operand in a `@media` condition keeps its parentheses, and
  unwrapping a `@supports` nested in a style rule keeps the `;` before the next
  sibling (#319, #370).
- A relative colour's channel list and a `rotate` axis after a `var()` angle
  print in a form that reads back as the same sheet (#1189).
- A comparison function's operand prints as written: `border-width:
  min(var(--x), 1px)` gains no `calc()` around the reference (#1175).
- A `calc()` printed without `--minify` keeps the parentheses the author wrote,
  and `--minify` keeps only the ones precedence needs (#721).
- A NaN prints as `calc(NaN)` or `calc(NaN * 1unit)`. `calc(sqrt(-1) * 1px)`
  printed `NaNpx`, which browsers drop (#425).
- `Css.to_string` renders the sheet once instead of twice (#479).

### Minification

- A colour folds only to the bytes a browser paints: `color(display-p3 .5 .5
  .5)` is not folded to `gray`, and a colour outside the sRGB gamut keeps its
  spelling (#1272).
- A relative colour whose channels are its own space's keywords in order folds
  to its origin, so `oklab(from rgb(0 0 0 / .1) l a b / 20%)` minifies to
  `#0003` (#1248).
- `calc(.5 * 100%)` in `flex-basis` folds to `50%` and `calc(infinity * 1px)` to
  `3.40282e38px`, so canonical diff reads Tailwind's spelling and lightningcss's
  as one (#1244, #1245).
- A pixel `stroke-width` minifies unitless, `stroke-width:1px` to
  `stroke-width:1`, and a signed number in a token stream drops its leading
  zero, `-0.5` to `-.5` (#1242, #1243).
- A registered `<length>` zero minifies to `0` in an `@property`
  `initial-value`. Canonical diff reported two equivalent registrations as
  different (#1237).
- Adjacent `@media` blocks with one condition merge even when a body holds a
  `prefers-color-scheme`, `prefers-contrast` or `prefers-reduced-motion` query
  (#1233).
- A shorthand holding a `var()` keeps every component, where `list-style: disc
  outside var(--x)` became `list-style: var(--x)`. Rebuild minified output with
  a `var()` in `list-style` or `text-decoration` (#1194).
- A math call a property's range keeps folds its arithmetic first: `width:
  calc(-5px - 5px)` prints `calc(-10px)` (#1181).
- `round()` sends a tie toward positive infinity. `round(-3, 2)` folds to `-2`
  where it folded to `-4` (#1182).
- `sign()` folds only when its argument's sign is settled: `calc(10px *
  sign(-1em))` keeps its call and `sign(-1px)` still folds (#1180).
- One `--minify` pass reaches its final output where a second pass changed it
  again, so `a{width:1.0px}b{width:1px}` merges at once (#1150, #1158, #1159,
  #1160, #1178, #1185, #1186, #1187, #1188, #1190).
- `--minify` contracts longhands into the box, border, `border-radius`,
  `border-image`, `background`, `mask`, `columns` and grid shorthands (#915,
  #916, #918, #920, #921, #922, #926, #927, #933, #935, #939, #942, #960, #961).
- The `offset`, `text-decoration`, `flex-flow`, timeline, `container`,
  `white-space`, `text-wrap` and `font` shorthands contract too (#923, #924,
  #925, #928, #929, #930, #931, #932, #937, #965).
- A component at its initial value is the slot its shorthand leaves out, so
  `flex: 0 0 auto` minifies to `flex: none`, and a repeated side folds to the
  shortest spelling (#917, #919, #934, #940, #959).
- A longhand that restates the shorthand before it is dropped. A border family
  written out in full minifies to `border: 1px solid red` (#956).
- Contraction resets no longhand the run did not write, so contracting a partial
  `transition` or `animation` run needs `--scope=stylesheet` (#843, #845, #914,
  #957, #958).
- Contraction skips a run holding a CSS-wide keyword, which turned four paddings
  into a `padding` browsers drop, and keeps a neighbour's `border-image` (#943,
  #968).
- `Css.Properties` gains `background-position-x` and `-y`, the `column-rule`
  width and style longhands, `white-space-collapse`, `column-height` and
  `column-wrap` (#926, #927, #928, #936, #938, #941).
- A value keeps every digit and unit the author wrote unless a shorter spelling
  is the same number, so `.4285714em` keeps its precision (#350, #354, #362,
  #367, #676, #685, #731).
- A `calc()` below zero keeps its call where the range starts at zero. `width:
  calc(-10px)` was folded to `-10px`, which browsers drop (#967, #1149).
- A sum leads with a positive term, so `calc(-10px + 100vw)` minifies to
  `calc(100vw - 10px)` (#1113).
- Rules that differ only in how they spell a hex colour, a NaN, an unreduced
  `min()`, a wide-gamut `color-mix()` or a same-unit `calc()` merge under
  `--minify` (#471, #482, #597, #618, #624, #647, #678, #755, #756).
- So do rules that differ by a shorthand component at its initial value, a
  repeated `font-family` entry, a logical minimum size or `steps(1)` (#635,
  #636, #637, #639, #640, #641, #650, #672, #675, #681).
- `--minify` keeps a value whose shorter spelling would mean something else,
  such as `display: block ruby` and `transition: opacity 0s 2s` (#387, #390,
  #401, #457, #470, #547, #633, #634, #637, #736, #766, #782, #803).
- `--minify` drops the space at a `%` or `)` boundary in the box shorthands and
  `inset()`, and prints a custom property as the author's token stream (#614,
  #619, #623, #697, #700, #703, #709, #719, #720, #722).
- A selector rewrite that changes what an element matches is gone:
  `.c:not(:enabled)`, a single-argument `:is()` and a double `:not()` keep their
  form (#377, #431, #593, #596, #655).
- `@media` and `@container` blocks merge by query structure, adjacent
  `@starting-style` runs merge, and `@media not all and (X)` minifies to `@media
  not (X)` (#323, #465, #516, #519, #585, #592, #809).
- A declaration keeps its place when moving it would change what an element
  computes (#352, #364, #376, #383, #386, #414, #415, #447, #452, #453, #454,
  #742).
- `--minify` optimises inside every at-rule with a body, `@-moz-document`,
  `@starting-style`, `@when` and `@else` included (#341, #343, #344, #349, #372,
  #374, #389, #396, #560).
- Flattening emits only selectors a browser can match. A rule under a
  pseudo-element is dropped, and `a { .x& { } }` becomes `.x:is(a)` where it
  became `.xa` (#759, #800, #818, #822, #823, #975).
- Default minification adds the WebKit fallbacks Safari 16.4 and Chrome 111
  need, and drops a vendor prefix only when its unprefixed twin is Baseline
  widely available (#325, #378, #447, #751, #758, #797, #1256).
- `Css.Resolve` matches an attribute selector as an HTML document does, folding
  names and the values of case-insensitive HTML attributes to lowercase (#944,
  #945).
- Computed-value evaluation resolves `inherit`, `initial`, `unset`, `revert` and
  `revert-layer` for every typed property (#763, #764).
- `--minify` is several times faster on a large stylesheet for the same output.
  The worst sheet of the SatCSS corpus drops from 3.5s to 0.6s (#1183, #1184).
- A long run of rules sharing one selector does not allocate quadratically under
  `--minify` (#413, #422, #468, #480, #486, #487, #493, #502, #505, #507, #523).
- `cascade diff` is faster on a large stylesheet too, and a 4,000 statement
  sheet is linear (#424, #517, #519, #542, #543, #566, #664, #746, #750).

### Custom properties

- `Css.inline_vars ~inline_runtime:true` inlines a reference marked `~runtime`
  when the sheet defines its variable once, as tw's `--inline` needs (#1238).
- A custom property's value, a container or media query and an `@supports`
  condition keep the whitespace and escapes the author wrote (#1064, #1065,
  #1066, #1067).
- `Cascade.Token.t` gains `repr`, `Cascade.Token.Whitespace` carries its run,
  and `Cascade.Parser.to_string_verbatim` writes the authored text back (#1064,
  #1065, #1066, #1067).
- `Css.inline_vars` and `Css.custom_props` reach a `var()` in every at-rule,
  descriptor and nested rule (#322, #341, #342, #375, #423, #555, #571, #573,
  #575, #577, #1048).
- `Css.inline_vars` resolves a property defined across cascade layers by the
  sheet's layer order, and keeps the `@property` rule of a custom property it
  leaves live (#357, #371, #373, #416).
- Substitution keeps a custom property's layer and metadata, a `page-break-*`
  declaration and a runtime marker, and reports an overridden variable through
  `~warn` (#315, #506, #520).
- `@scope { color: green }` reads, and an at-rule inside a keyframe or
  descriptor block costs only itself, where it cost the rest of the block
  (#1068, #1069).
- `--minify` keeps the quotes on a `<string>` in a custom property whose
  `@property` syntax takes only an ident, and the spaces of a `<type>+` initial
  value (#626, #704).
- A custom property holding only whitespace holds the empty value: `--x: `
  minifies to `--x:`, and `Css.Declaration.custom_property` trims to match
  (#1157).
- `Css.resolve_theme` covers declarations in `@keyframes`, `@page`,
  `@position-try` and `@supports`, and binds a name that needs escaping instead
  of refusing it (#317, #324, #327, #421, #439).
- `Css.inline_vars` is linear in at-rule nesting depth, and a 12,800-variable
  sheet is no longer quadratic in the variable count (#481, #568, #569).
- `Css.Variables.read_reference_body` reads a `var()` argument list into a typed
  handle, and `typed_custom_property` writes a declaration from an
  `@property`-typed value (#626, #630, #642).

### Canonical diff

- Two sheets that set the same layer order in the same places compare equal,
  whether they write it as one `@layer a, b;`, as per-layer statements or by
  block order (#1262, #1275).
- An `@supports` block whose guard no browser can answer yes, such as Tailwind's
  `@supports (@media(width>=1px): var(--tw))`, compares as nothing.
  `Supports.never_holds` is the test (#1270).
- A `color-mix()` or relative colour over `light-dark()` is the `light-dark()`
  of that operation on each branch, so `color-mix(in oklab, light-dark(red,
  #00f) 50%, #0000)` equals `light-dark(#ff000080, #0000ff80)` (#1270).
- An angle compares as degrees, so `.5turn` and `180deg` are one angle. A colour
  function in a custom property reads a missing axis as zero, so `oklab(0% none
  none/.5)` equals `#00000080` (#1270).
- `--diff=canonical` is several times faster on a large stylesheet. Comparing
  tw's site sheet with Tailwind's compiled one drops from 22s to 5.5s of CPU
  (#1267).
- A move is reported only when the moved rule conflicts with a statement it
  crossed. Swapping rules under `@media (X)` and `@media not (X)` is not a move
  (#1246, #1263).
- Rules grouped into different `@media`, `@supports` or `@container` blocks, or
  a named `@layer` block placed elsewhere among unlayered rules, are not a
  difference (#1259).
- A nested `&:where(.dark, .dark *)` under `svg *` flattens to `svg
  :where(.dark, .dark *)`. The pair was reported as a selector change (#1257).
- `--diff=canonical` judges for the evergreen browsers `--minify` targets, so a
  guard, prefix or colour fallback they all treat alike is no difference.
  `Css.optimize` gains `?judge` (#1256).
- A non-terminating quotient compares to six significant figures, so `calc(1/3 *
  100%)` and `33.3333%` are one width. `Values.calc_ctx` gains a `budget` field
  (#1255).
- A declaration a `@media`, `@supports` or `@container` block repeats from the
  rule with the same selector just before it is not a difference (#1254).
- Two conflicting declarations swapped inside one rule are reported as that
  rule's reorder, where the report fell back to a byte diff of the two forms
  (#1251).
- A negated range bound equals the opposite bound: `not (width >= V)` and
  `(width < V)` compare equal, and `--minify` writes the shorter (#1250).
- A `<time>` in an unregistered custom property compares as its duration:
  `--d:.1s` and `--d:100ms` are equal (#1247).
- Declarations in a `@keyframes` frame compare in canonical order, as a style
  rule's do (#1241).
- Nested `@media` blocks compare by the conditions around each rule, so two
  nesting orders of the same blocks are equal (#1240).
- A `var()` fallback compares as the typed value it substitutes into, a shadow's
  colour slot included, so `rgb(0 0 0 / .1)` and `rgb(0 0 0/.1)` match there
  (#1231, #1232).
- An `@container` `style()` or `scroll-state()` query compares the same in any
  case (#1226).
- A legacy `-webkit-` alias the WHATWG Compatibility Standard names reads as its
  unprefixed twin, so `a{-webkit-transform:none;transform:none}` equals
  `a{transform:none}` (#1200).
- Rewrites that cannot change what a browser computes are equal, such as a
  shorthand against its four side longhands and `calc(28/14)` against `2` (#475,
  #655, #753, #756, #775, #776, #777, #842).
- A colour compares as a colour wherever it is written, a `none` channel and a
  relative colour's origin included, and so does a one-word font family (#312,
  #313, #314, #440, #696, #705, #847).
- Only reorderings present in the inputs are reported. A changed declaration
  used to make a neighbour look moved (#779, #780, #781, #819, #825, #826,
  #831).
- A nested sheet and its flat form are identical. Nesting is flattened and the
  rules inside every at-rule block are normalised before comparing (#393, #529,
  #576, #760).
- `--diff=tree` compares values on their minified spelling, prints an added or
  removed rule's declarations, and pairs a repeated selector by its properties
  (#702, #706, #752).
- `cascade diff` does not abort when a reordered selector has the same rule
  index on both sides (#582).

### Library

- `Browser_compare.run` in `cascade.browser` runs the comparison behind `cascade
  diff --browser` on two sheets, and `Browser_compare.identical` is its verdict
  (#1260, #1268, #1274).
- `Css.add_var_fallbacks` gives each `var()` with no fallback the value a lookup
  answers, so a sheet whose tokens are declared elsewhere still resolves them
  (#1236).
- `Css.to_string` and `Css.to_buffer` take `?rename_custom_property`, one
  function that renames each custom property where it is declared and in every
  `var()` (#1235).
- `Cascade.Source_index` locates each at-rule and function token by byte offset,
  so a tool can splice the author's bytes at boundaries the parser supplies
  (#1234).
- `Css` builds every property the AST models bar `Unknown_property` and the
  `@font-face` `src`, adding the SVG, anchor positioning, view transition and
  motion path families and `Css.all` (#1027, #1028, #1029, #1030, #1031).
- `Css` builds the scroll-driven animation, border-image, multi-column,
  typography, vendor-prefixed and shape families, each with its value types
  (#1032, #1033, #1034, #1035, #1036, #1037, #1038, #1039, #1040).
- `Css.Stylesheet` folds, iterates and edits every statement and declaration a
  sheet holds, and `Css.map` and `Css.sort` reach rules inside every group
  at-rule (#317, #337, #355, #356, #363, #368, #381, #382, #384, #389).
- `Css.Resolve` answers `:nth-child(... of S)`, `:nth-of-type()`, `:has()`, the
  attribute case flags and `:empty`, and ranks a layer's own rules after its
  sublayers (#607, #873).
- `Resolve.prepare` and `Resolve.Make.resolve_prepared` split the sheet-only
  work out of `resolve`, so a caller walking a document pays it once (#394,
  #567).
- `Css.Context.matches_media` handles zero-valued boolean features and keeps
  unknown through negation, and `matches_container` requires every queried
  feature (#868).
- `Css.equal_statement` and `Css.hash_statement` compare and key a statement
  without printing it, and `Css.Properties.compare_property` is a total order
  (#513, #595).
- New helpers include `Css.unknown_at_rule`, `Css.Declaration.value_of`,
  `Css.Values.gamut_map_color`, `Properties.read_filter_function` and the
  statement-merging passes (#591, #592, #600, #616, #626, #717, #872).
- `Cascade_diff.Tree_diff.has_container_added_of_type` and its `removed` twin
  look inside a whole container, and `Cascade.Error.to_string` prints valid
  UTF-8 (#395, #472).
- `cascade` drops its `uutf` dependency and the library does not link `unix`.
  `mtime` reads the clock and ships a js_of_ocaml implementation (#609, #788).

### CLI tools

- `cascade diff --browser --html PAGE.html A.css B.css` renders both sheets over
  the page in headless Chromium, at each width and state either names, and
  compares the pixels (#1252, #1265, #1266, #1268).
- `cascade diff --browser` lists the computed styles under the pixels that
  differ, writes the report with `--json`, and exits 2 when it could not render
  (#1252, #1265, #1266, #1268).
- `cascade prune PAGE.html... STYLE.css` removes the rules a set of HTML
  documents cannot use, and `--dry-run` reports instead of writing (#605, #863).
- `cascade fmt --import-root DIR` keeps `--inline-imports` reads under that
  directory (#744).
- `cascade diff` reads a side from standard input when it is `-`, writes JSON
  with `--json`, and bounds a report by whole differences with `--limit` (#792,
  #796, #799).
- A `cascade diff` report counts each difference once, in the expected side's
  order. It shows what changed inside a block or a `@keyframes` frame (#345,
  #385, #389, #474, #580, #581, #783, #784, #794, #798, #814, #815, #906, #907).
- A `cascade diff` hunk escapes a byte with no glyph, a warning both inputs
  raise prints once, and a sheet where every rule changed runs near-linearly
  (#785, #786, #790, #791, #795).
- `cascade apply` reads `style` attributes in source order, empties the
  `<style>` blocks it projects, keeps a page's comments and keeps a declaration
  whose longhand a kept rule also writes (#326, #329, #332, #339, #340, #346).
- `cascade fmt` exits 1 when parse recovery left no statement, and `cascade
  apply` exits 0 when a `<style>` block parses to at least one statement (#489,
  #494).
- CLI help lists each option and exit status once and says what `--enforce-spec`
  gates, and `--profile` without `--minify` prints no empty report (#611, #625,
  #628, #740).

### Testing

- The render harnesses under `test/render` require a sheet and each transform of
  it to paint the same page, judged by the raster through `Browser_compare.run`
  (#1269).
- A sweep renders every pair `--diff=canonical` calls identical in headless
  Chrome and fails on any that paints differently.

## 1.1.0

### Breaking

- `Css.hex` raises `Invalid_argument` on a malformed hex string instead of
  returning opaque black, and `Css.hex_opt` returns an option (#232).
- `cascade fmt` and `cascade diff` drop `--memtrace`, and the library drops the
  `memtrace` dependency (#237).
- A parse failure that drops every rule makes `cascade fmt` exit 1 instead of
  writing an empty stylesheet (#273).
- `cascade diff --diff=canonical` exits 1 and prints the difference whenever the
  canonical forms differ, and `Css_compare.No_diff` does not carry the two forms
  any more (#290).
- `Tree_diff.t` gains `layer_order`, so record expressions must set it and
  record patterns must bind it or use `; _` (#295).
- Optimizer profiling is per run. `Css.optimize` and `Optimize.stylesheet` take
  `?stats:Stats.t`, and `Optimize.counters`, `pass_times`, `iteration_stats` and
  `set_profile` are gone (#288).
- `Resolve.NODE` gains `text_children`, the data of a node's direct text
  children, which `:empty` needs (#286).
- `Apply.Make(...).compute` takes `~sheet:Stylesheet.t` instead of
  `~css:string`, so the caller parses, keeps the warnings and can tell invalid
  CSS from empty CSS.
- `Apply.result.kept` counts every rule inside a block at-rule, where a `@media`
  holding three rules counted one (#287).
- `cascade apply` exits 1 and keeps the block when a `<style>` block or the
  supplementary stylesheet parses to nothing, where it deleted the block and
  exited 0 (#287).

### Parsing

- `Reader.peek_utf8_at` returns `None` for a negative or out-of-range offset
  instead of indexing outside the input (#308).
- URL references use RFC 3986 resolution, keeping data, scheme-relative,
  query-only and fragment-only URLs (#304).
- Container conditions and media queries parse component values directly, so
  strings, escapes and general-enclosed values keep their CSS meaning (#305,
  #306).
- Typed `-webkit-` and `-moz-` aliases use their standard value grammar.
- `shape-outside` reads its whole grammar, so `margin-box`, `url(shape.png)` and
  every basic shape read where the declaration was dropped.
- An identifier takes any code point at or above U+0080, so a class name holding
  one parses. `--enforce-spec` restricts identifiers to the CSS Syntax 3 ranges
  (#254).
- `perspective: none`, `text-underline-offset: auto` and a negative
  `text-underline-offset` read where they were dropped with a warning (#212).
- A nested rule whose selector starts with an identifier, such as
  `h2:where(...)`, reads, and an unrecognised media query such as
  `theme(static)` is kept as never-matching (#192, #193).
- The `scroll-margin` properties take a negative length, as CSS Scroll Snap 1
  allows (#280).
- A `}` ends a declaration value in `Parser.block_contents` instead of
  swallowing the block, and a bad string prints back as one (#284).

### Printing and nesting

- `background-position` and `mask-position` print one position per layer,
  comma-separated (#209).
- A non-integer above roughly 4.6e10 prints as a number, where it printed as
  `scale(1.2345678.9012e+19)` (#265).
- An unknown at-rule keeps the space before its prelude under `--minify`, where
  `@foo bar {x:1}` printed `@foobar{x:1}` (#272).
- Flattening distributes the parent over every branch of a nested selector list:
  `.p { a, b { ... } }` is `.p a, .p b` (#205).
- Substituting `&` wraps a complex parent in `:is()`: `.a .b { .dark & { ... }
  }` flattens to `.dark :is(.a .b)` (#194).

### Minification

- `--lossless` preserves logical sizing and corner aliases in source order.
- A hex colour folds to its name whenever the name is not longer, so `#ffe4c4`
  becomes `bisque`, and `Css.Values.read_color_name` reads all 148 names (#289).
- `--lossless` keeps a longhand in source order against the shorthand that
  resets it, and a flow-relative property against a physical one of its family
  (#267, #270, #277).
- `--lossless` keeps a colour's alpha exact, where `oklch(... / .74567)` printed
  `/.746` (#278).
- A vendor-prefixed value repeated with `!important` collapses to the later
  declaration, so `display:-webkit-box;display:-webkit-box!important` keeps one.
- A selector list mixing a vendor pseudo-element with ordinary selectors is
  split, and a rule with nested children absorbs a later disjoint rule with its
  selector (#203).
- SVG presentation properties are typed and minify like any other value, so
  `paint-order: stroke fill markers` becomes `stroke` (#214, #228, #234, #235,
  #236, #240, #241, #242).
- More values fold to an exact equivalent, among them `grid-auto-flow: row
  dense` to `dense`, `hue-rotate(0rad)`, `font-stretch` keywords and same-colour
  gradient stops (#206, #214, #229, #230, #257).
- `--log=cascade.factor:debug` reports the optimizer's factoring decisions
  (#239).
- `--minify` is faster on a large stylesheet, for the same output (#221).

### Custom properties and `@layer`

- `Css.inline_vars` resolves `var()` across `@layer` boundaries, so a layered
  stylesheet inlines like its unlayered form (#187, #189).
- `cascade apply` projects rules inside `@layer` onto elements, where a fully
  layered stylesheet such as Tailwind v4 output inlined nothing (#188).
- `cascade apply` weighs cascade layers: an unlayered declaration beats a
  layered one, and `!important` reverses that (#283).
- `cascade apply` keeps a declaration in the stylesheet when a rule inside
  `@scope`, `@starting-style`, `@when`, `@else` or `@-moz-document` sets the
  same property (#286).
- `cascade apply` inlines only a rule whose selector its matcher can represent,
  and `:empty` counts an element's text (#286).
- `Css.vars_of_declarations` reports the `var()` references of 39 more
  properties, so `Css.resolve_theme` binds `inline-size: var(--w)` as it does
  `width` (#266).

### Canonical diff

- `--diff=canonical` expands selector-list rules, folds conditional blocks
  sharing a condition, reads `@media not all and (X)` as `@media not (X)` and
  keeps the last `@property` of a name (#204, #211, #227, #231).
- Canonical mode skips the rule-regrouping passes, which depend on input order,
  and `Css.optimize` takes `?regroup` to turn them off (#215, #224).
- Canonical mode normalises the space after a top-level comma in a custom
  property and drops a declaration a later rule with the same selector repeats
  (#206).
- A custom property keeps its `!important`, layer and metadata, so two sheets
  that disagree about the flag differ (#271).
- A quoted multi-word font name in a custom property equals its unquoted form,
  and under `--lossless` `color(srgb 1 0 0)` equals `rgb(255 0 0)` (#289, #290).

### Diff report

- Grouping repeated selectors and conditions is linear in the number of rules
  and keeps source order (#307).
- The layer order the two sheets declare is compared, and the report names the
  layer pairs that swapped.
- Every entry names what it is about, `@property`, `@keyframes`, `@charset`,
  `@namespace` and `@layer a, b;` included.
- A container that changed places is reported whether or not its body changed,
  where swapping an `@media` with the rule below it read as identical.
- A rule that writes one property more than once is compared occurrence by
  occurrence, and containers are compared however deep they nest (#285).
- A container entry with nothing under it reads as `(modified, no details)`, and
  a comparison that classified nothing says so instead of claiming there are no
  differences (#285).
- A canonical-form difference the structural comparison missed prints as a
  string diff headed by the two file names (#290).
- A declaration reorder is reported inside `@media`, `@layer` and `@supports`,
  and selector-less at-rules such as `@page` are compared on their bodies (#268,
  #269).
- An `@property` entry names the descriptors that differ, a rule counts as
  reordered only when it moved against another rule, and the size summary
  follows the headers (#261, #263, #264).
- A selector written by several rules is reported once, and an entry names the
  move when its declarations are spread differently (#259, #260).
- Blocks sharing a condition are paired one for one: three `@container` blocks
  against two report the removed block (#258).
- Every statement of an added or removed container is reported, and a container
  no longer counts again as a rule (#253).
- `cascade diff` bounds its report to the deepest level that fits, prints parse
  warnings above it, and reports blocks that only moved as a shift run (#210).

### New properties and values

- The logical border properties are complete: `Css.border_block_color` and its
  siblings, the `border_inline_width` and `border_block_width` shorthands, and
  the start and end style longhands (#197, #198, #199, #200).
- `Css.parse_font_family`, `Css.parse_list_style_type` and
  `Css.parse_list_style_image` are new, the single-value readers behind the
  `font` and `list-style` shorthands (#201, #202).
- `Css.Values.oklch_none_hue` is new, for an achromatic colour with a missing
  hue, printed as `oklch(55.6% 0 none)` (#190).

### Testing

- CI rejects merlint findings and incomplete library record patterns (#310).
- `dune test` renders a stylesheet and its optimised forms in a headless browser
  and compares every element's computed style, skipping where no browser is
  installed (#275).

## 1.0.0

First public release. Cascade was extracted from the
[tw](https://github.com/samoht/tw) (Tailwind CSS v4 in OCaml) project as a
standalone CSS command-line tool and library.

### Library

- A typed CSS AST: selectors, declarations, values, statements and stylesheets
  are sealed ADTs, so an invalid construction is a compile error.
- `Css.of_string` runs CSS Syntax Level 3 recovery and returns the sheet with
  its warnings, `~strict:true` turns the first warning into an error, and
  `Css.of_string_exn` raises.
- A pretty-printer with pretty and minified modes (`Css.to_string ?minify`), and
  typed printers such as `pp_color` and `pp_length`.
- Structural transforms (`fold`, `map`, `sort`, `flatten_nesting`,
  `inline_imports`), and a structural CSS diff in the `cascade.diff`
  sub-library.
- An optimizer that deduplicates, merges rules, combines selectors and folds
  shorthands, reaching the same output whatever the source order of cascade-safe
  rules.
- Minification optimises estimated gzip size by default. ``~objective:`Raw``
  (`--objective=raw`) optimises raw bytes for output that ships uncompressed.
- Coverage of Selectors 4, Values and Units 4, Color 4 with 15 colour spaces,
  Conditional Rules 3 to 5, Cascade 5, Custom Properties 1, Fonts 4 and
  Animations 1.
- Over 400 typed properties, from the box model, flexbox and grid to anchor
  positioning, view transitions and the common vendor-prefixed properties.
- Custom-property support: typed `<syntax>` parsing for `@property`,
  substitution against an explicit `Css.Context.t`, and `var()` output that
  round-trips.

### CLI tools

- CLI commands share one binary file reader that closes its descriptor when a
  read fails (#309).
- `cascade` pretty-prints and minifies CSS files, reading standard input for `-`
  or no argument and writing to standard output.
- `cascade --minify` applies the safe transforms, among them deduplication, rule
  merging and nesting flattening, optimising estimated gzip size by default.
- `cascade --inline-imports` resolves local `@import` rules, `cascade
  --inline-vars` substitutes static custom properties, and `--keep-vars=NAMES`
  keeps some.
- `cascade diff` compares two files in `auto`, `tree`, `string` or `canonical`
  mode and exits 1 when they differ. `canonical` reads equivalent factorings as
  identical.
- The CLI installs as a binary from the Homebrew tap `samoht/tap/cascade`, and
  through opam.

### Notes

- `cascade` parses already-decoded UTF-8 strings. Byte-stream decoding (BOM,
  `@charset` sniffing, HTTP charset) is the caller's job.
- CSS nesting round-trips through the parser and printer, and the minifier
  flattens nested rules when safe.
- `@import` rules are kept by default, and `--inline-imports` inlines them from
  the filesystem.
- No source-map support.
