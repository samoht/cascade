## 1.3.0 (unreleased)

This release fixes minified output that rendered differently from its input,
and makes cascade cheaper to run.

- **Rendering fixes.** `--minify` turned `p:not(:not(.a .b))` into another
  selector and `env(--x 2)` into another variable. It dropped the name from
  `animation:"none" 1s` and could change the sign of a very large number. It
  could also move a rule past one that styles the same element.
  `--minify --scope=stylesheet` could drop a `background` image or a
  `font-weight`. If you ship output minified by 1.2.1, upgrade and minify
  again.
- **A diff you can gate on.** `cascade diff --diff=canonical` called two sheets
  identical when one lost a rule inside `@media` to a parse error. It now
  exits 2, as it does for a rule lost inside `@supports`.
- **Speed.** `--minify` is more than twice as fast as 1.2.1, over the 504
  sheets of the SatCSS corpus and on Tailwind's output. `cascade diff
  --diff=canonical` of Tailwind's stylesheet against tw's is about five times
  cheaper and uses less than half the memory. Parsing is faster too, most of
  all on Tailwind's output.
- **Readable warnings.** A warning reads `file:line:column: warning: message`
  over the source line and a caret. Repeats are counted under the first, so
  netflix.com's sheet prints about 40 lines where it printed nearly 500, and
  `-q` and `-v` work.
- **Stable output.** Minified output no longer depends on hash order, and a
  rule merged into a nested parent is nested in the first pass rather than on
  a second run. Numbers print the same on Windows as elsewhere. Minifying
  `--minify` output again returns it byte for byte, on all 504 SatCSS sheets
  and on Tailwind's output, where 53 of those sheets and Tailwind's own
  stylesheet shrank again.
- **Smaller output that follows one stated rule.** Over the 504 SatCSS
  sheets and Tailwind's output, `--minify` output is 1.3% smaller raw than
  with 1.2.1, and the same within 0.1% under gzip and brotli. Merges keep
  every selector within 128 rules of where it was written, and within that
  bound the output is as small as cascade can make it. A sheet can still come
  out a little larger than with 1.2.1 in one metric: 11 grow by more than 1%
  under gzip (at most 2.2%) and 24 under brotli (at most 7%, on one cnn
  sheet), while 148 shrink by more than 1% raw.

The price is under [Breaking](#breaking). Scripts that read cascade's
diagnostics need a new pattern. In the library, parsers return a `result`
instead of raising, and two spellings of one selector are now one value.
`Css.to_string ~minify:true` prints the sheet it is given, and `Css.minify`
does what it used to. Functions removed to tidy the API are listed with what
to call instead, and moving off most of them is a rename or a deleted call.

### Breaking

#### Command line

- Warnings and errors were hard to locate: they gave byte ranges, a trace of
  the reader's own functions (`read_declaration/float:`) and an
  `(in component)` tag, and cascade's own messages took another form
  (`Error: ...`). Every diagnostic now reads `file:line:column: warning:
  message` or `file: error: message`, as a compiler's does, and a parse
  diagnostic is followed by the source line with a caret under the span. A bad
  `@font-face` descriptor value is located in the sheet, where it pointed at
  line 1 of a copy of the value. This affects scripts that match cascade's
  standard error. Match the new form, or read `cascade diff --json`
  (#1299, #1330, #1331, #1337).
- `cascade diff --json` made a consumer parse the location out of each
  message. Each warning and error now has `file`, `line` and `column` members
  and its `message` is the reason alone. Read the location from the members
  (#1331).
- The text fallback of `cascade diff` counted lines and columns from 0 while
  warnings count from 1, so a difference at the start of the second line read
  `line 1, col 0`. The report and the `--json` document now count from 1, and
  the report's `@@` header names that line instead of a byte offset. A consumer
  that added 1 stops doing so (#1334).
- A warning from `cascade diff` did not say which of the two files it was
  about. Each now names its input. A sheet parsed from a string without a
  name prints as `<CSS input>` (#1299).
- `cascade fmt missing.css` exited 1, the status of a file that failed to
  parse, so a build could not tell the two apart. It now exits 124, as the
  other commands do. A script that tests for status 1 on a missing file tests
  for 124 (#1338).

#### OCaml library

- **Parsers return a `result`.** Parsers reported failure in three ways,
  `Error.Parse_error`, `Failure` and `Invalid_argument`, so no single handler
  caught a malformed input. `Selector.of_string`, `Container.of_string`,
  `Supports.of_string`, `Declaration.of_string`, `Declaration.with_value`,
  `Media.of_string_strict`, `Media.value_of_string`,
  `Keyframe.selector_of_string` and `Font_face.*_of_string` now return
  `(_, Error.t) result`, and `Aria.of_string` an option. Every caller is
  affected. Match on the result. For text written in the program, use
  `Selector.v` or `Container.v`, which raise `Invalid_argument` (#1299, #1367).
- **`Css.to_string ~minify:true` prints the sheet it is given.** The minified
  printer also rewrote the sheet, so the printed text did not read back as the
  value printed. It folded `:nth-child(1)` and `:is(:link,:visited)`, dropped
  an `@charset` and an `@font-face` without `font-family` or `src`, and moved
  a registered `<color>` variable in a `box-shadow`. It also rounded a colour's
  alpha to three decimals, dropped an opaque `/1` from `lab()` and `oklab()`,
  dropped `0%` and `100%` from a gradient's end stops and folded the spaces and
  dots in a `grid-template-areas` row. This affects
  code that prints with `~minify:true` without optimising first: it compiles,
  and its output gets longer. Call `Css.minify`, which optimises and prints in
  one call as `cascade --minify` does, or one step of `Css.optimize`:
  `Selector.canonicalize`, `Stylesheet.drop_inert_at_rules` or
  `Stylesheet.normalize_shadow_color_vars` (#1350, #1351, #1354, #1367,
  #1370).
- **A selector is one value whatever its spelling.** `:before` and `::before`,
  `[x=a]` and `[x="a"]`, `odd` and `2n+1` were different values, so equality,
  hashing and deduplication missed them, and parsing a printed selector could
  give back another value. This affects code that builds or matches these
  constructors. `Before`, `After`, `First_letter` and `First_line` take no
  argument and `colon_form` is gone. `Exact_quoted (v, _)` and the other
  `*_quoted` attribute matches are `Exact v` and so on, with `v` decoded.
  `Odd` is `An_plus_b (2, 1)` and `Even` is `An_plus_b (2, 0)`. The printer
  picks the spelling (#1357).
- **Misuse raises `Invalid_argument`.** `Declaration.custom_property`,
  `Supports.property`, `Supports.func` and `Supports.property_name` raised
  `Failure` on a bad argument. A `try ... with Failure _` around them still
  compiles and stops catching, so search for it: catch `Invalid_argument`
  instead (#1299).
- **`Component.t` has a `Flat` case.** Deeply nested input overflowed the
  stack (see [Parsing](#parsing)). The contents of a block or function nested
  past `Parser.max_nesting_depth` (128) are now kept as the tokens read rather
  than as a tree. An exhaustive match over `Component.t` needs a `Flat` case.
  Code that only looks for a `Block` or a `Func` can ignore it (#1296).
- **`Error.t`'s `source` is an `Error.Source.t`.** Each error located itself by
  counting lines from the start of its source, so printing the warnings of a
  sheet cost its size per warning. The errors of one parse now share one index.
  This affects code that builds an `Error.t` record or calls `Error.in_source`,
  `Cursor.of_components` or `Stylesheet.read_stylesheet_of_rules`. Wrap the
  text with `Error.Source.v` and read it back with `Error.Source.text`.
  `Error.position` gives the line and column (#1299, #1310).
- **`Error.kind` has an `Invalid_statement` case.** An `@import` after a style
  rule was reported as a `Bad_value` for a property called `stylesheet`. It is
  now a statement the rules around it refuse. An exhaustive match over
  `Error.kind` needs the case (#1331).
- **A custom property's tokens carry no source position.** Two declarations of
  one custom property with one value compared and hashed differently depending
  on where they were read, so a table keyed on declarations kept both. The
  positions were offsets into the value's own text and located nothing in the
  sheet. `Component.source_loc` now gives `Loc.dummy` for these tokens, and
  `Component.without_locs` drops the positions from a token list you build
  (#1353).
- **`Stats` reports only what the optimizer did.** A factoring run records one
  row, so `Stats.iteration_stat`'s `local_iteration` and `active_passes` were
  always 1, and its `iteration` and `Stats.counters`'s `iterations` repeated
  the run number. Read as counts of passes, they described work the optimizer
  never did. They are gone, and `changed_passes` is `changed : bool`. `elapsed`
  is 0 unless the recorder was made with `~profile:true`, as the byte columns
  already were, so a run without `--profile` no longer pays for the timer. This
  affects code that reads `Stats` records. Read the run number from `fixpoint`
  and test `changed` where you tested `changed_passes > 0`. `--profile` prints
  one row per run, with `run`, `rules-in`, `rules-out`, `bytes-in`, `saved`,
  `changed` and `time` (#1372).
- **`Stats` no longer counts transfer reverts.** `Stats.revert_fixpoint` and
  `Stats.counters`'s `factor_transfer_reverts` counted factoring runs thrown
  away because an estimate of the gzip size grew. The optimizer no longer
  judges a run on that estimate, so the count would always be 0. Delete the
  call and the field read; `--profile` no longer prints a `factor transfer
  gate` line (#1399).
- **`Reader` has no error type of its own.** `Reader.Parse_error`, its
  `Reader.parse_error` record, `Reader.pp_parse_error`, `Reader.with_filename`,
  the `err` functions (`err`, `err_eof`, `err_expected`, `err_expected_but_eof`,
  `err_invalid`) and the context stack (`push_context`, `pop_context`,
  `with_context`, `callstack`) are gone. They kept a second error type beside
  `Error.t` that no parser raised and no `of_string` returned, and nothing in
  cascade pushed onto the stack. `Reader.skip` past the end, the one function
  that raised `Reader.Parse_error`, raises `Invalid_argument`, since reading
  past the end is a bug in the caller rather than bad input. This affects code
  that writes its own readers on `Reader`. Build an `Error.t` and raise it with
  `Error.fail`: `Error.unterminated` for `err_eof`, `Error.missing_token` for
  `err_expected` and `err_expected_but_eof`, `Error.bad_value` for `err` and
  `err_invalid`. Catch `Error.Parse_error`, print with `Error.to_string`, name
  the source with `Error.with_filename` and label a sub-reader with
  `Error.with_context`. Test `Reader.is_done` before `skip` (#1299).
- **`Css.of_string_exn` is gone.** It raised `Error.Parse_error`, where every
  other function for CSS written in the program, such as `Selector.v`, raises
  `Invalid_argument`. Use `Css.stylesheet`, which raises `Invalid_argument`. A
  handler for `Error.Parse_error` around the call moves to `Css.of_string` and
  its `Error` case (#1299, #1367, #1391).
- **The `Error.fail_*` functions are gone.** `Error.fail_sort_mismatch`,
  `fail_unexpected_token`, `fail_missing_token`, `fail_bad_selector`,
  `fail_bad_value`, `fail_bad_condition`, `fail_unknown_at_rule` and
  `fail_unterminated` each spelled `Error.fail` on one constructor. Call
  `Error.fail` on the constructor's value, as in
  `Error.fail (Error.bad_value loc ~property ~reason)` (#1371, #1391).
- **`Cursor.try_enum` and `Cursor.err_eof` are gone.** Each repeated another
  function. Use `Cursor.option (Cursor.enum label table)`, which also skips
  leading whitespace, and `Error.fail (Error.unterminated loc sort)` (#1371,
  #1391).
- **`Optimize.edges_of_rule` is gone,** with its `edge` and `packed_property`
  types. Nothing in cascade read them. Build the pairs from
  `Selector_summary.of_selector` and `Stylesheet.declarations`, keyed by
  `Declaration.prop_key` (#1371, #1391).
- **`Optimize.duplicate_buggy_properties` and
  `Optimize.apply_property_duplication` are gone.** They were documented as
  duplicating properties for buggy browsers, and returned their input
  unchanged. Delete the call. `Optimize.add_compatibility_prefixes` writes the
  WebKit fallbacks they were documented to add (#1345, #1367, #1391).

### Output

- Minifying `--minify` output again could shrink it further: 53 of the 504
  SatCSS sheets did, by up to 296 bytes, and so did Tailwind's stylesheet. One
  run left work behind. Above 128 rules the merge search judged distances and
  orders on its own working state, where rules already merged away still stood
  between the others and still ordered them, so a search of the output reached
  merges it had not. Joining two `@media` blocks put two runs of rules side by
  side after each had been optimised alone, and moving a declaration back into
  its rule left two nested blocks adjacent and unmerged. Dropping a nested rule
  that matches nothing left its `@media` empty, a composed `transition` left the
  one it overrode, and a `-webkit-mask-composite` twin was written that the next
  run dropped. Each run's merge search now ends only when a search of its output
  would find nothing more, and each of the others is finished in the same pass.
  The optimiser makes one pass at every size, where it made up to five below 129
  rules. The output of 69 SatCSS sheets is 2115 bytes smaller raw in all, and
  none is larger raw; 40 are 213 bytes larger in all under gzip, as minifying
  their earlier output again made them. `--minify` takes about 9% more CPU over
  the corpus, about 4% on Tailwind's stylesheet and 2% on tw's, most of it on
  the sheets whose output shrinks (#1401, #1402, #1403).
- `--minify` merged rules at any distance, then threw a whole factoring run
  away when an estimate of the gzip size grew, so near merges were lost with
  far ones and a second run could decide the other way. The default
  `--objective=transfer` now minimises output bytes over merges that keep
  every selector within 128 rules of where it was written, because a
  compressor encodes a repeated declaration in two or three bytes wherever it
  sits, but a selector moved away from its neighbours stops compressing
  against them. Nothing judges the result afterwards. `--objective=raw` merges
  at any distance (#1399).
- `--minify --scope=stylesheet` dropped declarations a page needs. A
  declaration written before longhands of its family was reset by the shorthand
  built from them: `background:#fff url(a.png) no-repeat center` lost its image
  and colour to `background-position` and `background-size`, and
  `font-weight:700` to `font-size` and `font-family`. It is now kept. Default
  output is unchanged (#1348).
- `--minify` printed `p:not(:not(.a .b))` as `p.a .b`, another selector. A
  double negation holding a type or a combinator now prints as `:is(X)`
  (#1350).
- `--minify` turned `animation:"none" 1s`, which runs the `@keyframes` named
  `"none"`, into `animation:1s`, and `animation:"none"` into the keyword
  `none`, which runs no animation. The string is kept (#1368).
- `--minify` printed `env(--branding-padding 2)` as `env(--branding-padding2)`,
  another variable. The space before the indices is kept (#1369).
- `--minify` could move a rule past another that styles the same element. A
  later `@media` or `@container` block merged into an earlier one across a
  list like `.a:after,.a:before` that styles the same pseudo-element. Two
  rules could also swap when they differ only in the case of a type selector
  (`DIV`, `div`), of an ID (quirks mode) or of an attribute value read through
  an `i` or `s` flag. Both orders are now kept, and a block merges across a
  more specific rule where that is safe (#1295).
- `--minify` dropped an `@import` after a `/*! */` comment or an unrecognised
  at-rule, though browsers apply it. It is now kept. An `@import` after an
  `@namespace`, and an `@namespace` after a rule, go, as browsers ignore them
  (#1294).
- `--minify` unquoted attribute values into selectors browsers drop
  (`[x="-2"]`, `[x="-"]`) or into another value (`[x="a\\b"]`). It now prints
  the shorter of an escaped ident and a string that reads back as written, so
  `[x="a b"]` prints as `[x=a\ b]` (#1292, #1357).
- Very large numbers changed sign: `width:4611686018427387904px` came out
  negative under `--minify`, and `z-index:calc(1e30)` as `-1`. They keep their
  sign (#1280).
- Very small numbers became zero: `(min-resolution: 4e-33dpi)` and
  `entry 4e-33%` came out as `0dpi` and `0%`, and a negative one as `-0`. A
  number too small for eight decimals keeps its value (#1292).
- `@media (aspect-ratio:1.5/1)` was rounded down to `1/1`. It prints as written
  (#1280).
- A number with more than 15 significant digits lost its last decimals:
  `123456789.12345679` came out as `123456789.1234568` (#1300).
- `lab()` and `oklab()` printed a percentage alpha as a number:
  `lab(50% 20 30 / 40%)` came out as `lab(50% 20 30 / .4)` even without
  `--minify`. It prints as written, as in the other colour functions (#1370).
- A `local()` font name with an escape or a quote did not read back as the
  same name. It is held decoded, so `local(\2197)` and a quote inside
  `local("...")` print back as that name (#1351).
- Minified output shifted for unrelated reasons. Merges that save the same
  were ordered by hash, so a change inside cascade could move the output of a
  sheet nobody touched. A tie now breaks by the shortest result, then by source
  order. Some sheets come out slightly smaller (#1355).
- Minifying `--minify` output shrank it again, so a pipeline that minifies
  twice gave other bytes than one that minifies once. Nesting a merged rule
  under a neighbour now happens in the first pass. A `0deg` gradient with end
  stops at `0%` and `100%` is reversed in the first pass too:
  `linear-gradient(0deg,red 0%,blue 100%)` came out as
  `linear-gradient(0deg,red,#00f)`, which a second pass shortened to
  `linear-gradient(#00f,red)`. An opaque `/1` goes from an `oklch()` or `lch()`
  with a `none` component, as from every other colour. For library users,
  `Selector.compound`, `combine` and `reshape` build a selector in the shape
  the parser reads (#1342, #1370).
- In a sheet of more than 128 rules, a `-webkit-` twin cascade adds, such as
  `-webkit-user-select`, could cover an earlier rule that only a second
  `--minify` dropped. That rule now goes in the first pass (#1395).
- Output read the same number in two spellings. Large numbers in media
  features and keyframe offsets print as `1e30` like everywhere else, instead
  of `1e+30` (#1292).
- Pretty output kept whichever spelling the input used. It now spells a
  selector one way: `::before`, a quoted attribute value and `odd`/`even`
  (#1357).
- `--minify` could lose a border side. Factoring out what two rules share
  wrote a rule's own `border` after the longhands it had written later, so
  `.a{color:red;border:1px solid #ccc;border-top-color:red}` beside
  `.b{color:red;border:1px solid #ddd}` lost its red top border. What a rule
  keeps now stays in the order it wrote it (#1404).
- `--minify` listed a rule over a vendor pseudo-class with others, as in
  `.a:-moz-focusring,.b{color:red}`, and a browser that does not read the
  pseudo-class drops the whole list, so Chromium lost `.b`'s colour. A
  pseudo-class some default target does not read stays out of a list, as a
  vendor pseudo-element did (#1407).
- `--minify` dropped rules over `:-webkit-full-screen`,
  `:-webkit-full-screen-ancestor` and `:-moz-full-screen` as unknown
  pseudo-classes, though Chromium, WebKit and Firefox read them. They are kept
  as written (#1408).

### Comparing stylesheets

- `cascade diff --diff=canonical` reported two sheets identical when one lost a
  rule or a declaration inside an `@media` block to a parse error:
  `@media screen{a!{color:red}b{color:red}}` and `@media screen{b{color:red}}`
  exited 0. That loss now counts as unread, as one inside `@supports` does,
  and the command exits 2 (#1312).
- `cascade diff` dropped a changed rule when the other file held two identical
  copies of its counterpart, because pairing one copy claimed both. The change
  is reported (#1297).
- `cascade diff --diff=canonical` reported a difference between two equivalent
  sheets when one wrote `top`, `right`, `bottom` and `left` in one rule and the
  other split them across two rules, or put an unrelated declaration between
  them: the comparison folded the longhands into `inset` on one side only. It
  now composes a rule's longhands wherever they stand, so the two compare
  identical, and so do `border-width`, `border-style` and `border-color` against
  `border`. Re-minifying cascade's own output of `facebook1` and `tumblr-3` in
  the SatCSS corpus reported this difference. Two rules whose declarations agree
  on every longhand they share, such as `border:1px solid red` and
  `border-width:1px`, no longer count as ordered, so the comparison does not
  report one of them as moved (#1394).
- `cascade diff` without `--diff` reported a colour written two ways, such as
  `color:#ffffff` against `color:#fff`, as a modified rule annotated
  "(declarations differ in subtle ways)". Over `outlook-3` and its cssnano
  output in the SatCSS corpus it listed 12 such rules. It now compares
  declarations the way `--diff=tree` does. When the two files parse to the same
  stylesheet, the report says "no structural change; the text differs" above the
  text diff, instead of "none classified structurally". It still exits 1,
  because the default mode counts any difference in the text (#1400).
- `cascade diff --diff=canonical` reported a difference when one sheet wrote a
  selector's declarations in two rules and the other in one, though no rule
  between them orders the two:
  `.a{width:1px}.b{width:2px}.c{height:3px}.a{height:4px}` against
  `.c{height:3px}.a{height:4px;width:1px}.b{width:2px}`. They compare
  identical (#1405).
- `cascade diff --diff=canonical` called
  `.o{width:76px}.g,.o{display:none}.p{display:block}.g{width:24px}` identical
  to a sheet that gives an element of both classes a width of 76px instead of
  24px. The two now compare different (#1406).
- `cascade diff --diff=canonical` called `.a,.b{speak:none}.a,.b{top:7px}`
  different from `.a,.b{speak:none;top:7px}`. Two adjacent rules over one
  selector list compare as one rule (#1409).
- `cascade diff --diff=canonical` ordered two rules with the same declarations
  when two of those declarations touch, as an unplaced `-moz-` longhand
  touches every property, so the same pair written as one selector list
  compared different. An element both match takes the declarations twice
  either way, and the two compare identical (#1410).
- `cascade diff --diff=canonical` kept a normal and an important declaration
  of one rule in their written order when they touch, so the rule written the
  other way round compared different. Importance sorts before order of
  appearance (CSS Cascade 5 sec. 6.1), and the two commute (#1411).
- `cascade diff --diff=canonical` kept an important declaration that a later
  rule with the same selector writes again at the same importance, so a sheet
  holding it in a shared rule compared different from one without it. It is
  dropped, as a normal one already was (#1412).
- `cascade diff --diff=canonical` called two sheets different when they wrote
  a declaration in either of two occurrences of its selector that other rules
  hold apart, though nothing between the two meets it. The declaration
  compares as written in the later occurrence (#1413).

### Speed

- `--minify` takes less than half the CPU of 1.2.1 over the SatCSS corpus,
  and about a third of the instructions on Tailwind's stylesheet and on tw's.
  Each step left the output byte for byte as it was (#1306, #1307, #1313,
  #1322, #1323, #1324, #1325, #1329, #1344, #1347, #1356, #1361, #1362, #1363,
  #1364, #1365, #1374, #1375, #1376, #1377, #1378, #1379, #1380, #1384,
  #1385).
- `cascade diff --diff=canonical` of Tailwind's stylesheet against tw's takes
  about a fifth of the instructions of 1.2.1 and less than half the memory.
  The faster ordering keeps a rule after an earlier one that writes another
  value to an element both match, as 1.2.1 did, also when a rule writing its
  own value stands between them (#1304, #1305, #1308, #1309, #1326, #1327,
  #1328).
- `cascade diff` slowed down with the square of the number of rules that moved
  or changed selector. netflix.com's stylesheet against its own `--minify`
  output takes 0.2 s of CPU instead of 1.2 s (#1297).
- Parsing is faster. A sheet of `var()` fallbacks that do not match their
  property's grammar cost its size squared, and a custom property's value was
  lexed twice. Each top-level rule, and each rule of a
  top-level `@layer`, `@media`, `@supports` or `@container` block, is now read
  before the next is parsed, which helps Tailwind's output most since it sits
  in one `@layer`. The lexer allocates only the token (#1311, #1314, #1315,
  #1381, #1382, #1383, #1387).
- Printing the warnings of a large sheet cost its size per warning. A 2 MB
  sheet now prints about twenty times faster (#1310).
- To see where a run goes, `cascade fmt --minify --profile` opens with the
  time spent reading, optimising and printing and names its columns in words.
  It counts `nested-merge probes`, also read as
  `Stats.counters.nested_merge_probes`, and prints one row per factoring run
  (#1301, #1336, #1372).

### Warnings and errors

- A noisy sheet buried its warnings in near-identical lines. A warning already
  printed for the same file is counted under the first, and a last line gives
  the total and points to `-v`. netflix.com's sheet prints about 40 lines of
  warnings where it printed nearly 500 (#1332, #1341).
- `-q` was documented and silenced nothing. `-q` now prints no warning and `-v`
  every one, in `fmt`, `apply`, `prune` and the `diff` report (#1332).
- A selector the reader refuses was reported as a `bad value: ...` naming no
  property. It is a `bad selector: ...` (#1340).
- A declaration with no value, such as `.b { color: }`, was reported one
  column past the `}` that ends it. It is reported at the `}` (#1339).
- A mistyped command was taken for a file: `cascade fmtt a.css` complained
  about `a.css`. It now says `fmtt` is an unknown command and suggests `fmt`.
  A word that names no file and holds no `.` or `/` is not taken for a
  stylesheet (#1333).
- `--flatten-nesting` without `--minify` did nothing and said nothing. It is
  now noted. `--keep-vars='*'` no longer prints a stray `[1]` (#1333).
- The help pages group the options by what they act on, give every command
  examples and describe the `diff` report and its `--json` members (#1333,
  #1335).

### Parsing

- A custom property's value could change on the way through. It was printed
  and lexed again, so `--x:\!important` became a lone backslash marked
  important, and `--x:a/**/b` became `--x:a b`, another token sequence. The
  value is now the tokens the parser read. `'a'` and `"a"` stay one value
  (#1386).
- Input nesting blocks or functions more than 128 deep crashed cascade with a
  stack overflow. A value nested deeper is now kept and printed back as
  written. A rule nested deeper loses its contents with a warning (#1296).
- A selector nesting `:is()` more than 100 deep read as an `:is()` that
  matches nothing. Its rule is dropped with a warning (#1296).
- A rule whose attribute selector has a number as its value, such as
  `[data-x=2]`, `[data-x=2px]` or `[data-x=50%]`, was kept as `[data-x="2"]`
  and so matched elements no browser matches. It is dropped with a warning, as
  browsers drop it (#1292).
- Declarations a browser ignores in an `@keyframes` frame went unreported. An
  invalid one was dropped silently, and an `!important` one or an
  `animation-*` property other than `animation-timing-function` stayed in the
  output. Each now warns and goes (#1294).
- An `@charset` warns and goes unless the sheet opens with exactly
  `@charset "UTF-8";`, so not after whitespace, a comment or an `@layer`
  statement. An `@import` after an unrecognised at-rule no longer warns, since
  browsers apply it (#1294).

### Options

- `--inline-vars` could put a value in the wrong rule. It read `.x&` under `a`
  as the class `.xa` and folded a value defined there into a rule styling
  `a.x`. It now composes a nested rule's selector as CSS Nesting does, and a
  definition on a selector list reaches a consumer under one of its branches
  (#1352).
- `--inline-vars` kept a variable that several rules define with one value,
  then folded it on a second run. It folds it the first time, so inlining its
  own output changes nothing. One whose definitions disagree is named:
  `--x takes different values in different scopes` (#1343).

### Windows

- Numbers printed differently on Windows: `3.40282e38px` came out as
  `3.40282e038px`, and `100000000000.5` as `100000000001`. They print as
  elsewhere, so a build gives the same bytes on every platform (#1286, #1292,
  #1300).
- `cascade` wrote CRLF line endings in its output and help pages. It writes
  LF, as elsewhere. `cascade diff --browser` starts node without a POSIX shell
  and finds it on `PATH` under `PATHEXT` (#1291).
- `--inline-imports` left Windows drive paths (`C:/a.css`, `C:\a.css`,
  `C:a.css`) and `file:` URLs alone as remote URLs, and looked a UNC path up
  under the importing file's directory. It inlines them and checks them
  against `--import-root` (#1291).

### Library

- **The optimizer's building blocks are public modules.** `Rule_graph`,
  `Rule_candidate`, `Rule_rewrite`, `Rule_scheduler`, `Ctx`, `Factor`, `Size`
  and twenty other modules the tests and benches drive were private, and a
  build could compile those clients before the modules they name, failing
  with "is an alias for module Cascade__Rule_candidate, which is missing".
  They are now part of `Cascade`'s public interface (#1543).
- `Source_index` offsets and slices drifted by a byte per CRLF, so a tool
  locating `@import` in a CRLF file cut statements in the wrong place, and a
  NUL could make `Source_index.v` raise. They index the string you passed, also
  when it starts with a BOM. `Reader.origin` and `Reader.original_offset` map a
  position in the preprocessed input back to a byte offset in that string
  (#1285).
- `Css.Context.resolve_url` resolves `1:/x` against the base as a relative
  path, as WHATWG URL does, since a digit cannot open a scheme (#1291).

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
