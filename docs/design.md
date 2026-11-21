# Design notes

How the implementation keeps the guarantees in
[invariants.md](invariants.md). The invariants are the contract; these notes
describe the mechanisms behind them and change with the code.

## Authored spellings

A few parser nodes keep the text the author wrote beside the value:
`Authored_hex` for a hex colour, the number text of a `Dimension`, and the
quote and source text of a quoted `content` string. The pretty printer writes
that text back (`0.90rem`, `#0000FF` and `"\66oo"` print as read), the
minified printer writes the value, and normalize drops the text, so two
spellings of one value are one normal form with one `Declaration.hash`, which
factoring buckets on. `Css.of_string ~preserve_source:true` keeps the
byte-exact input in a separate `source` snapshot, which optimising and the
other transforms leave untouched.

## Math functions over several units

A comparison over units with a fixed ratio (`Values_calc.compared_results`)
compares their values in the type's canonical unit and answers the argument
it picks, as written, so no converted coefficient reaches the print; two
values a conversion leaves a hair apart (`1cm`, `10mm`) are one
(`same_quantity`). A sum converts a time through `ms` and an angle through
`deg`, where whole values stay whole and the folded leaf reads back to the
same float. `rad` has an irrational ratio, so a sum holding one stays, and
`hypot()`, `round()`, `mod()` and `rem()` fold over one unit only, since
their result is a coefficient no literal spells exactly. A call that does not
fold prints bare only where the printer passes `~bare_call` to
`pp_calc_with`: a time, a percentage, `font-stretch`, `font-size-adjust`,
`text-size-adjust` and `border-image-slice` read the bare call back as the
same calculation node, while a length slot reads a bare `min()` into its own
typed node, so a length calculation keeps its `calc()`.

## Substitutions beside components

A typed reader gives a `var()` beside other components one slot of the
property's grammar and decides defaults, elisions and order around it. The
declaration reader keeps such a reading only when its pretty print passes
`Declaration_core.slots_kept` against the source: every substitution call as
written, among the same separators and inside the same functions, with no
component added, dropped or moved. Otherwise it keeps the value as an
`Unknown_property` token stream under the property's name.
`Declaration.normalize` applies the same test to each fold of such a value and
keeps the value when a fold fails it. A substitution may supply commas, so a
comma list is compared run by run: a segment holding a substitution keeps its
slots, and a run of segments holding none stays on each side where it was,
free to fold inside. The normalisers keep what the test would refuse whole: a
`box-shadow` keeps an `inset` written after a body holding a `var()`
(`Trailing`), a gradient keeps a direction written before a `var()` first
stop and never merges two `var()` stops, and `flex` keeps the unit of a zero
basis after a `var()` factor: the factor may supply nothing, and a bare `0`
after one factor is the shrink. That is the one respelling in place found to
move a component to another slot. A zero length loses its unit only where no
`<number>` can stand, and beside a `var()` that slot may shift, which matters
only where a neighbouring slot takes a number: `flex` does; `font` does not,
since a `0` there cannot be a weight (its range starts at 1) and Chromium 153
reads it as the size either way. `Context.eval` substitutes the
references a context binds into such a stream and reads the result again, so a
value that types once substituted evaluates as any other.

## Interning and physical identity

`Declaration.v` and `Declaration.theme_guarded` intern each declaration in a
per-domain weak table (`lib/declaration_core.ml`), so equal declarations built
in one domain are one object and `equal_declaration` answers most pairs with
`==`. A pass returns its input physically when it changes nothing
(`map_preserve`, `x == y` checks). `List.memq` over declarations, in
`lib/rule_trace.ml`, `lib/rule_order.ml` and `lib/rule.ml`, asks whether a pass
kept a declaration equal to the one given, which holds only under both
properties. "optimize preserves physical identity on a fixed point" in
`test/test_optimize.ml` and `fuzz/fuzz_optimize.ml`, and "equal_declaration
agrees with compare" in `fuzz/fuzz_declaration.ml`, check them.

## The canonical comparison

Each side runs `Css.optimize ~regroup:false`, judged for the default targets
unless `--enforce-spec` (`?judge`), then `Css.flatten_nesting`, the optimizer
again and `Css.canonicalize_rule_order`, and the two minified prints are
compared byte for byte (`canonical_of_stylesheet` in
`lib/diff/css_compare.ml`). Regrouping is off because it depends on how the
input happened to order its rules.

The projection (`lib/rule_order.mli`) adds normalisations emission does not
make:

- `@media not all and (X)` against `not (X)` (Media Queries 4 sec. 2.2.1,
  2.3 and 2.5), and `min-X`/`max-X` against the range form (sec. 2.4.3 and
  2.4.4), which a Level 3 parser rejects, so `--enforce-spec` emission keeps
  the long forms;
- in a custom property's tokens, a quoted font family against its ident
  sequence where a bare generic family proves a font stack, a time (`500ms`
  against `.5s`) and, bar `--lossless`, an angle; emission keeps the tokens as
  written;
- `oklab(0% none none/.5)` against `oklab(0% 0 0/.5)` for a colour standing as
  a whole colour value outside `@keyframes`, `@starting-style` and a rule that
  transitions it (CSS Color 4 sec. 4.4 and 13.4);
- a repeating quotient against its six-figure value (`calc(1/3*100%)` against
  `33.3333%`), bar `--lossless`, the bound M6 sets;
- under `--lossless`, `color(srgb ...)` on whole bytes against its `rgb()`,
  hex or named spelling;
- a run of `@property` rules sorted by name, keeping the last registration of
  each (CSS Properties and Values API 1 sec. 3), which is the last valid one
  since the reader keeps no other as an `@property` (under `--enforce-spec`
  one a shipping engine drops stays as an unknown at-rule, which splits the
  run), and a top-level `@layer a,b;`
  name the following blocks already declare in that order (CSS Cascade 5 sec.
  6.4.3);
- a legacy name alias (CSS Cascade 5 sec. 3.1, WHATWG Compatibility sec.
  3.4.1) read as its property, nested `@media` conditions sorted and
  deduplicated, and a keyframe's declarations sorted as a rule's are.

Beyond the optimizer's own deduplication, the projection deletes an
`@supports` guard false in every release of B, as one over a
`<general-enclosed>` term is (CSS Conditional 3 sec. 6), a superseded
registration or layer name, and, when judging, a prefix no target reads and
a rule no target parses. The structural diff's comparison key
(`Css.declaration_value_for_equivalence`) equates values of a proven type by
their minified spelling. A phantom canonical difference on equal content is
a projection gap in `rule_order.ml` or `rule_trace.ml`.

## The trace

The projection reads a run of style rules as the trace of its declarations:
an entry per declaration under its selector and conditions, two entries
ordered only where swapping them side by side could change a computed value,
and the form printed is a function of that partial order and the entries'
text. Where the run's rules begin and end, and which occurrences of a selector
were folded in which order, never reach it. A fact that frees a pair by
reading a third entry (a slot a later write of the group rewrites, a later
twin of one in the other's group) never rests on an entry the projection
drops: an entry is dropped only when it is redundant in the run's own order
and no such fact about another entry of its group is read off it. The
projection compares declarations by node, in normal form. Shorthand
composition runs on the rules the trace prints and keeps their order: the
box merge (`Shorthand_box.absorb_box_longhands`) folds a longhand into an
earlier shorthand only past declarations that write none of its slots, so a
printed form projects to itself.

## Target decisions

A target version is a floor: that release and every later one in the data.
`Supports.implemented_by` decides an `@supports` guard over those releases
when the run judges for targets (`?judge`): a declaration leaf is true from the
first release that parses every production its declaration uses, value
included, and is unknown in an engine whose releases disagree on it; `not`,
`and` and `or` follow CSS Conditional 3 sec. 6's three-valued logic engine by
engine, so the guard is unwrapped when true in every release and dropped when
false in every one. `Supports.never_holds` answers for a
`<general-enclosed>` term. `Declaration.fallback_for` decides whether an earlier
declaration is dead: it is when, in each engine, the first release that
parses it (`Declaration.first_release`) parses the later one too, so, a
shipped production staying, no release from the floor to the newest in the
data paints it. It is asked of the same floors (`Ctx.floors`) by in-rule
deduplication (`Shorthand_dedup.deduplicate_step`), by same-selector
shadowing across rules (`Cover.covered`, `Rule.drop_shadowed`) and by the
projection (`Rule_order`), so whatever the minifier deletes the judge also
counts dead. `Merge.compatible` lists two selectors only when every engine
ships them in the same release. The projection splits a selector list into
one rule per branch only when `Merge.listable` says every release reads its
branches alike (`Rule_order.expand_lists`); any other list stays one rule.
When judging, it first drops a rule each target engine drops in every
release (`Merge.parsed_by_none`, `Rule_order.drop_unparsed_rules`): only the
recorded facts about prefixed names settle that, never an unshipped key,
since the data records a partial implementation as none.

## Cost models

Factoring, its preflight and the rule graph weigh a declaration by the length
of its minified print under the run's own `--lossless` and `--enforce-spec`,
which choose some spellings (an oklch chroma's `76%` or `.304`, a family
name's quotes). `Declaration.size` caches the default print's length; a
`Size.print`, which `Rule_graph.of_rules` and `Preflight.summarize` take,
carries the run's flags, and `Size.decl` measures the flagged print when one
is set. `optimize_factor` "factoring weighs the flagged print" checks it. A
rule weighs everything it prints, its nested statements included:
`Size.block` measures a rule with nested content by printing it. The `size`
case "rule size counts nested content" and the `rule_graph` case "node size
counts nested content" check it.

## Typed property decisions

The optimizer decides on the property, never on its name.
`Properties.is_vendor_prefixed` and `Properties.legacy_alias` match the
property GADT with no wildcard, so a new constructor fails the build until it
is classified; a property cascade does not type answers by the name the reader
kept for it. `properties` "vendor prefix and aliases are property facts" checks
both against the WHATWG Compatibility Standard sec. 3.4.1 list.

## Rewrites that move rules

Factoring a default value into a shared block keeps a rule's own declaration
when an earlier overlapping rule writes a different value. Whether nesting may
stay is `Ctx.parses_nesting`: every engine of the floors at or past
`css.selectors.nesting` in the compat data, and never under `--enforce-spec`.
Otherwise `Optimize.stylesheet` runs `Flatten.block` before the pipeline;
when it holds, `Flatten.rules_with_nested_declarations` still flattens each
rule holding a nested declarations rule, which the compat data has no key
for. The `&` relaxation and synthesis run only when it holds. Nesting synthesis
is adjacent-only, within one cascade context, with an exact selector extension
under a non-list parent that carries no pseudo-element, leaving no `&` in what
remains of the child, since sec. 4 has that `&` name `:scope` or the enclosing
rule, and only when it shortens the output. Every composition goes through `Nest.substitute`, which
splices in for `&` only the parent branches without a pseudo-element and
stands `:is()` of the parent for an `&` left with none, a selector
`Nest.keep_readable_branches` then drops.
In an `@scope` block `Flatten` composes under the parent with the implied
root spelled out (`& <parent>`, which sec. 3.5.1 weighs at zero) and prints
the bare composition when the nested selector starts with its parent
(`Nest.parent_first`); `Nest.merge_lone` merges there only in that case.

The rule graph (`lib/rule_graph.ml`) that factoring and merging reorder over
reads a rule with nested content as parts: its own declarations under its
selector, and each rule its body holds at any depth under the composed
selector `Nest.leaves` gives it, a conditional group's rules as though the
condition held. Two nodes conflict when one part of each does. The indexes
file a node under the union of its parts, and such a node stays out of the
compact runs, which assume one declaration per node. The factoring scan and
`Rule_graph.facts` read the same parts. A rule whose body holds an `@scope`
has no selector to weigh its scoped rules by, so factoring takes it as a
barrier and factors the runs on either side apart.

## Speed

Speed on real stylesheets is a requirement, and a slow path on realistic input
is a defect. `bench/corpus_sweep.sh` measures the SatCSS corpus.

## Registrations

`Optimize.known_registrations` is the one answer to what a custom property's
registration is. It knows none in a fragment. Under `--scope=stylesheet` it
takes the last `@property` for a name at the top level or in an `@layer`; one
inside another block, or kept as an unknown at-rule, leaves the name unknown.
The calc-simplifier context, the typed promotion of custom-property values and
the fold of a registered `var()` into a shorthand all read it.

## Partial shorthands

`Ctx.resets_held` answers whether a shorthand composed under
`--scope=stylesheet` resets a slot something else holds. `Optimize` builds it
from a count, per slot and importance, of the sheet's declarations that hold a
slot at other than its initial value, less the composing rule's own, and from
the slots the user-agent sheet holds: `font-weight`, `font-style`,
`font-size`, `font-family`, `background-color` and `background-image`.
`Shorthand_common.guard_reset` applies it to every composer that builds a
shorthand from part of its family.
