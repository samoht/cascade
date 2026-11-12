(** Optimize tests: merging, factoring, fixpoints and node canonicalisation. *)

open Cascade
open Css.Optimize
open Css.Values
open Optimize_helpers

(** {1 Selector merging tests (cascade semantics)} *)

(* A [:root] or [:host] rule sorts its custom properties by name, which is safe
   because two custom properties of different names never decide each other. The
   declarations that are not custom properties do decide things, so the sort
   moves the custom ones AMONG THEIR OWN POSITIONS and leaves every other slot
   where the author put it. Sorting a run that holds nothing else says nothing
   about that: a pass that walked the other positions instead answers those
   cases alike and drops the declaration standing in one. *)
let root_custom_sort_keeps_other_positions () =
  let same name expected css =
    Alcotest.(check string) name expected (optimized_string css)
  in
  same "a custom run sorts by name" ":root{--a:2;--b:1}" ":root{--b:1;--a:2}";
  same "and sorts around a declaration that is not one"
    ":root{--a:2;color:red;--b:1}" ":root{--b:1;color:red;--a:2}";
  same "wherever that one sits" ":root{color:red;--a:2;--b:1}"
    ":root{color:red;--b:1;--a:2}";
  (* The sort is [:root] and [:host] only: anywhere else the author's order is
     the one that ships. *)
  same "another selector keeps the authored order" ".x{--b:1;--a:2}"
    ".x{--b:1;--a:2}"

let test_merge_consecutive_identical () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.padding [ Px 10. ] ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  Alcotest.(check bool)
    "merges consecutive identical rules" true
    (String.contains output_str ',')

let test_combine_identical_oklab_none () =
  (* A [none] channel in oklab() is a missing component that matters only for
     color interpolation (CSS Color 4 sec. 4.4): color-mix, gradients, and
     transitions carry it forward. It does not affect rule grouping, which is
     set union (Selectors 4 sec. 4.2) and never interpolates. So two rules with
     byte-identical oklab-none declarations combine into one comma-selector rule
     exactly like any other identical pair - no minifier-specific exception.
     oklab() serialises space-separated, so a comma in the output is the
     combined selector list. *)
  let output =
    optimized_string
      ".a { color: oklab(40% none .1) } .b { color: oklab(40% none .1) }"
  in
  Alcotest.(check bool)
    "identical oklab(none) rules combine into a comma selector" true
    (String.contains output ',')

let test_no_merge_different_declarations () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.padding [ Px 20. ] ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  let has_foo = Astring.String.is_infix ~affix:".foo{" output_str in
  let has_bar = Astring.String.is_infix ~affix:".bar{" output_str in
  Alcotest.(check bool)
    "keeps rules with different declarations separate" true (has_foo && has_bar)

let test_merge_non_consecutive_non_conflicting () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.margin [ Px 5. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "baz")
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.margin [ Px 5. ] ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  Alcotest.(check string)
    "merges non-consecutive non-conflicting rules"
    ".bar,.foo{margin:5px}.baz{padding:10px}" output_str

(* pp alone, with no optimize pass: serialize the parsed AST minified. Under the
   policy "pp must not change semantics", this is faithful - it may pick the
   shortest spelling of a node but must not transform one node into another. *)
let pp_min css =
  match Css.of_string ~strict:false css with
  | Ok { Css.stylesheet; _ } ->
      Css.to_string ~minify:true stylesheet |> String.trim
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let assert_pp_and_optimize input ~minified ~optimized =
  Alcotest.(check string) (input ^ " [minify]") minified (pp_min input);
  Alcotest.(check string)
    (input ^ " [minify+optimize]")
    optimized (minify_str input)

let test_factor_shared_declarations () =
  (* Two sibling rules sharing a declaration subset factor that subset into a
     combined selector, leaving each rule its unique declarations. Safe here -
     adjacent, distinct selectors, no conflicting property - and shorter, in a
     single pass. *)
  let once =
    minify_str
      ".emoji-icon{display:inline-block;height:20px;width:20px}.emoji-result{display:inline-block;font-size:18px;height:20px}"
  in
  Alcotest.(check string)
    "shared declarations factor into a combined selector"
    ".emoji-icon,.emoji-result{display:inline-block;height:20px}.emoji-icon{width:20px}.emoji-result{font-size:18px}"
    once;
  Alcotest.(check string) "factoring reached in one pass" once (minify_str once)

let test_factor_interval_schedule () =
  (* The whole run shares display:block, but the optimal rewrite is two
     non-overlapping local intervals: .a/.b/.c additionally share color:red,
     while .d/.e/.f additionally share background-color:blue. This pins the
     weighted interval scheduler rather than a single broad greedy factoring. *)
  let once =
    minify_str
      ".a{display:block;color:red;width:1px}.b{display:block;color:red;height:1px}.c{display:block;color:red;padding:1px}.d{display:block;background-color:blue;width:2px}.e{display:block;background-color:blue;height:2px}.f{display:block;background-color:blue;padding:2px}"
  in
  Alcotest.(check string)
    "disjoint local common blocks factor independently"
    ".a,.b,.c{display:block;color:red}.a{width:1px}.b{height:1px}.c{padding:1px}.d,.e,.f{display:block;background-color:#00f}.d{width:2px}.e{height:2px}.f{padding:2px}"
    once;
  Alcotest.(check string)
    "interval factoring is idempotent" once (minify_str once)

let test_factor_interval_keeps_overrides () =
  (* A same-property different-value member can still join the common-property
     group, but its value must be emitted after the shared rule. This catches
     the score/build path for interval leftovers, including the offset-aware
     earlier-overrides check. *)
  Alcotest.(check string)
    "interval factoring preserves later overrides"
    ".a,.b,.c{display:block;color:#00f}.a,.c{color:red}"
    (minify_str
       ".a{display:block;color:red}.b{display:block;color:blue}.a{display:block;color:red}.c{display:block;color:red}")

let test_factor_selector_branch_keeps_later_override () =
  (* Extracting the shared overflow/position declarations must not let the
     earlier selector-list branch overwrite the later .scroll-item a z-index.
     Only that branch is overridden; .scroll-pic-frame keeps z-index:100. *)
  Alcotest.(check string)
    "selector-list branch keeps its later override"
    ".scroll-item \
     a,.scroll-pic-frame,.scroll-pic-wrap{overflow:hidden;position:relative}.scroll-item \
     a,.scroll-pic-frame{z-index:100}.scroll-item{display:inline;float:left;height:164px;overflow:hidden;width:200px}.scroll-item \
     a{border:1px solid#fff;display:block;height:162px;width:198px;z-index:50}"
    (minify_str
       ".scroll-item \
        a,.scroll-pic-frame{overflow:hidden;position:relative;z-index:100}.scroll-pic-wrap{overflow:hidden;position:relative}.scroll-item{display:inline;float:left;height:164px;overflow:hidden;width:200px}.scroll-item \
        a{border:1px solid \
        #fff;display:block;height:162px;width:198px;z-index:50}")

let test_factoring_reaches_fixpoint () =
  (* Factoring one shared subset can expose another: grouping .a/.b on color:red
     leaves .b with padding:0, now groupable with .c. A correct optimizer
     applies every such factoring in a single pass - a second pass that shrinks
     the output means the first stopped early. This pins idempotence rather than
     the exact grouping, since the triangle of shared declarations has several
     equal-length factorings. *)
  let once =
    minify_str
      ".a{color:red;margin:0}.b{color:red;padding:0}.c{margin:0;padding:0}"
  in
  Alcotest.(check string)
    "interacting factorings converge in one pass" once (minify_str once)

let test_large_stylesheet_reaches_local_fixpoint () =
  (* Large sheets deliberately limit the expensive global factoring walk, but
     rule-local normalization must still converge. Composing these longhands
     creates a shorthand whose explicit initial delay only disappears when the
     synthesized declaration itself is normalized. *)
  let padding =
    List.init 129 (fun i -> Fmt.str ".padding-%d{z-index:%d}" i i)
    |> String.concat ""
  in
  let input =
    padding
    ^ ".target{transition-property:opacity;transition-duration:70ms;transition-delay:0ms;transition-timing-function:linear}"
  in
  let once = minify_str input in
  Alcotest.(check string)
    "large-sheet local rewrites converge in one pass" once (minify_str once)

let test_large_stylesheet_normalizes_vendor_aliases () =
  (* Alias comparison must see the normalized typed values. Otherwise the
     prefixed 250ms/500ms values differ structurally from the normalized .25s/
     .5s unprefixed twins until the serialized output is parsed again. *)
  let padding =
    List.init 129 (fun i -> Fmt.str ".vendor-padding-%d{z-index:%d}" i i)
    |> String.concat ""
  in
  let input =
    padding
    ^ ".target{-webkit-transition:opacity 250ms;transition:opacity \
       250ms;-webkit-animation:spin 500ms linear;animation:spin 500ms linear}"
  in
  let once = minify_str input in
  Alcotest.(check string)
    "large-sheet aliases compare normalized values" once (minify_str once)

let test_large_stylesheet_factoring_reaches_fixpoint () =
  (* Large graphs refresh candidates in batches. The refreshed batch can itself
     expose another profitable grouping, which must be enumerated before the
     scheduler returns. Inert custom-property rules select the large-graph path
     without participating in these factoring groups. *)
  let padding =
    List.init 129 (fun i ->
        Fmt.str ".factor-padding-%d{--factor-padding-%d:0}" i i)
    |> String.concat ""
  in
  let input =
    padding
    ^ ".scroll-item{display:inline;float:left;height:164px;overflow:hidden;width:200px}.scroll-item \
       a{border:1px solid \
       #fff;display:block;height:162px;overflow:hidden;position:relative;width:198px;z-index:50}.part-h-m{margin-right:20px;width:360px}.part-j-l,.part-j-m,.part-j-r{display:inline;float:left}.part-n-l,.part-n-m,.part-n-r{display:inline;float:left}.part-n-l{margin-right:20px;width:240px}.mod44-list{display:inline;float:right;margin-right:5px}.mod44-list \
       li{display:inline;float:left;line-height:34px;height:34px;margin-right:5px}.mod-a \
       .tab-nav-a \
       a{border-left:0;float:left;line-height:23px;height:23px;padding:0}.mod-a \
       .tab-nav-a \
       span{border-left:0;float:left;line-height:23px;height:23px;padding:0 \
       2px}"
  in
  let once = minify_str input in
  Alcotest.(check string)
    "large-sheet factoring converges in one scheduler run" once
    (minify_str once)

(* A second pass that moves is a first pass that stopped early, so each vector
   is checked by re-emitting what the first pass wrote. [emit] is the mode's own
   emitter, and the name carries the vector so a failure says which one
   moved. *)
let assert_emission_is_a_fixed_point ~emit label vectors =
  List.iter
    (fun css ->
      let once = emit css in
      Alcotest.(check string)
        (String.concat "" [ label; " is a fixed point on "; css ])
        once (emit once))
    vectors

let minify_stylesheet_scope css =
  match Css.of_string ~strict:false css with
  | Ok { Css.stylesheet; _ } ->
      Css.to_string ~minify:true (Css.optimize ~scope:`Stylesheet stylesheet)
      |> String.trim
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let test_prefix_synthesis_reaches_fixpoint () =
  (* The compatibility-prefix pass writes declarations the rest of the pipeline
     decides about: a prefixed twin is a rule's shared subset for the factoring,
     and a prefixed longhand run is a shorthand for the contraction. Synthesised
     after the pipeline settled, neither pass ever sees it. *)
  assert_emission_is_a_fixed_point ~emit:minify_str "minify"
    [
      "a{user-select:all}b{-webkit-user-select:ALL}";
      "a{-webkit-user-select:all}b{user-select:ALL}";
      "a{mask-size:auto}b{-webkit-mask-size:AUTO}";
      "a{mask-clip:content-box}b{-webkit-mask-clip:CONTENT-BOX}";
      "a{mask-position:10% 20%}b{-webkit-mask-position:10%\t20%}";
      "a{backdrop-filter:blur(max(0px, \
       1em))}b{-webkit-backdrop-filter:blur(max(0px,1em))}";
    ]

let test_prefixed_longhand_contracts_in_one_pass () =
  (* [`Stylesheet] licenses the partial-coverage shorthand, so the synthesised
     [-webkit-mask-image] contracts with its unprefixed twin. That contraction
     runs in the pipeline the prefix pass follows, so it never sees them. *)
  assert_emission_is_a_fixed_point ~emit:minify_stylesheet_scope
    "minify --scope=stylesheet"
    [
      "a{mask-image:var(--x, 10px)}";
      "a{mask-image:var(--x, image-set())}";
      "a{mask-image:var(--x, var(--y, 1px))}";
    ]

let test_authored_dimension_reaches_fixpoint () =
  (* The reader keeps [2e0ch] as a dimension carrying its own text, and the
     printer drops that text under [--minify]. Two spellings then reach one
     minified text through two nodes, which the rule merge reads as two values
     until a second pass re-reads them as one. *)
  assert_emission_is_a_fixed_point ~emit:minify_str "minify"
    [
      "a{tab-size:2e0ch}b{tab-size:2ch}";
      "a{tab-size:2ch}b{tab-size:2e0ch}";
      "a{overflow-clip-margin:1E0px}b{overflow-clip-margin:1px}";
      "a{line-height:+120%}b{line-height:120%}";
      "a{line-height:12.0px}b{line-height:12px}";
      "a{grid-auto-rows:100.0px}b{grid-auto-rows:100px}";
      "a{grid-auto-rows:+100px}b{grid-auto-rows:100px}";
      "a{grid-auto-columns:00100px}b{grid-auto-columns:100px}";
      "a{grid-auto-columns:100e0px}b{grid-auto-columns:100px}";
      "a{contain-intrinsic-width:100.0px}b{contain-intrinsic-width:100px}";
      "a{contain-intrinsic-height:00100px}b{contain-intrinsic-height:100px}";
      "a{contain-intrinsic-inline-size:100E0px}b{contain-intrinsic-inline-size:100px}";
      "a{contain-intrinsic-block-size:00100px}b{contain-intrinsic-block-size:100px}";
      "a{columns:12E0em}b{columns:12em}";
      "a{columns:+12em}b{columns:12em}";
    ]

let test_unwrapped_calc_leaf_reaches_fixpoint () =
  (* [pp_min] prints without running a pass, which is the mode that asks whether
     the printer is a serialiser. Dropping a [calc()] wrapper there leaves a
     leaf that is no longer a calc operand, so the next reader gives it the
     spelling a leaf takes on its own, and for [animation] the unwrapped [1] is
     the slot's initial and the whole shorthand collapses. *)
  assert_emission_is_a_fixed_point ~emit:pp_min "pp minify"
    [
      "a{transition-duration:calc(120ms)}";
      "a{animation-duration:calc(120ms)}";
      "a{animation:calc(1)}";
    ]

let test_no_factor_across_conflict () =
  (* CSS Cascade 6.1: the two .x rules conflict on color, so they merge (last
     wins). The later .y carries the first .x's value, but grouping it with that
     .x would reorder it past the conflicting .x{color:blue} and change the
     cascade - so .y stays separate. *)
  Alcotest.(check string)
    "no grouping across a conflicting same-selector override"
    ".x{color:#00f}.y{color:red}"
    (minify_str ".x{color:red}.x{color:blue}.y{color:red}")

let test_zero_box_side_covered_by_shorthand () =
  (* Margin and padding have only the four side longhands, so a zero shorthand
     and same-side zero longhand are equivalent on that side. Border is not:
     border shorthands also carry style and color state. *)
  assert_pp_and_optimize ".x{margin:0;margin-top:0}"
    ~minified:".x{margin:0;margin-top:0}" ~optimized:".x{margin:0}";
  assert_pp_and_optimize ".x{margin-top:0}.x{margin:0}"
    ~minified:".x{margin-top:0}.x{margin:0}" ~optimized:".x{margin:0}";
  assert_pp_and_optimize ".x{padding:0}.x{padding-left:0}"
    ~minified:".x{padding:0}.x{padding-left:0}" ~optimized:".x{padding:0}";
  assert_pp_and_optimize ".x{margin:5px}.x{margin-top:0}"
    ~minified:".x{margin:5px}.x{margin-top:0}" ~optimized:".x{margin:0 5px 5px}";
  assert_pp_and_optimize ".x{border:0}.x{border-top-width:0}"
    ~minified:".x{border:0}.x{border-top-width:0}"
    ~optimized:".x{border:0;border-top-width:0}"

let test_keep_zero_duration_transition () =
  (* transition:color has 0s duration, so nothing animates now - but it still
     sets transition-property:color, which becomes live the moment any duration
     override applies (a :hover rule, inline style, or JS). Minify is open over
     that runtime state, so the declaration is not dead and must not be dropped.
     cubic-bezier(.25,.1,.25,1) is ease (the default timing function), so only
     the timing function drops; lightningcss agrees on transition:color. *)
  Alcotest.(check string)
    "0s transition keeps its property, not dropped" "a{transition:color}"
    (minify_str "a{transition:color}");
  Alcotest.(check string)
    "bezier folds to ease, the default ease then drops" "a{transition:color}"
    (minify_str "a{transition:color cubic-bezier(0.25,0.1,0.25,1)}")

let test_keep_var_border_spaces () =
  (* The spaces between var() references in a border value are significant: they
     are token separators in the substituted value, so [var(--bw) var(--bs)]
     keeps the components apart where [var(--bw)var(--bs)] would glue them into
     a single token on substitution. The minify round trip must preserve
     them. *)
  Alcotest.(check string)
    "var()-valued border keeps significant spaces"
    "a{border:var(--bw) var(--bs) var(--bc)}"
    (minify_str "a{border:var(--bw) var(--bs) var(--bc)}")

let test_custom_value_close_paren_whitespace () =
  (* A [)] closing a non-substitution function or a block is a hard token
     boundary that no neighbour can merge across, so the whitespace after it is
     insignificant: optimize folds it out (and the canonical diff inherits
     that), while pp keeps it because pp is a pure serializer. *)
  Alcotest.(check string)
    "pp keeps whitespace after a function close (held)"
    "a{--x:drop-shadow(0 0 0) drop-shadow(1px 1px)}"
    (pp_min "a{--x:drop-shadow(0 0 0) drop-shadow(1px 1px)}");
  Alcotest.(check string)
    "optimize folds whitespace after a function close (canonical)"
    "a{--x:drop-shadow(0 0 0)drop-shadow(1px 1px)}"
    (minify_str "a{--x:drop-shadow(0 0 0) drop-shadow(1px 1px)}");
  (* A [var()]-carrying calc does not reduce to a leaf, so it stays a function
     and exercises the whitespace-after-[)] fold (a fully-constant angle calc
     instead folds to a dimension - see the custom-property folding tests). *)
  Alcotest.(check string)
    "optimize folds whitespace after calc() before an ident"
    "a{--x:calc(45deg*var(--n))in oklab}"
    (minify_str "a{--x:calc(45deg * var(--n)) in oklab}");
  (* A [var()] substitutes a token stream textually, so dropping the whitespace
     after it could merge the substituted values ([1px2px]); it stays. *)
  Alcotest.(check string)
    "optimize keeps whitespace after var()" "a{--x:var(--a) var(--b)}"
    (minify_str "a{--x:var(--a) var(--b)}")

let test_distant_media_merge () =
  (* Non-adjacent same-condition @media merge: safe when the crossed rule sets a
     different property, so no element's computed value changes. *)
  Alcotest.(check string)
    "merges across a non-conflicting intervening rule"
    "@media(width>=1px){a{background:#0b0;color:red}}a{color:green}"
    (minify_str
       "@media (width>=1px){a{color:red}}a{color:green}@media \
        (width>=1px){a{background:#0b0}}");
  (* A crossed rule that reorders the same property to a different value blocks
     the merge. *)
  Alcotest.(check string)
    "keeps blocks apart on a conflicting reorder"
    "@media(width>=1px){a{color:red}}a{color:green}@media(width>=1px){a{color:#00f}}"
    (minify_str
       "@media (width>=1px){a{color:red}}a{color:green}@media \
        (width>=1px){a{color:blue}}");
  (* A layer statement establishes cascade order; never merge across it. *)
  Alcotest.(check string)
    "no merge across a layer statement"
    "@media(width>=1px){a{color:red}}@layer \
     theme;@media(width>=1px){a{color:#00f}}"
    (minify_str
       "@media (width>=1px){a{color:red}}@layer theme;@media \
        (width>=1px){a{color:blue}}");
  (* A crossed shorthand writes the longhand slot the hoisted block writes, so
     the two do not commute even though no property name is shared: hoisting
     computes blue where the source computes green. *)
  Alcotest.(check string)
    "keeps blocks apart when a crossed shorthand writes the same slot"
    "@media(width>=1px){a{background-color:red}}a{background:#00f}@media(width>=1px){a{background-color:green}}"
    (minify_str
       "@media (width>=1px){a{background-color:red}}a{background:blue}@media \
        (width>=1px){a{background-color:green}}");
  (* CSS Nesting 1 sec. 3.4: a declaration written after a nested rule stays
     behind it, so the run between the two blocks is one more rule with the
     enclosing selector. Hoisting the second block over it would compute blue
     where the source computes green. *)
  Alcotest.(check string)
    "keeps blocks apart across a conflicting nested declarations run"
    ".a{@media(width>=1px){color:red}color:#00f;@media(width>=1px){color:green}}"
    (minify_str
       ".a{@media (width>=1px){color:red}color:blue;@media \
        (width>=1px){color:green}}");
  (* The run sets another property, so nothing an element computes depends on
     the order: the blocks still merge, and the disjoint run rejoins the rule's
     own declarations. *)
  Alcotest.(check string)
    "merges across a nested declarations run that sets another property"
    ".a{outline-color:#00f;@media(width>=1px){color:red;color:green}}"
    (minify_str
       ".a{@media (width>=1px){color:red}outline-color:blue;@media \
        (width>=1px){color:green}}");
  (* Adjacent blocks cross nothing, so the run after them keeps its place and
     the merge stands. *)
  Alcotest.(check string)
    "merges adjacent blocks inside a rule"
    ".a{@media(width>=1px){color:red;color:green}color:#00f}"
    (minify_str
       ".a{@media (width>=1px){color:red}@media \
        (width>=1px){color:green}color:blue}")

(* CSS Conditional Rules 5 sec. 5: a [@container] rule applies to an element
   when the container its name selects matches its query, so two blocks under
   one name and one query hold the same content as one block over their
   concatenation. What a distant merge has to earn is the move: hoisting the
   later block over the statements written between changes the order of
   appearance of its declarations against theirs, and an element computes that
   differently only where a pair of declarations does not commute. *)
let test_distant_container_merge () =
  (* Conditional Rules 5 sec. 5.4: the query container is the nearest ancestor
     the name selects, so [main] and [side] ask about different elements. The
     conditions are the same characters and the blocks still hold apart. *)
  Alcotest.(check string)
    "keeps distinct container names apart"
    "@container main (width>=10px){.a{color:red}}.c{background:#00f}@container \
     side (width>=10px){.b{color:green}}"
    (minify_str
       "@container main \
        (width>=10px){.a{color:red}}.c{background:blue}@container side \
        (width>=10px){.b{color:green}}");
  (* An unnamed query takes the nearest ancestor container whatever its name,
     which is not the nearest one called [main]. *)
  Alcotest.(check string)
    "keeps a named and an unnamed query apart"
    "@container main \
     (width>=10px){.a{color:red}}.c{background:#00f}@container(width>=10px){.b{color:green}}"
    (minify_str
       "@container main \
        (width>=10px){.a{color:red}}.c{background:blue}@container \
        (width>=10px){.b{color:green}}");
  (* The crossed rule writes the colour slot the hoisted block writes on a
     selector it overlaps, so hoisting computes blue where the source computes
     green. *)
  Alcotest.(check string)
    "keeps blocks apart on a conflicting reorder"
    "@container(width>=10px){.a{color:red}}.a{color:green}@container(width>=10px){.a{color:#00f}}"
    (minify_str
       "@container (width>=10px){.a{color:red}}.a{color:green}@container \
        (width>=10px){.a{color:blue}}");
  (* A layer statement establishes cascade order; never merge across it. *)
  Alcotest.(check string)
    "no merge across a layer statement"
    "@container(width>=10px){.a{color:red}}@layer \
     theme;@container(width>=10px){.b{color:#00f}}"
    (minify_str
       "@container (width>=10px){.a{color:red}}@layer theme;@container \
        (width>=10px){.b{color:blue}}");
  (* The crossed rule sets another property on another selector, so no element
     computes anything differently once the block moves before it. *)
  Alcotest.(check string)
    "merges across a non-conflicting intervening rule"
    "@container(width>=10px){.a{color:red}.b{color:green}}.c{background:#00f}"
    (minify_str
       "@container (width>=10px){.a{color:red}}.c{background:blue}@container \
        (width>=10px){.b{color:green}}")

let test_keep_bang_comment_leading () =
  (* A bang comment (/*! ... */) is preserved by minify; it stays where it was
     authored rather than being moved past the following rule. *)
  Alcotest.(check string)
    "leading bang comment stays leading" "/*! important */a{color:red}"
    (minify_str "/*! important */a{color:red}")

(* New policy: pp serializes the AST faithfully and must not change semantics.
   The litmus for where a normalization belongs: if two spellings parse to the
   SAME node it is a pure serialization choice and pp picks the shortest; if
   they parse to DIFFERENT nodes the unification is semantic and belongs in
   optimize, so pp must keep them textually distinct. The pairs below are
   semantically equal but parse to distinct nodes - a math function vs a
   literal, a named vs a hex colour, a color-mix() vs its result. *)
let normalize_pairs =
  [
    ("a{width:calc(1px + 2px)}", "a{width:3px}");
    ("a{width:calc(2 * 5px)}", "a{width:10px}");
    ("a{color:red}", "a{color:#f00}");
    ("a{color:black}", "a{color:#000}");
    ("a{color:rgb(255 255 255)}", "a{color:#fff}");
    ("a{color:rgb(255 0 0)}", "a{color:red}");
    ("a{color:hsl(0 100% 50%)}", "a{color:red}");
    ("a{color:color-mix(in srgb,red,red)}", "a{color:red}");
  ]

(* Same normalization story for gradients, basic shapes, and clip-path: each
   pair is semantically equal (a side keyword vs the matching angle, an
   explicit-defaults shape vs the bare functional form), so optimize must
   collapse it to one canonical node and pp alone must stay a fixed point.
   (Whether pp keeps these textually distinct depends on which forms the parser
   canonicalizes, so that side is asserted only for the colour/calc pairs, where
   the node distinction is certain.) The basic-shape pairs use [clip-path]:
   [shape-outside] holds its value as raw text, so nothing about it is
   normalized. *)
let gradient_shape_pairs =
  [
    ( "a{background:linear-gradient(to top,red,blue)}",
      "a{background:linear-gradient(0deg,red,blue)}" );
    ("a{clip-path:circle(closest-side at center)}", "a{clip-path:circle()}");
    ( "a{clip-path:ellipse(closest-side closest-side at center)}",
      "a{clip-path:ellipse()}" );
  ]

let assert_pp_keeps_distinct pairs =
  List.iter
    (fun (a, b) ->
      if String.equal (pp_min a) (pp_min b) then
        Alcotest.failf
          "pp collapsed distinct nodes %S and %S to %S (normalize in optimize, \
           not pp)"
          a b (pp_min a))
    pairs

let assert_optimize_unifies pairs =
  List.iter
    (fun (a, b) ->
      Alcotest.(check string)
        (Fmt.str "optimize unifies %s / %s" a b)
        (minify_str a) (minify_str b))
    pairs

let assert_pp_idempotent pairs =
  List.iter
    (fun (a, b) ->
      Alcotest.(check string)
        (Fmt.str "pp idempotent on %s" a)
        (pp_min a)
        (pp_min (pp_min a));
      Alcotest.(check string)
        (Fmt.str "pp idempotent on %s" b)
        (pp_min b)
        (pp_min (pp_min b)))
    pairs

let test_pp_keeps_distinct_nodes () =
  (* pp must keep each pair textually distinct: collapsing them is a node
     transform, which is optimize's job, not pp's. *)
  assert_pp_keeps_distinct normalize_pairs

let test_optimize_unifies_equivalent_nodes () =
  (* The flip side: optimize is the AST->AST transform that collapses each pair
     to one canonical node, so the optimized minified output matches. *)
  assert_optimize_unifies normalize_pairs

let test_optimize_unifies_gradients_shapes () =
  assert_optimize_unifies gradient_shape_pairs

let test_optimize_folds_flex_calc () =
  (* The reader holds a flex calc() unfolded and pp serializes it lexically; the
     constant fold to a <number> literal is this optimize+minify transform
     (matching lightningcss). pp alone keeps calc(1 + 2). *)
  Alcotest.(check string)
    "flex-grow constant calc folds under optimize" "a{flex-grow:3}"
    (minify_str "a{flex-grow:calc(1 + 2)}");
  Alcotest.(check string)
    "flex shorthand grow constant calc folds under optimize" "a{flex:3 1 0}"
    (minify_str "a{flex:calc(1 + 2) 1 0}")

let test_pp_picks_shortest_same_node () =
  (* Spellings that parse to the SAME node are a pure serialization choice, so
     pp emits one shortest spelling for both with no optimize pass. *)
  let same a b =
    Alcotest.(check string)
      (Fmt.str "pp canonicalizes same-node %s / %s" a b)
      (pp_min a) (pp_min b)
  in
  same "a{width:0.5px}" "a{width:.5px}";
  same "a{width:10.0px}" "a{width:10px}";
  same "a{color:#FFFFFF}" "a{color:#fff}";
  same "a{color:#AbC}" "a{color:#abc}";
  same "a{color:RED}" "a{color:red}"

let test_pp_minify_is_idempotent () =
  (* pp alone (no optimize) is a fixed point: re-parsing minified output and
     re-printing changes nothing, because pp emits canonical bytes per node. *)
  assert_pp_idempotent normalize_pairs;
  assert_pp_idempotent gradient_shape_pairs

let test_no_merge_vendor_pseudo () =
  let input =
    [
      Css.rule ~selector:Css.Selector.File_selector_button
        [ Css.Declaration.margin [ Px 4. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.margin [ Px 4. ] ];
    ]
  in
  let optimized = stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  let has_file_selector =
    Astring.String.is_infix ~affix:"::file-selector-button{" output_str
  in
  let has_foo = Astring.String.is_infix ~affix:".foo{" output_str in
  Alcotest.(check bool)
    "doesn't merge vendor pseudo-elements" true
    (has_file_selector && has_foo)

let test_no_merge_with_nested () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        ~nested:
          [
            Css.rule ~selector:Css.Selector.Hover
              [ Css.Declaration.padding [ Px 20. ] ];
          ]
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.padding [ Px 10. ] ];
    ]
  in
  let optimized = stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  let has_foo = Astring.String.is_infix ~affix:".foo{" output_str in
  let has_bar = Astring.String.is_infix ~affix:".bar{" output_str in
  Alcotest.(check bool)
    "doesn't merge rules with nested statements" true (has_foo && has_bar)

let suite =
  ( "optimize_factor",
    [
      ( "root custom sort keeps other positions",
        `Quick,
        root_custom_sort_keeps_other_positions );
      ("merge consecutive identical", `Quick, test_merge_consecutive_identical);
      ( "combine identical oklab(none) rules",
        `Quick,
        test_combine_identical_oklab_none );
      ( "no merge different declarations",
        `Quick,
        test_no_merge_different_declarations );
      ( "merge non-consecutive non-conflicting",
        `Quick,
        test_merge_non_consecutive_non_conflicting );
      ("factor shared declarations", `Quick, test_factor_shared_declarations);
      ("factor weighted intervals", `Quick, test_factor_interval_schedule);
      ("factor interval overrides", `Quick, test_factor_interval_keeps_overrides);
      ( "factor selector branch keeps later override",
        `Quick,
        test_factor_selector_branch_keeps_later_override );
      ( "factoring reaches fixpoint in one pass",
        `Quick,
        test_factoring_reaches_fixpoint );
      ( "large stylesheet reaches local fixpoint",
        `Quick,
        test_large_stylesheet_reaches_local_fixpoint );
      ( "large stylesheet normalizes vendor aliases",
        `Quick,
        test_large_stylesheet_normalizes_vendor_aliases );
      ( "large stylesheet factoring reaches fixpoint",
        `Quick,
        test_large_stylesheet_factoring_reaches_fixpoint );
      ( "prefix synthesis reaches fixpoint in one pass",
        `Quick,
        test_prefix_synthesis_reaches_fixpoint );
      ( "prefixed longhand contracts in one pass",
        `Quick,
        test_prefixed_longhand_contracts_in_one_pass );
      ( "authored dimension reaches fixpoint in one pass",
        `Quick,
        test_authored_dimension_reaches_fixpoint );
      ( "unwrapped calc leaf reaches fixpoint in one pass",
        `Quick,
        test_unwrapped_calc_leaf_reaches_fixpoint );
      ("no factor across conflict", `Quick, test_no_factor_across_conflict);
      ( "zero box side covered by shorthand",
        `Quick,
        test_zero_box_side_covered_by_shorthand );
      ( "0s transition keeps its property",
        `Quick,
        test_keep_zero_duration_transition );
      ( "var border keeps significant spaces",
        `Quick,
        test_keep_var_border_spaces );
      ( "custom value folds whitespace after a hard close-paren",
        `Quick,
        test_custom_value_close_paren_whitespace );
      ("distant @media merge when safe", `Quick, test_distant_media_merge);
      ( "distant @container merge when safe",
        `Quick,
        test_distant_container_merge );
      ( "leading bang comment stays leading",
        `Quick,
        test_keep_bang_comment_leading );
      ("pp keeps distinct nodes distinct", `Quick, test_pp_keeps_distinct_nodes);
      ( "optimize unifies equivalent nodes",
        `Quick,
        test_optimize_unifies_equivalent_nodes );
      ( "optimize unifies gradient/shape/clip-path nodes",
        `Quick,
        test_optimize_unifies_gradients_shapes );
      ("optimize folds flex calc", `Quick, test_optimize_folds_flex_calc);
      ( "pp picks shortest spelling of same node",
        `Quick,
        test_pp_picks_shortest_same_node );
      ("pp minify is idempotent", `Quick, test_pp_minify_is_idempotent);
      ("no merge vendor pseudo", `Quick, test_no_merge_vendor_pseudo);
      ("no merge with nested", `Quick, test_no_merge_with_nested);
    ] )
