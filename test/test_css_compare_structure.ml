open Css_compare_helpers

(* The canonical mode promises that split and grouped selector lists compare
   equal. Selectors 4 sec. 4.2 weighs [:is()] as its most specific argument, so
   with equal-specificity arguments [:is(a, b)] and [a, b] select the same
   elements at the same weight and are the same rule written two ways. Where the
   arguments disagree they are not: the list weighs an [a] match at (0,0,1), the
   wrapper at (0,1,0). *)
let equal_canonical_top_level_is_unwrap () =
  Alcotest.(check bool)
    ":is(a,b) and a,b are one rule" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ":is(a,b){color:red}"
       "a,b{color:red}");
  Alcotest.(check bool)
    ":is(.x,a) and .x,a are not" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ":is(.x,a){color:red}"
       ".x,a{color:red}");
  (* Sec. 4.4: neither [:where()] nor its arguments contribute specificity. *)
  Alcotest.(check bool)
    ":where(a,b) and a,b are not" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ":where(a,b){color:red}"
       "a,b{color:red}")

(* CSS Properties and Values API 1 sec. 2: registrations for different names are
   order-independent, and for the same name the last wins. Two sheets that
   register the same set differ only in the order they happened to emit them. *)
let equal_canonical_ignores_property_order () =
  let a =
    "@property --z{syntax:\"*\";inherits:false}@property \
     --a{syntax:\"*\";inherits:false}"
  in
  let b =
    "@property --a{syntax:\"*\";inherits:false}@property \
     --z{syntax:\"*\";inherits:false}"
  in
  Alcotest.(check bool)
    "@property order is not a difference" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical a b);
  (* the last registration of a name still wins, so a differing duplicate is a
     real difference *)
  Alcotest.(check bool)
    "a differing duplicate still differs" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       "@property --a{syntax:\"*\";inherits:false}@property \
        --a{syntax:\"*\";inherits:true}"
       "@property --a{syntax:\"*\";inherits:false}");
  (* @property is valid inside a conditional group rule and inside @layer, which
     is where a generator emitting a block of registrations puts them *)
  Alcotest.(check bool)
    "@property order inside @layer is not a difference" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ("@layer properties{" ^ a ^ "}")
       ("@layer properties{" ^ b ^ "}"));
  Alcotest.(check bool)
    "@property order inside @supports is not a difference" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ("@supports (color:red){" ^ a ^ "}")
       ("@supports (color:red){" ^ b ^ "}"));
  (* a run is split by any other statement, so this reordering crosses a barrier
     and stays a difference *)
  Alcotest.(check bool)
    "reordering across an intervening rule still differs" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       "@property --z{syntax:\"*\";inherits:false}.x{top:0}@property \
        --a{syntax:\"*\";inherits:false}"
       "@property --a{syntax:\"*\";inherits:false}.x{top:0}@property \
        --z{syntax:\"*\";inherits:false}")

(* Media Queries 4 sec. 2.3: [all] matches every media type, so [not all and
   (X)] and [not (X)] are the same query. Equating them is projection-only - a
   Level 3 parser rejects the shorter form, so emission keeps what it read. *)
let equal_canonical_media_not_all () =
  Alcotest.(check bool)
    "not all and (X) is not a difference against not (X)" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       "@media not all and (hover){.a{color:red}}"
       "@media not (hover){.a{color:red}}");
  (* A media type other than [all] restricts the query, so it stays. *)
  Alcotest.(check bool)
    "not screen and (X) still differs from not (X)" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       "@media not screen and (hover){.a{color:red}}"
       "@media not (hover){.a{color:red}}");
  (* Bare [not all] matches nothing, which no condition spells. *)
  Alcotest.(check bool)
    "not all still differs from not (X)" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       "@media not all{.a{color:red}}" "@media not (hover){.a{color:red}}")

(* Media Queries 4 sec. 2.4.3: [not (width >= X)] and [(width < X)] hold for the
   same widths, and the container form alike. Tailwind's compiled output writes
   a [max-*] variant the second way, lightningcss and tw the first. *)
let equal_canonical_negated_bound () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "a negated lower bound is the strict upper bound" true
    (equal "@media not all and (min-width:96rem){.a{color:red}}"
       "@media (width < 96rem){.a{color:red}}");
  Alcotest.(check bool)
    "nested under a breakpoint too" true
    (equal
       "@media not all and (min-width:64rem){@media (width >= \
        40rem){.a{color:red}}}"
       "@media (width < 64rem){@media (width >= 40rem){.a{color:red}}}");
  Alcotest.(check bool)
    "a container query alike" true
    (equal "@container (not (width >= 28rem)){.a{color:red}}"
       "@container (width < 28rem){.a{color:red}}");
  Alcotest.(check bool)
    "a different bound still differs" false
    (equal "@media not all and (min-width:96rem){.a{color:red}}"
       "@media (width < 80rem){.a{color:red}}")

(* CSS Conditional 3 sec. 2: a rule inside nested conditional group rules
   applies when every condition holds, so the order the [@media] blocks nest in
   is not part of what the rule matches. Tailwind writes a breakpoint around a
   colour-scheme block and tw the other way round, and the two render the
   same. *)
let equal_canonical_nested_media_order () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  let wide = "@media (width>=40rem)"
  and dark = "@media (prefers-color-scheme:dark)" in
  Alcotest.(check bool)
    "nesting order is not a difference" true
    (equal
       (wide ^ "{" ^ dark ^ "{.a{color:red}}}")
       (dark ^ "{" ^ wide ^ "{.a{color:red}}}"));
  Alcotest.(check bool)
    "a block closed and reopened around its nested blocks is not a difference"
    true
    (equal
       (wide ^ "{" ^ dark ^ "{.a{color:red}}.b{color:blue}" ^ dark
      ^ "{.c{color:green}}}")
       (wide ^ "{" ^ dark ^ "{.a{color:red}}}" ^ wide ^ "{.b{color:blue}}"
      ^ dark ^ "{" ^ wide ^ "{.c{color:green}}}"));
  Alcotest.(check bool)
    "a condition nested inside itself is the condition once" true
    (equal "@media (hover){@media (hover){.a{color:red}}}"
       "@media (hover){.a{color:red}}");
  (* The blue rule applies wherever the red one does, so which comes last is
     what an element computes. *)
  Alcotest.(check bool)
    "two conflicting rules keep their order" false
    (equal
       (wide ^ "{.a{color:red}}" ^ dark ^ "{" ^ wide ^ "{.a{color:blue}}}")
       (dark ^ "{" ^ wide ^ "{.a{color:blue}}}" ^ wide ^ "{.a{color:red}}"));
  Alcotest.(check bool)
    "a condition only one side applies still differs" false
    (equal (wide ^ "{" ^ dark ^ "{.a{color:red}}}") (wide ^ "{.a{color:red}}"))

(* CSS Animations 1 sec. 3: a keyframe block is a declaration block, so two
   declarations for different properties in one frame do not observe each
   other's order, as in a style rule. Tailwind writes [ping]'s last frame
   transform first and tw writes it opacity first. *)
let equal_canonical_keyframe_declaration_order () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "keyframe declaration order is not a difference" true
    (equal "@keyframes k{75%{opacity:0;transform:scale(2)}}.a{animation:k 1s}"
       "@keyframes k{75%{transform:scale(2);opacity:0}}.a{animation:k 1s}");
  (* The longhand written after its shorthand is what the frame computes. *)
  Alcotest.(check bool)
    "a shorthand and its longhand keep their order" false
    (equal "@keyframes k{to{margin:0;margin-top:1px}}.a{animation:k 1s}"
       "@keyframes k{to{margin-top:1px;margin:0}}.a{animation:k 1s}")

(* CSS Cascade 5 sec. 3.2: [all] resets [content], so which of the two a rule
   writes last decides whether a pseudo-element has any. The projection keeps
   both, since the earlier one is a fallback an engine without [all] reads, and
   the tree over the two canonical forms sees them swapped inside one rule; the
   report dropped that as the projection's own ordering churn and fell back to a
   byte diff of the forms. *)
let canonical_reports_reset_crossing_declaration () =
  let result =
    Cascade_diff.Css_compare.diff ~mode:`Canonical
      "p::after{all:unset}p::after{content:\"OCaml\"}"
      "p::after{content:\"OCaml\"}p::after{all:unset}"
  in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff d -> (
      match d.Cascade_diff.Tree_diff.rules with
      | [
       Cascade_diff.Tree_diff.Reordered
         { selector; old_declarations = Some _; new_declarations = Some _; _ };
      ] ->
          Alcotest.(check string)
            "the rule the reset crosses" "p:after" selector
      | _ -> Alcotest.fail "expected one declaration-level reorder")
  | Cascade_diff.Css_compare.String_diff _ ->
      Alcotest.fail "fell back to a byte diff of the canonical forms"
  | _ -> Alcotest.fail "expected a tree diff"

(* A declaration a conditional group repeats from the rule with the identical
   selector before it, value and all, changes nothing: an engine reading the
   guard sets the same value twice, one that does not reads the first rule
   alone. Tailwind's compiled output nests a colour twin inside its rule and
   lightningcss flattens it carrying the colour alone, where a sheet written
   flat repeats the [content]. A rule between the two that writes the property
   makes the repeat the winner for an element both match, so it stays. *)
let equal_canonical_guarded_repeat () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  let guard = "@supports (color:color-mix(in lab,red,red))" in
  Alcotest.(check bool)
    "a repeated declaration under a guard is not a difference" true
    (equal
       (".a::after{content:var(--c);--x:#000}" ^ guard
      ^ "{.a::after{content:var(--c);--x:red}}")
       (".a::after{content:var(--c);--x:#000}" ^ guard ^ "{.a::after{--x:red}}"));
  Alcotest.(check bool)
    "under a media condition too" true
    (equal ".a{color:red;top:0}@media (hover){.a{color:red;top:1px}}"
       ".a{color:red;top:0}@media (hover){.a{top:1px}}");
  Alcotest.(check bool)
    "a rule between that writes the property keeps the repeat" false
    (equal
       (".a::after{content:var(--c)}.b::after{content:none}" ^ guard
      ^ "{.a::after{content:var(--c);--x:red}}")
       (".a::after{content:var(--c)}.b::after{content:none}" ^ guard
      ^ "{.a::after{--x:red}}"));
  Alcotest.(check bool)
    "a different value under the guard still differs" false
    (equal
       (".a::after{content:var(--c)}" ^ guard
      ^ "{.a::after{content:none;--x:red}}")
       (".a::after{content:var(--c)}" ^ guard ^ "{.a::after{--x:red}}"))

(* Canonical mode judges for the browsers [--minify] targets, as the minifier
   does, unless [--enforce-spec] asks for the spec-only reading above. Every
   target implements [color-mix()], so a guard on it holds everywhere and the
   fallback written before it is dead; a target still reads [user-select] and
   [mask-composite] under their [-webkit-] spellings, so both sides carry the
   prefixed twin and a sheet that wrote it is the sheet that did not. tw writes
   lightningcss's fallbacks and prefixes where Tailwind's compiled output writes
   its own, and the two render alike on every target. *)
let equal_canonical_targets () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let guard = "@supports (color:color-mix(in lab,red,red))" in
  let twin =
    "{.a{background-color:color-mix(in oklab,var(--c) 20%,transparent)}}"
  in
  Alcotest.(check bool)
    "a fallback before a guard every target satisfies is dead" true
    (equal
       (".a{background-color:#54a2ff33}" ^ guard ^ twin)
       (".a{background-color:color-mix(in srgb,oklch(70% .1 250) \
         20%,transparent)}" ^ guard ^ twin));
  Alcotest.(check bool)
    "the guard itself is not a difference" true
    (equal (".a{color:red}" ^ guard ^ "{.a{color:blue}}") ".a{color:blue}");
  Alcotest.(check bool)
    "a prefix a target needs is on both sides" true
    (equal ".a{-webkit-user-select:none;user-select:none}"
       ".a{user-select:none}");
  Alcotest.(check bool)
    "a mask composite's prefixed vocabulary too" true
    (equal ".a{-webkit-mask-composite:source-in;mask-composite:intersect}"
       ".a{mask-composite:intersect}");
  Alcotest.(check bool)
    "and a mask mode's" true
    (equal ".a{-webkit-mask-source-type:alpha;mask-mode:alpha}"
       ".a{mask-mode:alpha}");
  Alcotest.(check bool)
    "a supports guard a target reads prefixed too" true
    (equal
       "@supports (-webkit-backdrop-filter:blur(0)) or \
        (backdrop-filter:blur(0)){.a{color:red}}"
       "@supports (backdrop-filter:blur(0)){.a{color:red}}");
  Alcotest.(check bool)
    "a guard no target satisfies still separates" false
    (equal ".a{color:red}@supports (display:wibble){.a{color:blue}}"
       ".a{color:blue}");
  Alcotest.(check bool)
    "a different fallback under an unknown guard still differs" false
    (equal ".a{color:red}@supports (display:wibble){.a{color:blue}}"
       ".a{color:green}@supports (display:wibble){.a{color:blue}}")

(* A declaration the reader refuses is one the browser refuses:
   test/spec/browser/accept_set holds the reader to the browser's accept set. A
   browser drops it from whichever sheet holds it and paints the same, so it
   separates nothing, and the pair compares by what remains while the warning
   stays as information. Tailwind writes [filter: blur(<value>)] for a docs
   placeholder class where tw writes nothing. A rule the reader dropped is text
   the comparison never saw and nothing holds the rule reader to that set, so it
   still separates unless both sides lost the same run of text. *)
let canonical_refused_declaration_separates_nothing () =
  let diff a b = Cascade_diff.Css_compare.diff ~mode:`Canonical a b in
  let separates a b = Cascade_diff.Css_compare.unread_separates (diff a b) in
  let no_diff a b =
    match (diff a b).Cascade_diff.Css_compare.result with
    | Cascade_diff.Css_compare.No_diff -> true
    | _ -> false
  in
  Alcotest.(check bool)
    "the refused declaration reaches neither side" true
    (no_diff ".a{filter:blur(<value>)}" "");
  Alcotest.(check bool)
    "the warning is kept" true
    (Cascade_diff.Css_compare.has_warnings (diff ".a{filter:blur(<value>)}" ""));
  Alcotest.(check bool)
    "a refused declaration on one side separates nothing" false
    (separates ".a{filter:blur(<value>)}" "");
  Alcotest.(check bool)
    "against an empty rule too" false
    (separates ".a{filter:blur(<value>)}" ".a{}");
  Alcotest.(check bool)
    "nor one refused on both sides in two spellings" false
    (separates ".a{filter:blur(<value>)}" ".a{filter:blur( <value> )}");
  Alcotest.(check bool)
    "a difference beside it is still a difference" false
    (no_diff ".a{filter:blur(<value>)}.b{color:red}" ".b{color:blue}");
  Alcotest.(check bool)
    "a rule one side dropped still separates" true
    (separates ".b{color:red}.a[[ {color:red}" ".b{color:red}");
  Alcotest.(check bool)
    "the same dropped rule on both sides does not" false
    (separates ".b{color:red}.a[[ {color:red}"
       ".b { color: red }.a[[ {color:red}")

(* CSS Conditional 3 sec. 6.1: a parenthesised term that is neither a
   [<supports-decl>] nor a nested condition is a [<general-enclosed>], and "the
   result is false", so a guard it makes false in every world selects no user
   agent and its block never applies. Tailwind writes [@supports
   (@media(width>=1px): var(--tw))] for [supports-[@media(width>=1px)]:flex],
   whose "declaration" has no property ident, and tw writes nothing; the two
   render alike. The projection drops the block under either reading, since no
   browser and no reading of the spec answers the guard yes. A guard a browser
   can answer stays, and so does one the term cannot decide alone. *)
let canonical_drops_unanswerable_supports () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let enforce a b =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~enforce_spec:true a b
  in
  let guard = "@supports (@media(width>=1px): var(--tw)){.x{display:flex}}" in
  Alcotest.(check bool)
    "a guard over a declaration with no property ident selects nothing" true
    (equal guard "");
  Alcotest.(check bool) "under --enforce-spec too" true (enforce guard "");
  Alcotest.(check bool)
    "a bare general-enclosed term selects nothing" true
    (equal "@supports (future syntax){.x{display:flex}}" "");
  Alcotest.(check bool)
    "and so does one conjoined with a real feature" true
    (equal
       "@supports (display:grid) and (@media(width>=1px): \
        var(--tw)){.x{display:flex}}"
       "");
  Alcotest.(check bool)
    "a guard a browser answers stays" false
    (equal "@supports (display: grid){.x{display:flex}}" "");
  Alcotest.(check bool)
    "under --enforce-spec too" false
    (enforce "@supports (display: grid){.x{display:flex}}" "");
  Alcotest.(check bool)
    "a disjunction the real feature can carry stays" false
    (equal
       "@supports (display:grid) or (@media(width>=1px): \
        var(--tw)){.x{display:flex}}"
       "");
  Alcotest.(check bool)
    "a function form stays open for the browser" false
    (equal "@supports selector(:has(+ img)){.x{display:flex}}" "")

(* Every rewrite the optimizer gates behind [~enforce_spec] is justified by what
   maintained browsers support rather than by what the two sheets say, and the
   ones that delete content leave the reader of that content - an engine without
   the feature - with a different page. A comparison projection may re-spell,
   but it cannot delete on a target assumption, so the projection takes none of
   them. *)
let canonical_keeps_target_gated_content () =
  let equal a b =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~enforce_spec:true a b
  in
  (* The declaration an engine without the guarded feature paints is the one
     before the guard, so two sheets that disagree there paint differently. *)
  Alcotest.(check bool)
    "differing fallbacks under an @supports guard differ" false
    (equal ".a{color:red;@supports (display:grid){color:blue}}"
       ".a{color:green;@supports (display:grid){color:blue}}");
  Alcotest.(check bool)
    "so do they with the guard written at the top level" false
    (equal ".a{color:red}@supports (display:grid){.a{color:blue}}"
       ".a{color:green}@supports (display:grid){.a{color:blue}}");
  (* A vendor-prefixed declaration the WHATWG Compatibility Standard sec. 3.4.1
     does not name is a private extension in the vendor's namespace and the only
     declaration an engine that reads it sees, so dropping it is not dropping a
     spelling. A prefix the section DOES name is the same property as its twin
     and folds; [canonical_legacy_name_alias] covers that side. *)
  Alcotest.(check bool)
    "an unlisted vendor-prefixed twin is not nothing" false
    (equal ".a{-webkit-user-select:none;user-select:none}"
       ".a{user-select:none}");
  (* CSS Cascade 5 sec. 3.1: [supports()] on an [@import] decides whether the
     sheet loads at all. *)
  Alcotest.(check bool)
    "an @import supports() guard is not nothing" false
    (equal "@import url(\"a.css\") supports(display:grid);"
       "@import url(\"a.css\");");
  (* Guards that agree are still no difference, spelled either way. *)
  Alcotest.(check bool)
    "matching guards agree through a respelling" true
    (equal ".a{color:red;@supports (display:grid){color:blue}}"
       ".a { color: #f00; @supports (display: grid) { color: #00f } }")

(* Cascade's configured normalization treats the WebKit spelling as a typed
   alias once an identical, widely available unprefixed declaration follows.
   Canonical comparison should project that redundant twin away without erasing
   a differing fallback or a prefixed-only declaration. *)
let canonical_drops_redundant_decoration_color_alias () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "an identical WebKit decoration-color twin is redundant" true
    (equal ".a{text-decoration-color:red}"
       ".a{-webkit-text-decoration-color:red;text-decoration-color:red}");
  Alcotest.(check bool)
    "a differing WebKit decoration-color fallback remains" false
    (equal ".a{text-decoration-color:blue}"
       ".a{-webkit-text-decoration-color:red;text-decoration-color:blue}");
  Alcotest.(check bool)
    "a prefixed-only decoration color remains" false
    (equal ".a{text-decoration-color:red}"
       ".a{-webkit-text-decoration-color:red}")

(* Media Queries 4 sec. 4.2 gives [min-X]/[max-X] and the range form one
   meaning, and cascade's own minified output writes the range form, so the fold
   has to hold on the comparison side once the projection stops taking the
   optimizer's target facts. Deleting nothing, it is a respelling and stays. *)
(* CSS Values 4 sec. 9: a function name is ASCII case-insensitive, so
   [style(...)] and [STYLE(...)] name one query and so do the two spellings of
   [scroll-state(...)]. The AST keeps the case the author wrote so emission can
   round-trip it, which leaves the projection to bring the two together, and
   cascade's own [Container.equal] already reads them as one. *)
let canonical_folds_container_function_case () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "a style() query agrees with its uppercase spelling" true
    (equal "@container STYLE(--x:1){.a{color:red}}"
       "@container style(--x:1){.a{color:red}}");
  Alcotest.(check bool)
    "and a scroll-state() query with its own" true
    (equal "@container SCROLL-STATE(stuck:top){.a{color:red}}"
       "@container scroll-state(stuck:top){.a{color:red}}");
  (* The case is all that folds: a different query is still a different one. *)
  Alcotest.(check bool)
    "a different style() property still differs" false
    (equal "@container STYLE(--x:1){.a{color:red}}"
       "@container style(--y:1){.a{color:red}}")

let canonical_folds_media_range_spellings () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "min-width agrees with the range form" true
    (equal "@media (min-width:48rem){.a{color:red}}"
       "@media (width>=48rem){.a{color:red}}");
  Alcotest.(check bool)
    "a bound pair agrees with the two-sided interval" true
    (equal "@media (min-width:1px) and (max-width:9px){.a{color:red}}"
       "@media (1px<=width<=9px){.a{color:red}}");
  Alcotest.(check bool)
    "an @container prelude folds the same way" true
    (equal "@container (min-width:48rem){.a{color:red}}"
       "@container (width>=48rem){.a{color:red}}");
  (* A different bound is still a different query. *)
  Alcotest.(check bool)
    "a different bound still differs" false
    (equal "@media (min-width:48rem){.a{color:red}}"
       "@media (width>=49rem){.a{color:red}}")

let canonical_nested_and_flattened_selectors_are_equal () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "a nested selector equals its flattened expansion" true
    (equal ".long-component-name{color:red;&:hover{color:blue}}"
       ".long-component-name{color:red}.long-component-name:hover{color:blue}");
  Alcotest.(check bool)
    "different flattened selectors remain distinct" false
    (equal ".long-component-name{color:red;&:hover{color:blue}}"
       ".long-component-name{color:red}.long-component-name:focus{color:blue}")

(* A zero-specificity nested branch ties with its parent, so the two orders are
   both cascade-significant and the projection has to place a movable at-rule
   the same way whichever spelling it meets. Minify synthesizes the nesting, so
   a sheet that disagreed with its own minified form could not certify it. *)
let canonical_movable_at_rule_ignores_nesting () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let flat =
    "@media(hover){.M mark{color:#fff}}.L a{color:#888}.L \
     a:where(.dark){color:#666}"
  in
  let nested =
    "@media(hover){.M mark{color:#fff}}.L \
     a{color:#888;&:where(.dark){color:#666}}"
  in
  Alcotest.(check bool)
    "a movable at-rule lands alike either side of a :where() nesting" true
    (equal flat nested);
  (* The pair the projection must still keep apart: swapping two tied writers of
     one property on overlapping selectors changes which one wins. *)
  Alcotest.(check bool)
    "tied writers keep their order" false
    (equal ".L a{color:#888}.L a:where(.dark){color:#666}"
       ".L a:where(.dark){color:#666}.L a{color:#888}")

(* Two adjacent rules of one selector are always safe to fold together, so a
   sheet that writes them apart and one that writes them merged are the same
   sheet. Sorting first could pull the pair apart and strand a merge that was
   there to begin with. *)
let canonical_folds_an_adjacent_same_selector_pair () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let apart =
    "@media(prefers-color-scheme:dark){.g:where(.system,.system \
     *){--t:var(--c);scrollbar-color:var(--h) var(--t)}}.w:where(.dark,.dark \
     *){--t:#ffffff1a}.w:where(.dark,.dark *){scrollbar-color:var(--h) \
     var(--t)}.b:where(.dark,.dark \
     *){background-image:linear-gradient(var(--s))}"
  in
  let merged =
    "@media(prefers-color-scheme:dark){.g:where(.system,.system \
     *){--t:var(--c);scrollbar-color:var(--h) var(--t)}}.w:where(.dark,.dark \
     *){--t:#ffffff1a;scrollbar-color:var(--h) var(--t)}.b:where(.dark,.dark \
     *){background-image:linear-gradient(var(--s))}"
  in
  Alcotest.(check bool)
    "an adjacent pair folds either way" true (equal apart merged);
  (* The pair that must stay apart: folding these would drop the first write. *)
  Alcotest.(check bool)
    "two writes of one property stay distinct" false
    (equal ".w{color:red}.w{color:blue}" ".w{color:blue}.w{color:red}")

(* Factoring writes one declaration list into every branch of a selector list,
   so the expanded copies are identical wherever they meet. A prefixed property
   beside its unprefixed twin writes one cascade slot twice, and the pairwise
   conflict test reads the two copies as tied writers of that slot; the
   projection has to see that both rules write the same thing and fold the group
   back. *)
let canonical_folds_a_factored_vendor_twin_group () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let inline =
    ".o95{--t:opacity(95%);-webkit-backdrop-filter:var(--t);backdrop-filter:var(--t)}.o100{--t:opacity(100%);-webkit-backdrop-filter:var(--t);backdrop-filter:var(--t)}"
  in
  let factored =
    ".o100,.o95{--t:opacity(95%);-webkit-backdrop-filter:var(--t);backdrop-filter:var(--t)}.o100{--t:opacity(100%)}"
  in
  Alcotest.(check bool)
    "a hoisted vendor-twin group folds back inline" true (equal inline factored);
  (* Same shape, but the group in between writes the slot with a different
     value, so the two spellings disagree on an element carrying both classes:
     [backdrop-filter] ends at [blur(1px)] on the left and at [opacity(100%)] on
     the right. *)
  Alcotest.(check bool)
    "a different write in between stays distinct" false
    (equal
       ".o100{-webkit-backdrop-filter:var(--t);backdrop-filter:var(--t)}.o95{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}.o100{--t:opacity(100%)}"
       ".o95{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}.o100{--t:opacity(100%);-webkit-backdrop-filter:var(--t);backdrop-filter:var(--t)}");
  (* Hoisting the group above the specialisation flips [--t] on [.o100]: the
     group wins it at [opacity(95%)] where the inline spelling has
     [opacity(100%)]. Folding backwards is what would equate these. *)
  Alcotest.(check bool)
    "a group written after the specialisation stays distinct" false
    (equal inline
       ".o100{--t:opacity(100%)}.o100,.o95{--t:opacity(95%);-webkit-backdrop-filter:var(--t);backdrop-filter:var(--t)}");
  (* A conditional block writes its declarations under several selectors, so
     reading its body as one declaration list says nothing about what any single
     element sees. These two blocks hold the same list under swapped selectors,
     and folding the [@media (hover)] pair across the one in between hands [.p]
     [blue] on the left and [red] on the right. *)
  Alcotest.(check bool)
    "a block keeps its distance from a block writing the same list" false
    (equal
       "@media (hover){.p{color:red}.q{color:blue}}@media \
        (min-width:1px){.q{color:red}.p{color:blue}}@media \
        (hover){.r{color:lime}}"
       "@media (min-width:1px){.q{color:red}.p{color:blue}}@media \
        (hover){.p{color:red}.q{color:blue}.r{color:lime}}")

(* The same rules, grouped into blocks differently. On the left [.b] sits in the
   last 64rem block beside the colour-scheme block; on the right it sits in the
   first, across the 80rem block, which nothing there observes: [.y:before]
   writes [background-color] on a pseudo-element [.b] cannot match, and [.x]
   writes only [color]. Tailwind writes the left, tw the right, and the two
   render alike. A block that moves as one unit and is keyed by all its rules
   together pairs [.x] with the [background-color] of [.y:before], so which
   rules the input grouped decided where [.b] could go. *)
let canonical_ignores_block_grouping () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let pair wide =
    ( String.concat ""
        [
          "@media (width>=64rem){.l{color:red}}";
          wide;
          "@media (width>=64rem){.b{background-color:black}";
          "@media (prefers-color-scheme:dark){.c{background-color:black}}}";
        ],
      String.concat ""
        [
          "@media (width>=64rem){.l{color:red}.b{background-color:black}}";
          wide;
          "@media (prefers-color-scheme:dark)";
          "{@media (width>=64rem){.c{background-color:black}}}";
        ] )
  in
  let left, right =
    pair "@media (width>=80rem){.x{color:red}.y:before{background-color:red}}"
  in
  Alcotest.(check bool)
    "a rule moved across a block that does not observe it" true
    (equal left right);
  (* Here [.x] does write [background-color], and an element carrying [.x] and
     [.b] at 80rem ends black on the left and red on the right. *)
  let left, right =
    pair
      "@media \
       (width>=80rem){.x{background-color:red}.y:before{background-color:red}}"
  in
  Alcotest.(check bool)
    "a rule moved across a block that writes its property" false
    (equal left right)

(* CSS Cascade 5 sec. 6.1 sorts declarations by layer before order of
   appearance, so where a layer block stands among unlayered rules decides no
   tie: an unlayered declaration beats a layered one wherever the block is, and
   an important one loses to it the same way. Tailwind's compiled sheet leaves
   [@layer components] among the project's unlayered rules and tw writes every
   layer first. The order of two layer blocks is the order of the layers
   themselves, and that one is kept. *)
let canonical_ignores_layer_block_position () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let dark = "@media (prefers-color-scheme:dark)" in
  Alcotest.(check bool)
    "a layer block among unlayered blocks" true
    (equal
       (String.concat ""
          [
            dark;
            "{.a{color:red}}@layer c{.p{color:red}}";
            dark;
            "{.b{color:red}}";
          ])
       (String.concat ""
          [
            "@layer c{.p{color:red}}";
            dark;
            "{.a{color:red}}";
            dark;
            "{.b{color:red}}";
          ]));
  Alcotest.(check bool)
    "a layer block either side of an unlayered rule it loses to" true
    (equal "@layer c{.p{color:red}}.p{color:blue}"
       ".p{color:blue}@layer c{.p{color:red}}");
  Alcotest.(check bool)
    "a layer block either side of an important rule it beats" true
    (equal "@layer c{.p{color:red!important}}.p{color:blue!important}"
       ".p{color:blue!important}@layer c{.p{color:red!important}}");
  Alcotest.(check bool)
    "two layer blocks swapped across an unlayered rule" false
    (equal "@layer c{.p{color:red}}.z{color:blue}@layer d{.p{color:blue}}"
       "@layer d{.p{color:blue}}.z{color:blue}@layer c{.p{color:red}}")

(* CSS Cascade 5 sec. 6.4.3 orders layers by where each is first declared, and
   sec. 6.1 sorts declarations by that order before order of appearance. A
   statement pinning [a, b] and blocks written [b] then [a] give the same order
   as blocks written [a] then [b], so the two sheets compute the same values;
   Tailwind's compiled sheet pins its layers and writes the blocks out of that
   order, and tw writes them in it. The order of appearance still decides
   between two blocks of one layer, and a layer order the two sheets spell
   differently is a difference. *)
let canonical_ignores_how_the_layer_order_is_written () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "a pin against blocks written in its order" true
    (equal "@layer a,b;@layer b{.x{color:red}}@layer a{.y{color:blue}}"
       "@layer a{.y{color:blue}}@layer b{.x{color:red}}");
  Alcotest.(check bool)
    "the same, the two layers writing one property" true
    (equal "@layer a,b;@layer b{.p{color:red}}@layer a{.p{color:blue}}"
       "@layer a{.p{color:blue}}@layer b{.p{color:red}}");
  Alcotest.(check bool)
    "two layer orders" false
    (equal "@layer b{.p{color:red}}@layer a{.p{color:blue}}"
       "@layer a{.p{color:blue}}@layer b{.p{color:red}}");
  Alcotest.(check bool)
    "two blocks of one layer swapped" false
    (equal "@layer a{.p{color:red}}@layer a{.p{color:blue}}"
       "@layer a{.p{color:blue}}@layer a{.p{color:red}}");
  Alcotest.(check bool)
    "a sublayer written as a name and as a nested block, swapped" false
    (equal "@layer a.b{.p{color:red}}@layer a{@layer b{.p{color:blue}}}"
       "@layer a{@layer b{.p{color:blue}}}@layer a.b{.p{color:red}}")

(* The projection is confluent for the layer declaration forms: a single [@layer
   a, b;] statement, per-layer statements and an empty block, and the blocks
   alone all establish the same order and compare equal. A dotted name expands
   to its prefixes on the way, and a conditional group that declares a layer is
   a barrier no name is hoisted across. The shape is Tailwind's preflight, whose
   layers are declared in one statement, against a sheet that declares them
   block by block. *)
let canonical_layer_declaration_forms_converge () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "one statement against per-layer statements and an empty block" true
    (equal
       "@layer theme,base,components,utilities;@layer \
        theme{.x{color:red}}@layer base{.y{color:blue}}@layer utilities{}"
       "@layer theme{.x{color:red}}@layer base{.y{color:blue}}@layer \
        components;@layer utilities{}");
  Alcotest.(check bool)
    "a dotted name expands to its prefixes" true
    (equal "@layer a.b;@layer a.b{x{top:0}}"
       "@layer a;@layer a.b;@layer a.b{x{top:0}}");
  Alcotest.(check bool)
    "a conditional barrier keeps the order apart" false
    (equal "@layer a;@media print{@layer b{x{top:0}}}"
       "@media print{@layer b{x{top:0}}}@layer a;")

(* Two rules under a condition and its exact negation never apply together:
   Media Queries 4 sec. 3.4 has [not] negate the whole query, CSS Conditional 3
   sec. 6.1 the whole [@supports] condition, and CSS Conditional 5 sec. 7.2
   picks the query container of [@container card (...)] by name and by the
   features queried, which its negation shares, so both ask one container. Their
   order decides nothing, whatever they write. A condition that merely differs,
   or a negation aimed at another container, keeps the order. *)
let canonical_ignores_order_under_exclusive_conditions () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let swapped a b = equal (a ^ b) (b ^ a) in
  Alcotest.(check bool)
    "a media query and its negation" true
    (swapped "@media (width>=20rem){.a{display:flex}}"
       "@media not (width>=20rem){.b{display:grid}}");
  Alcotest.(check bool)
    "the same, both writing one property on one selector" true
    (swapped "@media (width>=20rem){.p{color:red}}"
       "@media not (width>=20rem){.p{color:blue}}");
  Alcotest.(check bool)
    "a media type and its negation" true
    (swapped "@media print{.p{color:red}}" "@media not print{.p{color:blue}}");
  Alcotest.(check bool)
    "a named container query and its negation" true
    (swapped "@container card (width>=20rem){.p{color:red}}"
       "@container card not (width>=20rem){.p{color:blue}}");
  Alcotest.(check bool)
    "a supports condition and its negation" true
    (swapped "@supports (display:grid){.p{color:red}}"
       "@supports not (display:grid){.p{color:blue}}");
  Alcotest.(check bool)
    "two conditions that can both hold" false
    (swapped "@media (width>=20rem){.p{color:red}}"
       "@media (width>=30rem){.p{color:blue}}");
  Alcotest.(check bool)
    "a negation querying another container" false
    (swapped "@container card (width>=20rem){.p{color:red}}"
       "@container other not (width>=20rem){.p{color:blue}}");
  Alcotest.(check bool)
    "a negation of a different query" false
    (swapped "@media (width>=20rem){.p{color:red}}"
       "@media not (width>=30rem){.p{color:blue}}")

(* CSS Nesting 1 sec. 3.4 keeps a declaration written after a nested rule where
   the author wrote it, which only matters for a property the nested rule also
   sets. Where nothing clashes across the boundary the two spellings compute the
   same values on every element, so the projection has to bring them
   together. *)
let canonical_declaration_after_nested_rule () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "a disjoint declaration agrees either side of a nested rule" true
    (equal ".a{color:red;& b{width:1px}}" ".a{& b{width:1px}color:red}");
  Alcotest.(check bool)
    "so does one either side of a nested @media" true
    (equal ".a{width:2px;@media (hover){color:blue}}"
       ".a{@media (hover){color:blue}width:2px}");
  (* A [var()] reader writes its own property, never the one it reads, so it
     crosses a nested definition of that custom property freely. *)
  Alcotest.(check bool)
    "a var() reader crosses a nested definition of what it reads" true
    (equal ".a{width:var(--x);& b{--x:1px}}" ".a{& b{--x:1px}width:var(--x)}");
  (* The clashing pair uses the same effective selector, so its equal
     specificity makes source order decide the winner. *)
  Alcotest.(check bool)
    "a clashing declaration still differs" false
    (equal ".a{color:red;&{color:blue}}" ".a{&{color:blue}color:red}");
  Alcotest.(check bool)
    "a longhand still clashes with a crossed shorthand" false
    (equal ".a{margin-top:2px;&{margin:1px}}" ".a{&{margin:1px}margin-top:2px}")

let canonical_supports_hoisting () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "equivalent supports blocks merge after disjoint rules" true
    (equal
       ".a{color:red}@supports (color:color-mix(in \
        lab,red,red)){.a{color:color-mix(in oklab,red \
        50%,transparent)}}.x{display:block}.b{color:blue}@supports \
        (color:color-mix(in lab,red,red)){.b{color:color-mix(in oklab,blue \
        50%,transparent)}}.y{display:grid}"
       ".a{color:red}.x{display:block}.b{color:blue}.y{display:grid}@supports \
        (color:color-mix(in lab,red,red)){.a{color:color-mix(in oklab,red \
        50%,transparent)}.b{color:color-mix(in oklab,blue 50%,transparent)}}");
  Alcotest.(check bool)
    "an overlapping rule prevents supports hoisting" false
    (equal
       ".a{color:red}@supports (color:color-mix(in \
        lab,red,red)){.a{color:color-mix(in oklab,red \
        50%,transparent)}}.a{color:green}.b{color:blue}@supports \
        (color:color-mix(in lab,red,red)){.b{color:color-mix(in oklab,blue \
        50%,transparent)}}.y{display:grid}"
       ".a{color:red}.a{color:green}.b{color:blue}.y{display:grid}@supports \
        (color:color-mix(in lab,red,red)){.a{color:color-mix(in oklab,red \
        50%,transparent)}.b{color:color-mix(in oklab,blue 50%,transparent)}}");
  (* CSS Custom Properties 1 sec. 3 substitutes [var()] at computed-value time.
     The reader writes [color], not the custom property it reads, so moving it
     cannot change which [--x] declaration wins. *)
  Alcotest.(check bool)
    "a competing color write prevents crossing the guard" false
    (equal
       ".a{--x:red;color:var(--x)}@supports (color:color-mix(in \
        lab,red,red)){.a{color:blue}}"
       ".a{--x:red}@supports (color:color-mix(in \
        lab,red,red)){.a{color:blue}}.a{color:var(--x)}");
  Alcotest.(check bool)
    "a var reader crosses an independent guarded custom-property write" true
    (equal
       ".a{--x:red}.a{color:var(--x)}@supports (color:color-mix(in \
        lab,red,red)){.a{--x:blue}}"
       ".a{--x:red}@supports (color:color-mix(in \
        lab,red,red)){.a{--x:blue}}.a{color:var(--x)}")

(* WHATWG Compatibility Standard sec. 3.4.1 lists the [-webkit-] properties that
   "must be supported as legacy name aliases of the corresponding unprefixed
   property", and CSS Cascade 5 sec. 2.3 makes a legacy name alias the SAME
   property under a second name. So a listed prefixed declaration and its
   unprefixed twin carrying one value declare that property twice with that
   value, which is what declaring it once says: the pair compares equal to the
   twin alone, whatever engine reads it. Nothing about a browser target is
   assumed, so this holds under [~enforce_spec:true] too.

   A prefix the list does NOT name is a private extension in the vendor's own
   namespace and a property of its own, so it stays distinct however its value
   reads. A differing value or importance is two writes of one property and
   stays distinct as well. *)
let canonical_legacy_name_alias () =
  let case label expected a b =
    Alcotest.(check bool)
      label expected
      (Cascade_diff.Css_compare.equal ~mode:`Canonical a b)
  in
  case "a listed alias with one value is the twin alone" true
    "a{-webkit-transform:none;transform:none}" "a{transform:none}";
  case "the listed alias holds for box-shadow" true
    "a{-webkit-box-shadow:1px 1px red;box-shadow:1px 1px red}"
    "a{box-shadow:1px 1px red}";
  case "the listed alias holds for flex-wrap" true
    "a{-webkit-flex-wrap:wrap;flex-wrap:wrap}" "a{flex-wrap:wrap}";
  case "the listed alias holds for animation-delay" true
    "a{-webkit-animation-delay:1s;animation-delay:1s}" "a{animation-delay:1s}";
  case "a differing value is two writes of one property" false
    "a{-webkit-transform:none;transform:scale(2)}" "a{transform:scale(2)}";
  case "a listed alias alone is not the unprefixed property dropped" false
    "a{-webkit-transform:none}" "a{}";
  (* [-moz-] prefixes are named nowhere in the list, and neither is
     [-webkit-user-select]: each is its own property, which the spec-only
     reading keeps. For the targets, the first is a prefix none of them needs
     and the second one a target does, and either way both sides carry the same
     set. *)
  let spec label expected a b =
    Alcotest.(check bool)
      label expected
      (Cascade_diff.Css_compare.equal ~mode:`Canonical ~enforce_spec:true a b)
  in
  spec "an unlisted -moz- prefix stays distinct under the spec" false
    "a{-moz-box-sizing:border-box;box-sizing:border-box}"
    "a{box-sizing:border-box}";
  spec "an unlisted -webkit- prefix stays distinct under the spec" false
    "a{-webkit-user-select:none;user-select:none}" "a{user-select:none}";
  case "a prefix no target needs is dropped from both sides" true
    "a{-moz-box-sizing:border-box;box-sizing:border-box}"
    "a{box-sizing:border-box}";
  case "a prefix a target needs is written on both sides" true
    "a{-webkit-user-select:none;user-select:none}" "a{user-select:none}"

(* CSS Variables 1 secs. 2 and 3 make a custom property an ordinary cascade slot
   and substitute its computed value into the property containing [var()]. A
   [color] declaration therefore reads [--x] without writing it: crossing a
   guarded [--x] write is safe, while crossing a guarded [color] write is
   not. *)
let canonical_var_reader_crosses_guarded_writer () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  let condition = "(color:color-mix(in lab,red,red))" in
  Alcotest.(check bool)
    "a var() reader crosses an independent guarded writer" true
    (equal
       (String.concat ""
          [
            ".a{--x:red}.a{color:var(--x)}@supports ";
            condition;
            "{.a{--x:blue}}";
          ])
       (String.concat ""
          [
            ".a{--x:red}@supports ";
            condition;
            "{.a{--x:blue}}.a{color:var(--x)}";
          ]));
  Alcotest.(check bool)
    "a competing guarded write pins the var() reader" false
    (equal
       (String.concat ""
          [
            ".a{--x:red;color:var(--x)}@supports ";
            condition;
            "{.a{color:blue}}";
          ])
       (String.concat ""
          [
            ".a{--x:red}@supports ";
            condition;
            "{.a{color:blue}}.a{color:var(--x)}";
          ]))

(* Cascade 5 sec. 6.2: an important custom property beats a later normal one, so
   the flag is part of what the declaration means and two sheets that disagree
   about it are different sheets. *)
let canonical_important_custom_property_distinct () =
  Alcotest.(check bool)
    "an important custom property differs from a normal one" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ":root{--x:red!important}"
       ":root{--x:red}");
  (* the same importance on both sides is not a difference *)
  Alcotest.(check bool)
    "matching important custom properties agree" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ":root{--x:red!important}"
       ":root{--x:red !important}");
  (* the flag also decides which of two definitions of one name survives *)
  Alcotest.(check bool)
    "an important definition is not shadowed by a later normal one" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ":root{--x:red!important}:root{--x:blue}" ":root{--x:blue}")

(* Selectors L4 [:where()] and [:is()] take a forgiving-selector-list whose
   semantics is a union (commutative); [:where()]'s specificity is always 0 and
   [:is()]/[:not()]'s is the max of their arguments, so any permutation of the
   comma-list body matches the same elements with the same specificity. The same
   holds for [:has()]'s relative list and the [of <selector-list>] clause in
   [:nth-child()]/[:nth-last-child()]. Canonical comparison must fold those
   permutations. *)

let canonical_where_permutation_equal () =
  let expected = ".x :where(ul ul,ul ol,ol ul,ol ol){color:red}" in
  let actual = ".x :where(ol ol,ol ul,ul ol,ul ul){color:red}" in
  Alcotest.(check bool)
    ":where(...) body permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_is_permutation_equal () =
  let expected = ".x :is(.a,.b){color:red}" in
  let actual = ".x :is(.b,.a){color:red}" in
  Alcotest.(check bool)
    ":is(...) body permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_not_permutation_equal () =
  let expected = ".x :not(.a,.b){color:red}" in
  let actual = ".x :not(.b,.a){color:red}" in
  Alcotest.(check bool)
    ":not(...) body permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_has_permutation_equal () =
  let expected = ".x:has(>.a,>.b){color:red}" in
  let actual = ".x:has(>.b,>.a){color:red}" in
  Alcotest.(check bool)
    ":has(...) body permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_nth_child_of_permutation_equal () =
  let expected = ".x :nth-child(2 of .a,.b){color:red}" in
  let actual = ".x :nth-child(2 of .b,.a){color:red}" in
  Alcotest.(check bool)
    ":nth-child(... of <list>) body permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_nested_where_is_permutation_equal () =
  let expected = ".x :where(:is(.a,.b),.c){color:red}" in
  let actual = ".x :where(.c,:is(.b,.a)){color:red}" in
  Alcotest.(check bool)
    "nested :where/:is bodies canonicalize recursively" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Already canonical (regression pin): top-level selector lists. The rule
   applies to the union of matched elements and per-element specificity is the
   matching branch's, regardless of where it sits in the list. *)
let canonical_top_level_selector_list_permutation_equal () =
  let expected = ".a,.b{color:red}" in
  let actual = ".b,.a{color:red}" in
  Alcotest.(check bool)
    "top-level selector list permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Custom-property declarations in :root/:host don't have cascade-relevant
   source order (they all set the same theme variable for the matching element),
   so canonical comparison sorts them alphabetically by name. This restores the
   "all paths canonicalize the same" invariant for theme blocks regardless of
   the source's emission order (Tailwind's (priority, suborder) convention, a
   typed Var.binding constructor's order, or hand-written CSS all converge). In
   a regular rule the sort is footprint-gated instead: declarations that write
   no common cascade slot sort into a deterministic order, while overlapping
   ones (same property, shorthand and longhand) keep source order. *)

let canonical_root_custom_props_permutation_equal () =
  let expected = ":root{--zebra:1;--apple:2;--mango:3}" in
  let actual = ":root{--apple:2;--mango:3;--zebra:1}" in
  Alcotest.(check bool)
    ":root custom-property permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_host_custom_props_permutation_equal () =
  let expected = ":host{--zebra:1;--apple:2;--mango:3}" in
  let actual = ":host{--mango:3;--apple:2;--zebra:1}" in
  Alcotest.(check bool)
    ":host custom-property permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Two custom-property declarations for DIFFERENT names write disjoint cascade
   slots and resolve independently, so their order in a regular rule cannot
   change a computed value; canonical mode reorders them into a deterministic
   order. Two declarations of the SAME name still stay ordered (later wins), but
   optimize already dedups those to the last value. *)
let canonical_distinct_custom_props_permutation_equal () =
  let expected = ".x{--zebra:1;--apple:2}" in
  let actual = ".x{--apple:2;--zebra:1}" in
  Alcotest.(check bool)
    "distinct custom-property permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Regular declarations whose footprints are disjoint (they write no common
   cascade slot after shorthand expansion) can never change each other's
   computed value, so canonical mode sorts them into a deterministic order and
   permutations compare equal. *)
let canonical_disjoint_decls_permutation_equal () =
  let expected = ".x{color:red;background:blue;margin:1px}" in
  let actual = ".x{background:blue;margin:1px;color:red}" in
  Alcotest.(check bool)
    "disjoint property declaration permutations canonicalize equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* SAFETY: overlapping declarations keep their source order. A shorthand and a
   longhand that write a common slot, and two writes that expand onto a shared
   sub-property, resolve differently when swapped, so canonical mode must keep
   such permutations distinct. *)
let canonical_overlapping_decls_stay_distinct () =
  let check desc a b =
    Alcotest.(check bool)
      desc false
      (Cascade_diff.Css_compare.equal ~mode:`Canonical a b)
  in
  check "shorthand and its longhand stay ordered" ".x{margin:0;margin-top:5px}"
    ".x{margin-top:5px;margin:0}";
  check "overlapping shorthands stay ordered"
    ".x{border-top:1px solid red;border-color:blue}"
    ".x{border-color:blue;border-top:1px solid red}";
  check "background shorthand over its longhand stays ordered"
    ".x{background:blue;background-color:red}"
    ".x{background-color:red;background:blue}"

let canonical_same_selector_merge_across_intervening_rules () =
  let merged =
    "@layer \
     base{.form-select{appearance:none;background-color:#fff;border-color:#6b7280;border-width:1px;border-radius:0;padding:.5rem \
     .75rem;font-size:1rem;line-height:1.5rem;background-image:url(x);background-position:right \
     .5rem center;background-repeat:no-repeat;background-size:1.5em \
     1.5em;padding-right:2.5rem}.form-select:focus{outline:2px solid \
     transparent;outline-offset:2px;border-color:#2563eb}.form-select:where([size]:not([size=\"1\"])){background-image:none;background-position:0 \
     0;background-repeat:unset;background-size:initial;padding-right:.75rem}}"
  in
  let split =
    "@layer \
     base{.form-select{appearance:none;background-color:#fff;border-color:#6b7280;border-width:1px;border-radius:0;padding:.5rem \
     .75rem;font-size:1rem;line-height:1.5rem}.form-select:focus{outline:2px \
     solid \
     transparent;outline-offset:2px;border-color:#2563eb}.form-select{background-image:url(x);background-position:right \
     .5rem center;background-repeat:no-repeat;background-size:1.5em \
     1.5em;padding-right:2.5rem}.form-select:where([size]:not([size=\"1\"])){background-image:none;background-position:0 \
     0;background-repeat:unset;background-size:initial;padding-right:.75rem}}"
  in
  Alcotest.(check bool)
    "canonical diff reconciles split/merged same-selector forms rules" true
    (equal ~mode:`Canonical merged split)

(* A property with no typed spelling still writes cascade slots: [background]
   resets the X position [background-position-x] set, so the two orders render
   differently and the canonical projection must keep them apart. *)
let canonical_unknown_longhand_order () =
  Alcotest.(check bool)
    "an unknown longhand swapped past its shorthand is a difference" false
    (equal ~mode:`Canonical ".a{background:red;background-position-x:10px}"
       ".a{background-position-x:10px;background:red}");
  Alcotest.(check bool)
    "the same order is still equal" true
    (equal ~mode:`Canonical ".a{background:red;background-position-x:10px}"
       ".a{background:red;background-position-x:10px}")

(* [gap] resets [row-gap], so the two orders give the rule a different row gap
   and the canonical projection must keep them apart. *)
let canonical_typed_longhand_order () =
  Alcotest.(check bool)
    "a longhand swapped past the shorthand that resets it is a difference" false
    (equal ~mode:`Canonical ".a{row-gap:9px;gap:1px}" ".a{gap:1px;row-gap:9px}");
  Alcotest.(check bool)
    "the same order is still equal" true
    (equal ~mode:`Canonical ".a{row-gap:9px;gap:1px}" ".a{row-gap:9px;gap:1px}")

let suite =
  ( "css_compare_structure",
    [
      Alcotest.test_case
        "canonical same-selector merge across intervening rules" `Quick
        canonical_same_selector_merge_across_intervening_rules;
      Alcotest.test_case "canonical unknown longhand order" `Quick
        canonical_unknown_longhand_order;
      Alcotest.test_case "canonical typed longhand order" `Quick
        canonical_typed_longhand_order;
      Alcotest.test_case "canonical :where(...) permutation" `Quick
        canonical_where_permutation_equal;
      Alcotest.test_case "canonical :is(...) permutation" `Quick
        canonical_is_permutation_equal;
      Alcotest.test_case "canonical :not(...) permutation" `Quick
        canonical_not_permutation_equal;
      Alcotest.test_case "canonical :has(...) permutation" `Quick
        canonical_has_permutation_equal;
      Alcotest.test_case "canonical :nth-child(of ...) permutation" `Quick
        canonical_nth_child_of_permutation_equal;
      Alcotest.test_case "canonical nested :where/:is permutation" `Quick
        canonical_nested_where_is_permutation_equal;
      Alcotest.test_case "canonical top-level selector list permutation" `Quick
        canonical_top_level_selector_list_permutation_equal;
      Alcotest.test_case "canonical :root custom-props alphabetised" `Quick
        canonical_root_custom_props_permutation_equal;
      Alcotest.test_case "canonical :host custom-props alphabetised" `Quick
        canonical_host_custom_props_permutation_equal;
      Alcotest.test_case "canonical distinct custom-props canonicalize equal"
        `Quick canonical_distinct_custom_props_permutation_equal;
      Alcotest.test_case "canonical disjoint decls canonicalize equal" `Quick
        canonical_disjoint_decls_permutation_equal;
      Alcotest.test_case "canonical overlapping decls stay distinct" `Quick
        canonical_overlapping_decls_stay_distinct;
      Alcotest.test_case "canonical folds a disjoint trailing declaration"
        `Quick canonical_declaration_after_nested_rule;
      Alcotest.test_case "canonical supports hoisting" `Quick
        canonical_supports_hoisting;
      Alcotest.test_case "canonical legacy name alias" `Quick
        canonical_legacy_name_alias;
      Alcotest.test_case "canonical var reader crosses guarded writer" `Quick
        canonical_var_reader_crosses_guarded_writer;
      Alcotest.test_case "canonical keeps custom-property importance" `Quick
        canonical_important_custom_property_distinct;
      Alcotest.test_case "canonical ignores @property order" `Quick
        equal_canonical_ignores_property_order;
      Alcotest.test_case "canonical splits a top-level :is()" `Quick
        equal_canonical_top_level_is_unwrap;
      Alcotest.test_case "canonical equates not all and (X) with not (X)" `Quick
        equal_canonical_media_not_all;
      Alcotest.test_case "canonical equates a negated bound" `Quick
        equal_canonical_negated_bound;
      Alcotest.test_case "canonical reports a reset crossing a declaration"
        `Quick canonical_reports_reset_crossing_declaration;
      Alcotest.test_case "canonical guarded repeat" `Quick
        equal_canonical_guarded_repeat;
      Alcotest.test_case "canonical nested media order" `Quick
        equal_canonical_nested_media_order;
      Alcotest.test_case "canonical keyframe declaration order" `Quick
        equal_canonical_keyframe_declaration_order;
      Alcotest.test_case "canonical keeps target-gated content" `Quick
        canonical_keeps_target_gated_content;
      Alcotest.test_case "canonical judges for the targets" `Quick
        equal_canonical_targets;
      Alcotest.test_case "canonical refused declaration separates nothing"
        `Quick canonical_refused_declaration_separates_nothing;
      Alcotest.test_case "canonical drops an unanswerable @supports" `Quick
        canonical_drops_unanswerable_supports;
      Alcotest.test_case "canonical drops redundant decoration-color alias"
        `Quick canonical_drops_redundant_decoration_color_alias;
      Alcotest.test_case "canonical folds container function case" `Quick
        canonical_folds_container_function_case;
      Alcotest.test_case "canonical folds media range spellings" `Quick
        canonical_folds_media_range_spellings;
      Alcotest.test_case "canonical nested and flattened selectors" `Quick
        canonical_nested_and_flattened_selectors_are_equal;
      Alcotest.test_case "canonical movable at-rule ignores nesting" `Quick
        canonical_movable_at_rule_ignores_nesting;
      Alcotest.test_case "canonical folds an adjacent same-selector pair" `Quick
        canonical_folds_an_adjacent_same_selector_pair;
      Alcotest.test_case "canonical folds a factored vendor-twin group" `Quick
        canonical_folds_a_factored_vendor_twin_group;
      Alcotest.test_case "canonical ignores how a block groups its rules" `Quick
        canonical_ignores_block_grouping;
      Alcotest.test_case "canonical ignores where a layer block stands" `Quick
        canonical_ignores_layer_block_position;
      Alcotest.test_case "canonical ignores how the layer order is written"
        `Quick canonical_ignores_how_the_layer_order_is_written;
      Alcotest.test_case "canonical layer declaration forms converge" `Quick
        canonical_layer_declaration_forms_converge;
      Alcotest.test_case "canonical ignores order under exclusive conditions"
        `Quick canonical_ignores_order_under_exclusive_conditions;
    ] )
