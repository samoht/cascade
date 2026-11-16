(** Stylesheet tests: dead declarations, layers, vendor prefixes and selector
    forms under minify. *)

open Cascade
open Stylesheet_helpers

(* CSS Cascading and Inheritance Module Level 5, section 6.1 (Cascade Sorting
   Order): when two declarations of the same property tie on every higher-
   priority criterion, only the later wins. The earlier declaration is dead and
   may be removed. Both Lightning CSS and cssnano drop dead duplicates for
   shorthand properties; for differing-value duplicates of the same longhand the
   spec leaves the choice to the implementation. *)
let c6_1_dead_shorthand_removed () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "shadowed margin shorthand removed" ".x{margin:5px}"
    (normalize ".x { margin: 10px; margin: 5px }");
  Alcotest.(check string)
    "exact duplicate property collapsed" ".x{color:red}"
    (normalize ".x { color: red; color: red }")

(* CSS Cascading and Inheritance Module Level 5, section 6.1: an empty
   declaration block contributes no declared values to the cascade. The rule may
   be removed entirely as a no-op. Both minifiers drop empty rules. *)
let c6_1_empty_rule_removed () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "empty rule before populated rule is dropped" ".y{color:red}"
    (normalize ".x { } .y { color: red }");
  Alcotest.(check string)
    "empty rule after populated rule is dropped" ".y{color:red}"
    (normalize ".y { color: red } .x { }")

(* CSS Cascading and Inheritance Module Level 5, section 7.3 (CSS-Wide
   Keywords): the keywords [initial], [inherit], [unset], [revert], and
   [revert-layer] are valid for every property. Implementations must preserve
   them through serialization since they have observable cascade semantics that
   no shorter spelling captures. *)
let c67_css_wide_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let cases = [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] in
  List.iter
    (fun kw ->
      let css = String.concat "" [ ".x { color: "; kw; " }" ] in
      let expected = String.concat "" [ ".x{color:"; kw; "}" ] in
      Alcotest.(check string)
        (String.concat "" [ kw; " preserved through minify" ])
        expected (normalize css))
    cases

(* Cascade L5 SS 7.3: CSS-wide keywords are whole-property values, invalid
   inside lists like [font-family: Arial, inherit]. Per the dual-mode contract
   ([cross_mode_pinning]) lenient parsing drops the declaration and warns;
   nothing remains to preserve in either pretty or minified output. *)
let c67_bad_css_wide_list () =
  let minified css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        parsed.stylesheet
        |> Css.optimize ~flatten_nesting:true
        |> Css.to_string ~minify:true
    | Error e -> Alcotest.fail (Cascade.Error.to_string e)
  in
  Alcotest.(check string)
    "invalid CSS-wide list item drops under minify" ".y{color:red}"
    (minified ".x { font-family: Arial, inherit }.y { color: red }")

(* CSS Cascading and Inheritance Module Level 5, section 3.2 (The all
   Shorthand): the [all] property is a shorthand that sets every CSS-wide
   keyword for all properties. It only accepts CSS-wide keywords as values and
   must round-trip through the printer unchanged. *)
let c632_all_shorthand_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "all: unset preserved" ".x{all:unset}"
    (normalize ".x { all: unset }");
  Alcotest.(check string)
    "all: revert preserved" ".x{all:revert}"
    (normalize ".x { all: revert }");
  Alcotest.(check string)
    "all: initial preserved" ".x{all:initial}"
    (normalize ".x { all: initial }")

(* CSS Cascading and Inheritance Module Level 5, section 6.4 (Cascade Layers):
   named layers preserve their declared order. Two non-adjacent [@layer base]
   blocks separated by an [@layer theme] block still contribute to the same
   layer, so they may be serialized in one [base] block at the first [base]
   occurrence. *)
let c64_named_layers_order () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let output =
    normalize
      "@layer base { .a { color: red } } @layer theme { .a { color: blue } } \
       @layer base { .a { padding: 10px } }"
  in
  Alcotest.(check bool)
    "@layer base keeps color declaration" true
    (Astring.String.is_infix ~affix:"color:red" output);
  Alcotest.(check bool)
    "@layer theme block remains" true
    (Astring.String.is_infix ~affix:"color:#00f" output);
  Alcotest.(check bool)
    "@layer base keeps padding declaration" true
    (Astring.String.is_infix ~affix:"padding:10px" output);
  Alcotest.(check bool)
    "same-name @layer base blocks are serialized together before theme" true
    (let find sub = Astring.String.find_sub ~sub output in
     match (find "color:red", find "color:#00f", find "padding:10px") with
     | Some r, Some t, Some p -> r < p && p < t
     | _ -> false)

(* CSS Cascade L5 section 2.1 (Conditional @import) and CSS Custom Properties L1
   section 2 (var()): a [var()] reference with a fallback must be preserved
   end-to-end. The optimizer cannot resolve [var(--undef, red)] to [red] without
   a context that says [--undef] is undefined - that's a cascade-time fact, not
   a syntax-time one. *)
let css_var_fallback_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var() with fallback preserved" ".x{color:var(--undef,red)}"
    (normalize ".x { color: var(--undef, red) }");
  Alcotest.(check string)
    "nested var() preserved" ".x{color:var(--a,var(--b,red))}"
    (normalize ".x { color: var(--a, var(--b, red)) }")

(* CSS Cascade L5 section 6.4.2.1 (anonymous @layer): two anonymous layers are
   distinct - they must NOT merge, because the spec states that each [@layer {
   ... }] without a name creates a new, independent layer. *)
let c644_anonymous_layers_distinct () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let output =
    normalize "@layer { .a { color: red } } @layer { .b { color: blue } }"
  in
  let layer_count =
    let rec count from acc =
      match Astring.String.find_sub ~start:from ~sub:"@layer" output with
      | None -> acc
      | Some i -> count (i + 1) (acc + 1)
    in
    count 0 0
  in
  Alcotest.(check int)
    "two anonymous @layer blocks remain as two distinct blocks" 2 layer_count

(* {2 Fidelity tests for dead-code and layer edges} *)

let fidelity_dead_property_preserved () =
  pretty_preserves ".x { color: red; color: blue }"
    [ "color: red"; "color: blue" ];
  pretty_preserves ".x { margin: 10px; margin: 5px }"
    [ "margin: 10px"; "margin: 5px" ]

let fidelity_empty_rule_preserved () =
  (* Empty rules carry no declarations and are dropped even in pretty mode,
     matching Lightning CSS / cssnano. *)
  pretty_preserves ".x { } .y { color: red }" [ ".y" ]

let fidelity_incomplete_font_face_preserved () =
  pretty_preserves "@font-face { src: url(font.woff2); }"
    [ "@font-face"; "src: url(font.woff2)" ];
  pretty_preserves "@font-face { font-family: Brand; }"
    [ "@font-face"; "font-family: Brand" ]

let fidelity_css_wide_keywords_preserved () =
  pretty_preserves ".x { color: revert }" [ "revert" ];
  pretty_preserves ".x { color: revert-layer }" [ "revert-layer" ];
  pretty_preserves ".x { color: unset }" [ "unset" ];
  pretty_preserves ".x { all: initial }" [ "all"; "initial" ]

(* CSS Backgrounds and Borders Module Level 3, section 2.6
   (background-position): the keyword pairs [top left] / [left top] denote the
   same position as [0% 0%], and [bottom right] denotes [100% 100%]. Both
   Lightning CSS and cssnano canonicalize the keyword form to the numeric
   one. *)
let bg336_position_keyword () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  (* Per CSS Backgrounds L3 sec. 2.6 [top left], [left top], and [0% 0%] all
     denote position [0 0]. *)
  Alcotest.(check string)
    "background-position: top left -> 0 0" ".x{background-position:0 0}"
    (normalize ".x { background-position: top left }");
  Alcotest.(check string)
    "background-position: left top -> 0 0" ".x{background-position:0 0}"
    (normalize ".x { background-position: left top }");
  Alcotest.(check string)
    "background-position: bottom right -> 100% 100%"
    ".x{background-position:100%100%}"
    (normalize ".x { background-position: bottom right }")

(* CSS Cascade L5 section 6.1: when two rules with different selectors share the
   same declaration block, they may be grouped into a selector list with one
   block. The cascade evaluates the grouped form identically because each
   selector contributes the same declared values at its own specificity. Both
   minifiers group same-block rules. *)
let c6_1_selector_grouping () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let grouped =
    normalize ".a { color: red } .b { color: red } .c { color: red }"
  in
  Alcotest.(check bool)
    "consecutive same-block rules group into a selector list" true
    (Astring.String.is_infix ~affix:".a,.b,.c" grouped
    || Astring.String.is_infix ~affix:".a, .b, .c" grouped);
  Alcotest.(check bool)
    "the grouped block has only one declaration appearance" true
    (let rec count from acc =
       match Astring.String.find_sub ~start:from ~sub:"color:red" grouped with
       | None -> acc
       | Some i -> count (i + 1) (acc + 1)
     in
     count 0 0 = 1)

(* CSS Syntax L3 section 4.3.9 (Check if three code points would start an ident
   sequence): vendor-prefixed properties such as [-webkit-transform] and
   [-moz-user-select] use the dashed-ident escape hatch and are unknown to the
   CSS spec. The printer must round-trip them unchanged - both Lightning CSS and
   cssnano keep vendor prefixes. *)
let vendor_prefix_preservation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "-webkit-transform preserved" ".x{-webkit-transform:rotate(45deg)}"
    (normalize ".x { -webkit-transform: rotate(45deg) }");
  Alcotest.(check string)
    "-moz-user-select preserved" ".x{-moz-user-select:none}"
    (normalize ".x { -moz-user-select: none }")

(* CSS Text Decoration 3 section 2.3 defines [text-decoration-color] as the
   standards-track property. [-webkit-text-decoration-color] is compatibility
   syntax: preserve it when authored, but do not synthesize it from the standard
   property or collapse authored prefixed/unprefixed pairs. Lightning CSS,
   esbuild, CSSO, clean-css, and cssnano all keep both spellings distinct. *)
let webkit_decoration_color_compat () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "standard text-decoration-color does not synthesize webkit"
    ".x{text-decoration-color:#00f}"
    (normalize ".x { text-decoration-color: blue }");
  Alcotest.(check string)
    "authored webkit text-decoration-color is preserved"
    ".x{-webkit-text-decoration-color:#00f}"
    (normalize ".x { -webkit-text-decoration-color: blue }");
  Alcotest.(check string)
    "webkit and standard pair remains in source order"
    ".x{-webkit-text-decoration-color:red;text-decoration-color:#00f}"
    (normalize
       ".x { -webkit-text-decoration-color: red; text-decoration-color: blue }");
  Alcotest.(check string)
    "standard and webkit pair remains in source order"
    ".x{text-decoration-color:#00f;-webkit-text-decoration-color:red}"
    (normalize
       ".x { text-decoration-color: blue; -webkit-text-decoration-color: red }");
  Alcotest.(check string)
    "webkit text-decoration inherit fallback survives stylesheet optimize"
    ".x{-webkit-text-decoration:inherit;text-decoration:inherit}"
    (normalize
       ".x { -webkit-text-decoration: inherit; text-decoration: inherit }")

(* CSS Selectors Level 4, section 4.3 (Negation Pseudo-class): the two forms
   [:not(A, B)] and [:not(A):not(B)] are spec-distinct - the first is a single
   negation matching anything that's not A or B, the second chains two
   negations. Both forms must round-trip preserved through the minifier (both
   Lightning CSS and cssnano keep them). *)
let s443_not_form_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":not(.a, .b) preserved" ".x :not(.a,.b){color:red}"
    (normalize ".x :not(.a, .b) { color: red }");
  Alcotest.(check string)
    ":not(.a):not(.b) preserved" ".x :not(.a):not(.b){color:red}"
    (normalize ".x :not(.a):not(.b) { color: red }")

(* CSS Backgrounds and Borders Module Level 3 (border shorthand) and Outline
   Module (outline shorthand): keyword shortcuts like [border: 0], [border:
   none], [outline: 0], [outline: none] and Text Decoration L3 4.1
   [text-decoration: none] must round-trip unchanged. Both Lightning CSS and
   cssnano preserve these forms. *)
let display3_border_keyword_preservation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "border: 0 preserved" ".x{border:0}"
    (normalize ".x { border: 0 }");
  Alcotest.(check string)
    "border: none preserved" ".x{border:none}"
    (normalize ".x { border: none }");
  Alcotest.(check string)
    "outline: 0 preserved" ".x{outline:0}"
    (normalize ".x { outline: 0 }");
  Alcotest.(check string)
    "outline: none preserved" ".x{outline:none}"
    (normalize ".x { outline: none }");
  Alcotest.(check string)
    "text-decoration: none preserved" ".x{text-decoration:none}"
    (normalize ".x { text-decoration: none }")

(* {2 Fidelity tests for the new edges} *)

let fidelity_position_keywords_preserved () =
  pretty_preserves ".x { background-position: top left }" [ "top left" ];
  pretty_preserves ".x { background-position: bottom right }" [ "bottom right" ];
  pretty_preserves ".x { transform-origin: center center }" [ "center center" ]

let fidelity_vendor_prefix_preserved () =
  pretty_preserves ".x { -webkit-transform: rotate(45deg) }"
    [ "-webkit-transform" ];
  pretty_preserves ".x { -moz-user-select: none }" [ "-moz-user-select" ];
  pretty_preserves
    ".x { -webkit-text-decoration-color: red; text-decoration-color: blue }"
    [ "-webkit-text-decoration-color"; "text-decoration-color" ]

let fidelity_not_form_preserved () =
  pretty_preserves ".x :not(.a, .b) { color: red }" [ ":not(.a, .b)" ];
  pretty_preserves ".x :not(.a):not(.b) { color: red }" [ ":not(.a):not(.b)" ]

(* CSS Selectors L4 section 4.2 (:is()): a single-argument [:is(.x)] matches the
   same elements as bare [.x] with the same specificity. Per shortest-wins
   cascade picks the unwrapped form. *)
let s417_is_unwrap () =
  (* CSS Selectors L4 sec. 4.2: a single-argument [:is(.a)] is spec-equivalent
     to bare [.a] (same match set, same specificity). Under [~minify:true] the
     printer picks the shortest spec-equivalent spelling - the unwrapped form,
     matching Lightning CSS. Non-minified output preserves the wrapper (see
     fidelity_nested_is_preserved). *)
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":is(:is(.a)) unwraps to .a"
    (normalize ".x .a { color: red }")
    (normalize ".x :is(:is(.a)) { color: red }");
  Alcotest.(check string)
    ":not(:is(.a)) unwraps inner :is to :not(.a)"
    (normalize ".x :not(.a) { color: red }")
    (normalize ".x :not(:is(.a)) { color: red }");
  Alcotest.(check string)
    ":is(.a):is(.b) unwraps to .a.b"
    (normalize ".x .a.b { color: red }")
    (normalize ".x :is(.a):is(.b) { color: red }")

(* CSS Selectors L4 section 4.2 again, at the top of a rule selector: [:is()]
   takes the specificity of its most specific argument, so equal-specificity
   arguments make [:is(a, b)] and the selector list [a, b] the same rule. Two
   rules with the same selector are one rule, and factoring reads selector
   nodes, so the split has to be an AST rewrite: printed only, the two rules
   below show one selector, never merge, and a second pass over the output finds
   the merge the first missed. *)
let s417_is_unwrap_top_level () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let two_rules = ":is(a, b) { color: red } a, b { margin: 0 }" in
  Alcotest.(check string)
    ":is(a,b) and a,b are one rule" "a,b{color:red;margin:0}"
    (normalize two_rules);
  Alcotest.(check string)
    "minify is a fixpoint" (normalize two_rules)
    (normalize (normalize two_rules));
  (* A lone argument has nothing to disagree with, and at the top of a rule
     selector it is not landing in a compound, so section 3.1 (a type or
     universal selector comes first in a compound) does not hold it back. *)
  Alcotest.(check string)
    ":is(a) unwraps at the top" "a{color:red}"
    (normalize ":is(a) { color: red }");
  Alcotest.(check string)
    ":is(*) unwraps at the top" "*{color:red}"
    (normalize ":is(*) { color: red }");
  (* Unequal specificity, so the list would weigh an [a] match lighter than the
     wrapper does. *)
  Alcotest.(check string)
    ":is(.x,a) keeps its wrapper" ":is(.x,a){color:red}"
    (normalize ":is(.x, a) { color: red }");
  (* [:where()] contributes zero specificity, so it never becomes a list. *)
  Alcotest.(check string)
    ":where(a,b) keeps its wrapper" ":where(a,b){color:red}"
    (normalize ":where(a, b) { color: red }")

(* CSS Selectors L4 section 4.2 (compound selector): "if it contains a type
   selector or universal selector, that selector must come first". So a
   single-argument [:is(<type>)] unwraps only into a compound it leads: spliced
   anywhere else the two names fuse, and [.a:is(code)] turns into [.acode],
   which matches a class nobody wrote. Same for [:not(:not(<type>))]. *)
let s442_compound_type_first () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ".a:is(code) keeps its wrapper" ".x .a:is(code){color:red}"
    (normalize ".x .a:is(code) { color: red }");
  Alcotest.(check string)
    "div:is(code) keeps its wrapper" ".x div:is(code){color:red}"
    (normalize ".x div:is(code) { color: red }");
  Alcotest.(check string)
    ":is(<ancestor> *):is(code) keeps its wrapper"
    ":is(.a *):is(code){color:red}"
    (normalize ":is(.a *):is(code) { color: red }");
  Alcotest.(check string)
    ":is(code.b) keeps its wrapper" ".a:is(code.b){color:red}"
    (normalize ".a:is(code.b) { color: red }");
  Alcotest.(check string)
    "a namespaced universal keeps its wrapper" ".a:is(svg|*){color:red}"
    (normalize ".a:is(svg|*) { color: red }");
  Alcotest.(check string)
    ":not(:not(code)) keeps its wrappers" ".a:not(:not(code)){color:red}"
    (normalize ".a:not(:not(code)) { color: red }");
  (* Leading its compound the type selector would be where it belongs, and
     unwrapping there is sound - but the rewrite is node-local, so it declines
     rather than guess at a position it cannot see. *)
  Alcotest.(check string)
    ":is(code) leading a compound keeps its wrapper too"
    ".x :is(code).a{color:red}"
    (normalize ".x :is(code).a { color: red }");
  Alcotest.(check string)
    ":is(*) keeps its wrapper" ".a:is(*){color:red}"
    (normalize ".a:is(*) { color: red }");
  (* An argument with no type selector is spliceable wherever the [:is()]
     stands, so those keep unwrapping. *)
  Alcotest.(check string)
    ":is(.b) still unwraps" ".a.b{color:red}"
    (normalize ".a:is(.b) { color: red }");
  Alcotest.(check string)
    ":not(:not(.b)) still folds" ".a.b{color:red}"
    (normalize ".a:not(:not(.b)) { color: red }")

(* CSS Selectors L4 section 6.1: attribute compound selectors drop quotes per
   attribute when the value is a valid identifier. *)
let s462_compound_attr () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "[data-x=\"a\"][data-y=\"b\"] drops quotes per attribute"
    ".x [data-x=a][data-y=b]{color:red}"
    (normalize ".x [data-x=\"a\"][data-y=\"b\"] { color: red }")

(* CSS Selectors L4 section 3.1 (compound selector): chained pseudo- classes
   preserve their order and do not deduplicate. *)
let s442_compound_pseudo_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":hover:focus:active preserved" ".x :hover:focus:active{color:red}"
    (normalize ".x :hover:focus:active { color: red }")

(* CSS Transforms L1 section 8: multiple transform functions stack in source
   order; whitespace between them is optional for parsing. Lightning CSS drops
   it; per shortest-wins cascade follows. *)
let transforms1_11_chain_whitespace_dropped () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "transform chain whitespace dropped"
    ".x{transform:translate(10px)translate(20px)}"
    (normalize ".x { transform: translate(10px) translate(20px) }")

let fidelity_nested_is_preserved () =
  pretty_preserves ".x :is(:is(.a)) { color: red }" [ ":is(:is(.a))" ];
  pretty_preserves ".x :not(:is(.a)) { color: red }" [ ":not(:is(.a))" ];
  pretty_preserves ".x :is(.a):is(.b) { color: red }" [ ":is(.a):is(.b)" ]

let fidelity_compound_pseudo_preserved () =
  pretty_preserves ".x :hover:focus:active { color: red }"
    [ ":hover:focus:active" ]

let fidelity_transform_chain_preserved () =
  pretty_preserves ".x { transform: translate(10px) translate(20px) }"
    [ "translate(10px) translate(20px)" ]

let suite =
  ( "stylesheet_elision",
    [
      ("spec selectors 4 17 :is single-argument unwrap", `Quick, s417_is_unwrap);
      ( "spec selectors 4 17 :is top-level unwrap",
        `Quick,
        s417_is_unwrap_top_level );
      ( "spec selectors 4 4.2 compound type selector first",
        `Quick,
        s442_compound_type_first );
      ( "spec selectors 4 6.2 compound attribute canonicalization",
        `Quick,
        s462_compound_attr );
      ( "spec selectors 4 4.2 compound pseudo preserved",
        `Quick,
        s442_compound_pseudo_kept );
      ( "spec transforms 1 11 chain whitespace dropped",
        `Quick,
        transforms1_11_chain_whitespace_dropped );
      ("fidelity nested :is preserved", `Quick, fidelity_nested_is_preserved);
      ( "fidelity compound pseudo preserved",
        `Quick,
        fidelity_compound_pseudo_preserved );
      ( "fidelity transform chain preserved",
        `Quick,
        fidelity_transform_chain_preserved );
      ( "spec bg 3 3.6 position keyword canonicalization",
        `Quick,
        bg336_position_keyword );
      ("spec cascade 6.1 selector grouping", `Quick, c6_1_selector_grouping);
      ("vendor prefix preservation", `Quick, vendor_prefix_preservation);
      ( "vendor text-decoration-color compatibility",
        `Quick,
        webkit_decoration_color_compat );
      ("spec selectors 4 4.3 not form preserved", `Quick, s443_not_form_kept);
      ( "spec display 3 border keyword preservation",
        `Quick,
        display3_border_keyword_preservation );
      ( "fidelity position keywords preserved",
        `Quick,
        fidelity_position_keywords_preserved );
      ( "fidelity vendor prefix preserved",
        `Quick,
        fidelity_vendor_prefix_preserved );
      ("fidelity not form preserved", `Quick, fidelity_not_form_preserved);
      ( "fidelity dead property preserved",
        `Quick,
        fidelity_dead_property_preserved );
      ("fidelity empty rule preserved", `Quick, fidelity_empty_rule_preserved);
      ( "fidelity css-wide keywords preserved",
        `Quick,
        fidelity_css_wide_keywords_preserved );
      ( "fidelity incomplete font-face preserved",
        `Quick,
        fidelity_incomplete_font_face_preserved );
      ("css var fallback preserved", `Quick, css_var_fallback_preserved);
      ( "spec cascade 6.4.4 anonymous layers distinct",
        `Quick,
        c644_anonymous_layers_distinct );
      ( "spec cascade 6.3.2 all shorthand preserved",
        `Quick,
        c632_all_shorthand_kept );
      ( "spec cascade 6.4 named layers preserve order",
        `Quick,
        c64_named_layers_order );
      ( "spec cascade 6.1 dead shorthand removed",
        `Quick,
        c6_1_dead_shorthand_removed );
      ("spec cascade 6.1 empty rule removed", `Quick, c6_1_empty_rule_removed);
      ("spec cascade 6.7 css-wide keywords preserved", `Quick, c67_css_wide_kept);
      ( "spec cascade 6.7 css-wide keyword inside list",
        `Quick,
        c67_bad_css_wide_list );
    ] )
