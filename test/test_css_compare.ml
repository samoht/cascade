(** Tests for Cascade_diff.Css_compare module *)

open Cascade

let () = ignore (Css.of_string ~strict:false "")

let contains_substring s needle =
  let len = String.length s in
  let needle_len = String.length needle in
  let rec loop i =
    if needle_len = 0 then true
    else if i + needle_len > len then false
    else if String.sub s i needle_len = needle then true
    else loop (i + 1)
  in
  loop 0

(* ===== equal tests ===== *)

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

let equal_identical () =
  let css = ".a { color: red }" in
  Alcotest.(check bool)
    "identical CSS compares equal" true
    (Cascade_diff.Css_compare.equal css css)

let equal_different () =
  Alcotest.(check bool)
    "different CSS not equal" false
    (Cascade_diff.Css_compare.equal ".a { color: red }" ".a { color: blue }")

let equal_empty () =
  Alcotest.(check bool)
    "empty strings equal" true
    (Cascade_diff.Css_compare.equal "" "")

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

(* Every rewrite the optimizer gates behind [~enforce_spec] is justified by what
   maintained browsers support rather than by what the two sheets say, and the
   ones that delete content leave the reader of that content - an engine without
   the feature - with a different page. A comparison projection may re-spell,
   but it cannot delete on a target assumption, so the projection takes none of
   them. *)
let canonical_keeps_target_gated_content () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
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
  (* A vendor-prefixed declaration is the only one an engine that needs the
     prefix reads, so dropping it is not dropping a spelling. *)
  Alcotest.(check bool)
    "a vendor-prefixed twin is not nothing" false
    (equal ".a{-webkit-transition:all 1s;transition:all 1s}"
       ".a{transition:all 1s}");
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

(* CSS Color 4 sec. 10.2: [color(srgb r g b)] scales each channel by 255, so
   [color(srgb 1 0 0)] and [rgb(255 0 0)] are one colour in two spellings.
   [--lossless] keeps a modern colour function on output, which leaves the
   projection reading the spelling as a difference. The fold is exact-only: a
   channel that does not land on a whole byte, and a colour in another gamut,
   stay distinct. *)
let equal_canonical_lossless_exact_srgb () =
  let equal a b =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~lossless:true a b
  in
  Alcotest.(check bool)
    "color(srgb 1 0 0) is rgb(255 0 0)" true
    (equal ".a{color:color(srgb 1 0 0)}" ".a{color:rgb(255,0,0)}");
  Alcotest.(check bool)
    "color(srgb 1 0 0) is #f00" true
    (equal ".a{color:color(srgb 1 0 0)}" ".a{color:#f00}");
  Alcotest.(check bool)
    "color(srgb .2 .4 .6) is rgb(51 102 153)" true
    (equal ".a{color:color(srgb .2 .4 .6)}" ".a{color:rgb(51,102,153)}");
  Alcotest.(check bool)
    "the alpha scales exactly too" true
    (equal ".a{color:color(srgb 1 0 0/.5)}" ".a{color:rgba(255,0,0,.5)}");
  (* The spelling folds wherever the declaration sits: an at-rule that groups
     rules holds the same declarations a caller could have written at the top
     level. *)
  Alcotest.(check bool)
    "the fold reaches inside @starting-style" true
    (equal "@starting-style{.a{color:color(srgb 1 0 0)}}"
       "@starting-style{.a{color:#f00}}");
  Alcotest.(check bool)
    "the fold reaches inside @scope" true
    (equal "@scope (.p){.a{color:color(srgb 1 0 0)}}"
       "@scope (.p){.a{color:#f00}}");
  (* display-p3 red is a different colour, not a different spelling. *)
  Alcotest.(check bool)
    "color(display-p3 1 0 0) still differs from rgb(255 0 0)" false
    (equal ".a{color:color(display-p3 1 0 0)}" ".a{color:rgb(255,0,0)}");
  (* One byte off is a difference, and .501 does not scale to a whole byte, so
     neither folds onto the other. *)
  Alcotest.(check bool)
    "color(srgb 1 0 0) still differs from rgb(254 0 0)" false
    (equal ".a{color:color(srgb 1 0 0)}" ".a{color:rgb(254,0,0)}");
  Alcotest.(check bool)
    "an off-grid channel keeps its function" false
    (equal ".a{color:color(srgb .501 0 0)}" ".a{color:maroon}")

(* Canonical comparison applies the optimizer's precision mode symmetrically to
   both inputs. The default six-significant-figure budget therefore equates the
   minifier's static line-height fold, while [--lossless] keeps the authored
   quotient distinct from every finite approximation. *)
let canonical_numeric_division_follows_precision_mode () =
  let equal ?(lossless = false) a b =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~lossless a b
  in
  Alcotest.(check bool)
    "an exact quotient folds" true
    (equal ".a{line-height:calc(28/14)}" ".a{line-height:2}");
  Alcotest.(check bool)
    "the default precision budget matches minified output" true
    (equal ".a{line-height:calc(28/18)}" ".a{line-height:1.55556}");
  Alcotest.(check bool)
    "even the nearest emitted decimal is outside zero tolerance" false
    (equal ".a{line-height:calc(28/18)}" ".a{line-height:1.55555556}");
  Alcotest.(check bool)
    "lossless keeps an exact numeric boundary" false
    (equal ~lossless:true ".a{line-height:calc(28/18)}"
       ".a{line-height:1.55556}")

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
        50%,transparent)}.b{color:color-mix(in oklab,blue 50%,transparent)}}")

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

(* Filter Effects 1 sec. 6.1 makes an omitted [hue-rotate()] argument 0, and
   [hue-rotate] names a filter function and nothing else, so the two spellings
   are one value wherever the stream is substituted. *)
let canonical_custom_hue_rotate_zero () =
  Alcotest.(check bool)
    "hue-rotate() and hue-rotate(0deg) agree in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a{--f:hue-rotate();filter:var(--f)}"
       ".a{--f:hue-rotate(0deg);filter:var(--f)}");
  (* Any zero angle, since the unit does not survive normalisation. *)
  Alcotest.(check bool)
    "a zero turn agrees too" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a{--f:hue-rotate()}"
       ".a{--f:hue-rotate(0turn)}");
  (* A non-zero angle is a different filter. *)
  Alcotest.(check bool)
    "a non-zero angle still differs" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a{--f:hue-rotate()}"
       ".a{--f:hue-rotate(90deg)}")

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

let equal_prune_unused_custom_props () =
  let with_bind = ":root{--spacing:.25rem}.top-0{top:0}" in
  let without = ".top-0{top:0}" in
  Alcotest.(check bool)
    "default: a dead binding is a real difference" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical with_bind without);
  Alcotest.(check bool)
    "opt-in: a dead binding compares equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ~prune_unused_custom_props:true with_bind without);
  Alcotest.(check bool)
    "opt-in: a referenced binding still differs" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ~prune_unused_custom_props:true
       ":root{--spacing:.25rem}.gap{gap:var(--spacing)}"
       ".gap{gap:var(--spacing)}")

let equal_whitespace_difference () =
  (* Structurally same but different formatting - default mode does not collapse
     formatting, so this returns false. *)
  let css1 = ".a { color: red }" in
  let css2 = ".a{color:red}" in
  Alcotest.(check bool)
    "whitespace-different CSS not equal under default mode" false
    (Cascade_diff.Css_compare.equal css1 css2)

(* ===== semantic equivalence tests ===== *)

let color_mix_transparent = "color-mix(in oklab, currentcolor 50%, transparent)"
let color_mix_transparent_hex = "color-mix(in oklab, currentcolor 50%, #0000)"

let semantically_equivalent_color_mix_values () =
  Alcotest.(check bool)
    "transparent and #0000 are equivalent inside color-mix" true
    (Cascade_diff.Css_compare.equivalent_value ~property:"color"
       color_mix_transparent color_mix_transparent_hex)

let semantically_equivalent_color_mix_css () =
  let expected = ".a { color: " ^ color_mix_transparent ^ " }" in
  let actual = ".a { color: " ^ color_mix_transparent_hex ^ " }" in
  Alcotest.(check bool)
    "CSS with equivalent color-mix values is semantically equivalent" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_property_tokens () =
  let expected = ".a { --tw-ring-color: transparent }" in
  let actual = ".a { --tw-ring-color: #0000 }" in
  Alcotest.(check bool)
    "unregistered custom property token streams stay opaque" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let registered_custom_property_tokens () =
  let expected =
    "@property --tw-ring-color { syntax: \"<color>\"; inherits: true; \
     initial-value: transparent } .a { --tw-ring-color: transparent }"
  in
  let actual =
    "@property --tw-ring-color { syntax: \"<color>\"; inherits: true; \
     initial-value: transparent } .a { --tw-ring-color: #0000 }"
  in
  Alcotest.(check bool)
    "registered color custom properties compare after typed canonicalization"
    true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_color_mix_tokens () =
  (* color-mix() is unconditionally a colour wherever it is substituted, so two
     spellings of the same colour-mix are equal under canonical comparison even
     inside an unregistered custom property. transparent and #0000 are the same
     colour, so these two forms compare equal. *)
  let expected = ".a { --tw-gradient: " ^ color_mix_transparent ^ " }" in
  let actual = ".a { --tw-gradient: " ^ color_mix_transparent_hex ^ " }" in
  Alcotest.(check bool)
    "custom color-mix spellings of one colour compare equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_color_function_alpha () =
  (* oklab() is unconditionally a colour, so an alpha written as 25% or .25 is
     the same value; canonical comparison equates them inside a custom-property
     body. *)
  Alcotest.(check bool)
    "oklab alpha 25% and .25 compare equal in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --s: oklab(50% 0 0 / 25%) }" ".a { --s: oklab(50% 0 0 / .25) }");
  (* A bare colour keyword could be a custom-ident in a non-colour substitution
     context, so it stays opaque: transparent and #0000 are not equated. *)
  Alcotest.(check bool)
    "bare transparent and #0000 stay distinct in a custom property" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a { --c: transparent }"
       ".a { --c: #0000 }")

let canonical_custom_shadow_color () =
  (* A complete shadow grammar fixes its final keyword's type to <color>, even
     when the shadow is carried through an unregistered custom property. *)
  Alcotest.(check bool)
    "named and hex shadow colours compare equal in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ":root { --drop-shadow-glow: 0 0 8px red }"
       ":root { --drop-shadow-glow: 0 0 8px #f00 }");
  (* An arbitrary identifier after the lengths is not a valid shadow colour and
     must remain opaque. *)
  Alcotest.(check bool)
    "a non-colour custom identifier stays distinct" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a { --x: 0 0 8px red }"
       ".a { --x: 0 0 8px custom-ident }")

let canonical_relative_color_origin () =
  (* A relative colour function fixes its origin's type to <color>, so
     equivalent spellings of that origin must compare equal, including when the
     whole function sits inside an otherwise opaque custom-property value. *)
  let parsed =
    Cascade.Css.Values.read_color
      (Cascade.Cursor.of_string "oklab(from red l a b / var(--x))")
  in
  Alcotest.(check bool)
    "relative-colour parser retains a typed origin" true
    (match parsed with
    | Cascade.Css.Relative_color
        ("oklab", Cascade.Css.Named Cascade.Css.Red, "l a b/var(--x)") ->
        true
    | _ -> false);
  Alcotest.(check bool)
    "named and hex relative-colour origins compare equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { color: oklab(from red l a b / var(--x)) }"
       ".a { color: oklab(from #f00 l a b / var(--x)) }");
  Alcotest.(check bool)
    "named and hex relative-rgb origins compare equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { color: rgb(from red r g b / var(--x)) }"
       ".a { color: rgb(from #f00 r g b / var(--x)) }");
  Alcotest.(check bool)
    "relative-colour origins compare equal inside a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --s: 0 0 8px oklab(from red l a b / var(--x)) }"
       ".a { --s: 0 0 8px oklab(from #f00 l a b / var(--x)) }")

let canonical_fully_transparent_missing_oklab () =
  let equivalent property =
    Cascade_diff.Css_compare.equal ~mode:`Canonical
      (Fmt.str ".a { %s: oklab(0%% none none / 0) }" property)
      (Fmt.str ".a { %s: #0000 }" property)
  in
  Alcotest.(check bool)
    "fully transparent missing-component oklab equals #0000 in a colour \
     property"
    true
    (equivalent "background-color");
  Alcotest.(check bool)
    "fully transparent missing-component oklab equals #0000 in a custom \
     property"
    true
    (equivalent "--tw-gradient-via");
  Alcotest.(check bool)
    "non-transparent missing-component oklab stays distinct from #0000" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { background-color: oklab(0% none none / .5) }"
       ".a { background-color: #0000 }")

let canonical_custom_font_family_quotes () =
  (* A quoted multi-word font name and the unquoted ident sequence substitute
     identically into font-family, so canonical comparison equates them inside a
     custom-property body even though both forms stay verbatim on output. *)
  Alcotest.(check bool)
    "quoted and unquoted multi-word font name compare equal in a custom \
     property"
    true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       {|:root { --font-sans: ui-sans-serif, system-ui, "Noto Color Emoji" }|}
       {|:root { --font-sans: ui-sans-serif, system-ui, Noto Color Emoji }|});
  (* A single-word string could be a custom-ident in a non-font substitution
     context, so it stays opaque: "foo" and foo are not equated. *)
  Alcotest.(check bool)
    "single-word quoted string stays distinct in a custom property" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical {|.a { --x: "foo" }|}
       {|.a { --x: foo }|})

let canonical_custom_calc_percentage () =
  let expected = ".a { --tw-translate-x: calc(1 / 2 * 100%) }" in
  let actual = ".a { --tw-translate-x: 50% }" in
  Alcotest.(check bool)
    "unregistered custom calc token streams stay opaque" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_calc_constant_fold () =
  (* A math function whose operands are all constants reduces to the same number
     in every substitution site, so [calc(2.25/1.875)] and [1.2] compare equal
     inside a custom property. A calc that carries units or a var() does not
     reduce and stays opaque. *)
  Alcotest.(check bool)
    "constant number calc folds in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --lh: calc(2.25 / 1.875) }" ".a { --lh: 1.2 }");
  Alcotest.(check bool)
    "calc with a var stays distinct in a custom property" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --x: calc(var(--y) * 2) }" ".a { --x: 2 }")

let semantic_with_recovered_warning () =
  let expected =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: transparent; \
     color: " ^ color_mix_transparent ^ " }"
  in
  let actual =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: #0000; color: "
    ^ color_mix_transparent_hex ^ " }"
  in
  Alcotest.(check bool)
    "canonical comparison falls back to lenient canonicalization" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_custom_var_fallback () =
  let expected =
    ".a { --tw-ring-shadow: 0 0 0 1px var(--tw-ring-color, transparent) }"
  in
  let actual =
    ".a { --tw-ring-shadow: 0 0 0 1px var(--tw-ring-color, #0000) }"
  in
  Alcotest.(check bool)
    "shadow colour var fallbacks compare after typed canonicalization" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_custom_nested_functions () =
  let expected =
    ".a { --tw-image: linear-gradient(transparent, " ^ color_mix_transparent
    ^ ") }"
  in
  let actual =
    ".a { --tw-image: linear-gradient(#0000, " ^ color_mix_transparent_hex
    ^ ") }"
  in
  Alcotest.(check bool)
    "nested custom function token streams are compared after minification" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_custom_oklab_sign_boundaries () =
  let expected =
    ".prose { --tw-prose-kbd-shadows: oklab(21% -.00316127 -.0338527 / .1) }"
  in
  let actual = ".prose { --tw-prose-kbd-shadows: oklab(21%-.003 -.034/.1) }" in
  Alcotest.(check bool)
    "unregistered OKLab custom property token streams stay opaque" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_prose_shadow_color_var () =
  let selector =
    ".hover\\:prose:hover \
     :where(kbd):not(:where([class~=not-prose],[class~=not-prose] *))"
  in
  let expected =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 0 0 1px var(--tw-prose-kbd-shadows),0 3px 0 \
       var(--tw-prose-kbd-shadows)}.hover\\:prose:hover{--tw-prose-kbd-shadows:oklab(21%-.003 \
       -.034/.1)}}}"
  in
  let actual =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 0 0 1px var(--tw-prose-kbd-shadows),0 3px \
       var(--tw-prose-kbd-shadows)}.hover\\:prose:hover{--tw-prose-kbd-shadows:oklab(21%-.003 \
       -.034/.1)}}}"
  in
  Alcotest.(check bool)
    "unregistered custom property cannot justify zero blur elision before var()"
    false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_prose_shadow_unknown_var () =
  let selector =
    ".hover\\:prose:hover \
     :where(kbd):not(:where([class~=not-prose],[class~=not-prose] *))"
  in
  let expected =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 3px 0 var(--tw-prose-kbd-shadows)}}}"
  in
  let actual =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 3px var(--tw-prose-kbd-shadows)}}}"
  in
  Alcotest.(check bool)
    "unknown custom property cannot justify zero blur elision before var()"
    false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_before_content_redundant_seed () =
  let selector = ".before\\:content-\\[\\\"\\>\\\"\\]:before" in
  let expected =
    "@layer utilities{" ^ selector
    ^ "{--tw-content:\">\";content:var(--tw-content)}}"
  in
  let actual =
    "@layer utilities{" ^ selector
    ^ "{content:var(--tw-content);--tw-content:\">\";content:var(--tw-content)}}"
  in
  Alcotest.(check bool)
    "leading duplicate content var() is dead before later content declaration"
    true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_minified css =
  match Css.of_string ~strict:true css with
  | Ok { Css.stylesheet; _ } -> Css.to_string ~minify:true stylesheet
  | Error error -> Alcotest.fail (Error.to_string error)

let canonical_overflow_trailing_ws () =
  let expected = ".sr-only { overflow: hidden; }" in
  let actual = ".sr-only{overflow:hidden}" in
  Alcotest.(check string)
    "pretty overflow declaration survives canonicalization"
    ".sr-only{overflow:hidden}"
    (canonical_minified expected);
  Alcotest.(check string)
    "minified overflow declaration survives canonicalization"
    ".sr-only{overflow:hidden}"
    (canonical_minified actual);
  Alcotest.(check bool)
    "pretty and minified overflow compare canonically equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

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

(* CSS Values L4 sec. 10.10: inside math functions (calc/min/max/clamp/mod/rem/
   round/sin/cos/tan/asin/acos/atan/atan2/pow/sqrt/hypot/log/exp/abs/sign),
   whitespace is OPTIONAL around *, /, (, ), and ,; it is REQUIRED around + and
   - (sign disambiguation, var(--a)-var(--b)). Whitespace inside the math
   grammar is purely lexical with no consumer-context dependence, so canonical
   comparison normalizes it - including inside custom-property values, where the
   calc body is otherwise round-tripped verbatim. *)

let canonical_bare_ratio_whitespace_equal () =
  (* The motivating cross-tool case: Tailwind authors [--aspect-video: 16 / 9]
     while a typed ratio serialises as [16/9]; the whitespace around [/] is
     insignificant wherever the stream is substituted, so canonical comparison
     folds it even outside a math function. *)
  let expected = ".x{--aspect-video: 16 / 9}" in
  let actual = ".x{--aspect-video:16/9}" in
  Alcotest.(check bool)
    "bare ratio whitespace around / canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_calc_mul_div_whitespace_equal () =
  let expected = ".x{--v:calc(1 / 2 * 100%)}" in
  let actual = ".x{--v:calc(1/2*100%)}" in
  Alcotest.(check bool)
    "calc(...) whitespace around * and / canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_calc_paren_whitespace_equal () =
  let expected = ".x{--v:calc( ( 1 / 2 ) * 100% )}" in
  let actual = ".x{--v:calc((1/2)*100%)}" in
  Alcotest.(check bool)
    "calc(...) whitespace around ( and ) canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_min_comma_whitespace_equal () =
  let expected = ".x{--v:min(50px, 100% / 2)}" in
  let actual = ".x{--v:min(50px,100%/2)}" in
  Alcotest.(check bool)
    "min(...) whitespace around , and / canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_clamp_commas_whitespace_equal () =
  let expected = ".x{--v:clamp(1rem, 4vw, 2rem)}" in
  let actual = ".x{--v:clamp(1rem,4vw,2rem)}" in
  Alcotest.(check bool)
    "clamp(...) comma whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_round_strategy_whitespace_equal () =
  (* round(...) takes a rounding-strategy keyword (up/down/nearest/to-zero) as
     its first argument; the , whitespace rule applies the same as elsewhere. *)
  let expected = ".x{--v:round(up, 3.5, 1)}" in
  let actual = ".x{--v:round(up,3.5,1)}" in
  Alcotest.(check bool)
    "round(<strategy>, ...) comma whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_trig_whitespace_equal () =
  let expected = ".x{--v:sin( 45deg )}" in
  let actual = ".x{--v:sin(45deg)}" in
  Alcotest.(check bool)
    "trig function ( and ) whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_nested_math_whitespace_equal () =
  let expected = ".x{--v:min( 50px, calc( 100% / 2 ) )}" in
  let actual = ".x{--v:min(50px,calc(100%/2))}" in
  Alcotest.(check bool)
    "nested math-function whitespace canonicalizes equal recursively" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_tailwind_translate_calc_equal () =
  (* The original motivating case: Tailwind's translate-x-1/2 emits calc(1 / 2 *
     100%) into --tw-translate-x, while cascade's pp_calc produces
     calc(1/2*100%). Custom-property values don't go through pp_calc so the
     divergence is purely whitespace inside the math body - canonical comparison
     must fold it. *)
  let expected = ".x{--tw-translate-x:calc(1 / 2 * 100%)}" in
  let actual = ".x{--tw-translate-x:calc(1/2*100%)}" in
  Alcotest.(check bool)
    "Tailwind --tw-translate-x:calc(...) whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* SAFETY: + and - whitespace is REQUIRED by the spec and must NOT be normalized
   away. The stripped form is illegal (parses as a different token sequence:
   100% followed by -var ident, then (--a)), and treating it equal would
   silently mask a real bug. *)
let canonical_calc_plus_minus_whitespace_distinct () =
  let expected = ".x{--v:calc(100% - var(--a))}" in
  let actual = ".x{--v:calc(100%-var(--a))}" in
  Alcotest.(check bool)
    "calc(...) + and - whitespace stays distinct (required for parse)" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Fonts L4 sec. 2.1.1: a font family name is either a <string> or a sequence of
   <custom-ident>s joined with single spaces; the two forms are equivalent for
   matching whenever the unquoted form would be valid identifiers. The
   equivalence is consumer-dependent (it only holds in font-family-typed
   contexts), so cascade applies the fold at the typed leaf and at
   font-family-registered custom properties, but never at unregistered custom
   properties (consumer unknown). *)

let canonical_font_family_quoted_unquoted_equal () =
  let expected = ".x{font-family:\"Segoe UI Symbol\"}" in
  let actual = ".x{font-family:Segoe UI Symbol}" in
  Alcotest.(check bool)
    "typed font-family: quoted string equals unquoted ident sequence" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* SAFETY: a quoted generic-family token is a literal font name of that
   spelling, not the generic family - they must stay distinct. Same applies to
   sans-serif/monospace/cursive/fantasy/system-ui/ui-*/math/emoji/fangsong, and
   to CSS-wide keywords like "inherit"/"initial"/etc. *)
let canonical_font_family_generic_keyword_distinct () =
  let expected = ".x{font-family:\"serif\"}" in
  let actual = ".x{font-family:serif}" in
  Alcotest.(check bool)
    "typed font-family: quoted \"serif\" literal stays distinct from generic"
    false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Unregistered custom property: the consumer is unknown (var(--font) could land
   in content: where the quoted/unquoted distinction is observable), so the fold
   must not apply - same call as the calc number_percentage handling for
   unregistered custom props. *)
let canonical_unregistered_custom_font_distinct () =
  let expected = ".x{--font:\"Segoe UI Symbol\"}" in
  let actual = ".x{--font:Segoe UI Symbol}" in
  Alcotest.(check bool)
    "unregistered --font custom property: quoted/unquoted stays distinct" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* [<custom-ident>+#] chains two multipliers where CSS Properties and Values API
   1 (ED) sec. 5.2 allows one per syntax component, so the registration is
   invalid and [--font] stays an ordinary unregistered custom property. The
   unregistered case above already settles that state: the consumer is unknown,
   so the two spellings stay distinct. No registration reaches the font-family
   position that would make them one name either, [<family-name>] not being
   among the syntax component names ED sec. 5.1 supports. *)
let canonical_invalid_registration_font_distinct () =
  let registration =
    "@property \
     --font{syntax:\"<custom-ident>+#\";inherits:true;initial-value:serif}"
  in
  let expected = registration ^ ".x{--font:\"Segoe UI Symbol\"}" in
  let actual = registration ^ ".x{--font:Segoe UI Symbol}" in
  Alcotest.(check bool)
    "invalid @property registration: quoted/unquoted stays distinct" false
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

let semantic_legacy_pseudo_element_alias () =
  let legacy =
    "@layer utilities{.prose :where(blockquote \
     p:first-of-type):not(:where([class~=not-prose],[class~=not-prose] \
     *)):before{content:\"\"}}"
  in
  let modern =
    "@layer utilities{.prose :where(blockquote \
     p:first-of-type):not(:where([class~=not-prose],[class~=not-prose] \
     *))::before{content:\"\"}}"
  in
  match
    (Cascade_diff.Css_compare.diff ~mode:`Canonical legacy modern)
      .Cascade_diff.Css_compare.result
  with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected :before and ::before to canonicalize equally"

let semantic_vendor_recovered () =
  let expected =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: transparent }"
  in
  let actual =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: #0000 }"
  in
  Alcotest.(check bool)
    "vendor color aliases survive lenient canonicalization" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_transparent_string_is_literal () =
  let expected = ".a { --tw-content: \"transparent\" }" in
  let actual = ".a { --tw-content: \"#0000\" }" in
  Alcotest.(check bool)
    "quoted transparent is not a color alias" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_transparent_ident_boundary () =
  let expected = ".a { --tw-filter: transparentize(0.5) }" in
  let actual = ".a { --tw-filter: #0000ize(0.5) }" in
  Alcotest.(check bool)
    "transparent identifier substrings are not color aliases" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantically_equivalent_rejects_different_colors () =
  Alcotest.(check bool)
    "different color values are not semantically equivalent" false
    (Cascade_diff.Css_compare.equivalent_value ~property:"color" "red" "blue")

(* [equivalent_value] gives its two values the declaration context [property]
   names. CSS Syntax 3 sec. 4.3.7 lets an escape carry a [;] or a [}] into a
   custom property's name, and written raw into that context such a name closes
   the rule and takes both values with it, so any two of them read back the
   same. *)
let semantically_equivalent_escaping_property () =
  List.iter
    (fun property ->
      Alcotest.(check bool)
        (String.concat "" [ property; ": red and blue stay different" ])
        false
        (Cascade_diff.Css_compare.equivalent_value ~property "red" "blue");
      Alcotest.(check bool)
        (String.concat ""
           [ property; ": the same colour spelled twice agrees" ])
        true
        (Cascade_diff.Css_compare.equivalent_value ~property "#ffffff" "#fff"))
    [ "--x"; "--x;y"; "--x}y"; "--x y" ]

(* ===== diff returning No_diff ===== *)

let diff_no_diff () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff"

let diff_no_diff_empty () =
  let result = Cascade_diff.Css_compare.diff "" "" in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff for empty strings"

(* ===== diff returning Tree_diff ===== *)

let diff_tree_diff () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff d ->
      Alcotest.(check bool)
        "tree diff is not empty" false
        (Cascade_diff.Tree_diff.is_empty d)
  | _ -> Alcotest.fail "expected Tree_diff"

let diff_tree_diff_added_rule () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: red } .b { margin: 0 }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ -> ()
  | _ -> Alcotest.fail "expected Tree_diff for added rule"

(* ===== diff returning String_diff ===== *)

let diff_string_diff () =
  (* To trigger String_diff, we need structurally identical CSS that differs
     only in formatting. This is tricky because the parser normalizes things. We
     use diff with `String to force it. *)
  let expected = ".a { color: red }" in
  let actual = ".a  { color: red }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`String expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.String_diff d ->
      Alcotest.(check bool) "string diff has position" true (d.position >= 0)
  | Cascade_diff.Css_compare.No_diff ->
      (* Strings might be considered equal after strip_tool_header/trim *)
      ()
  | _ -> Alcotest.fail "expected String_diff or No_diff"

(* ===== diff returning parse errors ===== *)

let diff_actual_error () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  (* The parser may or may not error on this - just check it doesn't crash *)
  ignore result

let diff_expected_error () =
  let expected = ".a { color: }" in
  let actual = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  ignore result

(* ===== parse warnings surfaced in diff reports ===== *)

(* When one side's declaration is rejected by the value parser, the declaration
   is dropped from the AST and the structural diff alone reads as a phantom
   addition on the side that parsed. The rendered report must surface the parse
   warning so the reader sees the real cause. *)
let diff_surfaces_parse_warnings () =
  let expected = ".a { color: red; scroll-snap-align: corner }" in
  let actual = ".a { color: red; scroll-snap-align: start }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp buf result;
  let rendered = Buffer.contents buf in
  Alcotest.(check bool)
    "report mentions the rejected value" true
    (contains_substring rendered "corner");
  Alcotest.(check bool)
    "report labels the parse warning" true
    (contains_substring rendered "parse warning")

let diff_surfaces_parse_warnings_same_ast () =
  (* Both sides parse to the same AST once the bad declaration is dropped; the
     report must still surface the warning instead of presenting the difference
     as purely textual. *)
  let expected = ".a { scroll-snap-align: corner }" in
  let actual = ".a { }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp buf result;
  let rendered = Buffer.contents buf in
  Alcotest.(check bool)
    "report labels the parse warning" true
    (contains_substring rendered "parse warning")

(* ===== strip_tool_header tests ===== *)

let strip_tool_header_no_header () =
  let css = ".a { color: red }" in
  Alcotest.(check string)
    "no header unchanged" css
    (Cascade_diff.Css_compare.strip_tool_header css)

let strip_tool_header_with_header () =
  let css = "/*! Generated by tool v1.0 */\n.a { color: red }" in
  let result = Cascade_diff.Css_compare.strip_tool_header css in
  Alcotest.(check string) "header stripped" ".a { color: red }" result

let strip_tool_header_regular_comment () =
  let css = "/* regular comment */\n.a { color: red }" in
  let result = Cascade_diff.Css_compare.strip_tool_header css in
  (* Regular comments (not /*!) should NOT be stripped *)
  Alcotest.(check string)
    "regular comment preserved" "/* regular comment */\n.a { color: red }"
    result

let strip_tool_header_empty () =
  Alcotest.(check string)
    "empty string" ""
    (Cascade_diff.Css_compare.strip_tool_header "")

let strip_tool_header_only_header () =
  let css = "/*! header only */" in
  let result = Cascade_diff.Css_compare.strip_tool_header css in
  Alcotest.(check string) "only header stripped" "" result

(* ===== stats tests ===== *)

let stats_no_diff () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:css ~actual_str:css result
  in
  Alcotest.(check int) "expected chars" (String.length css) s.expected_chars;
  Alcotest.(check int) "actual chars" (String.length css) s.actual_chars;
  Alcotest.(check int) "added rules" 0 s.added_rules;
  Alcotest.(check int) "removed rules" 0 s.removed_rules;
  Alcotest.(check int) "modified rules" 0 s.modified_rules;
  Alcotest.(check int) "reordered rules" 0 s.reordered_rules;
  Alcotest.(check int) "container changes" 0 s.container_changes

let stats_with_tree_diff () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: red } .b { margin: 0 }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:expected ~actual_str:actual
      result
  in
  Alcotest.(check int)
    "expected chars" (String.length expected) s.expected_chars;
  Alcotest.(check int) "actual chars" (String.length actual) s.actual_chars;
  (* With a tree diff, at least one of added/removed/modified should be > 0 *)
  let total = s.added_rules + s.removed_rules + s.modified_rules in
  Alcotest.(check bool) "has rule changes" true (total > 0)

let pp_stats_does_not_crash () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:css ~actual_str:css result
  in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp_stats buf s;
  let output = Buffer.contents buf in
  Alcotest.(check bool)
    "pp_stats produces output" true
    (String.length output > 0)

(* The counters only ever count a tree diff, so they read zero on every result
   that never reached one: a comparison that fell through to strings, and a side
   whose content the parser discarded. Announcing that as an absence of
   structural differences states a conclusion the comparator never drew. *)

let rendered_stats ?mode expected actual =
  let result = Cascade_diff.Css_compare.diff ?mode expected actual in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:expected ~actual_str:actual
      result
  in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp_stats buf s;
  Buffer.contents buf

let zero_counters_do_not_claim_equivalence () =
  (* The stray braces are dropped with a parse warning, so both sides reach the
     same AST and the report falls through to a string diff. *)
  let output = rendered_stats "a{color:red}" "a{color:red}}}}" in
  Alcotest.(check bool)
    "does not claim the two sheets match structurally" false
    (contains_substring output "No structural differences");
  Alcotest.(check bool)
    "says the changes were not classified" true
    (contains_substring output "none classified structurally")

let zero_counters_unparsed_do_not_claim_equivalence () =
  (* Mode [`String] never parses, so the counters read zero over two sheets that
     paint differently. *)
  let output = rendered_stats ~mode:`String "a{color:red}" "a{color:blue}" in
  Alcotest.(check bool)
    "a comparison that never parsed is not an absence of differences" false
    (contains_substring output "No structural differences")

(* The canonical minified form is the verdict in [`Canonical] mode: two sheets
   whose canonical bytes differ differ, whether or not the tree diff reached the
   divergence. Here a layer-order pin that is the only declaration of its layer,
   so CSS Cascade 5 sec. 6.4.3 has it order [a] before [b] and the projection
   keeps it; the tree diff walks past a statement that carries no rules. *)
let canonical_byte_residual_is_a_difference () =
  let pinned = "@layer a;@layer b{x{top:0}}" in
  let unpinned = "@layer b{x{top:0}}" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical pinned unpinned in
  (match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.String_diff _ -> ()
  | _ ->
      Alcotest.fail
        "expected a string diff of the two differing canonical forms");
  Alcotest.(check bool)
    "the equality answer follows the bytes" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical pinned unpinned)

let canonical_byte_equal_is_no_diff () =
  (* The converse: once the two sheets reach one canonical form, the spelling
     they started from is immaterial and the verdict is equality. [equal] is
     [true] only on {!No_diff}, so it pins the constructor too. *)
  let spaced = ".x { color: red }" in
  let tight = ".x{color:red}" in
  Alcotest.(check bool)
    "the equality answer follows the bytes" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical spaced tight);
  Alcotest.(check bool)
    "no tree diff is reported over one canonical form" true
    (Option.is_none
       (Cascade_diff.Css_compare.as_tree_diff
          (Cascade_diff.Css_compare.diff ~mode:`Canonical spaced tight)))

(* ===== diff tests ===== *)

let diff_auto () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Auto expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ -> ()
  | _ -> Alcotest.fail "expected Tree_diff in auto mode"

let diff_tree () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Tree expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ -> ()
  | _ -> Alcotest.fail "expected Tree_diff in tree mode"

let diff_string () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`String expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.String_diff _ -> ()
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected String_diff or No_diff in string mode"

let diff_string_identical () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`String css css in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff for identical in string mode"

let semantic_color_mix_mode () =
  let expected = ".a { color: " ^ color_mix_transparent ^ " }" in
  let actual = ".a { color: " ^ color_mix_transparent_hex ^ " }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff for semantically equivalent CSS"

let diff_canonical_uses_outputs () =
  let expected =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: transparent; \
     color: red }"
  in
  let actual =
    "@unknown{color:red}.a{-webkit-tap-highlight-color:#0000;color:blue}"
  in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ ->
      let buf = Buffer.create 256 in
      Cascade_diff.Css_compare.pp buf result;
      let rendered = Buffer.contents buf in
      Alcotest.(check bool)
        "normalized diff does not report transparent alias" false
        (contains_substring rendered "-webkit-tap-highlight-color");
      Alcotest.(check bool)
        "normalized diff reports remaining color change" true
        (contains_substring rendered "color:")
  | _ -> Alcotest.fail "expected canonical diff on normalized outputs"

(* ===== as_tree_diff tests ===== *)

let as_tree_diff_with_tree () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  Alcotest.(check bool)
    "as_tree_diff returns Some for Tree_diff" true
    (Option.is_some (Cascade_diff.Css_compare.as_tree_diff result))

let as_tree_diff_no_diff () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  Alcotest.(check bool)
    "as_tree_diff returns None for No_diff" true
    (Option.is_none (Cascade_diff.Css_compare.as_tree_diff result))

(* ===== pp tests ===== *)

let pp_does_not_crash () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp buf result;
  (* Just verify it doesn't crash *)
  ignore (Buffer.contents buf)

(* ===== @import diffs ===== *)

let equal = Cascade_diff.Css_compare.equal

(* Two @import rules that differ in target, media, or order are
   cascade-significant differences, not equal, in both the structural Tree mode
   and the semantic Canonical mode. *)
let import_tree_url () =
  Alcotest.(check bool)
    "different @import targets differ (tree)" false
    (equal ~mode:`Tree "@import url(a.css);" "@import url(b.css);");
  Alcotest.(check bool)
    "identical @import equal (tree)" true
    (equal ~mode:`Tree "@import url(a.css);" "@import url(a.css);")

let import_tree_media () =
  Alcotest.(check bool)
    "different @import media differ (tree)" false
    (equal ~mode:`Tree "@import url(a.css) screen;" "@import url(a.css) print;")

let import_tree_order () =
  Alcotest.(check bool)
    "swapped @import order differs (tree)" false
    (equal ~mode:`Tree "@import url(a.css);@import url(b.css);"
       "@import url(b.css);@import url(a.css);")

let import_canonical_media () =
  Alcotest.(check bool)
    "different @import media differ (canonical)" false
    (equal ~mode:`Canonical "@import url(a.css) screen;"
       "@import url(a.css) print;");
  Alcotest.(check bool)
    "identical @import equal (canonical)" true
    (equal ~mode:`Canonical "@import url(a.css);" "@import url(a.css);")

(* Two same-selector rules with conflicting declarations cascade last-wins, so
   swapping their order flips the winner: a real difference the structural Tree
   mode must report, not only Auto/Canonical. *)
let tree_same_selector_reorder () =
  Alcotest.(check bool)
    "same-selector reorder flips the cascade winner (tree)" false
    (equal ~mode:`Tree ".a{color:red}.a{color:blue}"
       ".a{color:blue}.a{color:red}");
  Alcotest.(check bool)
    "same order stays equal (tree)" true
    (equal ~mode:`Tree ".a{color:red}.a{color:blue}"
       ".a{color:red}.a{color:blue}")

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

(* ===== Suite ===== *)

let suite =
  ( "css_compare",
    [
      Alcotest.test_case "import tree url" `Quick import_tree_url;
      Alcotest.test_case "import tree media" `Quick import_tree_media;
      Alcotest.test_case "import tree order" `Quick import_tree_order;
      Alcotest.test_case "import canonical media" `Quick import_canonical_media;
      Alcotest.test_case "tree same-selector reorder" `Quick
        tree_same_selector_reorder;
      Alcotest.test_case
        "canonical same-selector merge across intervening rules" `Quick
        canonical_same_selector_merge_across_intervening_rules;
      Alcotest.test_case "canonical unknown longhand order" `Quick
        canonical_unknown_longhand_order;
      Alcotest.test_case "canonical typed longhand order" `Quick
        canonical_typed_longhand_order;
      Alcotest.test_case "equal identical" `Quick equal_identical;
      Alcotest.test_case "equal different" `Quick equal_different;
      Alcotest.test_case "equal empty" `Quick equal_empty;
      Alcotest.test_case "equal whitespace difference" `Quick
        equal_whitespace_difference;
      Alcotest.test_case "semantically equivalent color-mix values" `Quick
        semantically_equivalent_color_mix_values;
      Alcotest.test_case "semantically equivalent color-mix CSS" `Quick
        semantically_equivalent_color_mix_css;
      Alcotest.test_case "canonical custom property tokens" `Quick
        canonical_custom_property_tokens;
      Alcotest.test_case "registered custom property tokens" `Quick
        registered_custom_property_tokens;
      Alcotest.test_case "canonical custom color-mix tokens" `Quick
        canonical_custom_color_mix_tokens;
      Alcotest.test_case "canonical custom color-function alpha" `Quick
        canonical_custom_color_function_alpha;
      Alcotest.test_case "canonical custom shadow colour" `Quick
        canonical_custom_shadow_color;
      Alcotest.test_case "canonical relative-colour origin" `Quick
        canonical_relative_color_origin;
      Alcotest.test_case "canonical fully transparent missing oklab" `Quick
        canonical_fully_transparent_missing_oklab;
      Alcotest.test_case "canonical custom font-family quotes" `Quick
        canonical_custom_font_family_quotes;
      Alcotest.test_case "canonical custom calc percentage" `Quick
        canonical_custom_calc_percentage;
      Alcotest.test_case "canonical custom calc constant fold" `Quick
        canonical_custom_calc_constant_fold;
      Alcotest.test_case "semantic with recovered warning" `Quick
        semantic_with_recovered_warning;
      Alcotest.test_case "semantic custom var fallback" `Quick
        semantic_custom_var_fallback;
      Alcotest.test_case "semantic custom nested functions" `Quick
        semantic_custom_nested_functions;
      Alcotest.test_case "semantic custom OKLab sign boundaries" `Quick
        semantic_custom_oklab_sign_boundaries;
      Alcotest.test_case "semantic hover prose kbd shadow color var" `Quick
        semantic_prose_shadow_color_var;
      Alcotest.test_case "semantic hover prose kbd shadow unknown var" `Quick
        semantic_prose_shadow_unknown_var;
      Alcotest.test_case "semantic before content redundant seed" `Quick
        semantic_before_content_redundant_seed;
      Alcotest.test_case "canonical overflow before semicolon" `Quick
        canonical_overflow_trailing_ws;
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
      Alcotest.test_case "canonical bare ratio whitespace" `Quick
        canonical_bare_ratio_whitespace_equal;
      Alcotest.test_case "canonical calc *,/ whitespace" `Quick
        canonical_calc_mul_div_whitespace_equal;
      Alcotest.test_case "canonical calc paren whitespace" `Quick
        canonical_calc_paren_whitespace_equal;
      Alcotest.test_case "canonical min comma whitespace" `Quick
        canonical_min_comma_whitespace_equal;
      Alcotest.test_case "canonical clamp commas whitespace" `Quick
        canonical_clamp_commas_whitespace_equal;
      Alcotest.test_case "canonical round strategy whitespace" `Quick
        canonical_round_strategy_whitespace_equal;
      Alcotest.test_case "canonical trig whitespace" `Quick
        canonical_trig_whitespace_equal;
      Alcotest.test_case "canonical nested math whitespace" `Quick
        canonical_nested_math_whitespace_equal;
      Alcotest.test_case "canonical Tailwind translate calc whitespace" `Quick
        canonical_tailwind_translate_calc_equal;
      Alcotest.test_case "canonical calc +/- whitespace stays distinct" `Quick
        canonical_calc_plus_minus_whitespace_distinct;
      Alcotest.test_case "canonical typed font-family quoted vs unquoted" `Quick
        canonical_font_family_quoted_unquoted_equal;
      Alcotest.test_case "canonical font-family generic keyword distinct" `Quick
        canonical_font_family_generic_keyword_distinct;
      Alcotest.test_case "canonical unregistered --font distinct" `Quick
        canonical_unregistered_custom_font_distinct;
      Alcotest.test_case "canonical invalid --font registration distinct" `Quick
        canonical_invalid_registration_font_distinct;
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
      Alcotest.test_case "semantic legacy pseudo-element alias" `Quick
        semantic_legacy_pseudo_element_alias;
      Alcotest.test_case "semantic vendor color with recovered warning" `Quick
        semantic_vendor_recovered;
      Alcotest.test_case "semantic transparent string is literal" `Quick
        semantic_transparent_string_is_literal;
      Alcotest.test_case "semantic transparent ident boundary" `Quick
        semantic_transparent_ident_boundary;
      Alcotest.test_case "semantically equivalent rejects different colors"
        `Quick semantically_equivalent_rejects_different_colors;
      Alcotest.test_case "semantically equivalent under an escaping property"
        `Quick semantically_equivalent_escaping_property;
      Alcotest.test_case "diff No_diff" `Quick diff_no_diff;
      Alcotest.test_case "diff No_diff empty" `Quick diff_no_diff_empty;
      Alcotest.test_case "diff Tree_diff" `Quick diff_tree_diff;
      Alcotest.test_case "diff Tree_diff added rule" `Quick
        diff_tree_diff_added_rule;
      Alcotest.test_case "diff String_diff" `Quick diff_string_diff;
      Alcotest.test_case "diff actual error" `Quick diff_actual_error;
      Alcotest.test_case "diff expected error" `Quick diff_expected_error;
      Alcotest.test_case "diff surfaces parse warnings" `Quick
        diff_surfaces_parse_warnings;
      Alcotest.test_case "diff surfaces parse warnings on same AST" `Quick
        diff_surfaces_parse_warnings_same_ast;
      Alcotest.test_case "strip_tool_header no header" `Quick
        strip_tool_header_no_header;
      Alcotest.test_case "strip_tool_header with header" `Quick
        strip_tool_header_with_header;
      Alcotest.test_case "strip_tool_header regular comment" `Quick
        strip_tool_header_regular_comment;
      Alcotest.test_case "strip_tool_header empty" `Quick
        strip_tool_header_empty;
      Alcotest.test_case "strip_tool_header only header" `Quick
        strip_tool_header_only_header;
      Alcotest.test_case "stats no diff" `Quick stats_no_diff;
      Alcotest.test_case "stats with tree diff" `Quick stats_with_tree_diff;
      Alcotest.test_case "pp_stats does not crash" `Quick
        pp_stats_does_not_crash;
      Alcotest.test_case "zero counters do not claim equivalence" `Quick
        zero_counters_do_not_claim_equivalence;
      Alcotest.test_case "zero counters unparsed do not claim equivalence"
        `Quick zero_counters_unparsed_do_not_claim_equivalence;
      Alcotest.test_case "canonical byte residual is a difference" `Quick
        canonical_byte_residual_is_a_difference;
      Alcotest.test_case "canonical byte equality is no diff" `Quick
        canonical_byte_equal_is_no_diff;
      Alcotest.test_case "diff auto" `Quick diff_auto;
      Alcotest.test_case "diff tree" `Quick diff_tree;
      Alcotest.test_case "diff string" `Quick diff_string;
      Alcotest.test_case "diff string identical" `Quick diff_string_identical;
      Alcotest.test_case "diff semantic color-mix" `Quick
        semantic_color_mix_mode;
      Alcotest.test_case "diff canonical uses outputs" `Quick
        diff_canonical_uses_outputs;
      Alcotest.test_case "as_tree_diff with tree" `Quick as_tree_diff_with_tree;
      Alcotest.test_case "as_tree_diff with no diff" `Quick as_tree_diff_no_diff;
      Alcotest.test_case "pp does not crash" `Quick pp_does_not_crash;
      Alcotest.test_case "opt-in prune-unused-custom-props" `Quick
        equal_prune_unused_custom_props;
      Alcotest.test_case "canonical folds a zero hue-rotate in a custom value"
        `Quick canonical_custom_hue_rotate_zero;
      Alcotest.test_case "canonical folds a disjoint trailing declaration"
        `Quick canonical_declaration_after_nested_rule;
      Alcotest.test_case "canonical supports hoisting" `Quick
        canonical_supports_hoisting;
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
      Alcotest.test_case "canonical keeps target-gated content" `Quick
        canonical_keeps_target_gated_content;
      Alcotest.test_case "canonical drops redundant decoration-color alias"
        `Quick canonical_drops_redundant_decoration_color_alias;
      Alcotest.test_case "canonical folds media range spellings" `Quick
        canonical_folds_media_range_spellings;
      Alcotest.test_case "canonical lossless equates exact srgb spellings"
        `Quick equal_canonical_lossless_exact_srgb;
      Alcotest.test_case "canonical numeric precision modes" `Quick
        canonical_numeric_division_follows_precision_mode;
      Alcotest.test_case "canonical nested and flattened selectors" `Quick
        canonical_nested_and_flattened_selectors_are_equal;
      Alcotest.test_case "canonical movable at-rule ignores nesting" `Quick
        canonical_movable_at_rule_ignores_nesting;
    ] )
