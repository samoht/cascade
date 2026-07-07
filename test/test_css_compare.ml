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
    "custom property var fallback token streams stay opaque" false
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

(* Fonts L4 sec. 15.3: a font family name is either a <string> or a sequence of
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

(* Registered custom property whose syntax matches the font-family grammar gets
   typed-promoted to the font-family AST, so normalize_font_family applies the
   same fold as the typed leaf. *)
let canonical_registered_font_family_quoted_unquoted_equal () =
  let registration =
    "@property \
     --font{syntax:\"<custom-ident>+#\";inherits:true;initial-value:serif}"
  in
  let expected = registration ^ ".x{--font:\"Segoe UI Symbol\"}" in
  let actual = registration ^ ".x{--font:Segoe UI Symbol}" in
  Alcotest.(check bool)
    "@property-registered font-family custom prop: quoted equals unquoted" true
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
  | Cascade_diff.Css_compare.No_diff _ -> ()
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

(* ===== diff returning No_diff ===== *)

let diff_no_diff () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff _ -> ()
  | _ -> Alcotest.fail "expected No_diff"

let diff_no_diff_empty () =
  let result = Cascade_diff.Css_compare.diff "" "" in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff _ -> ()
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
  | Cascade_diff.Css_compare.No_diff _ ->
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
  | Cascade_diff.Css_compare.No_diff _ -> ()
  | _ -> Alcotest.fail "expected String_diff or No_diff in string mode"

let diff_string_identical () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`String css css in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff _ -> ()
  | _ -> Alcotest.fail "expected No_diff for identical in string mode"

let semantic_color_mix_mode () =
  let expected = ".a { color: " ^ color_mix_transparent ^ " }" in
  let actual = ".a { color: " ^ color_mix_transparent_hex ^ " }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff _ -> ()
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
      Alcotest.test_case "canonical registered --font quoted vs unquoted" `Quick
        canonical_registered_font_family_quoted_unquoted_equal;
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
    ] )
