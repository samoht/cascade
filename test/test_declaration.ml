(** Tests for CSS Declaration parsing *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers

(* One-liner check functions for each type *)
let check_declaration ?minify ?roundtrip ?expected ?optimized input =
  check_value_cursor "declaration" read_declaration (Css.Pp.option pp) ?minify
    ?roundtrip ?expected input;
  match optimized with
  | None -> ()
  | Some into ->
      (* held = the pp-minified declaration (the [expected] held form, or the
         already-minified [input]); just-minify must hold it, optimize folds. *)
      decl_optimizes_to ~held:(Option.value ~default:input expected) ~into input

let check = check_declaration

let check_specified_value name css expected =
  let decl = Css.Declaration.of_string css in
  Alcotest.(check string)
    name expected
    (Css.Declaration.string_of_value ~minify:false decl)

let check_visible_var name css expected =
  let declaration = Css.Declaration.of_string css in
  let names =
    Css.vars_of_declarations [ declaration ] |> List.map Css.any_var_name
  in
  Alcotest.(check (list string)) name [ expected ] names

let check_declarations input expected_count =
  let r = Cursor.of_string input in
  let decls = read_declarations r in
  Alcotest.check int
    (Fmt.str "declarations count %s" input)
    expected_count (List.length decls);
  decls

let check_block input expected_count =
  let r = Cursor.of_string input in
  let decls = read_block r in
  Alcotest.check int
    (Fmt.str "block count %s" input)
    expected_count (List.length decls);
  decls

let simple () =
  (* Basic declarations *)
  check_declaration ~expected:"color:red" "color: red;";
  check_declaration ~expected:"margin:10px" "margin: 10px;";
  check_declaration ~expected:"padding:5px" "padding: 5px;";
  (* With whitespace *)
  check_declaration ~expected:"color:red" "  color  :  red  ;  ";
  check_declaration ~expected:"margin:10px" "\tmargin\t:\t10px\t;\t"

let complex_values () =
  (* Function values *)
  check_declaration ~expected:"background:url(image.png)"
    "background: url(image.png);";
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width: calc(100% - 10px);";
  check_declaration ~expected:"transform:rotate(45deg)"
    "transform: rotate(45deg);";

  (* Multiple values *)
  check_declaration ~expected:"font-family:Arial,sans-serif"
    "font-family: \"Arial\", sans-serif;";
  check_declaration ~expected:"margin:10px 20px 30px 40px"
    "margin: 10px 20px 30px 40px;";

  (* Gradients. pp holds the side keyword and Named blue; side->angle and
     Named->hex are optimize folds. *)
  check_declaration ~expected:"background:linear-gradient(to right,red,blue)"
    "background: linear-gradient(to right, red, blue);";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right, red, blue)";

  (* CSS Images 4 section 3.5.1 defines [<color> <p1> <p2>] as exactly [<color>
     <p1>, <color> <p2>], so adjacent stops of one colour fold into a
     double-position stop. pp holds the pair; the fold is an optimize step. *)
  check_declaration
    ~expected:"background:linear-gradient(currentColor 0,currentColor 1px)"
    "background: linear-gradient(currentColor 0, currentColor 1px);";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(currentColor 0 1px)"
    "linear-gradient(currentColor 0, currentColor 1px)";
  (* The colours fold first, so two spellings of one colour still pair up. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(red 0 10px)"
    "linear-gradient(red 0, #f00 10px)";
  (* A stop without a position is not the same stop repeated: the position it
     would take is interpolated, so there is nothing to absorb. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(red,red 10px)"
    "linear-gradient(red, red 10px)";
  (* A stop already carrying both positions absorbs no further neighbour. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(red 0 5px,red 10px)"
    "linear-gradient(red 0 5px, red 10px)";

  (* [0deg] points the gradient line at the top; turning it 180 degrees reaches
     the default [to bottom] and reversing the stops undoes the turn, so the
     angle costs nothing but bytes. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(#00f,red)"
    "linear-gradient(0deg, red, blue)";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(#00f,red)"
    "linear-gradient(to top, red, blue)";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(#00f,green,red)"
    "linear-gradient(0deg, red, green, blue)";
  (* A positioned stop would have to mirror to [100% - p] to survive the
     reversal, which is not shorter, so the angle stays. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(0deg,red,#00f)"
    "linear-gradient(0deg, red 0%, blue 100%)";
  (* An interpolation hint is positional in the same way. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(0deg,red,30%,#00f)"
    "linear-gradient(0deg, red, 30%, blue)";
  (* The legacy prefixed gradients measure their angle from a different zero, so
     the same rewrite would point them elsewhere. *)
  decl_optimizes ~prop:"background"
    ~into:"-webkit-linear-gradient(0deg,red,#00f)"
    "-webkit-linear-gradient(0deg, red, blue)";

  (* The SVG and filter opacities are <alpha-value>, so they minify like
     [opacity] rather than surviving as opaque unknown-property text. *)
  check_declaration ~expected:"fill-opacity:.1" "fill-opacity: 0.1;";
  check_declaration ~expected:"stroke-opacity:1" "stroke-opacity: 1.0;";
  check_declaration ~expected:"stop-opacity:.5" "stop-opacity: .50;";
  (* A percentage is the same alpha as the number, and the number is never
     longer. *)
  decl_optimizes ~prop:"flood-opacity" ~into:".5" "50%";

  (* SVG 2 sec. 14.2.4.2 and Filter Effects 1 sec. 9.13.1 / 11.5 make each of
     these a plain <color>, so they shorten like any other colour-valued
     property instead of surviving as opaque unknown-property text. *)
  check_declaration ~expected:"stop-color:#fff" "stop-color: #ffffff;";
  check_declaration ~expected:"lighting-color:currentColor"
    "lighting-color: currentColor;";
  (* Cross-notation folds are node changes, so they belong to the optimizer
     rather than the printer. *)
  check_declaration ~expected:"flood-color:rgb(255 0 0)"
    "flood-color: rgb(255, 0, 0);";
  decl_optimizes ~prop:"flood-color" ~into:"red" "rgb(255, 0, 0)";
  decl_optimizes ~prop:"stop-color" ~into:"#0000" "rgba(0, 0, 0, 0)";

  (* SVG 2 sec. 13.4.2 and CSS Masking 1 sec. 6.2 give [fill-rule] and
     [clip-rule] the same <fill-rule>, which is the keyword pair alone: the
     argument form inside polygon() is a different production. *)
  check_declaration ~expected:"fill-rule:evenodd" "fill-rule: evenodd;";
  check_declaration ~expected:"clip-rule:nonzero" "clip-rule: nonzero;";
  check_declaration ~expected:"fill-rule:var(--r)" "fill-rule: var(--r);";
  (* SVG 2 Editor's Draft sec. 13.5.3 gives [stroke-width] the value
     [<length-percentage> | <number>] and says "A <number> value represents a
     value in user units", the production sec. 13.5.6 already gives the dash
     lengths. The bare number is not a CSS <length>, so accepting it here is
     what keeps a printed [stroke-width] readable again. *)
  check_declaration ~roundtrip:true ~expected:"stroke-width:1.5"
    "stroke-width: 1.5;";
  check_declaration ~roundtrip:true ~expected:"stroke-width:1.5px"
    "stroke-width: 1.5px;";
  check_declaration ~roundtrip:true ~expected:"stroke-width:50%"
    "stroke-width: 50%;";
  check_declaration ~roundtrip:true ~expected:"stroke-width:var(--w)"
    "stroke-width: var(--w);";
  (* Same section: "A negative value is invalid." *)
  none_cursor read_declaration "stroke-width: -1";
  none_cursor read_declaration "stroke-width: -1px";
  (* SVG 2 sec. 13.5.4 and 13.5.5. [miter-clip] and [arcs] are the Level 2
     additions to [stroke-linejoin]; a reader that takes the grammar takes all
     five. *)
  check_declaration ~expected:"stroke-linecap:square" "stroke-linecap: square;";
  check_declaration ~expected:"stroke-linejoin:arcs" "stroke-linejoin: arcs;";
  (* SVG 2 sec. 13.5.5 makes the miter limit a plain <number>, so it minifies
     like one and a constant calc() folds. *)
  check_declaration ~expected:"stroke-miterlimit:4" "stroke-miterlimit: 4.0;";
  decl_optimizes ~prop:"stroke-miterlimit" ~into:"6" "calc(2 * 3)";
  (* SVG 2 sec. 13.5.6 separates dashes by comma and/or whitespace and the
     rendered pattern is the flat sequence either way, so both spellings read to
     one list and print with the shorter separator. *)
  check_declaration ~expected:"stroke-dasharray:4 2" "stroke-dasharray: 4, 2;";
  check_declaration ~expected:"stroke-dasharray:none" "stroke-dasharray: none;";
  (* A dash length is a <length-percentage> or a bare <number> in user units,
     and it minifies like every other length. *)
  decl_optimizes ~prop:"stroke-dashoffset" ~into:"0" "0px";
  decl_optimizes ~prop:"stroke-dasharray" ~into:"0 4px" "0px 4px";
  check_declaration ~expected:"stroke-dashoffset:.5px"
    "stroke-dashoffset: 0.50px;";
  (* SVG 2 sec. 13.8: a keyword left out is painted last, in the order [normal]
     would use, so the specification's own example is that [paint-order: stroke]
     equals [paint-order: stroke fill markers]. The shortest spelling is the
     shortest prefix that expands back to the same order. *)
  decl_optimizes ~prop:"paint-order" ~into:"stroke" "stroke fill markers";
  decl_optimizes ~prop:"paint-order" ~into:"normal" "fill stroke markers";
  decl_optimizes ~prop:"paint-order" ~into:"normal" "fill";
  (* Reordering the tail is load-bearing, so this one keeps both keywords. *)
  decl_optimizes ~prop:"paint-order" ~into:"markers stroke" "markers stroke";
  (* SVG 2 sec. 8.13 makes [viewport] what an omitted host space means, so
     writing it is redundant; [screen] is not. *)
  decl_optimizes ~prop:"vector-effect" ~into:"non-scaling-stroke"
    "non-scaling-stroke viewport";
  decl_optimizes ~prop:"vector-effect" ~into:"non-scaling-stroke screen"
    "non-scaling-stroke screen";

  (* Complex nested functions. Per CSS Values 4 section 10.10.1 the printer
     simplifies all-constant calc subexpressions, reducing same-unit additions
     to a single value. Calcs containing [var()] cannot reduce at syntax time
     and are preserved. *)
  check_declaration ~expected:"width:calc(100% - calc(50px + 10px))"
    "width: calc(100% - calc(50px + 10px));";

  (* Multiple nested calc() with var(): the inner calcs that contain [var()]
     cannot reduce, so the structure is preserved. *)
  check_declaration
    ~expected:
      "margin-block-end:calc(calc(var(--spacing)*2)*calc(1 - \
       var(--tw-space-y-reverse)))"
    "margin-block-end: calc(calc(var(--spacing) * 2) * calc(1 - \
     var(--tw-space-y-reverse)));";

  check_declaration ~expected:"width:calc(calc(var(--x)*2)*calc(var(--y) + 1))"
    "width: calc(calc(var(--x) * 2) * calc(var(--y) + 1));";

  (* Runtime substitutions are not known at parse time, so identity-looking
     calc() operators around env()/attr() are preserved. *)
  check_declaration ~expected:"width:calc(env(safe-area-inset-left)*1)"
    "width: calc(env(safe-area-inset-left) * 1);";
  check_declaration ~expected:"width:calc(attr(data-w px)*1)"
    "width: calc(attr(data-w px) * 1);";

  (* pp holds nested calc unfolded; optimize reduces the constant part. *)
  check_declaration ~expected:"height:calc(calc(50px + 10px) - 100%)"
    "height: calc(calc(50px + 10px) - 100%);";

  (* pp holds triple-nested calc; optimize reduces it fully. *)
  check_declaration ~expected:"width:calc(100% - calc(10px - calc(5px + 2px)))"
    "width: calc(100% - calc(10px - calc(5px + 2px)));";
  (* CSS Values 4 sec. 10.1 gives a [calc()] body exactly one [<calc-sum>]; a
     top-level [calc()] already checks this (the comment above [read_calc] in
     values.ml names [calc(1px 2px)] directly), but a nested [calc()] read
     through [read_nested_calc_factor] shares the same [Cursor.call] and had no
     such check, so [calc(calc(1px 2px) + 3px)] read only [1px] for the inner
     factor and answered [calc(1px + 3px)] instead of invalidating the
     declaration. *)
  neg_cursor read_declaration "width:calc(calc(1px 2px) + 3px)";
  check_declaration ~expected:"width:calc(calc(1px + 2px) + 3px)"
    "width: calc( calc( 1px + 2px ) + 3px );"

let quoted_strings () =
  (* Simple quoted strings *)
  check_declaration ~expected:"content:\"hello\"" "content: \"hello\";";
  check_declaration ~expected:"content:\"world\"" "content: 'world';";
  check_declaration ~expected:"content:\"nav  main\"" "content: \"nav  main\";";

  (* Escaped quotes *)
  check_declaration ~expected:"content:\"a\\\"b\"" "content: \"a\\\"b\";";
  check_declaration ~expected:"content:\"a'b\"" "content: 'a\\'b';";

  (* Strings with special characters *)
  check_declaration ~expected:"content:\"a;b\"" "content: \"a;b\";";
  check_declaration ~expected:"content:\"a:b\"" "content: \"a:b\";";
  check_declaration ~expected:"content:\"a{b}\"" "content: \"a{b}\";"

let custom_properties_basic () =
  check_declaration ~expected:"--color:red" "--color: red;";
  check_declaration ~expected:"--my-var:10px" "--my-var: 10px;";
  check_declaration ~expected:"--complex:var(--other,10px)"
    "--complex: var(--other, 10px);";
  check_declaration ~expected:"--important:value!important"
    "--important: value !important;"

let vendor_prefixes () =
  check_declaration ~expected:"-webkit-transform:rotate(45deg)"
    "-webkit-transform: rotate(45deg);";
  check_declaration ~expected:"-moz-appearance:none" "-moz-appearance: none;";
  check_declaration ~expected:"-ms-filter:blur(5px)" "-ms-filter: blur(5px);";
  check_declaration ~expected:"-o-transition:all .3s" "-o-transition: all 0.3s;";
  (* Canonical forms for the two webkit-prefixed properties whose typed emitters
     are still missing from Css.Declaration. Cascade already has
     webkit_box_decoration_break / webkit_background_clip / the entire
     webkit_mask_{composite,source_type,size,position,repeat,clip,origin}
     family; webkit_print_color_adjust and webkit_mask_image fill the holes.
     These round-trip pins document the output the typed emitters must
     produce. *)
  check_declaration ~expected:"-webkit-print-color-adjust:exact"
    "-webkit-print-color-adjust: exact;";
  check_declaration ~expected:"-webkit-mask-image:linear-gradient(red,blue)"
    "-webkit-mask-image: linear-gradient(red, blue);";
  (* -webkit-mask-image normalizes like mask-image: its gradient angle and
     colours fold under optimize (200grad -> 180deg, the default direction, is
     dropped; blue -> #00f). *)
  decl_optimizes ~prop:"-webkit-mask-image" ~into:"linear-gradient(red,#00f)"
    "linear-gradient(200grad, red, blue)"

let multiple () =
  (* Basic multiple declarations *)
  let decls = check_declarations "color: red; margin: 10px;" 2 in
  (* Check first declaration *)
  (match List.nth_opt decls 0 with
  | Some decl ->
      Alcotest.check string "first property" "color" (property_name decl);
      Alcotest.check bool "first not important" false (is_important decl)
  | None -> fail "Missing first declaration");

  (* Check second declaration *)
  (match List.nth_opt decls 1 with
  | Some decl ->
      Alcotest.check string "second property" "margin" (property_name decl);
      Alcotest.check bool "second not important" false (is_important decl)
  | None -> fail "Missing second declaration");

  (* Mixed important and normal *)
  let decls =
    check_declarations "color: red; margin: 10px !important; padding: 5px;" 3
  in
  match List.nth_opt decls 1 with
  | Some decl ->
      Alcotest.check bool "second is important" true (is_important decl)
  | None -> fail "Missing second declaration"

let block () =
  (* Basic block *)
  let decls = check_block "{ color: blue; display: block; }" 2 in
  (match List.nth_opt decls 0 with
  | Some decl ->
      Alcotest.check string "first property" "color" (property_name decl)
  | None -> fail "Missing first declaration");

  (* Block with important *)
  let decls = check_block "{ padding: 10px !important; margin: auto; }" 2 in
  (match List.nth_opt decls 0 with
  | Some decl ->
      Alcotest.check string "first property" "padding" (property_name decl);
      Alcotest.check bool "first is important" true (is_important decl)
  | None -> fail "Missing first declaration");

  (* Empty blocks *)
  let _ = check_block "{}" 0 in
  let _ = check_block "{ }" 0 in
  ()

let missing_semicolon () =
  (* Last declaration without semicolon *)
  let _ = check_declarations "color: red; margin: 10px" 2 in
  (* Single declaration without semicolon *)
  check_declaration ~expected:"color:red" "color: red";

  (* Complex value without semicolon *)
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width: calc(100% - 10px)"

let empty_input () =
  let r = Cursor.of_string "" in
  let decls = read_declarations r in
  Alcotest.check int "empty input" 0 (List.length decls);

  let r = Cursor.of_string "   " in
  let decls = read_declarations r in
  Alcotest.check int "whitespace only" 0 (List.length decls)

let property_name () =
  (* Test read_property_name directly *)
  let test_prop_name input expected =
    let r = Cursor.of_string input in
    let name = read_property_name r in
    Alcotest.check string
      (Fmt.str "property name %s" input)
      expected (String.trim name)
  in

  test_prop_name "color:" "color";
  test_prop_name "  margin  :" "margin";
  test_prop_name "-webkit-transform:" "-webkit-transform";
  test_prop_name "--custom-var:" "--custom-var";
  test_prop_name "font-family:" "font-family"

let property_value () =
  (* Test read_property_value directly *)
  let test_prop_value input expected =
    let r = Cursor.of_string input in
    let value = read_property_value r in
    Alcotest.check string (Fmt.str "property value %s" input) expected value
  in

  test_prop_value "red;" "red";
  test_prop_value "10px 20px;" "10px 20px";
  test_prop_value "rgb(255, 0, 0);" "rgb(255, 0, 0)";
  test_prop_value "\"Arial\", sans-serif;" "\"Arial\", sans-serif";
  test_prop_value "calc(100% - 10px);" "calc(100% - 10px)";
  test_prop_value "linear-gradient(to right, red, blue);"
    "linear-gradient(to right, red, blue)"

let roundtrip () =
  (* Test that parsing and re-serializing gives expected results *)
  check_declaration "color:red";
  check_declaration "margin:10px";
  check_declaration "padding:5px!important";
  check_declaration ~expected:"font-family:Arial,sans-serif"
    "font-family:\"Arial\",sans-serif";
  check_declaration "background:url(image.png)";
  check_declaration "content:\"a\\\"b\"";
  check_declaration "width:calc(100% - 10px)";

  (* With spacing normalization *)
  check_declaration ~expected:"color:red" "color: red";
  check_declaration ~expected:"margin:10px" "margin : 10px";
  check_declaration ~expected:"padding:5px!important" "padding: 5px !important"

(* The exact error-message shape moved from [Reader.Parse_error] to [Error.t];
   we now only assert that parsing fails. *)
let expect_parse_error name f =
  try
    f ();
    Alcotest.failf "%s: expected Parse_error but none was raised" name
  with
  | Cursor.Parse_error _ -> ()
  | Reader.Parse_error _ -> ()

let error_missing_colon () =
  let r = Cursor.of_string "color red;" in
  expect_parse_error "missing colon" (fun () -> ignore (read_declaration r))

(* [Declaration.of_string] documents one failure mode, so a caller writes one
   handler. Text that is not a declaration reaches it two ways - the reader
   raises, or it answers [None] for a component a declaration cannot start with
   - and both must leave through the same exception. *)
let of_string_one_failure_mode () =
  let refused input =
    match of_string input with
    | d ->
        Alcotest.failf "%S: expected a parse error, parsed %S" input
          (to_string d)
    | exception Cursor.Parse_error _ -> ()
  in
  (* The reader reaches the value and rejects it. *)
  refused "color";
  refused "color red";
  refused "color:";
  refused "1px";
  (* [read_declaration] answers [None]: no declaration starts here. *)
  refused "";
  refused "   ";
  refused ".foo";
  refused "#id";
  refused "&:hover";
  refused "[data-x]";
  refused ": red"

let error_stray_semicolon () =
  let r = Cursor.of_string "; color: red;" in
  expect_parse_error "stray semicolon" (fun () -> ignore (read_declaration r))

let error_unclosed_block () =
  (* CSS Syntax 5.3.7 auto-closes unterminated blocks at EOF, so this now parses
     with an implicit [}]. *)
  let r = Cursor.of_string "{ color: red;" in
  ignore (read_block r : Css.Declaration.declaration list)

let special_cases () =
  (* Per CSS Values 4 section 10.10.1 the inner all-constant calc reduces to
     [60px], leaving the mixed-unit outer calc preserved. *)
  check_declaration ~expected:"width:calc(100% - calc(50px + 10px))"
    "width: calc(100% - calc(50px + 10px));";

  (* Custom property with var() value *)
  check_declaration ~expected:"--x:var(--y,10px)" "--x: var(--y, 10px)";

  (* Multiple backgrounds *)
  check_declaration ~expected:"background:url(x.png),linear-gradient(red,blue)"
    ~optimized:"background:url(x.png),linear-gradient(red,#00f)"
    "background: url(x.png), linear-gradient(red, blue);";

  (* One position per background layer, comma-separated: joining the layers with
     spaces reads as a single four-value position. *)
  check_declaration ~expected:"background-position:30%50%,70%50%"
    ~optimized:"background-position:30%,70%"
    "background-position: 30% 50%, 70% 50%;";
  check_declaration ~expected:"background-position:var(--x) 20%"
    "background-position: var(--x) 20%;";
  check_declaration ~expected:"mask-position:0 0,10px 10px"
    "mask-position: 0 0, 10px 10px;";

  (* CSS Box 4 sec. 7.1 box shorthands elide the same percentage token boundary
     as background-position (CSS Syntax 3 sec. 4.3.3): a % closes the
     percentage-token, so a following length starts a fresh token with no
     re-tokenisation risk. A unit keeps its space: [10px0] would re-tokenise as
     the single dimension 10px0. *)
  check_declaration ~expected:"margin:10%0" "margin: 10% 0;";
  check_declaration ~expected:"margin:10px 0" "margin: 10px 0;";
  check_declaration ~expected:"padding:10%0" "padding: 10% 0;";
  check_declaration ~expected:"inset:10%0" "inset: 10% 0;";
  check_declaration ~expected:"border-radius:10%0" "border-radius: 10% 0;";
  check_declaration ~expected:"border-radius:10%20%/30%0"
    "border-radius: 10% 20% / 30% 0;";

  (* border-color shares the same box-shorthand printer, so a colour function's
     closing paren gets the same elision. *)
  check_declaration ~expected:"border-color:var(--c)red"
    "border-color: var(--c) red;";

  (* CSS Box 4 (ED) sec. 3.2 and sec. 4.2: "If there is only one component
     value, it applies to all sides. If there are two values, the top and bottom
     margins are set to the first value and the right and left margins are set
     to the second. If there are three values, the top is set to the first
     value, the left and right are set to the second, and the bottom is set to
     the third." A repeated side therefore has a shorter spelling naming the
     same four sides, and picking it is a node change, so pp prints what it
     parsed and the optimizer folds. *)
  check_declaration ~expected:"margin:2px 2px 2px 2px" ~optimized:"margin:2px"
    "margin: 2px 2px 2px 2px";
  check_declaration ~expected:"margin:1px 2px 1px 2px"
    ~optimized:"margin:1px 2px" "margin: 1px 2px 1px 2px";
  check_declaration ~expected:"margin:1px 2px 3px 2px"
    ~optimized:"margin:1px 2px 3px" "margin: 1px 2px 3px 2px";
  check_declaration ~expected:"margin:1px 1px 1px" ~optimized:"margin:1px"
    "margin: 1px 1px 1px";
  check_declaration ~expected:"margin:1px 2px 1px" ~optimized:"margin:1px 2px"
    "margin: 1px 2px 1px";
  (* Four distinct sides have no shorter spelling. *)
  check_declaration ~expected:"margin:1px 2px 3px 4px"
    ~optimized:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"padding:0 0 0 0" ~optimized:"padding:0"
    "padding: 0 0 0 0";
  (* The logical shorthands take the same one-to-four assignment over their own
     two sides. *)
  check_declaration ~expected:"padding-inline:1px 1px"
    ~optimized:"padding-inline:1px" "padding-inline: 1px 1px";
  (* CSS Scroll Snap 1 sec. 4.1 and 5.1 use the same one-to-four side assignment
     for the physical shorthands and the same one-to-two assignment for their
     logical axis shorthands. *)
  check_declaration ~expected:"scroll-margin:1px 1px 1px 1px"
    ~optimized:"scroll-margin:1px" "scroll-margin: 1px 1px 1px 1px";
  check_declaration ~expected:"scroll-margin-inline:2px 2px"
    ~optimized:"scroll-margin-inline:2px" "scroll-margin-inline: 2px 2px";
  check_declaration ~expected:"scroll-margin-block:3px 3px"
    ~optimized:"scroll-margin-block:3px" "scroll-margin-block: 3px 3px";
  check_declaration ~expected:"scroll-padding:4px 5px 4px 5px"
    ~optimized:"scroll-padding:4px 5px" "scroll-padding: 4px 5px 4px 5px";
  check_declaration ~expected:"scroll-padding-inline:6px 6px"
    ~optimized:"scroll-padding-inline:6px" "scroll-padding-inline: 6px 6px";
  check_declaration ~expected:"scroll-padding-block:7px 7px"
    ~optimized:"scroll-padding-block:7px" "scroll-padding-block: 7px 7px";
  (* CSS Position 3 (ED) sec. 3.2 defines [inset] as [<'top'>{1,4}], and CSS
     Backgrounds 3 (ED) sec. 3.1 defines [border-color] over the same
     one-to-four side assignment. *)
  check_declaration ~expected:"inset:1px 1px 1px 1px" ~optimized:"inset:1px"
    "inset: 1px 1px 1px 1px";
  check_declaration ~expected:"border-color:red red red red"
    ~optimized:"border-color:red" "border-color: red red red red";

  (* CSS Lists 3 (ED) sec. 3.6: [list-style] is [<'list-style-position'> ||
     <'list-style-image'> || <'list-style-type'>], so a component left out takes
     its longhand initial - [outside] (sec. 3.5), [none] (sec. 3.3) and [disc]
     (sec. 3.4). Writing an initial out names what leaving it out names, and
     dropping it is a node change, so pp holds it and the optimizer folds. *)
  check_declaration ~expected:"list-style:disc outside"
    ~optimized:"list-style:disc" "list-style: disc outside";
  check_declaration ~expected:"list-style:outside" ~optimized:"list-style:disc"
    "list-style: outside";
  check_declaration ~expected:"list-style:disc outside none"
    ~optimized:"list-style:disc" "list-style: disc outside none";
  check_declaration ~expected:"list-style:square outside"
    ~optimized:"list-style:square" "list-style: square outside";
  (* sec. 3.6 resolves a bare [none] onto whichever of the image and the type
     the shorthand does not otherwise set, so [list-style: none] sets both. That
     makes [none] the shortest spelling of that one node rather than a fold, and
     it is what both layers print. *)
  check_declaration ~expected:"list-style:none" ~optimized:"list-style:none"
    "list-style: none";
  check_declaration ~expected:"list-style:none" ~optimized:"list-style:none"
    "list-style: none none";
  (* With the image set, the [none] lands on the type and both stay. *)
  check_declaration ~expected:"list-style:none url(bullet.png)"
    ~optimized:"list-style:none url(bullet.png)"
    "list-style: none url(bullet.png)";
  (* With the type set, the [none] lands on the image, which is its initial, so
     what is left is the all-initial value the single [disc] names. *)
  check_declaration ~expected:"list-style:disc none"
    ~optimized:"list-style:disc" "list-style: none disc";
  (* A non-initial position stands. *)
  check_declaration ~expected:"list-style:square inside"
    ~optimized:"list-style:square inside" "list-style: square inside";

  (* clip-path/object-view-box inset() and margin-inline/margin-block hit the
     same CSS Syntax 3 sec. 4.3.3 percentage-token boundary as margin/padding
     above, but print through their own list combinator rather than the shared
     pp_box_shorthand. A unit boundary still keeps its space. *)
  check_declaration ~expected:"clip-path:inset(0 0 10%0)"
    "clip-path: inset(0 0 10% 0);";
  check_declaration ~expected:"clip-path:inset(0 0 10px 0)"
    "clip-path: inset(0 0 10px 0);";
  check_declaration ~expected:"object-view-box:inset(0 0 10%0)"
    "object-view-box: inset(0 0 10% 0);";
  check_declaration ~expected:"margin-inline:10%20%" "margin-inline: 10% 20%;";
  check_declaration ~expected:"margin-inline:10px 20px"
    "margin-inline: 10px 20px;";
  check_declaration ~expected:"margin-block:10%20%" "margin-block: 10% 20%;";
  (* CSS Logical 1 (ED) sec. 4.2 defines [margin-inline] and [margin-block] as
     [<'margin-top'>{1,2}]: "If only one value is given, it applies to both the
     start and end edges." Two equal edges are therefore the longer spelling of
     the single value, the way the physical shorthand's four sides are. *)
  check_declaration ~expected:"margin-inline:1px 1px"
    ~optimized:"margin-inline:1px" "margin-inline: 1px 1px";
  check_declaration ~expected:"margin-block:1px 1px"
    ~optimized:"margin-block:1px" "margin-block: 1px 1px"

let colors () =
  (* Same-node spellings the printer keeps (case-fold, hex shorten). The
     cross-node folds - named<->hex, rgb()/hsl()->hex, transparent->#0000 - are
     optimize rewrites (checked via [decl_optimizes_to]). *)
  check_declaration ~expected:"color:red" "color: red";
  check_declaration ~expected:"color:green" "color: green";
  check_declaration ~expected:"color:#0f0" "color: #00ff00";
  check_declaration ~expected:"color:#00f" "color: #0000ff";
  check_declaration ~expected:"color:#fff" "color: #fff";
  check_declaration ~expected:"color:#000" "color: #000";

  (* Cross-node colour folds (named<->hex, rgb/hsl->hex/named, transparent,
     opaque rgb()->named) happen in optimize; pp holds the authored node. *)
  decl_optimizes_to ~held:"color:blue" ~into:"color:#00f" "color: blue";
  decl_optimizes_to ~held:"color:black" ~into:"color:#000" "color: black";
  decl_optimizes_to ~held:"color:white" ~into:"color:#fff" "color: white";
  decl_optimizes_to ~held:"color:transparent" ~into:"color:#0000"
    "color: transparent";
  decl_optimizes_to ~held:"color:#f00" ~into:"color:red" "color: #ff0000";
  decl_optimizes_to ~held:"color:rgb(255 0 0)" ~into:"color:red"
    "color: rgb(255, 0, 0)";
  decl_optimizes_to ~held:"color:rgb(0 255 0)" ~into:"color:#0f0"
    "color: rgb(0, 255, 0)";
  decl_optimizes_to ~held:"color:rgb(255 0 0/.5)" ~into:"color:#ff000080"
    "color: rgba(255, 0, 0, 0.5)";
  decl_optimizes_to ~held:"color:hsl(0 100%50%)" ~into:"color:red"
    "color: hsl(0, 100%, 50%)";
  decl_optimizes_to ~held:"color:hsl(120 100%50%/.5)" ~into:"color:#00ff0080"
    "color: hsla(120, 100%, 50%, 0.5)";
  decl_optimizes_to ~held:"color:hsl(.5turn 50%50%/var(--a))"
    ~into:"color:hsl(180 50%50%/var(--a))"
    "color: hsl(.5turn 50% 50% / var(--a))";

  check_declaration ~expected:"background-color:red" "background-color: red";
  decl_optimizes_to ~held:"border-color:blue" ~into:"border-color:#00f"
    "border-color: blue";
  decl_optimizes_to ~held:"outline-color:#f00" ~into:"outline-color:red"
    "outline-color: #ff0000"

let lengths () =
  (* Pixels *)
  check_declaration ~expected:"width:100px" "width: 100px";
  check_declaration ~expected:"height:50px" "height: 50px";
  check_declaration ~expected:"margin:10px" "margin: 10px";
  check_declaration ~expected:"padding:20px" "padding: 20px";

  (* Percentages *)
  check_declaration ~expected:"width:100%" "width: 100%";
  check_declaration ~expected:"width:0%" "width: 0%";
  check_declaration ~expected:"height:50%" "height: 50%";

  (* Em and rem *)
  check_declaration ~expected:"font-size:1.5em" "font-size: 1.5em";
  check_declaration ~expected:"font-size:2rem" "font-size: 2rem";
  check_declaration ~expected:"margin:1.5rem" "margin: 1.5rem";

  (* Zero. pp holds the authored unit (0px is the shortest <length> spelling of
     zero); stripping it to the unitless 0 changes <length> to <number>, a typed
     rewrite that optimize does. 0% stays a percentage. *)
  check_declaration ~expected:"width:0px" "width: 0px";
  decl_optimizes ~prop:"width" ~into:"0" "0px";
  check_declaration ~expected:"margin:0" "margin: 0";
  check_declaration ~expected:"padding:0" "padding: 0";

  (* Auto keyword *)
  check_declaration ~expected:"margin:auto" "margin: auto";
  check_declaration ~expected:"width:auto" "width: auto";
  check_declaration ~expected:"height:auto" "height: auto";
  (* CSS Logical 1 sec. 4 pairs each logical minimum-size property with its
     physical counterpart and gives the pair one shared computed value, so
     [initial] resolves through min-width / min-height to CSS Sizing 3 sec.
     3.1.2's [auto]. [expected] pins the pp-held form (the keyword is not folded
     at print time); [optimized] pins the normalize fold. *)
  check_declaration ~expected:"min-inline-size:initial"
    ~optimized:"min-inline-size:auto" "min-inline-size: initial";
  check_declaration ~expected:"min-block-size:initial"
    ~optimized:"min-block-size:auto" "min-block-size: initial";

  (* Min/max content *)
  check_declaration ~expected:"width:min-content" "width: min-content";
  check_declaration ~expected:"width:max-content" "width: max-content";
  check_declaration ~expected:"width:fit-content" "width: fit-content";

  (* Viewport units *)
  check_declaration ~expected:"width:100vw" "width: 100vw";
  check_declaration ~expected:"height:100vh" "height: 100vh";
  check_declaration ~expected:"width:50vmin" "width: 50vmin";
  check_declaration ~expected:"height:50vmax" "height: 50vmax"

let display () =
  let c = check_declaration in
  c ~expected:"display:none" "display: none";
  c ~expected:"display:block" "display: block";
  c ~expected:"display:inline" "display: inline";
  c ~expected:"display:inline-block" "display: inline-block";
  c ~expected:"display:flex" "display: flex";
  c ~expected:"display:inline-flex" "display: inline-flex";
  c ~expected:"display:grid" "display: grid";
  c ~expected:"display:inline-grid" "display: inline-grid";
  (* CSS Grid 3 (ED, 2 September 2026) sec. 2.2: "New values: grid-lanes |
     inline-grid-lanes". *)
  c ~expected:"display:grid-lanes" "display: grid-lanes";
  c ~expected:"display:inline-grid-lanes" "display: inline-grid-lanes";
  c ~expected:"display:table" "display: table";
  c ~expected:"display:table-row" "display: table-row";
  c ~expected:"display:table-cell" "display: table-cell";
  c ~expected:"display:list-item" "display: list-item";
  c ~expected:"display:contents" "display: contents";
  c ~expected:"display:flow-root" "display: flow-root";
  (* CSS Display 3 (ED) sec. 2.1: a [<display-outside>] written without an
     inside defaults the inner type to [flow]. sec. 2.2: an inside written alone
     defaults the outer type to [block], except [ruby], which defaults to
     [inline]. sec. 2.6 names the four precomposed inline-level keywords. Each
     two-value form below therefore has a single-keyword spelling of the same
     value, and the keyword is the shorter one; swapping them is a node change,
     so pp prints what it parsed and the optimizer folds. *)
  c ~expected:"display:block flow" ~optimized:"display:block"
    "display: block flow";
  c ~expected:"display:inline flow" ~optimized:"display:inline"
    "display: inline flow";
  c ~expected:"display:run-in flow" ~optimized:"display:run-in"
    "display: run-in flow";
  c ~expected:"display:block flow-root" ~optimized:"display:flow-root"
    "display: block flow-root";
  c ~expected:"display:inline flow-root" ~optimized:"display:inline-block"
    "display: inline flow-root";
  c ~expected:"display:block flex" ~optimized:"display:flex"
    "display: block flex";
  c ~expected:"display:inline flex" ~optimized:"display:inline-flex"
    "display: inline flex";
  c ~expected:"display:block grid" ~optimized:"display:grid"
    "display: block grid";
  c ~expected:"display:inline grid" ~optimized:"display:inline-grid"
    "display: inline grid";
  c ~expected:"display:block table" ~optimized:"display:table"
    "display: block table";
  c ~expected:"display:inline table" ~optimized:"display:inline-table"
    "display: inline table";
  c ~expected:"display:inline ruby" ~optimized:"display:ruby"
    "display: inline ruby";
  (* sec. 2.2's ruby exception cuts the other way too: the bare [ruby] keyword
     means [inline ruby], so [block ruby] is the one two-value form with no
     shorter spelling and both layers hold it. *)
  c ~expected:"display:block ruby" ~optimized:"display:block ruby"
    "display: block ruby";
  (* sec. 2.3: a [list-item] with no inside takes [flow] and with no outside
     takes [block], so [block flow list-item] is what [list-item] alone says.
     Dropping the [flow] alone names the same value either way, which is why pp
     may do it and the outer keyword has to wait for the optimizer. *)
  c ~expected:"display:block list-item" ~optimized:"display:list-item"
    "display: block flow list-item";
  c ~expected:"display:inline list-item" ~optimized:"display:inline list-item"
    "display: inline flow list-item";
  (* sec. 2 orders none of the three: [<display-outside>? && [ flow | flow-root
     ]? && list-item] reads the inside keyword on either side of the
     [list-item]. sec. 2.3 defaults the unwritten inside to [flow] and the
     unwritten outside to [block], so [flow-root list-item] is what [block
     flow-root list-item] says and pp writes the shorter one. *)
  c ~expected:"display:flow-root list-item"
    ~optimized:"display:flow-root list-item" "display: flow-root list-item";
  c ~expected:"display:flow-root list-item"
    ~optimized:"display:flow-root list-item" "display: list-item flow-root";
  c ~expected:"display:flow-root list-item"
    ~optimized:"display:flow-root list-item"
    "display: block flow-root list-item";
  c ~expected:"display:block list-item" ~optimized:"display:list-item"
    "display: flow list-item";
  c ~expected:"display:block list-item" ~optimized:"display:list-item"
    "display: list-item flow"

let position () =
  let c = check_declaration in
  c ~expected:"position:static" "position: static";
  c ~expected:"position:relative" "position: relative";
  c ~expected:"position:absolute" "position: absolute";
  c ~expected:"position:fixed" "position: fixed";
  c ~expected:"position:sticky" "position: sticky"

let font_properties () =
  (* CSS Fonts 4 (ED) sec. 2.2 defines [normal] as "Same as 400" and [bold] as
     "Same as 700", so each keyword and its number are one value with two
     spellings, and the number is the shorter one. Swapping them is a node
     change, so pp holds what it was handed and the optimizer folds. *)
  check_declaration ~expected:"font-weight:normal" ~optimized:"font-weight:400"
    "font-weight: normal";
  check_declaration ~expected:"font-weight:bold" ~optimized:"font-weight:700"
    "font-weight: bold";
  (* sec. 2.7 gives the [font] shorthand a [<'font-weight'>] slot, so the slot
     takes the longhand's fold. *)
  check_declaration ~expected:"font:bold 12px serif"
    ~optimized:"font:700 12px serif" "font: bold 12px serif";
  check_declaration ~expected:"font-weight:lighter" "font-weight: lighter";
  check_declaration ~expected:"font-weight:bolder" "font-weight: bolder";
  check_declaration ~expected:"font-weight:100" "font-weight: 100";
  check_declaration ~expected:"font-weight:400" "font-weight: 400";
  check_declaration ~expected:"font-weight:700" "font-weight: 700";
  check_declaration ~expected:"font-weight:900" "font-weight: 900";

  (* CSS Fonts 4 (ED) sec. 2.3 maps every [font-width] keyword onto a percentage
     ([condensed] is 75%, [normal] is 100%) and has getComputedStyle() serialize
     the property as a percentage whichever spelling was authored, so keyword
     and percentage are one value and the percentage is the shorter one.
     Swapping them is a node change, so pp holds what it parsed and the
     optimizer folds. *)
  check_declaration ~expected:"font-stretch:condensed"
    ~optimized:"font-stretch:75%" "font-stretch: condensed";
  check_declaration ~expected:"font-stretch:normal"
    ~optimized:"font-stretch:100%" "font-stretch: normal";
  check_declaration ~expected:"font-stretch:semi-expanded"
    ~optimized:"font-stretch:112.5%" "font-stretch: semi-expanded";
  check_declaration ~expected:"font-stretch:ultra-expanded"
    ~optimized:"font-stretch:200%" "font-stretch: ultra-expanded";
  check_declaration ~expected:"font-stretch:75%" ~optimized:"font-stretch:75%"
    "font-stretch: 75%";
  (* sec. 2.7 gives the [font] shorthand's width slot the grammar
     [<font-width-css3>], which is the keywords alone: the percentage the
     longhand folds to is not a value the slot accepts, so neither layer folds
     it here. *)
  check_declaration ~expected:"font:condensed 12px serif"
    ~optimized:"font:condensed 12px serif" "font: condensed 12px serif";

  (* CSS Values 4 (ED) sec. 10.10.1 simplifies a Sum by replacing each set of
     children with identical units by their sum, and returns the lone remaining
     child, so a same-unit [font-size] calc folds the way a [width] one does.
     [em] and [%] resolve against the parent font size here rather than the
     element's own, but both terms of one declaration share that reference, so
     the sum is the same value whatever it turns out to be. *)
  check_declaration ~expected:"font-size:calc(1px + 1px)"
    ~optimized:"font-size:2px" "font-size: calc(1px + 1px)";
  check_declaration ~expected:"font-size:calc(1em + 1em)"
    ~optimized:"font-size:2em" "font-size: calc(1em + 1em)";
  check_declaration ~expected:"font-size:calc(50% + 50%)"
    ~optimized:"font-size:100%" "font-size: calc(50% + 50%)";
  check_declaration ~expected:"font-size:calc(2*3px)" ~optimized:"font-size:6px"
    "font-size: calc(2 * 3px)";
  (* Terms of different units carry no conversion factor until layout, so the
     Sum keeps both children and both layers hold the call. *)
  check_declaration ~expected:"font-size:calc(1em + 1px)"
    ~optimized:"font-size:calc(1em + 1px)" "font-size: calc(1em + 1px)";
  (* sec. 2.7 of CSS Fonts 4 gives the [font] shorthand a [<'font-size'>] slot,
     so the slot takes the longhand's fold. *)
  check_declaration ~expected:"font:calc(1px + 1px) serif"
    ~optimized:"font:2px serif" "font: calc(1px + 1px) serif";

  (* sec. 2.7 first resets every subproperty of [font] to its initial value and
     then applies the slots given explicitly, so a slot carrying its longhand's
     initial says exactly what leaving it out says. The initials are [normal]
     for style, variant and width, [normal] (that is 400) for weight, and
     [normal] for line-height. Dropping a slot is a node change, so pp holds
     every slot it parsed and the optimizer drops. *)
  check_declaration ~expected:"font:400 12px serif" ~optimized:"font:12px serif"
    "font: 400 12px serif";
  check_declaration ~expected:"font:12px/normal serif"
    ~optimized:"font:12px serif" "font: 12px/normal serif";
  check_declaration ~expected:"font:small-caps 400 12px/normal serif"
    ~optimized:"font:small-caps 12px serif"
    "font: normal small-caps 400 normal 12px/normal serif";
  (* A bare [normal] in the prefix names the initial of whichever of the four
     slots is still open, so the reader binds no slot for it and the two layers
     agree from the start. *)
  check_declaration ~expected:"font:12px serif" ~optimized:"font:12px serif"
    "font: normal 12px serif";
  (* A slot that is not its longhand's initial stays in both layers. *)
  check_declaration ~expected:"font:italic 12px serif"
    ~optimized:"font:italic 12px serif" "font: italic 12px serif";
  check_declaration ~expected:"font:12px/1.5 serif"
    ~optimized:"font:12px/1.5 serif" "font: 12px/1.5 serif";

  (* Font style *)
  check_declaration ~expected:"font-style:normal" "font-style: normal";
  check_declaration ~expected:"font-style:italic" "font-style: italic";
  check_declaration ~expected:"font-style:oblique" "font-style: oblique";

  (* Font family - list type *)
  check_declaration ~expected:"font-family:Arial" "font-family: Arial";
  check_declaration
    ~expected:"font-family:Helvetica Neue,Helvetica,Arial,sans-serif"
    "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif";
  check_declaration ~expected:"font-family:Georgia,serif"
    "font-family: Georgia, serif";
  (* CSS Fonts 4 (ED) sec. 2.1 has the user agent iterate the list until a
     family matches, so a repeat of an earlier entry is unreachable and names
     the same value as the list without it. Dropping it is a node change, so pp
     holds the list it parsed and the optimizer folds. A one-entry list is the
     entry, so the fold has to land on the node [font-family: Arial] parses to.
     The exception is a list that would collapse to a lone generic keyword:
     [monospace, monospace] opts a bare generic back into the normal UA size, so
     dropping the duplicate would shrink the text. *)
  check_declaration ~expected:"font-family:Arial,Helvetica,Arial"
    ~optimized:"font-family:Arial,Helvetica"
    "font-family: Arial, Helvetica, Arial";
  check_declaration ~expected:"font-family:Arial,Arial"
    ~optimized:"font-family:Arial" "font-family: Arial, Arial";
  check_declaration ~expected:"font-family:monospace,monospace"
    ~optimized:"font-family:monospace,monospace"
    "font-family: monospace, monospace";
  check_declaration ~expected:"font-family:serif,serif"
    ~optimized:"font-family:serif,serif" "font-family: serif, serif";
  (* sec. 2.7 gives the [font] shorthand a [<'font-family'>#] slot, so the slot
     takes the longhand's fold. *)
  check_declaration ~expected:"font:12px Arial,Arial"
    ~optimized:"font:12px Arial" "font: 12px Arial, Arial";
  (* CSS Fonts 4 defines font-family names as <custom-ident> sequences. Quoted
     reserved words are family names; unquoted CSS-wide keywords remain CSS-wide
     keywords and must not be reinterpreted as family names. *)
  check_declaration ~expected:"font-family:\"default\"" "font-family: 'default'";
  check_declaration ~expected:"font-family:\"revert\"" "font-family: 'revert'";
  check_declaration ~expected:"font-family:revert" "font-family: revert";
  check_declaration ~expected:"font-family:revert-layer"
    "font-family: revert-layer";

  (* Line height *)
  check_declaration ~expected:"line-height:1.5" "line-height: 1.5";
  check_declaration ~expected:"line-height:2" "line-height: 2";
  check_declaration ~expected:"line-height:normal" "line-height: normal";
  check_declaration ~expected:"line-height:20px" "line-height: 20px";
  check_declaration ~expected:"line-height:1.5em" "line-height: 1.5em"

let text_properties () =
  (* Text align *)
  check_declaration ~expected:"text-align:left" "text-align: left";
  check_declaration ~expected:"text-align:right" "text-align: right";
  check_declaration ~expected:"text-align:center" "text-align: center";
  check_declaration ~expected:"text-align:justify" "text-align: justify";
  check_declaration ~expected:"text-align:start" "text-align: start";
  check_declaration ~expected:"text-align:end" "text-align: end";

  (* Text decoration *)
  check_declaration ~expected:"text-decoration:none" "text-decoration: none";
  check_declaration ~expected:"text-decoration:underline"
    "text-decoration: underline";
  check_declaration ~expected:"text-decoration:overline"
    "text-decoration: overline";
  check_declaration ~expected:"text-decoration:line-through"
    "text-decoration: line-through";
  (* CSS Text Decoration 4 (ED) sec. 2.6: the [text-decoration] shorthand sets
     the line, thickness, style and colour longhands, and "Omitted values are
     set to their initial values" - [solid] (sec. 2.2) and [currentcolor] (sec.
     2.3). Writing one out names what leaving it out names, and dropping it is a
     node change, so pp holds it and the optimizer folds. *)
  check_declaration ~expected:"text-decoration:underline solid"
    ~optimized:"text-decoration:underline" "text-decoration: underline solid";
  check_declaration ~expected:"text-decoration:underline currentColor"
    ~optimized:"text-decoration:underline"
    "text-decoration: underline currentcolor";
  check_declaration ~expected:"text-decoration:underline solid currentColor"
    ~optimized:"text-decoration:underline"
    "text-decoration: underline solid currentcolor";
  (* A non-initial style or colour stands. *)
  check_declaration ~expected:"text-decoration:underline dotted"
    ~optimized:"text-decoration:underline dotted"
    "text-decoration: underline dotted";
  check_declaration ~expected:"text-decoration:underline red"
    ~optimized:"text-decoration:underline red" "text-decoration: underline red";

  (* Text transform *)
  check_declaration ~expected:"text-transform:none" "text-transform: none";
  check_declaration ~expected:"text-transform:uppercase"
    "text-transform: uppercase";
  check_declaration ~expected:"text-transform:lowercase"
    "text-transform: lowercase";
  check_declaration ~expected:"text-transform:capitalize"
    "text-transform: capitalize";

  (* White space *)
  check_declaration ~expected:"white-space:normal" "white-space: normal";
  check_declaration ~expected:"white-space:nowrap" "white-space: nowrap";
  check_declaration ~expected:"white-space:pre" "white-space: pre";
  check_declaration ~expected:"white-space:pre-wrap" "white-space: pre-wrap";
  check_declaration ~expected:"white-space:pre-line" "white-space: pre-line";
  check_declaration ~expected:"white-space:break-spaces"
    "white-space: break-spaces";

  (* CSS Text 4 sec. 5.5: [text-wrap] is [<'text-wrap-mode'> ||
     <'text-wrap-style'>], with sec. 5.1 giving the mode [wrap | nowrap] and
     sec. 5.4 the style [auto | balance | stable | pretty]. Either component may
     be omitted and they may appear in either order, but neither may repeat. *)
  check_declaration ~expected:"text-wrap:wrap" "text-wrap: wrap";
  check_declaration ~expected:"text-wrap:nowrap" "text-wrap: nowrap";
  check_declaration ~expected:"text-wrap:auto" "text-wrap: auto";
  check_declaration ~expected:"text-wrap:balance" "text-wrap: balance";
  check_declaration ~expected:"text-wrap:pretty" "text-wrap: pretty";
  check_declaration ~expected:"text-wrap:stable" "text-wrap: stable";
  check_declaration ~expected:"text-wrap:nowrap balance"
    "text-wrap: nowrap balance";
  (* The two components print mode-first whichever order they arrived in, which
     is how Chrome serialises them too. *)
  check_declaration ~expected:"text-wrap:nowrap balance"
    "text-wrap: balance nowrap";
  check_declaration ~expected:"text-wrap:wrap auto" "text-wrap: wrap auto";
  neg_cursor read_declaration "text-wrap: wrap nowrap";
  neg_cursor read_declaration "text-wrap: wrap wrap";
  neg_cursor read_declaration "text-wrap: balance pretty";
  neg_cursor read_declaration "text-wrap: wrap balance pretty"

let flexbox_direction () =
  (* Flex direction *)
  check_declaration ~expected:"flex-direction:row" "flex-direction: row";
  check_declaration ~expected:"flex-direction:row-reverse"
    "flex-direction: row-reverse";
  check_declaration ~expected:"flex-direction:column" "flex-direction: column";
  check_declaration ~expected:"flex-direction:column-reverse"
    "flex-direction: column-reverse"

let flexbox_wrap () =
  check_declaration ~expected:"flex-wrap:nowrap" "flex-wrap: nowrap";
  check_declaration ~expected:"flex-wrap:wrap" "flex-wrap: wrap";
  check_declaration ~expected:"flex-wrap:wrap-reverse" "flex-wrap: wrap-reverse"

let flexbox_flex_and_basis () =
  (* Flex shorthand *)
  check_declaration ~expected:"flex:1" "flex: 1";
  check_declaration ~expected:"flex:1 auto" "flex: 1 1 auto";
  check_declaration ~expected:"flex:0 auto" "flex: 0 1 auto";
  check_declaration ~expected:"flex:initial" "flex: initial";
  check_declaration ~expected:"flex:none" "flex: none";
  check_declaration ~expected:"flex:auto" "flex: auto";
  (* Flex grow/shrink *)
  check_declaration ~expected:"flex-grow:0" "flex-grow: 0";
  check_declaration ~expected:"flex-grow:1" "flex-grow: 1";
  check_declaration ~expected:"flex-grow:2" "flex-grow: 2";
  check_declaration ~expected:"flex-shrink:0" "flex-shrink: 0";
  check_declaration ~expected:"flex-shrink:1" "flex-shrink: 1";
  (* Flex basis *)
  check_declaration ~expected:"flex-basis:auto" "flex-basis: auto";
  check_declaration ~expected:"flex-basis:0px" "flex-basis: 0px";
  decl_optimizes ~prop:"flex-basis" ~into:"0" "0px";
  check_declaration ~expected:"flex-basis:0%" "flex-basis: 0%";
  check_declaration ~expected:"flex-basis:100px" "flex-basis: 100px";
  check_declaration ~expected:"flex-basis:50%" "flex-basis: 50%";
  (* Canonical form for [flex-basis: calc(<var> * <number>)] - pins the output
     that a typed-constructor path must produce. When [Calc.float : float -> 'a
     calc] is generalised (CSS Values L4 sec. 10: a bare <number> is
     dimensionally neutral and unifies with any typed calc context), this is
     what the typed flex-basis Calc emitter has to round-trip to. *)
  check_declaration ~expected:"flex-basis:calc(var(--spacing)*4)"
    "flex-basis: calc(var(--spacing) * 4)";
  (* Math functions over <length-percentage> are valid flex-basis values and
     round-trip; sign() yields a <number> and is rejected. *)
  check_declaration ~expected:"flex-basis:min(10px,100%)"
    "flex-basis: min(10px, 100%)";
  check_declaration ~expected:"flex-basis:clamp(1px,var(--spacing),100%)"
    "flex-basis: clamp(1px, var(--spacing), 100%)";
  check_declaration ~expected:"flex-basis:abs(var(--x))"
    "flex-basis: abs(var(--x))";
  neg_cursor read_declaration "flex-basis: sign(var(--x))";
  (* order: a calc carrying a var stays a typed calc and round-trips; an
     all-numeric order calc folds to an integer. *)
  check_declaration ~expected:"order:2" "order: 2";
  check_declaration ~expected:"order:calc(var(--o)*-1)"
    "order: calc(var(--o) * -1)";
  check_declaration ~expected:"order:6" "order: calc(3 * 2)"

let flexbox_alignment () =
  (* Align items *)
  check_declaration ~expected:"align-items:stretch" "align-items: stretch";
  check_declaration ~expected:"align-items:flex-start" "align-items: flex-start";
  check_declaration ~expected:"align-items:flex-end" "align-items: flex-end";
  check_declaration ~expected:"align-items:center" "align-items: center";
  check_declaration ~expected:"align-items:baseline" "align-items: baseline";
  (* Justify content *)
  check_declaration ~expected:"justify-content:flex-start"
    "justify-content: flex-start";
  check_declaration ~expected:"justify-content:flex-end"
    "justify-content: flex-end";
  check_declaration ~expected:"justify-content:center" "justify-content: center";
  check_declaration ~expected:"justify-content:space-between"
    "justify-content: space-between";
  check_declaration ~expected:"justify-content:space-around"
    "justify-content: space-around";
  check_declaration ~expected:"justify-content:space-evenly"
    "justify-content: space-evenly"

let borders () =
  (* Border style *)
  check_declaration ~expected:"border-style:none" "border-style: none";
  check_declaration ~expected:"border-style:solid" "border-style: solid";
  check_declaration ~expected:"border-style:dashed" "border-style: dashed";
  check_declaration ~expected:"border-style:dotted" "border-style: dotted";
  check_declaration ~expected:"border-style:double" "border-style: double";
  check_declaration ~expected:"border-style:groove" "border-style: groove";
  check_declaration ~expected:"border-style:ridge" "border-style: ridge";
  check_declaration ~expected:"border-style:inset" "border-style: inset";
  check_declaration ~expected:"border-style:outset" "border-style: outset";

  (* Border width *)
  check_declaration ~expected:"border-width:1px" "border-width: 1px";
  check_declaration ~expected:"border-width:.0625rem" "border-width: .0625rem";
  check_declaration ~expected:"border-width:2px" "border-width: 2px";
  check_declaration ~expected:"border-width:thin" "border-width: thin";
  check_declaration ~expected:"border-width:medium" "border-width: medium";
  check_declaration ~expected:"border-width:thick" "border-width: thick";

  (* Border radius *)
  check_declaration ~expected:"border-radius:0" "border-radius: 0";
  check_declaration ~expected:"border-radius:5px" "border-radius: 5px";
  check_declaration ~expected:"border-radius:50%" "border-radius: 50%";
  check_declaration ~expected:"border-radius:10px" "border-radius: 10px";

  (* Individual borders *)
  check_declaration ~expected:"border-top-style:solid" "border-top-style: solid";
  check_declaration ~expected:"border-right-style:dashed"
    "border-right-style: dashed";
  check_declaration ~expected:"border-bottom-style:dotted"
    "border-bottom-style: dotted";
  check_declaration ~expected:"border-left-style:double"
    "border-left-style: double";

  check_declaration ~expected:"border-top-width:1px" "border-top-width: 1px";
  check_declaration ~expected:"border-right-width:2px" "border-right-width: 2px";
  check_declaration ~expected:"border-bottom-width:3px"
    "border-bottom-width: 3px";
  check_declaration ~expected:"border-left-width:4px" "border-left-width: 4px";
  check_declaration ~expected:"border:.0625rem solid#333"
    "border: .0625rem solid #333";

  check_declaration ~expected:"border-top-color:red" "border-top-color: red";
  check_declaration ~expected:"border-right-color:blue"
    ~optimized:"border-right-color:#00f" "border-right-color: blue";
  check_declaration ~expected:"border-bottom-color:green"
    "border-bottom-color: green";
  check_declaration ~expected:"border-left-color:yellow"
    ~optimized:"border-left-color:#ff0" "border-left-color: yellow"

(* CSS Basic User Interface 4 (ED) sec. 3.2 gives outline-width the value
   [<line-width>] and says it "accepts the same values as border-width [...]
   with the same meaning"; CSS Backgrounds 3 (ED) sec. 3.3 defines
   [<line-width>] as [thin | medium | thick | <length>]. The width slot of the
   outline shorthand (sec. 3.1) is [<'outline-width'>], so the keywords and the
   math functions read there too. *)
let outline_line_width () =
  check_declaration ~expected:"outline-width:thin" "outline-width: thin";
  check_declaration ~expected:"outline-width:medium" "outline-width: medium";
  check_declaration ~expected:"outline-width:thick" "outline-width: thick";
  check_declaration ~expected:"outline:thin solid red" "outline: thin solid red";
  (* CSS UI 4 (ED) sec. 3.1: the outline shorthand sets all three longhands, and
     CSS Cascade 5 (ED) sec. 3 assigns an omitted sub-property its initial
     value, which sec. 3.2 gives as [medium]. Spelling [medium] beside another
     slot says what leaving the slot out already says, so the optimizer drops it
     and pp prints the node it was handed. A lone [medium] leaves the shorthand
     declaring nothing but initial values, which is what [outline: none]
     declares. *)
  check_declaration ~expected:"outline:medium solid red"
    ~optimized:"outline:solid red" "outline: medium solid red";
  check_declaration ~expected:"outline:medium dashed"
    ~optimized:"outline:dashed" "outline: medium dashed";
  check_declaration ~expected:"outline:medium red" ~optimized:"outline:red"
    "outline: medium red";
  (* sec. 3.1: a lone [auto], and an [auto] beside a width but no explicit style
     or colour, both set outline-style and outline-color to [auto], so the two
     spellings are the same declaration here too. *)
  check_declaration ~expected:"outline:medium auto" ~optimized:"outline:auto"
    "outline: medium auto";
  check_declaration ~expected:"outline:medium" ~optimized:"outline:none"
    "outline: medium";
  check_declaration ~expected:"outline:thick solid red"
    "outline: thick solid red";
  (* CSS Values 4 sec. 10.2: a comparison over same-unit constants denotes one
     length, so the optimizer reduces it to that length, as at every other
     [<line-width>]; pp holds the call it was written as. *)
  check_declaration ~expected:"outline-width:calc(1px + 1px)"
    ~optimized:"outline-width:2px" "outline-width: calc(1px + 1px)";
  check_declaration ~expected:"outline-width:min(3px)"
    ~optimized:"outline-width:3px" "outline-width: min(3px)";
  check_declaration ~expected:"outline-width:clamp(1px,2px,3px)"
    ~optimized:"outline-width:2px" "outline-width: clamp(1px,2px,3px)";
  check_declaration ~expected:"outline:calc(1px + 1px) solid red"
    ~optimized:"outline:2px solid red" "outline: calc(1px + 1px) solid red";
  check_declaration ~expected:"outline:min(3px) solid red"
    ~optimized:"outline:3px solid red" "outline: min(3px) solid red";
  check_declaration ~expected:"outline:clamp(1px,2px,3px) solid red"
    ~optimized:"outline:2px solid red" "outline: clamp(1px,2px,3px) solid red";
  (* No unit relates dvh to px, so the comparison stands as written. *)
  check_declaration ~roundtrip:true "outline-width:max(3dvh,4px)";
  (* A [<line-width>] is non-negative. *)
  neg_cursor read_declaration "outline-width:-1px";
  (* Controls: a plain length still reads, and the other shorthand slots are
     untouched. *)
  check_declaration ~expected:"outline-width:0px" ~optimized:"outline-width:0"
    "outline-width: 0px";
  check_declaration ~expected:"outline-width:2px" "outline-width: 2px";
  check_declaration ~expected:"outline:0px solid red"
    ~optimized:"outline:0 solid red" "outline: 0px solid red";
  check_declaration ~expected:"outline:2px solid red" "outline: 2px solid red";
  check_declaration ~expected:"outline:none" "outline: none";
  check_declaration ~expected:"outline:auto" "outline: auto";
  check_declaration ~expected:"outline:red" "outline: red";
  check_declaration ~expected:"outline:solid red" "outline: solid red"

(* CSS Backgrounds 3 (ED) sec. 3.3 gives border-width the value
   [<line-width>{1,4}] and defines [<line-width>] as [<length [0,inf]> | thin |
   medium | thick]; the border shorthands (sec. 3.4) read [<line-width> ||
   <line-style> || <color>], so their width slot is that same production.
   css-logical-1 sec. 4.4 repeats it for the logical shorthands, and CSS
   Multi-column 1 (ED) sec. 4.5 gives column-rule [<'column-rule-width'> ||
   <'column-rule-style'> || <'column-rule-color'>], whose width is a
   [<line-width>] (sec. 4.4). Every fold the longhand performs is therefore due
   in the shorthand width slot. *)
let border_line_width () =
  (* CSS Values 4 sec. 10.2: a sum or comparison over same-unit constants
     denotes one length, so the optimizer reduces it to that length; pp holds
     the call it was written as. *)
  check_declaration ~expected:"border-width:calc(1px + 1px)"
    ~optimized:"border-width:2px" "border-width: calc(1px + 1px)";
  check_declaration ~expected:"border:calc(1px + 1px) solid red"
    ~optimized:"border:2px solid red" "border: calc(1px + 1px) solid red";
  check_declaration ~expected:"border:min(3px) solid red"
    ~optimized:"border:3px solid red" "border: min(3px) solid red";
  check_declaration ~expected:"border:clamp(1px,2px,3px) solid red"
    ~optimized:"border:2px solid red" "border: clamp(1px,2px,3px) solid red";
  check_declaration ~expected:"border-top:calc(1px + 1px) solid red"
    ~optimized:"border-top:2px solid red"
    "border-top: calc(1px + 1px) solid red";
  check_declaration ~expected:"border-inline-start:min(3px) dotted red"
    ~optimized:"border-inline-start:3px dotted red"
    "border-inline-start: min(3px) dotted red";
  check_declaration ~expected:"column-rule:clamp(1px,2px,3px) solid red"
    ~optimized:"column-rule:2px solid red"
    "column-rule: clamp(1px,2px,3px) solid red";
  (* No unit relates dvh to px, so the comparison stands as written. *)
  check_declaration ~expected:"border:max(3dvh,4px) solid red"
    ~optimized:"border:max(3dvh,4px) solid red"
    "border: max(3dvh,4px) solid red";
  (* A zero length is the same length whatever unit carries it, so [0] is its
     shortest spelling, in the shorthand slot as in the longhand. *)
  check_declaration ~expected:"border-width:0px" ~optimized:"border-width:0"
    "border-width: 0px";
  check_declaration ~expected:"border:0px" ~optimized:"border:0" "border: 0px";
  check_declaration ~expected:"border:0px solid red"
    ~optimized:"border:0 solid red" "border: 0px solid red";
  check_declaration ~expected:"border-top:0em dashed blue"
    ~optimized:"border-top:0 dashed#00f" "border-top: 0em dashed blue";
  check_declaration ~expected:"column-rule:0rem solid red"
    ~optimized:"column-rule:0 solid red" "column-rule: 0rem solid red";
  (* [thin] and [thick] name a UA-chosen thickness no length spells, so no fold
     reaches them. *)
  check_declaration ~expected:"border:thin solid red"
    ~optimized:"border:thin solid red" "border: thin solid red";
  check_declaration ~expected:"border:thick solid red"
    ~optimized:"border:thick solid red" "border: thick solid red";
  (* [medium] is the initial [<line-width>], and sec. 3.4 sets an omitted
     shorthand slot to its initial value, so an explicit [medium] beside another
     slot is what omitting it already means: the optimizer drops it, pp holds
     the node. A lone [medium] leaves the shorthand declaring nothing but
     initial values, which is what [border: none] declares. *)
  check_declaration ~expected:"border:medium solid red"
    ~optimized:"border:solid red" "border: medium solid red";
  check_declaration ~expected:"border-top:medium dashed blue"
    ~optimized:"border-top:dashed#00f" "border-top: medium dashed blue";
  check_declaration ~expected:"column-rule:medium solid red"
    ~optimized:"column-rule:solid red" "column-rule: medium solid red";
  check_declaration ~expected:"border-inline-start:medium dotted red"
    ~optimized:"border-inline-start:dotted red"
    "border-inline-start: medium dotted red";
  check_declaration ~expected:"border:medium red" ~optimized:"border:red"
    "border: medium red";
  check_declaration ~expected:"border:medium" ~optimized:"border:none"
    "border: medium";
  (* Controls: a plain length stands, and a shorthand that fills no width slot
     is untouched. *)
  check_declaration ~expected:"border:1px solid red"
    ~optimized:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"border:solid red" ~optimized:"border:solid red"
    "border: solid red";
  check_declaration ~expected:"border:red" ~optimized:"border:red" "border: red"

(* CSS Backgrounds 3 (ED) sec. 3.2 gives the border-style properties the initial
   value [none], and sec. 3.4 sets an omitted shorthand slot to its initial
   value, so an explicit [none] beside another slot says what leaving the slot
   out already says: the optimizer drops it, pp holds the node it was handed.
   Drained of every slot the shorthand declares nothing but initial values,
   which is what the [none] keyword declares, so that is the node it folds to -
   and the node [border: none] parses to directly, which is how the two
   spellings reach factoring as one. CSS UI 4 (ED) sec. 3.3 gives outline-style
   the same initial value and sec. 3.1 the same shorthand rule. *)
let border_line_style () =
  check_declaration ~expected:"border:medium none" ~optimized:"border:none"
    "border: medium none";
  check_declaration ~expected:"outline:medium none" ~optimized:"outline:none"
    "outline: medium none";
  check_declaration ~expected:"border:none red" ~optimized:"border:red"
    "border: none red";
  check_declaration ~expected:"outline:none red" ~optimized:"outline:red"
    "outline: none red";
  (* The keyword itself is already that node, and must keep printing, not empty
     out. *)
  check_declaration ~expected:"border:none" ~optimized:"border:none"
    "border: none";
  check_declaration ~expected:"outline:none" ~optimized:"outline:none"
    "outline: none";
  (* The other shorthands reading the same production. *)
  check_declaration ~expected:"border-top:medium none"
    ~optimized:"border-top:none" "border-top: medium none";
  check_declaration ~expected:"border-inline:medium none"
    ~optimized:"border-inline:none" "border-inline: medium none";
  check_declaration ~expected:"border-block-start:medium none"
    ~optimized:"border-block-start:none" "border-block-start: medium none";
  check_declaration ~expected:"column-rule:medium none"
    ~optimized:"column-rule:none" "column-rule: medium none";
  (* Controls: a style that is not the initial one stands, a zero width is not
     the initial width, and [auto] is outline-style's own value, not [none]. *)
  check_declaration ~expected:"border:1px solid red"
    ~optimized:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"border:solid red" ~optimized:"border:solid red"
    "border: solid red";
  check_declaration ~expected:"border:0 none" ~optimized:"border:0"
    "border: 0 none";
  check_declaration ~expected:"border:0" ~optimized:"border:0" "border: 0";
  check_declaration ~expected:"outline:0" ~optimized:"outline:0" "outline: 0";
  check_declaration ~expected:"outline:auto" ~optimized:"outline:auto"
    "outline: auto"

(* CSS Backgrounds 3 (ED) sec. 2.10 has the [background] shorthand reset every
   longhand it covers and then set the ones written, so a slot holding its own
   initial value says what leaving it out says: sec. 2.3 makes that [none] for
   the image, sec. 2.4 [repeat] for the repeat, sec. 2.5 [scroll] for the
   attachment, sec. 2.9 [auto] for the size and sec. 2.2 [transparent] for the
   colour. The fold is the optimizer's; pp holds the node it was handed. *)
let background_initial_slots () =
  check_declaration ~expected:"background:none red" ~optimized:"background:red"
    "background: none red";
  check_declaration ~expected:"background:repeat red"
    ~optimized:"background:red" "background: red repeat";
  check_declaration ~expected:"background:scroll red"
    ~optimized:"background:red" "background: red scroll";
  check_declaration ~expected:"background:url(a.png)transparent"
    ~optimized:"background:url(a.png)" "background: url(a.png) transparent";
  (* Controls: a slot that is not at its initial value stands. *)
  check_declaration ~expected:"background:no-repeat red"
    ~optimized:"background:no-repeat red" "background: red no-repeat";
  check_declaration ~expected:"background:fixed red"
    ~optimized:"background:fixed red" "background: red fixed";
  check_declaration ~expected:"background:url(a.png)red"
    ~optimized:"background:url(a.png)red" "background: url(a.png) red"

(* CSS Backgrounds 3 (ED) sec. 2.6 gives background-position the initial value
   [0% 0%], which [0 0] and [left top] both name, so the slot drops with the
   rest. It also reads a lone value as "the second value is assumed to be
   center", so [0] names [0 50%] and stays: no initial position is spelled that
   way. sec. 2.10 writes the size after the position and a [/], so the position
   can only leave once the size has. *)
let background_position_slot () =
  check_declaration ~expected:"background:0 0 red" ~optimized:"background:red"
    "background: red 0 0";
  check_declaration ~expected:"background:left top red"
    ~optimized:"background:red" "background: red left top";
  check_declaration ~expected:"background:0 0/auto red"
    ~optimized:"background:red" "background: red 0 0 / auto";
  (* A single value is horizontal, with [center] vertically: not the initial
     position, so it stays whatever the layer carries. *)
  check_declaration ~expected:"background:0 red" ~optimized:"background:0 red"
    "background: red 0";
  check_declaration ~expected:"background:left red"
    ~optimized:"background:0 red" "background: red left";
  check_declaration ~expected:"background:url(a.png)0"
    ~optimized:"background:url(a.png)0" "background: url(a.png) 0";
  check_declaration ~expected:"background:url(a.png)left"
    ~optimized:"background:url(a.png)0" "background: url(a.png) left";
  (* A size keeps its position, which has to be written for the [/] to attach
     to. *)
  check_declaration ~expected:"background:0 0/cover red"
    ~optimized:"background:0 0/cover red" "background: red 0 0 / cover"

(* CSS Backgrounds 3 (ED) sec. 2.10 reads one [<box>] as setting both
   background-origin and background-clip and two as setting origin then clip, so
   the pair drops together or not at all: sec. 2.8 makes [padding-box] the
   initial origin and sec. 2.7 [border-box] the initial clip. Dropping just the
   one at its initial value would leave a single [<box>] that reassigns the
   other. *)
let background_box_slots () =
  check_declaration ~expected:"background:padding-box border-box red"
    ~optimized:"background:red" "background: red padding-box border-box";
  check_declaration ~expected:"background:border-box border-box red"
    ~optimized:"background:border-box red"
    "background: red border-box border-box";
  check_declaration ~expected:"background:content-box content-box red"
    ~optimized:"background:content-box red"
    "background: red content-box content-box";
  (* A single [<box>] is one slot in the node, and pp holds the node: it is the
     re-reading of that single keyword that sets both longhands. *)
  check_declaration ~expected:"background:border-box red"
    ~optimized:"background:border-box red" "background: red border-box";
  (* Both are written out where a single [<box>] would name a different pair. *)
  check_declaration ~expected:"background:padding-box content-box red"
    ~optimized:"background:padding-box content-box red"
    "background: red padding-box content-box";
  check_declaration ~expected:"background:content-box border-box red"
    ~optimized:"background:content-box border-box red"
    "background: red content-box border-box"

(* CSS Masking 1 (ED) sec. 8.2 gives mask-border-mode the initial value [alpha],
   and sec. 8.7 sets an omitted shorthand slot to its initial value, so an
   explicit [alpha] says what leaving the slot out says. [luminance] is the
   other mode and stays. *)
let mask_border_mode_slot () =
  check_declaration ~expected:"mask-border:url(a.png)alpha"
    ~optimized:"mask-border:url(a.png)" "mask-border: url(a.png) alpha";
  check_declaration ~expected:"mask-border:url(a.png)luminance"
    ~optimized:"mask-border:url(a.png)luminance"
    "mask-border: url(a.png) luminance";
  check_declaration ~expected:"border-image:url(a.png)"
    ~optimized:"border-image:url(a.png)" "border-image: url(a.png)"

(* CSS Masking 1 (ED) sec. 8.7 writes mask-border as [<'mask-border-source'> ||
   <'mask-border-slice'> [ / <'mask-border-width'>? [ / <'mask-border-outset'>
   ]? ]? || <'mask-border-repeat'> || <'mask-border-mode'>], and CSS Values 4
   (ED) sec. 2.2 has [||] ask for one or more of its options, so the mode on its
   own is a whole value. sec. 8.2 gives mask-border-mode the initial value
   [alpha], so [alpha] drains the shorthand and what is left is the six
   initials, which is what [none] declares (sec. 8.1). CSS Backgrounds 3 (ED)
   sec. 5.7 writes border-image without a mode slot, so neither keyword is a
   border-image value wherever it is written. *)
let mask_border_mode_only () =
  check_declaration ~roundtrip:true ~expected:"mask-border:alpha"
    ~optimized:"mask-border:none" "mask-border: alpha";
  check_declaration ~roundtrip:true ~expected:"mask-border:luminance"
    ~optimized:"mask-border:luminance" "mask-border: luminance";
  check_declaration ~expected:"mask-border:none" ~optimized:"mask-border:none"
    "mask-border: none";
  neg_cursor read_declaration "border-image: alpha";
  neg_cursor read_declaration "border-image: luminance";
  neg_cursor read_declaration "border-image: url(a.png) alpha";
  neg_cursor read_declaration "border-image: url(a.png) luminance"

(* CSS Box 4 (ED) sec. 3.2 assigns the values of a one-to-four value box
   shorthand to the four sides: one value goes to all four, two to top-bottom
   then left-right, three to top, left-right, bottom. A repeat that those rules
   already supply is the longer spelling of the same declaration, so the
   optimizer drops it and pp prints the list it was handed. sec. 4.2 says the
   same for padding, css-logical-1 sec. 4.3 assigns inset's values "as for
   margin" and sec. 4.4 padding-block/padding-inline's, and CSS Backgrounds 3
   (ED) sec. 3.1 and sec. 4.1 give border-color and border-radius the same
   four-value form. *)
let box_shorthand_repeats () =
  check_declaration ~expected:"inset:0 0 0 0" ~optimized:"inset:0"
    "inset: 0 0 0 0";
  check_declaration ~expected:"margin:1px 1px 1px 1px" ~optimized:"margin:1px"
    "margin: 1px 1px 1px 1px";
  check_declaration ~expected:"padding:1px 2px 1px 2px"
    ~optimized:"padding:1px 2px" "padding: 1px 2px 1px 2px";
  check_declaration ~expected:"margin:1px 2px 3px 2px"
    ~optimized:"margin:1px 2px 3px" "margin: 1px 2px 3px 2px";
  check_declaration ~expected:"margin:1px 1px 2px"
    ~optimized:"margin:1px 1px 2px" "margin: 1px 1px 2px";
  check_declaration ~expected:"border-color:red red red red"
    ~optimized:"border-color:red" "border-color: red red red red";
  check_declaration ~expected:"inset-block:0 0" ~optimized:"inset-block:0"
    "inset-block: 0 0";
  check_declaration ~expected:"padding-inline:1px 1px"
    ~optimized:"padding-inline:1px" "padding-inline: 1px 1px";
  check_declaration ~expected:"border-radius:1px 1px 1px 1px"
    ~optimized:"border-radius:1px" "border-radius: 1px 1px 1px 1px";
  (* CSS Backgrounds 3 (ED) sec. 4.1 puts the vertical radii after a [/], so
     each group collapses on its own and neither reaches across the slash. With
     no slash the values set both axes equally, so a vertical group equal to the
     horizontal one says what omitting it says. *)
  check_declaration
    ~expected:"border-radius:5px 5px 5px 5px/10px 10px 10px 10px"
    ~optimized:"border-radius:5px/10px"
    "border-radius: 5px 5px 5px 5px / 10px 10px 10px 10px";
  check_declaration ~expected:"border-radius:5px 10px 5px 10px/1px 2px 1px 2px"
    ~optimized:"border-radius:5px 10px/1px 2px"
    "border-radius: 5px 10px 5px 10px / 1px 2px 1px 2px";
  check_declaration ~expected:"border-radius:5px/5px"
    ~optimized:"border-radius:5px" "border-radius: 5px / 5px";
  check_declaration ~expected:"border-radius:5px 10px/5px 10px"
    ~optimized:"border-radius:5px 10px" "border-radius: 5px 10px / 5px 10px";
  (* Control: the two axes differ, so both groups are written. *)
  check_declaration ~expected:"border-radius:5px/10px"
    ~optimized:"border-radius:5px/10px" "border-radius: 5px / 10px";
  (* The per-side folds run first, so sides that only agree once normalised
     still collapse. *)
  check_declaration ~expected:"margin:0px 0 0px 0" ~optimized:"margin:0"
    "margin: 0px 0 0px 0";
  (* Controls: a list the rules cannot rebuild stays as written. *)
  check_declaration ~expected:"margin:1px 2px" ~optimized:"margin:1px 2px"
    "margin: 1px 2px";
  check_declaration ~expected:"margin:1px 2px 3px 4px"
    ~optimized:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"padding:1px 2px 1px 3px"
    ~optimized:"padding:1px 2px 1px 3px" "padding: 1px 2px 1px 3px"

let substitution_shorthand_cardinality () =
  let concat = String.concat "" in
  let check_preserved property ~held value =
    let input = concat [ property; ": "; value ] in
    let expected = concat [ property; ":"; held ] in
    check_declaration ~expected ~optimized:expected input
  in
  (* CSS Values 5 arbitrary substitution happens before the property's grammar
     is checked. A top-level var() can therefore contribute several box
     components: dropping one authored slot can turn an invalid computed value
     into a valid one or change the side assignment. *)
  List.iter
    (fun (property, repeated, separator) ->
      check_preserved property
        ~held:
          (concat
             [ "var(--v)"; separator; repeated; " "; repeated; " "; repeated ])
        (concat [ "var(--v) "; repeated; " "; repeated; " "; repeated ]))
    [
      ("margin", "1px", "");
      ("padding", "1px", "");
      ("inset", "1px", "");
      ("scroll-margin", "1px", " ");
      ("scroll-padding", "1px", " ");
      ("border-color", "red", "");
    ];
  List.iter
    (fun (property, separator) ->
      check_preserved property
        ~held:(concat [ "var(--v)"; separator; "var(--v)" ])
        "var(--v) var(--v)")
    [
      ("margin-inline", "");
      ("margin-block", "");
      ("padding-inline", "");
      ("padding-block", "");
      ("inset-inline", "");
      ("inset-block", "");
      ("scroll-margin-inline", " ");
      ("scroll-margin-block", " ");
      ("scroll-padding-inline", " ");
      ("scroll-padding-block", " ");
    ];
  (* env() and attr() are substitution functions too; keep the guard attached to
     the AST shape rather than special-casing var() spellings. *)
  check_preserved "margin" ~held:"env(safe-area-inset-top)1px 1px 1px"
    "env(safe-area-inset-top) 1px 1px 1px";
  check_preserved "margin" ~held:"attr(data-m px)1px 1px 1px"
    "attr(data-m px) 1px 1px 1px";
  check_preserved "border-color"
    ~held:"attr(data-color type(<color>))red red red"
    "attr(data-color type(<color>)) red red red";
  check_preserved "border-radius" ~held:"var(--r)1px 1px 1px"
    "var(--r) 1px 1px 1px";
  check_preserved "border-radius" ~held:"var(--r)/var(--r)" "var(--r)/var(--r)"

(* A [var()] standing beside other components is one operand of the property's
   own grammar rather than a substitution of the whole value: CSS Cascade 5 sec.
   6 keeps the whole-value reading for the CSS-wide keywords, which are valid
   only as the entire value, and a lone [var()] takes it the same way.
   Committing to the whole-value reading on sight of a leading [var()] left the
   components after it unread, so the typed parse failed and the declaration was
   carried as an opaque token stream, costing the components beside the
   reference every fold they have on their own. *)
let component_var_keeps_typed_value () =
  check_declaration ~expected:"border-radius:var(--x)0px"
    ~optimized:"border-radius:var(--x)0" "border-radius: var(--x) 0px";
  (* A top-level var() can expand to several radii, so changing the authored
     component count is not safe before substitution. *)
  check_declaration ~expected:"border-radius:var(--x)1px 1px 1px"
    ~optimized:"border-radius:var(--x)1px 1px 1px"
    "border-radius: var(--x) 1px 1px 1px";
  check_declaration ~expected:"gap:var(--g) 0px" ~optimized:"gap:var(--g) 0"
    "gap: var(--g) 0px";
  check_declaration ~expected:"transform-origin:var(--t) 0px"
    ~optimized:"transform-origin:var(--t) 0" "transform-origin: var(--t) 0px";
  check_declaration ~expected:"border-spacing:var(--s) 0px"
    ~optimized:"border-spacing:var(--s) 0" "border-spacing: var(--s) 0px";
  (* Controls: a lone [var()] is the whole value, a CSS-wide keyword after one
     is still the invalid mix that keeps the declaration opaque, and a reader
     whose grammar takes one component per slot keeps reading it that way. *)
  check_declaration ~expected:"border-radius:var(--x)" "border-radius: var(--x)";
  check_declaration ~expected:"border-radius:var(--x) inherit"
    "border-radius: var(--x) inherit";
  (* CSS Values 4 sec. 4.1 makes a keyword ASCII case-insensitive, so a typed
     read answers with the keyword where an opaque stream keeps the case the
     author wrote. *)
  check_specified_value "place-content reads its components"
    "place-content: var(--p) CENTER" "var(--p) center";
  check_visible_var "place-content component var stays visible"
    "place-content:var(--p) center" "--p";
  (* A var() can substitute more than one component. Keeping two equal
     references is therefore cardinality-sensitive: if [--p] is [center end],
     the two-reference spelling is invalid after substitution while a folded
     single reference is valid. *)
  check_declaration ~expected:"place-content:var(--p) var(--p)"
    ~optimized:"place-content:var(--p) var(--p)"
    "place-content: var(--p) var(--p)";
  check_specified_value "place-items reads a leading component var"
    "place-items: var(--pi) CENTER" "var(--pi) center";
  check_specified_value "place-items keeps its existing trailing component var"
    "place-items: CENTER var(--pi)" "center var(--pi)";
  check_visible_var "place-items component var stays visible"
    "place-items:var(--pi) center" "--pi";
  check_declaration ~expected:"place-items:var(--pi) var(--pi)"
    ~optimized:"place-items:var(--pi) var(--pi)"
    "place-items: var(--pi) var(--pi)";
  check_specified_value "grid-auto-flow keywords are case-insensitive"
    "grid-auto-flow: ROW DENSE" "row dense";
  check_specified_value "grid-auto-flow reads a leading component var"
    "grid-auto-flow: var(--flow) DENSE" "var(--flow) dense";
  check_specified_value "grid-auto-flow reads a trailing component var"
    "grid-auto-flow: ROW var(--flow)" "row var(--flow)";
  check_visible_var "grid-auto-flow component var stays visible"
    "grid-auto-flow:row var(--flow)" "--flow";
  check_specified_value "border-image-width reads a leading component var"
    "border-image-width: var(--width) AUTO" "var(--width) auto";
  check_specified_value "border-image-width reads a trailing component var"
    "border-image-width: AUTO var(--width)" "auto var(--width)";
  check_visible_var "border-image-width component var stays visible"
    "border-image-width:var(--width) auto" "--width";
  check_specified_value "border-image-outset reads a leading component var"
    "border-image-outset: var(--outset) 1PX" "var(--outset) 1px";
  check_specified_value "border-image-outset reads a trailing component var"
    "border-image-outset: 1PX var(--outset)" "1px var(--outset)";
  check_visible_var "border-image-outset component var stays visible"
    "border-image-outset:var(--outset) 1px" "--outset";
  check_specified_value "overflow slots are unchanged"
    "overflow: var(--o) HIDDEN" "var(--o) hidden"

(* CSS Tables 3 (ED) writes [border-spacing] as one or two non-negative lengths
   and reads a single one as "both the horizontal and vertical spacing", so a
   pair of equal lengths is the longer spelling of that one value. The per-side
   fold runs first, so a pair that only agrees once normalised collapses too. *)
let border_spacing_pair () =
  check_declaration ~expected:"border-spacing:1px 1px"
    ~optimized:"border-spacing:1px" "border-spacing: 1px 1px";
  check_declaration ~expected:"border-spacing:0px 0"
    ~optimized:"border-spacing:0" "border-spacing: 0px 0";
  check_declaration ~expected:"border-spacing:0px" ~optimized:"border-spacing:0"
    "border-spacing: 0px";
  (* Controls: two different lengths name two different spacings. *)
  check_declaration ~expected:"border-spacing:1px 2px"
    ~optimized:"border-spacing:1px 2px" "border-spacing: 1px 2px";
  check_declaration ~expected:"border-spacing:1px"
    ~optimized:"border-spacing:1px" "border-spacing: 1px"

(* CSS Backgrounds 3 (ED) sec. 2.4 gives every single [<repeat-style>] keyword
   the pair it computes to: [repeat] is [repeat repeat], [space] is [space
   space], [round] is [round round], [no-repeat] is [no-repeat no-repeat],
   [repeat-x] is [repeat no-repeat] and [repeat-y] is [no-repeat repeat]. Each
   pair therefore has a one-keyword spelling of the same value, and the
   optimizer picks it; pp prints the pair it was handed. *)
let background_repeat_axes () =
  check_declaration ~expected:"background-repeat:repeat repeat"
    ~optimized:"background-repeat:repeat" "background-repeat: repeat repeat";
  check_declaration ~expected:"background-repeat:space space"
    ~optimized:"background-repeat:space" "background-repeat: space space";
  check_declaration ~expected:"background-repeat:round round"
    ~optimized:"background-repeat:round" "background-repeat: round round";
  check_declaration ~expected:"background-repeat:no-repeat no-repeat"
    ~optimized:"background-repeat:no-repeat"
    "background-repeat: no-repeat no-repeat";
  check_declaration ~expected:"background-repeat:repeat no-repeat"
    ~optimized:"background-repeat:repeat-x"
    "background-repeat: repeat no-repeat";
  check_declaration ~expected:"background-repeat:no-repeat repeat"
    ~optimized:"background-repeat:repeat-y"
    "background-repeat: no-repeat repeat";
  (* Each layer of the comma-separated list folds on its own. *)
  check_declaration ~expected:"background-repeat:repeat repeat,space space"
    ~optimized:"background-repeat:repeat,space"
    "background-repeat: repeat repeat, space space";
  (* The shorthand's repeat slot reads the same production, so the fold reaches
     it and leaves the slot at its initial value (sec. 2.4). *)
  check_declaration ~expected:"background:repeat repeat red"
    ~optimized:"background:red" "background: red repeat repeat";
  (* Controls: a pair with two different axes has no one-keyword spelling, and
     the one-keyword forms are already the node they fold to. *)
  check_declaration ~expected:"background-repeat:repeat space"
    ~optimized:"background-repeat:repeat space"
    "background-repeat: repeat space";
  check_declaration ~expected:"background-repeat:space no-repeat"
    ~optimized:"background-repeat:space no-repeat"
    "background-repeat: space no-repeat";
  check_declaration ~expected:"background-repeat:repeat-x"
    ~optimized:"background-repeat:repeat-x" "background-repeat: repeat-x";
  check_declaration ~expected:"background-repeat:repeat"
    ~optimized:"background-repeat:repeat" "background-repeat: repeat"

(* CSS Backgrounds 3 (ED) sec. 2.10 resets every longhand the shorthand covers,
   so a layer that fills no slot declares what [background: none] declares. [0
   0] is the shortest spelling of that layer, so it is the node the spellings
   meet on. *)
let background_drained_layer () =
  check_declaration ~expected:"background:none" ~optimized:"background:0 0"
    "background: none";
  check_declaration ~expected:"background:0 0" ~optimized:"background:0 0"
    "background: 0 0";
  check_declaration ~expected:"background:left top" ~optimized:"background:0 0"
    "background: left top";
  check_declaration ~expected:"background:transparent"
    ~optimized:"background:0 0" "background: transparent"

(* CSS Backgrounds 3 (ED) sec. 3.1 gives the border-color properties the initial
   value [currentColor], and sec. 3.4 sets an omitted shorthand slot to its
   initial value, so an explicit [currentColor] beside another slot says what
   leaving the colour out already says. Drained of every slot the shorthand
   declares nothing but initial values, which is what [none] declares. CSS
   Multi-column 1 (ED) sec. 4.2 and css-logical-1 sec. 4.5.3 give column-rule
   and the logical borders the same initial colour, so they take the same fold.

   CSS UI 4 (ED) sec. 3.4 does not: outline-color's initial value is [auto], a
   UA-chosen colour that [currentColor] does not name, so the outline colour
   slot holds whatever it was written with. *)
let border_line_color () =
  check_declaration ~expected:"border:solid currentColor"
    ~optimized:"border:solid" "border: solid currentcolor";
  check_declaration ~expected:"border:1px solid currentColor"
    ~optimized:"border:1px solid" "border: 1px solid currentColor";
  check_declaration ~expected:"border:currentColor" ~optimized:"border:none"
    "border: currentcolor";
  check_declaration ~expected:"border-top:solid currentColor"
    ~optimized:"border-top:solid" "border-top: solid currentcolor";
  check_declaration ~expected:"border-inline:solid currentColor"
    ~optimized:"border-inline:solid" "border-inline: solid currentcolor";
  check_declaration ~expected:"border-block-start:solid currentColor"
    ~optimized:"border-block-start:solid"
    "border-block-start: solid currentcolor";
  check_declaration ~expected:"column-rule:solid currentColor"
    ~optimized:"column-rule:solid" "column-rule: solid currentcolor";
  (* outline-color's initial value is [auto], so nothing drops here. *)
  check_declaration ~expected:"outline:solid currentColor"
    ~optimized:"outline:solid currentColor" "outline: solid currentcolor";
  check_declaration ~expected:"outline:currentColor"
    ~optimized:"outline:currentColor" "outline: currentcolor";
  (* Controls: a colour that is not the initial one stands. *)
  check_declaration ~expected:"border:solid red" ~optimized:"border:solid red"
    "border: solid red";
  check_declaration ~expected:"column-rule:solid red"
    ~optimized:"column-rule:solid red" "column-rule: solid red"

(* CSS Syntax 3 (ED) sec. 5.5.6 keeps a parsed declaration only if it "is valid
   in the current context", which for a non-custom property means its value
   matches the property's grammar. CSS Backgrounds 3 (ED) sec. 3.4 writes the
   border shorthands as [<line-width> || <line-style> || <color>], css-logical-1
   sec. 4.5.4 and CSS Multi-column 1 (ED) sec. 4.5 route the logical shorthands
   and column-rule through the same production, and CSS UI 4 (ED) sec. 3.1
   writes outline the same way. CSS Values 4 (ED) sec. 2.2 has [||] require one
   or more of its options to occur, so nothing at all matches none of these
   grammars: the declaration is dropped, not filled in with a value nobody
   wrote. *)
let empty_shorthand_value () =
  List.iter
    (fun prop -> neg_cursor read_declaration (prop ^ ": "))
    [
      "border";
      "border-top";
      "border-right";
      "border-bottom";
      "border-left";
      "border-block";
      "border-block-start";
      "border-block-end";
      "border-inline";
      "border-inline-start";
      "border-inline-end";
      "column-rule";
      "outline";
    ];
  (* Whitespace is discarded before the value is read, so a run of it is still
     an empty value. *)
  neg_cursor read_declaration "border:";
  neg_cursor read_declaration "outline:   ";
  (* Controls: every slot is optional, so any one of them is a whole value. *)
  check_declaration ~expected:"border:none" ~optimized:"border:none"
    "border: none";
  check_declaration ~expected:"border:1px solid red"
    ~optimized:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"outline:none" ~optimized:"outline:none"
    "outline: none";
  check_declaration ~expected:"outline:auto" ~optimized:"outline:auto"
    "outline: auto";
  check_declaration ~expected:"column-rule:1px solid red"
    ~optimized:"column-rule:1px solid red" "column-rule: 1px solid red"

(* The record behind each shorthand is public, so a caller can hand the printer
   a value with no slot filled. That value declares nothing but the initial
   longhands, which is what the [none] keyword declares (CSS Backgrounds 3 (ED)
   sec. 3.4 and sec. 5.7, CSS UI 4 (ED) sec. 3.1, CSS Text Decoration 4 (ED)
   sec. 2.6, CSS Masking 1 (ED) sec. 8.7), and [none] is the spelling that says
   so. An empty string is not a spelling of anything: no parser accepts it.
   [Css.to_string] does not normalize, so the printer is the only thing standing
   between such a record and the output. *)
let all_initial_shorthand_prints_none () =
  let pp v = Css.Pp.to_string ~minify:true Css.Declaration.pp v in
  let border : Css.border =
    Shorthand { width = None; style = None; color = None }
  in
  let outline : Css.outline =
    Shorthand { width = None; style = None; color = None }
  in
  let text_decoration : Css.text_decoration =
    Shorthand { lines = []; style = None; color = None; thickness = None }
  in
  let border_image : Css.Properties.border_image =
    {
      source = None;
      slice = None;
      width = None;
      outset = None;
      repeat = None;
      mode = None;
    }
  in
  Alcotest.(check string)
    "drained border shorthand" "border:none"
    (pp (Css.Declaration.v Border border));
  Alcotest.(check string)
    "drained outline shorthand" "outline:none"
    (pp (Css.Declaration.v Outline outline));
  Alcotest.(check string)
    "drained text-decoration shorthand" "text-decoration:none"
    (pp (Css.Declaration.v Text_decoration text_decoration));
  Alcotest.(check string)
    "outline_shorthand with no slot" "outline:none"
    (pp (Css.Declaration.outline (Css.outline_shorthand ())));
  Alcotest.(check string)
    "border_shorthand with no slot" "border:none"
    (pp (Css.Declaration.v Border (Css.border_shorthand ())));
  Alcotest.(check string)
    "text_decoration_shorthand with no slot" "text-decoration:none"
    (pp (Css.Declaration.v Text_decoration (Css.text_decoration_shorthand ())));
  Alcotest.(check string)
    "drained mask-border shorthand" "mask-border:none"
    (pp (Css.Declaration.v Mask_border border_image));
  Alcotest.(check string)
    "drained border-image shorthand" "border-image:none"
    (pp (Css.Declaration.v Border_image border_image))

let logical_border_shorthands () =
  (* css-logical-1 sec. 4.4.1: border-block-start, border-block-end,
     border-inline-start and border-inline-end take <'border-top-width'> ||
     <'border-top-style'> || <'border-top-color'>. sec. 4.4.2: border-block and
     border-inline take <'border-block-start'>. Chrome 151 accepts every vector
     below and reports the same serialisation. *)
  check_declaration ~expected:"border-inline:1px solid red"
    "border-inline: 1px solid red";
  check_declaration ~expected:"border-inline-start:2px dashed red"
    "border-inline-start: 2px dashed red";
  check_declaration ~expected:"border-inline-end:3px dotted red"
    "border-inline-end: 3px dotted red";
  check_declaration ~expected:"border-block-start:4px double red"
    "border-block-start: 4px double red";
  check_declaration ~expected:"border-block-end:5px groove red"
    "border-block-end: 5px groove red";
  (* The || combinator makes each component optional and order-free; the
     serialisation is width, style, colour. *)
  check_declaration ~expected:"border-inline:solid" "border-inline: solid";
  check_declaration ~expected:"border-inline:1px" "border-inline: 1px";
  check_declaration ~expected:"border-block-start:medium"
    "border-block-start: medium";
  check_declaration ~expected:"border-inline:1px solid red"
    "border-inline: red 1px solid";
  (* A second width is not in the grammar; Chrome drops the declaration. *)
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "border-inline: 1px solid red 2px";
      "border-inline-start: 1px solid red 2px";
      "border-block-end: 1px solid red 2px";
    ]

let overflow () =
  check_declaration ~expected:"overflow:visible" "overflow: visible";
  check_declaration ~expected:"overflow:visible!important"
    "overflow: visible !important";
  check_declaration ~expected:"overflow:hidden" "overflow: hidden";
  check_declaration ~expected:"overflow:scroll" "overflow: scroll";
  check_declaration ~expected:"overflow:auto" "overflow: auto";
  check_declaration ~expected:"overflow:clip" "overflow: clip";
  (* CSS Overflow 3 (ED) sec. 3.1: [overflow] is [<'overflow-block'>{1,2}] and
     "sets the specified values of overflow-x and overflow-y in that order. If
     the second value is omitted, it is copied from the first." A pair of equal
     values therefore says what the single value says, and dropping the second
     is a node change, so pp holds the pair and the optimizer folds. *)
  check_declaration ~expected:"overflow:auto auto" ~optimized:"overflow:auto"
    "overflow: auto auto";
  check_declaration ~expected:"overflow:hidden hidden"
    ~optimized:"overflow:hidden" "overflow: hidden hidden";
  (* Two different values name two axes, so the pair stands. *)
  check_declaration ~expected:"overflow:visible hidden"
    ~optimized:"overflow:visible hidden" "overflow: visible hidden";

  check_declaration ~expected:"overflow-x:visible" "overflow-x: visible";
  check_declaration ~expected:"overflow-x:hidden" "overflow-x: hidden";
  check_declaration ~expected:"overflow-x:scroll" "overflow-x: scroll";
  check_declaration ~expected:"overflow-x:auto" "overflow-x: auto";

  check_declaration ~expected:"overflow-y:visible" "overflow-y: visible";
  check_declaration ~expected:"overflow-y:hidden" "overflow-y: hidden";
  check_declaration ~expected:"overflow-y:scroll" "overflow-y: scroll";
  check_declaration ~expected:"overflow-y:auto" "overflow-y: auto"

let animations_timing () =
  (* Animation properties *)
  check_declaration ~expected:"animation-name:slide-in"
    "animation-name: slide-in";
  check_declaration ~expected:"animation-name:none" "animation-name: none";
  (* CSS Animations 1 sec. 3: a <keyframes-name> is a <custom-ident> or a
     <string>, and only the ident arm excludes none, default and the CSS-wide
     keywords. Unquoting such a name is a different declaration, and in a list
     it is no declaration at all. *)
  check_declaration ~expected:"animation-name:\"unset\""
    "animation-name: \"unset\"";
  check_declaration ~expected:"animation-name:\"none\""
    "animation-name: \"none\"";
  check_declaration ~expected:"animation-name:\"default\""
    "animation-name: \"default\"";
  check_declaration ~expected:"animation-name:\"UNSET\""
    "animation-name: \"UNSET\"";
  check_declaration ~expected:"animation-name:\"unset\",\"revert\""
    "animation-name: \"unset\", \"revert\"";
  check_declaration ~expected:"animation-name:slide" "animation-name: \"slide\"";
  check_declaration ~expected:"animation-name:a,b"
    "animation-name: \"a\", \"b\"";

  check_declaration ~expected:"animation-duration:1s" "animation-duration: 1s";
  check_declaration ~expected:"animation-duration:.5s"
    ~optimized:"animation-duration:.5s" "animation-duration: 500ms";
  check_declaration ~expected:"transition-duration:round(1.1s,.5s)"
    ~optimized:"transition-duration:1s" "transition-duration: round(1.1s, .5s)";
  check_declaration ~expected:"animation-duration:2.5s"
    "animation-duration: 2.5s";
  (* CSS Animations 2 sec. 4.1: [ auto | <time [0s,inf]> ]#, with auto the
     initial value. The time arm keeps its lower bound, and the delays and
     transition-duration have no auto arm at all. *)
  check_declaration ~expected:"animation-duration:auto"
    "animation-duration: auto";
  check_declaration ~expected:"animation-duration:auto,1s"
    "animation-duration: auto, 1s";
  neg_cursor read_declaration "animation-duration: -1s";
  neg_cursor read_declaration "transition-duration: auto";
  neg_cursor read_declaration "animation-delay: auto";
  neg_cursor read_declaration "transition-delay: auto";

  check_declaration ~expected:"animation-timing-function:ease"
    "animation-timing-function: ease";
  check_declaration ~expected:"animation-timing-function:ease-in"
    "animation-timing-function: ease-in";
  check_declaration ~expected:"animation-timing-function:ease-out"
    "animation-timing-function: ease-out";
  check_declaration ~expected:"animation-timing-function:ease-in-out"
    "animation-timing-function: ease-in-out";
  check_declaration ~expected:"animation-timing-function:linear"
    "animation-timing-function: linear";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.4,0,.2,1)"
    "animation-timing-function: cubic-bezier(0.4, 0, 0.2, 1)";
  (* CSS Easing 1 (ED) sec. 2.2 gives each cubic bezier keyword its equivalent
     curve, and sec. 2.3 computes [step-start] to [steps(1, start)] and
     [step-end] to [steps(1, end)], with [start] behaving as [jump-start], [end]
     as [jump-end], and an omitted step position assumed to be [end]. Every pair
     below is one easing under two spellings and the keyword is the shorter one,
     so pp prints what it parsed and the optimizer folds. *)
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.25,.1,.25,1)"
    ~optimized:"animation-timing-function:ease"
    "animation-timing-function: cubic-bezier(0.25, 0.1, 0.25, 1)";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.42,0,1,1)"
    ~optimized:"animation-timing-function:ease-in"
    "animation-timing-function: cubic-bezier(0.42, 0, 1, 1)";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(0,0,.58,1)"
    ~optimized:"animation-timing-function:ease-out"
    "animation-timing-function: cubic-bezier(0, 0, 0.58, 1)";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.42,0,.58,1)"
    ~optimized:"animation-timing-function:ease-in-out"
    "animation-timing-function: cubic-bezier(0.42, 0, 0.58, 1)";
  check_declaration ~expected:"animation-timing-function:cubic-bezier(0,0,1,1)"
    ~optimized:"animation-timing-function:linear"
    "animation-timing-function: cubic-bezier(0, 0, 1, 1)";
  check_declaration ~expected:"transition-timing-function:steps(1,jump-start)"
    ~optimized:"transition-timing-function:step-start"
    "transition-timing-function: steps(1, jump-start)";
  check_declaration ~expected:"transition-timing-function:steps(1,start)"
    ~optimized:"transition-timing-function:step-start"
    "transition-timing-function: steps(1, start)";
  check_declaration ~expected:"transition-timing-function:steps(1,jump-end)"
    ~optimized:"transition-timing-function:step-end"
    "transition-timing-function: steps(1, jump-end)";
  check_declaration ~expected:"transition-timing-function:steps(1)"
    ~optimized:"transition-timing-function:step-end"
    "transition-timing-function: steps(1)";
  (* [jump-none] and [jump-both] have no keyword spelling, and neither does a
     step count above one. *)
  check_declaration ~expected:"transition-timing-function:steps(1,jump-both)"
    ~optimized:"transition-timing-function:steps(1,jump-both)"
    "transition-timing-function: steps(1, jump-both)";
  check_declaration ~expected:"transition-timing-function:steps(2,jump-start)"
    ~optimized:"transition-timing-function:steps(2,jump-start)"
    "transition-timing-function: steps(2, jump-start)";

  (* Per CSS Values 4 section 7.2 the time unit ([s] or [ms]) is required for
     [<time>]. [0s] does not drop the unit. *)
  check_declaration ~expected:"animation-delay:0s" "animation-delay: 0s";
  check_declaration ~expected:"animation-delay:1s" "animation-delay: 1s";
  check_declaration ~expected:"animation-delay:-.5s"
    ~optimized:"animation-delay:-.5s" "animation-delay: -500ms";
  (* CSS Values 4 sec. 10.3 gives these functions the type of their arguments,
     so time-valued calls fit the delay longhands' [<time>] grammar. *)
  check_declaration ~expected:"transition-delay:round(1.1s,.5s)"
    ~optimized:"transition-delay:1s" "transition-delay:round(1.1s,.5s)";
  check_declaration ~expected:"animation-delay:mod(1.1s,.5s)"
    ~optimized:"animation-delay:.1s" "animation-delay:mod(1.1s,.5s)";
  check_declaration ~expected:"animation-delay:rem(1.1s,.5s)"
    ~optimized:"animation-delay:.1s" "animation-delay:rem(1.1s,.5s)";
  check_declaration ~expected:"transition-duration:var(--d,.5s)"
    ~optimized:"transition-duration:var(--d,.5s)"
    "transition-duration:var(--d,500ms)";
  check_declaration ~expected:"interest-delay:round(1100ms,500ms)"
    ~optimized:"interest-delay:1000ms" "interest-delay:round(1100ms,500ms)";
  (* CSS Values 4 sec. 7.2: [ms] and [s] are interchangeable, and the shorter
     spelling is picked by the AST normalisation, which canonicalises a typed
     [<time>] - a [round()] operand, a [var()] fallback - but leaves the
     operands of a [calc()] that survives to the output as authored. The printer
     mirrors that split at every depth, so a fallback nested inside a [calc()]
     keeps its unit exactly like the operand beside it: two declarations that
     print alike must be structurally alike. *)
  check_declaration ~expected:"transition-duration:calc(var(--d,1000ms))"
    ~optimized:"transition-duration:calc(var(--d,1000ms))"
    "transition-duration:calc(var(--d,1000ms))";
  check_declaration
    ~expected:"transition-duration:calc(var(--a,var(--b,1000ms)))"
    ~optimized:"transition-duration:calc(var(--a,var(--b,1000ms)))"
    "transition-duration:calc(var(--a,var(--b,1000ms)))";
  check_declaration ~expected:"transition-duration:calc(var(--x) + 1500ms)"
    ~optimized:"transition-duration:calc(var(--x) + 1500ms)"
    "transition-duration:calc(var(--x) + 1500ms)";
  check_declaration ~expected:"transition-duration:round(var(--d,1s),1s)"
    ~optimized:"transition-duration:round(var(--d,1s),1s)"
    "transition-duration:round(var(--d,1000ms),1s)";
  (* [interest-delay] keeps authored milliseconds for its own operands, so a
     fallback nested in one of its [calc()] operands keeps them too. *)
  check_declaration ~expected:"interest-delay:calc(var(--a,var(--b,1000ms)))"
    ~optimized:"interest-delay:calc(var(--a,var(--b,1000ms)))"
    "interest-delay:calc(var(--a,var(--b,1000ms)))";
  let c = Cursor.of_string "transition-delay:bogus" in
  match read_declaration c with
  | exception
      Error.Parse_error { kind = Error.Bad_value { property; reason }; _ } ->
      Alcotest.(check string) "diagnostic property" "transition-delay" property;
      Alcotest.(check string) "diagnostic reason" "expected time value" reason
  | exception Error.Parse_error e ->
      Alcotest.failf "unexpected diagnostic: %s" (Error.to_string e)
  | _ -> Alcotest.fail "expected an invalid delay to be rejected"

let animations_state () =
  check_declaration ~expected:"animation-iteration-count:1"
    "animation-iteration-count: 1";
  check_declaration ~expected:"animation-iteration-count:3"
    "animation-iteration-count: 3";
  check_declaration ~expected:"animation-iteration-count:infinite"
    "animation-iteration-count: infinite";

  check_declaration ~expected:"animation-direction:normal"
    "animation-direction: normal";
  check_declaration ~expected:"animation-direction:reverse"
    "animation-direction: reverse";
  check_declaration ~expected:"animation-direction:alternate"
    "animation-direction: alternate";
  check_declaration ~expected:"animation-direction:alternate-reverse"
    "animation-direction: alternate-reverse";

  check_declaration ~expected:"animation-fill-mode:none"
    "animation-fill-mode: none";
  check_declaration ~expected:"animation-fill-mode:forwards"
    "animation-fill-mode: forwards";
  check_declaration ~expected:"animation-fill-mode:backwards"
    "animation-fill-mode: backwards";
  check_declaration ~expected:"animation-fill-mode:both"
    "animation-fill-mode: both";

  check_declaration ~expected:"animation-play-state:running"
    "animation-play-state: running";
  check_declaration ~expected:"animation-play-state:paused"
    "animation-play-state: paused"

(* CSS Animations 1 (ED) sec. 4.9 joins the eight components of a
   [<single-animation>] with [||] and assigns a keyword to a property other than
   animation-name wherever that property has not been filled yet, so the first
   [none] of [animation: none none] is the fill mode and the second the name.
   Both are the initial values of their longhands, so every slot written here
   holds one, and what the whole declares is what [animation: none] declares. *)
let animation_drained_shorthand () =
  check_declaration ~expected:"animation:none" ~optimized:"animation:none"
    "animation: none none";
  check_declaration ~expected:"animation:none" ~optimized:"animation:none"
    "animation: 0s ease 0s 1 normal none running none";
  (* Controls: the drop still runs wherever a slot outlives it. *)
  check_declaration ~expected:"animation:1s" ~optimized:"animation:1s"
    "animation: 1s none";
  check_declaration ~expected:"animation:slide" ~optimized:"animation:slide"
    "animation: none slide";
  check_declaration ~expected:"animation:none" ~optimized:"animation:none"
    "animation: none"

let transforms () =
  (* Transform functions *)
  check_declaration ~expected:"transform:none" "transform: none";
  check_declaration ~expected:"transform:translateX(10px)"
    "transform: translateX(10px)";
  check_declaration ~expected:"transform:translateY(20px)"
    "transform: translateY(20px)";
  check_declaration ~expected:"transform:translate(10px,20px)"
    "transform: translate(10px, 20px)";
  check_declaration ~expected:"transform:scale(2)" "transform: scale(2)";
  check_declaration ~expected:"transform:scale(1.5,2)"
    "transform: scale(1.5, 2)";
  check_declaration ~expected:"transform:rotate(45deg)"
    "transform: rotate(45deg)";
  check_declaration ~expected:"transform:skewX(30deg)" "transform: skewX(30deg)";
  check_declaration ~expected:"transform:skewY(15deg)" "transform: skewY(15deg)";
  check_declaration ~expected:"transform:matrix(1,0,0,1,0,0)"
    "transform: matrix(1, 0, 0, 1, 0, 0)";

  (* Multiple transforms. Per CSS Transforms 1 section 8 the printer drops
     whitespace between back-to-back transform functions under minify, matching
     Lightning CSS. *)
  check_declaration ~expected:"transform:translateX(10px)rotate(45deg)"
    "transform: translateX(10px) rotate(45deg)";
  check_declaration ~expected:"transform:scale(2)translateY(20px)rotate(180deg)"
    "transform: scale(2) translateY(20px) rotate(180deg)";

  (* Transform origin *)
  (* Per CSS Transforms 1 sec. 4 [center] is shorthand for [50% 50%] and the
     keyword pair [top left] is [0 0]. A single [0] would mean [0 50%], so the
     two-value form must be preserved. *)
  check_declaration ~expected:"transform-origin:center"
    ~optimized:"transform-origin:50%" "transform-origin: center";
  check_declaration ~expected:"transform-origin:top left"
    ~optimized:"transform-origin:0 0" "transform-origin: top left";
  check_declaration ~expected:"transform-origin:50% 50%"
    ~optimized:"transform-origin:50%" "transform-origin: 50% 50%";
  check_declaration ~expected:"transform-origin:var(--o,center)"
    ~optimized:"transform-origin:var(--o,50%)"
    "transform-origin:var(--o,center)";
  check_declaration ~expected:"transform-origin:10px 20px"
    "transform-origin: 10px 20px"

let grid () =
  (* Grid template columns/rows *)
  check_declaration ~expected:"grid-template-columns:none"
    "grid-template-columns: none";
  check_declaration ~expected:"grid-template-columns:100px 200px"
    "grid-template-columns: 100px 200px";
  check_declaration ~expected:"grid-template-columns:1fr 2fr"
    "grid-template-columns: 1fr 2fr";
  check_declaration ~expected:"grid-template-columns:repeat(3,1fr)"
    "grid-template-columns: repeat(3, 1fr)";
  (* a var() in the repeat count is a pending-substitution count, kept verbatim
     rather than dropped *)
  check_declaration ~expected:"grid-template-columns:repeat(var(--n),1fr)"
    "grid-template-columns: repeat(var(--n), 1fr)";
  check_declaration ~expected:"grid-template-columns:repeat(var(--n,3),1fr)"
    "grid-template-columns: repeat(var(--n, 3), 1fr)";

  (* minmax with fr units *)
  check_declaration ~expected:"grid-template-columns:minmax(100px,1fr)200px"
    "grid-template-columns: minmax(100px, 1fr) 200px";
  check_declaration ~expected:"grid-template-rows:none"
    "grid-template-rows: none";
  check_declaration ~expected:"grid-template-rows:100px auto"
    "grid-template-rows: 100px auto";

  check_declaration ~expected:"grid-template-rows:repeat(2,minmax(0,1fr))"
    "grid-template-rows: repeat(2, minmax(0, 1fr))";
  (* CSS Grid 2 (ED) sec. 7.2 gives the longhands a track-list grammar. The
     slash and string-area forms belong only to grid-template in sec. 7.4. *)
  neg_cursor read_declaration "grid-template-columns: 1px / 2px";
  neg_cursor read_declaration "grid-template-rows: 1px / 2px";
  neg_cursor read_declaration "grid-template-columns: \"a\" 1px";
  neg_cursor read_declaration "grid-template-rows: \"a\" 1px";

  (* Grid areas *)
  check_declaration
    ~expected:"grid-template-areas:\"header header\"\"sidebar main\""
    "grid-template-areas: \"header header\" \"sidebar main\"";
  check_declaration ~expected:"grid-template-areas:\"nav main\"\". foot\""
    "grid-template-areas: \"nav  main\" \".    foot\"";
  check_declaration ~expected:"grid-template-areas:\". .\""
    "grid-template-areas: \".  .\"";
  check_declaration ~expected:"grid-area:header" "grid-area: header";

  (* Grid lines *)
  check_declaration ~expected:"grid-row-start:1" "grid-row-start: 1";
  check_declaration ~expected:"grid-row-start:span 2" "grid-row-start: span 2";
  check_declaration ~expected:"grid-row-end:3" "grid-row-end: 3";
  check_declaration ~expected:"grid-column-start:1" "grid-column-start: 1";
  check_declaration ~expected:"grid-column-end:-1" "grid-column-end: -1";
  (* A grid-line calc carrying a var is kept as a typed calc and round-trips; an
     all-numeric grid-line calc folds to an integer. *)
  check_declaration ~expected:"grid-column-start:calc(var(--n)*-1)"
    "grid-column-start: calc(var(--n) * -1)";
  check_declaration ~expected:"grid-column-end:6" "grid-column-end: calc(3 * 2)";

  (* Grid auto flow *)
  check_declaration ~expected:"grid-auto-flow:row" "grid-auto-flow: row";
  check_declaration ~expected:"grid-auto-flow:column" "grid-auto-flow: column";
  check_declaration ~expected:"grid-auto-flow:row dense"
    "grid-auto-flow: row dense";
  check_declaration ~expected:"grid-auto-flow:column dense"
    "grid-auto-flow: column dense";
  (* CSS Grid 2 sec. 7.7 gives [ row | column ] || dense with [row] as the
     omitted axis, so [row dense] and [dense] are one value and the optimizer
     picks the shorter. The axis is load-bearing on [column dense]. *)
  decl_optimizes_to ~held:"grid-auto-flow:row dense"
    ~into:"grid-auto-flow:dense" "grid-auto-flow: row dense";
  decl_optimizes_to ~held:"grid-auto-flow:row dense"
    ~into:"grid-auto-flow:dense" "grid-auto-flow: dense row";
  decl_optimizes ~prop:"grid-auto-flow" ~into:"column dense" "column dense";

  (* CSS Grid 2 (ED) sec. 7.6 gives grid-auto-columns and grid-auto-rows
     [<track-size>+], so they route to a reader with no [<line-names>] position
     and none of the sec. 7.2 [<track-list>] forms. *)
  check_declaration ~expected:"grid-auto-rows:1px" "grid-auto-rows: 1px";
  check_declaration ~expected:"grid-auto-rows:minmax(1px,2px)"
    "grid-auto-rows: minmax(1px, 2px)";
  check_declaration ~expected:"grid-auto-columns:1fr 2fr"
    "grid-auto-columns: 1fr 2fr";
  check_declaration ~expected:"grid-auto-rows:fit-content(10px)"
    "grid-auto-rows: fit-content(10px)";
  check_declaration ~expected:"grid-auto-rows:auto" "grid-auto-rows: auto";
  check_declaration ~expected:"grid-auto-rows:initial" "grid-auto-rows: initial";
  neg_cursor read_declaration "grid-auto-rows: [a] 1px";
  neg_cursor read_declaration "grid-auto-rows: [a]";
  neg_cursor read_declaration "grid-auto-rows: 1px [a]";
  neg_cursor read_declaration "grid-auto-columns: [a] 1px";
  neg_cursor read_declaration "grid-auto-rows: none";
  neg_cursor read_declaration "grid-auto-rows: subgrid";
  neg_cursor read_declaration "grid-auto-rows: repeat(2, 1px)";
  neg_cursor read_declaration "grid-auto-rows: 1px / 2px";

  (* sec. 7.2 keeps every [<line-names>] next to a track size, in the shorthands
     as well as in grid-template-columns / grid-template-rows. *)
  check_declaration ~expected:"grid-template-columns:[a]1px"
    "grid-template-columns: [a] 1px";
  check_declaration ~expected:"grid-template-columns:1px[a]2px"
    "grid-template-columns: 1px [a] 2px";
  (* CSS Grid 2 sec. 7.2 gives [subgrid] its own [<line-name-list>] grammar.
     Unlike an ordinary track list, that list may contain adjacent line-name
     blocks or a name-only [repeat()]. *)
  check_declaration ~expected:"grid-template-columns:subgrid[a]"
    "grid-template-columns: subgrid [a]";
  check_declaration ~expected:"grid-template-rows:subgrid[a][b]"
    "grid-template-rows: subgrid [a] [b]";
  check_declaration ~expected:"grid-template-columns:subgrid repeat(2,[a])"
    "grid-template-columns: subgrid repeat(2, [a])";
  neg_cursor read_declaration "grid-template-columns: [a]";
  neg_cursor read_declaration "grid-template-rows: [a] [b] 1px";
  neg_cursor read_declaration "grid-template: 1px / [a]";
  neg_cursor read_declaration "grid: auto-flow [a] / 1px";

  (* CSS Grid 2 sec. 7.4 builds the shorthand from [<'grid-template-rows'> /
     <'grid-template-columns'>], and sec. 7.2 gives each of those a [none] arm,
     so [none] is a track list like any other on either side of the slash. *)
  check_declaration ~expected:"grid-template:none/1fr"
    "grid-template: none / 1fr";
  check_declaration ~expected:"grid-template:1fr/none"
    "grid-template: 1fr / none";
  check_declaration ~expected:"grid-template:none/none"
    "grid-template: none / none";
  check_declaration ~expected:"grid-template:none/auto"
    "grid-template: none / auto";
  check_declaration ~expected:"grid:none/200px" "grid: none / 200px";
  check_declaration ~expected:"grid:200px/none" "grid: 200px / none";
  check_declaration ~expected:"grid:none/auto" "grid: none / auto";
  (* sec. 7.8: [auto-flow] with no [<grid-auto-rows>] leaves the row track list
     empty, which is the same declaration as a [none] rows arm. *)
  check_declaration ~expected:"grid:none/200px" "grid: auto-flow / 200px";

  (* CSS Cascade 5 (ED) sec. 7.3: explicit defaulting takes the whole
     declaration, so a CSS-wide keyword is a placement value on its own and
     never one slot of one. *)
  check_declaration ~expected:"grid-column:initial" "grid-column: initial";
  check_declaration ~expected:"grid-area:initial" "grid-area: initial";
  neg_cursor read_declaration "grid-column: 2 / initial";
  neg_cursor read_declaration "grid-row: 2 / initial";
  neg_cursor read_declaration "grid-area: 1 / 2 / 3 / initial";
  neg_cursor read_declaration "grid-column: initial / 2";
  neg_cursor read_declaration "grid-column-start: 2 initial";

  (* Grid gaps *)
  check_declaration ~expected:"gap:10px" "gap: 10px";
  check_declaration ~expected:"gap:10px 20px" "gap: 10px 20px";
  check_declaration ~expected:"column-gap:10px" "column-gap: 10px";
  check_declaration ~expected:"row-gap:20px" "row-gap: 20px"

let misc () =
  (* Opacity *)
  check_declaration ~expected:"opacity:0" "opacity: 0";
  check_declaration ~expected:"opacity:.5" "opacity: 0.5";
  check_declaration ~expected:"opacity:1" "opacity: 1";

  (* Z-index *)
  check_declaration ~expected:"z-index:auto" "z-index: auto";
  check_declaration ~expected:"z-index:0" "z-index: 0";
  check_declaration ~expected:"z-index:10" "z-index: 10";
  check_declaration ~expected:"z-index:-1" "z-index: -1";
  check_declaration ~expected:"z-index:9999" "z-index: 9999";
  (* A calc that cannot const-fold (carries a var) is kept as a typed calc and
     round-trips; an all-numeric calc still folds to an integer. *)
  check_declaration ~expected:"z-index:calc(var(--z)*-1)"
    "z-index: calc(var(--z) * -1)";
  check_declaration ~expected:"z-index:10" "z-index: calc(5 * 2)";

  (* Cursor *)
  check_declaration ~expected:"cursor:auto" "cursor: auto";
  check_declaration ~expected:"cursor:default" "cursor: default";
  check_declaration ~expected:"cursor:pointer" "cursor: pointer";
  check_declaration ~expected:"cursor:move" "cursor: move";
  check_declaration ~expected:"cursor:text" "cursor: text";
  check_declaration ~expected:"cursor:wait" "cursor: wait";
  check_declaration ~expected:"cursor:help" "cursor: help";
  check_declaration ~expected:"cursor:crosshair" "cursor: crosshair";
  check_declaration ~expected:"cursor:not-allowed" "cursor: not-allowed";
  check_declaration ~expected:"cursor:none" "cursor: none";

  (* Visibility *)
  check_declaration ~expected:"visibility:visible" "visibility: visible";
  check_declaration ~expected:"visibility:hidden" "visibility: hidden";
  check_declaration ~expected:"visibility:collapse" "visibility: collapse";

  (* Box sizing *)
  check_declaration ~expected:"box-sizing:content-box" "box-sizing: content-box";
  check_declaration ~expected:"box-sizing:border-box" "box-sizing: border-box";
  (* Vendor-prefixed box-sizing is typed, so it round-trips and canonicalises
     its value (an [Unknown_property] would keep [BORDER-BOX] verbatim). *)
  check_declaration ~expected:"-webkit-box-sizing:border-box"
    "-webkit-box-sizing: border-box";
  check_declaration ~expected:"-moz-box-sizing:border-box"
    "-moz-box-sizing: BORDER-BOX";
  (* The vendor-prefixed transform properties are typed too. *)
  check_declaration ~expected:"-moz-transform:none" "-moz-transform: NONE";
  check_declaration ~expected:"-o-transform:rotate(45deg)"
    "-o-transform: rotate(45deg)";
  check_declaration ~expected:"-ms-user-select:none" "-ms-user-select: NONE";
  check_declaration ~expected:"-webkit-text-fill-color:#fff"
    "-webkit-text-fill-color: #ffffff";

  (* User select *)
  check_declaration ~expected:"user-select:none" "user-select: none";
  check_declaration ~expected:"user-select:auto" "user-select: auto";
  check_declaration ~expected:"user-select:text" "user-select: text";
  check_declaration ~expected:"user-select:all" "user-select: all";

  (* Pointer events *)
  check_declaration ~expected:"pointer-events:none" "pointer-events: none";
  check_declaration ~expected:"pointer-events:auto" "pointer-events: auto";

  (* Resize *)
  check_declaration ~expected:"resize:none" "resize: none";
  check_declaration ~expected:"resize:both" "resize: both";
  check_declaration ~expected:"resize:horizontal" "resize: horizontal";
  check_declaration ~expected:"resize:vertical" "resize: vertical"

let list_properties () =
  (* Box shadow *)
  check_declaration ~expected:"box-shadow:none" "box-shadow: none";
  (* CSS Backgrounds 3 sec. 6.1: <shadow> is [<color>? && [<length>{2,4}] &&
     inset?], so the two offsets alone are a whole shadow whatever they hold,
     and one length is not. *)
  check_declaration ~expected:"box-shadow:0 0" "box-shadow: 0 0";
  check_declaration ~expected:"box-shadow:inset 0 0" "box-shadow: inset 0 0";
  check_declaration ~expected:"text-shadow:0 0" "text-shadow: 0 0";
  neg_cursor read_declaration "box-shadow: 0";
  neg_cursor read_declaration "text-shadow: 1px";
  neg_cursor read_declaration "box-shadow: inset inset 0 0 1px";

  (* CSS Transitions 1 sec. 3: the shorthand takes a
     <single-transition-property>, and none is one of them, so it is the whole
     value only when the item ends there. The optimizer contracts a none
     property beside a duration and a timing function into this spelling. *)
  check_declaration ~expected:"transition:none" "transition: none";
  check_declaration ~expected:"transition:none 1s" "transition: none 1s";
  check_declaration ~expected:"transition:none 1s steps(2,end)"
    "transition: none 1s steps(2, end)";
  check_declaration ~expected:"transition:none,opacity 1s"
    "transition: none, opacity 1s";

  (* CSS Text Decoration 4 sec. 2.5: the shorthand is a || of its longhands and
     [none] is a <text-decoration-line>, so it is the whole value only when
     nothing follows it. The optimizer contracts a none line beside a style and
     a colour into exactly this spelling. *)
  check_declaration ~expected:"text-decoration:none" "text-decoration: none";
  check_declaration ~expected:"text-decoration:none dotted"
    "text-decoration: none dotted";
  check_declaration ~expected:"text-decoration:none dotted #12345680"
    "text-decoration: none dotted #12345680";
  check_declaration ~expected:"text-decoration:underline dotted red"
    "text-decoration: underline dotted red";

  (* CSS Align 3 sec. 5.2: [place-items] is [<'align-items'>
     <'justify-items'>?], so it takes every <self-position> keyword align-items
     does, and a lone value sets both axes. The optimizer contracts
     [align-items: flex-start; justify-items: baseline] into this spelling. *)
  check_declaration ~expected:"place-items:flex-start baseline"
    "place-items: flex-start baseline";
  check_declaration ~expected:"place-items:self-start end"
    "place-items: self-start end";
  check_declaration ~expected:"place-items:start baseline"
    "place-items: start baseline";

  (* CSS Color 5 sec. 5.1: [color(from <origin> srgb r g b)] is the origin's own
     sRGB channels, so it folds to the origin whether that was written as a hex
     or as a [color()] the same conversion reaches. *)
  check_declaration ~expected:"color:rgb(179 128 77)"
    "color: color(from #b3804d srgb r g b)";
  check_declaration ~expected:"color:rgb(179 128 77)"
    "color: color(from color(srgb .7 .5 .3) srgb r g b)";
  check_declaration ~expected:"color:rgb(179 128 77/.4)"
    "color: color(from color(srgb .7 .5 .3/40%) srgb r g b)";
  check_declaration ~expected:"color:rgb(179 128 77)"
    "color: color(from color(from color(srgb .7 .5 .3) srgb r g b) srgb r g b)";

  (* CSS Color 4 sec. 9.4: an out-of-range Oklab lightness clamps rather than
     invalidating the colour, and 0% to 100% is the same 0 to 1 a bare number
     names, so both spellings clamp alike. *)
  (* pp holds the percentage spelling; each pair asserts that the out-of-range
     value reads as the in-range one beside it. *)
  check_declaration ~expected:"color:oklab(100%0 0)" "color: oklab(100% 0 0)";
  check_declaration ~expected:"color:oklab(100%0 0)" "color: oklab(200% 0 0)";
  check_declaration ~expected:"color:oklab(0%0 0)" "color: oklab(0% 0 0)";
  check_declaration ~expected:"color:oklab(0%0 0)" "color: oklab(-200% 0 0)";
  check_declaration ~expected:"color:oklch(100%0 0)" "color: oklch(100% 0 0)";
  check_declaration ~expected:"color:oklch(100%0 0)" "color: oklch(200% 0 0)";
  check_declaration ~expected:"color:oklch(0%0 0)" "color: oklch(0% 0 0)";
  check_declaration ~expected:"color:oklch(0%0 0)" "color: oklch(-200% 0 0)";

  (* [center] minifies to the same pair of percentages a written-out position
     does, and a percentage delimits itself, so neither spelling keeps the space
     between them. *)
  check_declaration
    ~expected:
      "background:-webkit-gradient(radial,50%50%,0,50%50%,100,from(blue),to(yellow))"
    "background: -webkit-gradient(radial, center center, 0, center center, \
     100, from(blue), to(yellow))";
  check_declaration
    ~expected:
      "background:-webkit-gradient(radial,50%50%,0,50%50%,100,from(#00f),to(#ff0))"
    "background: -webkit-gradient(radial, 50% 50%, 0, 50% 50%, 100, \
     from(#00f), to(#ff0))";
  (* A [-webkit-gradient] stop position is a number or a percentage, and the
     minified spelling is the number, so both read back. *)
  check_declaration
    ~expected:
      "background:-webkit-gradient(linear,0 0,0 \
       100%,from(#00f),color-stop(.5,red),to(#ff0))"
    "background: -webkit-gradient(linear, 0 0, 0 100%, from(#00f), \
     color-stop(.5, red), to(#ff0))";
  check_declaration
    ~expected:
      "background:-webkit-gradient(linear,0 0,0 \
       100%,from(#00f),color-stop(.5,red),to(#ff0))"
    "background: -webkit-gradient(linear, 0 0, 0 100%, from(#00f), \
     color-stop(50%, red), to(#ff0))";

  (* CSS Transforms 2 sec. 5: minified [rotate] leads with the angle, and a
     following axis number that starts with a sign needs no separator. The first
     one does: the angle ends in its unit, so [0deg-1] is the one dimension
     [0deg-1]. *)
  check_declaration ~expected:"rotate:0deg -1 0 0" "rotate: -1 0 0 0deg";
  check_declaration ~expected:"rotate:45deg 1 0-1" "rotate: 1 0 -1 45deg";
  check_declaration ~expected:"rotate:x 45deg" "rotate: 1 0 0 45deg";

  (* CSS Syntax 3 sec. 4.3.6 ends a url token at EOF as it does at [)]: the
     missing closer is a parse error and the token still reads. *)
  check_declaration ~expected:"background-image:url()" "background-image:url(";
  check_declaration ~expected:"background-image:url(foo)"
    "background-image:url(foo";
  check_declaration ~expected:"background-image:url()" "background-image:url()";
  (* CSS Backgrounds 3 sec. 6.1 and CSS Text Decoration 3 sec. 5 both build a
     shadow with &&, so the length run is contiguous and neither the colour nor
     inset may sit inside it. text-shadow takes three lengths, not four, and one
     colour. *)
  check_declaration ~expected:"text-shadow:1px 1px red"
    "text-shadow: red 1px 1px";
  check_declaration ~expected:"text-shadow:1px 1px 2px"
    "text-shadow: 1px 1px 2px";
  neg_cursor read_declaration "text-shadow: 1px red 1px";
  neg_cursor read_declaration "text-shadow: 1px 1px 1px 1px";
  neg_cursor read_declaration "text-shadow: 1px 1px red 1px";
  neg_cursor read_declaration "text-shadow: red 1px 1px blue";
  check_declaration ~expected:"box-shadow:inset 1px 1px"
    "box-shadow: 1px 1px inset";
  neg_cursor read_declaration "box-shadow: 1px inset 1px";
  neg_cursor read_declaration "box-shadow: 1px red 1px";
  check_declaration ~expected:"box-shadow:0 1px 3px rgb(0 0 0/.12)"
    ~optimized:"box-shadow:0 1px 3px #0000001f"
    "box-shadow: 0 1px 3px rgba(0,0,0,0.12)";
  (* Faithful minify keeps an author-explicit zero spread (pp serializes the
     [Some Zero] node); optimize drops it as redundant since spread defaults to
     0, and also folds the colour to hex. *)
  check_declaration ~expected:"box-shadow:0 1px 3px 0 rgb(0 0 0/10%)"
    ~optimized:"box-shadow:0 1px 3px #0000001a"
    "box-shadow: 0 1px 3px 0 rgb(0 0 0 / 10%)";
  check_declaration ~expected:"box-shadow:0 1px 3px 0"
    ~optimized:"box-shadow:0 1px 3px" "box-shadow: 0 1px 3px 0";
  (* A zero blur is positional - it cannot be dropped while a non-zero spread
     follows, or the spread would rebind as the blur. Held and canonical
     match. *)
  check_declaration ~expected:"box-shadow:0 1px 0 5px" "box-shadow: 0 1px 0 5px";
  check_declaration
    ~expected:"box-shadow:0 1px 3px rgb(0 0 0/.12),0 1px 2px rgb(0 0 0/.24)"
    ~optimized:"box-shadow:0 1px 3px #0000001f,0 1px 2px #0000003d"
    "box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)";
  check_declaration ~expected:"box-shadow:inset 0 2px 4px rgb(0 0 0/.06)"
    ~optimized:"box-shadow:inset 0 2px 4px #0000000f"
    "box-shadow: inset 0 2px 4px rgba(0,0,0,0.06)";

  (* Text shadow *)
  check_declaration ~expected:"text-shadow:none" "text-shadow: none";
  check_declaration ~expected:"text-shadow:1px 1px 2px black"
    ~optimized:"text-shadow:1px 1px 2px #000" "text-shadow: 1px 1px 2px black";
  check_declaration ~expected:"text-shadow:0 0 10px blue,0 0 20px red"
    ~optimized:"text-shadow:0 0 10px #00f,0 0 20px red"
    "text-shadow: 0 0 10px blue, 0 0 20px red";
  (* CSS Text Decoration 4 (ED) sec. 4 reads a text shadow as a [<shadow>] "as
     for box-shadow", and CSS Backgrounds 3 (ED) sec. 6.1 says "Omitted lengths
     are 0". The blur is the last length here, so a zero blur is the spelled-out
     form of leaving it off, and dropping it is a node change. *)
  check_declaration ~expected:"text-shadow:1px 1px 0"
    ~optimized:"text-shadow:1px 1px" "text-shadow: 1px 1px 0";
  check_declaration ~expected:"text-shadow:1px 1px 0 red"
    ~optimized:"text-shadow:1px 1px red" "text-shadow: 1px 1px 0 red";
  check_declaration ~expected:"text-shadow:1px 1px 2px"
    ~optimized:"text-shadow:1px 1px 2px" "text-shadow: 1px 1px 2px";

  (* Background image *)
  check_declaration ~expected:"background-image:none" "background-image: none";
  check_declaration ~expected:"background-image:url(image.png)"
    "background-image: url(image.png)";
  check_declaration
    ~expected:"background-image:linear-gradient(to right,red,blue)"
    "background-image: linear-gradient(to right, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right, red, blue)";
  check_declaration ~expected:"background-image:url(a.png),url(b.png)"
    "background-image: url(a.png), url(b.png)";

  (* Transition *)
  check_declaration ~expected:"transition:none" "transition: none";
  (* CSS Transitions 1 (ED) sec. 2.5 builds a [<single-transition>] out of
     components that each fall back to their longhand initial when left out:
     [0s] for the duration (sec. 2.2), [ease] for the easing (sec. 2.3), [0s]
     for the delay (sec. 2.4). Spelling an initial out therefore names the value
     the shorter form already names, and swapping the two is a node change, so
     pp prints what it parsed and the optimizer folds. *)
  check_declaration ~expected:"transition:all .3s ease"
    ~optimized:"transition:all .3s" "transition: all 0.3s ease";
  check_declaration ~expected:"transition:all 1s ease 0s"
    ~optimized:"transition:all 1s" "transition: all 1s ease 0s";
  check_declaration ~expected:"transition:opacity 1s normal"
    ~optimized:"transition:opacity 1s" "transition: opacity 1s normal";
  check_declaration ~expected:"transition:opacity 0s 0s"
    ~optimized:"transition:opacity" "transition: opacity 0s 0s";
  (* sec. 2.5: "the first value that can be parsed as a time is assigned to the
     transition-duration, and the second value that can be parsed as a time is
     assigned to transition-delay". A delay is only reachable behind a duration,
     so a zero duration that carries a non-zero delay has to stay written out:
     dropping it hands the delay to the duration slot and starts the transition
     immediately instead of two seconds late. *)
  check_declaration ~expected:"transition:opacity 0s 2s"
    ~optimized:"transition:opacity 0s 2s" "transition: opacity 0s 2s";
  check_declaration ~expected:"transition:opacity 0s linear 2s"
    ~optimized:"transition:opacity 0s linear 2s"
    "transition: opacity 0s linear 2s";
  check_declaration ~expected:"transition:all .3s linear"
    "transition: all .3s linear";
  check_declaration ~expected:"transition:opacity 1s ease-in .5s"
    "transition: opacity 1s ease-in .5s";
  check_declaration ~expected:"transition:opacity .3s,transform .3s"
    "transition: opacity 0.3s, transform 0.3s";
  check_declaration ~expected:"transition:all .5s"
    ~optimized:"transition:all .5s" "transition: all 500ms";

  (* Animation *)
  check_declaration ~expected:"animation:none" "animation: none";
  check_declaration ~expected:"animation:spin 1s linear infinite"
    "animation: spin 1s linear infinite";
  check_declaration ~expected:"animation:slide .5s ease-out"
    "animation: slide 0.5s ease-out";
  check_declaration ~expected:"animation:spin .5s"
    ~optimized:"animation:spin .5s" "animation: spin 500ms"

let animation_infinite_name () =
  (* CSS Animations 1 section 3.10 assigns a keyword to another property only
     when that property has not already been set. Section 3 makes keyframe names
     case-sensitive, even when their spelling is also a keyword. *)
  let open Css.Properties in
  let check_slots input expected_name expected_count =
    let cursor = Cursor.of_string input in
    let value = read_animation cursor in
    Alcotest.(check bool) "consumes animation" true (Cursor.is_done cursor);
    (match value with
    | Shorthand { name = Some (Ambiguous name | Name name); iteration_count; _ }
      ->
        Alcotest.(check string) "animation name" expected_name name;
        let shown c =
          Css.Pp.to_string ~minify:true pp_animation_iteration_count c
        in
        Alcotest.(check (option string))
          "iteration count"
          (Some (shown expected_count))
          (Option.map shown iteration_count)
    | _ -> Alcotest.failf "missing animation name in %s" input);
    value
  in
  List.iter
    (fun (input, name, count) ->
      let value = check_slots input name count in
      List.iter
        (fun minify ->
          let printed = Css.Pp.to_string ~minify pp_animation value in
          ignore (check_slots printed name count))
        [ false; true ])
    [
      ("infinite infinite", "infinite", Infinite);
      ("infinite INFINITE", "INFINITE", Infinite);
      ("INFINITE infinite", "infinite", Infinite);
      ("2 infinite", "infinite", Num 2.);
      ("2 InFiNiTe", "InFiNiTe", Num 2.);
    ];
  List.iter
    (fun value -> none_cursor read_declaration ("animation:" ^ value))
    [
      "2 3";
      "infinite 2";
      "infinite infinite infinite";
      "spin infinite infinite";
    ]

let animation_keyword_names () =
  (* CSS Animations 1 sections 3 and 3.10: names are case-sensitive and
     colliding keywords fill the other shorthand slots before the name. *)
  let open Css.Properties in
  let read input =
    let cursor = Cursor.of_string input in
    match read_animation cursor with
    | Shorthand value when Cursor.is_done cursor -> value
    | _ -> Alcotest.failf "expected one animation: %s" input
  in
  let check_slots expected name input =
    let actual = read input in
    (match actual.name with
    | Some (Name value | Ambiguous value) ->
        Alcotest.(check string) input name value
    | _ -> Alcotest.failf "missing name in %s" input);
    (* The slots read the same when the value they print does, and the printed
       form says which slot differs when they do not. *)
    let without_name v =
      Css.Pp.to_string ~minify:true pp_animation
        (Shorthand { v with name = Option.None })
    in
    Alcotest.(check string)
      (input ^ " keeps non-name slots")
      (without_name expected) (without_name actual)
  in
  let check_print expected name value =
    List.iter
      (fun minify ->
        check_slots expected name (Css.Pp.to_string ~minify pp_animation value))
      [ false; true ]
  in
  List.iter
    (fun (prefix, names) ->
      let expected = read (prefix ^ " keyframe") in
      List.iter
        (fun name ->
          List.iter
            (fun name ->
              let input = prefix ^ " " ^ name in
              check_slots expected name input;
              check_print expected name (Shorthand (read input));
              check_print expected name
                (animation_shorthand ~name ?duration:expected.duration
                   ?timing_function:expected.timing_function
                   ?delay:expected.delay
                   ?iteration_count:expected.iteration_count
                   ?direction:expected.direction ?fill_mode:expected.fill_mode
                   ?play_state:expected.play_state ?timeline:expected.timeline
                   ()))
            [ name; String.uppercase_ascii name ])
        names)
    [
      ( "linear",
        [
          "ease";
          "linear";
          "ease-in";
          "ease-out";
          "ease-in-out";
          "step-start";
          "step-end";
        ] );
      ("reverse", [ "normal"; "reverse"; "alternate"; "alternate-reverse" ]);
      ("forwards", [ "forwards"; "backwards"; "both" ]);
      ("paused", [ "running"; "paused" ]);
      ("2", [ "infinite" ]);
    ];
  let expected = read "keyframe" in
  List.iter
    (fun name -> check_print expected name (animation_shorthand ~name ()))
    [ "EASE"; "LINEAR"; "NORMAL"; "BOTH"; "PAUSED"; "INFINITE" ]

let list_style_custom_names () =
  (* CSS Lists 3 sections 3.4 and 3.6 allow custom counter-style names,
     including names that collide with an already-filled position slot. The
     referenced counter style need not be defined in this stylesheet. *)
  List.iter
    (fun name -> check_declaration ~roundtrip:true ("list-style-type:" ^ name))
    [ "footsteps"; "FootSteps"; "inside"; "OUTSIDE"; "--markers" ];
  List.iter
    (fun (value, expected) ->
      check_declaration ~roundtrip:true ~expected:("list-style:" ^ expected)
        ~optimized:("list-style:" ^ expected) ("list-style:" ^ value))
    [
      ("FootSteps inside", "FootSteps inside");
      ("inside outside", "inside outside");
      ("inside OUTSIDE", "inside OUTSIDE");
      ("outside inside", "outside inside");
      ("outside OUTSIDE", "outside OUTSIDE");
      ("inside inside", "inside inside");
    ];
  List.iter
    (none_cursor read_declaration)
    [
      "list-style-type:default";
      "list-style-type:FootSteps Other";
      "list-style:inside outside outside";
      "list-style:inside FootSteps Other";
    ]

let auto_is_not_a_color () =
  (* CSS Color 4 section 4.1 excludes auto from <color>. SVG 2 section 13.2 uses
     <color> both as paint and as a paint-server fallback. *)
  List.iter
    (fun property ->
      List.iter
        (fun value -> none_cursor read_declaration (property ^ ":" ^ value))
        [
          "auto";
          "AUTO";
          "color-mix(in srgb,auto,red)";
          "light-dark(auto,red)";
          "rgb(from auto r g b)";
        ])
    [
      "color";
      "background-color";
      "border-color";
      "text-decoration-color";
      "fill";
      "stroke";
      "stop-color";
    ];
  List.iter
    (fun property ->
      none_cursor read_declaration (property ^ ":url(#paint) auto");
      check_declaration ~roundtrip:true (property ^ ":none");
      check_declaration ~roundtrip:true (property ^ ":context-fill");
      check_declaration ~roundtrip:true (property ^ ":url(#paint)red"))
    [ "fill"; "stroke" ];
  (* CSS UI 4 gives these properties an explicit auto alternative. *)
  List.iter
    (fun property ->
      check_declaration ~roundtrip:true (property ^ ":auto");
      check_declaration ~roundtrip:true (property ^ ":red");
      check_declaration ~roundtrip:true (property ^ ":var(--color,auto)"))
    [ "accent-color"; "caret-color"; "outline-color" ];
  (* A var() fallback is a token stream; validity waits for substitution. *)
  check_declaration ~roundtrip:true "color:var(--color,auto)";
  check_declaration ~roundtrip:true "caret:auto";
  check_declaration ~roundtrip:true "scrollbar-color:auto"

let outline_offset_length_only () =
  (* CSS UI 4 section 3.5 takes <length>, with no percentage basis, and
     explicitly permits negative offsets. Section 2.1 admits CSS-wide values. *)
  List.iter
    (fun value -> check_declaration ~roundtrip:true ("outline-offset:" ^ value))
    [
      "0";
      "2px";
      "-2px";
      "-1em";
      "calc(1em - 2px)";
      "min(-1em,2px)";
      "inherit";
      "initial";
      "unset";
      "revert";
      "revert-layer";
      "var(--offset,10%)";
    ];
  List.iter
    (fun value -> none_cursor read_declaration ("outline-offset:" ^ value))
    [
      "10%";
      "0%";
      "-10%";
      "calc(1% + 2px)";
      "min(1%,2px)";
      "max(1px,2%)";
      "clamp(0px,10%,2px)";
      "auto";
      "min-content";
      "fit-content(2px)";
      "1s";
      "45deg";
    ]

let line_height_step_length_only () =
  (* CSS Rhythmic Sizing 1 section 3 takes <length [0,infinity]> with no
     percentage basis; section 1.2 also admits CSS-wide keywords. *)
  List.iter
    (fun value ->
      check_declaration ~roundtrip:true ("line-height-step:" ^ value))
    [
      "0";
      "4px";
      "1em";
      "calc(1em - 2px)";
      "min(1em,2px)";
      "inherit";
      "initial";
      "unset";
      "revert";
      "revert-layer";
      "var(--step,10%)";
    ];
  List.iter
    (fun value -> none_cursor read_declaration ("line-height-step:" ^ value))
    [
      "10%";
      "0%";
      "calc(1% + 2px)";
      "min(1%,2px)";
      "max(1px,2%)";
      "clamp(0px,10%,2px)";
      "-4px";
      "auto";
      "normal";
      "none";
      "min-content";
      "fit-content(2px)";
      "1s";
      "45deg";
    ]

let gap_normal () =
  (* CSS Box Alignment 3 sections 8.1 and 8.2 admit normal in both gap longhands
     and either shorthand slot. Its used value depends on layout, so normal
     cannot be replaced with zero without a layout context. *)
  List.iter
    (fun property ->
      List.iter
        (fun value ->
          let css = property ^ ":" ^ value in
          check_declaration ~roundtrip:true ~optimized:css css)
        [ "normal"; "10%"; "2px" ];
      check_declaration ~roundtrip:true (property ^ ":var(--gap,normal)");
      check_declaration ~roundtrip:true ~expected:(property ^ ":normal")
        (property ^ ":NORMAL");
      List.iter
        (fun value -> none_cursor read_declaration (property ^ ":" ^ value))
        [ "normal normal"; "normal 2px"; "auto"; "-2px" ])
    [ "row-gap"; "column-gap" ];
  List.iter
    (fun value ->
      let css = "gap:" ^ value in
      check_declaration ~roundtrip:true ~optimized:css css)
    [ "normal"; "normal 2px"; "2px normal"; "normal 10%" ];
  check_declaration ~roundtrip:true ~expected:"gap:normal"
    ~optimized:"gap:normal" "gap:normal normal";
  List.iter
    (fun value -> none_cursor read_declaration ("gap:" ^ value))
    [ "normal inherit"; "inherit normal"; "normal normal normal" ]

let sizing_keyword_domains () =
  (* CSS Sizing 3 sections 3.1.1-3.1.3 distinguish auto sizes from none
     maximums. CSS Logical Properties 1 section 4.1 uses the same grammars for
     the corresponding flow-relative dimensions. *)
  List.iter
    (fun dimension ->
      List.iter
        (fun (prefix, valid, invalid) ->
          let property = prefix ^ dimension in
          List.iter
            (fun value ->
              check_declaration ~roundtrip:true (property ^ ":" ^ value))
            [
              valid;
              "min-content";
              "max-content";
              "fit-content(10%)";
              "0";
              "2px";
              "10%";
              "calc(1em + 2px)";
              "inherit";
              "initial";
              "unset";
              "revert";
              "revert-layer";
              "var(--size," ^ invalid ^ ")";
            ];
          List.iter
            (fun value -> none_cursor read_declaration (property ^ ":" ^ value))
            [
              invalid;
              String.uppercase_ascii invalid;
              "normal";
              "from-font";
              "size";
            ])
        [
          ("", "auto", "none");
          ("min-", "auto", "none");
          ("max-", "none", "auto");
        ])
    [ "width"; "height"; "inline-size"; "block-size" ]

let custom_properties () =
  (* Basic custom properties *)
  check_declaration ~expected:"--color:red" "--color: red";
  check_declaration ~expected:"--my-var:10px" "--my-var: 10px";
  check_declaration ~expected:"--complex:1px solid black"
    "--complex: 1px solid black";

  (* With var() references *)
  check_declaration ~expected:"--primary:var(--base-color)"
    "--primary: var(--base-color)";
  check_declaration ~expected:"--size:calc(var(--base) * 2)"
    ~optimized:"--size:calc(var(--base)*2)" "--size: calc(var(--base) * 2)";
  check_declaration ~expected:"--aspect-video:16 / 9"
    ~optimized:"--aspect-video:16/9" "--aspect-video: 16 / 9";
  check_declaration ~expected:"--stops:var(--from) var(--from-position)"
    "--stops: var(--from) var(--from-position)";
  check_declaration ~expected:"--fallback:var(--undefined,10px)"
    "--fallback: var(--undefined, 10px)";

  (* var() with empty fallback - declaration level coverage *)
  check_declaration ~expected:"color:var(--x,)" "color: var(--x,)";
  check_declaration ~expected:"background:var(--bg,)" "background: var(--bg,)";
  (* CSS Custom Properties for Cascading Variables 1 sec. 3: [var()] takes a
     name and an optional [, <fallback>]; content after the name without a
     leading comma is not part of the grammar (CSS Syntax 3 sec. 8.2 invalidates
     the declaration). [Cursor.call] did not require [var()]'s reader to consume
     its whole sub-cursor, so [var(--x 10px)] silently dropped [ 10px] and
     answered [var(--x)]. *)
  neg_cursor read_declaration "color:var(--x 10px)";
  check_declaration ~expected:"color:var(--x)" "color: var( --x )"

(* A numeric token's source spelling is not part of its value. Opaque custom-
   and unknown-property streams therefore use the same shortest numeric spelling
   as typed values, while retaining separators when dropping a sign would
   otherwise merge two tokens. *)
let opaque_numeric_tokens () =
  check_declaration ~expected:"--t:1px" "--t: 1.0px";
  check_declaration ~expected:"--t:.5px" "--t: 0.5px";
  check_declaration ~expected:"--t:.5" "--t: .50";
  check_declaration ~expected:"--t:1px" "--t: 01px";
  check_declaration ~expected:"--t:1" "--t: +1";
  check_declaration ~expected:"-x-y:.5%" "-x-y: 0.50%";
  check_declaration ~expected:"--t:x 1" "--t: x +1";
  check_declaration ~expected:"--t:1 2px" "--t: 1 +2px";
  check_declaration ~expected:"--t:- 1" "--t: - +1"

(* CSS Values 4 (ED) sec. 10.8 "Syntax": inside a math function whitespace is
   required on both sides of the [+] and [-] operators, while [*] and [/] may be
   written without any. A custom property and an unknown property both carry an
   opaque token stream that is minified token by token, so deleting that
   whitespace hands the browser a declaration it drops on the floor. *)
let math_sign_whitespace () =
  check_declaration ~expected:"--t:calc(100% - 10px)"
    ~optimized:"--t:calc(100% - 10px)" "--t: calc(100% - 10px)";
  check_declaration ~expected:"--t:calc(100% + 10px)"
    ~optimized:"--t:calc(100% + 10px)" "--t: calc(100% + 10px)";
  check_declaration ~expected:"--t:calc((100%) - 10px)"
    ~optimized:"--t:calc((100%) - 10px)" "--t: calc((100%) - 10px)";
  check_declaration ~expected:"--t:calc(min(1px,2px) - 3px)"
    ~optimized:"--t:calc(min(1px,2px) - 3px)" "--t: calc(min(1px,2px) - 3px)";
  check_declaration ~expected:"--t:calc(50% + var(--a))"
    ~optimized:"--t:calc(50% + var(--a))" "--t: calc(50% + var(--a))";
  (* An unknown property holds the same opaque stream. *)
  check_declaration ~expected:"-x-y:calc(100% - 10px)"
    ~optimized:"-x-y:calc(100% - 10px)" "-x-y: calc(100% - 10px)";
  check_declaration ~expected:"-x-y:calc(min(1px,2px) - 3px)"
    ~optimized:"-x-y:calc(min(1px,2px) - 3px)" "-x-y: calc(min(1px,2px) - 3px)";
  (* The rule is about a [+] / [-] delim token. [calc(1 +2)] lexes as two number
     tokens with nothing between them but whitespace, so it keeps folding. *)
  check_declaration ~expected:"--t:calc(1+2)" ~optimized:"--t:calc(1+2)"
    "--t: calc(1 +2)";
  (* The reader keeps what it read: the whitespace-free spelling is a different
     (invalid) declaration, and minifying never inserts a separator. *)
  check_declaration ~expected:"--t:calc(100%- 10px)"
    ~optimized:"--t:calc(100%- 10px)" "--t: calc(100%- 10px)";
  (* Whitespace that no grammar asks for still folds. *)
  check_declaration ~expected:"--t:16 / 9" ~optimized:"--t:16/9" "--t: 16 / 9";
  check_declaration ~expected:"--t:calc(var(--base) * 2)"
    ~optimized:"--t:calc(var(--base)*2)" "--t: calc(var(--base) * 2)";
  check_declaration ~expected:"--a:cubic-bezier(.4,0,.6,1) infinite"
    ~optimized:"--a:cubic-bezier(.4,0,.6,1)infinite"
    "--a: cubic-bezier(.4,0,.6,1) infinite";
  check_declaration ~expected:"--t:100%x" ~optimized:"--t:100%x" "--t: 100% x"

(* CSS Syntax 3 (ED) sec. 4.3.9 "Would start an identifier" and sec. 4.3.10
   "Would start a number" fix what a token absorbs on re-lexing. A [+] is
   neither a name code point nor a continuation of the number in front of it, so
   a plus-signed numeric always re-lexes on its own; a [-] is a name code point,
   so it only does after a number. A [(] joins the ident before it and nothing
   else. Writing a separator into any of those pairs hands [var()] a token
   sequence the source never held, which is the one thing minification may not
   do. *)
let inserted_token_boundary () =
  (* A plus-signed numeric after a name, a number or a dimension. *)
  check_declaration ~expected:"--t:x 1px+2px" ~optimized:"--t:x 1px+2px"
    "--t: x 1px+2px";
  check_declaration ~expected:"--t:span+2" ~optimized:"--t:span+2" "--t: span+2";
  check_declaration ~expected:"--t:#abc+1px" ~optimized:"--t:#abc+1px"
    "--t: #abc+1px";
  check_declaration ~expected:"--t:@foo+2px" ~optimized:"--t:@foo+2px"
    "--t: @foo+2px";
  check_declaration ~expected:"--t:1e3+2%" ~optimized:"--t:1e3+2%" "--t: 1e3+2%";
  (* A minus-signed numeric after a number: the number cannot absorb the [-]. *)
  check_declaration ~expected:"--t:9-9px" ~optimized:"--t:9-9px" "--t: 9-9px";
  (* A parenthesised block only makes a function token of a preceding ident. *)
  check_declaration ~expected:"--t:1px(a)" ~optimized:"--t:1px(a)" "--t: 1px(a)";
  check_declaration ~expected:"--t:#abc(a)" ~optimized:"--t:#abc(a)"
    "--t: #abc(a)";
  check_declaration ~expected:"--t:@foo(a)" ~optimized:"--t:@foo(a)"
    "--t: @foo(a)";
  (* An unknown property carries the same opaque stream. *)
  check_declaration ~expected:"-x-y:1px+2px" ~optimized:"-x-y:1px+2px"
    "-x-y: 1px+2px";
  (* Controls. A [-] after a dimension is read into the unit, so the separator
     carries the boundary and stays. *)
  check_declaration ~expected:"--t:x 1px -2px" ~optimized:"--t:x 1px -2px"
    "--t: x 1px -2px";
  check_declaration ~expected:"-x-y:1px -2px" ~optimized:"-x-y:1px -2px"
    "-x-y: 1px -2px";
  (* An unsigned numeric merges into the number or the unit before it. *)
  check_declaration ~expected:"--t:1 2px" ~optimized:"--t:1 2px" "--t: 1 2px";
  check_declaration ~expected:"--t:1px solid" ~optimized:"--t:1px solid"
    "--t: 1px solid";
  check_declaration ~expected:"--t:foo bar" ~optimized:"--t:foo bar"
    "--t: foo bar";
  (* [ident(] is a function token, and a hash absorbs a following name. *)
  check_declaration ~expected:"--t:translate (1px)"
    ~optimized:"--t:translate (1px)" "--t: translate (1px)";
  check_declaration ~expected:"--t:#abc var(--x)" ~optimized:"--t:#abc var(--x)"
    "--t: #abc var(--x)";
  (* [/] then [*] would open a comment (CSS Syntax 3 (ED) sec. 4.3.2). *)
  check_declaration ~expected:"-x-y:a/ *b" ~optimized:"-x-y:a/ *b"
    "-x-y: a / *b";
  (* Two numbers already fold: the second carries its own sign. *)
  check_declaration ~expected:"-x-y:1+2" ~optimized:"-x-y:1+2" "-x-y: 1 +2"

let important () =
  (* Standard properties with !important *)
  check_declaration ~expected:"color:red!important" "color: red !important";
  check_declaration ~expected:"display:none!important"
    "display: none !important";
  check_declaration ~expected:"width:100px!important" "width: 100px !important";
  check_declaration ~expected:"margin:auto!important" "margin: auto !important";

  (* Custom properties with !important *)
  check_declaration ~expected:"--custom:value!important"
    "--custom: value !important";

  (* Spacing/format variations for !important, including comments and casing *)
  check_declaration ~expected:"color:red!important" "color: red!important";
  check_declaration ~expected:"color:red!important" "color: red ! important";
  check_declaration ~expected:"color:red!important"
    "color: red   !   important   ";
  check_declaration ~expected:"color:red!important" "color: red !IMPORTANT";
  check_declaration ~expected:"color:red!important"
    "color: red! /*x*/ important";
  check_declaration ~expected:"margin:10px!important"
    "margin: 10px!/**/important";
  (* Multiple spaces should be valid *)
  check_declaration ~expected:"color:blue!important"
    ~optimized:"color:#00f!important" "color: blue !   important";
  (* Invalid/dangling/duplicate !important should be rejected *)
  none_cursor read_declaration "color: red !;";
  none_cursor read_declaration "color: red !notimportant;";
  none_cursor read_declaration "color: red !important !important;";
  none_cursor read_declaration "color: red !importent;";
  check_declaration ~expected:"color:red!important" "color: red! important";
  (* Valid per CSS spec *)
  none_cursor read_declaration "color: red !IMPORTANT!;"

let invalid () =
  let neg = none_cursor read_declaration in
  let typed_invalid input =
    let cursor = Cursor.of_string input in
    match read_declaration cursor with
    | Some declaration ->
        Alcotest.(check bool)
          (input ^ " is represented as invalid")
          true
          (Css.Declaration.is_invalid declaration)
    | None -> Alcotest.failf "expected a typed invalid declaration for %S" input
    | exception Error.Parse_error _ ->
        Alcotest.failf "expected a typed invalid declaration for %S" input
  in
  (* Unknown property names are syntactically valid declarations. *)
  check_declaration ~expected:"not-a-property:value" "not-a-property: value";
  (* Invalid property names *)
  neg "123invalid: value";
  neg ": value";
  (* Invalid values for known properties *)
  none_cursor read_declaration "color: not-a-color";
  neg "display: not-a-display";
  neg "position: nowhere";
  neg "width: invalid";
  (* Type mismatches *)
  neg "opacity: red";
  neg "z-index: blue";
  neg "font-weight: green";
  neg "font-family: default";
  neg "font-family: system-ui default";
  typed_invalid "font-family: Arial, inherit";
  typed_invalid "font-family: revert-layer, serif";
  neg "font-family: system-ui revert-layer, serif";

  (* CSS-wide keywords mixing - should fail when mixed with other values *)
  neg "margin: inherit 10px";
  neg "padding: 10px inherit";
  neg "color: red inherit";
  neg "font-size: unset 12px";
  neg "opacity: revert 0.5";
  neg "z-index: revert-layer 10"

let spec_property_grammar_table_expansion () =
  (* Cross-spec property grammar vectors. This grows toward an exhaustive table:
     each property area gets both accepted values and values that should be
     rejected by that property's grammar. *)
  let positive =
    [
      ("display", "inline flow-root");
      ("display", "list-item flow-root");
      ("position", "sticky");
      ("inset", "1px 2px 3px 4px");
      ("inset-inline", "auto 10%");
      ("box-sizing", "border-box");
      ("width", "fit-content(20rem)");
      ("min-width", "min-content");
      ("max-width", "stretch");
      ("aspect-ratio", "16/9");
      ("contain", "layout paint");
      ("container-type", "inline-size");
      ("container-name", "card");
      ("container", "card / inline-size");
      ("overflow", "clip");
      ("overflow-clip-margin", "content-box 1px");
      ("overscroll-behavior", "contain none");
      ("scroll-snap-type", "x mandatory");
      ("scrollbar-width", "thin");
      ("scrollbar-color", "red blue");
      ("margin", "anchor-size(width)");
      ("padding", "max(1rem, 2vw)");
      ("border", "1px solid currentColor");
      ("border-radius", "10px 20px / 30px 40px");
      ("border-image", "linear-gradient(red, blue) 30");
      ("background", "url(bg.png) no-repeat center / cover border-box");
      ("background-position", "left 10px top 20px");
      ("box-shadow", "0 1px 2px rgb(0 0 0 / .2)");
      ("clip-path", "inset(10px round 2px)");
      ("shape-outside", "circle(50%)");
      ("shape-margin", "1rem");
      ("color", "color(display-p3 1 0 0)");
      ("color", "light-dark(black, white)");
      ("accent-color", "auto");
      ("opacity", ".5");
      ("opacity", "2");
      ("mix-blend-mode", "multiply");
      ("filter", "blur(5px) contrast(120%)");
      ("font", "italic small-caps bold 16px/1.5 serif");
      ("font-size", "clamp(1rem, 2vw, 2rem)");
      ("font-weight", "650");
      ("font-weight", "1000");
      ("font-stretch", "75%");
      ("font-feature-settings", "\"kern\" 1");
      ("font-variation-settings", "\"wght\" 650");
      ("font-palette", "--brand");
      ("text-wrap-mode", "wrap");
      ("text-wrap-style", "balance");
      ("white-space", "preserve nowrap");
      ("line-height", "normal");
      ("word-break", "break-word");
      ("writing-mode", "vertical-rl");
      ("text-combine-upright", "digits 2");
      ("text-combine-upright", "digits 3");
      ("transform", "translateX(10px) rotate(45deg) scale(1.2)");
      ("translate", "10px 20px");
      ("rotate", "1 0 0 45deg");
      ("scale", "1.2 2");
      ("perspective-origin", "left 10px top 20px");
      ("transition", "opacity 1s ease-in .2s");
      ("transition-behavior", "allow-discrete");
      ("animation", "fade 1s linear 2 alternate both running");
      ("animation-timeline", "scroll()");
      ("animation-range", "entry 10% exit 90%");
      ("view-transition-name", "card");
      ("grid-template-columns", "subgrid");
      ("grid-template-rows", "masonry");
      ("grid-auto-flow", "row dense");
      ("gap", "1rem 2rem");
      ("flex", "1 1 0");
      ("flex-flow", "row wrap");
      ("place-content", "center space-between");
      ("place-items", "start stretch");
      ("place-self", "auto center");
      ("list-style", "square inside");
      ("content", "open-quote attr(title) close-quote");
      ("counter-reset", "section 1");
      ("counter-increment", "section");
      ("resize", "both");
      ("cursor", "url(cursor.cur), pointer");
      ("user-select", "none");
      ("appearance", "none");
      ("pointer-events", "auto");
      ("anchor-name", "--tooltip");
      ("position-anchor", "--tooltip");
      ("position-area", "top span-right");
      ("position-try-fallbacks", "--below, flip-block");
      ("object-fit", "cover");
      ("object-position", "left 10px top 20px");
      ("mask", "url(mask.svg) center / contain no-repeat");
      ("mask-type", "luminance");
    ]
  in
  List.iter
    (fun (property, value) ->
      let input = property ^ ":" ^ value in
      let expected, optimized =
        match (property, value) with
        | "container", "card / inline-size" ->
            (Some "container:card/inline-size", None)
        | "padding", "max(1rem, 2vw)" -> (Some "padding:max(1rem,2vw)", None)
        | "border-radius", "10px 20px / 30px 40px" ->
            (Some "border-radius:10px 20px/30px 40px", None)
        | "border-image", "linear-gradient(red, blue) 30" ->
            ( Some "border-image:linear-gradient(red,blue)30",
              Some "border-image:linear-gradient(red,blue)30" )
        | "background", "url(bg.png) no-repeat center / cover border-box" ->
            ( Some "background:url(bg.png)center/cover no-repeat border-box",
              Some "background:url(bg.png)50%/cover no-repeat border-box" )
        | "scrollbar-color", "red blue" ->
            (Some "scrollbar-color:red blue", Some "scrollbar-color:red #00f")
        | "border", "1px solid currentColor" ->
            (Some "border:1px solid currentColor", Some "border:1px solid")
        | "background-position", "left 10px top 20px" ->
            ( Some "background-position:left 10px top 20px",
              Some "background-position:10px 20px" )
        | "box-shadow", "0 1px 2px rgb(0 0 0 / .2)" ->
            ( Some "box-shadow:0 1px 2px rgb(0 0 0/.2)",
              Some "box-shadow:0 1px 2px #0003" )
        | "color", "light-dark(black, white)" ->
            ( Some "color:light-dark(black,white)",
              Some "color:light-dark(#000,#fff)" )
        | "filter", "blur(5px) contrast(120%)" ->
            (Some "filter:blur(5px)contrast(120%)", None)
        | "font-size", "clamp(1rem, 2vw, 2rem)" ->
            (Some "font-size:clamp(1rem,2vw,2rem)", None)
        | "transform", "translateX(10px) rotate(45deg) scale(1.2)" ->
            (Some "transform:translateX(10px)rotate(45deg)scale(1.2)", None)
        | "rotate", "1 0 0 45deg" -> (Some "rotate:x 45deg", None)
        | "font", "italic small-caps bold 16px/1.5 serif" ->
            ( Some "font:italic small-caps bold 16px/1.5 serif",
              Some "font:italic small-caps 700 16px/1.5 serif" )
        | "animation", "fade 1s linear 2 alternate both running" ->
            (Some "animation:fade 1s linear 2 alternate both", None)
        | "animation-range", "entry 10% exit 90%" ->
            (Some "animation-range:entry 10%exit 90%", None)
        | "display", "inline flow-root" ->
            (Some "display:inline flow-root", Some "display:inline-block")
        | "display", "list-item flow-root" ->
            (* CSS Display 3 (ED) sec. 2 combines the list-item components with
               [&&], so the two spellings are one value; sec. 2.3 leaves the
               [block] outside unwritten. *)
            (Some "display:flow-root list-item", None)
        | "position-try-fallbacks", "--below, flip-block" ->
            (Some "position-try-fallbacks:--below,flip-block", None)
        | "cursor", "url(cursor.cur), pointer" ->
            (Some "cursor:url(cursor.cur),pointer", None)
        | "mask", "url(mask.svg) center / contain no-repeat" ->
            (Some "mask:url(mask.svg)center/contain no-repeat", None)
        | _ -> (None, None)
      in
      check_declaration ?expected ?optimized input)
    positive;
  let negative =
    [
      ("display", "block inline flex");
      ("position", "sticky absolute");
      ("inset-inline", "1px 2px 3px");
      ("box-sizing", "padding-box");
      ("width", "-1px");
      ("min-width", "-1px");
      ("aspect-ratio", "16 /");
      ("contain", "none layout");
      ("container-type", "inline-size size");
      ("container", "/ inline-size");
      ("overflow", "visible clip scroll");
      ("overflow-clip-margin", "-1px");
      ("overscroll-behavior", "contain none auto");
      ("scroll-snap-type", "mandatory x");
      ("scrollbar-width", "wide");
      ("scrollbar-color", "red");
      ("padding", "-1px");
      ("border", "solid solid");
      ("border-radius", "10px /");
      ("border-image", "none none");
      ("background-size", "cover contain");
      ("background-position", "left right");
      ("box-shadow", "inset inset 0 0 red");
      ("shape-margin", "-1px");
      ("color", "light-dark(black)");
      ("mix-blend-mode", "normal multiply");
      ("filter", "blur(1px, 2px)");
      ("font", "bold serif");
      ("font-stretch", "-10%");
      ("font-feature-settings", "\"kern\" maybe");
      ("font-variation-settings", "\"wght\"");
      ("font-palette", "1");
      ("text-wrap-style", "loud");
      ("white-space", "normal pre");
      ("line-height", "-1");
      ("writing-mode", "vertical");
      ("transform", "rotate()");
      ("translate", "10px 20px 30px 40px");
      ("rotate", "1 0 45deg");
      ("scale", "1 2 3 4");
      ("transition", "opacity ease ease");
      ("transition-behavior", "allow-discrete normal");
      ("animation", "1s 2s 3s");
      ("view-transition-name", "none card");
      ("grid-auto-flow", "dense dense");
      ("gap", "-1px");
      ("flex", "1 2 3 4");
      ("flex-flow", "row column");
      ("place-content", "left");
      ("content", "open-quote none");
      ("counter-reset", "none section");
      ("resize", "block inline both");
      ("cursor", "url(cursor.cur)");
      ("user-select", "all none");
      ("anchor-name", "tooltip");
      ("position-anchor", "tooltip");
      ("position-area", "top top");
      ("object-fit", "cover contain");
      ("mask-type", "alpha luminance");
    ]
  in
  List.iter
    (fun (property, value) ->
      neg_cursor read_declaration (property ^ ":" ^ value))
    negative

let edge_cases () =
  (* Empty/whitespace values where valid *)
  check_declaration ~expected:"content:\"\"" "content: \"\"";
  check_declaration ~expected:"content:\" \"" "content: \" \"";

  (* Complex calc expressions *)
  (* Cases with / operator - should be minified without spaces per CSS spec *)
  check_declaration ~expected:"width:calc(100% + 10px)"
    "width: -webkit-calc(100% + 10px)";
  check_declaration ~expected:"margin-left:calc(-1*5px)"
    ~optimized:"margin-left:-5px" "margin-left: -webkit-calc(-1 * 5px)";
  check_declaration ~expected:"width:calc((100% - 20px)/2)"
    "width: calc((100% - 20px) / 2)";
  check_declaration ~expected:"height:calc(100vh - calc(50px + 1em))"
    "height: calc(100vh - calc(50px + 1em))";

  (* Per CSS Values 4 section 10.10.1 the printer fully simplifies all-constant
     calcs and reduces multiplicative subexpressions against same-unit
     operands. *)
  check_declaration ~expected:"width:calc((10px + 5px)*2)"
    "width: calc((10px + 5px) * 2)";
  check_declaration ~expected:"height:calc(100% - 10px*2)"
    "height: calc(100% - 10px * 2)";
  check_declaration ~expected:"top:calc(50% - (20px + 10px)*1.5)"
    "top: calc(50% - (20px + 10px) * 1.5)";

  (* Very long values *)
  let long_shadow =
    "0 1px 2px rgba(0,0,0,0.1), 0 2px 4px rgba(0,0,0,0.1), "
    ^ "0 4px 8px rgba(0,0,0,0.1), 0 8px 16px rgba(0,0,0,0.1)"
  in
  check_declaration
    ~expected:
      "box-shadow:0 1px 2px rgb(0 0 0/.1),0 2px 4px rgb(0 0 0/.1),0 4px 8px \
       rgb(0 0 0/.1),0 8px 16px rgb(0 0 0/.1)"
    ~optimized:
      "box-shadow:0 1px 2px #0000001a,0 2px 4px #0000001a,0 4px 8px \
       #0000001a,0 8px 16px #0000001a"
    ("box-shadow: " ^ long_shadow)

let css_wide_keywords () =
  (* All properties accept CSS-wide keywords per spec *)
  check_declaration ~expected:"display:inherit" "display: inherit";
  check_declaration ~expected:"margin:unset" "margin: unset";
  check_declaration ~expected:"width:revert-layer" "width: revert-layer";
  check_declaration ~expected:"color:revert" "color: revert"

let spec_cascade7_defaulting () =
  (* CSS Cascade section 7: defaulting keywords are CSS-wide values. They are
     valid as the entire value of any property, including the [all] shorthand,
     and invalid when mixed with other component values. *)
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.serialization_row) ->
      check_declaration ~expected:row.expected row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_positive;
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.invalid_row) ->
      neg_cursor read_declaration row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_negative

let spec_cascade3_shorthands () =
  (* CSS Cascade section 3: shorthand declarations set all of their longhand
     sub-properties as if expanded in place. Omitted sub-properties are reset to
     their initial values unless the individual shorthand says otherwise. *)
  check_declaration ~expected:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"padding:1em 2em" "padding: 1em 2em";
  check_declaration ~expected:"background:green" "background: green";
  check_declaration ~expected:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"font:bold 12pt/14pt Helvetica"
    ~optimized:"font:700 12pt/14pt Helvetica" "font: bold 12pt/14pt Helvetica";
  check_declaration ~expected:"margin:inherit" "margin: inherit";
  check_declaration ~expected:"padding:initial" "padding: initial";
  check_declaration ~expected:"background:unset" "background: unset";
  check_declaration ~expected:"border:revert" "border: revert";
  check_declaration ~expected:"font:revert-layer" "font: revert-layer";
  neg_cursor read_declaration "margin: inherit 1px";
  neg_cursor read_declaration "padding: 1px initial";
  neg_cursor read_declaration "background: green inherit";
  neg_cursor read_declaration "border: 1px solid revert";
  neg_cursor read_declaration "font: bold inherit 12pt Helvetica"

(* A parse error names the property the declaration wrote. [page-break-*]
   minifies to the CSS Fragmentation 3 sec. 3.4 [break-*] property it aliases,
   which is a different property with its own values, so a diagnostic rendered
   under minify reports the failure against a property the author never wrote
   and cannot tell the two apart. *)
let error_names_the_property_written () =
  let message input =
    let r = Cursor.of_string input in
    match read_declaration r with
    | _ -> Alcotest.failf "%s: expected Parse_error but none was raised" input
    | exception Cursor.Parse_error e -> Error.to_string e
  in
  List.iter
    (fun (input, written, alias) ->
      let msg = message input in
      Alcotest.(check bool)
        (input ^ " is reported against " ^ written)
        true
        (Astring.String.is_infix ~affix:("bad value for " ^ written ^ ":") msg);
      Alcotest.(check bool)
        (input ^ " is not reported against " ^ alias)
        false
        (Astring.String.is_infix ~affix:("bad value for " ^ alias ^ ":") msg))
    [
      ("page-break-before: bogus", "page-break-before", "break-before");
      ("page-break-after: bogus", "page-break-after", "break-after");
      ("page-break-inside: bogus", "page-break-inside", "break-inside");
    ]

let spec_cascade3_aliasing () =
  (* CSS Cascade section 3.1: legacy shorthands behave as shorthands at parse
     time but are not selected for serialization. The spec example maps
     page-break-before: always to break-before: page. *)
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.serialization_row) ->
      check_declaration ~expected:row.expected row.input)
    Cascade_spec_inventory.Declaration_grammar.alias_positive;
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.invalid_row) ->
      neg_cursor read_declaration row.input)
    Cascade_spec_inventory.Declaration_grammar.alias_negative

let spec_break3_page_break_var () =
  (* CSS Fragmentation 3 sec. 3.4 aliases [page-break-*] to [break-*] through a
     value mapping table: [auto | left | right | avoid] map to themselves and
     [always] maps to [page]. The table is keyed on the value, so a [var()]
     declaration has no alias to take: substitution happens at computed-value
     time, and the one mapping that is not the identity has no [break-*]
     spelling to fall back on. The declaration names [page-break-*] and stays
     there, in both modes. *)
  List.iter
    (fun (input, pretty, minified) ->
      check_declaration ~minify:false ~expected:pretty input;
      check_declaration ~expected:minified input)
    [
      ( "page-break-before: var(--x)",
        "page-break-before: var(--x)",
        "page-break-before:var(--x)" );
      ( "page-break-after: var(--x)",
        "page-break-after: var(--x)",
        "page-break-after:var(--x)" );
      ( "page-break-inside: var(--x)",
        "page-break-inside: var(--x)",
        "page-break-inside:var(--x)" );
      (* The fallback shows what the alias cannot carry: an unset [--x] leaves
         [always], which no [break-*] property accepts. *)
      ( "page-break-after: var(--x, always)",
        "page-break-after: var(--x, always)",
        "page-break-after:var(--x,always)" );
      ( "page-break-inside: var(--x) !important",
        "page-break-inside: var(--x) !important",
        "page-break-inside:var(--x)!important" );
    ];
  (* A value the table does map still takes the alias, unmoved. *)
  List.iter
    (fun (input, pretty, minified) ->
      check_declaration ~minify:false ~expected:pretty input;
      check_declaration ~expected:minified input)
    [
      ( "page-break-before: always",
        "page-break-before: always",
        "break-before:page" );
      ( "page-break-after: always",
        "page-break-after: always",
        "break-after:page" );
      ("page-break-after: avoid", "page-break-after: avoid", "break-after:avoid");
      ("page-break-before: left", "page-break-before: left", "break-before:left");
      ( "page-break-inside: avoid",
        "page-break-inside: avoid",
        "break-inside:avoid" );
      ("page-break-inside: auto", "page-break-inside: auto", "break-inside:auto");
    ];
  (* The [var()] declaration is the property it names, not an opaque one that
     happens to spell the same: it answers [same_property] against a keyword
     declaration of that property, the way every other property does. *)
  List.iter
    (fun (var_decl, keyword_decl) ->
      Alcotest.(check bool)
        (var_decl ^ " names the same property as " ^ keyword_decl)
        true
        (same_property
           (Css.Declaration.of_string var_decl)
           (Css.Declaration.of_string keyword_decl)))
    [
      ("page-break-before: var(--x)", "page-break-before: left");
      ("page-break-after: var(--x)", "page-break-after: always");
      ("page-break-inside: var(--x)", "page-break-inside: avoid");
      ("break-after: var(--x)", "break-after: page");
    ]

let spec_cascade3_all () =
  (* CSS Cascade section 3.2: [all] is a shorthand that accepts only CSS-wide
     keywords and resets all CSS properties except direction, unicode-bidi, and
     custom properties. The parser surface can verify the allowed value set. *)
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.serialization_row) ->
      if String.starts_with ~prefix:"all:" row.expected then
        check_declaration ~expected:row.expected row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_positive;
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.invalid_row) ->
      if String.starts_with ~prefix:"all:" row.input then
        neg_cursor read_declaration row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_negative

let comments () =
  (* Comments around colon and inside values *)
  check_declaration ~expected:"color:red" "color/*c*/:/**/red";
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width:/*x*/calc(100% - 10px)";
  check_declaration ~expected:"color:red!important" "color: red!/**/important"

let unit_case () =
  (* Units are ASCII case-insensitive per spec *)
  check_declaration ~expected:"width:10px" "width: 10PX";
  check_declaration ~expected:"margin:1em" "margin: 1EM"

let number_formats () =
  (* Leading dot numbers are valid; scientific notation is also valid per CSS
     spec *)
  check_declaration ~expected:"opacity:.5" "opacity: .5";
  (* Scientific notation IS valid in CSS per the spec *)
  check_declaration ~expected:"opacity:100" "opacity: 1e2"

let integer_precision () =
  check_declaration "order:9007199254740993";
  check_declaration "grid-template-columns:repeat(9007199254740993,1px)";
  List.iter
    (neg_cursor read_declaration)
    [
      "order:999999999999999999999999999999999999";
      "grid-template-columns:repeat(999999999999999999999999999999999999,1px)";
    ]

let unterminated () =
  (* CSS Syntax 5.3.7 / 4.3.5 auto-close unterminated strings, brackets and
     function calls at EOF. Assert the recovered declaration matches the shape
     an explicit closer would have produced -- the parser must not silently drop
     content. *)
  check_declaration ~expected:"content:\"abc\"" "content: \"abc";
  (* The cursor parser preserves the auto-closed inner parens. The stylesheet
     recovery path drops this incomplete declaration before normalization. *)
  check_declaration ~expected:"width:calc(100% - (10px))"
    "width: calc(100% - (10px)";
  (* Declaration-level recovery can accept the unterminated color function; the
     stylesheet parser drops it, so assert the optimize oracle directly through
     [Declaration.normalize]. *)
  let input = "color: rgb(0, 0, 0" in
  check_declaration ~expected:"color:rgb(0 0 0)" input;
  let c = Cursor.of_string input in
  let decl =
    match read_declaration c with
    | Some decl -> decl
    | None -> Alcotest.fail "expected declaration recovery"
  in
  Alcotest.(check string)
    "unterminated rgb declaration normalize" "color:#000"
    (decl |> Css.Declaration.normalize |> Css.Declaration.to_string ~minify:true);
  (* A missing semicolon between two declarations in a block remains a parse
     error. *)
  Css_test_helpers.neg_cursor Css.Declaration.read_block
    "{ color:red margin:10px; }"

let custom_property_values () =
  (* Balanced braces in custom property values *)
  check_specified_value "custom property block specified" "--x: { a: b; }"
    "{ a: b; }";
  check_declaration ~expected:"--x:{a:b;}" "--x: { a: b; }";
  (* Semicolons inside strings are fine *)
  check_declaration ~expected:"--x:\"a;b\"" "--x: \"a;b\"";
  (* var() usage in standard properties, with and without fallback *)
  check_declaration ~expected:"color:var(--c,red)" "color: var(--c, red)";
  check_declaration ~expected:"width:var(--w,10px)" "width: var(--w, 10px)";
  check_declaration ~expected:"margin:var(--m)" "margin: var(--m)";
  (* CSS Custom Properties: var() fallbacks are parsed as declaration-value
     token streams. They are not eagerly type-checked against the destination
     property grammar at parse time. *)
  check_declaration ~expected:"z-index:var(--spec-value,{color:red;})"
    "z-index: var(--spec-value, { color: red; })"

(* A typed custom property carries its layer as metadata; the layer does not
   select a different CSS value serialization. In particular, the historical
   [theme] layer and any caller-defined layer print the same font-family
   value. *)
let typed_custom_font_family_layer_printing () =
  let render layer =
    let declaration, _ =
      Css.var ~layer "font-body" Css.Font_family
        (Css.font_stack [ Css.Name "Inter"; Css.Sans_serif ])
    in
    Css.Declaration.to_string ~minify:true declaration
  in
  let theme = render "theme" in
  Alcotest.(check string)
    "typed font family" "--font-body:Inter,sans-serif" theme;
  Alcotest.(check string)
    "layer-independent serialization" theme (render "utilities")

(* [parse_custom_property] builds a declaration from a name and value that came
   from outside the parser, so it takes only a pair that writes back as the one
   declaration it claims to be: a [<dashed-ident>] name, written back with the
   escapes that read it (CSS Syntax 3 sec. 4.3.7), and a CSS Syntax 3 sec. 8.2
   [<declaration-value>]. *)
let parse_custom_property_guard () =
  let bind name value =
    match parse_custom_property name value with
    | Some d -> to_string ~minify:true d
    | None -> "<rejected>"
  in
  List.iter
    (fun (value, expected) ->
      Alcotest.(check string) ("--x:" ^ value) expected (bind "--x" value))
    [
      (* One declaration value: a matched block, a [;] inside a string and a
         comment all stay inside the declaration. *)
      ("red", "--x:red");
      ("0 0 var(--spacing) black", "--x:0 0 var(--spacing) black");
      ("\"a;b\"", "--x:\"a;b\"");
      ("{a:b;}", "--x:{a:b;}");
      ("red/*", "--x:red");
      (" ", "--x: ");
      (* Not one declaration value: each of these closes the enclosing block,
         starts a second declaration, or runs to end of input. *)
      ("red}", "<rejected>");
      ("red}.evil{color:lime", "<rejected>");
      ("red}@media print{.x{color:red}", "<rejected>");
      ("red;--y:lime", "<rejected>");
      ("red)", "<rejected>");
      ("red]", "<rejected>");
      ("rgb(1,2,3", "<rejected>");
      ("{a:b", "<rejected>");
      ("\"abc", "<rejected>");
      ("", "<rejected>");
    ];
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string) (name ^ ":red") expected (bind name "red"))
    [
      ("--x", "--x:red");
      ("--color-red-500", "--color-red-500:red");
      ("--x;y", "--x\\;y:red");
      ("--x}y", "--x\\}y:red");
      ("--x y", "--x\\ y:red");
      ("--", "<rejected>");
      ("-x", "<rejected>");
      ("color", "<rejected>");
    ]

(* [custom_property] takes authored CSS text, so a pair it accepts has to write
   back as the one declaration it names. CSS Variables 1 sec. 2 gives a custom
   property the value grammar [<declaration-value>?] and a [<dashed-ident>]
   name; a value carrying a top-level [;] or [}], an unterminated function,
   block or string, or an unmatched closing bracket stops being part of the
   declaration as soon as the text is read back, so it is not a pair this
   library can write. A name carrying one of those is written back with the
   escapes that read it (CSS Syntax 3 sec. 4.3.7), so it names the declaration
   it made. *)
let custom_property_guard () =
  (* Build the declaration, print it into a rule and read the rule back. The
     answer is derived from the round-trip, not from a pinned spelling. *)
  let build name value =
    match custom_property name value with
    | exception Failure _ -> "<refused>"
    | d -> (
        let text = to_string ~minify:true d in
        let back =
          match
            Css.of_string ~strict:false
              (String.concat "" [ ":root{"; text; "}" ])
          with
          | Ok { Css.stylesheet = [ Css.Stylesheet.Rule r ]; _ } ->
              List.map (to_string ~minify:true) r.declarations
          | Ok { Css.stylesheet; _ } ->
              [
                String.concat ""
                  [
                    "<"; string_of_int (List.length stylesheet); " statements>";
                  ];
              ]
          | Error _ -> [ "<unparsable>" ]
        in
        match back with
        | [ text' ] when String.equal text text' -> "<round-trips>"
        | back ->
            String.concat ""
              [ text; " reads back as "; String.concat " + " back ])
  in
  List.iter
    (fun (value, expected) ->
      Alcotest.(check string)
        (String.concat "" [ "--x:"; value ])
        expected (build "--x" value))
    [
      (* A [<declaration-value>], and the empty value CSS Variables 1 allows. *)
      ("red", "<round-trips>");
      ("", "<round-trips>");
      (" ", "<round-trips>");
      ("0 0 var(--spacing) black", "<round-trips>");
      ("\"a;b\"", "<round-trips>");
      ("{a:b;}", "<round-trips>");
      ("red/*", "<round-trips>");
      (* CSS Syntax 3 sec. 4.3.6 returns the [<url-token>] at end of input, so
         [url(foo] is the same token [url(foo)] is. *)
      ("url(foo", "<round-trips>");
      (* Not a [<declaration-value>]: a second declaration, a closed rule, an
         unterminated function, block or string, an unmatched bracket. *)
      ("red;--b:blue", "<refused>");
      ("red} .evil{color:lime", "<refused>");
      ("red}", "<refused>");
      ("rgb(1,2,3", "<refused>");
      ("url(foo bar)", "<refused>");
      ("{a:b", "<refused>");
      ("\"abc", "<refused>");
      ("red)", "<refused>");
      ("red]", "<refused>");
    ];
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        (String.concat "" [ name; ":red" ])
        expected (build name "red"))
    [
      ("--x", "<round-trips>");
      ("--color-red-500", "<round-trips>");
      ("--x}y", "<round-trips>");
      ("--x;y", "<round-trips>");
      ("--x y", "<round-trips>");
      ("--", "<refused>");
      ("-x", "<refused>");
      ("color", "<refused>");
    ]

(* [parse_declaration] is handed the name and the value as two strings, and CSS
   Syntax 3 sec. 4.3.7 lets an escape carry a [;], a [}] or a space into a
   custom property's name. Written into the text in front of the [:], such a
   name ends its own declaration, closes the rule around it, or names a
   different property, so the two never meet as text: the name reaches the
   reader as the one ident it is. *)
let parse_declaration_name_case () =
  let bound name value =
    match parse_declaration name value with
    | None -> "<none>"
    | Some d -> (
        let text = to_string ~minify:true d in
        let names decl =
          Option.equal String.equal
            (Css.custom_declaration_name decl)
            (Some name)
        in
        let read_back =
          Css.of_string ~strict:true (String.concat "" [ ":root{"; text; "}" ])
        in
        match read_back with
        | Ok { Css.stylesheet = [ Css.Stylesheet.Rule r ]; _ } -> (
            match r.declarations with
            | [ d' ] when String.equal text (to_string ~minify:true d') ->
                if names d && names d' then text
                else String.concat "" [ text; " names another property" ]
            | back ->
                String.concat ""
                  [
                    text;
                    " reads back as ";
                    String.concat " + " (List.map (to_string ~minify:true) back);
                  ])
        | Ok { Css.stylesheet; _ } ->
            String.concat ""
              [
                text;
                " reads back as ";
                string_of_int (List.length stylesheet);
                " statements";
              ]
        | Error _ -> String.concat "" [ text; " does not read back" ])
  in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string) name expected (bound name "red"))
    [
      ("--x", "--x:red");
      ("--x;y", "--x\\;y:red");
      ("--x}y", "--x\\}y:red");
      ("--x y", "--x\\ y:red");
    ];
  (* A name is one ident or it names nothing: a [:] or a [;] inside it belongs
     to no property cascade can write. *)
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (String.concat "" [ name; " names no declaration" ])
        true
        (parse_declaration name "red" = None))
    [ "a:b"; "color;background"; "color:red"; "" ]

let spec_custom_tokens () =
  check_specified_value "custom property token stream specified"
    "--tokens: [a, b] (c) { d: e; }" "[a, b] (c) { d: e; }";
  check_declaration ~expected:"--tokens:[a,b](c){d:e;}"
    "--tokens: [a, b] (c) { d: e; }";
  check_declaration ~expected:"--empty:" "--empty:";
  check_declaration ~expected:"--commented:a b" "--commented: a /*x*/ b";
  check_declaration ~expected:"--important-token:1 ! important"
    "--important-token: 1 ! important";
  check_declaration ~expected:"--real-important:1!important"
    "--real-important: 1 !important";
  check_declaration ~expected:"--fallback:var(--missing,)"
    "--fallback: var(--missing,)";
  check_specified_value "custom property nested fallback specified"
    "--nested-var: var(--a, var(--b, { color: red; }))"
    "var(--a, var(--b, { color: red; }))";
  check_declaration ~expected:"--nested-var:var(--a,var(--b,{color:red;}))"
    "--nested-var: var(--a, var(--b, { color: red; }))";
  (* CSS Syntax 3 sec. 4.3.5 returns the string token when the input ends before
     the closing quote, so the value holds a string and prints as one. Keeping
     the original bytes instead left it swallowing whatever followed. *)
  check_declaration ~expected:"--bad-string:\"unterminated\""
    "--bad-string: \"unterminated";
  neg_cursor read_declaration "--: value";
  neg_cursor read_declaration "-x: value";
  neg_cursor read_declaration "--x";

  (* CSS Syntax 3 sec. 7.2: a <declaration-value> is any token sequence that
     holds no unmatched closer and no bad-url, so a custom property whose value
     carries one is invalid rather than opaque. *)
  check_declaration ~expected:"--x:hover{}" "--x: hover { }";
  check_declaration ~expected:"--x:a b" "--x: a b";
  neg_cursor read_declaration "--x: hover { ] }";
  neg_cursor read_declaration "--x: a ] b";
  neg_cursor read_declaration "--x: a ) b";
  neg_cursor read_declaration "--x: url(a b)"

(* CSS Fonts 4 sec. 2.1.1 gives a [<font-family-name>] two spellings, a
   [<string>] and a [<custom-ident>+], and they name the same family whether
   that ident sequence runs to one word or several, so the structural-diff key
   folds the quoted spelling onto the bare one. The fold needs a generic family
   in the stream to prove the stream is a font-family list: a custom property is
   otherwise an arbitrary token stream, in which dropping the quotes is a
   different value wherever the property substitutes into another grammar. A
   name the section excludes from [<custom-ident>] keeps its quotes, since
   unquoting it would name the keyword instead of the family. *)
let custom_font_equivalence_key () =
  let key css =
    Css.declaration_value_for_equivalence (Css.Declaration.of_string css)
  in
  List.iter
    (fun (css, expected) -> Alcotest.(check string) css expected (key css))
    [
      (* [sans-serif] is only valid in a font-family list, so both spellings of
         the multi-word name reach the unquoted one. *)
      ({|--f: a,"Segoe UI",sans-serif|}, "a,Segoe UI,sans-serif");
      ({|--f: a,Segoe UI,sans-serif|}, "a,Segoe UI,sans-serif");
      (* No generic family, no proof: the quotes are part of the value. *)
      ({|--f: a,"Segoe UI",b|}, {|a,"Segoe UI",b|});
      ({|--f: a,Segoe UI,b|}, "a,Segoe UI,b");
      (* A [<custom-ident>+] of one word is still a [<custom-ident>+], so the
         lone name reaches the unquoted spelling from either side of the generic
         family. *)
      ({|--f: "Arial",sans-serif|}, "Arial,sans-serif");
      ({|--f: Arial,sans-serif|}, "Arial,sans-serif");
      ({|--f: sans-serif,"Arial"|}, "sans-serif,Arial");
      (* The gate rules the lone name as it rules the sequence. *)
      ({|--f: "Arial",b|}, {|"Arial",b|});
      ({|--f: Arial,b|}, "Arial,b");
      (* The words sec. 2.1.1 excludes from [<custom-ident>] keep their quotes:
         [font-family:serif] names the generic family and [font-family:inherit]
         the CSS-wide keyword, so unquoting either loses the author's family. *)
      ({|--f: "serif",sans-serif|}, {|"serif",sans-serif|});
      ({|--f: "default",sans-serif|}, {|"default",sans-serif|});
      ({|--f: "inherit",sans-serif|}, {|"inherit",sans-serif|});
      ({|--f: "emoji",sans-serif|}, {|"emoji",sans-serif|});
      (* A name with no [<custom-ident>] spelling at all keeps its quotes: [+]
         is no ident code point, and a word opening on a digit tokenises as a
         number rather than an ident. *)
      ({|--f: "Foo+Bar",sans-serif|}, {|"Foo+Bar",sans-serif|});
      ({|--f: "Foo Bar 2",sans-serif|}, {|"Foo Bar 2",sans-serif|});
    ];
  let equal a b = String.equal (key a) (key b) in
  Alcotest.(check bool)
    "a generic family equates the two spellings" true
    (equal {|--f: a,"Segoe UI",sans-serif|} {|--f: a,Segoe UI,sans-serif|});
  Alcotest.(check bool)
    "without one they stay distinct" false
    (equal {|--f: a,"Segoe UI",b|} {|--f: a,Segoe UI,b|});
  Alcotest.(check bool)
    "a single-word name equates too" true
    (equal {|--f: "Arial",sans-serif|} {|--f: Arial,sans-serif|});
  Alcotest.(check bool)
    "a single-word name without a generic family stays distinct" false
    (equal {|--f: "Arial",b|} {|--f: Arial,b|});
  Alcotest.(check bool)
    "a quoted generic family stays distinct from the keyword" false
    (equal {|--f: "serif",sans-serif|} {|--f: serif,sans-serif|})

let color_functions () =
  (* color() with alternate spaces and alpha *)
  check_declaration ~expected:"color:color(display-p3 1 0 0/.5)"
    "color: color(display-p3 1 0 0 / 0.5)"

let filter_omitted_arguments () =
  (* Filter Effects 1 section 6.1: omitted blur/hue-rotate arguments default to
     zero; all optional number-percentage arguments default to one. *)
  List.iter
    (fun prop ->
      List.iter
        (fun (fn, default) ->
          let omitted = String.concat "" [ prop; ":"; fn; "()" ] in
          let explicit =
            String.concat "" [ prop; ":"; fn; "("; default; ")" ]
          in
          check_declaration ~expected:omitted omitted;
          check_declaration ~expected:omitted
            (String.concat "" [ prop; ":"; fn; "( /**/ )" ]);
          let canonical text =
            of_string text |> normalize |> string_of_value ~minify:true
          in
          Alcotest.(check string)
            omitted (canonical explicit) (canonical omitted))
        [
          ("blur", "0px");
          ("brightness", "1");
          ("contrast", "1");
          ("grayscale", "1");
          ("hue-rotate", "0deg");
          ("invert", "1");
          ("opacity", "1");
          ("saturate", "1");
          ("sepia", "1");
        ])
    [ "filter"; "backdrop-filter"; "-webkit-backdrop-filter" ]

let filter_blur_grammar () =
  (* Filter Effects 1 section 6.1: blur takes a non-negative length, never a
     percentage. CSS Values 4 section 10: math ranges clamp at computed-value
     time, but a percentage cannot acquire a length type by cancelling out. *)
  List.iter
    (fun prop ->
      List.iter
        (fun value ->
          none_cursor read_declaration (prop ^ ":blur(" ^ value ^ ")"))
        [
          "0%";
          "10%";
          "-1px";
          "auto";
          "none";
          "min-content";
          "calc(0%)";
          "calc(1px + 0%)";
          "calc(10% - 10%)";
          "min(1px, 2%)";
          "min(0, 1px)";
          "clamp(0, 1px, 2px)";
          "max(0%, 1px)";
          "clamp(0px, 1%, 2px)";
          "minmax(1px, 2px)";
          "fit-content(1px)";
        ];
      List.iter
        (fun value ->
          let input = prop ^ ":blur(" ^ value ^ ")" in
          let declarations = check_declarations input 1 in
          List.iter
            (fun declaration ->
              let printed =
                Css.Declaration.to_string ~minify:true declaration
              in
              ignore (check_declarations printed 1))
            declarations)
        [
          "";
          "0";
          "1px";
          "1em";
          "calc(1px + 2px)";
          "calc(-1px)";
          "min(-1px, 2px)";
          "max(0px, 1em)";
          "clamp(0px, 1em, 2px)";
          "var(--radius)";
        ])
    [ "filter"; "backdrop-filter"; "-webkit-backdrop-filter" ]

let angle_units () =
  check_declaration ~expected:"transform:rotate(.5turn)"
    "transform: rotate(0.5turn)";
  check_declaration ~expected:"transform:rotate(1.5708rad)"
    "transform: rotate(1.5708rad)";
  (* deg/turn/grad conversion is an optimize rewrite, not a pp spelling: pp
     holds the authored units, optimize converts to the shortest. *)
  decl_optimizes_to ~held:"transform:skew(.25turn,100grad)"
    ~into:"transform:skew(90deg,90deg)" "transform: skew(0.25turn, 100grad)";
  (* A radian converts through pi, so it is never exactly a whole number of
     degrees - except at zero, which is the same angle in every unit. *)
  decl_optimizes_to ~held:"transform:rotate(0rad)"
    ~into:"transform:rotate(0deg)" "transform: rotate(0rad)";
  decl_optimizes_to ~held:"filter:hue-rotate(0rad)" ~into:"filter:hue-rotate()"
    "filter: hue-rotate(0rad)";
  (* hue-rotate() takes [ <angle> | <zero> ]? and means 0 when omitted, so the
     other zero spellings drop the argument as well. *)
  decl_optimizes ~prop:"filter" ~into:"hue-rotate()" "hue-rotate(0turn)"

let property_case () =
  (* Property names are ASCII case-insensitive *)
  check_declaration ~expected:"color:red" "COLOR: red";
  check_declaration ~expected:"background-image:url(\"a b (1).png\")"
    "BACKGROUND-IMAGE: url(\"a b (1).png\")"

let url_values () =
  check_declaration
    ~expected:"background-image:url(https://example.com/img/a.png)"
    "background-image: url(https://example.com/img/a.png)";
  check_declaration ~expected:"background-image:url(\"a b.png\")"
    "background-image: url(\"a b.png\")";
  check_declaration
    ~expected:"background-image:url(data:image/svg+xml;utf8,<svg/>)"
    "background-image: url(data:image/svg+xml;utf8,<svg/>)"

let spec_platform_property_vectors () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("display: block", "display:block");
      ("position: absolute", "position:absolute");
      ("float: left", "float:left");
      ("clear: both", "clear:both");
      ("table-layout: fixed", "table-layout:fixed");
      ("border-collapse: collapse", "border-collapse:collapse");
      ("caption-side: bottom", "caption-side:bottom");
      ("z-index: auto", "z-index:auto");
      ("width: stretch", "width:stretch");
      ("height: contain", "height:contain");
      ("display: contents", "display:contents");
      ("anchor-name: --tooltip", "anchor-name:--tooltip");
      ("position-anchor: --tooltip", "position-anchor:--tooltip");
      ( "position-try-fallbacks: --below, --above",
        "position-try-fallbacks:--below,--above" );
      ("grid-template-columns: subgrid", "grid-template-columns:subgrid");
      ("grid-template-rows: masonry", "grid-template-rows:masonry");
      ("shape-outside: circle(50%)", "shape-outside:circle(50%)");
      ("shape-margin: 1rem", "shape-margin:1rem");
      ("overflow-clip-margin: 1px", "overflow-clip-margin:1px");
      ("overflow-anchor: auto", "overflow-anchor:auto");
      ("scrollbar-width: thin", "scrollbar-width:thin");
      ( "scrollbar-gutter: stable both-edges",
        "scrollbar-gutter:stable both-edges" );
      ("line-height-step: 4px", "line-height-step:4px");
      ("font-palette: --brand", "font-palette:--brand");
      ( "font-synthesis: weight style small-caps",
        "font-synthesis:weight style small-caps" );
      ("text-wrap-style: pretty", "text-wrap-style:pretty");
      ("text-box-trim: trim-both", "text-box-trim:trim-both");
      ("writing-mode: sideways-rl", "writing-mode:sideways-rl");
      ("animation-timeline: scroll()", "animation-timeline:scroll()");
      ("animation-timeline: scroll(Block)", "animation-timeline:scroll()");
      ( "animation-range: entry 0% exit 100%",
        "animation-range:entry 0%exit 100%" );
      ("animation-range: Cover 10%", "animation-range:cover 10%");
      ( "transition-behavior: allow-discrete",
        "transition-behavior:allow-discrete" );
      ("view-transition-name: card", "view-transition-name:card");
      ("image-orientation: from-image", "image-orientation:from-image");
      ("background-clip: text", "background-clip:text");
      ("box-sizing: border-box", "box-sizing:border-box");
      ("aspect-ratio: 16 / 9", "aspect-ratio:16/9");
      ("contain: size layout style paint", "contain:size layout style paint");
      ("content-visibility: auto", "content-visibility:auto");
      ("contain-intrinsic-size: auto 300px", "contain-intrinsic-size:auto 300px");
      ("container-type: inline-size", "container-type:inline-size");
      ("container-name: card", "container-name:card");
      ("container: card / inline-size", "container:card/inline-size");
      (* Name-only form (no slash, no type) - the spec allows [container:
         <name>] alone; pins the canonical form for the typed shorthand emitter
         ([container : ?type_:_ -> container_name -> declaration]). *)
      ("container: card", "container:card");
      ("overscroll-behavior: contain", "overscroll-behavior:contain");
      ("overscroll-behavior-inline: none", "overscroll-behavior-inline:none");
      ("overscroll-behavior-block: contain", "overscroll-behavior-block:contain");
      ("scroll-snap-type: x mandatory", "scroll-snap-type:x mandatory");
      ("scroll-snap-align: none start", "scroll-snap-align:none start");
      ("scroll-margin-block: 1px 1px", "scroll-margin-block:1px 1px");
      ("scroll-margin: 1px 2px 3px 4px", "scroll-margin:1px 2px 3px 4px");
      ("scroll-margin-block: 1rem 2rem", "scroll-margin-block:1rem 2rem");
      ("scroll-padding-inline: 10px", "scroll-padding-inline:10px");
      ("scroll-padding: 1px 2px", "scroll-padding:1px 2px");
      ("scroll-padding: 1px 2px 3px 4px", "scroll-padding:1px 2px 3px 4px");
      ("scroll-padding-block: auto 10%", "scroll-padding-block:auto 10%");
      ("margin-trim: block", "margin-trim:block");
      ("field-sizing: content", "field-sizing:content");
      ("color-scheme: light dark", "color-scheme:light dark");
      ("accent-color: auto", "accent-color:auto");
      ("mask-mode: alpha", "mask-mode:alpha");
      ("mask-composite: add", "mask-composite:add");
      ("offset-path: path('M 0 0 L 1 1')", "offset-path:path(\"M 0 0 L 1 1\")");
      ("offset-distance: 50%", "offset-distance:50%");
      ("font-size-adjust: from-font", "font-size-adjust:from-font");
      ("font-variant-emoji: emoji", "font-variant-emoji:emoji");
      ("text-spacing-trim: trim-start", "text-spacing-trim:trim-start");
      ("hyphenate-limit-chars: 6 3 2", "hyphenate-limit-chars:6 3 2");
      ("initial-letter: 2 3", "initial-letter:2 3");
      ( "text-decoration-thickness: from-font",
        "text-decoration-thickness:from-font" );
      ("view-timeline-name: --reveal", "view-timeline-name:--reveal");
      ("view-timeline-axis: block", "view-timeline-axis:block");
      ("timeline-scope: --reveal", "timeline-scope:--reveal");
      ("width: min(10px, 5vw)", "width:min(10px,5vw)");
      ("width: max(10px, 5vw)", "width:max(10px,5vw)");
      ("width: clamp(10px, 5vw, 100px)", "width:clamp(10px,5vw,100px)");
      ( "width: calc-size(auto, size + 1rem)",
        "width:calc-size(auto,size + 1rem)" );
      ("opacity: abs(-0.5)", "opacity:abs(-.5)");
      ("opacity: sign(var(--delta))", "opacity:sign(var(--delta))");
      ( "background-image: image-set(url(a.avif) type(\"image/avif\") 1x, \
         url(a.png) type(\"image/png\") 1x)",
        "background-image:image-set(url(a.avif) \
         type(\"image/avif\")1x,url(a.png) type(\"image/png\")1x)" );
      ("width: attr(data-w px, 10px)", "width:attr(data-w px,10px)");
      ( "width: attr(data-w px, calc(100% - 1rem))",
        "width:attr(data-w px,calc(100% - 1rem))" );
      ( "width: attr(data-w px, var(--fallback, 10px))",
        "width:attr(data-w px,var(--fallback,10px))" );
      ( "height: attr(data-h type(<length>), 1rem)",
        "height:attr(data-h type(<length>),1rem)" );
      ( "color: attr(data-color type(<color>), red)",
        "color:attr(data-color type(<color>),red)" );
      ( "content: attr(data-label string, \"x y\")",
        "content:attr(data-label string,\"x y\")" );
      ( "content: attr(data-label string, var(--label, \"x y\"))",
        "content:attr(data-label string,var(--label,\"x y\"))" );
      ( "font: italic small-caps 650 condensed 16px/1.5 \"Brand\", serif",
        "font:italic small-caps 650 condensed 16px/1.5 Brand,serif" );
      ("display: block flex", "display:block flex");
      ( "grid-template: \"head head\" auto \"nav main\" 1fr / 12rem 1fr",
        "grid-template:\"head head\" auto \"nav main\" 1fr/12rem 1fr" );
      ( "transition: opacity 1s ease-in .2s allow-discrete",
        "transition:opacity 1s ease-in .2s allow-discrete" );
      ("animation-range: exit entry", "animation-range:exit entry");
      ("scroll-timeline: --scroller block", "scroll-timeline:--scroller block");
      ("view-timeline: --reveal inline", "view-timeline:--reveal inline");
    ];
  check_declaration ~expected:"scrollbar-color:red blue"
    ~optimized:"scrollbar-color:red #00f" "scrollbar-color: red blue";
  (* All-constant math reductions are optimize transforms; pp holds the authored
     function node and only drops default arguments. *)
  check_declaration ~expected:"width:round(10px,3px)" ~optimized:"width:9px"
    "width: round(nearest, 10px, 3px)";
  check_declaration ~expected:"width:mod(18px,5px)" ~optimized:"width:3px"
    "width: mod(18px, 5px)";
  check_declaration ~expected:"width:rem(18px,5px)" ~optimized:"width:3px"
    "width: rem(18px, 5px)";
  check_declaration ~expected:"width:hypot(3px,4px)" ~optimized:"width:5px"
    "width: hypot(3px, 4px)";
  check_declaration ~expected:"color:color-mix(in oklab,red 40%,blue)"
    ~optimized:"color:#7551b6" "color: color-mix(in oklab, red 40%, blue)";
  check_declaration ~expected:"color:light-dark(canvastext,white)"
    ~optimized:"color:light-dark(canvastext,#fff)"
    "color: light-dark(CanvasText, white)";
  check_declaration ~expected:"width:attr(data-w px,calc(10px + 0px))"
    ~optimized:"width:attr(data-w px,calc(10px + 0px))"
    "width: attr(data-w px, calc(10px + 0px))";
  check_declaration
    ~expected:"border-image:linear-gradient(red,blue)30 fill/10px/1 stretch"
    ~optimized:"border-image:linear-gradient(red,blue)30 fill/10px/1 stretch"
    "border-image: linear-gradient(red, blue) 30 fill / 10px / 1 stretch";
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "position: sticky absolute";
      "float: center";
      "table-layout: grid";
      "anchor-name: tooltip";
      "position-anchor: tooltip";
      "shape-margin: -1px";
      "overflow-clip-margin: -1px";
      "overflow-anchor: sometimes";
      "scrollbar-width: wide";
      "scrollbar-gutter: stable auto";
      "line-height-step: -4px";
      "font-palette: 1";
      "text-wrap-style: loud";
      "text-box-trim: 1px";
      "animation-timeline: scroll(";
      "view-transition-name: none card";
      "image-orientation: upside-down";
      "aspect-ratio: 16 /";
      "contain: layout layout";
      "container-type: inline-size size";
      "container: / inline-size";
      "scroll-snap-type: mandatory x";
      "margin-trim: block inline block";
      "field-sizing: auto";
      "mask-composite: plus";
      "offset-distance: -10%";
      "font-size-adjust: from-font 1";
      "font-variant-emoji: smile";
      "initial-letter: 0";
      "view-timeline-axis: diagonal";
      "width: clamp(10px, 20px)";
      "width: round(10px)";
      "width: mod(10px)";
      "opacity: abs()";
      "opacity: sign()";
      "background-image: image-set(url(a.png))";
      "border-image: linear-gradient(red, blue) fill fill";
      "font: bold serif";
      "transition: allow-discrete allow-discrete";
      "scroll-timeline: block --scroller";
      "view-timeline: inline --reveal";
    ]

let spec_values_l45_edges () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("width: calc(100% - 2rem)", "width:calc(100% - 2rem)");
      (* pp is pure: it holds calc() unfolded and serializes lexically; the
         all-constant fold to 2px / 50px is an optimize transform. *)
      ("width: calc(1px * 2)", "width:calc(1px*2)");
      ("width: calc(100px / 2)", "width:calc(100px/2)");
      ("width: min(10px, 5cqw)", "width:min(10px,5cqw)");
      ("width: max(10svw, 20lvw)", "width:max(10svw,20lvw)");
      ("height: clamp(10dvh, 50%, 100dvh)", "height:clamp(10dvh,50%,100dvh)");
      ("margin: anchor-size(width)", "margin:anchor-size(width)");
      ("top: anchor(bottom)", "top:anchor(bottom)");
      ("font-size: calc(1rem + 1cqi)", "font-size:calc(1rem + 1cqi)");
      ("color: oklab(60% .1 .2)", "color:oklab(60%.1 .2)");
      ("color: oklch(60% .2 120)", "color:oklch(60%.2 120)");
      ("color: color(display-p3 1 0 0 / .5)", "color:color(display-p3 1 0 0/.5)");
      ( "color: rgb(from var(--c) r g b / 50%)",
        "color:rgb(from var(--c) r g b/.5)" );
      (* Each channel is one component value, so a space between two of them is
         a separator the tokens do not need: [20%g] is a percentage and an
         ident, and [calc(...)10] a function and a number. *)
      ("color: rgb(from red 20%g b / alpha)", "color:rgb(from red 20%g b/alpha)");
      ( "color: rgb(from red 20% g b / alpha)",
        "color:rgb(from red 20%g b/alpha)" );
      ( "color: rgb(from red r calc(g * 2)10)",
        "color:rgb(from red r calc(g * 2)10)" );
      ( "color: rgb(from red r calc(g * 2) 10)",
        "color:rgb(from red r calc(g * 2)10)" );
      (* pp holds the authored node for the Named blue, the rgb()/alpha, and the
         turn unit. The colour cross-fold and angle conversion are optimize
         transforms. *)
      ( "background: conic-gradient(from 45deg, red, blue)",
        "background:conic-gradient(from 45deg,red,blue)" );
      ( "background: cross-fade(url(a.png) 40%, url(b.png))",
        "background:cross-fade(url(a.png) 40%,url(b.png))" );
      ( "filter: drop-shadow(0 0 2px rgb(0 0 0 / .4))",
        "filter:drop-shadow(0 0 2px rgb(0 0 0/.4))" );
      ( "transform: translate(10px, 20%) rotate(.25turn) scale(1.2)",
        "transform:translate(10px,20%)rotate(.25turn)scale(1.2)" );
      ( "background-position: left 10px top 20%",
        "background-position:left 10px top 20%" );
      ("border-radius: 10px / 20px", "border-radius:10px/20px");
      ( "clip-path: xywh(0 0 100% 100% round 10px)",
        "clip-path:xywh(0 0 100% 100% round 10px)" );
    ];
  (* optimize+minify owns the folds pp holds above: Named->hex, rgb()->hex,
     turn->shortest angle, and the position-keyword canonicalization. *)
  check_declaration ~expected:"color:lab(50%20 30)" ~optimized:"color:#a16945"
    "color: lab(50% 20 30)";
  check_declaration ~expected:"color:lch(50%30 40)" ~optimized:"color:#a26757"
    "color: lch(50% 30 40)";
  decl_optimizes ~prop:"background" ~into:"conic-gradient(from 45deg,red,#00f)"
    "conic-gradient(from 45deg, red, blue)";
  decl_optimizes ~prop:"filter" ~into:"drop-shadow(0 0 2px #0006)"
    "drop-shadow(0 0 2px rgb(0 0 0 / .4))";
  decl_optimizes ~prop:"transform"
    ~into:"translate(10px,20%)rotate(90deg)scale(1.2)"
    "translate(10px, 20%) rotate(.25turn) scale(1.2)";
  (* translateX(v) is translate(v, 0) = translate(v), one byte shorter; pp holds
     the authored function, optimize folds it. translateY has no shorter
     form. *)
  decl_optimizes ~prop:"transform" ~held:"translateX(10px)"
    ~into:"translate(10px)" "translateX(10px)";
  decl_optimizes ~prop:"transform" ~held:"translateY(10px)"
    ~into:"translateY(10px)" "translateY(10px)";
  decl_optimizes ~prop:"background-position" ~into:"10px 20%"
    "left 10px top 20%";
  decl_optimizes ~prop:"background-position" ~into:"50%" "center";
  decl_optimizes ~prop:"background-position" ~into:"top" "center top";
  decl_optimizes_to
    ~into:"-webkit-mask-position:10px 20px;mask-position:10px 20px"
    "mask-position:left 10px top 20px";
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "width: calc(1px + )";
      "width: calc(* 1px)";
      "width: min()";
      "height: clamp(10px, 20px)";
      "margin: anchor-size()";
      "top: anchor()";
      "color: lab(50% 20)";
      "color: oklch(60% .2)";
      "color: color(display-p3 1 0)";
      "color: rgb(from red r g)";
      "background: conic-gradient()";
      "background: cross-fade(url(a.png), )";
      "filter: drop-shadow()";
      "transform: translate()";
      "border-radius: 10px /";
      "clip-path: xywh(0 0)";
    ]

let spec_remaining_prop_vectors () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("display: ruby", "display:ruby");
      ("display: table-caption", "display:table-caption");
      ("position: fixed", "position:fixed");
      ("float: inline-start", "float:inline-start");
      ("clear: inline-end", "clear:inline-end");
      ("contain: strict", "contain:strict");
      ("content-visibility: hidden", "content-visibility:hidden");
      ( "contain-intrinsic-width: auto 10rem",
        "contain-intrinsic-width:auto 10rem" );
      ("contain-intrinsic-height: 20px", "contain-intrinsic-height:20px");
      ("overflow: clip auto", "overflow:clip auto");
      ("overflow: clip visible", "overflow:clip visible");
      ("overflow-block: scroll", "overflow-block:scroll");
      ("overflow-inline: hidden", "overflow-inline:hidden");
      ( "overscroll-behavior-inline: contain",
        "overscroll-behavior-inline:contain" );
      ("scroll-snap-align: start end", "scroll-snap-align:start end");
      ("scroll-snap-stop: always", "scroll-snap-stop:always");
      ("scroll-margin: 1px 2px 3px 4px", "scroll-margin:1px 2px 3px 4px");
      ("scroll-padding: 1rem 2rem", "scroll-padding:1rem 2rem");
      ("columns: 12rem 3", "columns:12rem 3");
      ( "column-rule: 1px solid currentColor",
        "column-rule:1px solid currentColor" );
      ("column-span: all", "column-span:all");
      ("break-before: page", "break-before:page");
      ("break-after: avoid-page", "break-after:avoid-page");
      ("break-inside: avoid-column", "break-inside:avoid-column");
      ("box-decoration-break: clone", "box-decoration-break:clone");
      ("background-origin: content-box", "background-origin:content-box");
      ("background-clip: padding-box", "background-clip:padding-box");
      ("background-size: contain", "background-size:contain");
      ("border-block: 1px solid red", "border-block:1px solid red");
      ("border-start-start-radius: 1rem", "border-start-start-radius:1rem");
      ("outline: 2px solid Highlight", "outline:2px solid highlight");
      ("outline-offset: -2px", "outline-offset:-2px");
      ( "text-decoration: underline wavy red 2px",
        "text-decoration:underline wavy red 2px" );
      ("text-underline-offset: 2px", "text-underline-offset:2px");
      ( "text-underline-position: under left",
        "text-underline-position:under left" );
      ("text-decoration-skip: auto", "text-decoration-skip:auto");
      ("text-decoration-skip-self: objects", "text-decoration-skip-self:objects");
      ("text-decoration-skip-box: none", "text-decoration-skip-box:none");
      ("text-decoration-skip-inset: auto", "text-decoration-skip-inset:auto");
      ( "text-decoration-skip-spaces: start end",
        "text-decoration-skip-spaces:start end" );
      ("text-emphasis: filled dot red", "text-emphasis:filled dot red");
      ("text-emphasis-style: open sesame", "text-emphasis-style:open sesame");
      ("text-emphasis-color: currentColor", "text-emphasis-color:currentColor");
      ("text-emphasis-position: over right", "text-emphasis-position:over right");
      ( "text-emphasis-skip: punctuation symbols",
        "text-emphasis-skip:punctuation symbols" );
      ("text-orientation: mixed", "text-orientation:mixed");
      ("tab-size: 4", "tab-size:4");
      ("line-break: anywhere", "line-break:anywhere");
      ("overflow-wrap: anywhere", "overflow-wrap:anywhere");
      ("hyphens: manual", "hyphens:manual");
      ("font-optical-sizing: auto", "font-optical-sizing:auto");
      ("font-kerning: normal", "font-kerning:normal");
      ("font-language-override: \"TRK\"", "font-language-override:\"TRK\"");
      ("font-synthesis-style: oblique-only", "font-synthesis-style:oblique-only");
      ("font-synthesis-style: auto", "font-synthesis-style:auto");
      ("font-synthesis-weight: none", "font-synthesis-weight:none");
      ("font-synthesis-small-caps: auto", "font-synthesis-small-caps:auto");
      ("font-synthesis-position: none", "font-synthesis-position:none");
      ( "font-variant-ligatures: common-ligatures",
        "font-variant-ligatures:common-ligatures" );
      ( "font-variant-ligatures: common-ligatures no-discretionary-ligatures \
         historical-ligatures contextual",
        "font-variant-ligatures:common-ligatures no-discretionary-ligatures \
         historical-ligatures contextual" );
      ("font-variant-caps: small-caps", "font-variant-caps:small-caps");
      ( "font-variant-numeric: tabular-nums slashed-zero",
        "font-variant-numeric:tabular-nums slashed-zero" );
      ( "font-variant-numeric: oldstyle-nums tabular-nums stacked-fractions \
         ordinal slashed-zero",
        "font-variant-numeric:oldstyle-nums tabular-nums stacked-fractions \
         ordinal slashed-zero" );
      ("font-variant-position: sub", "font-variant-position:sub");
      ("font-variant-east-asian: ruby", "font-variant-east-asian:ruby");
      ( "font-variant-east-asian: traditional proportional-width ruby",
        "font-variant-east-asian:traditional proportional-width ruby" );
      ("object-view-box: inset(0 0 10% 0)", "object-view-box:inset(0 0 10%0)");
      ("image-rendering: pixelated", "image-rendering:pixelated");
      ("image-resolution: from-image 2dppx", "image-resolution:from-image 2dppx");
      ("mask-border: url(mask.svg) 30 fill", "mask-border:url(mask.svg)30 fill");
      ("mask-size: contain", "mask-size:contain");
      ("mask-repeat: no-repeat", "mask-repeat:no-repeat");
      ("mask-position: left 10px top 20px", "mask-position:left 10px top 20px");
      ( "backdrop-filter: blur(4px) saturate(120%)",
        "backdrop-filter:blur(4px)saturate(120%)" );
      ("will-change: transform, opacity", "will-change:transform,opacity");
      ("touch-action: pan-x pinch-zoom", "touch-action:pan-x pinch-zoom");
      ("caret-color: auto", "caret-color:auto");
      ("resize: block", "resize:block");
      ( "transition-property: opacity, display",
        "transition-property:opacity,display" );
      (* CSS Transitions 1 sec. 2.1: none | <single-transition-property>#, where
         <single-transition-property> is [all | <custom-ident>]. The exclusion
         clause names none, inherit and initial as the values a list of more
         than one may not hold; all is not among them. *)
      ("transition-property: all, opacity", "transition-property:all,opacity");
      ("transition-property: opacity, all", "transition-property:opacity,all");
      ("transition-property: all, all", "transition-property:all,all");
      ("animation-composition: add", "animation-composition:add");
      ("animation-range-start: entry 10%", "animation-range-start:entry 10%");
      ("animation-range-end: exit 90%", "animation-range-end:exit 90%");
      ("scroll-timeline-name: --scroller", "scroll-timeline-name:--scroller");
      ("scroll-timeline-axis: inline", "scroll-timeline-axis:inline");
      ("view-timeline-inset: auto 10%", "view-timeline-inset:auto 10%");
      ("position-try-order: most-width", "position-try-order:most-width");
      ( "position-visibility: anchors-visible",
        "position-visibility:anchors-visible" );
      ("accent-color: auto", "accent-color:auto");
      ("color-scheme: only light dark", "color-scheme:light dark only");
      ( "forced-color-adjust: preserve-parent-color",
        "forced-color-adjust:preserve-parent-color" );
      ("print-color-adjust: exact", "print-color-adjust:exact");
      ("isolation: isolate", "isolation:isolate");
      ("mix-blend-mode: plus-lighter", "mix-blend-mode:plus-lighter");
      ("shape-margin: 2cqi", "shape-margin:2cqi");
      ("shape-image-threshold: .4", "shape-image-threshold:.4");
      ("container-type: inline-size", "container-type:inline-size");
      ("container-name: card layout", "container-name:card layout");
      ("container: card / inline-size", "container:card/inline-size");
      ("anchor-name: --target, --tooltip", "anchor-name:--target,--tooltip");
      ("position-anchor: --target", "position-anchor:--target");
      ("position-area: top span-left", "position-area:top span-left");
      ( "position-try-fallbacks: --below, flip-block",
        "position-try-fallbacks:--below,flip-block" );
      (* CSS Anchor Positioning 1 sec. 6.1: [<dashed-ident> || <try-tactic>],
         and <try-tactic> is itself [flip-block || flip-inline || flip-start].
         || is order-free, so each component appears at most once in any order
         and reads back name-first. *)
      ( "position-try-fallbacks: flip-block --below",
        "position-try-fallbacks:--below flip-block" );
      ( "position-try-fallbacks: --below flip-block",
        "position-try-fallbacks:--below flip-block" );
      ( "position-try-fallbacks: flip-inline flip-block",
        "position-try-fallbacks:flip-block flip-inline" );
      ( "position-try-fallbacks: --a flip-start, flip-inline",
        "position-try-fallbacks:--a flip-start,flip-inline" );
      ("scrollbar-color: auto", "scrollbar-color:auto");
      ("overlay: auto", "overlay:auto");
      ( "transition-behavior: allow-discrete",
        "transition-behavior:allow-discrete" );
      ("font-size-adjust: ex-height 0.5", "font-size-adjust:ex-height .5");
      ("font-variant-emoji: text", "font-variant-emoji:text");
      ("initial-letter: 2 3", "initial-letter:2 3");
      ("line-height-step: 4px", "line-height-step:4px");
      ("text-box: trim-both cap alphabetic", "text-box:trim-both cap alphabetic");
      ("field-sizing: content", "field-sizing:content");
      ("margin-trim: block inline", "margin-trim:block inline");
      ("offset-distance: 10%", "offset-distance:10%");
      ("offset-rotate: reverse 45deg", "offset-rotate:reverse 45deg");
      ( "view-transition-class: card primary",
        "view-transition-class:card primary" );
    ];
  check_declaration ~expected:"border-inline-color:red blue"
    ~optimized:"border-inline-color:red #00f" "border-inline-color: red blue";
  check_declaration ~expected:"border-block-color:red blue"
    ~optimized:"border-block-color:red #00f" "border-block-color: red blue";
  check_declaration ~expected:"border-inline-width:1px 2px"
    "border-inline-width: 1px 2px";
  check_declaration ~expected:"border-block-width:4px" "border-block-width: 4px";
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "display: ruby block";
      "float: inline-start left";
      "contain: strict layout";
      "content-visibility: visible hidden";
      "contain-intrinsic-width: auto auto 10px";
      "overflow-block: visible hidden";
      "overscroll-behavior-inline: contain auto none";
      "scroll-snap-align: start center end";
      "scroll-snap-stop: normal always";
      "columns: 1 2 3";
      "column-rule: solid solid";
      "column-span: all none";
      "break-before: page column";
      "box-decoration-break: slice clone";
      "background-origin: border-box padding-box content-box border-box";
      "background-size: contain cover";
      "border-inline-color: red blue green";
      "border-block-color: red blue green";
      "border-inline-width: 1px 2px 3px";
      "border-start-start-radius: 1rem /";
      "text-decoration: underline none";
      "text-underline-position: auto under";
      "text-decoration-skip: none auto";
      "text-decoration-skip-self: auto";
      "text-decoration-skip-box: all none";
      "text-decoration-skip-inset: 1px";
      "text-decoration-skip-spaces: start start";
      "text-emphasis: filled open";
      "text-emphasis-style: dot circle";
      "text-emphasis-color: red blue";
      "text-emphasis-position: over under";
      "text-emphasis-skip: spaces spaces";
      "tab-size: -1";
      "line-break: anywhere strict";
      "font-optical-sizing: auto none";
      "font-kerning: normal none";
      "font-language-override: 1";
      "font-synthesis-style: auto none";
      "font-synthesis-small-caps: auto none";
      "font-synthesis-position: normal";
      "font-variant-ligatures: normal common-ligatures";
      "font-variant-ligatures: common-ligatures no-common-ligatures";
      "font-variant-caps: small-caps unicase";
      "font-variant-numeric: normal tabular-nums";
      "font-variant-numeric: lining-nums oldstyle-nums";
      "font-variant-position: sub super";
      "font-variant-east-asian: jis78 jis83";
      "font-variant-east-asian: normal ruby";
      "object-view-box: inset()";
      "image-rendering: pixelated smooth";
      "image-resolution: from-image from-image";
      "mask-border: fill fill";
      "mask-size: contain cover";
      "mask-repeat: no-repeat repeat repeat-x";
      "backdrop-filter: blur(1px, 2px)";
      "will-change: auto, transform";
      "touch-action: pan-x pan-left";
      "resize: block inline both";
      "transition-property: none, opacity";
      "transition-property: opacity, none";
      "transition-property: inherit, opacity";
      "animation-composition: add replace";
      "animation-range-start: exit entry";
      "view-timeline-inset: auto auto auto";
      "position-try-order: most-width normal";
      "position-visibility: anchors-visible always";
      "accent-color: auto red";
      "color-scheme: only only";
      "color-scheme: only light only";
      "color-scheme: only dark only";
      "forced-color-adjust: auto none";
      "print-color-adjust: exact economy";
      "isolation: isolate auto";
      "mix-blend-mode: multiply plus-lighter";
      "shape-image-threshold: 1.5";
      "container-name: none card";
      "container: card inline-size";
      "anchor-name: target";
      "anchor-name: --a none";
      "position-anchor: --a --b";
      "position-try-fallbacks: --a --b";
      "position-try-fallbacks: flip-block flip-block";
      "position-area: top bottom";
      "overlay: auto none";
      "transition-behavior: allow-discrete normal";
      "font-size-adjust: ex-height";
      "font-variant-emoji: text emoji";
      "initial-letter: 2 0";
      "text-box: cap trim-both";
      "field-sizing: content fixed";
      "margin-trim: block block";
      "offset-rotate: reverse reverse";
      "view-transition-class: none card";
    ]

let test_declaration () =
  (* Basic declarations - test the declaration type itself *)
  check "color:red";
  check "margin:10px";
  check "display:block";

  (* Custom properties *)
  check "--custom:value";
  check "--color:red";

  (* Important declarations *)
  check "color:red!important";
  check "--custom:value!important";

  (* Complex values. Per CSS Transforms 1 section 8 the printer drops whitespace
     between back-to-back transform functions under minify. *)
  check_declaration ~expected:"background:linear-gradient(to right,red,blue)"
    "background:linear-gradient(to right,red,blue)";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right,red,blue)";
  check "transform:translateX(10px)rotate(45deg)";
  check "font-family:Arial,sans-serif";

  (* Vendor prefixes *)
  check "-webkit-transform:rotate(45deg)";
  check "-moz-appearance:none";

  (* Test invalid declarations using none *)
  none_cursor read_declaration "color red";
  (* Missing colon *)
  none_cursor read_declaration "color:";
  (* Missing value *)
  none_cursor read_declaration ":red";
  (* Missing property *)
  none_cursor read_declaration "123invalid:value";
  (* Invalid property name *)
  none_cursor read_declaration "color:not-a-color";

  (* Invalid value *)

  (* Duplicate !important should fail *)
  neg_cursor read_declaration "color: red blue !important !important"

let css_wide_custom_property_vectors () =
  (* CSS Cascading: CSS-wide keywords are whole declaration values for standard
     properties, not component values inside a larger value. *)
  List.iter
    (fun keyword ->
      check_declaration ("color:" ^ keyword);
      (* pp holds the CSS-wide keyword verbatim; folding margin:initial to 0 is
         an optimize transform, not a pp serialization. *)
      check_declaration ("margin:" ^ keyword);
      none_cursor read_declaration ("color:" ^ keyword ^ " red");
      none_cursor read_declaration ("margin:1px " ^ keyword);
      none_cursor read_declaration ("background:red " ^ keyword))
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  (* CSS Custom Properties: specified values preserve authored token streams;
     the minified declaration path may compact block/list whitespace where
     tokenization is unchanged. *)
  check_specified_value "custom property block token stream specified"
    "--tokens: { color: red }" "{ color: red }";
  check_declaration ~expected:"--tokens:{color:red}" "--tokens: { color: red }";
  check_specified_value "custom property list token stream specified"
    "--list: [a, b, c]" "[a, b, c]";
  check_declaration ~expected:"--list:[a,b,c]" "--list: [a, b, c]";
  check_declaration ~expected:"--empty-fallback:var(--missing,)"
    "--empty-fallback: var(--missing,)";
  check_declaration ~expected:"--not-important:1 ! important"
    "--not-important: 1 ! important";
  check_declaration ~expected:"--is-important:1!important"
    "--is-important: 1 !important";
  none_cursor read_declaration "--: invalid"

let substitution_defers_css_wide_mix_validation () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("margin: var(--x) inherit", "margin:var(--x) inherit");
      ("color: var(--x) initial", "color:var(--x) initial");
      ("all: var(--x) initial", "all:var(--x) initial");
      ( "margin: env(safe-area-inset-top) inherit",
        "margin:env(safe-area-inset-top) inherit" );
      ("width: attr(data-width px) initial", "width:attr(data-width px) initial");
    ];
  (* With no substitution, the CSS-wide keyword is still invalid beside any
     other component. Empty calls match none of the substitution grammars and do
     not defer validation either. *)
  List.iter
    (fun value -> none_cursor read_declaration ("margin:" ^ value))
    [ "1px inherit"; "var() inherit"; "env() inherit"; "attr() inherit" ]

type property_grammar_row = Cascade_spec_inventory.Property_grammar.row

let property_grammar_matrix = Cascade_spec_inventory.Property_grammar.rows

let parse_property_decl property value =
  let input = property ^ ":" ^ value in
  try
    let c = Cursor.of_string input in
    match read_declaration c with
    | None -> None
    | Some decl ->
        let serialized = Css.Declaration.to_string ~minify:true decl in
        let c2 = Cursor.of_string serialized in
        Some (input, serialized, decl, read_declaration c2)
  with Cursor.Parse_error _ | Reader.Parse_error _ -> None

let same_property_reparse (row : property_grammar_row) decl reparsed =
  Css.Declaration.property_name reparsed = row.property
  && Css.Declaration.to_string ~minify:true decl
     = Css.Declaration.to_string ~minify:true reparsed

let check_property_positive (row : property_grammar_row) value =
  match parse_property_decl row.property value with
  | Some (_input, serialized, decl, Some reparsed)
    when same_property_reparse row decl reparsed ->
      ignore serialized
  | Some (input, serialized, _, _) ->
      Alcotest.failf
        "%s positive vector serialized to unparsable or structurally different \
         declaration: %s -> %s"
        row.property input serialized
  | None -> Alcotest.failf "%s positive vector rejected: %s" row.property value

let check_property_negative (row : property_grammar_row) value =
  match parse_property_decl row.property value with
  | None -> ()
  | Some (_, _, decl, _) when Css.Declaration.is_invalid decl -> ()
  | Some (input, serialized, _, _) ->
      Alcotest.failf "%s negative vector parsed: %s -> %s" row.property input
        serialized

let check_property_css_wide (row : property_grammar_row) keyword =
  match parse_property_decl row.property keyword with
  | Some (_, _, decl, Some reparsed)
    when same_property_reparse row decl reparsed ->
      ()
  | Some (input, serialized, _, _) ->
      Alcotest.failf
        "%s CSS-wide keyword did not structurally reparse: %s -> %s"
        row.property input serialized
  | None ->
      Alcotest.failf "%s CSS-wide keyword rejected: %s" row.property keyword

let check_property_var (row : property_grammar_row) =
  let token_stream_fallbacks =
    [
      "{ color: red; }";
      "[a, b, c]";
      "translateX(10px) rotate(45deg)";
      "1 ! important";
    ]
  in
  List.iter
    (fun fallback ->
      match
        parse_property_decl row.property ("var(--spec-value," ^ fallback ^ ")")
      with
      | Some (_, _, decl, Some reparsed)
        when same_property_reparse row decl reparsed ->
          ()
      | Some (input, serialized, _, _) ->
          Alcotest.failf "%s var() value did not structurally reparse: %s -> %s"
            row.property input serialized
      | None ->
          Alcotest.failf "%s var() value rejected with fallback: %s"
            row.property fallback)
    (row.positives @ token_stream_fallbacks)

let property_value_has_substitution value =
  let value = String.lowercase_ascii value in
  List.exists
    (fun name -> Astring.String.is_infix ~affix:(name ^ "(") value)
    [ "var"; "env"; "attr" ]

let check_property_trailing_token_rejected (row : property_grammar_row) value =
  (* Arbitrary substitutions defer property-grammar validation, including an
     otherwise-unexpected token beside the function. *)
  if not (property_value_has_substitution value) then
    match parse_property_decl row.property (value ^ " )") with
    | None -> ()
    | Some (input, serialized, _, _) ->
        Alcotest.failf
          "%s positive vector accepted an extra trailing token: %s -> %s"
          row.property input serialized

(* CSS Syntax 3 (ED) sec. 5.5.6 stops a declaration's value at the top-level [;]
   and lifts a trailing [!important] out of it into the important flag, so every
   vector the grammar accepts bare it has to accept with either tail behind it,
   and to the same value. *)
let check_property_value_tail (row : property_grammar_row) value =
  let bare = row.property ^ ":" ^ value in
  match parse_property_decl row.property value with
  | None -> Alcotest.failf "%s positive vector rejected: %s" row.property value
  | Some (_, _, decl, _) ->
      let expected = Css.Declaration.string_of_value ~minify:true decl in
      List.iter
        (fun (suffix, important) ->
          let input = bare ^ suffix in
          let c = Cursor.of_string input in
          match read_declaration c with
          | None ->
              Alcotest.failf "%s dropped the declaration: %s" row.property input
          | exception (Cursor.Parse_error _ | Reader.Parse_error _) ->
              Alcotest.failf "%s rejected the declaration: %s" row.property
                input
          | Some tailed ->
              Alcotest.(check string)
                (Fmt.str "%s holds the value of %S" input bare)
                expected
                (Css.Declaration.string_of_value ~minify:true tailed);
              Alcotest.(check bool)
                (Fmt.str "%s important flag" input)
                important
                (Css.Declaration.is_important tailed))
        [ (";", false); (" !important", true); (" !important;", true) ]

let check_property_name (row : property_grammar_row) =
  if String.trim row.property <> row.property then
    Alcotest.failf "%S has leading or trailing property-name whitespace"
      row.property;
  if row.property = "" then Alcotest.fail "empty property-name row";
  String.iter
    (function
      | 'a' .. 'z' | '0' .. '9' | '-' -> ()
      | c ->
          Alcotest.failf "%s contains non-CSS property-name character %C"
            row.property c)
    row.property

let check_branch_counts (row : property_grammar_row) =
  if row.positives = [] then
    Alcotest.failf "%s has no positive grammar vectors" row.property;
  if row.negatives = [] then
    Alcotest.failf "%s has no negative grammar vectors" row.property;
  if List.length row.positives < 2 then
    Alcotest.failf "%s needs at least two positive branch vectors" row.property;
  if List.length row.negatives < 2 then
    Alcotest.failf "%s needs at least two negative branch vectors" row.property

let manifest_normalize_value = String.trim

let check_no_branch_duplicates (row : property_grammar_row) =
  let duplicate_values values =
    let normalized = List.map manifest_normalize_value values in
    List.length normalized
    <> List.length (List.sort_uniq String.compare normalized)
  in
  if duplicate_values row.positives then
    Alcotest.failf "%s has duplicate positive grammar vectors" row.property;
  if duplicate_values row.negatives then
    Alcotest.failf "%s has duplicate negative grammar vectors" row.property

let check_no_positive_negative_overlap (row : property_grammar_row) =
  let positive_set =
    List.sort_uniq String.compare
      (List.map manifest_normalize_value row.positives)
  in
  List.iter
    (fun negative ->
      let normalized = manifest_normalize_value negative in
      if List.mem normalized positive_set then
        Alcotest.failf "%s grammar vector is both positive and negative: %S"
          row.property normalized)
    row.negatives

let check_no_empty_branches (row : property_grammar_row) =
  List.iter
    (fun value ->
      if String.trim value = "" then
        Alcotest.failf "%s has an empty grammar vector" row.property)
    (row.positives @ row.negatives)

let check_manifest_row_shape (row : property_grammar_row) =
  check_property_name row;
  check_branch_counts row;
  check_no_branch_duplicates row;
  check_no_positive_negative_overlap row;
  check_no_empty_branches row

let calc_expected_properties =
  [
    "width";
    "height";
    "margin";
    "padding";
    "border-radius";
    "opacity";
    "font-size";
    "line-height";
    "translate";
    "rotate";
    "scale";
    "transition";
    "animation";
    "filter";
    "background-position";
    "object-position";
    "transform-origin";
  ]

let check_manifest_calc_surface (row : property_grammar_row) =
  if List.mem row.property calc_expected_properties then
    let has_calc =
      List.exists
        (fun value ->
          List.exists
            (fun prefix -> String.starts_with ~prefix value)
            [ "calc("; "min("; "max("; "clamp("; "round("; "mod("; "rem(" ]
          || List.exists
               (fun needle -> Re.execp Re.(compile (str needle)) value)
               [ "calc("; "min("; "max("; "clamp("; "round("; "mod("; "rem(" ])
        row.positives
    in
    if not has_calc then
      Alcotest.failf
        "%s should include at least one function/calc-family grammar vector"
        row.property

let check_property_row (row : property_grammar_row) =
  check_manifest_row_shape row;
  check_manifest_calc_surface row;
  List.iter (check_property_positive row) row.positives;
  List.iter (check_property_trailing_token_rejected row) row.positives;
  List.iter (check_property_value_tail row) row.positives;
  List.iter (check_property_negative row) row.negatives;
  List.iter
    (check_property_css_wide row)
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  check_property_var row

let spec_property_grammar_manifest () =
  let unique_properties =
    List.sort_uniq String.compare
      (List.map
         (fun (row : property_grammar_row) -> row.property)
         property_grammar_matrix)
  in
  if List.length unique_properties <> List.length property_grammar_matrix then
    Alcotest.fail "property grammar manifest has duplicate property rows";
  Alcotest.(check int)
    "property grammar manifest covers every tracked spec property name" 455
    (List.length unique_properties);
  List.iter check_property_row property_grammar_matrix

let parse_declaration_case () =
  (* A known property parses to a typed declaration. *)
  (match parse_declaration "mask-type" "luminance" with
  | Some d ->
      Alcotest.(check string)
        "typed prop" "mask-type"
        (Css.Declaration.property_name d)
  | None -> Alcotest.fail "mask-type:luminance should parse");
  (* A known property's var() refs are visible to vars_of_declarations. *)
  (match parse_declaration "color" "var(--color-black)" with
  | Some d ->
      let names = List.map Css.any_var_name (Css.vars_of_declarations [ d ]) in
      Alcotest.(check bool)
        "typed var ref visible" true
        (List.mem "--color-black" names)
  | None -> Alcotest.fail "color:var(--color-black) should parse");
  (* A custom property parses (and keeps its component stream, not an opaque
     wrapper); it is dispatched as a custom property, not dropped. *)
  (match parse_declaration "--gradient-bg" "var(--color-black)" with
  | Some d ->
      Alcotest.(check (option string))
        "custom name" (Some "--gradient-bg")
        (Css.custom_declaration_name d)
  | None -> Alcotest.fail "--gradient-bg should parse");
  (* A layer attaches to a custom property. *)
  (match parse_declaration ~layer:"theme" "--c" "red" with
  | Some d ->
      Alcotest.(check (option string))
        "layer" (Some "theme")
        (custom_declaration_layer d)
  | None -> Alcotest.fail "--c should parse");
  (* An unparseable value yields None, not a recovered declaration. *)
  Alcotest.(check bool)
    "invalid value is None" true
    (parse_declaration "mask-type" "definitely-not-a-mask-type" = None)

(* CSS Scroll Snap 1 sec. 5.1: the [scroll-margin] longhands are [<length>],
   with no [0,inf] range and no "negative values are invalid" clause - that
   clause belongs to [scroll-padding] alone (sec. 4.2), and the CR changelog
   records the restriction as applying to [scroll-padding] only. Outsets may
   therefore be negative, exactly like [margin]. *)
let scroll_margin_negative () =
  let props =
    [
      "scroll-margin";
      "scroll-margin-top";
      "scroll-margin-right";
      "scroll-margin-bottom";
      "scroll-margin-left";
      "scroll-margin-inline";
      "scroll-margin-inline-start";
      "scroll-margin-inline-end";
      "scroll-margin-block";
      "scroll-margin-block-start";
      "scroll-margin-block-end";
    ]
  in
  List.iter
    (fun prop ->
      (* Minified (no space after the colon) and spaced spellings agree. *)
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-2vh")
        (prop ^ ":-2vh");
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-2vh")
        (prop ^ ": -2vh");
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-1px")
        (prop ^ ": -1px");
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-.5em")
        (prop ^ ": -.5em");
      check_declaration ~roundtrip:true
        ~expected:(prop ^ ":calc(-1px - 2px)")
        (prop ^ ": calc(-1px - 2px)");
      (* Percentages stay invalid: the longhands are [<length>], "Percentages:
         n/a". *)
      neg_cursor read_declaration (prop ^ ": -10%");
      neg_cursor read_declaration (prop ^ ": 10%"))
    props;
  (* Multi-value shorthand forms take a negative in any position. *)
  check_declaration ~roundtrip:true ~expected:"scroll-margin:-1px 2px"
    "scroll-margin:-1px 2px";
  check_declaration ~roundtrip:true ~expected:"scroll-margin:1px -2px"
    "scroll-margin: 1px -2px";
  check_declaration ~roundtrip:true
    ~expected:"scroll-margin:-1px -2px -3px -4px"
    "scroll-margin: -1px -2px -3px -4px";
  check_declaration ~roundtrip:true ~expected:"scroll-margin-block:-1px -2px"
    "scroll-margin-block: -1px -2px";
  check_declaration ~roundtrip:true ~expected:"scroll-margin-inline:-1px -2px"
    "scroll-margin-inline: -1px -2px";
  (* Control: [scroll-padding] is [auto | <length-percentage>] with "Negative
     values are invalid", so its longhands keep rejecting them. *)
  List.iter
    (fun prop ->
      neg_cursor read_declaration (prop ^ ":-2vh");
      neg_cursor read_declaration (prop ^ ": -2vh"))
    [
      "scroll-padding";
      "scroll-padding-top";
      "scroll-padding-inline";
      "scroll-padding-block-end";
    ]

(* The whole-sheet path must accept the same values without recovering: a
   swallowed warning is what hid this from [Css.of_string] callers. *)
let scroll_margin_negative_sheet () =
  List.iter
    (fun css ->
      match Css.of_string ~strict:true css with
      | Ok { stylesheet; _ } ->
          Alcotest.(check string)
            "scroll-margin sheet roundtrip" css
            (String.trim (Css.to_string ~minify:true stylesheet))
      | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e))
    [
      "a{scroll-margin:-2vh}";
      "a{scroll-margin:-1px 2px}";
      "a{scroll-margin-top:-2vh}";
      "a{scroll-margin-block:-1px -2px}";
      "a{scroll-margin-inline-start:-1px}";
    ]

(* A declaration the reader rejects is dropped from the sheet with nothing but a
   warning, so every reader gap needs a whole-sheet pin too. *)
let check_sheet_roundtrip name css =
  match Css.of_string ~strict:true css with
  | Ok { stylesheet; _ } ->
      Alcotest.(check string)
        (name ^ " sheet roundtrip")
        css
        (String.trim (Css.to_string ~minify:true stylesheet))
  | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)

(* CSS Text Decoration 4 sec. 2.6 joins the line, thickness, style and colour
   components with [||], so no single component is mandatory. *)
let text_decoration_thickness_range () =
  (* CSS Text Decoration 4 section 2.4: <length-percentage> has no non-negative
     grammar range. The minimum device-pixel thickness is a rendering rule. *)
  List.iter
    (fun (value, printed) ->
      check_declaration ~roundtrip:true
        ~expected:("text-decoration-thickness:" ^ printed)
        ("text-decoration-thickness:" ^ value))
    [
      ("-1px", "-1px");
      ("-10%", "-10%");
      ("-.25em", "-.25em");
      ("calc(-1px)", "-1px");
      ("calc(-10%)", "-10%");
      ("calc(2px - 3px)", "calc(2px - 3px)");
      ("min(-1px,2px)", "min(-1px,2px)");
    ];
  check_declaration ~roundtrip:true "text-decoration:underline -1px";
  none_cursor read_declaration "text-decoration-thickness:-1s"

let text_decoration_optional_line () =
  check_declaration ~roundtrip:true "text-decoration:red";
  check_sheet_roundtrip "text-decoration" "a{text-decoration:red}"

(* CSS Text Decoration 4 (ED) sec. 2.6 sets an omitted shorthand slot to its
   initial value - [solid] (sec. 2.2) and [currentcolor] (sec. 2.3) - so a slot
   written with that value says what leaving it out says. Written alone the slot
   is all there is, and dropping it drains the shorthand: what is left declares
   nothing but the four initials, which is what [none] declares. *)
let text_decoration_drained_shorthand () =
  check_declaration ~expected:"text-decoration:solid"
    ~optimized:"text-decoration:none" "text-decoration: solid";
  check_declaration ~expected:"text-decoration:currentColor"
    ~optimized:"text-decoration:none" "text-decoration: currentcolor";
  check_declaration ~expected:"text-decoration:solid currentColor"
    ~optimized:"text-decoration:none" "text-decoration: solid currentcolor";
  check_sheet_roundtrip "text-decoration solid" "a{text-decoration:solid}";
  (* The drop still runs wherever a slot outlives it. *)
  check_declaration ~expected:"text-decoration:solid red"
    ~optimized:"text-decoration:red" "text-decoration: solid red";
  check_declaration ~expected:"text-decoration:underline solid"
    ~optimized:"text-decoration:underline" "text-decoration: underline solid";
  check_declaration ~expected:"text-decoration:underline currentColor"
    ~optimized:"text-decoration:underline"
    "text-decoration: underline currentcolor";
  check_declaration ~expected:"text-decoration:solid 2px"
    ~optimized:"text-decoration:2px" "text-decoration: solid 2px";
  check_declaration ~expected:"text-decoration:none"
    ~optimized:"text-decoration:none" "text-decoration: none"

(* CSS Text 4 sec. 3 allows each longhand component of [white-space] on its own;
   omitted components take their initial values. *)
let white_space_collapse_only () =
  check_declaration ~roundtrip:true "white-space:collapse";
  check_sheet_roundtrip "white-space" "a{white-space:collapse}"

(* CSS Backgrounds 3 sec. 5.7 sets omitted border-image shorthand slots to their
   initial values, so the repeat component is valid on its own. *)
let border_image_repeat_only () =
  check_declaration ~roundtrip:true "border-image:round";
  check_sheet_roundtrip "border-image" "a{border-image:round}"

(* Scroll-driven Animations 1 sec. 2.3.3 and 3.4.4 make the axis optional in
   both named timeline shorthands. *)
let timeline_name_only () =
  List.iter
    (fun (property, name) ->
      let declaration = String.concat "" [ property; ":"; name ] in
      check_declaration ~roundtrip:true declaration;
      check_sheet_roundtrip property
        (String.concat "" [ "a{"; declaration; "}" ]))
    [ ("scroll-timeline", "--t"); ("view-timeline", "--v") ]

(* Scroll-driven Animations 1 sec. 3.4.4 includes the optional
   view-timeline-inset slot in each view-timeline shorthand item. *)
let view_timeline_inset_slot () =
  check_declaration ~roundtrip:true "view-timeline:--v 10% 20%";
  check_sheet_roundtrip "view-timeline" "a{view-timeline:--v 10% 20%}"

(* CSS Inline 3 sec. 6.1 joins text-box-trim and text-box-edge with [||], so the
   edge is valid without an explicit trim value. *)
let text_box_edge_only () =
  check_declaration ~roundtrip:true "text-box:cap alphabetic";
  check_sheet_roundtrip "text-box" "a{text-box:cap alphabetic}"

(* CSS Inline 3 sec. 6.1 gives [normal] its own text-box shorthand branch. *)
let text_box_normal () =
  check_declaration ~roundtrip:true "text-box:normal";
  check_sheet_roundtrip "text-box" "a{text-box:normal}"

(* CSS Values 4 sec. 8.3 allows four edge-offset components but excludes the
   three-value form. CSS Backgrounds 3 sec. 2.6 retains valid three-value
   <bg-position>s, while CSS Transforms 1 sec. 4 gives transform-origin a
   narrower grammar with an optional Z length after a two-value origin. *)
let edge_offset_position_grammar () =
  List.iter
    (fun (declaration, expected) ->
      check_declaration ~expected ~roundtrip:true declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      let expected_css = String.concat "" [ "a{"; expected; "}" ] in
      match Css.of_string ~strict:true css with
      | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)
      | Ok { stylesheet; _ } ->
          Alcotest.(check string)
            "edge-offset position sheet roundtrip" expected_css
            (String.trim (Css.to_string ~minify:true stylesheet)))
    [
      ("background-position:left top 10px", "background-position:left top 10px");
      ("background-position:left top 10%", "background-position:left top 10%");
      ( "background-position:center top 10px",
        "background-position:center top 10px" );
      ( "background-position:center left 10px",
        "background-position:center left 10px" );
      ("background-position:top 10px left", "background-position:top 10px left");
      ( "perspective-origin:left 10px top 20px",
        "perspective-origin:left 10px top 20px" );
      ("transform-origin:left 10px", "transform-origin:0% 10px");
      ("transform-origin:left top 10px", "transform-origin:left top 10px");
      ("transform-origin:center top 10px", "transform-origin:center top 10px");
    ];
  List.iter
    (fun declaration ->
      none_cursor read_declaration declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      match Css.of_string ~strict:true css with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "strict parsing accepted %s" css)
    [
      "transform-origin:foo 1px bar 2px";
      "transform-origin:left 1px top 2px";
      "transform-origin:right 10% bottom 20%";
      "transform-origin:top 2px left 1px";
      "transform-origin:left 1px top 2px 3px";
      "background-position:left 1px right 2px";
      "object-position:left 1px middle";
      "perspective-origin:top 1px bottom";
      "object-position:left 1px top";
      "perspective-origin:left top 1px";
    ]

(* Motion Path 1 secs. 2.3-2.4 define offset-position as [normal | auto |
   <position>] and offset-anchor as [auto | <position>]. Both use the generic
   <position> grammar, not background's three-value extension. *)
let offset_position_properties () =
  List.iter
    (fun (declaration, expected) ->
      check_declaration ~expected ~roundtrip:true declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      let expected_css = String.concat "" [ "a{"; expected; "}" ] in
      match Css.of_string ~strict:true css with
      | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)
      | Ok { stylesheet; _ } ->
          Alcotest.(check string)
            "offset position property sheet roundtrip" expected_css
            (String.trim (Css.to_string ~minify:true stylesheet)))
    [
      ("offset-anchor:auto", "offset-anchor:auto");
      ("offset-anchor:center", "offset-anchor:center");
      ("offset-anchor:left top", "offset-anchor:left top");
      ("offset-anchor:20% 30%", "offset-anchor:20%30%");
      ("offset-anchor:left 10px top 20px", "offset-anchor:left 10px top 20px");
      ("offset-position:normal", "offset-position:normal");
      ("offset-position:auto", "offset-position:auto");
      ("offset-position:center", "offset-position:center");
      ("offset-position:left top", "offset-position:left top");
      ("offset-position:20% 30%", "offset-position:20%30%");
      ( "offset-position:left 10px top 20px",
        "offset-position:left 10px top 20px" );
    ];
  decl_optimizes ~prop:"offset-anchor" ~into:"50%" "center";
  decl_optimizes ~prop:"offset-anchor" ~into:"0 0" "left top";
  decl_optimizes ~prop:"offset-anchor" ~into:"auto" "initial";
  decl_optimizes ~prop:"offset-position" ~into:"50%" "center";
  decl_optimizes ~prop:"offset-position" ~into:"0 0" "left top";
  decl_optimizes ~prop:"offset-position" ~into:"normal" "initial";
  List.iter
    (fun declaration ->
      none_cursor read_declaration declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      match Css.of_string ~strict:true css with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "strict parsing accepted %s" css)
    [
      "offset-anchor:foo bar baz";
      "offset-anchor:normal";
      "offset-anchor:size";
      "offset-anchor:auto center";
      "offset-anchor:left top 10px";
      "offset-position:foo bar baz";
      "offset-position:none";
      "offset-position:normal center";
      "offset-position:left top 10px";
      "object-position:normal";
    ]

(* CSS Syntax 3 (ED) sec. 5.5.6 "consume a declaration" reads the value with
   [<semicolon-token>] as the stop token, then removes a trailing [!]
   [important] pair from that value and sets the declaration's important flag
   instead. A property grammar therefore never sees a [;] or an [!important],
   and neither may change what the value in front of it parses to. *)
let value_tail_forms property value =
  let bare = String.concat "" [ property; ":"; value ] in
  [
    (bare, false);
    (bare ^ ";", false);
    (bare ^ " !important", true);
    (bare ^ "!important", true);
    (bare ^ " !important;", true);
    (bare ^ " !IMPORTANT", true);
  ]

let read_one_declaration what input =
  let c = Cursor.of_string input in
  match read_declaration c with
  | Some decl -> decl
  | None -> Alcotest.failf "%s: %S read no declaration" what input
  | exception Error.Parse_error e ->
      Alcotest.failf "%s: %S was rejected: %s" what input (Error.to_string e)

(* The value a declaration holds, and its important flag, are independent of the
   tail the declaration consumer strips. Compared against the tail-free spelling
   rather than a pinned string, so the invariant is what is asserted. *)
let check_value_end property value =
  let bare = String.concat "" [ property; ":"; value ] in
  let expected =
    Css.Declaration.string_of_value ~minify:true
      (read_one_declaration "value end" bare)
  in
  List.iter
    (fun (input, important) ->
      let decl = read_one_declaration "value end" input in
      Alcotest.(check string)
        (Fmt.str "%S holds the value of %S" input bare)
        expected
        (Css.Declaration.string_of_value ~minify:true decl);
      Alcotest.(check bool)
        (Fmt.str "%S important flag" input)
        important
        (Css.Declaration.is_important decl);
      (* A declaration cascade prints has to read back as itself: the minified
         spelling of an important declaration ends in [!important], which is the
         very tail the reader has to see past. *)
      let printed = Css.Declaration.to_string ~minify:true decl in
      Alcotest.(check string)
        (Fmt.str "%S reparses" printed)
        printed
        (Css.Declaration.to_string ~minify:true
           (read_one_declaration "value end reparse" printed)))
    (value_tail_forms property value)

(* One reader per spelling of the end-of-value question that was open-coded:
   readers that ended on cursor exhaustion alone dropped the declaration over a
   [;] as well as over an [!important], readers that also peeked for a [;]
   dropped it over an [!important] only. Each entry is a property whose grammar
   has an optional trailing component, so the reader has to ask the question at
   all. *)
let value_end_properties =
  [
    ("font-style", "oblique");
    ("caret", "red");
    ("color-scheme", "dark");
    ("rotate", "45deg");
    ("clip-path", "border-box");
    ("animation-range", "normal");
    ("contain-intrinsic-size", "auto 300px");
    ("text-box", "none");
    ("hyphenate-limit-chars", "6");
    ("initial-letter", "2 3");
    ("justify-items", "legacy");
    ("overflow-clip-margin", "1px");
    ("grid-row", "span 2");
    ("border-image-slice", "30%");
    ("size", "A4");
    ("mask-border-slice", "30%");
    ("column-rule", "1px");
    ("text-emphasis", "filled");
    ("scale", "2");
    ("transform-origin", "left");
    (* Controls: readers that already spelled the question with all three of its
       answers, and a value with no optional tail at all. *)
    ("overflow", "hidden");
    ("color", "red");
  ]

let declaration_value_end () =
  List.iter
    (fun (property, value) -> check_value_end property value)
    value_end_properties

(* The pretty spelling of a declaration ends in a [;], so a reader that stops at
   the value's last component and then fails on the [;] cannot read back what
   cascade itself wrote. *)
let declaration_value_end_sheet () =
  List.iter
    (fun (property, value) ->
      List.iter
        (fun (decl, _) ->
          let css = String.concat "" [ "x{"; decl; "}" ] in
          match Css.of_string ~strict:true css with
          | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)
          | Ok { stylesheet; _ } -> (
              let minified =
                String.trim (Css.to_string ~minify:true stylesheet)
              in
              let pretty = Css.to_string ~minify:false stylesheet in
              match Css.of_string ~strict:true pretty with
              | Error e ->
                  Alcotest.failf "reparse of %S: %s" pretty (Error.to_string e)
              | Ok p ->
                  Alcotest.(check string)
                    (Fmt.str "%S survives a pretty round trip" css)
                    minified
                    (String.trim (Css.to_string ~minify:true p.stylesheet))))
        (value_tail_forms property value))
    value_end_properties

(* Seeing past the tail is not licence to accept it: a [!] that does not open
   [!important], a second [!important], a value component after one, and a value
   with more components than the grammar has slots all stay invalid. *)
let declaration_value_end_negatives () =
  List.iter
    (neg_cursor read_declaration)
    [
      "color:red !foo";
      "color:red !important !important";
      "color:red !important blue";
      "font-style:oblique !";
      "text-box:none !foo";
      "rotate:45deg 45deg 45deg 45deg";
      "rotate:45deg 45deg !important";
      "initial-letter:2 3 4";
      "hyphenate-limit-chars:2 3 4 5";
      "font-style:oblique 20deg 30deg 40deg";
      (* CSS Sizing 4 sec. 5.2 spells [contain-intrinsic-size] as [[ auto? [
         none | <length [0,inf]> ] ]{1,2}], so a third slot is one too many. *)
      "contain-intrinsic-size:auto 300px 400px 500px";
      "animation-range:normal normal normal";
    ]

(* A cursor over a function's arguments is asking a different question: its
   grammar ends at the closing paren, where neither a [;] nor an [!] can stand,
   so it must still be read to true end of input. Widening it would accept an
   argument list longer than the function takes. *)
let function_argument_end_negatives () =
  List.iter
    (neg_cursor read_declaration)
    [
      "transform:skew(10deg,red)";
      "transform:matrix(1,2,3,4,5,6,7)";
      "transform:translate(1px,2px,3px)";
      "clip-path:inset(1px 2px 3px 4px 5px)";
      "transform:rotate(45deg,45deg)";
      "background:linear-gradient(red,blue) extra";
    ]

(* CSS Shapes 1 sec. 6.1: [shape-outside] is [none | [<basic-shape> ||
   <shape-box>] | <image>]. The reader only looked at the first component and
   only knew [none], [circle()] and [inset()] there, so a reference box, a
   box/shape pair, the other basic shapes and an image were all rejected and the
   declaration dropped. *)
let shape_outside_grammar () =
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [
      "shape-outside:none";
      "shape-outside:inherit";
      "shape-outside:var(--shape)";
      (* <basic-shape> *)
      "shape-outside:circle(50%)";
      "shape-outside:circle()";
      "shape-outside:circle(50% at 20% 30%)";
      "shape-outside:ellipse(closest-side farthest-side at 10px 20px)";
      "shape-outside:inset(10px round 2px)";
      "shape-outside:polygon(0 0,100% 0,100% 100%)";
      "shape-outside:xywh(0 0 100% 100%)";
      (* <shape-box> on its own, and either order in the [||] pair *)
      "shape-outside:margin-box";
      "shape-outside:content-box";
      "shape-outside:circle() border-box";
      "shape-outside:padding-box circle(50%)";
      (* <image> *)
      "shape-outside:url(shape.png)";
      "shape-outside:linear-gradient(red,blue)";
    ];
  (* Controls: an unknown keyword is no part of the grammar, [none] and a box
     are alternatives rather than a [||] pair, the box appears once, and
     [<shape-box>] excludes the three SVG boxes [<geometry-box>] adds. *)
  List.iter
    (neg_cursor read_declaration)
    [
      "shape-outside:not-a-shape";
      "shape-outside:none margin-box";
      "shape-outside:margin-box border-box";
      "shape-outside:circle(50%) fill-box";
    ]

let shape_outside_sheet () =
  List.iter
    (check_sheet_roundtrip "shape-outside")
    [
      "a{shape-outside:none}";
      "a{shape-outside:margin-box}";
      "a{shape-outside:circle(50%) content-box}";
      "a{shape-outside:url(shape.png)}";
    ]

(* Every property that reads a [<line-width>]: the four physical longhands and
   their logical counterparts, the 1-4 value shorthand, the two-value logical
   shorthands, and the width slot of [border] and of [outline]. *)
let line_width_sites value =
  List.map
    (fun property -> String.concat "" [ property; ":"; value ])
    [
      "border-width";
      "border-top-width";
      "border-right-width";
      "border-bottom-width";
      "border-left-width";
      "border-block-start-width";
      "border-block-end-width";
      "border-inline-start-width";
      "border-inline-end-width";
      "border-block-width";
      "border-inline-width";
      "border";
      "outline-width";
      "outline";
    ]

(* A [<length>] site reading the same comparison, for contrast. *)
let length_math_sites value =
  List.map
    (fun property -> String.concat "" [ property; ":"; value ])
    [ "margin"; "margin-top"; "width" ]

(* CSS Values 4 sec. 10.2 gives [min()] / [max()] a comma-separated list of
   [<calc-sum>] and [clamp()] exactly three arguments, and CSS Syntax 3 sec. 8.2
   drops a declaration whose value is invalid. The [<length>] sites answer that
   way already. The [<line-width>] sites read arguments up to the first one they
   could not read and compared those, so [max(1px,red)] answered [1px]: a
   narrower comparison than the one written, and one the author never asked
   for. *)
let line_width_invalid_math_argument () =
  List.iter
    (fun value ->
      List.iter (neg_cursor read_declaration) (line_width_sites value);
      List.iter (neg_cursor read_declaration) (length_math_sites value))
    [
      "max(1px,red)";
      "min(3px,red,1px)";
      "max(1px 2px)";
      "clamp(1px,2px,3px,4px)";
    ];
  (* Controls: two bounds no unit relates stand as written at every site, and
     the 1-4 value shorthand still takes one per side. *)
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    (line_width_sites "max(3dvh,4px)" @ length_math_sites "max(3dvh,4px)");
  check_declaration ~roundtrip:true "border-width:1px max(3dvh,4px)"

(* [skew()] (CSS Transforms 1 sec. 13.2) takes one or two [<angle>], [matrix()]
   (sec. 12.1) exactly six [<number>], [matrix3d()] (CSS Transforms 2 sec. 12.2)
   exactly sixteen, and [repeat()] (CSS Grid 1 sec. 7.2.3) a count and a track
   list; a trailing argument outside that grammar is invalid (CSS Syntax 3 sec.
   8.2), which invalidates the declaration. The same [Cursor.call] gap #617
   closed for the [<line-width>] readers left these four reading only up to the
   first argument they could not read and answering with a truncated function,
   e.g. [transform: skew(10deg, red)] as [skew(10deg)]. *)
let function_argument_validity () =
  List.iter
    (neg_cursor read_declaration)
    [
      "transform:skew(10deg,red)";
      "transform:matrix(1,2,3,4,5,6,red)";
      "transform:matrix3d(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,red)";
      "grid-template-columns:repeat(2,1fr red)";
      "grid-template-columns:repeat(2,1fr,red)";
    ];
  (* Controls: a valid call at every site still stands, including interior
     whitespace around commas and parens. *)
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [
      "transform:skew(10deg)";
      "transform:skew(10deg,20deg)";
      "transform:matrix(1,2,3,4,5,6)";
      "transform:matrix3d(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)";
      "grid-template-columns:repeat(2,1fr)";
    ];
  check_declaration ~expected:"transform:skew(10deg,20deg)"
    "transform:skew( 10deg , 20deg )"

(* CSS list productions are non-empty unless their grammar says otherwise.
   Backgrounds 3 gives both longhands a comma-separated item list, CSS Values 4
   gives [hypot()] one or more calculations, and Grid 2 gives [repeat()] a
   positive explicit count followed by one or more tracks. *)
let non_empty_list_grammar () =
  List.iter
    (neg_cursor read_declaration)
    [
      "background-image:";
      "background-blend-mode:";
      "width:hypot()";
      "grid-template-columns:repeat(2,)";
      "grid-template-columns:repeat(0,1px)";
      "grid-template-columns:repeat(-2,1px)";
    ];
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [
      "background-image:none";
      "background-blend-mode:normal";
      "width:hypot(3px,4px)";
      "grid-template-columns:repeat(2,1px)";
    ];
  (* Grid 2 defines a line-name block with a [*] multiplier, so this is the one
     audited grammar list that deliberately remains empty. *)
  check_declaration ~expected:"grid-template-columns:[]1px"
    "grid-template-columns:[] 1px"

(* CSS Values 4 sec. 5.7.3: a [#] multiplier's comma never trails the last item.
   [min()]/[max()] take a comma-separated [<calc-sum>] list (sec. 10.2), and
   [matrix()]/[matrix3d()] (CSS Transforms 1 sec. 12.1, 2 sec. 12.2) a
   fixed-arity comma list of [<number>]; [list] committed a separator before it
   knew whether another item followed it, so [min(1px,)] read as [min(1px)] and
   [matrix(1,2,3,4,5,6,)] as [matrix(1,2,3,4,5,6)] instead of invalidating the
   declaration (CSS Syntax 3 sec. 8.2). *)
let list_trailing_separator_invalid () =
  List.iter
    (fun value ->
      List.iter (neg_cursor read_declaration) (line_width_sites value);
      List.iter (neg_cursor read_declaration) (length_math_sites value))
    [ "min(1px,)"; "min(1px,2px,)"; "max(1px,2px,)" ];
  List.iter
    (neg_cursor read_declaration)
    [
      "transform:matrix(1,2,3,4,5,6,)";
      "transform:matrix3d(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,)";
    ];
  (* Controls: no trailing separator, with and without interior whitespace, and
     a single-item list, all still parse. *)
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    (line_width_sites "min(1px)"
    @ length_math_sites "min(1px)"
    @ line_width_sites "min(1px,2px)"
    @ length_math_sites "min(1px,2px)");
  check_declaration ~expected:"border-width:min(1px,2px)"
    "border-width:min( 1px , 2px )";
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [ "transform:matrix(1,2,3,4,5,6)" ];
  check_declaration ~expected:"transform:matrix(1,2,3,4,5,6)"
    "transform:matrix( 1 , 2 , 3 , 4 , 5 , 6 )"

(* CSS Backgrounds 3 sec. 5.1: [border-radius] is [<length-percentage
   [0,inf]>{1,4} [ / <length-percentage [0,inf]>{1,4} ]?], so an
   intrinsic-sizing keyword ([auto], [max-content], [stretch], ...) is no part
   of it. CSS Shapes 1 sec. 3.1 spells the rounded-corner suffix of a basic
   shape [round <'border-radius'>], and [<'property'>] (CSS Values 4 sec. 2.2)
   names that same value definition, so the keywords stay out there too. Chrome
   151 and WebKit 26.5 drop every declaration below. *)
let radius_keywords =
  [
    "auto";
    "none";
    "normal";
    "size";
    "max-content";
    "min-content";
    "fit-content";
    "-webkit-max-content";
    "-webkit-min-content";
    "-webkit-fit-content";
    "-moz-max-content";
    "-moz-min-content";
    "-moz-fit-content";
    "contain";
    "stretch";
    "from-font";
  ]

let radius_shorthand_sites radius =
  List.map
    (fun property -> String.concat "" [ property; ":"; radius ])
    [ "border-radius"; "-webkit-border-radius"; "-moz-border-radius" ]

(* Every [round <'border-radius'>] site: the basic shapes behind [clip-path] and
   [shape-outside], and the [rect()] / [xywh()] of [object-view-box]. *)
let radius_nested_sites radius =
  List.map
    (fun (before, after) -> String.concat "" [ before; radius; after ])
    [
      ("clip-path:inset(0 round ", ")");
      ("clip-path:rect(0 1px 1px 0 round ", ")");
      ("clip-path:xywh(0 0 1px 1px round ", ")");
      ("shape-outside:inset(0 round ", ")");
      ("object-view-box:rect(0 1px 1px 0 round ", ")");
      ("object-view-box:xywh(0 0 1px 1px round ", ")");
    ]

let border_radius_keyword_radii () =
  List.iter
    (fun radius ->
      List.iter (neg_cursor read_declaration) (radius_shorthand_sites radius);
      List.iter (neg_cursor read_declaration) (radius_nested_sites radius))
    radius_keywords;
  (* Controls: a length and a percentage are radii everywhere, a negative one is
     a radius nowhere. *)
  List.iter
    (fun radius ->
      List.iter
        (fun css -> check_declaration ~roundtrip:true css)
        (radius_shorthand_sites radius @ radius_nested_sites radius))
    [ "10px"; "10%" ];
  List.iter (neg_cursor read_declaration) (radius_shorthand_sites "-5px");
  List.iter (neg_cursor read_declaration) (radius_nested_sites "-5px")

(* CSS Cascade 5 sec. 6: a CSS-wide keyword is only valid "as the entire
   property value", so it is a whole [border-radius] and never one radius inside
   a basic shape. Chrome 151 and WebKit 26.5 keep the shorthand and drop the
   nested form. *)
let border_radius_css_wide_keywords () =
  List.iter
    (fun radius ->
      List.iter
        (fun css -> check_declaration ~roundtrip:true css)
        (radius_shorthand_sites radius);
      List.iter (neg_cursor read_declaration) (radius_nested_sites radius))
    [ "inherit"; "initial"; "unset"; "revert"; "revert-layer" ]

(* Vendor-prefixed properties with a typed constructor need a reader arm as
   well: without one the dispatch falls through to "unsupported property reader"
   and the declaration is dropped. Each takes the same value type as the
   unprefixed property, so each reads with the unprefixed reader. *)
let vendor_prefixed_shorthands () =
  let values =
    [
      ("animation", "spin 1s");
      ("animation-delay", "1s");
      ("animation-direction", "reverse");
      ("animation-duration", "2s");
      ("animation-fill-mode", "forwards");
      ("animation-iteration-count", "infinite");
      ("animation-name", "spin");
      ("animation-play-state", "paused");
      ("animation-timing-function", "ease-in");
      ("transition", "all .3s");
      ("transition-delay", "1s");
      ("transition-duration", "2s");
      ("transition-property", "opacity");
      ("transition-timing-function", "linear");
      ("border-radius", "4px");
      ("box-shadow", "0 1px 2px #000");
      ("background-size", "cover");
      ("flex-direction", "column");
      ("flex-flow", "row wrap");
      ("flex-wrap", "wrap");
      ("justify-content", "space-between");
      ("align-content", "center");
      ("align-items", "center");
      ("align-self", "flex-end");
    ]
  in
  let prefixed prefix names =
    List.filter_map
      (fun (name, value) ->
        if List.mem name names then Some (prefix ^ name ^ ":" ^ value) else None)
      values
  in
  let cases =
    prefixed "-webkit-"
      [
        "animation-delay";
        "animation-direction";
        "animation-duration";
        "animation-fill-mode";
        "animation-iteration-count";
        "animation-name";
        "animation-play-state";
        "animation-timing-function";
        "transition-delay";
        "transition-duration";
        "transition-property";
        "transition-timing-function";
        "border-radius";
        "box-shadow";
        "background-size";
        "flex-direction";
        "flex-flow";
        "flex-wrap";
        "justify-content";
        "align-content";
        "align-items";
        "align-self";
      ]
    @ prefixed "-moz-"
        [
          "animation";
          "animation-delay";
          "animation-direction";
          "animation-duration";
          "animation-fill-mode";
          "animation-iteration-count";
          "animation-name";
          "animation-play-state";
          "animation-timing-function";
          "transition";
          "transition-delay";
          "transition-duration";
          "transition-property";
          "transition-timing-function";
          "border-radius";
          "box-shadow";
        ]
  in
  List.iter (fun css -> check_declaration ~roundtrip:true css) cases;
  List.iter
    (fun css -> check_sheet_roundtrip "vendor prefix" ("a{" ^ css ^ "}"))
    cases

(* CSS Values 4 sec. 10.7.2 makes NaN a keyword of the <number> grammar that
   resolves at parse time, and sec. 10.13 serialises every NaN-valued
   calculation as calc(NaN): CSS has one NaN and one spelling for it, so two
   declarations that spell it are one declaration. The cached [Declaration.hash]
   already answers that way, and [Declaration.same_minified] merges rules on
   [hash a = hash b && equal_declaration a b], so the two have to agree or a
   merge is lost. *)
let optimized_declarations css =
  Css.of_string_exn css |> Css.optimize |> Css.statements
  |> List.concat_map (function
    | Css.Stylesheet.Rule r -> r.Css.Stylesheet.declarations
    | _ -> [])

let sole_declaration css =
  match optimized_declarations css with
  | [ d ] -> d
  | l ->
      Alcotest.failf "%s: expected one declaration, got %d" css (List.length l)

let minified css =
  Css.to_string ~minify:true (Css.optimize (Css.of_string_exn css))

let nan_declaration_is_one_value () =
  (* Parsed apart so the two are distinct heap blocks: a physical-equality
     short-circuit must not stand in for the answer. *)
  let a = sole_declaration ".a{opacity:calc(infinity - infinity)}" in
  let b = sole_declaration ".b{opacity:calc(infinity - infinity)}" in
  Alcotest.(check string)
    "the fold spells calc(NaN)" "opacity:calc(NaN)"
    (Css.Declaration.to_string ~minify:true a);
  Alcotest.(check int)
    "hash reads the two as one value" (Css.Declaration.hash a)
    (Css.Declaration.hash b);
  Alcotest.(check bool)
    "a NaN declaration equals itself" true
    (Css.Declaration.equal_declaration a a);
  Alcotest.(check bool)
    "two NaN declarations are equal" true
    (Css.Declaration.equal_declaration a b);
  (* An ordinary float still answers both ways. *)
  let half = sole_declaration ".c{opacity:.5}" in
  let half' = sole_declaration ".d{opacity:.5}" in
  let other = sole_declaration ".e{opacity:.6}" in
  Alcotest.(check bool)
    "equal floats are equal" true
    (Css.Declaration.equal_declaration half half');
  Alcotest.(check bool)
    "different floats are not" false
    (Css.Declaration.equal_declaration half other);
  (* What the disagreement costs: the NaN pair is left unmerged while the
     ordinary-float control merges. *)
  Alcotest.(check string)
    "the NaN pair merges" ".a,.b{opacity:calc(NaN)}"
    (minified
       ".a{opacity:calc(infinity - infinity)}.b{opacity:calc(infinity - \
        infinity)}");
  Alcotest.(check string)
    "the float control merges" ".c,.d{opacity:.5}"
    (minified ".c{opacity:.5}.d{opacity:.5}")

(* CSS Values 4 sec. 10.7.2 gives NaN one spelling and one resolution: it is a
   keyword of the <number> grammar that resolves at parse time, so a NaN reaches
   the AST as that keyword rather than as a float a calculation happened to land
   on. Sec. 10.13 serialises every NaN-valued calculation the same way,
   calc(NaN) for a <number> and calc(NaN * 1px) once the value carries a unit.
   One value, one node: however the source spelled the NaN, the declaration that
   holds it is the same declaration, so it merges and it hashes the same.

   infinity and -infinity are separate constants of the same sec. 10.7.2 list,
   each with its own serialisation, so they stay three distinct values. *)
let same_text_same_hash label a b =
  let text = Css.Declaration.to_string ~minify:true a in
  Alcotest.(check string)
    (label ^ ": one minified text")
    text
    (Css.Declaration.to_string ~minify:true b);
  Alcotest.(check int)
    (label ^ ": one hash") (Css.Declaration.hash a) (Css.Declaration.hash b);
  Alcotest.(check bool)
    (label ^ ": one declaration")
    true
    (Css.Declaration.equal_declaration a b)

let distinct_value label a b =
  Alcotest.(check bool)
    (label ^ ": two texts") false
    (String.equal
       (Css.Declaration.to_string ~minify:true a)
       (Css.Declaration.to_string ~minify:true b));
  Alcotest.(check bool)
    (label ^ ": two declarations")
    false
    (Css.Declaration.equal_declaration a b)

let aspect_ratio_has_one_node () =
  let parsed = Css.Declaration.of_string "aspect-ratio:16/9" in
  same_text_same_hash "Ratio constructor vs parsed"
    (Css.aspect_ratio (Css.Ratio (16., 9.)))
    parsed;
  same_text_same_hash "ratio helper vs parsed"
    (Css.aspect_ratio (Css.ratio 16. 9.))
    parsed;
  same_text_same_hash "Auto_ratio constructor vs parsed"
    (Css.aspect_ratio (Css.Auto_ratio (16., 9.)))
    (Css.Declaration.of_string "aspect-ratio:auto 16/9")

let caret_auto_has_one_node () =
  same_text_same_hash "caret Auto constructor vs parsed"
    (Css.caret (Css.Auto : Css.caret))
    (Css.Declaration.of_string "caret:auto")

let gradient_var kind constructor =
  let var =
    Css.Values.read_var Css.Properties.read_gradient_stop
      (Cursor.of_string "var(--stops)")
  in
  same_text_same_hash
    (kind ^ " var constructor vs parsed")
    (Css.background_image (constructor var))
    (Css.Declaration.of_string ("background-image:" ^ kind ^ "(var(--stops))"))

let radial_gradient_var_has_one_node () =
  gradient_var "radial-gradient" (fun var -> Css.Radial_gradient_var var)

let conic_gradient_var_has_one_node () =
  gradient_var "conic-gradient" (fun var -> Css.Conic_gradient_var var)

let nan_has_one_node () =
  (* The keyword and the calculation that lands on NaN are the same value. *)
  let keyword = sole_declaration ".a{opacity:calc(NaN)}" in
  let computed = sole_declaration ".b{opacity:calc(infinity - infinity)}" in
  Alcotest.(check string)
    "the keyword spells calc(NaN)" "opacity:calc(NaN)"
    (Css.Declaration.to_string ~minify:true keyword);
  same_text_same_hash "keyword vs computed" keyword computed;
  Alcotest.(check string)
    "the two spellings merge" ".a,.b{opacity:calc(NaN)}"
    (minified ".a{opacity:calc(NaN)}.b{opacity:calc(infinity - infinity)}");
  (* Spellings a calculation keeps rather than folds stay one value each. *)
  Alcotest.(check string)
    "calc(0/0) merges" ".a,.b{opacity:calc(0/0)}"
    (minified ".a{opacity:calc(0/0)}.b{opacity:calc(0/0)}");
  Alcotest.(check string)
    "calc(asin(2)) merges" ".a,.b{opacity:calc(asin(2))}"
    (minified ".a{opacity:calc(asin(2))}.b{opacity:calc(asin(2))}");
  Alcotest.(check string)
    "a NaN length merges" ".a,.b{width:calc(NaN*1px)}"
    (minified ".a{width:calc(NaN*1px)}.b{width:calc(NaN*1px)}");
  Alcotest.(check string)
    "a NaN duration merges" ".a,.b{transition-duration:calc(NaN*1s)}"
    (minified
       ".a{transition-duration:calc(NaN*1s)}.b{transition-duration:calc(NaN*1s)}");
  Alcotest.(check string)
    "a NaN length expression merges"
    ".a,.b{width:calc(infinity*1px - infinity*1px)}"
    (minified
       ".a{width:calc(infinity*1px - infinity*1px)}.b{width:calc(infinity*1px \
        - infinity*1px)}");
  (* The other two degenerate constants keep their own values. *)
  let inf = sole_declaration ".c{opacity:calc(infinity)}" in
  let inf' = sole_declaration ".d{opacity:calc(infinity)}" in
  let neg_inf = sole_declaration ".e{opacity:calc(-infinity)}" in
  same_text_same_hash "infinity vs infinity" inf inf';
  distinct_value "infinity vs -infinity" inf neg_inf;
  distinct_value "infinity vs NaN" inf keyword;
  distinct_value "-infinity vs NaN" neg_inf keyword;
  Alcotest.(check string)
    "infinity merges with itself" ".c,.d{opacity:calc(infinity)}"
    (minified ".c{opacity:calc(infinity)}.d{opacity:calc(infinity)}");
  Alcotest.(check string)
    "the three constants stay apart"
    ".a{opacity:calc(NaN)}.c{opacity:calc(infinity)}.e{opacity:calc(-infinity)}"
    (minified
       ".a{opacity:calc(NaN)}.c{opacity:calc(infinity)}.e{opacity:calc(-infinity)}")

(* CSS Color 4 sec. 5.2 reads the hexadecimal digits ASCII case-insensitively
   and expands [#RGB] to [#RRGGBB] by duplicating each digit, so [#FFF], [#fff]
   and [#ffffff] name one colour. The source spelling is a pretty-printing
   detail, not part of the value: once optimisation has canonicalised it away
   the three are one declaration, they hash alike, and the rules holding them
   merge. *)
let hex_spellings_have_one_node () =
  let short_upper = sole_declaration ".a{color:#FFF}" in
  let short_lower = sole_declaration ".b{color:#fff}" in
  let long = sole_declaration ".c{color:#ffffff}" in
  Alcotest.(check string)
    "the fold spells #fff" "color:#fff"
    (Css.Declaration.to_string ~minify:true short_upper);
  same_text_same_hash "#FFF vs #fff" short_upper short_lower;
  same_text_same_hash "#fff vs #ffffff" short_lower long;
  Alcotest.(check string)
    "the three spellings merge" ".a,.b,.c{color:#fff}"
    (minified ".a{color:#FFF}.b{color:#fff}.c{color:#ffffff}");
  (* A colour with no shorter named form takes the same route. *)
  let mixed = sole_declaration ".a{color:#AbC}" in
  let expanded = sole_declaration ".b{color:#aabbcc}" in
  same_text_same_hash "#AbC vs #aabbcc" mixed expanded;
  Alcotest.(check string)
    "a mixed-case spelling merges" ".a,.b{color:#abc}"
    (minified ".a{color:#AbC}.b{color:#aabbcc}");
  (* Sec. 5.2 also gives the fourth digit pair as alpha, and sec. 4.2 drops a
     fully opaque one, so [#FFFF] is the same value again. *)
  same_text_same_hash "#FFFF vs #fff"
    (sole_declaration ".a{color:#FFFF}")
    short_lower;
  (* The control that already merged: a hex whose colour has a shorter name
     folds across notations. *)
  Alcotest.(check string)
    "hex and name merge" ".a,.b,.c{color:red}"
    (minified ".a{color:#FF0000}.b{color:#f00}.c{color:red}");
  (* Different colours stay different values. *)
  distinct_value "#fff vs #eee" short_lower (sole_declaration ".d{color:#EEE}");
  distinct_value "#fff vs #fff8" short_lower
    (sole_declaration ".e{color:#FFF8}")

(* Cascade keeps no raw-token sidecar, so a printed declaration is rebuilt from
   its typed values alone: reading the print back has to land on the node that
   printed it, not merely on the same text. *)
let reads_back input =
  let d = Css.Declaration.of_string input in
  let printed = Css.Declaration.to_string ~minify:true d in
  Alcotest.(check bool)
    (input ^ ": the print reads back as the same node")
    true
    (Css.Declaration.equal_declaration d (Css.Declaration.of_string printed))

(* Four families whose grammar takes more component values than the reader took.
   Each block pins the multi-value forms the grammar allows, the single-value
   forms that already worked, and the neighbouring grammar that must not widen
   with them. *)
let multi_value_grammars () =
  (* CSS Box 4 (ED) sec. 3.2 gives [margin] the value [<'margin-top'>{1,4}] and
     sec. 3.1 gives [margin-top] the value [<length-percentage> | auto], so
     [auto] is a component of the box: it may stand in any of the four slots and
     beside any length, at any count. It is not a whole-value keyword. *)
  check_declaration ~roundtrip:true ~expected:"margin:auto 2px"
    "margin: auto 2px";
  check_declaration ~roundtrip:true ~expected:"margin:auto auto auto 2px"
    "margin: auto auto auto 2px";
  check_declaration ~roundtrip:true ~expected:"margin:2px auto auto"
    "margin: 2px auto auto";
  reads_back "margin: auto 2px";
  reads_back "margin: auto auto auto 2px";
  (* Working before the widening, so a fix must not move them. *)
  check_declaration ~expected:"margin:auto" "margin: auto";
  check_declaration ~expected:"margin:0 auto" "margin: 0 auto";
  check_declaration ~expected:"margin:10px auto" "margin: 10px auto";
  check_declaration ~expected:"margin:0 auto 20px" "margin: 0 auto 20px";
  check_declaration ~expected:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"margin-inline:0 auto" "margin-inline: 0 auto";
  (* A fifth component is outside {1,4}, and the CSS-wide keywords stay
     whole-value (CSS Cascade 5 sec. 6). *)
  neg_cursor read_declaration "margin: 1px 2px 3px 4px 5px";
  neg_cursor read_declaration "margin: inherit 1px";
  (* Sec. 4.1 gives [padding] the same {1,4} box over [<length-percentage
     [0,inf]>], which has no [auto], so widening the margin box must leave this
     one alone. *)
  neg_cursor read_declaration "padding: 0 auto";
  neg_cursor read_declaration "padding: auto";

  (* CSS Backgrounds 3 (ED) sec. 3.2, "Line Patterns: the border-style
     properties": [border-style] is [<line-style>{1,4}], the box shorthand over
     the four side styles, exactly as sec. 3.1 gives [border-color] and sec. 3.3
     gives [border-width] theirs. *)
  check_declaration ~roundtrip:true ~expected:"border-style:double dashed"
    "border-style: double dashed";
  check_declaration ~roundtrip:true ~expected:"border-style:solid none"
    "border-style: solid none";
  check_declaration ~roundtrip:true ~expected:"border-style:none none solid"
    "border-style: none none solid";
  check_declaration ~roundtrip:true
    ~expected:"border-style:none dashed none solid"
    "border-style: none dashed none solid";
  reads_back "border-style: double dashed";
  reads_back "border-style: none dashed none solid";
  check_declaration ~expected:"border-style:solid" "border-style: solid";
  check_declaration ~expected:"border-style:none" "border-style: none";
  (* The box fills the sides by position, so a list repeating what those rules
     already supply is the longer spelling of the same declaration and
     canonicalises to the shortest, as [border-color]'s does. pp holds the
     authored node; the fold is a normalize step. *)
  check_declaration ~expected:"border-style:dashed dashed dashed dashed"
    ~optimized:"border-style:dashed" "border-style: dashed dashed dashed dashed";
  check_declaration ~expected:"border-style:dashed none dashed"
    ~optimized:"border-style:dashed none" "border-style: dashed none dashed";
  neg_cursor read_declaration "border-style: solid solid solid solid solid";
  neg_cursor read_declaration "border-style: solid 1px";

  (* CSS Logical 1 (ED) sec. 4.5.2, "Flow-Relative Border Styles": the
     [border-block-style] and [border-inline-style] shorthands are
     [<'border-top-style'>{1,2}], the first value the start edge and the second
     the end edge, as sec. 4.5.1 gives the widths and sec. 4.5.3 the colours. *)
  check_declaration ~roundtrip:true ~expected:"border-block-style:dashed double"
    "border-block-style: dashed double";
  check_declaration ~roundtrip:true
    ~expected:"border-inline-style:dashed double"
    "border-inline-style: dashed double";
  check_declaration ~roundtrip:true ~expected:"border-inline-style:solid none"
    "border-inline-style: solid none";
  reads_back "border-block-style: dashed double";
  reads_back "border-inline-style: dashed double";
  check_declaration ~expected:"border-block-style:solid"
    "border-block-style: solid";
  check_declaration ~expected:"border-inline-style:dashed"
    "border-inline-style: dashed";
  (* A third value is outside {1,2}; the sibling widths already stop there. *)
  neg_cursor read_declaration "border-inline-style: solid dashed dotted";
  neg_cursor read_declaration "border-block-style: solid dashed dotted";

  (* CSS Backgrounds 3 (ED) sec. 4.1, "Curve Radii: the border-radius
     properties": each corner longhand is [<length-percentage [0,inf]>{1,2}],
     "The first value is the horizontal radius, the second the vertical radius."
     The 11 March 2024 CR draft numbers both sections the same way. *)
  check_declaration ~roundtrip:true ~expected:"border-top-left-radius:1px 5px"
    "border-top-left-radius: 1px 5px";
  check_declaration ~roundtrip:true
    ~expected:"border-bottom-right-radius:10%20%"
    "border-bottom-right-radius: 10% 20%";
  check_declaration ~roundtrip:true
    ~expected:"border-start-start-radius:1px 5px"
    "border-start-start-radius: 1px 5px";
  reads_back "border-top-left-radius: 1px 5px";
  reads_back "border-bottom-right-radius: 10% 20%";
  check_declaration ~expected:"border-top-left-radius:1px"
    "border-top-left-radius: 1px";
  check_declaration ~expected:"border-top-left-radius:50%"
    "border-top-left-radius: 50%";
  (* One value already sets both radii, so a vertical radius equal to the
     horizontal one says what omitting it says. *)
  check_declaration ~expected:"border-top-left-radius:1px 1px"
    ~optimized:"border-top-left-radius:1px" "border-top-left-radius: 1px 1px";
  (* The shorthand already spelled both axes across the slash, and keeps to
     it. *)
  check_declaration ~expected:"border-radius:10%/20%" "border-radius: 10% / 20%";
  (* [0,inf] bars a negative radius, and a third value is outside {1,2}. *)
  neg_cursor read_declaration "border-top-left-radius: -1px";
  neg_cursor read_declaration "border-top-left-radius: 1px 2px 3px"

let declaration_tests =
  [
    (* Core declaration type testing *)
    test_case "declaration" `Quick test_declaration;
    test_case "aspect-ratio has one node" `Quick aspect_ratio_has_one_node;
    test_case "caret auto has one node" `Quick caret_auto_has_one_node;
    test_case "radial gradient var has one node" `Quick
      radial_gradient_var_has_one_node;
    test_case "conic gradient var has one node" `Quick
      conic_gradient_var_has_one_node;
    test_case "NaN is one declared value" `Quick nan_declaration_is_one_value;
    test_case "NaN has one node" `Quick nan_has_one_node;
    test_case "hex spellings have one node" `Quick hex_spellings_have_one_node;
    test_case "parse_declaration" `Quick parse_declaration_case;
    (* Parsing basics *)
    test_case "simple" `Quick simple;
    test_case "multiple" `Quick multiple;
    test_case "block" `Quick block;
    test_case "complex values" `Quick complex_values;
    test_case "quoted strings" `Quick quoted_strings;
    test_case "property name" `Quick property_name;
    test_case "property value" `Quick property_value;
    test_case "missing semicolon" `Quick missing_semicolon;
    test_case "empty input" `Quick empty_input;
    test_case "roundtrip" `Quick roundtrip;
    (* !important handling *)
    test_case "important" `Quick important;
    (* Custom properties and vendor prefixes *)
    test_case "custom properties basic" `Quick custom_properties_basic;
    test_case "custom properties" `Quick custom_properties;
    test_case "opaque numeric tokens" `Quick opaque_numeric_tokens;
    test_case "math sign whitespace" `Quick math_sign_whitespace;
    test_case "inserted token boundary" `Quick inserted_token_boundary;
    test_case "custom property values" `Quick custom_property_values;
    test_case "typed custom font family layers print alike" `Quick
      typed_custom_font_family_layer_printing;
    test_case "parse_custom_property rejects an escaping pair" `Quick
      parse_custom_property_guard;
    test_case "custom_property refuses an escaping pair" `Quick
      custom_property_guard;
    test_case "parse_declaration keeps the name out of the value" `Quick
      parse_declaration_name_case;
    test_case "spec custom property token stream values" `Quick
      spec_custom_tokens;
    test_case "a generic family gates the equivalence key" `Quick
      custom_font_equivalence_key;
    test_case "vendor prefixes" `Quick vendor_prefixes;
    test_case "vendor-prefixed shorthand readers" `Quick
      vendor_prefixed_shorthands;
    (* Property value categories *)
    test_case "colors" `Quick colors;
    test_case "color functions" `Quick color_functions;
    test_case "lengths" `Quick lengths;
    test_case "display" `Quick display;
    test_case "position" `Quick position;
    test_case "font properties" `Quick font_properties;
    test_case "text properties" `Quick text_properties;
    test_case "flexbox direction" `Quick flexbox_direction;
    test_case "flexbox wrap" `Quick flexbox_wrap;
    test_case "flexbox flex+basis" `Quick flexbox_flex_and_basis;
    test_case "flexbox alignment" `Quick flexbox_alignment;
    test_case "borders" `Quick borders;
    test_case "multi-value grammars" `Quick multi_value_grammars;
    test_case "outline line-width" `Quick outline_line_width;
    test_case "border line-width" `Quick border_line_width;
    test_case "border line-style" `Quick border_line_style;
    test_case "background initial slots" `Quick background_initial_slots;
    test_case "background position slot" `Quick background_position_slot;
    test_case "background box slots" `Quick background_box_slots;
    test_case "mask-border mode slot" `Quick mask_border_mode_slot;
    test_case "mask-border mode only" `Quick mask_border_mode_only;
    test_case "box shorthand repeats" `Quick box_shorthand_repeats;
    test_case "substitution shorthand cardinality" `Quick
      substitution_shorthand_cardinality;
    test_case "component var keeps typed value" `Quick
      component_var_keeps_typed_value;
    test_case "border-spacing pair" `Quick border_spacing_pair;
    test_case "background repeat axes" `Quick background_repeat_axes;
    test_case "background drained layer" `Quick background_drained_layer;
    test_case "border line-color" `Quick border_line_color;
    test_case "empty shorthand value" `Quick empty_shorthand_value;
    test_case "all-initial shorthand prints none" `Quick
      all_initial_shorthand_prints_none;
    test_case "logical border shorthands" `Quick logical_border_shorthands;
    test_case "overflow" `Quick overflow;
    test_case "animations (timing)" `Quick animations_timing;
    test_case "animations (state)" `Quick animations_state;
    test_case "animation drained shorthand" `Quick animation_drained_shorthand;
    test_case "transforms" `Quick transforms;
    test_case "filter omitted arguments" `Quick filter_omitted_arguments;
    test_case "filter blur grammar" `Quick filter_blur_grammar;
    test_case "angle units" `Quick angle_units;
    test_case "grid" `Quick grid;
    test_case "list properties" `Quick list_properties;
    test_case "misc properties" `Quick misc;
    test_case "url values" `Quick url_values;
    test_case "spec platform property vectors" `Quick
      spec_platform_property_vectors;
    test_case "spec values level 4/5 edge vectors" `Quick spec_values_l45_edges;
    test_case "spec property grammar table expansion" `Quick
      spec_property_grammar_table_expansion;
    test_case "spec remaining property vectors" `Quick
      spec_remaining_prop_vectors;
    test_case "spec declaration additional grammar vectors" `Quick
      css_wide_custom_property_vectors;
    test_case "spec property grammar manifest" `Quick
      spec_property_grammar_manifest;
    (* Error handling *)
    test_case "error missing colon" `Quick error_missing_colon;
    test_case "of_string has one failure mode" `Quick of_string_one_failure_mode;
    test_case "error stray semicolon" `Quick error_stray_semicolon;
    test_case "error unclosed block" `Quick error_unclosed_block;
    test_case "unterminated parsing" `Quick unterminated;
    test_case "invalid declarations" `Quick invalid;
    test_case "substitution defers CSS-wide mix validation" `Quick
      substitution_defers_css_wide_mix_validation;
    test_case "scroll-margin negative lengths" `Quick scroll_margin_negative;
    test_case "scroll-margin negative lengths (sheet)" `Quick
      scroll_margin_negative_sheet;
    test_case "text-decoration optional line" `Quick
      text_decoration_optional_line;
    test_case "text decoration thickness range" `Quick
      text_decoration_thickness_range;
    test_case "animation infinite name" `Quick animation_infinite_name;
    test_case "animation keyword names" `Quick animation_keyword_names;
    test_case "list-style custom names" `Quick list_style_custom_names;
    test_case "auto is not a color" `Quick auto_is_not_a_color;
    test_case "outline-offset length only" `Quick outline_offset_length_only;
    test_case "line-height-step length only" `Quick line_height_step_length_only;
    test_case "gap normal" `Quick gap_normal;
    test_case "sizing keyword domains" `Quick sizing_keyword_domains;
    test_case "text-decoration drained shorthand" `Quick
      text_decoration_drained_shorthand;
    test_case "white-space collapse only" `Quick white_space_collapse_only;
    test_case "border-image repeat only" `Quick border_image_repeat_only;
    test_case "timeline name only" `Quick timeline_name_only;
    test_case "view-timeline inset slot" `Quick view_timeline_inset_slot;
    test_case "text-box edge only" `Quick text_box_edge_only;
    test_case "text-box normal" `Quick text_box_normal;
    test_case "edge-offset position grammar" `Quick edge_offset_position_grammar;
    test_case "offset position properties" `Quick offset_position_properties;
    test_case "declaration value end" `Quick declaration_value_end;
    test_case "declaration value end (sheet)" `Quick declaration_value_end_sheet;
    test_case "declaration value end negatives" `Quick
      declaration_value_end_negatives;
    test_case "function argument end negatives" `Quick
      function_argument_end_negatives;
    test_case "shape-outside grammar" `Quick shape_outside_grammar;
    test_case "shape-outside grammar (sheet)" `Quick shape_outside_sheet;
    test_case "line-width invalid math argument" `Quick
      line_width_invalid_math_argument;
    test_case "function argument validity" `Quick function_argument_validity;
    test_case "non-empty list grammar" `Quick non_empty_list_grammar;
    test_case "list rejects a trailing separator" `Quick
      list_trailing_separator_invalid;
    test_case "border-radius keyword radii" `Quick border_radius_keyword_radii;
    test_case "border-radius CSS-wide keywords" `Quick
      border_radius_css_wide_keywords;
    (* Spec details and edge cases *)
    test_case "CSS-wide keywords" `Quick css_wide_keywords;
    test_case "spec cascade 3 shorthand properties" `Quick
      spec_cascade3_shorthands;
    test_case "spec cascade 3.1 property aliasing" `Quick spec_cascade3_aliasing;
    test_case "spec break 3.4 page-break alias with a var" `Quick
      spec_break3_page_break_var;
    test_case "a parse error names the property written" `Quick
      error_names_the_property_written;
    test_case "spec cascade 3.2 all property" `Quick spec_cascade3_all;
    test_case "spec cascade 7 defaulting keywords" `Quick
      spec_cascade7_defaulting;
    test_case "comments handling" `Quick comments;
    test_case "unit case-insensitivity" `Quick unit_case;
    test_case "number formats" `Quick number_formats;
    test_case "integer precision" `Quick integer_precision;
    test_case "property name case" `Quick property_case;
    test_case "special cases" `Quick special_cases;
    test_case "edge cases" `Quick edge_cases;
  ]

let suite = ("declaration", declaration_tests)
