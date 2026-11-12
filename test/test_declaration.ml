(** Declaration tests: reading a declaration and a block, and the errors the
    reader reports. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

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
  check_declaration ~expected:"content:\"a{b}\"" "content: \"a{b}\";";
  (* CSS Generated Content 3 sec. 2 puts an <image> in both the replacement and
     the list form of the property. *)
  check_declaration ~expected:"content:url(a.png)" "content: url(a.png)";
  check_declaration ~expected:"content:linear-gradient(red,blue)"
    "content: linear-gradient(red, blue)";
  check_declaration ~expected:"content:\"x\" url(a.png)"
    "content: \"x\" url(a.png)"

let custom_properties_basic () =
  check_declaration ~expected:"--color:red" "--color: red;";
  check_declaration ~expected:"--my-var:10px" "--my-var: 10px;";
  check_declaration ~expected:"--complex:var(--other,10px)"
    "--complex: var(--other, 10px);";
  check_declaration ~expected:"--important:value!important"
    "--important: value !important;";
  (* CSS Custom Properties 1 (ED) sec. 4.1 forbids normalizing the whitespace of
     a custom property's value, and Chrome 153 returns [a b] for the first of
     these and [a] for the second, so a run survives and the ends are trimmed.
     The boundary space between two components that would otherwise merge is not
     whitespace normalization: without it [a/**/b] becomes the ident [ab]. *)
  check_declaration ~minify:false ~expected:"--x: a  b" "--x:a  b";
  check_declaration ~minify:false ~expected:"--sp: a" "--sp:  a  ";
  check_declaration ~minify:false ~expected:"--c: a b" "--c:a/**/b";
  (* Trimming the ends is that same step with nothing left between them: CSS
     Custom Properties 1 (ED) sec. 2 gives the [--*] family [Value:
     <declaration-value>?], and sec. 2.2 calls an empty value written into a
     custom property valid, so a whitespace-only value is the empty value and
     not a one-token stream. *)
  check_declaration ~expected:"--e:" "--e: ";
  check_declaration ~expected:"--e:" "--e:";
  (* Section 9.1 serializes an ident by escaping only what must be, so an escape
     the author wrote otherwise does not come back. A custom property's value is
     not a reserialized stream, so its tokens keep the text they were read
     from. *)
  check_declaration ~minify:false ~expected:"--v: gre\\en" "--v: gre\\en";
  check_declaration ~minify:false ~expected:"--w: colo\\r" "--w: colo\\r"

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
   - and both must come back as an [Error]. *)
let of_string_one_failure_mode () =
  let refused input =
    match of_string input with
    | Ok d ->
        Alcotest.failf "%S: expected a parse error, parsed %S" input
          (to_string d)
    | Error _ -> ()
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
  (* CSS Backgrounds 4 sec. 3.6 spells each axis longhand with the same [#] the
     pair carries, so it names one position per layer too. Chrome 153 reads each
     of these and gives them back comma-separated. *)
  check_declaration ~expected:"background-position-x:center,10px"
    "background-position-x: center, 10px;";
  check_declaration ~expected:"background-position-y:center,10px"
    "background-position-y: center, 10px;";
  check_declaration
    ~expected:"background-position-x:left 10px,right 20px,center"
    "background-position-x: left 10px, right 20px, center;";
  (* CSS Values 4 (ED) sec. 8.3 spells one alternative of <position> as "[ left
     | center | right | <length-percentage> ] [ top | center | bottom |
     <length-percentage> ]", so an offset in the first slot pairs with an edge
     keyword in the second. Chrome 153 keeps every one of these and refuses the
     reverse order, which no alternative grants. *)
  check_declaration ~expected:"background-position:50%100%"
    ~optimized:"background-position:bottom" "background-position: 50% bottom";
  check_declaration ~expected:"background-position:50%0%"
    ~optimized:"background-position:top" "background-position: 50% top";
  check_declaration ~expected:"background-position:10px 100%"
    ~optimized:"background-position:10px 100%"
    "background-position: 10px bottom";
  check_declaration ~expected:"object-position:50%100%"
    ~optimized:"object-position:bottom" "object-position: 50% bottom";
  check_declaration ~expected:"transform-origin:50% 100%"
    ~optimized:"transform-origin:50% 100%" "transform-origin: 50% bottom";
  check_declaration ~expected:"background:url(a.png)50%100%"
    ~optimized:"background:url(a.png)bottom" "background: url(a.png) 50% bottom";
  neg_cursor read_declaration "background-position: bottom 50%";
  neg_cursor read_declaration "object-position: bottom 50%";
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
  (* Sec. 5.1 writes [scroll-margin] as [<length>{1,4}] with "Percentages: n/a",
     and a percentage nested in math is still a percentage. Sec. 4.2 gives
     [scroll-padding] a [<length-percentage>], so only the margin turns one
     away. Chrome 153 agrees on all four. *)
  neg_cursor read_declaration "scroll-margin-top: 10%";
  neg_cursor read_declaration "scroll-margin-top: calc(50% + 25%)";
  neg_cursor read_declaration "scroll-margin-inline: calc(10% + 1px) 20px";
  check_declaration ~expected:"scroll-margin-top:calc(1px + 2em)"
    "scroll-margin-top: calc(1px + 2em)";
  check_declaration ~expected:"scroll-padding-top:10%" "scroll-padding-top: 10%";
  (* CSS Position 3 (ED) sec. 3.2 defines [inset] as [<'top'>{1,4}], and CSS
     Backgrounds 3 (ED) sec. 3.1 defines [border-color] over the same
     one-to-four side assignment. *)
  check_declaration ~expected:"inset:1px 1px 1px 1px" ~optimized:"inset:1px"
    "inset: 1px 1px 1px 1px";
  check_declaration ~expected:"border-color:red red red red"
    ~optimized:"border-color:red" "border-color: red red red red";
  (* CSS Backgrounds 3 (ED) sec. 3.2 and 3.3 assign [border-width] and
     [border-style] over the same four sides, and CSS Logical 1 sec. 4.3 and 4.4
     give each of the three border axes its own one-to-two assignment. *)
  check_declaration ~expected:"border-width:2px 2px 2px 2px"
    ~optimized:"border-width:2px" "border-width: 2px 2px 2px 2px";
  check_declaration ~expected:"border-width:2px 3px 2px 3px"
    ~optimized:"border-width:2px 3px" "border-width: 2px 3px 2px 3px";
  check_declaration ~expected:"border-inline-width:2px 2px"
    ~optimized:"border-inline-width:2px" "border-inline-width: 2px 2px";
  check_declaration ~expected:"border-block-width:1px 2px"
    ~optimized:"border-block-width:1px 2px" "border-block-width: 1px 2px";
  check_declaration ~expected:"border-block-style:solid solid"
    ~optimized:"border-block-style:solid" "border-block-style: solid solid";
  (* CSS Sizing 3 sec. 5 gives the intrinsic sizes to the sizing properties, so
     those keep them where a property reading a plain length does not. [gap]
     reads [normal] from CSS Box Alignment 3 sec. 8.3. *)
  check_declaration ~expected:"width:min-content" ~optimized:"width:min-content"
    "width: min-content";
  check_declaration ~expected:"max-width:none" ~optimized:"max-width:none"
    "max-width: none";
  check_declaration ~expected:"flex-basis:min-content"
    ~optimized:"flex-basis:min-content" "flex-basis: min-content";
  check_declaration ~expected:"gap:normal" ~optimized:"gap:normal" "gap: normal";
  check_declaration ~expected:"column-height:auto"
    ~optimized:"column-height:auto" "column-height: auto";
  check_declaration ~expected:"column-height:10px"
    ~optimized:"column-height:10px" "column-height: 10px";
  check_declaration ~expected:"column-wrap:wrap" ~optimized:"column-wrap:wrap"
    "column-wrap: wrap";
  check_declaration ~expected:"border-inline-color:red red"
    ~optimized:"border-inline-color:red" "border-inline-color: red red";

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
  (* One [none] beside a position still lands on both the image and the type,
     which is the node all three slots print. *)
  check_declaration ~expected:"list-style:none inside none"
    ~optimized:"list-style:none inside" "list-style: none inside";
  (* CSS Values 4 sec. 2.2 takes each option of a [||] at most once, so the
     image and the type hold two [none] between them and no more. Sec. 3.6
     spells the third out as a syntax error itself. *)
  neg_cursor read_declaration "list-style: none none none";
  neg_cursor read_declaration "list-style: none disc url(bullet.png)";

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

let suite =
  ( "declaration",
    [
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
      (* Custom properties and vendor prefixes *)
      test_case "custom properties basic" `Quick custom_properties_basic;
      test_case "vendor prefixes" `Quick vendor_prefixes;
      (* Error handling *)
      test_case "error missing colon" `Quick error_missing_colon;
      test_case "of_string has one failure mode" `Quick
        of_string_one_failure_mode;
      test_case "error stray semicolon" `Quick error_stray_semicolon;
      test_case "error unclosed block" `Quick error_unclosed_block;
      test_case "special cases" `Quick special_cases;
    ] )
