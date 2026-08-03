(** Tests for CSS Declaration parsing *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers

(* One-liner check functions for each type *)
let check_declaration ?minify ?roundtrip ?expected ?optimized input =
  check_value_cursor "declaration" read_declaration
    (Css.Pp.option pp_declaration)
    ?minify ?roundtrip ?expected input;
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

  (* CSS Images 4 section 3.4.1 defines [<color> <p1> <p2>] as exactly [<color>
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

  (* SVG 2 sec. 13.4 and Filter Effects 1 sec. 9.3 / 12.2 make each of these a
     plain <color>, so they shorten like any other colour-valued property
     instead of surviving as opaque unknown-property text. *)
  check_declaration ~expected:"stop-color:#fff" "stop-color: #ffffff;";
  check_declaration ~expected:"lighting-color:currentColor"
    "lighting-color: currentColor;";
  (* Cross-notation folds are node changes, so they belong to the optimizer
     rather than the printer. *)
  check_declaration ~expected:"flood-color:rgb(255 0 0)"
    "flood-color: rgb(255, 0, 0);";
  decl_optimizes ~prop:"flood-color" ~into:"red" "rgb(255, 0, 0)";
  decl_optimizes ~prop:"stop-color" ~into:"#0000" "rgba(0, 0, 0, 0)";

  (* SVG 2 sec. 13.5 and 14.4 give [fill-rule] and [clip-rule] the same
     <fill-rule>, which is the keyword pair alone: the argument form inside
     polygon() is a different production. *)
  check_declaration ~expected:"fill-rule:evenodd" "fill-rule: evenodd;";
  check_declaration ~expected:"clip-rule:nonzero" "clip-rule: nonzero;";
  check_declaration ~expected:"fill-rule:var(--r)" "fill-rule: var(--r);";
  (* SVG 2 sec. 13.3. [miter-clip] and [arcs] are the Level 2 additions to
     [stroke-linejoin]; a reader that takes the grammar takes all five. *)
  check_declaration ~expected:"stroke-linecap:square" "stroke-linecap: square;";
  check_declaration ~expected:"stroke-linejoin:arcs" "stroke-linejoin: arcs;";
  (* SVG 2 sec. 13.3 makes the miter limit a <number> that cannot go below 1, so
     it minifies like one and a constant calc() folds. *)
  check_declaration ~expected:"stroke-miterlimit:4" "stroke-miterlimit: 4.0;";
  decl_optimizes ~prop:"stroke-miterlimit" ~into:"6" "calc(2 * 3)";
  (* SVG 2 sec. 13.3 separates dashes by comma and/or whitespace and the
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
  (* SVG 2 sec. 13.7: a keyword left out is painted last, in the order [normal]
     would use, so the specification's own example is that [paint-order: stroke]
     equals [paint-order: stroke fill markers]. The shortest spelling is the
     shortest prefix that expands back to the same order. *)
  decl_optimizes ~prop:"paint-order" ~into:"stroke" "stroke fill markers";
  decl_optimizes ~prop:"paint-order" ~into:"normal" "fill stroke markers";
  decl_optimizes ~prop:"paint-order" ~into:"normal" "fill";
  (* Reordering the tail is load-bearing, so this one keeps both keywords. *)
  decl_optimizes ~prop:"paint-order" ~into:"markers stroke" "markers stroke";
  (* SVG 2 sec. 7.10 makes [viewport] what an omitted host space means, so
     writing it is redundant; [screen] is not. *)
  decl_optimizes ~prop:"vector-effect" ~into:"non-scaling-stroke"
    "non-scaling-stroke viewport";
  decl_optimizes ~prop:"vector-effect" ~into:"non-scaling-stroke screen"
    "non-scaling-stroke screen";

  (* Complex nested functions. Per CSS Values 4 section 10.7 the printer
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
    "width: calc(100% - calc(10px - calc(5px + 2px)));"

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

let error_stray_semicolon () =
  let r = Cursor.of_string "; color: red;" in
  expect_parse_error "stray semicolon" (fun () -> ignore (read_declaration r))

let error_unclosed_block () =
  (* CSS Syntax 5.3.7 auto-closes unterminated blocks at EOF, so this now parses
     with an implicit [}]. *)
  let r = Cursor.of_string "{ color: red;" in
  ignore (read_block r : Css.Declaration.declaration list)

let special_cases () =
  (* Per CSS Values 4 section 10.7 the inner all-constant calc reduces to
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
  check_declaration ~expected:"background-position:30% 50%,70% 50%"
    ~optimized:"background-position:30%,70%"
    "background-position: 30% 50%, 70% 50%;";
  check_declaration ~expected:"mask-position:0 0,10px 10px"
    "mask-position: 0 0, 10px 10px;"

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
  decl_optimizes_to ~held:"color:hsl(0 100% 50%)" ~into:"color:red"
    "color: hsl(0, 100%, 50%)";
  decl_optimizes_to ~held:"color:hsl(120 100% 50%/.5)" ~into:"color:#00ff0080"
    "color: hsla(120, 100%, 50%, 0.5)";

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
  c ~expected:"display:table" "display: table";
  c ~expected:"display:table-row" "display: table-row";
  c ~expected:"display:table-cell" "display: table-cell";
  c ~expected:"display:list-item" "display: list-item";
  c ~expected:"display:contents" "display: contents";
  c ~expected:"display:flow-root" "display: flow-root"

let position () =
  let c = check_declaration in
  c ~expected:"position:static" "position: static";
  c ~expected:"position:relative" "position: relative";
  c ~expected:"position:absolute" "position: absolute";
  c ~expected:"position:fixed" "position: fixed";
  c ~expected:"position:sticky" "position: sticky"

let font_properties () =
  (* Font weight. Per CSS Fonts 4 section 5.1.2 the keywords [normal] and [bold]
     map to [400] / [700]; the printer canonicalizes to the numeric form under
     minify. *)
  check_declaration ~expected:"font-weight:400" "font-weight: normal";
  check_declaration ~expected:"font-weight:700" "font-weight: bold";
  check_declaration ~expected:"font-weight:lighter" "font-weight: lighter";
  check_declaration ~expected:"font-weight:bolder" "font-weight: bolder";
  check_declaration ~expected:"font-weight:100" "font-weight: 100";
  check_declaration ~expected:"font-weight:400" "font-weight: 400";
  check_declaration ~expected:"font-weight:700" "font-weight: 700";
  check_declaration ~expected:"font-weight:900" "font-weight: 900";

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
  (* minify dedups a repeated family, but must not collapse the list to a lone
     generic keyword - [monospace, monospace] opts a bare generic back into the
     normal UA size, so dropping the duplicate would shrink the text. *)
  check_declaration ~expected:"font-family:Arial,Helvetica"
    "font-family: Arial, Helvetica, Arial";
  check_declaration ~expected:"font-family:monospace,monospace"
    "font-family: monospace, monospace";
  check_declaration ~expected:"font-family:serif,serif"
    "font-family: serif, serif";
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
    "white-space: break-spaces"

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

let overflow () =
  check_declaration ~expected:"overflow:visible" "overflow: visible";
  check_declaration ~expected:"overflow:visible!important"
    "overflow: visible !important";
  check_declaration ~expected:"overflow:hidden" "overflow: hidden";
  check_declaration ~expected:"overflow:scroll" "overflow: scroll";
  check_declaration ~expected:"overflow:auto" "overflow: auto";
  check_declaration ~expected:"overflow:clip" "overflow: clip";

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

  check_declaration ~expected:"animation-duration:1s" "animation-duration: 1s";
  check_declaration ~expected:"animation-duration:.5s" (* 500ms -> .5s *)
    "animation-duration: 500ms";
  check_declaration ~expected:"animation-duration:2.5s"
    "animation-duration: 2.5s";

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

  (* Per CSS Values 4 section 6.6 the time unit ([s] or [ms]) is required for
     [<time>]. [0s] does not drop the unit. *)
  check_declaration ~expected:"animation-delay:0s" "animation-delay: 0s";
  check_declaration ~expected:"animation-delay:1s" "animation-delay: 1s";
  check_declaration ~expected:"animation-delay:-.5s" "animation-delay: -500ms"

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

  (* Multiple transforms. Per CSS Transforms 1 section 11 the printer drops
     whitespace between back-to-back transform functions under minify, matching
     Lightning CSS. *)
  check_declaration ~expected:"transform:translateX(10px)rotate(45deg)"
    "transform: translateX(10px) rotate(45deg)";
  check_declaration ~expected:"transform:scale(2)translateY(20px)rotate(180deg)"
    "transform: scale(2) translateY(20px) rotate(180deg)";

  (* Transform origin *)
  (* Per CSS Transforms 1 sec. 6 [center] is shorthand for [50% 50%] and the
     keyword pair [top left] is [0 0]. A single [0] would mean [0 50%], so the
     two-value form must be preserved. *)
  check_declaration ~expected:"transform-origin:50%" "transform-origin: center";
  check_declaration ~expected:"transform-origin:0 0"
    "transform-origin: top left";
  check_declaration ~expected:"transform-origin:50%" "transform-origin: 50% 50%";
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
  (* CSS Grid 2 sec. 7.6 gives [ row | column ] || dense with [row] as the
     omitted axis, so [row dense] and [dense] are one value and the optimizer
     picks the shorter. The axis is load-bearing on [column dense]. *)
  decl_optimizes_to ~held:"grid-auto-flow:row dense"
    ~into:"grid-auto-flow:dense" "grid-auto-flow: row dense";
  decl_optimizes_to ~held:"grid-auto-flow:row dense"
    ~into:"grid-auto-flow:dense" "grid-auto-flow: dense row";
  decl_optimizes ~prop:"grid-auto-flow" ~into:"column dense" "column dense";

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
  check_declaration ~expected:"transition:all .3s" "transition: all 0.3s ease";
  check_declaration ~expected:"transition:all .3s linear"
    "transition: all .3s linear";
  check_declaration ~expected:"transition:opacity 1s ease-in .5s"
    "transition: opacity 1s ease-in .5s";
  check_declaration ~expected:"transition:opacity .3s,transform .3s"
    "transition: opacity 0.3s, transform 0.3s";

  (* Animation *)
  check_declaration ~expected:"animation:none" "animation: none";
  check_declaration ~expected:"animation:spin 1s linear infinite"
    "animation: spin 1s linear infinite";
  check_declaration ~expected:"animation:slide .5s ease-out"
    "animation: slide 0.5s ease-out"

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
  check_declaration ~expected:"background:var(--bg,)" "background: var(--bg,)"

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
  neg "font-family: revert-layer, serif";
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
      ("transform-origin", "left 10px top 20px");
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
        | "border", "1px solid currentColor" -> (Some "border:1px solid", None)
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
            (Some "font:italic small-caps 700 16px/1.5 serif", None)
        | "animation", "fade 1s linear 2 alternate both running" ->
            (Some "animation:fade 1s linear 2 alternate both", None)
        | "animation-range", "entry 10% exit 90%" ->
            (Some "animation-range:entry 10%exit 90%", None)
        | "display", "inline flow-root" -> (Some "display:inline-block", None)
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
      ("filter", "blur()");
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

  (* Per CSS Values 4 section 10.7 the printer fully simplifies all-constant
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
  check_declaration ~expected:"font:700 12pt/14pt Helvetica"
    "font: bold 12pt/14pt Helvetica";
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

let unterminated () =
  (* CSS Syntax 5.3.7 / 4.3.5 auto-close unterminated strings, brackets and
     function calls at EOF. Assert the recovered declaration matches the shape
     an explicit closer would have produced -- the parser must not silently drop
     content. *)
  check_declaration ~expected:"content:\"abc\"" "content: \"abc";
  (* The auto-closed inner parens collapse to a single value, leaving the outer
     mixed-unit calc preserved. *)
  check_declaration ~expected:"width:calc(100% - 10px)"
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
    (decl |> Css.Declaration.normalize
    |> Css.Declaration.string_of_declaration ~minify:true);
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
  check_declaration ~expected:"--bad-string:\"unterminated"
    "--bad-string: \"unterminated";
  neg_cursor read_declaration "--: value";
  neg_cursor read_declaration "-x: value";
  neg_cursor read_declaration "--x"

let color_functions () =
  (* color() with alternate spaces and alpha *)
  check_declaration ~expected:"color:color(display-p3 1 0 0/.5)"
    "color: color(display-p3 1 0 0 / 0.5)"

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
      ( "animation-range: entry 0% exit 100%",
        "animation-range:entry 0%exit 100%" );
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
      ("display: block flex", "display:flex");
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
      "grid-template: none / 1fr";
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
  decl_optimizes ~prop:"mask-position" ~into:"10px 20px" "left 10px top 20px";
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
      ("column-rule: 1px solid currentColor", "column-rule:1px solid");
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
      ("object-view-box: inset(0 0 10% 0)", "object-view-box:inset(0 0 10% 0)");
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
      "backdrop-filter: blur()";
      "will-change: auto, transform";
      "touch-action: pan-x pan-left";
      "resize: block inline both";
      "transition-property: none, opacity";
      "animation-composition: add replace";
      "animation-range-start: exit entry";
      "view-timeline-inset: auto auto auto";
      "position-try-order: most-width normal";
      "position-visibility: anchors-visible always";
      "accent-color: auto red";
      "color-scheme: only only";
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
      "position-area: top bottom";
      "position-try-fallbacks: flip-block --below";
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

  (* Complex values. Per CSS Transforms 1 section 11 the printer drops
     whitespace between back-to-back transform functions under minify. *)
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

type property_grammar_row = Cascade_spec_inventory.Property_grammar.row

let property_grammar_matrix = Cascade_spec_inventory.Property_grammar.rows

let parse_property_decl property value =
  let input = property ^ ":" ^ value in
  try
    let c = Cursor.of_string input in
    match read_declaration c with
    | None -> None
    | Some decl ->
        let serialized =
          Css.Declaration.string_of_declaration ~minify:true decl
        in
        let c2 = Cursor.of_string serialized in
        Some (input, serialized, decl, read_declaration c2)
  with Cursor.Parse_error _ | Reader.Parse_error _ -> None

let same_property_reparse (row : property_grammar_row) decl reparsed =
  Css.Declaration.property_name reparsed = row.property
  && Css.Declaration.string_of_declaration ~minify:true decl
     = Css.Declaration.string_of_declaration ~minify:true reparsed

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

let check_property_trailing_token_rejected (row : property_grammar_row) value =
  match parse_property_decl row.property (value ^ " )") with
  | None -> ()
  | Some (input, serialized, _, _) ->
      Alcotest.failf
        "%s positive vector accepted an extra trailing token: %s -> %s"
        row.property input serialized

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
    "property grammar manifest covers every tracked spec property name" 452
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

(* CSS Shapes 1 sec. 2: [shape-outside] is [none | [<basic-shape> ||
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

let declaration_tests =
  [
    (* Core declaration type testing *)
    test_case "declaration" `Quick test_declaration;
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
    test_case "custom property values" `Quick custom_property_values;
    test_case "spec custom property token stream values" `Quick
      spec_custom_tokens;
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
    test_case "overflow" `Quick overflow;
    test_case "animations (timing)" `Quick animations_timing;
    test_case "animations (state)" `Quick animations_state;
    test_case "transforms" `Quick transforms;
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
    test_case "error stray semicolon" `Quick error_stray_semicolon;
    test_case "error unclosed block" `Quick error_unclosed_block;
    test_case "unterminated parsing" `Quick unterminated;
    test_case "invalid declarations" `Quick invalid;
    test_case "scroll-margin negative lengths" `Quick scroll_margin_negative;
    test_case "scroll-margin negative lengths (sheet)" `Quick
      scroll_margin_negative_sheet;
    test_case "shape-outside grammar" `Quick shape_outside_grammar;
    test_case "shape-outside grammar (sheet)" `Quick shape_outside_sheet;
    (* Spec details and edge cases *)
    test_case "CSS-wide keywords" `Quick css_wide_keywords;
    test_case "spec cascade 3 shorthand properties" `Quick
      spec_cascade3_shorthands;
    test_case "spec cascade 3.1 property aliasing" `Quick spec_cascade3_aliasing;
    test_case "spec cascade 3.2 all property" `Quick spec_cascade3_all;
    test_case "spec cascade 7 defaulting keywords" `Quick
      spec_cascade7_defaulting;
    test_case "comments handling" `Quick comments;
    test_case "unit case-insensitivity" `Quick unit_case;
    test_case "number formats" `Quick number_formats;
    test_case "property name case" `Quick property_case;
    test_case "special cases" `Quick special_cases;
    test_case "edge cases" `Quick edge_cases;
  ]

let suite = ("declaration", declaration_tests)
