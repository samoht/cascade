(** Tests for CSS Declaration parsing *)

open Alcotest
open Css.Declaration

(* Generic check function for values - handles parse/print roundtrip testing *)
let check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  let r = Css.Reader.of_string input in
  let value = reader r in
  let printed = Css.Pp.to_string ~minify:true pp value in
  check string (Fmt.str "%s %s" name input) expected printed

(* Generic check function for declarations - handles parse/print roundtrip
   testing *)
let check_declaration ?expected input =
  let expected = Option.value ~default:input expected in
  let r = Css.Reader.of_string input in
  match read_declaration r with
  | Some decl -> (
      (* Test roundtrip: parse -> print -> parse -> print *)
      let printed1 = Css.Pp.to_string ~minify:true pp_declaration decl in
      let r2 = Css.Reader.of_string printed1 in
      match read_declaration r2 with
      | Some decl2 ->
          let printed2 = Css.Pp.to_string ~minify:true pp_declaration decl2 in
          (* Printed versions should be identical *)
          check string (Fmt.str "roundtrip %s" input) printed1 printed2;
          (* Check against expected *)
          check string (Fmt.str "declaration %s" input) expected printed1
      | None -> fail (Printf.sprintf "Failed to re-parse: %s" printed1))
  | None -> fail (Printf.sprintf "Failed to parse declaration: %s" input)

(* Check declaration with expected parts - more specific testing *)
let check_declaration_parts input exp_name exp_value exp_important =
  let expected =
    if exp_important then Printf.sprintf "%s:%s!important" exp_name exp_value
    else Printf.sprintf "%s:%s" exp_name exp_value
  in
  check_declaration ~expected input

(* Check multiple declarations *)
let check_declarations input expected_count =
  let r = Css.Reader.of_string input in
  let decls = read_declarations r in
  check int
    (Fmt.str "declaration count %s" input)
    expected_count (List.length decls);
  decls

(* Check declaration block *)
let check_block input expected_count =
  let r = Css.Reader.of_string input in
  let decls = read_block r in
  check int
    (Fmt.str "block declaration count %s" input)
    expected_count (List.length decls);
  decls

let test_declaration_simple () =
  (* Basic declarations *)
  check_declaration_parts "color: red;" "color" "red" false;
  check_declaration_parts "margin: 10px;" "margin" "10px" false;
  check_declaration_parts "padding: 5px;" "padding" "5px" false;

  (* With whitespace *)
  check_declaration_parts "  color  :  red  ;  " "color" "red" false;
  check_declaration_parts "\tmargin\t:\t10px\t;\t" "margin" "10px" false

let test_declaration_important () =
  check_declaration_parts "color: red !important;" "color" "red" true;
  check_declaration_parts "margin: 10px !important;" "margin" "10px" true;
  check_declaration_parts "padding: 5px !important;" "padding" "5px" true;

  (* With spacing variations *)
  check_declaration_parts "color: red!important;" "color" "red" true;
  check_declaration_parts "color: red ! important;" "color" "red" true;
  check_declaration_parts "color: red   !   important   ;" "color" "red" true

let test_declaration_complex_values () =
  (* Function values *)
  check_declaration_parts "background: url(image.png);" "background"
    "url(image.png)" false;
  check_declaration_parts "width: calc(100% - 10px);" "width"
    "calc(100% - 10px)" false;
  check_declaration_parts "transform: rotate(45deg);" "transform"
    "rotate(45deg)" false;

  (* Multiple values *)
  check_declaration_parts "font-family: \"Arial\", sans-serif;" "font-family"
    "\"Arial\", sans-serif" false;
  check_declaration_parts "margin: 10px 20px 30px 40px;" "margin"
    "10px 20px 30px 40px" false;

  (* Gradients *)
  check_declaration_parts "background: linear-gradient(to right, red, blue);"
    "background" "linear-gradient(to right, red, blue)" false;

  (* Complex nested functions *)
  check_declaration_parts "width: calc(100% - calc(50px + 10px));" "width"
    "calc(100% - calc(50px + 10px))" false

let test_declaration_quoted_strings () =
  (* Simple quoted strings *)
  check_declaration_parts "content: \"hello\";" "content" "\"hello\"" false;
  check_declaration_parts "content: 'world';" "content" "'world'" false;

  (* Escaped quotes *)
  check_declaration_parts "content: \"a\\\"b\";" "content" "\"a\\\"b\"" false;
  check_declaration_parts "content: 'a\\'b';" "content" "'a\\'b'" false;

  (* Strings with special characters *)
  check_declaration_parts "content: \"a;b\";" "content" "\"a;b\"" false;
  check_declaration_parts "content: \"a:b\";" "content" "\"a:b\"" false;
  check_declaration_parts "content: \"a{b}\";" "content" "\"a{b}\"" false

let test_declaration_custom_properties () =
  check_declaration_parts "--color: red;" "--color" "red" false;
  check_declaration_parts "--my-var: 10px;" "--my-var" "10px" false;
  check_declaration_parts "--complex: var(--other, 10px);" "--complex"
    "var(--other, 10px)" false;
  check_declaration_parts "--important: value !important;" "--important" "value"
    true

let test_declaration_vendor_prefixes () =
  check_declaration_parts "-webkit-transform: rotate(45deg);"
    "-webkit-transform" "rotate(45deg)" false;
  check_declaration_parts "-moz-appearance: none;" "-moz-appearance" "none"
    false;
  check_declaration_parts "-ms-filter: blur(5px);" "-ms-filter" "blur(5px)"
    false;
  check_declaration_parts "-o-transition: all 0.3s;" "-o-transition" "all 0.3s"
    false

let test_declaration_multiple () =
  (* Basic multiple declarations *)
  let decls = check_declarations "color: red; margin: 10px;" 2 in
  (* Check first declaration *)
  (match List.nth_opt decls 0 with
  | Some decl ->
      check string "first property" "color" (property_name decl);
      check bool "first not important" false (is_important decl)
  | None -> fail "Missing first declaration");

  (* Check second declaration *)
  (match List.nth_opt decls 1 with
  | Some decl ->
      check string "second property" "margin" (property_name decl);
      check bool "second not important" false (is_important decl)
  | None -> fail "Missing second declaration");

  (* Mixed important and normal *)
  let decls =
    check_declarations "color: red; margin: 10px !important; padding: 5px;" 3
  in
  match List.nth_opt decls 1 with
  | Some decl -> check bool "second is important" true (is_important decl)
  | None -> fail "Missing second declaration"

let test_declaration_block () =
  (* Basic block *)
  let decls = check_block "{ color: blue; display: block; }" 2 in
  (match List.nth_opt decls 0 with
  | Some decl -> check string "first property" "color" (property_name decl)
  | None -> fail "Missing first declaration");

  (* Block with important *)
  let decls = check_block "{ padding: 10px !important; margin: auto; }" 2 in
  (match List.nth_opt decls 0 with
  | Some decl ->
      check string "first property" "padding" (property_name decl);
      check bool "first is important" true (is_important decl)
  | None -> fail "Missing first declaration");

  (* Empty blocks *)
  let _ = check_block "{}" 0 in
  let _ = check_block "{ }" 0 in
  ()

let test_declaration_missing_semicolon () =
  (* Last declaration without semicolon *)
  let _ = check_declarations "color: red; margin: 10px" 2 in
  (* Single declaration without semicolon *)
  check_declaration_parts "color: red" "color" "red" false;

  (* Complex value without semicolon *)
  check_declaration_parts "width: calc(100% - 10px)" "width" "calc(100% - 10px)"
    false

let test_declaration_empty_input () =
  let r = Css.Reader.of_string "" in
  let decls = read_declarations r in
  check int "empty input" 0 (List.length decls);

  let r = Css.Reader.of_string "   " in
  let decls = read_declarations r in
  check int "whitespace only" 0 (List.length decls)

let test_declaration_property_name () =
  (* Test read_property_name directly *)
  let test_prop_name input expected =
    let r = Css.Reader.of_string input in
    let name = read_property_name r in
    check string (Fmt.str "property name %s" input) expected (String.trim name)
  in

  test_prop_name "color:" "color";
  test_prop_name "  margin  :" "margin";
  test_prop_name "-webkit-transform:" "-webkit-transform";
  test_prop_name "--custom-var:" "--custom-var";
  test_prop_name "font-family:" "font-family"

let test_declaration_property_value () =
  (* Test read_property_value directly *)
  let test_prop_value input expected =
    let r = Css.Reader.of_string input in
    let value = read_property_value r in
    check string (Fmt.str "property value %s" input) expected value
  in

  test_prop_value "red;" "red";
  test_prop_value "10px 20px;" "10px 20px";
  test_prop_value "rgb(255, 0, 0);" "rgb(255, 0, 0)";
  test_prop_value "\"Arial\", sans-serif;" "\"Arial\", sans-serif";
  test_prop_value "calc(100% - 10px);" "calc(100% - 10px)";
  test_prop_value "linear-gradient(to right, red, blue);"
    "linear-gradient(to right, red, blue)"

let test_declaration_roundtrip () =
  (* Test that parsing and re-serializing gives expected results *)
  check_declaration "color:red";
  check_declaration "margin:10px";
  check_declaration "padding:5px!important";
  check_declaration "font-family:\"Arial\",sans-serif";
  check_declaration "background:url(image.png)";
  check_declaration "content:\"a\\\"b\"";
  check_declaration "width:calc(100% - 10px)";

  (* With spacing normalization *)
  check_declaration ~expected:"color:red" "color: red";
  check_declaration ~expected:"margin:10px" "margin : 10px";
  check_declaration ~expected:"padding:5px!important" "padding: 5px !important"

let test_declaration_error_missing_colon () =
  let r = Css.Reader.of_string "color red;" in
  check_raises "missing colon"
    (Css.Reader.Parse_error ("Expected ':' but got ';'", r))
    (fun () -> ignore (read_declaration r))

let test_declaration_error_stray_semicolon () =
  let r = Css.Reader.of_string "; color: red;" in
  check_raises "stray semicolon"
    (Css.Reader.Parse_error ("Expected ':' but got ';'", r))
    (fun () -> ignore (read_declaration r))

let test_declaration_error_unclosed_block () =
  let r = Css.Reader.of_string "{ color: red;" in
  check_raises "missing closing brace"
    (Css.Reader.Parse_error ("unexpected end of input", r))
    (fun () -> ignore (read_block r))

let test_declaration_special_cases () =
  (* Nested parentheses *)
  check_declaration_parts "width: calc(100% - calc(50px + 10px));" "width"
    "calc(100% - calc(50px + 10px))" false;

  (* Custom property with var() value *)
  check_declaration_parts "--x: var(--y, 10px)" "--x" "var(--y, 10px)" false;

  (* Multiple backgrounds *)
  check_declaration_parts "background: url(x.png), linear-gradient(red, blue);"
    "background" "url(x.png), linear-gradient(red, blue)" false

(* Tests for read_typed_declaration function *)
let test_read_typed_declaration_colors () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        Printf.printf "DEBUG: Expected '%s', Got '%s'\n%!" expected_pp output;
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Named colors *)
  check_typed "color" "red" "color:red";
  check_typed "color" "blue" "color:blue";
  check_typed "color" "green" "color:green";
  check_typed "color" "black" "color:black";
  check_typed "color" "white" "color:white";
  check_typed "color" "transparent" "color:transparent";

  (* Hex colors *)
  check_typed "color" "#ff0000" "color:#ff0000";
  check_typed "color" "#00ff00" "color:#00ff00";
  check_typed "color" "#0000ff" "color:#0000ff";
  check_typed "color" "#fff" "color:#fff";
  check_typed "color" "#000" "color:#000";

  (* RGB colors - modern space-separated syntax *)
  check_typed "color" "rgb(255, 0, 0)" "color:rgb(255 0 0)";
  check_typed "color" "rgb(0, 255, 0)" "color:rgb(0 255 0)";
  check_typed "color" "rgba(255, 0, 0, 0.5)" "color:rgb(255 0 0/.5)";

  (* HSL colors - modern space-separated syntax *)
  check_typed "color" "hsl(0, 100%, 50%)" "color:hsl(0 100% 50%)";
  check_typed "color" "hsla(120, 100%, 50%, 0.5)" "color:hsl(120 100% 50%/.5)";

  (* Various color properties *)
  check_typed "background-color" "red" "background-color:red";
  check_typed "border-color" "blue" "border-color:blue";
  check_typed "outline-color" "#ff0000" "outline-color:#ff0000"

let test_read_typed_declaration_lengths () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Pixels *)
  check_typed "width" "100px" "width:100px";
  check_typed "height" "50px" "height:50px";
  check_typed "margin" "10px" "margin:10px";
  check_typed "padding" "20px" "padding:20px";

  (* Percentages *)
  check_typed "width" "100%" "width:100%";
  check_typed "height" "50%" "height:50%";

  (* Em and rem *)
  check_typed "font-size" "1.5em" "font-size:1.5em";
  check_typed "font-size" "2rem" "font-size:2rem";
  check_typed "margin" "1.5rem" "margin:1.5rem";

  (* Zero *)
  check_typed "margin" "0" "margin:0";
  check_typed "padding" "0" "padding:0";

  (* Auto keyword *)
  check_typed "margin" "auto" "margin:auto";
  check_typed "width" "auto" "width:auto";
  check_typed "height" "auto" "height:auto";

  (* Min/max content *)
  check_typed "width" "min-content" "width:min-content";
  check_typed "width" "max-content" "width:max-content";
  check_typed "width" "fit-content" "width:fit-content";

  (* Viewport units *)
  check_typed "width" "100vw" "width:100vw";
  check_typed "height" "100vh" "height:100vh";
  check_typed "width" "50vmin" "width:50vmin";
  check_typed "height" "50vmax" "height:50vmax"

let test_read_typed_declaration_display () =
  let check_typed value expected =
    match read_typed_declaration "display" value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "display: %s" value) expected output
    | None -> fail (Fmt.str "Failed to parse display: %s" value)
  in

  check_typed "none" "display:none";
  check_typed "block" "display:block";
  check_typed "inline" "display:inline";
  check_typed "inline-block" "display:inline-block";
  check_typed "flex" "display:flex";
  check_typed "inline-flex" "display:inline-flex";
  check_typed "grid" "display:grid";
  check_typed "inline-grid" "display:inline-grid";
  check_typed "table" "display:table";
  check_typed "table-row" "display:table-row";
  check_typed "table-cell" "display:table-cell";
  check_typed "list-item" "display:list-item";
  check_typed "contents" "display:contents";
  check_typed "flow-root" "display:flow-root"

let test_read_typed_declaration_position () =
  let check_typed value expected =
    match read_typed_declaration "position" value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "position: %s" value) expected output
    | None -> fail (Fmt.str "Failed to parse position: %s" value)
  in

  check_typed "static" "position:static";
  check_typed "relative" "position:relative";
  check_typed "absolute" "position:absolute";
  check_typed "fixed" "position:fixed";
  check_typed "sticky" "position:sticky"

let test_read_typed_declaration_font_properties () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Font weight *)
  check_typed "font-weight" "normal" "font-weight:normal";
  check_typed "font-weight" "bold" "font-weight:bold";
  check_typed "font-weight" "lighter" "font-weight:lighter";
  check_typed "font-weight" "bolder" "font-weight:bolder";
  check_typed "font-weight" "100" "font-weight:100";
  check_typed "font-weight" "400" "font-weight:400";
  check_typed "font-weight" "700" "font-weight:700";
  check_typed "font-weight" "900" "font-weight:900";

  (* Font style *)
  check_typed "font-style" "normal" "font-style:normal";
  check_typed "font-style" "italic" "font-style:italic";
  check_typed "font-style" "oblique" "font-style:oblique";

  (* Font family - list type *)
  check_typed "font-family" "Arial" "font-family:Arial";
  check_typed "font-family" "\"Helvetica Neue\", Helvetica, Arial, sans-serif"
    "font-family:\"Helvetica Neue\",Helvetica,Arial,sans-serif";
  check_typed "font-family" "Georgia, serif" "font-family:Georgia,serif";

  (* Line height *)
  check_typed "line-height" "1.5" "line-height:1.5";
  check_typed "line-height" "2" "line-height:2";
  check_typed "line-height" "normal" "line-height:normal";
  check_typed "line-height" "20px" "line-height:20px";
  check_typed "line-height" "1.5em" "line-height:1.5em"

let test_read_typed_declaration_text_properties () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Text align *)
  check_typed "text-align" "left" "text-align:left";
  check_typed "text-align" "right" "text-align:right";
  check_typed "text-align" "center" "text-align:center";
  check_typed "text-align" "justify" "text-align:justify";
  check_typed "text-align" "start" "text-align:start";
  check_typed "text-align" "end" "text-align:end";

  (* Text decoration *)
  check_typed "text-decoration" "none" "text-decoration:none";
  check_typed "text-decoration" "underline" "text-decoration:underline";
  check_typed "text-decoration" "overline" "text-decoration:overline";
  check_typed "text-decoration" "line-through" "text-decoration:line-through";

  (* Text transform *)
  check_typed "text-transform" "none" "text-transform:none";
  check_typed "text-transform" "uppercase" "text-transform:uppercase";
  check_typed "text-transform" "lowercase" "text-transform:lowercase";
  check_typed "text-transform" "capitalize" "text-transform:capitalize";

  (* White space *)
  check_typed "white-space" "normal" "white-space:normal";
  check_typed "white-space" "nowrap" "white-space:nowrap";
  check_typed "white-space" "pre" "white-space:pre";
  check_typed "white-space" "pre-wrap" "white-space:pre-wrap";
  check_typed "white-space" "pre-line" "white-space:pre-line";
  check_typed "white-space" "break-spaces" "white-space:break-spaces"

let test_read_typed_declaration_flexbox () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Flex direction *)
  check_typed "flex-direction" "row" "flex-direction:row";
  check_typed "flex-direction" "row-reverse" "flex-direction:row-reverse";
  check_typed "flex-direction" "column" "flex-direction:column";
  check_typed "flex-direction" "column-reverse" "flex-direction:column-reverse";

  (* Flex wrap *)
  check_typed "flex-wrap" "nowrap" "flex-wrap:nowrap";
  check_typed "flex-wrap" "wrap" "flex-wrap:wrap";
  check_typed "flex-wrap" "wrap-reverse" "flex-wrap:wrap-reverse";

  (* Flex shorthand *)
  check_typed "flex" "1" "flex:1";
  check_typed "flex" "1 1 auto" "flex:1 1 auto";
  check_typed "flex" "0 1 auto" "flex:0 1 auto";
  check_typed "flex" "initial" "flex:initial";
  check_typed "flex" "none" "flex:none";
  check_typed "flex" "auto" "flex:auto";

  (* Flex grow/shrink *)
  check_typed "flex-grow" "0" "flex-grow:0";
  check_typed "flex-grow" "1" "flex-grow:1";
  check_typed "flex-grow" "2" "flex-grow:2";
  check_typed "flex-shrink" "0" "flex-shrink:0";
  check_typed "flex-shrink" "1" "flex-shrink:1";

  (* Flex basis *)
  check_typed "flex-basis" "auto" "flex-basis:auto";
  check_typed "flex-basis" "100px" "flex-basis:100px";
  check_typed "flex-basis" "50%" "flex-basis:50%";

  (* Align items *)
  check_typed "align-items" "stretch" "align-items:stretch";
  check_typed "align-items" "flex-start" "align-items:flex-start";
  check_typed "align-items" "flex-end" "align-items:flex-end";
  check_typed "align-items" "center" "align-items:center";
  check_typed "align-items" "baseline" "align-items:baseline";

  (* Justify content *)
  check_typed "justify-content" "flex-start" "justify-content:flex-start";
  check_typed "justify-content" "flex-end" "justify-content:flex-end";
  check_typed "justify-content" "center" "justify-content:center";
  check_typed "justify-content" "space-between" "justify-content:space-between";
  check_typed "justify-content" "space-around" "justify-content:space-around";
  check_typed "justify-content" "space-evenly" "justify-content:space-evenly"

let test_read_typed_declaration_borders () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Border style *)
  check_typed "border-style" "none" "border-style:none";
  check_typed "border-style" "solid" "border-style:solid";
  check_typed "border-style" "dashed" "border-style:dashed";
  check_typed "border-style" "dotted" "border-style:dotted";
  check_typed "border-style" "double" "border-style:double";
  check_typed "border-style" "groove" "border-style:groove";
  check_typed "border-style" "ridge" "border-style:ridge";
  check_typed "border-style" "inset" "border-style:inset";
  check_typed "border-style" "outset" "border-style:outset";

  (* Border width *)
  check_typed "border-width" "1px" "border-width:1px";
  check_typed "border-width" "2px" "border-width:2px";
  check_typed "border-width" "thin" "border-width:thin";
  check_typed "border-width" "medium" "border-width:medium";
  check_typed "border-width" "thick" "border-width:thick";

  (* Border radius *)
  check_typed "border-radius" "0" "border-radius:0";
  check_typed "border-radius" "5px" "border-radius:5px";
  check_typed "border-radius" "50%" "border-radius:50%";
  check_typed "border-radius" "10px" "border-radius:10px";

  (* Individual borders *)
  check_typed "border-top-style" "solid" "border-top-style:solid";
  check_typed "border-right-style" "dashed" "border-right-style:dashed";
  check_typed "border-bottom-style" "dotted" "border-bottom-style:dotted";
  check_typed "border-left-style" "double" "border-left-style:double";

  check_typed "border-top-width" "1px" "border-top-width:1px";
  check_typed "border-right-width" "2px" "border-right-width:2px";
  check_typed "border-bottom-width" "3px" "border-bottom-width:3px";
  check_typed "border-left-width" "4px" "border-left-width:4px";

  check_typed "border-top-color" "red" "border-top-color:red";
  check_typed "border-right-color" "blue" "border-right-color:blue";
  check_typed "border-bottom-color" "green" "border-bottom-color:green";
  check_typed "border-left-color" "yellow" "border-left-color:yellow"

let test_read_typed_declaration_overflow () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  check_typed "overflow" "visible" "overflow:visible";
  check_typed "overflow" "hidden" "overflow:hidden";
  check_typed "overflow" "scroll" "overflow:scroll";
  check_typed "overflow" "auto" "overflow:auto";
  check_typed "overflow" "clip" "overflow:clip";

  check_typed "overflow-x" "visible" "overflow-x:visible";
  check_typed "overflow-x" "hidden" "overflow-x:hidden";
  check_typed "overflow-x" "scroll" "overflow-x:scroll";
  check_typed "overflow-x" "auto" "overflow-x:auto";

  check_typed "overflow-y" "visible" "overflow-y:visible";
  check_typed "overflow-y" "hidden" "overflow-y:hidden";
  check_typed "overflow-y" "scroll" "overflow-y:scroll";
  check_typed "overflow-y" "auto" "overflow-y:auto"

let test_read_typed_declaration_animations () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Animation properties *)
  check_typed "animation-name" "slide-in" "animation-name:slide-in";
  check_typed "animation-name" "none" "animation-name:none";

  check_typed "animation-duration" "1s" "animation-duration:1s";
  check_typed "animation-duration" "500ms" "animation-duration:500ms";
  check_typed "animation-duration" "2.5s" "animation-duration:2.5s";

  check_typed "animation-timing-function" "ease"
    "animation-timing-function:ease";
  check_typed "animation-timing-function" "ease-in"
    "animation-timing-function:ease-in";
  check_typed "animation-timing-function" "ease-out"
    "animation-timing-function:ease-out";
  check_typed "animation-timing-function" "ease-in-out"
    "animation-timing-function:ease-in-out";
  check_typed "animation-timing-function" "linear"
    "animation-timing-function:linear";
  check_typed "animation-timing-function" "cubic-bezier(0.4, 0, 0.2, 1)"
    "animation-timing-function:cubic-bezier(0.4,0,0.2,1)";

  check_typed "animation-delay" "0s" "animation-delay:0s";
  check_typed "animation-delay" "1s" "animation-delay:1s";
  check_typed "animation-delay" "-500ms" "animation-delay:-500ms";

  check_typed "animation-iteration-count" "1" "animation-iteration-count:1";
  check_typed "animation-iteration-count" "3" "animation-iteration-count:3";
  check_typed "animation-iteration-count" "infinite"
    "animation-iteration-count:infinite";

  check_typed "animation-direction" "normal" "animation-direction:normal";
  check_typed "animation-direction" "reverse" "animation-direction:reverse";
  check_typed "animation-direction" "alternate" "animation-direction:alternate";
  check_typed "animation-direction" "alternate-reverse"
    "animation-direction:alternate-reverse";

  check_typed "animation-fill-mode" "none" "animation-fill-mode:none";
  check_typed "animation-fill-mode" "forwards" "animation-fill-mode:forwards";
  check_typed "animation-fill-mode" "backwards" "animation-fill-mode:backwards";
  check_typed "animation-fill-mode" "both" "animation-fill-mode:both";

  check_typed "animation-play-state" "running" "animation-play-state:running";
  check_typed "animation-play-state" "paused" "animation-play-state:paused"

let test_read_typed_declaration_transforms () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Transform functions *)
  check_typed "transform" "none" "transform:none";
  check_typed "transform" "translateX(10px)" "transform:translateX(10px)";
  check_typed "transform" "translateY(20px)" "transform:translateY(20px)";
  check_typed "transform" "translate(10px, 20px)"
    "transform:translate(10px,20px)";
  check_typed "transform" "scale(2)" "transform:scale(2)";
  check_typed "transform" "scale(1.5, 2)" "transform:scale(1.5,2)";
  check_typed "transform" "rotate(45deg)" "transform:rotate(45deg)";
  check_typed "transform" "skewX(30deg)" "transform:skewX(30deg)";
  check_typed "transform" "skewY(15deg)" "transform:skewY(15deg)";
  check_typed "transform" "matrix(1, 0, 0, 1, 0, 0)"
    "transform:matrix(1,0,0,1,0,0)";

  (* Multiple transforms *)
  check_typed "transform" "translateX(10px) rotate(45deg)"
    "transform:translateX(10px) rotate(45deg)";
  check_typed "transform" "scale(2) translateY(20px) rotate(180deg)"
    "transform:scale(2) translateY(20px) rotate(180deg)";

  (* Transform origin *)
  check_typed "transform-origin" "center" "transform-origin:center";
  check_typed "transform-origin" "top left" "transform-origin:top left";
  check_typed "transform-origin" "50% 50%" "transform-origin:50% 50%";
  check_typed "transform-origin" "10px 20px" "transform-origin:10px 20px"

let test_read_typed_declaration_grid () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Grid template columns/rows *)
  check_typed "grid-template-columns" "none" "grid-template-columns:none";
  check_typed "grid-template-columns" "100px 200px"
    "grid-template-columns:100px 200px";
  check_typed "grid-template-columns" "1fr 2fr" "grid-template-columns:1fr 2fr";
  check_typed "grid-template-columns" "repeat(3, 1fr)"
    "grid-template-columns:repeat(3,1fr)";
  check_typed "grid-template-columns" "minmax(100px, 1fr) 200px"
    "grid-template-columns:minmax(100px,1fr) 200px";

  check_typed "grid-template-rows" "none" "grid-template-rows:none";
  check_typed "grid-template-rows" "100px auto" "grid-template-rows:100px auto";
  check_typed "grid-template-rows" "repeat(2, minmax(0, 1fr))"
    "grid-template-rows:repeat(2,minmax(0,1fr))";

  (* Grid areas *)
  check_typed "grid-template-areas" "\"header header\" \"sidebar main\""
    "grid-template-areas:\"header header\" \"sidebar main\"";
  check_typed "grid-area" "header" "grid-area:header";

  (* Grid lines *)
  check_typed "grid-row-start" "1" "grid-row-start:1";
  check_typed "grid-row-start" "span 2" "grid-row-start:span 2";
  check_typed "grid-row-end" "3" "grid-row-end:3";
  check_typed "grid-column-start" "1" "grid-column-start:1";
  check_typed "grid-column-end" "-1" "grid-column-end:-1";

  (* Grid auto flow *)
  check_typed "grid-auto-flow" "row" "grid-auto-flow:row";
  check_typed "grid-auto-flow" "column" "grid-auto-flow:column";
  check_typed "grid-auto-flow" "row dense" "grid-auto-flow:row dense";
  check_typed "grid-auto-flow" "column dense" "grid-auto-flow:column dense";

  (* Grid gaps *)
  check_typed "gap" "10px" "gap:10px";
  check_typed "gap" "10px 20px" "gap:10px 20px";
  check_typed "column-gap" "10px" "column-gap:10px";
  check_typed "row-gap" "20px" "row-gap:20px"

let test_read_typed_declaration_misc () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Opacity *)
  check_typed "opacity" "0" "opacity:0";
  check_typed "opacity" "0.5" "opacity:0.5";
  check_typed "opacity" "1" "opacity:1";

  (* Z-index *)
  check_typed "z-index" "auto" "z-index:auto";
  check_typed "z-index" "0" "z-index:0";
  check_typed "z-index" "10" "z-index:10";
  check_typed "z-index" "-1" "z-index:-1";
  check_typed "z-index" "9999" "z-index:9999";

  (* Cursor *)
  check_typed "cursor" "auto" "cursor:auto";
  check_typed "cursor" "default" "cursor:default";
  check_typed "cursor" "pointer" "cursor:pointer";
  check_typed "cursor" "move" "cursor:move";
  check_typed "cursor" "text" "cursor:text";
  check_typed "cursor" "wait" "cursor:wait";
  check_typed "cursor" "help" "cursor:help";
  check_typed "cursor" "crosshair" "cursor:crosshair";
  check_typed "cursor" "not-allowed" "cursor:not-allowed";
  check_typed "cursor" "none" "cursor:none";

  (* Visibility *)
  check_typed "visibility" "visible" "visibility:visible";
  check_typed "visibility" "hidden" "visibility:hidden";
  check_typed "visibility" "collapse" "visibility:collapse";

  (* Box sizing *)
  check_typed "box-sizing" "content-box" "box-sizing:content-box";
  check_typed "box-sizing" "border-box" "box-sizing:border-box";

  (* User select *)
  check_typed "user-select" "none" "user-select:none";
  check_typed "user-select" "auto" "user-select:auto";
  check_typed "user-select" "text" "user-select:text";
  check_typed "user-select" "all" "user-select:all";

  (* Pointer events *)
  check_typed "pointer-events" "none" "pointer-events:none";
  check_typed "pointer-events" "auto" "pointer-events:auto";

  (* Resize *)
  check_typed "resize" "none" "resize:none";
  check_typed "resize" "both" "resize:both";
  check_typed "resize" "horizontal" "resize:horizontal";
  check_typed "resize" "vertical" "resize:vertical"

let test_read_typed_declaration_list_properties () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Box shadow *)
  check_typed "box-shadow" "none" "box-shadow:none";
  check_typed "box-shadow" "0 1px 3px rgba(0,0,0,0.12)"
    "box-shadow:0 1px 3px rgba(0,0,0,0.12)";
  check_typed "box-shadow"
    "0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)"
    "box-shadow:0 1px 3px rgba(0,0,0,0.12),0 1px 2px rgba(0,0,0,0.24)";
  check_typed "box-shadow" "inset 0 2px 4px rgba(0,0,0,0.06)"
    "box-shadow:inset 0 2px 4px rgba(0,0,0,0.06)";

  (* Text shadow *)
  check_typed "text-shadow" "none" "text-shadow:none";
  check_typed "text-shadow" "1px 1px 2px black" "text-shadow:1px 1px 2px black";
  check_typed "text-shadow" "0 0 10px blue, 0 0 20px red"
    "text-shadow:0 0 10px blue,0 0 20px red";

  (* Background image *)
  check_typed "background-image" "none" "background-image:none";
  check_typed "background-image" "url(image.png)"
    "background-image:url(image.png)";
  check_typed "background-image" "linear-gradient(to right, red, blue)"
    "background-image:linear-gradient(to right,red,blue)";
  check_typed "background-image" "url(a.png), url(b.png)"
    "background-image:url(a.png),url(b.png)";

  (* Transition *)
  check_typed "transition" "none" "transition:none";
  check_typed "transition" "all 0.3s ease" "transition:all 0.3s ease";
  check_typed "transition" "opacity 0.3s, transform 0.3s"
    "transition:opacity 0.3s,transform 0.3s";

  (* Animation *)
  check_typed "animation" "none" "animation:none";
  check_typed "animation" "spin 1s linear infinite"
    "animation:spin 1s linear infinite";
  check_typed "animation" "slide 0.5s ease-out" "animation:slide 0.5s ease-out"

let test_read_typed_declaration_custom_properties () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Basic custom properties *)
  check_typed "--color" "red" "--color:red";
  check_typed "--my-var" "10px" "--my-var:10px";
  check_typed "--complex" "1px solid black" "--complex:1px solid black";

  (* With var() references *)
  check_typed "--primary" "var(--base-color)" "--primary:var(--base-color)";
  check_typed "--size" "calc(var(--base) * 2)" "--size:calc(var(--base) * 2)";
  check_typed "--fallback" "var(--undefined, 10px)"
    "--fallback:var(--undefined, 10px)"

let test_read_typed_declaration_important () =
  let check_typed name value is_important expected_pp =
    match read_typed_declaration name value is_important with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string
          (Fmt.str "%s: %s%s" name value
             (if is_important then " !important" else ""))
          expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Standard properties with !important *)
  check_typed "color" "red" true "color:red!important";
  check_typed "display" "none" true "display:none!important";
  check_typed "width" "100px" true "width:100px!important";
  check_typed "margin" "auto" true "margin:auto!important";

  (* Custom properties with !important *)
  check_typed "--custom" "value" true "--custom:value!important"

let test_read_typed_declaration_invalid () =
  let check_invalid name value =
    match read_typed_declaration name value false with
    | Some _ ->
        fail (Fmt.str "Should not parse invalid declaration: %s: %s" name value)
    | None -> () (* Expected *)
  in

  (* Invalid property names *)
  check_invalid "not-a-property" "value";
  check_invalid "123invalid" "value";
  check_invalid "" "value";

  (* Invalid values for known properties *)
  check_invalid "color" "not-a-color";
  check_invalid "display" "not-a-display";
  check_invalid "position" "nowhere";
  check_invalid "width" "invalid";

  (* Type mismatches *)
  check_invalid "opacity" "red";
  check_invalid "z-index" "blue";
  check_invalid "font-weight" "green"

let test_read_typed_declaration_edge_cases () =
  let check_typed name value expected_pp =
    match read_typed_declaration name value false with
    | Some decl ->
        let output = Css.Pp.to_string ~minify:true pp_declaration decl in
        check string (Fmt.str "%s: %s" name value) expected_pp output
    | None ->
        fail (Fmt.str "Failed to parse typed declaration: %s: %s" name value)
  in

  (* Empty/whitespace values where valid *)
  check_typed "content" "\"\"" "content:\"\"";
  check_typed "content" "\" \"" "content:\" \"";

  (* Case sensitivity *)
  check_typed "color" "RED" "color:red";
  check_typed "display" "BLOCK" "display:block";
  check_typed "position" "ABSOLUTE" "position:absolute";

  (* Complex calc expressions *)
  check_typed "width" "calc((100% - 20px) / 2)" "width:calc((100% - 20px) / 2)";
  check_typed "height" "calc(100vh - calc(50px + 1em))"
    "height:calc(100vh - calc(50px + 1em))";

  (* Very long values *)
  let long_shadow =
    "0 1px 2px rgba(0,0,0,0.1), 0 2px 4px rgba(0,0,0,0.1), "
    ^ "0 4px 8px rgba(0,0,0,0.1), 0 8px 16px rgba(0,0,0,0.1)"
  in
  check_typed "box-shadow" long_shadow
    "box-shadow:0 1px 2px rgba(0,0,0,0.1),0 2px 4px rgba(0,0,0,0.1),0 4px 8px \
     rgba(0,0,0,0.1),0 8px 16px rgba(0,0,0,0.1)"

let suite =
  [
    ( "declaration",
      [
        test_case "simple" `Quick test_declaration_simple;
        test_case "important" `Quick test_declaration_important;
        test_case "complex values" `Quick test_declaration_complex_values;
        test_case "quoted strings" `Quick test_declaration_quoted_strings;
        test_case "custom properties" `Quick test_declaration_custom_properties;
        test_case "vendor prefixes" `Quick test_declaration_vendor_prefixes;
        test_case "multiple" `Quick test_declaration_multiple;
        test_case "block" `Quick test_declaration_block;
        test_case "missing semicolon" `Quick test_declaration_missing_semicolon;
        test_case "empty input" `Quick test_declaration_empty_input;
        test_case "property name" `Quick test_declaration_property_name;
        test_case "property value" `Quick test_declaration_property_value;
        test_case "roundtrip" `Quick test_declaration_roundtrip;
        test_case "special cases" `Quick test_declaration_special_cases;
        test_case "error missing colon" `Quick
          test_declaration_error_missing_colon;
        test_case "error stray semicolon" `Quick
          test_declaration_error_stray_semicolon;
        test_case "error unclosed block" `Quick
          test_declaration_error_unclosed_block;
        (* Tests for read_typed_declaration *)
        test_case "typed colors" `Quick test_read_typed_declaration_colors;
        test_case "typed lengths" `Quick test_read_typed_declaration_lengths;
        test_case "typed display" `Quick test_read_typed_declaration_display;
        test_case "typed position" `Quick test_read_typed_declaration_position;
        test_case "typed font properties" `Quick
          test_read_typed_declaration_font_properties;
        test_case "typed text properties" `Quick
          test_read_typed_declaration_text_properties;
        test_case "typed flexbox" `Quick test_read_typed_declaration_flexbox;
        test_case "typed borders" `Quick test_read_typed_declaration_borders;
        test_case "typed overflow" `Quick test_read_typed_declaration_overflow;
        test_case "typed animations" `Quick
          test_read_typed_declaration_animations;
        test_case "typed transforms" `Quick
          test_read_typed_declaration_transforms;
        test_case "typed grid" `Quick test_read_typed_declaration_grid;
        test_case "typed misc" `Quick test_read_typed_declaration_misc;
        test_case "typed list properties" `Quick
          test_read_typed_declaration_list_properties;
        test_case "typed custom properties" `Quick
          test_read_typed_declaration_custom_properties;
        test_case "typed important" `Quick test_read_typed_declaration_important;
        test_case "typed invalid" `Quick test_read_typed_declaration_invalid;
        test_case "typed edge cases" `Quick
          test_read_typed_declaration_edge_cases;
      ] );
  ]
