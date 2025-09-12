(** Tests for CSS Declaration parsing *)

open Alcotest
open Css.Declaration

(* Generic check function for declaration types - handles parse/print roundtrip
   testing *)
let check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  let r = Css.Reader.of_string input in
  let value = reader r in
  match value with
  | Some v ->
      let printed = Css.Pp.to_string ~minify:true pp v in
      (* Test roundtrip stability *)
      let r2 = Css.Reader.of_string printed in
      let value2 = reader r2 in
      (match value2 with
      | Some v2 ->
          let printed2 = Css.Pp.to_string ~minify:true pp v2 in
          check string (Fmt.str "roundtrip %s %s" name input) printed printed2
      | None -> fail (Printf.sprintf "Failed to re-parse %s: %s" name printed));
      (* Check against expected *)
      check string (Fmt.str "%s %s" name input) expected printed
  | None -> fail (Printf.sprintf "Failed to parse %s: %s" name input)

(* Helper to check Parse_error fields match *)
let check_parse_error_fields name (expected : Css.Reader.parse_error)
    (actual : Css.Reader.parse_error) =
  if actual.message <> expected.message then
    Alcotest.failf "%s: expected message '%s' but got '%s'" name
      expected.message actual.message
  else if actual.got <> expected.got then
    Alcotest.failf "%s: expected got=%a but got=%a" name
      Fmt.(option string)
      expected.got
      Fmt.(option string)
      actual.got

(* Helper to check that a function raises a specific exception *)
let check_raises name expected_exn f =
  try
    f ();
    Alcotest.failf "%s: expected exception but none was raised" name
  with
  | Css.Reader.Parse_error actual
    when match expected_exn with
         | Css.Reader.Parse_error expected ->
             check_parse_error_fields name expected actual;
             true
         | _ -> false ->
      ()
  | exn when exn = expected_exn ->
      (* For other exceptions, use structural equality *)
      ()
  | exn ->
      Alcotest.failf "%s: expected %s but got %s" name
        (Printexc.to_string expected_exn)
        (Printexc.to_string exn)

(* One-liner check functions for each type *)
let check_declaration =
  check_value "declaration" pp_declaration read_declaration

let check_declarations input expected_count =
  let r = Css.Reader.of_string input in
  let decls = read_declarations r in
  check int
    (Fmt.str "declarations count %s" input)
    expected_count (List.length decls);
  decls

let check_block input expected_count =
  let r = Css.Reader.of_string input in
  let decls = read_block r in
  check int (Fmt.str "block count %s" input) expected_count (List.length decls);
  decls

let test_declaration_simple () =
  (* Basic declarations *)
  check_declaration ~expected:"color:red" "color: red;";
  check_declaration ~expected:"margin:10px" "margin: 10px;";
  check_declaration ~expected:"padding:5px" "padding: 5px;";
  (* With whitespace *)
  check_declaration ~expected:"color:red" "  color  :  red  ;  ";
  check_declaration ~expected:"margin:10px" "\tmargin\t:\t10px\t;\t"

let test_declaration_complex_values () =
  (* Function values *)
  check_declaration ~expected:"background:url(image.png)"
    "background: url(image.png);";
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width: calc(100% - 10px);";
  check_declaration ~expected:"transform:rotate(45deg)"
    "transform: rotate(45deg);";

  (* Multiple values *)
  check_declaration ~expected:"font-family:\"Arial\",sans-serif"
    "font-family: \"Arial\", sans-serif;";
  check_declaration ~expected:"margin:10px 20px 30px 40px"
    "margin: 10px 20px 30px 40px;";

  (* Gradients *)
  check_declaration ~expected:"background:linear-gradient(to right,red,blue)"
    "background: linear-gradient(to right, red, blue);";

  (* Complex nested functions *)
  check_declaration ~expected:"width:calc(100% - calc(50px + 10px))"
    "width: calc(100% - calc(50px + 10px));"

let test_declaration_quoted_strings () =
  (* Simple quoted strings *)
  check_declaration ~expected:"content:\"hello\"" "content: \"hello\";";
  check_declaration ~expected:"content:'world'" "content: 'world';";

  (* Escaped quotes *)
  check_declaration ~expected:"content:\"a\\\"b\"" "content: \"a\\\"b\";";
  check_declaration ~expected:"content:'a\\'b'" "content: 'a\\'b';";

  (* Strings with special characters *)
  check_declaration ~expected:"content:\"a;b\"" "content: \"a;b\";";
  check_declaration ~expected:"content:\"a:b\"" "content: \"a:b\";";
  check_declaration ~expected:"content:\"a{b}\"" "content: \"a{b}\";"

let test_declaration_custom_properties_basic () =
  check_declaration ~expected:"--color:red" "--color: red;";
  check_declaration ~expected:"--my-var:10px" "--my-var: 10px;";
  check_declaration ~expected:"--complex:var(--other, 10px)"
    "--complex: var(--other, 10px);";
  check_declaration ~expected:"--important:value!important"
    "--important: value !important;"

let test_declaration_vendor_prefixes () =
  check_declaration ~expected:"-webkit-transform:rotate(45deg)"
    "-webkit-transform: rotate(45deg);";
  check_declaration ~expected:"-moz-appearance:none" "-moz-appearance: none;";
  check_declaration ~expected:"-ms-filter:blur(5px)" "-ms-filter: blur(5px);";
  check_declaration ~expected:"-o-transition:all .3s" "-o-transition: all 0.3s;"

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
  check_declaration ~expected:"color:red" "color: red";

  (* Complex value without semicolon *)
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width: calc(100% - 10px)"

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
  check_declaration ~expected:"font-family:Arial,sans-serif"
    "font-family:\"Arial\",sans-serif";
  check_declaration "background:url(image.png)";
  check_declaration "content:\"a\\\"b\"";
  check_declaration "width:calc(100% - 10px)";

  (* With spacing normalization *)
  check_declaration ~expected:"color:red" "color: red";
  check_declaration ~expected:"margin:10px" "margin : 10px";
  check_declaration ~expected:"padding:5px!important" "padding: 5px !important"

let test_declaration_error_missing_colon () =
  let css = "color red;" in
  let r = Css.Reader.of_string css in
  check_raises "missing colon"
    (Css.Reader.Parse_error
       {
         message = "Expected ':' but got 'r'";
         got = None;
         position = 6;
         filename = "<string>";
         context_window = css;
         marker_pos = 6;
         callstack = [];
       })
    (fun () -> ignore (read_declaration r))

let test_declaration_error_stray_semicolon () =
  let css = "; color: red;" in
  let r = Css.Reader.of_string css in
  check_raises "stray semicolon"
    (Css.Reader.Parse_error
       {
         message = "expected identifier";
         got = None;
         position = 0;
         filename = "<string>";
         context_window = css;
         marker_pos = 0;
         callstack = [];
       })
    (fun () -> ignore (read_declaration r))

let test_declaration_error_unclosed_block () =
  let css = "{ color: red;" in
  let r = Css.Reader.of_string css in
  check_raises "missing closing brace"
    (Css.Reader.Parse_error
       {
         message = "unexpected end of input";
         got = None;
         position = 13;
         filename = "<string>";
         context_window = css;
         marker_pos = 13;
         callstack = [];
       })
    (fun () -> ignore (read_block r))

let test_declaration_special_cases () =
  (* Nested parentheses *)
  check_declaration ~expected:"width:calc(100% - calc(50px + 10px))"
    "width: calc(100% - calc(50px + 10px));";

  (* Custom property with var() value *)
  check_declaration ~expected:"--x:var(--y, 10px)" "--x: var(--y, 10px)";

  (* Multiple backgrounds *)
  check_declaration ~expected:"background:url(x.png),linear-gradient(red,blue)"
    "background: url(x.png), linear-gradient(red, blue);"

(* Helper for round-trip declaration testing *)
let check_declaration ~expected input =
  let reader = Css.Reader.of_string input in
  match read_declaration reader with
  | Some decl ->
      let output = Css.Pp.to_string ~minify:true pp_declaration decl in
      check string input expected output
  | None -> fail (Fmt.str "Failed to parse declaration: %s" input)

let test_declaration_colors () =
  (* Named colors *)
  check_declaration ~expected:"color:red" "color: red";
  check_declaration ~expected:"color:blue" "color: blue";
  check_declaration ~expected:"color:green" "color: green";
  check_declaration ~expected:"color:black" "color: black";
  check_declaration ~expected:"color:white" "color: white";
  check_declaration ~expected:"color:transparent" "color: transparent";

  (* Hex colors *)
  check_declaration ~expected:"color:#ff0000" "color: #ff0000";
  check_declaration ~expected:"color:#00ff00" "color: #00ff00";
  check_declaration ~expected:"color:#0000ff" "color: #0000ff";
  check_declaration ~expected:"color:#fff" "color: #fff";
  check_declaration ~expected:"color:#000" "color: #000";

  (* RGB colors - modern space-separated syntax *)
  check_declaration ~expected:"color:rgb(255 0 0)" "color: rgb(255, 0, 0)";
  check_declaration ~expected:"color:rgb(0 255 0)" "color: rgb(0, 255, 0)";
  check_declaration ~expected:"color:rgb(255 0 0/.5)"
    "color: rgba(255, 0, 0, 0.5)";

  (* HSL colors - modern space-separated syntax *)
  check_declaration ~expected:"color:hsl(0 100% 50%)" "color: hsl(0, 100%, 50%)";
  check_declaration ~expected:"color:hsl(120 100% 50%/.5)"
    "color: hsla(120, 100%, 50%, 0.5)";

  (* Various color properties *)
  check_declaration ~expected:"background-color:red" "background-color: red";
  check_declaration ~expected:"border-color:blue" "border-color: blue";
  check_declaration ~expected:"outline-color:#ff0000" "outline-color: #ff0000"

let test_declaration_lengths () =
  (* Pixels *)
  check_declaration ~expected:"width:100px" "width: 100px";
  check_declaration ~expected:"height:50px" "height: 50px";
  check_declaration ~expected:"margin:10px" "margin: 10px";
  check_declaration ~expected:"padding:20px" "padding: 20px";

  (* Percentages *)
  check_declaration ~expected:"width:100%" "width: 100%";
  check_declaration ~expected:"height:50%" "height: 50%";

  (* Em and rem *)
  check_declaration ~expected:"font-size:1.5em" "font-size: 1.5em";
  check_declaration ~expected:"font-size:2rem" "font-size: 2rem";
  check_declaration ~expected:"margin:1.5rem" "margin: 1.5rem";

  (* Zero *)
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

let test_declaration_display () =
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

let test_declaration_position () =
  let c = check_declaration in
  c ~expected:"position:static" "position: static";
  c ~expected:"position:relative" "position: relative";
  c ~expected:"position:absolute" "position: absolute";
  c ~expected:"position:fixed" "position: fixed";
  c ~expected:"position:sticky" "position: sticky"

let test_declaration_font_properties () =
  (* Font weight *)
  check_declaration ~expected:"font-weight:normal" "font-weight: normal";
  check_declaration ~expected:"font-weight:bold" "font-weight: bold";
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
    ~expected:"font-family:\"Helvetica Neue\",Helvetica,Arial,sans-serif"
    "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif";
  check_declaration ~expected:"font-family:Georgia,serif"
    "font-family: Georgia, serif";

  (* Line height *)
  check_declaration ~expected:"line-height:1.5" "line-height: 1.5";
  check_declaration ~expected:"line-height:2" "line-height: 2";
  check_declaration ~expected:"line-height:normal" "line-height: normal";
  check_declaration ~expected:"line-height:20px" "line-height: 20px";
  check_declaration ~expected:"line-height:1.5em" "line-height: 1.5em"

let test_declaration_text_properties () =
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

let test_declaration_flexbox () =
  (* Flex direction *)
  check_declaration ~expected:"flex-direction:row" "flex-direction: row";
  check_declaration ~expected:"flex-direction:row-reverse"
    "flex-direction: row-reverse";
  check_declaration ~expected:"flex-direction:column" "flex-direction: column";
  check_declaration ~expected:"flex-direction:column-reverse"
    "flex-direction: column-reverse";

  (* Flex wrap *)
  check_declaration ~expected:"flex-wrap:nowrap" "flex-wrap: nowrap";
  check_declaration ~expected:"flex-wrap:wrap" "flex-wrap: wrap";
  check_declaration ~expected:"flex-wrap:wrap-reverse" "flex-wrap: wrap-reverse";

  (* Flex shorthand *)
  check_declaration ~expected:"flex:1" "flex: 1";
  check_declaration ~expected:"flex:1 1 auto" "flex: 1 1 auto";
  check_declaration ~expected:"flex:0 1 auto" "flex: 0 1 auto";
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
  check_declaration ~expected:"flex-basis:100px" "flex-basis: 100px";
  check_declaration ~expected:"flex-basis:50%" "flex-basis: 50%";

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

let test_declaration_borders () =
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

  check_declaration ~expected:"border-top-color:red" "border-top-color: red";
  check_declaration ~expected:"border-right-color:blue"
    "border-right-color: blue";
  check_declaration ~expected:"border-bottom-color:green"
    "border-bottom-color: green";
  check_declaration ~expected:"border-left-color:yellow"
    "border-left-color: yellow"

let test_declaration_overflow () =
  check_declaration ~expected:"overflow:visible" "overflow: visible";
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

let test_declaration_animations () =
  (* Animation properties *)
  check_declaration ~expected:"animation-name:slide-in"
    "animation-name: slide-in";
  check_declaration ~expected:"animation-name:none" "animation-name: none";

  check_declaration ~expected:"animation-duration:1s" "animation-duration: 1s";
  check_declaration ~expected:"animation-duration:500ms"
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

  check_declaration ~expected:"animation-delay:0s" "animation-delay: 0s";
  check_declaration ~expected:"animation-delay:1s" "animation-delay: 1s";
  check_declaration ~expected:"animation-delay:-500ms" "animation-delay: -500ms";

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

let test_declaration_transforms () =
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

  (* Multiple transforms *)
  check_declaration ~expected:"transform:translateX(10px) rotate(45deg)"
    "transform: translateX(10px) rotate(45deg)";
  check_declaration
    ~expected:"transform:scale(2) translateY(20px) rotate(180deg)"
    "transform: scale(2) translateY(20px) rotate(180deg)";

  (* Transform origin *)
  check_declaration ~expected:"transform-origin:center"
    "transform-origin: center";
  check_declaration ~expected:"transform-origin:top left"
    "transform-origin: top left";
  check_declaration ~expected:"transform-origin:50% 50%"
    "transform-origin: 50% 50%";
  check_declaration ~expected:"transform-origin:10px 20px"
    "transform-origin: 10px 20px"

let test_declaration_grid () =
  (* Grid template columns/rows *)
  check_declaration ~expected:"grid-template-columns:none"
    "grid-template-columns: none";
  check_declaration ~expected:"grid-template-columns:100px 200px"
    "grid-template-columns: 100px 200px";
  check_declaration ~expected:"grid-template-columns:1fr 2fr"
    "grid-template-columns: 1fr 2fr";
  check_declaration ~expected:"grid-template-columns:repeat(3, 1fr)"
    "grid-template-columns: repeat(3, 1fr)";
  check_declaration ~expected:"grid-template-columns:minmax(100px,1fr) 200px"
    "grid-template-columns: minmax(100px, 1fr) 200px";

  check_declaration ~expected:"grid-template-rows:none"
    "grid-template-rows: none";
  check_declaration ~expected:"grid-template-rows:100px auto"
    "grid-template-rows: 100px auto";
  check_declaration ~expected:"grid-template-rows:repeat(2, minmax(0,1fr))"
    "grid-template-rows: repeat(2, minmax(0, 1fr))";

  (* Grid areas *)
  check_declaration
    ~expected:"grid-template-areas:\"header header\" \"sidebar main\""
    "grid-template-areas: \"header header\" \"sidebar main\"";
  check_declaration ~expected:"grid-area:header" "grid-area: header";

  (* Grid lines *)
  check_declaration ~expected:"grid-row-start:1" "grid-row-start: 1";
  check_declaration ~expected:"grid-row-start:span 2" "grid-row-start: span 2";
  check_declaration ~expected:"grid-row-end:3" "grid-row-end: 3";
  check_declaration ~expected:"grid-column-start:1" "grid-column-start: 1";
  check_declaration ~expected:"grid-column-end:-1" "grid-column-end: -1";

  (* Grid auto flow *)
  check_declaration ~expected:"grid-auto-flow:row" "grid-auto-flow: row";
  check_declaration ~expected:"grid-auto-flow:column" "grid-auto-flow: column";
  check_declaration ~expected:"grid-auto-flow:row dense"
    "grid-auto-flow: row dense";
  check_declaration ~expected:"grid-auto-flow:column dense"
    "grid-auto-flow: column dense";

  (* Grid gaps *)
  check_declaration ~expected:"gap:10px" "gap: 10px";
  check_declaration ~expected:"gap:10px 20px" "gap: 10px 20px";
  check_declaration ~expected:"column-gap:10px" "column-gap: 10px";
  check_declaration ~expected:"row-gap:20px" "row-gap: 20px"

let test_declaration_misc () =
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

let test_declaration_list_properties () =
  (* Box shadow *)
  check_declaration ~expected:"box-shadow:none" "box-shadow: none";
  check_declaration ~expected:"box-shadow:0 1px 3px rgb(0 0 0/.12)"
    "box-shadow: 0 1px 3px rgba(0,0,0,0.12)";
  check_declaration
    ~expected:"box-shadow:0 1px 3px rgb(0 0 0/.12),0 1px 2px rgb(0 0 0/.24)"
    "box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)";
  check_declaration ~expected:"box-shadow:inset 0 2px 4px rgb(0 0 0/.06)"
    "box-shadow: inset 0 2px 4px rgba(0,0,0,0.06)";

  (* Text shadow *)
  check_declaration ~expected:"text-shadow:none" "text-shadow: none";
  check_declaration ~expected:"text-shadow:1px 1px 2px black"
    "text-shadow: 1px 1px 2px black";
  check_declaration ~expected:"text-shadow:0 0 10px blue,0 0 20px red"
    "text-shadow: 0 0 10px blue, 0 0 20px red";

  (* Background image *)
  check_declaration ~expected:"background-image:none" "background-image: none";
  check_declaration ~expected:"background-image:url(image.png)"
    "background-image: url(image.png)";
  check_declaration
    ~expected:"background-image:linear-gradient(to right,red,blue)"
    "background-image: linear-gradient(to right, red, blue)";
  check_declaration ~expected:"background-image:url(a.png),url(b.png)"
    "background-image: url(a.png), url(b.png)";

  (* Transition *)
  check_declaration ~expected:"transition:none" "transition: none";
  check_declaration ~expected:"transition:all .3s ease"
    "transition: all 0.3s ease";
  check_declaration ~expected:"transition:opacity .3s,transform .3s"
    "transition: opacity 0.3s, transform 0.3s";

  (* Animation *)
  check_declaration ~expected:"animation:none" "animation: none";
  check_declaration ~expected:"animation:spin 1s linear infinite"
    "animation: spin 1s linear infinite";
  check_declaration ~expected:"animation:slide .5s ease-out"
    "animation: slide 0.5s ease-out"

let test_declaration_custom_properties () =
  (* Basic custom properties *)
  check_declaration ~expected:"--color:red" "--color: red";
  check_declaration ~expected:"--my-var:10px" "--my-var: 10px";
  check_declaration ~expected:"--complex:1px solid black"
    "--complex: 1px solid black";

  (* With var() references *)
  check_declaration ~expected:"--primary:var(--base-color)"
    "--primary: var(--base-color)";
  check_declaration ~expected:"--size:calc(var(--base) * 2)"
    "--size: calc(var(--base) * 2)";
  check_declaration ~expected:"--fallback:var(--undefined, 10px)"
    "--fallback: var(--undefined, 10px)"

let test_declaration_important () =
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
  check_declaration ~expected:"color:blue!important" "color: blue !   important";

  (* Multiple spaces should be valid *)

  (* Invalid/dangling/duplicate !important should be rejected *)
  let neg input label =
    let r = Css.Reader.of_string input in
    let got = Css.Reader.option Css.Declaration.read_declaration r in
    check bool label true (Option.is_none got)
  in
  neg "color: red !;" "dangling bang";
  neg "color: red !notimportant;" "invalid important ident";
  neg "color: red !important !important;" "duplicate important";
  neg "color: red !importent;" "misspelled important";
  check_declaration ~expected:"color:red!important" "color: red! important";
  (* Valid per CSS spec *)
  neg "color: red !IMPORTANT!;" "trailing bang"

let test_declaration_invalid () =
  let neg input label =
    let r = Css.Reader.of_string input in
    let got = Css.Reader.option Css.Declaration.read_declaration r in
    check bool label true (Option.is_none got)
  in
  (* Invalid property names *)
  neg "not-a-property: value" "invalid property name";
  neg "123invalid: value" "property cannot start with digit";
  neg ": value" "missing property name";
  (* Invalid values for known properties *)
  neg "color: not-a-color" "invalid color value";
  neg "display: not-a-display" "invalid display value";
  neg "position: nowhere" "invalid position value";
  neg "width: invalid" "invalid length value";
  (* Type mismatches *)
  neg "opacity: red" "opacity expects number";
  neg "z-index: blue" "z-index expects integer";
  neg "font-weight: green" "font-weight expects numeric/enum"

let test_declaration_edge_cases () =
  (* Empty/whitespace values where valid *)
  check_declaration ~expected:"content:\"\"" "content: \"\"";
  check_declaration ~expected:"content:\" \"" "content: \" \"";

  (* Complex calc expressions *)
  (* Cases with / operator - should be minified without spaces per CSS spec *)
  check_declaration ~expected:"width:calc((100% - 20px)/2)"
    "width: calc((100% - 20px) / 2)";
  check_declaration ~expected:"height:calc(100vh - calc(50px + 1em))"
    "height: calc(100vh - calc(50px + 1em))";

  (* Very long values *)
  let long_shadow =
    "0 1px 2px rgba(0,0,0,0.1), 0 2px 4px rgba(0,0,0,0.1), "
    ^ "0 4px 8px rgba(0,0,0,0.1), 0 8px 16px rgba(0,0,0,0.1)"
  in
  check_declaration
    ~expected:
      "box-shadow:0 1px 2px rgb(0 0 0/.1),0 2px 4px rgb(0 0 0/.1),0 4px 8px \
       rgb(0 0 0/.1),0 8px 16px rgb(0 0 0/.1)"
    ("box-shadow: " ^ long_shadow)

let test_declaration_css_wide_keywords () =
  (* All properties accept CSS-wide keywords per spec *)
  check_declaration ~expected:"display:inherit" "display: inherit";
  check_declaration ~expected:"margin:unset" "margin: unset";
  check_declaration ~expected:"width:revert-layer" "width: revert-layer";
  check_declaration ~expected:"color:revert" "color: revert"

let test_declaration_comments () =
  (* Comments around colon and inside values *)
  check_declaration ~expected:"color:red" "color/*c*/:/**/red";
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width:/*x*/calc(100% - 10px)";
  check_declaration ~expected:"color:red!important" "color: red!/**/important"

let test_declaration_unit_case () =
  (* Units are ASCII case-insensitive per spec *)
  check_declaration ~expected:"width:10px" "width: 10PX";
  check_declaration ~expected:"margin:1em" "margin: 1EM"

let test_declaration_number_formats () =
  (* Leading dot numbers are valid; scientific notation is also valid per CSS
     spec *)
  check_declaration ~expected:"opacity:.5" "opacity: .5";
  (* Scientific notation IS valid in CSS per the spec *)
  check_declaration ~expected:"opacity:100" "opacity: 1e2"

let test_declaration_unterminated () =
  (* Unterminated string *)
  let r1 = Css.Reader.of_string "content: \"abc" in
  let got1 = Css.Reader.option Css.Declaration.read_declaration r1 in
  check bool "unterminated string" true (Option.is_none got1);
  (* Unterminated calc *)
  let r2 = Css.Reader.of_string "width: calc(100% - (10px);" in
  let got2 = Css.Reader.option Css.Declaration.read_declaration r2 in
  check bool "unterminated calc" true (Option.is_none got2);
  (* Unterminated rgb() *)
  let r3 = Css.Reader.of_string "color: rgb(0, 0, 0;" in
  let got3 = Css.Reader.option Css.Declaration.read_declaration r3 in
  check bool "unterminated rgb" true (Option.is_none got3);
  (* Missing semicolon between decls in block *)
  let r4 = Css.Reader.of_string "{ color:red margin:10px; }" in
  let got4 = Css.Reader.option Css.Declaration.read_block r4 in
  check bool "missing semicolon between declarations" true (Option.is_none got4)

let test_declaration_custom_property_values () =
  (* Balanced braces in custom property values *)
  check_declaration ~expected:"--x:{ a: b; }" "--x: { a: b; }";
  (* Semicolons inside strings are fine *)
  check_declaration ~expected:"--x:\"a;b\"" "--x: \"a;b\"";
  (* var() usage in standard properties, with and without fallback *)
  check_declaration ~expected:"color:var(--c,red)" "color: var(--c, red)";
  check_declaration ~expected:"width:var(--w,10px)" "width: var(--w, 10px)";
  check_declaration ~expected:"margin:var(--m)" "margin: var(--m)"

let test_declaration_color_functions () =
  (* color() with alternate spaces and alpha *)
  check_declaration ~expected:"color:color(display-p3 1 0 0/.5)"
    "color: color(display-p3 1 0 0 / 0.5)";
  (* oklch/oklab/hwb forms *)
  check_declaration ~expected:"color:oklch(60% .16 30)"
    "color: oklch(60% 0.16 30)";
  check_declaration ~expected:"color:oklab(60% .1 .05/.75)"
    "color: oklab(0.6 0.1 0.05 / 0.75)";
  check_declaration ~expected:"color:hwb(180 10% 20%/.2)"
    "color: hwb(180 10% 20% / 0.2)";
  (* hex with alpha *)
  check_declaration ~expected:"color:#ff000080" "color: #ff000080";
  check_declaration ~expected:"color:#0f08" "color: #0f08"

let test_declaration_angle_units () =
  check_declaration ~expected:"transform:rotate(.5turn)"
    "transform: rotate(0.5turn)";
  check_declaration ~expected:"transform:rotate(1.5708rad)"
    "transform: rotate(1.5708rad)";
  check_declaration ~expected:"transform:skew(.25turn,100grad)"
    "transform: skew(0.25turn, 100grad)"

let test_declaration_property_case () =
  (* Property names are ASCII case-insensitive *)
  check_declaration ~expected:"color:red" "COLOR: red";
  check_declaration ~expected:"background-image:url(\"a b (1).png\")"
    "BACKGROUND-IMAGE: url(\"a b (1).png\")"

let test_declaration_url_values () =
  check_declaration
    ~expected:"background-image:url(https://example.com/img/a.png)"
    "background-image: url(https://example.com/img/a.png)";
  check_declaration ~expected:"background-image:url(\"a b.png\")"
    "background-image: url(\"a b.png\")";
  check_declaration
    ~expected:"background-image:url(data:image/svg+xml;utf8,<svg/>)"
    "background-image: url(data:image/svg+xml;utf8,<svg/>)"

let suite =
  [
    ( "declaration",
      [
        (* Parsing basics *)
        test_case "simple" `Quick test_declaration_simple;
        test_case "multiple" `Quick test_declaration_multiple;
        test_case "block" `Quick test_declaration_block;
        test_case "property name" `Quick test_declaration_property_name;
        test_case "property value" `Quick test_declaration_property_value;
        test_case "missing semicolon" `Quick test_declaration_missing_semicolon;
        test_case "empty input" `Quick test_declaration_empty_input;
        test_case "roundtrip" `Quick test_declaration_roundtrip;
        (* !important handling *)
        test_case "important" `Quick test_declaration_important;
        (* Custom properties and vendor prefixes *)
        test_case "custom properties basic" `Quick
          test_declaration_custom_properties_basic;
        test_case "custom properties" `Quick test_declaration_custom_properties;
        test_case "custom property values" `Quick
          test_declaration_custom_property_values;
        test_case "vendor prefixes" `Quick test_declaration_vendor_prefixes;
        (* Property value categories *)
        test_case "colors" `Quick test_declaration_colors;
        test_case "color functions" `Quick test_declaration_color_functions;
        test_case "lengths" `Quick test_declaration_lengths;
        test_case "display" `Quick test_declaration_display;
        test_case "position" `Quick test_declaration_position;
        test_case "font properties" `Quick test_declaration_font_properties;
        test_case "text properties" `Quick test_declaration_text_properties;
        test_case "flexbox" `Quick test_declaration_flexbox;
        test_case "borders" `Quick test_declaration_borders;
        test_case "overflow" `Quick test_declaration_overflow;
        test_case "animations" `Quick test_declaration_animations;
        test_case "transforms" `Quick test_declaration_transforms;
        test_case "angle units" `Quick test_declaration_angle_units;
        test_case "grid" `Quick test_declaration_grid;
        test_case "list properties" `Quick test_declaration_list_properties;
        test_case "misc properties" `Quick test_declaration_misc;
        test_case "url values" `Quick test_declaration_url_values;
        (* Error handling *)
        test_case "error missing colon" `Quick
          test_declaration_error_missing_colon;
        test_case "error stray semicolon" `Quick
          test_declaration_error_stray_semicolon;
        test_case "error unclosed block" `Quick
          test_declaration_error_unclosed_block;
        test_case "unterminated parsing" `Quick test_declaration_unterminated;
        test_case "invalid declarations" `Quick test_declaration_invalid;
        (* Spec details and edge cases *)
        test_case "CSS-wide keywords" `Quick test_declaration_css_wide_keywords;
        test_case "comments handling" `Quick test_declaration_comments;
        test_case "unit case-insensitivity" `Quick test_declaration_unit_case;
        test_case "number formats" `Quick test_declaration_number_formats;
        test_case "property name case" `Quick test_declaration_property_case;
        test_case "special cases" `Quick test_declaration_special_cases;
        test_case "edge cases" `Quick test_declaration_edge_cases;
      ] );
  ]
