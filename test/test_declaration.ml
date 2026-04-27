(** Tests for CSS Declaration parsing *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers

(* One-liyesner check functions for each type *)
let check_declaration =
  check_value_cursor "declaration" read_declaration
    (Css.Pp.option pp_declaration)

let check_declarations input expected_count =
  let r = Css.Cursor.of_string input in
  let decls = read_declarations r in
  check int
    (Fmt.str "declarations count %s" input)
    expected_count (List.length decls);
  decls

let check_block input expected_count =
  let r = Css.Cursor.of_string input in
  let decls = read_block r in
  check int (Fmt.str "block count %s" input) expected_count (List.length decls);
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

  (* Gradients *)
  check_declaration ~expected:"background:linear-gradient(to right,red,blue)"
    "background: linear-gradient(to right, red, blue);";

  (* Complex nested functions - nested calc() preserved *)
  check_declaration ~expected:"width:calc(100% - calc(50px + 10px))"
    "width: calc(100% - calc(50px + 10px));";

  (* Multiple nested calc() - tests Tailwind v4 space-y pattern *)
  (* Nested calc() preserved to match Tailwind output *)
  check_declaration
    ~expected:
      "margin-block-end:calc(calc(var(--spacing)*2)*calc(1 - \
       var(--tw-space-y-reverse)))"
    "margin-block-end: calc(calc(var(--spacing) * 2) * calc(1 - \
     var(--tw-space-y-reverse)));";

  (* Nested calc with multiplication on both sides - preserved *)
  check_declaration ~expected:"width:calc(calc(var(--x)*2)*calc(var(--y) + 1))"
    "width: calc(calc(var(--x) * 2) * calc(var(--y) + 1));";

  (* Nested calc on left side of subtraction - preserved *)
  check_declaration ~expected:"height:calc(calc(50px + 10px) - 100%)"
    "height: calc(calc(50px + 10px) - 100%);";

  (* Double nesting - nested calc() preserved *)
  check_declaration ~expected:"width:calc(100% - calc(10px - calc(5px + 2px)))"
    "width: calc(100% - calc(10px - calc(5px + 2px)));"

let quoted_strings () =
  (* Simple quoted strings *)
  check_declaration ~expected:"content:\"hello\"" "content: \"hello\";";
  check_declaration ~expected:"content:\"world\"" "content: 'world';";

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
  check_declaration ~expected:"--complex:var(--other, 10px)"
    "--complex: var(--other, 10px);";
  check_declaration ~expected:"--important:value!important"
    "--important: value !important;"

let vendor_prefixes () =
  check_declaration ~expected:"-webkit-transform:rotate(45deg)"
    "-webkit-transform: rotate(45deg);";
  check_declaration ~expected:"-moz-appearance:none" "-moz-appearance: none;";
  check_declaration ~expected:"-ms-filter:blur(5px)" "-ms-filter: blur(5px);";
  check_declaration ~expected:"-o-transition:all .3s" "-o-transition: all 0.3s;"

let multiple () =
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

let block () =
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

let missing_semicolon () =
  (* Last declaration without semicolon *)
  let _ = check_declarations "color: red; margin: 10px" 2 in
  (* Single declaration without semicolon *)
  check_declaration ~expected:"color:red" "color: red";

  (* Complex value without semicolon *)
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width: calc(100% - 10px)"

let empty_input () =
  let r = Css.Cursor.of_string "" in
  let decls = read_declarations r in
  check int "empty input" 0 (List.length decls);

  let r = Css.Cursor.of_string "   " in
  let decls = read_declarations r in
  check int "whitespace only" 0 (List.length decls)

let property_name () =
  (* Test read_property_name directly *)
  let test_prop_name input expected =
    let r = Css.Cursor.of_string input in
    let name = read_property_name r in
    check string (Fmt.str "property name %s" input) expected (String.trim name)
  in

  test_prop_name "color:" "color";
  test_prop_name "  margin  :" "margin";
  test_prop_name "-webkit-transform:" "-webkit-transform";
  test_prop_name "--custom-var:" "--custom-var";
  test_prop_name "font-family:" "font-family"

let property_value () =
  (* Test read_property_value directly *)
  let test_prop_value input expected =
    let r = Css.Cursor.of_string input in
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
  | Css.Cursor.Parse_error _ -> ()
  | Css.Reader.Parse_error _ -> ()

let error_missing_colon () =
  let r = Css.Cursor.of_string "color red;" in
  expect_parse_error "missing colon" (fun () -> ignore (read_declaration r))

let error_stray_semicolon () =
  let r = Css.Cursor.of_string "; color: red;" in
  expect_parse_error "stray semicolon" (fun () -> ignore (read_declaration r))

let error_unclosed_block () =
  (* CSS Syntax 5.3.7 auto-closes unterminated blocks at EOF, so this now parses
     with an implicit [}]. *)
  let r = Css.Cursor.of_string "{ color: red;" in
  ignore (read_block r : Css.Declaration.declaration list)

let special_cases () =
  (* Nested calc() - preserved *)
  check_declaration ~expected:"width:calc(100% - calc(50px + 10px))"
    "width: calc(100% - calc(50px + 10px));";

  (* Custom property with var() value *)
  check_declaration ~expected:"--x:var(--y, 10px)" "--x: var(--y, 10px)";

  (* Multiple backgrounds *)
  check_declaration ~expected:"background:url(x.png),linear-gradient(red,blue)"
    "background: url(x.png), linear-gradient(red, blue);"

let colors () =
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

let lengths () =
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
  check_declaration ~expected:"flex-basis:100px" "flex-basis: 100px";
  check_declaration ~expected:"flex-basis:50%" "flex-basis: 50%"

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

let overflow () =
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

  check_declaration ~expected:"animation-delay:0" "animation-delay: 0s";
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

  (* minmax with fr units *)
  check_declaration ~expected:"grid-template-columns:minmax(100px,1fr) 200px"
    "grid-template-columns: minmax(100px, 1fr) 200px";
  check_declaration ~expected:"grid-template-rows:none"
    "grid-template-rows: none";
  check_declaration ~expected:"grid-template-rows:100px auto"
    "grid-template-rows: 100px auto";

  check_declaration ~expected:"grid-template-rows:repeat(2,minmax(0,1fr))"
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

let list_properties () =
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
    "--size: calc(var(--base) * 2)";
  check_declaration ~expected:"--fallback:var(--undefined, 10px)"
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
  check_declaration ~expected:"color:blue!important" "color: blue !   important";
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
  (* Invalid property names *)
  neg "not-a-property: value";
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
      ("aspect-ratio", "16 / 9");
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
      ("mix-blend-mode", "multiply");
      ("filter", "blur(5px) contrast(120%)");
      ("font", "italic small-caps bold 16px/1.5 serif");
      ("font-size", "clamp(1rem, 2vw, 2rem)");
      ("font-weight", "650");
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
    (fun (property, value) -> check_declaration (property ^ ":" ^ value))
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
      ("clip-path", "circle()");
      ("shape-margin", "-1px");
      ("color", "light-dark(black)");
      ("opacity", "2");
      ("mix-blend-mode", "normal multiply");
      ("filter", "blur()");
      ("font", "bold serif");
      ("font-weight", "1000");
      ("font-stretch", "-10%");
      ("font-feature-settings", "\"kern\" maybe");
      ("font-variation-settings", "\"wght\"");
      ("font-palette", "1");
      ("text-wrap-style", "loud");
      ("white-space", "normal pre");
      ("line-height", "-1");
      ("writing-mode", "vertical");
      ("text-combine-upright", "digits 3");
      ("transform", "rotate()");
      ("translate", "10px 20px 30px 40px");
      ("rotate", "1 0 45deg");
      ("scale", "1 2 3 4");
      ("transition", "opacity ease ease");
      ("transition-behavior", "allow-discrete normal");
      ("animation", "1s 2s 3s");
      ("animation-range", "exit entry");
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
  check_declaration ~expected:"width:calc((100% - 20px)/2)"
    "width: calc((100% - 20px) / 2)";
  check_declaration ~expected:"height:calc(100vh - calc(50px + 1em))"
    "height: calc(100vh - calc(50px + 1em))";

  (* Nested operations mixing + and * to confirm only + and - get spaces *)
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
  check_declaration ~expected:"display:initial" "display: initial";
  check_declaration ~expected:"font-size:inherit" "font-size: inherit";
  check_declaration ~expected:"margin:unset" "margin: unset";
  check_declaration ~expected:"color:revert" "color: revert";
  check_declaration ~expected:"width:revert-layer" "width: revert-layer";
  check_declaration ~expected:"all:initial" "all: initial";
  check_declaration ~expected:"all:inherit" "all: inherit";
  check_declaration ~expected:"all:unset" "all: unset";
  check_declaration ~expected:"all:revert" "all: revert";
  check_declaration ~expected:"all:revert-layer" "all: revert-layer";
  check_declaration ~expected:"display:initial" "display: INITIAL";
  neg_cursor read_declaration "all: initial revert";
  neg_cursor read_declaration "display: block revert";
  neg_cursor read_declaration "margin: revert-layer 1rem";
  neg_cursor read_declaration "color: inherit red"

let spec_cascade3_shorthands () =
  (* CSS Cascade section 3: shorthand declarations set all of their longhand
     sub-properties as if expanded in place. Omitted sub-properties are reset to
     their initial values unless the individual shorthand says otherwise. *)
  check_declaration ~expected:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"padding:1em 2em" "padding: 1em 2em";
  check_declaration ~expected:"background:green" "background: green";
  check_declaration ~expected:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"font:bold 12pt/14pt Helvetica"
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
  check_declaration ~expected:"break-before:page" "page-break-before: always";
  check_declaration ~expected:"break-after:page" "page-break-after: always";
  check_declaration ~expected:"break-inside:avoid" "page-break-inside: avoid";
  neg_cursor read_declaration "page-break-before: recto";
  neg_cursor read_declaration "page-break-after: revert always";
  neg_cursor read_declaration "page-break-inside: avoid-page"

let spec_cascade3_all () =
  (* CSS Cascade section 3.2: [all] is a shorthand that accepts only CSS-wide
     keywords and resets all CSS properties except direction, unicode-bidi, and
     custom properties. The parser surface can verify the allowed value set. *)
  check_declaration ~expected:"all:initial" "all: initial";
  check_declaration ~expected:"all:inherit" "all: inherit";
  check_declaration ~expected:"all:unset" "all: unset";
  check_declaration ~expected:"all:revert" "all: revert";
  check_declaration ~expected:"all:revert-layer" "all: revert-layer";
  neg_cursor read_declaration "all: auto";
  neg_cursor read_declaration "all: none";
  neg_cursor read_declaration "all: initial inherit";
  neg_cursor read_declaration "all: revert-layer color";
  neg_cursor read_declaration "all: var(--reset)"

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
     an explicit closer would have produced — the parser must not silently drop
     content. *)
  check_declaration ~expected:"content:\"abc\"" "content: \"abc";
  check_declaration ~expected:"width:calc(100% - (10px))"
    "width: calc(100% - (10px)";
  check_declaration ~expected:"color:rgb(0 0 0)" "color: rgb(0, 0, 0";
  (* A missing semicolon between two declarations in a block remains a parse
     error. *)
  Css_test_helpers.neg_cursor Css.Declaration.read_block
    "{ color:red margin:10px; }"

let custom_property_values () =
  (* Balanced braces in custom property values *)
  check_declaration ~expected:"--x:{ a: b; }" "--x: { a: b; }";
  (* Semicolons inside strings are fine *)
  check_declaration ~expected:"--x:\"a;b\"" "--x: \"a;b\"";
  (* var() usage in standard properties, with and without fallback *)
  check_declaration ~expected:"color:var(--c,red)" "color: var(--c, red)";
  check_declaration ~expected:"width:var(--w,10px)" "width: var(--w, 10px)";
  check_declaration ~expected:"margin:var(--m)" "margin: var(--m)"

let spec_custom_tokens () =
  check_declaration ~expected:"--tokens:[a, b] (c) { d: e; }"
    "--tokens: [a, b] (c) { d: e; }";
  check_declaration ~expected:"--empty:" "--empty:";
  check_declaration ~expected:"--commented:a b" "--commented: a /*x*/ b";
  check_declaration ~expected:"--important-token:1 ! important"
    "--important-token: 1 ! important";
  check_declaration ~expected:"--real-important:1!important"
    "--real-important: 1 !important";
  check_declaration ~expected:"--fallback:var(--missing,)"
    "--fallback: var(--missing,)";
  check_declaration ~expected:"--nested-var:var(--a, var(--b, { color: red; }))"
    "--nested-var: var(--a, var(--b, { color: red; }))";
  check_declaration ~expected:"--bad-string:\"unterminated"
    "--bad-string: \"unterminated";
  check_declaration ~expected:"--:value" "--: value";
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
  check_declaration ~expected:"transform:skew(.25turn,100grad)"
    "transform: skew(0.25turn, 100grad)"

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
      ("scrollbar-color: red blue", "scrollbar-color:red blue");
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
        "animation-range:entry 0% exit 100%" );
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
      ("overscroll-behavior: contain", "overscroll-behavior:contain");
      ("scroll-snap-type: x mandatory", "scroll-snap-type:x mandatory");
      ("scroll-margin-block: 1rem 2rem", "scroll-margin-block:1rem 2rem");
      ("scroll-padding-inline: 10px", "scroll-padding-inline:10px");
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
      ("width: round(nearest, 10px, 3px)", "width:round(nearest,10px,3px)");
      ("width: mod(18px, 5px)", "width:mod(18px,5px)");
      ("width: rem(18px, 5px)", "width:rem(18px,5px)");
      ("width: hypot(3px, 4px)", "width:hypot(3px,4px)");
      ( "width: calc-size(auto, size + 1rem)",
        "width:calc-size(auto,size + 1rem)" );
      ("opacity: abs(-0.5)", "opacity:abs(-.5)");
      ("opacity: sign(var(--delta))", "opacity:sign(var(--delta))");
      ( "color: color-mix(in oklab, red 40%, blue)",
        "color:color-mix(in oklab,red 40%,blue)" );
      ( "color: light-dark(CanvasText, white)",
        "color:light-dark(CanvasText,white)" );
      ( "background-image: image-set(url(a.avif) type(\"image/avif\") 1x, \
         url(a.png) type(\"image/png\") 1x)",
        "background-image:image-set(url(a.avif) type(\"image/avif\") \
         1x,url(a.png) type(\"image/png\") 1x)" );
    ];
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "display: block flex";
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
      "animation-range: exit entry";
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
      "color: color-mix(red, blue)";
      "background-image: image-set(url(a.png))";
    ]

let spec_values_l45_edges () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("width: calc(100% - 2rem)", "width:calc(100% - 2rem)");
      ("width: calc(1px * 2)", "width:calc(1px*2)");
      ("width: calc(100px / 2)", "width:calc(100px/2)");
      ("width: min(10px, 5cqw)", "width:min(10px,5cqw)");
      ("width: max(10svw, 20lvw)", "width:max(10svw,20lvw)");
      ("height: clamp(10dvh, 50%, 100dvh)", "height:clamp(10dvh,50%,100dvh)");
      ("margin: anchor-size(width)", "margin:anchor-size(width)");
      ("top: anchor(bottom)", "top:anchor(bottom)");
      ("font-size: calc(1rem + 1cqi)", "font-size:calc(1rem + 1cqi)");
      ("color: lab(50% 20 30)", "color:lab(50% 20 30)");
      ("color: lch(50% 30 40)", "color:lch(50% 30 40)");
      ("color: oklab(60% .1 .2)", "color:oklab(60% .1 .2)");
      ("color: oklch(60% .2 120)", "color:oklch(60% .2 120)");
      ("color: color(display-p3 1 0 0 / .5)", "color:color(display-p3 1 0 0/.5)");
      ( "color: rgb(from var(--c) r g b / 50%)",
        "color:rgb(from var(--c) r g b/50%)" );
      ( "background: conic-gradient(from 45deg, red, blue)",
        "background:conic-gradient(from 45deg,red,blue)" );
      ( "background: cross-fade(url(a.png) 40%, url(b.png))",
        "background:cross-fade(url(a.png) 40%,url(b.png))" );
      ( "filter: drop-shadow(0 0 2px rgb(0 0 0 / .4))",
        "filter:drop-shadow(0 0 2px rgb(0 0 0/.4))" );
      ( "transform: translate(10px, 20%) rotate(.25turn) scale(1.2)",
        "transform:translate(10px,20%) rotate(.25turn) scale(1.2)" );
      ( "background-position: left 10px top 20%",
        "background-position:left 10px top 20%" );
      ("border-radius: 10px / 20px", "border-radius:10px/20px");
      ( "clip-path: xywh(0 0 100% 100% round 10px)",
        "clip-path:xywh(0 0 100% 100% round 10px)" );
    ];
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
      ("border-inline-color: red blue", "border-inline-color:red blue");
      ("border-start-start-radius: 1rem", "border-start-start-radius:1rem");
      ("outline: 2px solid Highlight", "outline:2px solid Highlight");
      ("outline-offset: -2px", "outline-offset:-2px");
      ( "text-decoration: underline wavy red 2px",
        "text-decoration:underline wavy red 2px" );
      ("text-underline-offset: 2px", "text-underline-offset:2px");
      ("text-emphasis: filled dot red", "text-emphasis:filled dot red");
      ("text-emphasis-position: over right", "text-emphasis-position:over right");
      ("text-orientation: mixed", "text-orientation:mixed");
      ("tab-size: 4", "tab-size:4");
      ("line-break: anywhere", "line-break:anywhere");
      ("overflow-wrap: anywhere", "overflow-wrap:anywhere");
      ("hyphens: manual", "hyphens:manual");
      ("font-optical-sizing: auto", "font-optical-sizing:auto");
      ("font-kerning: normal", "font-kerning:normal");
      ("font-language-override: \"TRK\"", "font-language-override:\"TRK\"");
      ("font-synthesis-style: auto", "font-synthesis-style:auto");
      ("font-synthesis-weight: none", "font-synthesis-weight:none");
      ( "font-variant-ligatures: common-ligatures",
        "font-variant-ligatures:common-ligatures" );
      ("font-variant-caps: small-caps", "font-variant-caps:small-caps");
      ( "font-variant-numeric: tabular-nums slashed-zero",
        "font-variant-numeric:tabular-nums slashed-zero" );
      ("font-variant-position: sub", "font-variant-position:sub");
      ("font-variant-east-asian: ruby", "font-variant-east-asian:ruby");
      ("object-view-box: inset(0 0 10% 0)", "object-view-box:inset(0 0 10% 0)");
      ("image-rendering: pixelated", "image-rendering:pixelated");
      ("image-resolution: from-image 2dppx", "image-resolution:from-image 2dppx");
      ("mask-border: url(mask.svg) 30 fill", "mask-border:url(mask.svg) 30 fill");
      ("mask-size: contain", "mask-size:contain");
      ("mask-repeat: no-repeat", "mask-repeat:no-repeat");
      ("mask-position: left 10px top 20px", "mask-position:left 10px top 20px");
      ( "backdrop-filter: blur(4px) saturate(120%)",
        "backdrop-filter:blur(4px) saturate(120%)" );
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
      ("color-scheme: only light dark", "color-scheme:only light dark");
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
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "display: ruby block";
      "float: inline-start left";
      "contain: strict layout";
      "content-visibility: visible hidden";
      "contain-intrinsic-width: auto auto 10px";
      "overflow: clip visible";
      "overflow-block: visible hidden";
      "overscroll-behavior-inline: contain auto none";
      "scroll-snap-align: start center end";
      "scroll-snap-stop: normal always";
      "scroll-margin: -1px";
      "columns: 1 2 3";
      "column-rule: solid solid";
      "column-span: all none";
      "break-before: page column";
      "box-decoration-break: slice clone";
      "background-origin: border-box padding-box content-box border-box";
      "background-size: contain cover";
      "border-inline-color: red blue green";
      "border-start-start-radius: 1rem /";
      "text-decoration: underline none";
      "text-emphasis: filled open";
      "text-emphasis-position: over under";
      "tab-size: -1";
      "line-break: anywhere strict";
      "font-optical-sizing: auto none";
      "font-kerning: normal none";
      "font-language-override: 1";
      "font-synthesis-style: auto none";
      "font-variant-caps: small-caps unicase";
      "font-variant-position: sub super";
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
  check_declaration "color:red";
  check_declaration "margin:10px";
  check_declaration "display:block";

  (* Custom properties *)
  check_declaration "--custom:value";
  check_declaration "--color:red";

  (* Important declarations *)
  check_declaration "color:red!important";
  check_declaration "--custom:value!important";

  (* Complex values *)
  check_declaration "background:linear-gradient(to right,red,blue)";
  check_declaration "transform:translateX(10px) rotate(45deg)";
  check_declaration "font-family:Arial,sans-serif";

  (* Vendor prefixes *)
  check_declaration "-webkit-transform:rotate(45deg)";
  check_declaration "-moz-appearance:none";

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

let spec_declaration_more_grammar_vectors () =
  (* CSS Cascading: CSS-wide keywords are whole declaration values for standard
     properties, not component values inside a larger value. *)
  List.iter
    (fun keyword ->
      check_declaration ("color:" ^ keyword);
      check_declaration ("margin:" ^ keyword);
      none_cursor read_declaration ("color:" ^ keyword ^ " red");
      none_cursor read_declaration ("margin:1px " ^ keyword);
      none_cursor read_declaration ("background:red " ^ keyword))
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  (* CSS Custom Properties: custom property values are token streams. Balanced
     blocks and non-important [! important] tokens are preserved as values. *)
  check_declaration ~expected:"--tokens:{ color: red }"
    "--tokens: { color: red }";
  check_declaration ~expected:"--list:[a, b, c]" "--list: [a, b, c]";
  check_declaration ~expected:"--empty-fallback:var(--missing,)"
    "--empty-fallback: var(--missing,)";
  check_declaration ~expected:"--not-important:1 ! important"
    "--not-important: 1 ! important";
  check_declaration ~expected:"--is-important:1!important"
    "--is-important: 1 !important";
  none_cursor read_declaration "--: invalid"

type property_grammar_row = {
  property : string;
  positives : string list;
  negatives : string list;
}

let property_grammar_matrix =
  [
    {
      property = "display";
      positives =
        [ "block"; "inline"; "inline flow-root"; "list-item flow-root" ];
      negatives = [ "block inline flex"; "unknown-display" ];
    };
    {
      property = "position";
      positives = [ "static"; "relative"; "absolute"; "fixed"; "sticky" ];
      negatives = [ "sticky absolute"; "center" ];
    };
    {
      property = "float";
      positives = [ "left"; "right"; "none"; "inline-start"; "inline-end" ];
      negatives = [ "center"; "left right" ];
    };
    {
      property = "overflow";
      positives = [ "visible"; "hidden"; "clip"; "auto"; "clip auto" ];
      negatives = [ "none"; "visible hidden scroll" ];
    };
    {
      property = "contain";
      positives = [ "none"; "layout paint"; "strict"; "content" ];
      negatives = [ "layout layout"; "strict layout" ];
    };
    {
      property = "container-type";
      positives = [ "normal"; "size"; "inline-size" ];
      negatives = [ "inline-size size"; "block-size" ];
    };
    {
      property = "container";
      positives = [ "card / inline-size"; "inline-size"; "normal" ];
      negatives = [ "/ inline-size"; "card / inline-size / size" ];
    };
    {
      property = "scroll-snap-type";
      positives = [ "none"; "x mandatory"; "block proximity"; "both mandatory" ];
      negatives = [ "mandatory x"; "x y mandatory" ];
    };
    {
      property = "scroll-snap-align";
      positives = [ "none"; "start"; "start end"; "center" ];
      negatives = [ "start center end"; "foo" ];
    };
    {
      property = "scroll-snap-stop";
      positives = [ "normal"; "always" ];
      negatives = [ "normal always"; "sometimes" ];
    };
    {
      property = "box-sizing";
      positives = [ "content-box"; "border-box" ];
      negatives = [ "padding-box"; "border-box content-box" ];
    };
    {
      property = "aspect-ratio";
      positives = [ "auto"; "16 / 9"; "auto 1 / 1" ];
      negatives = [ "16 /"; "auto auto" ];
    };
    {
      property = "width";
      positives = [ "auto"; "min-content"; "fit-content(20rem)"; "stretch" ];
      negatives = [ "red"; "fit-content()" ];
    };
    {
      property = "margin";
      positives = [ "0"; "1px 2px 3px 4px"; "auto"; "anchor-size(width)" ];
      negatives = [ "red"; "1px 2px 3px 4px 5px" ];
    };
    {
      property = "padding";
      positives = [ "0"; "1px 2px"; "max(1rem, 2vw)" ];
      negatives = [ "auto"; "1px 2px 3px 4px 5px" ];
    };
    {
      property = "border";
      positives = [ "1px solid red"; "solid"; "0"; "thin currentColor" ];
      negatives = [ "1px 2px"; "solid solid"; "red blue" ];
    };
    {
      property = "border-radius";
      positives = [ "10px"; "10px 20px / 30px 40px" ];
      negatives = [ "10px /"; "10px 20px 30px 40px 50px" ];
    };
    {
      property = "background";
      positives = [ "red"; "url(a.png) no-repeat center / cover"; "none" ];
      negatives = [ "red blue"; "url(" ];
    };
    {
      property = "background-image";
      positives = [ "none"; "url(a.png)"; "linear-gradient(red, blue)" ];
      negatives = [ "linear-gradient()"; "image-set()" ];
    };
    {
      property = "background-size";
      positives = [ "auto"; "cover"; "contain"; "10px 20%" ];
      negatives = [ "cover contain"; "-1px" ];
    };
    {
      property = "clip-path";
      positives =
        [ "none"; "inset(10px)"; "circle(50%)"; "xywh(0 0 100% 100%)" ];
      negatives = [ "circle()"; "inset()" ];
    };
    {
      property = "shape-outside";
      positives = [ "none"; "circle(50%)"; "inset(10px)" ];
      negatives = [ "circle()"; "invalid-shape" ];
    };
    {
      property = "color";
      positives =
        [ "red"; "color(display-p3 1 0 0)"; "light-dark(black, white)" ];
      negatives = [ "not-a-color"; "color(display-p3 1 0)" ];
    };
    {
      property = "opacity";
      positives = [ "0"; ".5"; "1"; "50%" ];
      negatives = [ "red"; "1 2" ];
    };
    {
      property = "filter";
      positives =
        [ "none"; "blur(5px) contrast(120%)"; "drop-shadow(0 0 2px black)" ];
      negatives = [ "blur()"; "drop-shadow()" ];
    };
    {
      property = "font";
      positives = [ "italic small-caps bold 16px/1.5 serif"; "16px serif" ];
      negatives = [ "bold serif"; "16px" ];
    };
    {
      property = "font-family";
      positives = [ "Arial, sans-serif"; "\"A B\", serif"; "system-ui" ];
      negatives = [ "Arial,,serif"; "," ];
    };
    {
      property = "font-weight";
      positives = [ "normal"; "bold"; "400"; "650"; "lighter" ];
      negatives = [ "1000"; "bold 400" ];
    };
    {
      property = "font-feature-settings";
      positives = [ "normal"; "\"kern\" 1"; "\"liga\" off" ];
      negatives = [ "\"kern\" maybe"; "1" ];
    };
    {
      property = "text-decoration";
      positives = [ "underline"; "underline wavy red 2px" ];
      negatives = [ "underline none"; "wavy solid" ];
    };
    {
      property = "white-space";
      positives = [ "normal"; "pre"; "preserve nowrap" ];
      negatives = [ "pre normal"; "wrap nowrap preserve" ];
    };
    {
      property = "word-break";
      positives = [ "normal"; "break-all"; "keep-all"; "break-word" ];
      negatives = [ "break"; "normal keep-all" ];
    };
    {
      property = "writing-mode";
      positives = [ "horizontal-tb"; "vertical-rl"; "sideways-rl" ];
      negatives = [ "vertical"; "vertical-rl horizontal-tb" ];
    };
    {
      property = "transform";
      positives = [ "none"; "translateX(10px) rotate(45deg) scale(1.2)" ];
      negatives = [ "translate()"; "none rotate(1deg)" ];
    };
    {
      property = "translate";
      positives = [ "none"; "10px"; "10px 20px"; "10px 20px 30px" ];
      negatives = [ "10px 20px 30px 40px"; "red" ];
    };
    {
      property = "rotate";
      positives = [ "none"; "45deg"; "1 0 0 45deg" ];
      negatives = [ "1 0 45deg"; "45px" ];
    };
    {
      property = "scale";
      positives = [ "none"; "1.2"; "1.2 2"; "1 2 3" ];
      negatives = [ "1 2 3 4"; "red" ];
    };
    {
      property = "transition";
      positives = [ "opacity 1s ease-in .2s"; "all .2s linear .1s" ];
      negatives = [ "1s 2s 3s"; "ease opacity ease" ];
    };
    {
      property = "transition-behavior";
      positives = [ "normal"; "allow-discrete" ];
      negatives = [ "normal allow-discrete"; "discrete" ];
    };
    {
      property = "animation";
      positives = [ "fade 1s linear 2 alternate both running"; "none" ];
      negatives = [ "1s 2s 3s"; "infinite infinite" ];
    };
    {
      property = "grid-auto-flow";
      positives = [ "row"; "column"; "row dense"; "dense" ];
      negatives = [ "row column"; "dense dense" ];
    };
    {
      property = "gap";
      positives = [ "0"; "1rem"; "1rem 2rem" ];
      negatives = [ "1rem 2rem 3rem"; "red" ];
    };
    {
      property = "flex";
      positives = [ "none"; "auto"; "1"; "1 1 0" ];
      negatives = [ "1 1 1 1"; "row wrap" ];
    };
    {
      property = "place-content";
      positives = [ "center"; "center space-between"; "start stretch" ];
      negatives = [ "center center center"; "left right" ];
    };
    {
      property = "place-items";
      positives = [ "start stretch"; "center"; "normal" ];
      negatives = [ "start center end"; "left right" ];
    };
    {
      property = "list-style";
      positives = [ "square inside"; "none"; "url(marker.png) outside" ];
      negatives = [ "inside outside"; "square disc" ];
    };
    {
      property = "content";
      positives =
        [ "normal"; "\"hello\""; "open-quote attr(title) close-quote" ];
      negatives = [ "attr()"; "open-quote close-quote none" ];
    };
  ]

let property_grammar_rows properties positives negatives =
  List.map (fun property -> { property; positives; negatives }) properties

let property_grammar_matrix =
  property_grammar_matrix
  @ property_grammar_rows
      [
        "background-color";
        "border-color";
        "border-top-color";
        "border-right-color";
        "border-bottom-color";
        "border-left-color";
        "border-inline-start-color";
        "border-inline-end-color";
        "text-decoration-color";
        "-webkit-text-decoration-color";
        "-webkit-tap-highlight-color";
        "outline-color";
        "fill";
        "stroke";
        "accent-color";
        "caret-color";
      ]
      [ "red"; "currentColor"; "rgb(0 0 0 / 50%)" ]
      [ "1px"; "red blue" ]
  @ property_grammar_rows
      [
        "border-style";
        "border-top-style";
        "border-right-style";
        "border-bottom-style";
        "border-left-style";
        "border-inline-style";
        "border-block-style";
      ]
      [ "none"; "solid"; "dashed"; "hidden" ]
      [ "solid dashed"; "foo" ]
  @ property_grammar_rows
      [
        "padding-left";
        "padding-right";
        "padding-bottom";
        "padding-top";
        "padding-inline-start";
        "padding-inline-end";
        "padding-block-start";
        "padding-block-end";
        "border-top-left-radius";
        "border-top-right-radius";
        "border-bottom-left-radius";
        "border-bottom-right-radius";
        "border-start-start-radius";
        "border-start-end-radius";
        "border-end-start-radius";
        "border-end-end-radius";
        "stroke-width";
        "outline-width";
        "outline-offset";
        "text-decoration-thickness";
        "text-underline-offset";
        "letter-spacing";
        "text-indent";
        "word-spacing";
        "line-height-step";
        "overflow-clip-margin";
        "offset-distance";
        "perspective";
        "shape-margin";
      ]
      [ "0"; "1px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px" ]
  @ property_grammar_rows
      [ "margin-left"; "margin-right"; "margin-top"; "margin-bottom" ]
      [ "0"; "1px"; "10%"; "auto"; "anchor-size(width)" ]
      [ "red"; "1px 2px" ]
  @ property_grammar_rows
      [ "padding-inline"; "padding-block"; "column-gap"; "row-gap" ]
      [ "0"; "1rem"; "10%" ]
      [ "auto"; "1px 2px 3px"; "red" ]
  @ property_grammar_rows
      [ "margin-inline"; "margin-block"; "scroll-margin-block" ]
      [ "0"; "1px"; "1px 2px"; "auto" ]
      [ "red"; "1px 2px 3px" ]
  @ property_grammar_rows
      [
        "margin-inline-start";
        "margin-inline-end";
        "margin-block-start";
        "margin-block-end";
        "scroll-margin";
        "scroll-margin-top";
        "scroll-margin-right";
        "scroll-margin-bottom";
        "scroll-margin-left";
        "scroll-margin-inline";
        "scroll-margin-inline-start";
        "scroll-margin-inline-end";
        "scroll-margin-block-start";
        "scroll-margin-block-end";
        "scroll-padding";
        "scroll-padding-top";
        "scroll-padding-right";
        "scroll-padding-bottom";
        "scroll-padding-left";
        "scroll-padding-inline";
        "scroll-padding-inline-start";
        "scroll-padding-inline-end";
        "scroll-padding-block";
        "scroll-padding-block-start";
        "scroll-padding-block-end";
      ]
      [ "0"; "1px"; "10%" ]
      [ "red"; "1px 2px 3px 4px 5px" ]
  @ property_grammar_rows
      [
        "height";
        "min-width";
        "min-height";
        "max-width";
        "max-height";
        "inline-size";
        "min-inline-size";
        "max-inline-size";
        "block-size";
        "min-block-size";
        "max-block-size";
        "flex-basis";
      ]
      [ "auto"; "10%"; "min-content"; "fit-content(20rem)" ]
      [ "red"; "fit-content()"; "1px 2px" ]
  @ property_grammar_rows
      [
        "border-width";
        "border-top-width";
        "border-right-width";
        "border-bottom-width";
        "border-left-width";
        "border-inline-start-width";
        "border-inline-end-width";
        "border-block-start-width";
        "border-block-end-width";
      ]
      [ "thin"; "medium"; "thick"; "1px" ]
      [ "auto"; "thin medium thick 1px 2px"; "red" ]
  @ property_grammar_rows
      [
        "inset";
        "inset-inline";
        "inset-block";
        "top";
        "right";
        "bottom";
        "left";
        "inset-inline-start";
        "inset-inline-end";
        "inset-block-start";
        "inset-block-end";
      ]
      [ "auto"; "1px"; "10%"; "1px 2px 3px 4px" ]
      [ "red"; "1px 2px 3px 4px 5px" ]
  @ property_grammar_rows
      [ "overflow-x"; "overflow-y" ]
      [ "visible"; "hidden"; "clip"; "auto"; "scroll" ]
      [ "none"; "visible hidden" ]
  @ property_grammar_rows
      [
        "backdrop-filter";
        "-webkit-backdrop-filter";
        "-webkit-filter";
        "-ms-filter";
      ]
      [ "none"; "blur(5px)"; "contrast(120%) brightness(.8)" ]
      [ "blur()"; "none blur(1px)" ]
  @ property_grammar_rows
      [
        "transition-duration";
        "transition-delay";
        "animation-duration";
        "animation-delay";
      ]
      [ "0s"; ".2s"; "120ms" ] [ "1px"; "1s 2s" ]
  @ property_grammar_rows
      [ "transition-timing-function"; "animation-timing-function" ]
      [ "ease"; "linear"; "steps(4, jump-end)"; "cubic-bezier(.1,.2,.3,.4)" ]
      [ "steps()"; "cubic-bezier(1, 2)" ]
  @ property_grammar_rows
      [ "background-origin"; "background-clip"; "-webkit-background-clip" ]
      [ "border-box"; "padding-box"; "content-box" ]
      [ "margin-box"; "border-box padding-box content-box content-box" ]
  @ property_grammar_rows
      [ "background-repeat"; "mask-repeat"; "-webkit-mask-repeat" ]
      [ "repeat"; "no-repeat"; "repeat-x"; "space round" ]
      [ "repeat no-repeat space"; "foo" ]
  @ property_grammar_rows
      [
        "background-position";
        "mask-position";
        "-webkit-mask-position";
        "object-position";
      ]
      [ "center"; "left 10px top 20px"; "10% 20%" ]
      [ "left top center"; "foo" ]
  @ property_grammar_rows
      [ "mask-size"; "-webkit-mask-size" ]
      [ "auto"; "cover"; "contain"; "10px 20%" ]
      [ "cover contain"; "-1px" ]
  @ property_grammar_rows
      [ "mask-image"; "-webkit-mask-image" ]
      [ "none"; "url(mask.png)"; "linear-gradient(red, blue)" ]
      [ "linear-gradient()"; "image-set()" ]
  @ [
      {
        property = "list-style-type";
        positives = [ "disc"; "square"; "decimal"; "\"-\"" ];
        negatives = [ "disc square"; "url(marker.png)" ];
      };
      {
        property = "list-style-position";
        positives = [ "inside"; "outside" ];
        negatives = [ "inside outside"; "center" ];
      };
      {
        property = "list-style-image";
        positives = [ "none"; "url(marker.png)" ];
        negatives = [ "square"; "url(marker.png) none" ];
      };
    ]
  @ [
      {
        property = "font-size";
        positives = [ "medium"; "larger"; "12px"; "clamp(1rem, 2vw, 2rem)" ];
        negatives = [ "red"; "12px 14px" ];
      };
      {
        property = "line-height";
        positives = [ "normal"; "1.5"; "12px"; "120%" ];
        negatives = [ "red"; "1 2" ];
      };
      {
        property = "font-style";
        positives = [ "normal"; "italic"; "oblique"; "oblique 20deg" ];
        negatives = [ "italic normal"; "oblique 20px" ];
      };
      {
        property = "font-stretch";
        positives = [ "normal"; "condensed"; "expanded"; "75%" ];
        negatives = [ "75px"; "normal condensed" ];
      };
      {
        property = "font-variation-settings";
        positives = [ "normal"; "\"wght\" 650"; "\"wdth\" 75, \"wght\" 650" ];
        negatives = [ "\"wght\""; "wght 650" ];
      };
      {
        property = "font-variant-numeric";
        positives = [ "normal"; "tabular-nums"; "lining-nums slashed-zero" ];
        negatives = [ "normal tabular-nums"; "tabular-nums tabular-nums" ];
      };
      {
        property = "font-size-adjust";
        positives = [ "none"; "0.5"; "ex-height 0.5" ];
        negatives = [ "auto"; "ex-height" ];
      };
      {
        property = "font-variant-emoji";
        positives = [ "normal"; "text"; "emoji"; "unicode" ];
        negatives = [ "text emoji"; "auto" ];
      };
      {
        property = "text-align";
        positives = [ "start"; "end"; "center"; "match-parent" ];
        negatives = [ "top"; "left right" ];
      };
      {
        property = "text-decoration-line";
        positives = [ "none"; "underline"; "underline overline line-through" ];
        negatives = [ "none underline"; "underline underline" ];
      };
      {
        property = "text-decoration-style";
        positives = [ "solid"; "double"; "dotted"; "wavy" ];
        negatives = [ "solid wavy"; "none" ];
      };
      {
        property = "text-transform";
        positives = [ "none"; "capitalize"; "uppercase"; "full-width" ];
        negatives = [ "uppercase lowercase"; "auto" ];
      };
      {
        property = "text-overflow";
        positives = [ "clip"; "ellipsis"; "\"...\""; "clip ellipsis" ];
        negatives = [ "clip ellipsis clip"; "auto" ];
      };
      {
        property = "text-wrap";
        positives = [ "wrap"; "nowrap"; "balance"; "pretty" ];
        negatives = [ "wrap nowrap"; "auto" ];
      };
      {
        property = "text-wrap-style";
        positives = [ "auto"; "balance"; "pretty"; "stable" ];
        negatives = [ "balance pretty"; "wrap" ];
      };
      {
        property = "text-box-trim";
        positives = [ "none"; "trim-both"; "trim-start"; "trim-end" ];
        negatives = [ "trim-start trim-end"; "auto" ];
      };
      {
        property = "text-spacing-trim";
        positives = [ "normal"; "space-all"; "trim-start"; "space-first" ];
        negatives = [ "normal trim-start"; "auto" ];
      };
      {
        property = "hyphenate-limit-chars";
        positives = [ "auto"; "6"; "6 3"; "6 3 2" ];
        negatives = [ "1 2 3 4"; "red" ];
      };
      {
        property = "initial-letter";
        positives = [ "normal"; "2"; "2 3" ];
        negatives = [ "2 3 4"; "auto" ];
      };
      {
        property = "visibility";
        positives = [ "visible"; "hidden"; "collapse" ];
        negatives = [ "none"; "visible hidden" ];
      };
      {
        property = "flex-direction";
        positives = [ "row"; "row-reverse"; "column"; "column-reverse" ];
        negatives = [ "row column"; "horizontal" ];
      };
      {
        property = "flex-wrap";
        positives = [ "nowrap"; "wrap"; "wrap-reverse" ];
        negatives = [ "wrap nowrap"; "reverse" ];
      };
      {
        property = "flex-grow";
        positives = [ "0"; "1"; "2.5" ];
        negatives = [ "-1"; "1 2" ];
      };
      {
        property = "flex-shrink";
        positives = [ "0"; "1"; "2.5" ];
        negatives = [ "-1"; "1 2" ];
      };
      {
        property = "order";
        positives = [ "0"; "-1"; "calc(1 + 2)" ];
        negatives = [ "1.5"; "red" ];
      };
      {
        property = "align-items";
        positives = [ "normal"; "stretch"; "first baseline"; "safe center" ];
        negatives = [ "left"; "safe unsafe center" ];
      };
      {
        property = "align-self";
        positives = [ "auto"; "normal"; "stretch"; "unsafe flex-end" ];
        negatives = [ "left"; "auto center" ];
      };
      {
        property = "align-content";
        positives = [ "normal"; "space-between"; "safe center"; "baseline" ];
        negatives = [ "auto"; "space-between stretch center" ];
      };
      {
        property = "justify-content";
        positives = [ "normal"; "space-evenly"; "unsafe right"; "stretch" ];
        negatives = [ "auto"; "left right" ];
      };
      {
        property = "justify-items";
        positives = [ "normal"; "stretch"; "legacy left"; "safe center" ];
        negatives = [ "space-between"; "legacy safe left" ];
      };
      {
        property = "justify-self";
        positives = [ "auto"; "normal"; "stretch"; "safe end" ];
        negatives = [ "space-between"; "auto end" ];
      };
      {
        property = "place-self";
        positives = [ "auto"; "center"; "start end" ];
        negatives = [ "start center end"; "left right" ];
      };
      {
        property = "grid-template-columns";
        positives = [ "none"; "subgrid"; "repeat(3, 1fr)"; "minmax(0, 1fr)" ];
        negatives = [ "repeat()"; "subgrid none" ];
      };
      {
        property = "grid-template-rows";
        positives = [ "none"; "subgrid"; "masonry"; "100px 1fr" ];
        negatives = [ "masonry subgrid"; "repeat()" ];
      };
      {
        property = "grid-template-areas";
        positives = [ "none"; "\"a a\" \"b c\"" ];
        negatives = [ "\"a\" \"a a\""; "a b" ];
      };
      {
        property = "grid-template";
        positives = [ "none"; "\"a a\" 1fr / 1fr 1fr"; "subgrid / subgrid" ];
        negatives = [ "/"; "none / 1fr" ];
      };
      {
        property = "grid-area";
        positives = [ "auto"; "header"; "1 / 2 / span 3 / 4" ];
        negatives = [ "1 / 2 / 3 / 4 / 5"; "/" ];
      };
      {
        property = "grid-auto-columns";
        positives = [ "auto"; "minmax(0, 1fr)"; "100px" ];
        negatives = [ "subgrid"; "repeat()" ];
      };
      {
        property = "grid-auto-rows";
        positives = [ "auto"; "minmax(0, 1fr)"; "100px" ];
        negatives = [ "subgrid"; "repeat()" ];
      };
      {
        property = "grid-column";
        positives = [ "auto"; "1 / span 2"; "header-start / header-end" ];
        negatives = [ "1 / 2 / 3"; "/" ];
      };
      {
        property = "grid-row";
        positives = [ "auto"; "1 / span 2"; "row-start / row-end" ];
        negatives = [ "1 / 2 / 3"; "/" ];
      };
      {
        property = "grid-column-start";
        positives = [ "auto"; "1"; "span 2"; "header-start" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "grid-column-end";
        positives = [ "auto"; "1"; "span 2"; "header-end" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "grid-row-start";
        positives = [ "auto"; "1"; "span 2"; "row-start" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "grid-row-end";
        positives = [ "auto"; "1"; "span 2"; "row-end" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "box-shadow";
        positives = [ "none"; "0 1px 2px rgb(0 0 0 / .2)"; "inset 0 0 1px red" ];
        negatives = [ "0 0"; "inset inset 0 0 1px" ];
      };
      {
        property = "mix-blend-mode";
        positives = [ "normal"; "multiply"; "screen"; "plus-lighter" ];
        negatives = [ "normal multiply"; "foo" ];
      };
      {
        property = "cursor";
        positives = [ "auto"; "pointer"; "url(cursor.png), pointer" ];
        negatives = [ "url(cursor.png)"; "pointer auto" ];
      };
      {
        property = "table-layout";
        positives = [ "auto"; "fixed" ];
        negatives = [ "fixed auto"; "block" ];
      };
      {
        property = "border-collapse";
        positives = [ "collapse"; "separate" ];
        negatives = [ "collapse separate"; "none" ];
      };
      {
        property = "border-spacing";
        positives = [ "0"; "1px 2px" ];
        negatives = [ "1px 2px 3px"; "auto" ];
      };
      {
        property = "user-select";
        positives = [ "auto"; "text"; "none"; "all" ];
        negatives = [ "text none"; "contain" ];
      };
      {
        property = "-webkit-user-select";
        positives = [ "auto"; "text"; "none"; "all" ];
        negatives = [ "text none"; "contain" ];
      };
      {
        property = "pointer-events";
        positives = [ "auto"; "none"; "visiblePainted"; "all" ];
        negatives = [ "auto none"; "visible-painted" ];
      };
      {
        property = "z-index";
        positives = [ "auto"; "0"; "-1"; "calc(1 + 2)" ];
        negatives = [ "1.5"; "0 1" ];
      };
      {
        property = "outline";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "outline-style";
        positives = [ "none"; "auto"; "solid"; "dotted" ];
        negatives = [ "solid dotted"; "foo" ];
      };
      {
        property = "forced-color-adjust";
        positives = [ "auto"; "none"; "preserve-parent-color" ];
        negatives = [ "auto none"; "preserve" ];
      };
      {
        property = "clip";
        positives = [ "auto"; "rect(0, 10px, 10px, 0)" ];
        negatives = [ "rect(0, 1px)"; "inset(1px)" ];
      };
      {
        property = "clear";
        positives = [ "none"; "left"; "right"; "both"; "inline-start" ];
        negatives = [ "left right"; "center" ];
      };
      {
        property = "tab-size";
        positives = [ "4"; "8"; "2ch" ];
        negatives = [ "-1"; "4 8" ];
      };
      {
        property = "-webkit-text-size-adjust";
        positives = [ "auto"; "none"; "100%" ];
        negatives = [ "auto none"; "1px" ];
      };
      {
        property = "text-size-adjust";
        positives = [ "auto"; "none"; "100%" ];
        negatives = [ "auto none"; "1px" ];
      };
      {
        property = "-webkit-text-decoration";
        positives = [ "underline"; "underline wavy red 2px" ];
        negatives = [ "underline none"; "wavy solid" ];
      };
      {
        property = "-webkit-appearance";
        positives = [ "none"; "auto"; "button"; "textfield" ];
        negatives = [ "none auto"; "foo" ];
      };
      {
        property = "-moz-appearance";
        positives = [ "none"; "auto"; "button"; "textfield" ];
        negatives = [ "none auto"; "foo" ];
      };
      {
        property = "appearance";
        positives = [ "none"; "auto"; "base-select"; "textfield" ];
        negatives = [ "none auto"; "foo" ];
      };
      {
        property = "container-name";
        positives = [ "none"; "card"; "card layout" ];
        negatives = [ "default"; "card / layout" ];
      };
      {
        property = "anchor-name";
        positives = [ "none"; "--anchor"; "--a, --b" ];
        negatives = [ "anchor"; "--a --b" ];
      };
      {
        property = "position-anchor";
        positives = [ "auto"; "--anchor" ];
        negatives = [ "anchor"; "--a --b" ];
      };
      {
        property = "position-try-fallbacks";
        positives = [ "none"; "--fallback"; "flip-block, --fallback" ];
        negatives = [ "flip-block --fallback"; "," ];
      };
      {
        property = "overflow-anchor";
        positives = [ "auto"; "none" ];
        negatives = [ "auto none"; "hidden" ];
      };
      {
        property = "scrollbar-width";
        positives = [ "auto"; "thin"; "none" ];
        negatives = [ "thin auto"; "1px" ];
      };
      {
        property = "scrollbar-color";
        positives = [ "auto"; "red blue" ];
        negatives = [ "red"; "red blue green" ];
      };
      {
        property = "scrollbar-gutter";
        positives = [ "auto"; "stable"; "stable both-edges" ];
        negatives = [ "both-edges"; "stable stable" ];
      };
      {
        property = "font-palette";
        positives = [ "normal"; "light"; "dark"; "--brand" ];
        negatives = [ "normal light"; "brand" ];
      };
      {
        property = "font-synthesis";
        positives = [ "none"; "weight"; "style small-caps position" ];
        negatives = [ "none weight"; "weight weight" ];
      };
      {
        property = "animation-timeline";
        positives = [ "auto"; "none"; "scroll()"; "--timeline" ];
        negatives = [ "auto none"; "scroll(" ];
      };
      {
        property = "animation-range";
        positives = [ "normal"; "entry 10% exit 90%"; "cover 0% 100%" ];
        negatives = [ "entry exit cover"; "10% 20% 30%" ];
      };
      {
        property = "view-transition-name";
        positives = [ "none"; "card"; "match-element" ];
        negatives = [ "card card"; "auto" ];
      };
      {
        property = "image-orientation";
        positives = [ "none"; "from-image" ];
        negatives = [ "90deg"; "from-image none" ];
      };
      {
        property = "contain-intrinsic-size";
        positives = [ "none"; "auto 300px"; "100px 200px" ];
        negatives = [ "auto"; "1px 2px 3px" ];
      };
      {
        property = "margin-trim";
        positives = [ "none"; "block"; "inline"; "block-start block-end" ];
        negatives = [ "none block"; "block block" ];
      };
      {
        property = "mask-mode";
        positives = [ "match-source"; "alpha"; "luminance"; "alpha, luminance" ];
        negatives = [ "match-source alpha luminance"; "foo" ];
      };
      {
        property = "offset-path";
        positives =
          [ "none"; "path(\"M 0 0 L 1 1\")"; "ray(45deg closest-side)" ];
        negatives = [ "path()"; "ray()" ];
      };
      {
        property = "view-timeline-name";
        positives = [ "none"; "--timeline"; "--a, --b" ];
        negatives = [ "timeline"; "--a --b" ];
      };
      {
        property = "view-timeline-axis";
        positives = [ "block"; "inline"; "x"; "y" ];
        negatives = [ "block inline"; "z" ];
      };
      {
        property = "timeline-scope";
        positives = [ "none"; "--timeline"; "--a, --b" ];
        negatives = [ "timeline"; "--a --b" ];
      };
      {
        property = "perspective-origin";
        positives = [ "center"; "left top"; "10px 20%" ];
        negatives = [ "left right"; "top bottom" ];
      };
      {
        property = "transform-style";
        positives = [ "flat"; "preserve-3d" ];
        negatives = [ "flat preserve-3d"; "none" ];
      };
      {
        property = "backface-visibility";
        positives = [ "visible"; "hidden" ];
        negatives = [ "visible hidden"; "none" ];
      };
      {
        property = "transition-property";
        positives = [ "none"; "all"; "opacity, transform" ];
        negatives = [ "none, opacity"; "all, opacity" ];
      };
      {
        property = "will-change";
        positives =
          [ "auto"; "scroll-position"; "contents"; "opacity, transform" ];
        negatives = [ "auto, opacity"; "will-change" ];
      };
      {
        property = "isolation";
        positives = [ "auto"; "isolate" ];
        negatives = [ "auto isolate"; "none" ];
      };
      {
        property = "break-before";
        positives = [ "auto"; "avoid"; "page"; "recto" ];
        negatives = [ "avoid page"; "none" ];
      };
      {
        property = "break-after";
        positives = [ "auto"; "avoid"; "page"; "verso" ];
        negatives = [ "avoid page"; "none" ];
      };
      {
        property = "break-inside";
        positives = [ "auto"; "avoid"; "avoid-page"; "avoid-column" ];
        negatives = [ "avoid page"; "none" ];
      };
      {
        property = "columns";
        positives = [ "auto"; "12em"; "3"; "12em 3" ];
        negatives = [ "3 4"; "red" ];
      };
      {
        property = "background-attachment";
        positives = [ "scroll"; "fixed"; "local"; "scroll, fixed" ];
        negatives = [ "scroll fixed"; "none" ];
      };
      {
        property = "border-top";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "border-right";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "border-bottom";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "border-left";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "transform-origin";
        positives = [ "center"; "left top"; "left 10px top 20px"; "10px 20px" ];
        negatives = [ "left right"; "top bottom" ];
      };
      {
        property = "transform-box";
        positives = [ "content-box"; "border-box"; "fill-box"; "view-box" ];
        negatives = [ "margin-box"; "content-box border-box" ];
      };
      {
        property = "text-shadow";
        positives = [ "none"; "1px 1px black"; "0 1px 2px red, 0 0 1px blue" ];
        negatives = [ "1px"; "red blue" ];
      };
      {
        property = "mask";
        positives = [ "none"; "url(mask.png) no-repeat center / contain" ];
        negatives = [ "url("; "red blue" ];
      };
      {
        property = "content-visibility";
        positives = [ "visible"; "auto"; "hidden" ];
        negatives = [ "visible hidden"; "none" ];
      };
      {
        property = "animation-name";
        positives = [ "none"; "fade"; "fade, slide" ];
        negatives = [ "initial fade"; "," ];
      };
      {
        property = "animation-iteration-count";
        positives = [ "infinite"; "1"; "2.5"; "1, infinite" ];
        negatives = [ "-1"; "infinite infinite" ];
      };
      {
        property = "animation-direction";
        positives = [ "normal"; "reverse"; "alternate"; "alternate-reverse" ];
        negatives = [ "normal reverse"; "forwards" ];
      };
      {
        property = "animation-fill-mode";
        positives = [ "none"; "forwards"; "backwards"; "both" ];
        negatives = [ "none forwards"; "running" ];
      };
      {
        property = "animation-play-state";
        positives = [ "running"; "paused"; "running, paused" ];
        negatives = [ "running paused"; "none" ];
      };
      {
        property = "background-blend-mode";
        positives = [ "normal"; "multiply"; "screen, overlay" ];
        negatives = [ "normal multiply"; "foo" ];
      };
      {
        property = "vertical-align";
        positives = [ "baseline"; "sub"; "text-top"; "10%" ];
        negatives = [ "baseline sub"; "red" ];
      };
      {
        property = "-webkit-font-smoothing";
        positives = [ "auto"; "none"; "antialiased"; "subpixel-antialiased" ];
        negatives = [ "auto none"; "smooth" ];
      };
      {
        property = "-moz-osx-font-smoothing";
        positives = [ "auto"; "grayscale" ];
        negatives = [ "auto grayscale"; "antialiased" ];
      };
      {
        property = "-webkit-line-clamp";
        positives = [ "none"; "3" ];
        negatives = [ "0"; "3 4" ];
      };
      {
        property = "-webkit-box-orient";
        positives = [ "horizontal"; "vertical"; "inline-axis"; "block-axis" ];
        negatives = [ "horizontal vertical"; "row" ];
      };
      {
        property = "overflow-wrap";
        positives = [ "normal"; "break-word"; "anywhere" ];
        negatives = [ "normal anywhere"; "break-all" ];
      };
      {
        property = "hyphens";
        positives = [ "none"; "manual"; "auto" ];
        negatives = [ "manual auto"; "normal" ];
      };
      {
        property = "-webkit-hyphens";
        positives = [ "none"; "manual"; "auto" ];
        negatives = [ "manual auto"; "normal" ];
      };
      {
        property = "-webkit-mask-composite";
        positives = [ "source-over"; "xor"; "source-in, source-out" ];
        negatives = [ "add"; "source-over xor" ];
      };
      {
        property = "-webkit-mask-source-type";
        positives = [ "auto"; "alpha"; "luminance" ];
        negatives = [ "alpha luminance"; "match-source" ];
      };
      {
        property = "-webkit-mask-clip";
        positives = [ "border-box"; "padding-box"; "content-box"; "no-clip" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "-webkit-mask-origin";
        positives = [ "border-box"; "padding-box"; "content-box" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "mask-composite";
        positives = [ "add"; "subtract"; "intersect"; "exclude" ];
        negatives = [ "source-over"; "add subtract" ];
      };
      {
        property = "mask-clip";
        positives = [ "border-box"; "padding-box"; "content-box"; "no-clip" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "mask-origin";
        positives = [ "border-box"; "padding-box"; "content-box" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "mask-type";
        positives = [ "alpha"; "luminance" ];
        negatives = [ "match-source"; "alpha luminance" ];
      };
      {
        property = "scroll-behavior";
        positives = [ "auto"; "smooth" ];
        negatives = [ "auto smooth"; "none" ];
      };
      {
        property = "field-sizing";
        positives = [ "fixed"; "content" ];
        negatives = [ "auto"; "fixed content" ];
      };
      {
        property = "caption-side";
        positives = [ "top"; "bottom" ];
        negatives = [ "left"; "top bottom" ];
      };
      {
        property = "resize";
        positives = [ "none"; "both"; "horizontal"; "block" ];
        negatives = [ "horizontal vertical"; "auto" ];
      };
      {
        property = "object-fit";
        positives = [ "fill"; "contain"; "cover"; "scale-down" ];
        negatives = [ "contain cover"; "auto" ];
      };
      {
        property = "color-scheme";
        positives = [ "normal"; "light"; "dark"; "only light" ];
        negatives = [ "normal light"; "only" ];
      };
      {
        property = "print-color-adjust";
        positives = [ "economy"; "exact" ];
        negatives = [ "economy exact"; "auto" ];
      };
      {
        property = "box-decoration-break";
        positives = [ "slice"; "clone" ];
        negatives = [ "slice clone"; "auto" ];
      };
      {
        property = "-webkit-box-decoration-break";
        positives = [ "slice"; "clone" ];
        negatives = [ "slice clone"; "auto" ];
      };
      {
        property = "quotes";
        positives = [ "auto"; "none"; "\"<\" \">\" \"'\" \"'\"" ];
        negatives = [ "\"<\""; "auto none" ];
      };
      {
        property = "touch-action";
        positives = [ "auto"; "none"; "pan-x pinch-zoom"; "manipulation" ];
        negatives = [ "auto none"; "pan-x pan-left" ];
      };
      {
        property = "direction";
        positives = [ "ltr"; "rtl" ];
        negatives = [ "ltr rtl"; "auto" ];
      };
      {
        property = "unicode-bidi";
        positives = [ "normal"; "embed"; "isolate"; "plaintext" ];
        negatives = [ "normal isolate"; "auto" ];
      };
      {
        property = "text-decoration-skip-ink";
        positives = [ "auto"; "none"; "all" ];
        negatives = [ "auto none"; "skip" ];
      };
      {
        property = "overscroll-behavior";
        positives = [ "auto"; "contain"; "none"; "contain none" ];
        negatives = [ "contain auto none"; "hidden" ];
      };
      {
        property = "overscroll-behavior-x";
        positives = [ "auto"; "contain"; "none" ];
        negatives = [ "contain none"; "hidden" ];
      };
      {
        property = "overscroll-behavior-y";
        positives = [ "auto"; "contain"; "none" ];
        negatives = [ "contain none"; "hidden" ];
      };
      {
        property = "-webkit-transform";
        positives = [ "none"; "translateX(10px) rotate(45deg)" ];
        negatives = [ "translate()"; "none rotate(1deg)" ];
      };
      {
        property = "-webkit-transition";
        positives = [ "opacity 1s ease-in .2s"; "all .2s linear .1s" ];
        negatives = [ "1s 2s 3s"; "ease opacity ease" ];
      };
      {
        property = "-o-transition";
        positives = [ "opacity 1s ease-in .2s"; "all .2s linear .1s" ];
        negatives = [ "1s 2s 3s"; "ease opacity ease" ];
      };
    ]

let spec_property_grammar_manifest () =
  let unique_properties =
    List.sort_uniq String.compare
      (List.map (fun row -> row.property) property_grammar_matrix)
  in
  if List.length unique_properties <> List.length property_grammar_matrix then
    Alcotest.fail "property grammar manifest has duplicate property rows";
  Alcotest.(check int)
    "property grammar manifest covers every unique public property name" 346
    (List.length unique_properties);
  let parse_decl property value =
    let input = property ^ ":" ^ value in
    let c = Css.Cursor.of_string input in
    match read_declaration c with
    | None -> None
    | Some decl ->
        let serialized =
          Css.Declaration.string_of_declaration ~minify:true decl
        in
        let c2 = Css.Cursor.of_string serialized in
        Some (input, serialized, decl, read_declaration c2)
  in
  let check_positive row value =
    match parse_decl row.property value with
    | Some (_input, serialized, decl, Some reparsed)
      when Css.Declaration.property_name reparsed = row.property
           && decl = reparsed ->
        ignore serialized
    | Some (input, serialized, _, _) ->
        Alcotest.failf
          "%s positive vector serialized to unparsable or structurally \
           different declaration: %s -> %s"
          row.property input serialized
    | None ->
        Alcotest.failf "%s positive vector rejected: %s" row.property value
  in
  let check_negative row value =
    match parse_decl row.property value with
    | None -> ()
    | Some (input, serialized, _, _) ->
        Alcotest.failf "%s negative vector parsed: %s -> %s" row.property input
          serialized
  in
  let check_css_wide row keyword =
    match parse_decl row.property keyword with
    | Some (_, _, decl, Some reparsed)
      when Css.Declaration.property_name reparsed = row.property
           && decl = reparsed ->
        ()
    | Some (input, serialized, _, _) ->
        Alcotest.failf
          "%s CSS-wide keyword did not structurally reparse: %s -> %s"
          row.property input serialized
    | None ->
        Alcotest.failf "%s CSS-wide keyword rejected: %s" row.property keyword
  in
  List.iter
    (fun row ->
      if row.positives = [] then
        Alcotest.failf "%s has no positive grammar vectors" row.property;
      if row.negatives = [] then
        Alcotest.failf "%s has no negative grammar vectors" row.property;
      List.iter (check_positive row) row.positives;
      List.iter (check_negative row) row.negatives;
      List.iter (check_css_wide row)
        [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ])
    property_grammar_matrix

let declaration_tests =
  [
    (* Core declaration type testing *)
    test_case "declaration" `Quick test_declaration;
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
      spec_declaration_more_grammar_vectors;
    test_case "spec property grammar manifest" `Quick
      spec_property_grammar_manifest;
    (* Error handling *)
    test_case "error missing colon" `Quick error_missing_colon;
    test_case "error stray semicolon" `Quick error_stray_semicolon;
    test_case "error unclosed block" `Quick error_unclosed_block;
    test_case "unterminated parsing" `Quick unterminated;
    test_case "invalid declarations" `Quick invalid;
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
