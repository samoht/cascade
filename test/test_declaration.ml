(** Tests for CSS Declaration parsing *)

open Alcotest
open Css.Declaration

let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Helper for roundtrip testing of declarations *)
let check_declaration ?expected input =
  let expected = Option.value ~default:input expected in
  let r = Css.Reader.of_string input in
  match read_declaration r with
  | Some (name, value, important) ->
      let reconstructed =
        if important then Printf.sprintf "%s:%s!important" name value
        else Printf.sprintf "%s:%s" name value
      in
      check string (Fmt.str "declaration %s" input) expected reconstructed
  | None -> fail (Printf.sprintf "Failed to parse declaration: %s" input)

let check_declaration_parts input exp_name exp_value exp_important =
  let r = Css.Reader.of_string input in
  match read_declaration r with
  | Some (name, value, important) ->
      check string
        (Fmt.str "property name %s" input)
        exp_name (String.trim name);
      check string
        (Fmt.str "property value %s" input)
        exp_value (String.trim value);
      check bool (Fmt.str "important flag %s" input) exp_important important
  | None -> fail (Printf.sprintf "Failed to parse declaration: %s" input)

let check_declarations input expected_count =
  let r = Css.Reader.of_string input in
  let decls = read_declarations r in
  check int
    (Fmt.str "declaration count %s" input)
    expected_count (List.length decls);
  decls

let check_block input expected_count =
  let r = Css.Reader.of_string input in
  let decls = read_block r in
  check int
    (Fmt.str "block declaration count %s" input)
    expected_count (List.length decls);
  decls

let test_simple_declarations () =
  (* Basic declarations *)
  check_declaration_parts "color: red;" "color" "red" false;
  check_declaration_parts "margin: 10px;" "margin" "10px" false;
  check_declaration_parts "padding: 5px;" "padding" "5px" false;

  (* With whitespace *)
  check_declaration_parts "  color  :  red  ;  " "color" "red" false;
  check_declaration_parts "\tmargin\t:\t10px\t;\t" "margin" "10px" false

let test_important_declarations () =
  check_declaration_parts "color: red !important;" "color" "red" true;
  check_declaration_parts "margin: 10px !important;" "margin" "10px" true;
  check_declaration_parts "padding: 5px !important;" "padding" "5px" true;

  (* With spacing variations *)
  check_declaration_parts "color: red!important;" "color" "red" true;
  check_declaration_parts "color: red ! important;" "color" "red" true;
  check_declaration_parts "color: red   !   important   ;" "color" "red" true

let test_complex_values () =
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

let test_quoted_strings () =
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

let test_custom_properties () =
  check_declaration_parts "--color: red;" "--color" "red" false;
  check_declaration_parts "--my-var: 10px;" "--my-var" "10px" false;
  check_declaration_parts "--complex: var(--other, 10px);" "--complex"
    "var(--other, 10px)" false;
  check_declaration_parts "--important: value !important;" "--important" "value"
    true

let test_vendor_prefixes () =
  check_declaration_parts "-webkit-transform: rotate(45deg);"
    "-webkit-transform" "rotate(45deg)" false;
  check_declaration_parts "-moz-appearance: none;" "-moz-appearance" "none"
    false;
  check_declaration_parts "-ms-filter: blur(5px);" "-ms-filter" "blur(5px)"
    false;
  check_declaration_parts "-o-transition: all 0.3s;" "-o-transition" "all 0.3s"
    false

let test_multiple_declarations () =
  (* Basic multiple declarations *)
  let decls = check_declarations "color: red; margin: 10px;" 2 in
  let n1, v1, i1 = List.nth decls 0 in
  check string "first property" "color" n1;
  check string "first value" "red" v1;
  check bool "first not important" false i1;
  let n2, v2, i2 = List.nth decls 1 in
  check string "second property" "margin" n2;
  check string "second value" "10px" v2;
  check bool "second not important" false i2;

  (* Mixed important and normal *)
  let decls =
    check_declarations "color: red; margin: 10px !important; padding: 5px;" 3
  in
  let _, _, i2 = List.nth decls 1 in
  check bool "second is important" true i2

let test_declaration_blocks () =
  (* Basic block *)
  let decls = check_block "{ color: blue; display: block; }" 2 in
  let n1, v1, _ = List.nth decls 0 in
  check string "first property" "color" n1;
  check string "first value" "blue" v1;

  (* Block with important *)
  let decls = check_block "{ padding: 10px !important; margin: auto; }" 2 in
  let n1, _, i1 = List.nth decls 0 in
  check string "first property" "padding" n1;
  check bool "first is important" true i1;

  (* Empty block *)
  let _ = check_block "{}" 0 in
  let _ = check_block "{ }" 0 in
  ()

let test_missing_semicolon () =
  (* Last declaration without semicolon *)
  let decls = check_declarations "color: red; margin: 10px" 2 in
  let n2, v2, _ = List.nth decls 1 in
  check string "second property" "margin" n2;
  check string "second value" "10px" v2;

  (* Single declaration without semicolon *)
  check_declaration_parts "color: red" "color" "red" false;

  (* Complex value without semicolon *)
  check_declaration_parts "width: calc(100% - 10px)" "width" "calc(100% - 10px)"
    false

let test_empty_input () =
  let r = Css.Reader.of_string "" in
  let decls = read_declarations r in
  check int "empty input" 0 (List.length decls);

  let r = Css.Reader.of_string "   " in
  let decls = read_declarations r in
  check int "whitespace only" 0 (List.length decls)

let test_property_name_formats () =
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

let test_property_value_formats () =
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

let test_roundtrip () =
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

let test_error_missing_colon () =
  let r = Css.Reader.of_string "color red;" in
  check_raises "missing colon"
    (Css.Reader.Parse_error ("Expected ':' but got ';'", r))
    (fun () -> ignore (read_declaration r))

let test_error_stray_semicolon () =
  let r = Css.Reader.of_string "; color: red;" in
  check_raises "stray semicolon"
    (Css.Reader.Parse_error ("Expected ':' but got ';'", r))
    (fun () -> ignore (read_declaration r))

let test_error_unclosed_block () =
  let r = Css.Reader.of_string "{ color: red;" in
  check_raises "missing closing brace"
    (Css.Reader.Parse_error ("unexpected end of input", r))
    (fun () -> ignore (read_block r))

let test_special_cases () =
  (* Nested parentheses *)
  check_declaration_parts "width: calc(100% - calc(50px + 10px));" "width"
    "calc(100% - calc(50px + 10px))" false;

  (* Custom property with var() value *)
  check_declaration_parts "--x: var(--y, 10px)" "--x" "var(--y, 10px)" false;

  (* Multiple backgrounds *)
  check_declaration_parts "background: url(x.png), linear-gradient(red, blue);"
    "background" "url(x.png), linear-gradient(red, blue)" false

let suite =
  [
    ( "declaration",
      [
        test_case "simple declarations" `Quick test_simple_declarations;
        test_case "important declarations" `Quick test_important_declarations;
        test_case "complex values" `Quick test_complex_values;
        test_case "quoted strings" `Quick test_quoted_strings;
        test_case "custom properties" `Quick test_custom_properties;
        test_case "vendor prefixes" `Quick test_vendor_prefixes;
        test_case "multiple declarations" `Quick test_multiple_declarations;
        test_case "declaration blocks" `Quick test_declaration_blocks;
        test_case "missing semicolon" `Quick test_missing_semicolon;
        test_case "empty input" `Quick test_empty_input;
        test_case "property name formats" `Quick test_property_name_formats;
        test_case "property value formats" `Quick test_property_value_formats;
        test_case "roundtrip" `Quick test_roundtrip;
        test_case "special cases" `Quick test_special_cases;
        test_case "error missing colon" `Quick test_error_missing_colon;
        test_case "error stray semicolon" `Quick test_error_stray_semicolon;
        test_case "error unclosed block" `Quick test_error_unclosed_block;
      ] );
  ]
