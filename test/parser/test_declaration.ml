open Alcotest
open Css_parser

let test_one () =
  let r = Reader.of_string "color: red;" in
  match Declaration.one r with
  | Some (name, value, important) ->
      check string "property" "color" name;
      check string "value" "red" value;
      check bool "not important" false important
  | None -> fail "Expected declaration"

let test_one_important () =
  let r = Reader.of_string "margin: 10px !important;" in
  match Declaration.one r with
  | Some (name, value, important) ->
      check string "property" "margin" name;
      check string "value" "10px" value;
      check bool "is important" true important
  | None -> fail "Expected declaration"

let test_declarations () =
  let r =
    Reader.of_string "color: red; margin: 10px; padding: 5px !important"
  in
  let decls = Declaration.declarations r in
  check int "count" 3 (List.length decls);

  let name1, value1, imp1 = List.nth decls 0 in
  check string "first property" "color" name1;
  check string "first value" "red" value1;
  check bool "first not important" false imp1;

  let name3, value3, imp3 = List.nth decls 2 in
  check string "third property" "padding" name3;
  check string "third value" "5px" value3;
  check bool "third is important" true imp3

let test_block () =
  let r = Reader.of_string "{ color: blue; display: block; }" in
  let decls = Declaration.block r in
  check int "count" 2 (List.length decls);

  let name1, value1, _ = List.nth decls 0 in
  check string "first property" "color" name1;
  check string "first value" "blue" value1

let test_empty () =
  let r = Reader.of_string "" in
  let decls = Declaration.declarations r in
  check int "empty" 0 (List.length decls)

let test_missing_semicolon_and_strings () =
  (* Missing trailing semicolon should still parse value; quoted strings with
     escapes *)
  let r = Reader.of_string "content: \"a\\\"b\"; width: calc(100% - 10px)" in
  (match Declaration.one r with
  | Some (n, v, imp) ->
      check string "prop1" "content" n;
      check string "val1" "\"a\\\"b\"" v;
      check bool "imp1" false imp
  | None -> fail "expected first decl");
  let _ = Reader.ws r in
  match Declaration.one r with
  | Some (n, v, _) ->
      check string "prop2" "width" n;
      check string "val2" "calc(100% - 10px)" v
  | None -> fail "expected second decl"

let test_block_nested_and_important () =
  let r =
    Reader.of_string
      "{ padding: 10px !important; background: url(x.png), \
       linear-gradient(red, blue); }"
  in
  let decls = Declaration.block r in
  check int "count" 2 (List.length decls);
  let n1, _v1, i1 = List.nth decls 0 in
  check string "name1" "padding" n1;
  check bool "important" true i1

let tests =
  [
    test_case "one" `Quick test_one;
    test_case "one_important" `Quick test_one_important;
    test_case "declarations" `Quick test_declarations;
    test_case "block" `Quick test_block;
    test_case "empty" `Quick test_empty;
    test_case "missing_semicolon_and_strings" `Quick
      test_missing_semicolon_and_strings;
    test_case "block_nested_and_important" `Quick
      test_block_nested_and_important;
  ]
