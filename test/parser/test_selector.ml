open Alcotest
open Css_parser

let test_one_simple () =
  let r = Reader.of_string ".class" in
  let _sel = Selector.one r in
  (* Just check that it parses without error *)
  check bool "parsed class selector" true true

let test_one_id () =
  let r = Reader.of_string "#identifier" in
  let _sel = Selector.one r in
  check bool "parsed id selector" true true

let test_one_element () =
  let r = Reader.of_string "div" in
  let _sel = Selector.one r in
  check bool "parsed element selector" true true

let test_one_complex () =
  let r = Reader.of_string ".class > div + p" in
  let _sel = Selector.one r in
  check bool "parsed complex selector" true true

let test_one_opt_valid () =
  let r = Reader.of_string ".valid-class" in
  match Selector.one_opt r with
  | Some _ -> check bool "parsed successfully" true true
  | None -> fail "Expected selector"

let test_one_opt_empty () =
  let r = Reader.of_string "" in
  match Selector.one_opt r with
  | Some _ -> fail "Should not parse empty string"
  | None -> check bool "returned None as expected" true true

let tests =
  [
    test_case "one_simple" `Quick test_one_simple;
    test_case "one_id" `Quick test_one_id;
    test_case "one_element" `Quick test_one_element;
    test_case "one_complex" `Quick test_one_complex;
    test_case "one_opt_valid" `Quick test_one_opt_valid;
    test_case "one_opt_empty" `Quick test_one_opt_empty;
  ]
