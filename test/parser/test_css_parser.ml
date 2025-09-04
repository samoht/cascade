open Alcotest
open Css_parser

let test_of_string () =
  (* Test basic CSS parsing *)
  let css = ".class { color: red; }" in
  match of_string css with
  | Ok _ -> check bool "parsed successfully" true true
  | Error e -> fail ("Failed to parse CSS: " ^ e)

let test_of_string_error () =
  (* Test CSS parsing with invalid input *)
  let css = ".class { color: " in
  (* Unclosed declaration *)
  match of_string css with
  | Ok _ -> fail "Should have failed to parse invalid CSS"
  | Error _ -> check bool "failed as expected" true true

let test_of_string_exn () =
  (* Test exception-throwing parser *)
  let css = ".class { margin: 10px; }" in
  let _ = of_string_exn css in
  check bool "parsed without exception" true true

let tests =
  [
    test_case "of_string" `Quick test_of_string;
    test_case "of_string_error" `Quick test_of_string_error;
    test_case "of_string_exn" `Quick test_of_string_exn;
  ]
