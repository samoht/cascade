(** Common helpers for CSS tests to reduce duplication and inconsistencies *)

(** Generic negative test combinator - tests that parsing should fail Use this
    for parsers that raise Parse_error on failure *)
let neg reader input =
  let r = Css.Reader.of_string input in
  try
    let _ = reader r in
    (* Check if there's unparsed content remaining *)
    if not (Css.Reader.is_done r) then ()
      (* Success - parser didn't consume everything *)
    else Alcotest.failf "Expected '%s' to fail parsing" input
  with Css.Reader.Parse_error _ -> ()

(** Test that an option-returning parser rejects invalid input Use this for
    parsers that return Some/None instead of raising *)
let none reader input =
  let r = Css.Reader.of_string input in
  try
    let got = reader r in
    Alcotest.(check bool)
      (Fmt.str "should reject: %s" input)
      true (Option.is_none got)
  with Css.Reader.Parse_error _ ->
    (* Parser correctly rejected input by raising exception *)
    ()

(** Test CSS-wide keywords mixing with other values (should fail) *)
let test_css_wide_keywords_mixing reader css_wide_keywords prop_name =
  List.iter
    (fun keyword ->
      neg reader (prop_name ^ ": " ^ keyword ^ " 10px");
      neg reader (prop_name ^ ": 10px " ^ keyword))
    css_wide_keywords

(** Generic check function for CSS value types - handles parse/print testing *)
let check_value type_name reader pp_func ?(minify = true) ?(roundtrip = false)
    ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected *)
  let t = Css.Reader.of_string input in
  let v = reader t in
  let s = Css.Pp.to_string ~minify pp_func v in
  Alcotest.(check string) (Fmt.str "%s %s" type_name input) expected s;
  (* Optional roundtrip stability: read printed output and ensure idempotent
     printing *)
  if roundtrip then
    let t2 = Css.Reader.of_string s in
    let v2 = reader t2 in
    let s2 = Css.Pp.to_string ~minify pp_func v2 in
    Alcotest.(check string) (Fmt.str "roundtrip %s %s" type_name input) s s2

(** Check that two Parse_error records have matching fields *)
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

(** Check that a function raises a specific exception *)
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

(** Generic helper for testing constructor string representations *)
let check_construct name to_string expected value =
  let actual = to_string value in
  Alcotest.(check string) name expected actual

(** Common CSS-wide keywords *)
let css_wide_keywords = [ "inherit"; "unset"; "revert"; "revert-layer" ]
