(** Error module tests: rendering of the sealed kind and named constructors. *)

open Cascade

let loc = Loc.v ~start_pos:12 ~end_pos:13

let check name e expected =
  Alcotest.(check string) name expected (Error.to_string e)

let sort_mismatch () =
  check "sort mismatch"
    (Error.sort_mismatch loc ~sort:Sort.Selector ~expected:Sort.Component
       ~found:Sort.Block)
    "expected component but found block at [12-13] (in selector)"

let unexpected_token () =
  check "unexpected token"
    (Error.unexpected_token loc ~sort:Sort.Declaration (Token.Delim "."))
    "unexpected <delim '.'> at [12-13] (in declaration)"

let missing_token () =
  check "missing token"
    (Error.missing_token loc ~sort:Sort.Declaration "':'")
    "missing ':' at [12-13] (in declaration)"

let bad_selector () =
  check "bad selector"
    (Error.bad_selector loc "double dot")
    "bad selector: double dot at [12-13] (in selector)"

let bad_value () =
  check "bad value"
    (Error.bad_value loc ~property:"color" ~reason:"not a color")
    "bad value for color: not a color at [12-13] (in property-value)"

let with_property () =
  (* A value reader raises [Bad_value] without a property name; the declaration
     parser stamps it back on. *)
  check "fills an empty property"
    (Error.with_property "width"
       (Error.bad_value loc ~property:"" ~reason:"bad"))
    "bad value for width: bad at [12-13] (in property-value)";
  (* An already-named property is left as-is. *)
  check "keeps an existing property"
    (Error.with_property "width"
       (Error.bad_value loc ~property:"color" ~reason:"bad"))
    "bad value for color: bad at [12-13] (in property-value)";
  (* A non-[Bad_value] error is untouched. *)
  check "leaves a non-bad-value error"
    (Error.with_property "width" (Error.bad_selector loc "double dot"))
    "bad selector: double dot at [12-13] (in selector)"

let unknown_at_rule () =
  check "unknown at-rule"
    (Error.unknown_at_rule loc "weird")
    "unknown at-rule @weird at [12-13] (in at-rule)"

let unterminated () =
  check "unterminated"
    (Error.unterminated loc Sort.Function)
    "unterminated function at [12-13] (in function)"

let source_context () =
  let t = Cursor.of_string "color red;" in
  match Cursor.colon t with
  | true -> Alcotest.fail "expected missing colon"
  | false -> (
      try Cursor.err_expected t "':'"
      with Cursor.Parse_error e ->
        check "source context" e
          "bad value for : expected ':' at [0-5] (in component)\n\
           color red;\n\
           ^^^^^")

let suite =
  ( "error",
    [
      Alcotest.test_case "sort mismatch" `Quick sort_mismatch;
      Alcotest.test_case "unexpected token" `Quick unexpected_token;
      Alcotest.test_case "missing token" `Quick missing_token;
      Alcotest.test_case "bad selector" `Quick bad_selector;
      Alcotest.test_case "with_property" `Quick with_property;
      Alcotest.test_case "bad value" `Quick bad_value;
      Alcotest.test_case "unknown at-rule" `Quick unknown_at_rule;
      Alcotest.test_case "unterminated" `Quick unterminated;
      Alcotest.test_case "source context" `Quick source_context;
    ] )
