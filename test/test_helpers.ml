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
  let got = reader r in
  Alcotest.(check bool)
    (Fmt.str "should reject: %s" input)
    true (Option.is_none got)

(** Test CSS-wide keywords mixing with other values (should fail) *)
let test_css_wide_keywords_mixing reader css_wide_keywords prop_name =
  List.iter
    (fun keyword ->
      neg reader (prop_name ^ ": " ^ keyword ^ " 10px");
      neg reader (prop_name ^ ": 10px " ^ keyword))
    css_wide_keywords

(** Common CSS-wide keywords *)
let css_wide_keywords = [ "inherit"; "unset"; "revert"; "revert-layer" ]
