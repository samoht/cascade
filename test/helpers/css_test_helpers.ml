(** Common helpers for CSS tests to reduce duplication and inconsistencies.

    Test expectations passed to these helpers must come from CSS specifications
    or from a documented Cascade canonical form that preserves the spec syntax.
    Do not derive [expected] values from the current implementation output. *)

open Cascade

(** Generic negative test combinator - tests that parsing should fail Use this
    for parsers that raise Parse_error on failure *)
let neg ?(allow_partial = false) reader input =
  let r = Reader.of_string input in
  try
    let _ = reader r in
    (* Stopping early is not rejecting. A parser that takes the prefix of
       [margin: inherit 10px] and leaves the rest has accepted a value the
       negative case says is invalid, so only [allow_partial] - for a reader
       whose job is to stop - lets leftover input stand for a rejection. *)
    if allow_partial && not (Reader.is_done r) then ()
    else Alcotest.failf "Expected '%s' to fail parsing" input
  with Reader.Parse_error _ -> ()

(** Test that an option-returning parser rejects invalid input Use this for
    parsers that return Some/None instead of raising *)
let none reader input =
  let r = Reader.of_string input in
  try
    let got = reader r in
    Alcotest.(check bool)
      (Fmt.str "should reject: %s" input)
      true (Option.is_none got)
  with Reader.Parse_error _ ->
    (* Parser correctly rejected input by raising exception *)
    ()

(** Test CSS-wide keywords mixing with other values (should fail) *)
let test_css_wide_keywords_mixing reader css_wide_keywords prop_name =
  List.iter
    (fun keyword ->
      neg reader (prop_name ^ ": " ^ keyword ^ " 10px");
      neg reader (prop_name ^ ": 10px " ^ keyword))
    css_wide_keywords

(** Cursor-based variant of {!neg}. *)
let neg_cursor ?(allow_partial = false) parse input =
  let c = Cursor.of_string input in
  match parse c with
  | exception Error.Parse_error _ -> ()
  | _ ->
      if not (allow_partial && not (Cursor.is_done c)) then
        Alcotest.failf "Expected '%s' to fail parsing" input

(** Cursor-based variant of {!none}: the parser returns an option and should
    yield [None] for malformed input (or raise). *)
let none_cursor parse input =
  let c = Cursor.of_string input in
  match parse c with
  | None -> ()
  | Some _ -> Alcotest.failf "expected no value for %S" input
  | exception Error.Parse_error _ -> ()

(** Test that [parse] rejects [input] with an [Error.t] whose rendered string
    matches [expected] exactly. *)
let check_error parse input expected =
  let c = Cursor.of_string input in
  match parse c with
  | _ -> Alcotest.failf "expected Parse_error for %S" input
  | exception Error.Parse_error e ->
      Alcotest.(check string)
        (Fmt.str "error for %S" input)
        expected (Error.to_string e)

(** Cursor-based variant of {!check_value}. Lexes [input], builds a {!Cursor.t},
    applies [parse], pretty-prints with [pp_func], and asserts the result equals
    the spec-derived [input] or [expected]. Use this for parsers whose signature
    is [Cursor.t -> 'a]. *)
let check_value_cursor ?unicode_ranges type_name parse pp_func ?(minify = true)
    ?(roundtrip = false) ?expected input =
  let expected = Option.value ~default:input expected in
  let c = Cursor.of_string ?unicode_ranges input in
  let v = parse c in
  let s = Css.Pp.to_string ~minify pp_func v in
  Alcotest.(check string) (Fmt.str "%s %s" type_name input) expected s;
  if roundtrip then
    let c2 = Cursor.of_string ?unicode_ranges s in
    let v2 = parse c2 in
    let s2 = Css.Pp.to_string ~minify pp_func v2 in
    Alcotest.(check string) (Fmt.str "roundtrip %s %s" type_name input) s s2

(** [decl_optimizes ~prop ?held ~into input] asserts that normalization happens
    in optimize, not pp. When [held] is given it asserts the just-minify (pp
    only) form of [prop:input] equals [prop:held] - pp must NOT normalize. It
    always asserts the minify+optimize form equals [prop:into] - the optimizer
    does the cross-node fold (360deg->1turn, 0px->0, named<->hex, calc). [into]
    is the spec-canonical shortest spelling, not a snapshot of current output.
*)
let decl_optimizes ~prop ?held ~into input =
  let wrap v = String.concat "" [ ".x{"; prop; ":"; v; "}" ] in
  match Css.of_string ~strict:false (wrap input) with
  | Ok p ->
      Option.iter
        (fun held ->
          Alcotest.(check string)
            (wrap input ^ " minify")
            (wrap held)
            (Css.to_string ~minify:true p.stylesheet |> String.trim))
        held;
      Alcotest.(check string)
        (wrap input ^ " minify+optimize")
        (wrap into)
        (Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim)
  | Error _ -> Alcotest.failf "parse failed: %s" (wrap input)

let decl_optimizes_to ?held ~into input =
  let wrap decl = String.concat "" [ ".x{"; decl; "}" ] in
  match Css.of_string ~strict:false (wrap input) with
  | Ok p ->
      Option.iter
        (fun held ->
          Alcotest.(check string)
            (wrap input ^ " minify")
            (wrap held)
            (Css.to_string ~minify:true p.stylesheet |> String.trim))
        held;
      Alcotest.(check string)
        (wrap input ^ " minify+optimize")
        (wrap into)
        (Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim)
  | Error _ -> Alcotest.failf "parse failed: %s" (wrap input)

(** [decl_lossless ~prop ~into input] asserts the lossless minify+optimize
    oracle for one declaration. The expected value is still a minified canonical
    spelling, but bounded approximation is disabled on both optimize and pp. *)
let decl_lossless ~prop ~into input =
  let wrap v = String.concat "" [ ".x{"; prop; ":"; v; "}" ] in
  match Css.of_string ~strict:false (wrap input) with
  | Ok p ->
      Alcotest.(check string)
        (wrap input ^ " lossless minify+optimize")
        (wrap into)
        (Css.to_string ~minify:true ~lossless:true
           (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim)
  | Error _ -> Alcotest.failf "parse failed: %s" (wrap input)

(** Generic check function for CSS value types. [expected] is a spec oracle, not
    a snapshot of current implementation behavior. *)
let check_value type_name reader pp_func ?(minify = true) ?(roundtrip = false)
    ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected *)
  let t = Reader.of_string input in
  let v = reader t in
  let s = Css.Pp.to_string ~minify pp_func v in
  Alcotest.(check string) (Fmt.str "%s %s" type_name input) expected s;
  (* Optional roundtrip stability: read printed output and ensure idempotent
     printing *)
  if roundtrip then
    let t2 = Reader.of_string s in
    let v2 = reader t2 in
    let s2 = Css.Pp.to_string ~minify pp_func v2 in
    Alcotest.(check string) (Fmt.str "roundtrip %s %s" type_name input) s s2

(** Check that two Parse_error records have matching fields *)
let check_parse_error_fields name (expected : Reader.parse_error)
    (actual : Reader.parse_error) =
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
  | Reader.Parse_error actual
    when match expected_exn with
         | Reader.Parse_error expected ->
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

(** Minor words allocated by a thunk, for the allocation guards *)
let measure f =
  Gc.full_major ();
  let w0 = Gc.minor_words () in
  let r = f () in
  ignore (Sys.opaque_identity r);
  Gc.minor_words () -. w0
