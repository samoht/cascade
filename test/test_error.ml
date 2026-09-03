(** Error module tests: rendering of the sealed kind and named constructors, and
    where a reader anchors the value it rejects. *)

open Cascade

let loc = Loc.v ~start_pos:12 ~end_pos:13

let check name e expected =
  Alcotest.(check string) name expected (Error.to_string e)

(* U+20AC EURO SIGN: three bytes, one caret column. Source stays 7-bit, so the
   multibyte inputs below are byte escapes. *)
let eur = "\xe2\x82\xac"
let repeat n s = String.concat "" (List.init n (fun _ -> s))

let located ~source ~start_pos ~end_pos =
  Error.v ~source
    ~loc:(Loc.v ~start_pos ~end_pos)
    ~sort:Sort.Selector (Error.Bad_selector "boom")

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

let caret_counts_characters () =
  (* Four EURO SIGNs are twelve bytes but four columns, so the caret belongs
     under the fifth character. *)
  let source = String.concat "" [ repeat 4 eur; "!" ] in
  check "caret counts characters"
    (located ~source ~start_pos:12 ~end_pos:13)
    (String.concat ""
       [
         "bad selector: boom at [12-13] (in selector)\n";
         repeat 4 eur;
         "!\n";
         "    ^";
       ])

let marker_spans_characters () =
  (* The marked range covers three EURO SIGNs: three carets, not nine. *)
  let source = String.concat "" [ "."; repeat 3 eur; "!" ] in
  check "marker spans characters"
    (located ~source ~start_pos:1 ~end_pos:10)
    (String.concat ""
       [
         "bad selector: boom at [1-10] (in selector)\n.";
         repeat 3 eur;
         "!\n";
         " ^^^";
       ])

let window_never_splits_a_code_point () =
  (* A 30-character class name puts the window boundary 50 bytes in, the last
     byte of a EURO SIGN. The snippet has to open on the lead byte at 48
     instead: a diagnostic must not be a truncated code point. *)
  let source = String.concat "" [ repeat 30 eur; "!" ] in
  let e = located ~source ~start_pos:90 ~end_pos:91 in
  Alcotest.(check bool)
    "renders valid utf-8" true
    (String.is_valid_utf_8 (Error.to_string e));
  check "window opens on a lead byte" e
    (String.concat ""
       [
         "bad selector: boom at [90-91] (in selector)\n";
         repeat 14 eur;
         "!\n";
         "              ^";
       ])

let ascii_caret_unmoved () =
  (* The counting change must leave every byte of a 7-bit snippet where it
     was. *)
  let source = "a { color: bogus; }" in
  let e =
    Error.v ~source
      ~loc:(Loc.v ~start_pos:11 ~end_pos:16)
      ~sort:Sort.Property_value
      (Error.Bad_value { property = "color"; reason = "not a color" })
  in
  Alcotest.(check bool)
    "renders valid utf-8" true
    (String.is_valid_utf_8 (Error.to_string e));
  check "ascii caret unmoved" e
    "bad value for color: not a color at [11-16] (in property-value)\n\
     a { color: bogus; }\n\
    \           ^^^^^"

let caret_marks_the_line_it_points_at () =
  (* The marked value sits on the third line of the window, so the caret line
     comes straight after that line, at that line's own column. *)
  let source =
    ".header {\n  color: red;\n  float: center;\n  margin: 0;\n}\n"
  in
  let start_pos = String.length ".header {\n  color: red;\n  float: " in
  let e =
    Error.v ~source
      ~loc:(Loc.v ~start_pos ~end_pos:(start_pos + String.length "center"))
      ~sort:Sort.Property_value
      (Error.Bad_value { property = "float"; reason = "unknown float-side" })
  in
  check "caret marks the line it points at" e
    (String.concat "\n"
       [
         "bad value for float: unknown float-side at [33-39] (in \
          property-value)";
         ".header {";
         "  color: red;";
         "  float: center;";
         "         ^^^^^^";
         "  margin: 0;";
         "}";
         "";
       ])

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

(* A reader that consumes the offending token before rejecting it still owns
   that token: the span marks the value the author wrote, not whatever follows
   it. When the bad value ends the declaration, "whatever follows" is the
   terminator, so the caret lands on punctuation. *)
let parse_warnings css =
  match Css.of_string ~strict:false css with
  | Error e -> Alcotest.failf "fatal parse error: %s" (Error.to_string e)
  | Ok { Css.warnings; _ } -> warnings

let marked css =
  List.map
    (fun (w : Error.t) ->
      String.sub css w.loc.start_pos (w.loc.end_pos - w.loc.start_pos))
    (parse_warnings css)

let value_span_marks_the_rejected_value () =
  List.iter
    (fun (css, expected) ->
      Alcotest.(check (list string)) css [ expected ] (marked css))
    [
      (* Readers reached through a keyword table already mark their own
         value. *)
      (".x { float: center }", "center");
      (* Readers that consume first, one per rejecting site. *)
      (".x { color: nope }", "nope");
      (".x { font-palette: nope }", "nope");
      (".x { font: nope }", "nope");
      (".x { offset: nope }", "nope");
      (".x { font-style: nope }", "nope");
      (".x { scrollbar-gutter: nope }", "nope");
      (".x { paint-order: nope }", "nope");
      (".x { vector-effect: nope }", "nope");
      (* A whole-value reader marks the whole value it read. *)
      (".x { font: bold nope 12pt sans-serif }", "bold nope 12pt sans-serif");
      (".x { animation-name: slide nope }", "nope");
      (* The span is one token late rather than pinned to the terminator: a
         value with something after it marks that something. *)
      (".x { color: nope red }", "nope");
    ]

(* [Cursor.one_of] fails once every alternative has failed, and the alternatives
   are anonymous readers, so there is nothing to list after "one of". The reason
   says that no form matched rather than trailing off. *)
let bad_value_reason_names_something () =
  let reasons css =
    List.filter_map
      (fun (w : Error.t) ->
        match w.kind with
        | Error.Bad_value { reason; _ } -> Some reason
        | _ -> None)
      (parse_warnings css)
  in
  List.iter
    (fun (css, expected) ->
      Alcotest.(check (list string)) css [ expected ] (reasons css))
    [
      (".x { top: nope }", "no accepted form");
      (".x { align-items: nope }", "no accepted form");
      (".x { aspect-ratio: nope }", "no accepted form");
    ]

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
      Alcotest.test_case "caret counts characters" `Quick
        caret_counts_characters;
      Alcotest.test_case "marker spans characters" `Quick
        marker_spans_characters;
      Alcotest.test_case "window never splits a code point" `Quick
        window_never_splits_a_code_point;
      Alcotest.test_case "ascii caret unmoved" `Quick ascii_caret_unmoved;
      Alcotest.test_case "caret marks the line it points at" `Quick
        caret_marks_the_line_it_points_at;
      Alcotest.test_case "value span marks the rejected value" `Quick
        value_span_marks_the_rejected_value;
      Alcotest.test_case "bad value reason names something" `Quick
        bad_value_reason_names_something;
    ] )
