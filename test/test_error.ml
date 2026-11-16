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
         "bad selector: boom at <CSS input>:1:5 (in selector)\n";
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
         "bad selector: boom at <CSS input>:1:2 (in selector)\n.";
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
         "bad selector: boom at <CSS input>:1:31 (in selector)\n";
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
    "bad value for color: not a color at <CSS input>:1:12 (in property-value)\n\
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
         "bad value for float: unknown float-side at <CSS input>:3:10 (in \
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
          "bad value for : expected ':' at <CSS input>:1:1 (in component)\n\
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

(* ===== Line and column =====

   CSS Syntax 3 (ED) sec. 3.3 preprocesses the input before tokenizing: CR, FF
   and the CRLF pair each become one LF, and a NUL becomes U+FFFD. A parse error
   is located in that preprocessed text, so its line is the count of LFs before
   it plus one. Sec. 3.2 decodes bytes into code points first, so a column
   counts code points (and one U+FFFD per maximal ill-formed subpart), not
   bytes. Lines and columns are 1-based, the [file:line:column] form an editor
   jumps to. *)

let euros n = repeat n eur

(* A local UTF-8 validator, so the oracle does not run through the decoder under
   test. *)
let is_utf8 s =
  let len = String.length s in
  let rec go i =
    if i >= len then true
    else
      let b = Char.code s.[i] in
      let n =
        if b < 0x80 then 1
        else if b land 0xe0 = 0xc0 then 2
        else if b land 0xf0 = 0xe0 then 3
        else if b land 0xf8 = 0xf0 then 4
        else 0
      in
      if n = 0 || i + n > len then false
      else
        let rec continuations k =
          k = n
          || (Char.code s.[i + k] land 0xc0 = 0x80 && continuations (k + 1))
        in
        continuations 1 && go (i + n)
  in
  go 0

(* An error at byte [pos] of [input] as the reader preprocesses it, which is the
   text every parse error carries. *)
let error_at ?filename input pos =
  let source = Reader.source (Reader.of_string input) in
  Error.v ?filename ~source
    ~loc:(Loc.v ~start_pos:pos ~end_pos:pos)
    ~sort:Sort.Component
    (Error.Missing_token "a character the input never holds")

let position e =
  match Error.position e with
  | Some p -> p
  | None -> Alcotest.fail "an error with a source has a position"

let error_line_and_column () =
  let check name input pos expected =
    Alcotest.(check (pair int int))
      name expected
      (position (error_at input pos))
  in
  (* An LF ends its line, so the byte after it opens the next at column 1. *)
  check "start of the second line" "abc\n" 4 (2, 1);
  check "start of the second line, longer first" "aaaaa\n" 6 (2, 1);
  check "start of the third line" "ab\ncd\n" 6 (3, 1);
  (* The position past the last code point of a line is one column beyond. *)
  check "past the last character" "abc" 3 (1, 4)

(* CSS Syntax 3 (ED) sec. 3.3: CRLF, a lone CR and FF are each one line break.
   [error_at] positions are in the preprocessed text, where each is one LF. *)
let line_breaks_after_preprocessing () =
  let check name input pos expected =
    Alcotest.(check (pair int int))
      name expected
      (position (error_at input pos))
  in
  check "CRLF is one break" "a\r\nb" 2 (2, 1);
  check "a lone CR is one break" "a\rb" 2 (2, 1);
  check "FF is one break" "a\x0cb" 2 (2, 1);
  check "two CRLFs are two breaks" "\r\n\r\nb" 2 (3, 1);
  check "CR CR is two breaks" "a\r\rb" 3 (3, 1);
  check "a CRLF then an LF are two breaks" "a\r\nb\nc" 4 (3, 1)

(* An error built over a string the reader never preprocessed counts the same
   breaks, so [position] does not depend on who built the error. *)
let line_breaks_in_a_raw_source () =
  let at source pos =
    position
      (Error.v ~source
         ~loc:(Loc.v ~start_pos:pos ~end_pos:pos)
         ~sort:Sort.Component (Error.Missing_token "x"))
  in
  Alcotest.(check (pair int int)) "CRLF" (2, 1) (at "a\r\nb" 3);
  Alcotest.(check (pair int int)) "CR" (2, 1) (at "a\rb" 2);
  Alcotest.(check (pair int int)) "FF" (2, 1) (at "a\x0cb" 2);
  Alcotest.(check (pair int int)) "CR CR" (3, 1) (at "a\r\rb" 3)

(* A warning from a whole parse is located in the author's lines whichever break
   the author used. [center] opens column 13 of the second line. *)
let parse_warning_line_and_column () =
  List.iter
    (fun (name, break) ->
      let css =
        String.concat "" [ "a{color:red}"; break; ".x { float: center }" ]
      in
      match parse_warnings css with
      | [ w ] -> Alcotest.(check (pair int int)) name (2, 13) (position w)
      | ws ->
          Alcotest.failf "%s: expected one warning, got %d" name
            (List.length ws))
    [ ("LF", "\n"); ("CRLF", "\r\n"); ("CR", "\r"); ("FF", "\x0c") ]

(* A column counts code points: three EURO SIGNs are nine bytes and three
   columns, so the error sits in column 4 with three characters before the
   caret. *)
let error_column_counts_characters () =
  let e = error_at (euros 3) 9 in
  Alcotest.(check (pair int int)) "column counts characters" (1, 4) (position e);
  match Error.snippet e with
  | Some { marker_pos; _ } ->
      Alcotest.(check int) "caret counts characters" 3 marker_pos
  | None -> Alcotest.fail "an error with a source has a snippet"

(* The caret of pure ASCII stays one column per byte; the snippet window keeps
   40 characters before it. *)
let error_ascii_caret_holds () =
  let e = error_at (String.make 50 'a') 50 in
  Alcotest.(check (pair int int)) "ascii column" (1, 51) (position e);
  match Error.snippet e with
  | Some { marker_pos; _ } -> Alcotest.(check int) "ascii caret" 40 marker_pos
  | None -> Alcotest.fail "an error with a source has a snippet"

(* A window boundary landing inside a UTF-8 sequence moves out to the lead byte,
   so the context a caret is drawn under is never a truncated code point. *)
let error_context_window_keeps_code_points () =
  let text e =
    match Error.snippet e with
    | Some { text; _ } -> text
    | None -> Alcotest.fail "an error with a source has a snippet"
  in
  Alcotest.(check bool)
    "leading boundary keeps code points" true
    (is_utf8 (text (error_at (euros 40 ^ "xx") 122)));
  Alcotest.(check bool)
    "trailing boundary keeps code points" true
    (is_utf8 (text (error_at ("x" ^ euros 40) 1)))

let first_line e = List.hd (String.split_on_char '\n' (Error.to_string e))

(* The source name holds no location. An error without one renders the
   placeholder [<CSS input>], and [with_filename] names it without moving the
   line, the column or the byte offset. *)
let error_filename_names_the_source () =
  let e = error_at "abc\ndef" 5 in
  Alcotest.(check (option string)) "no source name" None e.filename;
  Alcotest.(check (pair int int))
    "location of the failing byte" (2, 2) (position e);
  let stamped = Error.with_filename ~filename:"theme.css" e in
  Alcotest.(check (option string))
    "stamped source name" (Some "theme.css") stamped.filename;
  Alcotest.(check (pair int int))
    "stamping a name keeps the location" (2, 2) (position stamped);
  Alcotest.(check int)
    "stamping a name keeps the offset" 5 stamped.loc.start_pos

(* The rendered location is the three-part [source:line:column] an editor can
   jump to; the byte offset is not a fourth part. *)
let error_renders_three_part_location () =
  let e = error_at "abc\ndef" 5 in
  Alcotest.(check string)
    "renders source:line:column"
    "missing a character the input never holds at <CSS input>:2:2 (in \
     component)"
    (first_line e);
  Alcotest.(check string)
    "renders the stamped source name"
    "missing a character the input never holds at theme.css:2:2 (in component)"
    (first_line (Error.with_filename ~filename:"theme.css" e))

(* The window can span both sides of the failing line; its caret goes right
   under that line, at a column measured from the line start. *)
let error_renders_caret_on_marked_line () =
  check "caret interrupts the context below the marked line"
    (error_at "first\nsecond\nthird" 8)
    (String.concat ""
       [
         "missing a character the input never holds at <CSS input>:2:3 (in \
          component)\n";
         "first\n";
         "second\n";
         "  ^\n";
         "third";
       ])

(* Position zero is a valid marker. *)
let error_renders_caret_at_context_start () =
  check "caret at zero" (error_at "abc" 0)
    (String.concat ""
       [
         "missing a character the input never holds at <CSS input>:1:1 (in \
          component)\n";
         "abc\n";
         "^";
       ])

let tests_position () =
  error_line_and_column ();
  line_breaks_after_preprocessing ();
  line_breaks_in_a_raw_source ();
  parse_warning_line_and_column ();
  error_column_counts_characters ();
  error_ascii_caret_holds ();
  error_context_window_keeps_code_points ();
  error_filename_names_the_source ();
  error_renders_three_part_location ();
  error_renders_caret_on_marked_line ();
  error_renders_caret_at_context_start ()

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
      Alcotest.test_case "line and column" `Quick tests_position;
    ] )
