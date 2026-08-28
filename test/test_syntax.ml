open Cascade

let check_true name actual = Alcotest.(check bool) name true actual
let check_false name actual = Alcotest.(check bool) name false actual

let is_ascii_ident_start () =
  check_true "letter" (Syntax.is_ascii_ident_start 'a');
  check_true "uppercase" (Syntax.is_ascii_ident_start 'Z');
  check_true "underscore" (Syntax.is_ascii_ident_start '_');
  check_true "hyphen" (Syntax.is_ascii_ident_start '-');
  check_false "digit" (Syntax.is_ascii_ident_start '0');
  check_false "space" (Syntax.is_ascii_ident_start ' ')

let is_ascii_ident_continue () =
  check_true "letter" (Syntax.is_ascii_ident_continue 'a');
  check_true "digit" (Syntax.is_ascii_ident_continue '5');
  check_true "hyphen" (Syntax.is_ascii_ident_continue '-');
  check_true "underscore" (Syntax.is_ascii_ident_continue '_');
  check_false "punctuation" (Syntax.is_ascii_ident_continue '!')

let is_ident () =
  (* CSS Syntax 3 sec. 4.3.11 for the opening code points, sec. 4.2 for the
     rest. *)
  check_true "letters" (Syntax.is_ident "foo");
  check_true "underscore start" (Syntax.is_ident "_x");
  check_true "digit and hyphen inside" (Syntax.is_ident "a-1");
  check_true "dash then ident-start" (Syntax.is_ident "-foo");
  check_true "dashed-ident" (Syntax.is_ident "--foo");
  (* A second [-] opens an ident, so the reserved custom-property name is one
     lexically even though it names no property. *)
  check_true "two dashes" (Syntax.is_ident "--");
  check_false "empty" (Syntax.is_ident "");
  (* A lone [-] is a delim, and [-] before a digit opens no ident, so both slip
     past a scan that reads [-] as ident-start. *)
  check_false "bare hyphen" (Syntax.is_ident "-");
  check_false "dash then digit" (Syntax.is_ident "-5px");
  check_false "leading digit" (Syntax.is_ident "5px");
  check_false "space" (Syntax.is_ident "a b");
  check_false "backslash is no ident code point" (Syntax.is_ident "a\\b")

let is_ident_non_ascii () =
  (* Sec. 4.2 lists the non-ASCII ident code points as ranges rather than
     admitting every code point [>= U+0080]. U+00B7, U+00F6 and U+200C are in
     the list. *)
  check_true "U+00B7" (Syntax.is_ident "\xc2\xb7mid");
  check_true "U+00F6" (Syntax.is_ident "f\xc3\xb6o");
  check_true "U+200C" (Syntax.is_ident "\xe2\x80\x8cz");
  (* U+00D7 falls in the gap between U+00C0..U+00D6 and U+00D8..U+00F6, and
     U+2197 between U+2070..U+218F and U+2C00..U+2FEF. *)
  check_false "U+00D7" (Syntax.is_ident "a\xc3\x97b");
  check_false "U+2197" (Syntax.is_ident "text-\xe2\x86\x97");
  (* A byte [>= 0x80] is not a code point: a lone continuation byte decodes to
     nothing an ident can hold. *)
  check_false "lone continuation byte" (Syntax.is_ident "a\x80b")

let is_hex () =
  check_true "digit" (Syntax.is_hex '0');
  check_true "lower a-f" (Syntax.is_hex 'a');
  check_true "upper A-F" (Syntax.is_hex 'F');
  check_false "outside" (Syntax.is_hex 'g');
  check_false "non-letter" (Syntax.is_hex '!')

let url_needs_quotes () =
  check_false "plain" (Syntax.url_needs_quotes "foo.css");
  check_true "space" (Syntax.url_needs_quotes "foo bar.css");
  check_true "quote" (Syntax.url_needs_quotes "foo\"bar");
  check_true "paren" (Syntax.url_needs_quotes "foo(bar)")

let strip_url_suffix () =
  Alcotest.(check string)
    "no suffix" "foo.css"
    (Syntax.strip_url_suffix "foo.css");
  Alcotest.(check string)
    "query" "foo.css"
    (Syntax.strip_url_suffix "foo.css?v=1");
  Alcotest.(check string)
    "fragment" "foo.css"
    (Syntax.strip_url_suffix "foo.css#top");
  Alcotest.(check string)
    "both" "foo.css"
    (Syntax.strip_url_suffix "foo.css?v=1#top");
  Alcotest.(check string)
    "encoded delimiters stay in path" "foo%3Fbar%23baz.css"
    (Syntax.strip_url_suffix "foo%3Fbar%23baz.css?v=1#top");
  Alcotest.(check string)
    "space is encoded" "foo%20bar.css"
    (Syntax.strip_url_suffix "foo bar.css")

let url_file_path () =
  Alcotest.(check string)
    "encoded path becomes filesystem path" "foo?bar#baz.css"
    (Syntax.url_file_path "foo%3Fbar%23baz.css?v=1#top");
  Alcotest.(check string)
    "encoded space becomes filesystem space" "foo bar.css"
    (Syntax.url_file_path "foo%20bar.css")

let remote_url () =
  check_true "https" (Syntax.is_remote_url "HTTPS://example.test/a.css");
  check_true "data" (Syntax.is_remote_url "data:text/css,a{}");
  check_true "authority" (Syntax.is_remote_url "//example.test/a.css");
  check_false "relative" (Syntax.is_remote_url "a.css")

let suite =
  ( "syntax",
    [
      Alcotest.test_case "is_ascii_ident_start" `Quick is_ascii_ident_start;
      Alcotest.test_case "is_ascii_ident_continue" `Quick
        is_ascii_ident_continue;
      Alcotest.test_case "is_ident" `Quick is_ident;
      Alcotest.test_case "is_ident_non_ascii" `Quick is_ident_non_ascii;
      Alcotest.test_case "is_hex" `Quick is_hex;
      Alcotest.test_case "url_needs_quotes" `Quick url_needs_quotes;
      Alcotest.test_case "strip_url_suffix" `Quick strip_url_suffix;
      Alcotest.test_case "url_file_path" `Quick url_file_path;
      Alcotest.test_case "remote_url" `Quick remote_url;
    ] )
