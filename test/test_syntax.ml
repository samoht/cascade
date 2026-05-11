open Cascade

let check_bool name expected actual = Alcotest.(check bool) name expected actual

let is_ascii_ident_start () =
  check_bool "letter" true (Syntax.is_ascii_ident_start 'a');
  check_bool "uppercase" true (Syntax.is_ascii_ident_start 'Z');
  check_bool "underscore" true (Syntax.is_ascii_ident_start '_');
  check_bool "hyphen" true (Syntax.is_ascii_ident_start '-');
  check_bool "digit" false (Syntax.is_ascii_ident_start '0');
  check_bool "space" false (Syntax.is_ascii_ident_start ' ')

let is_ascii_ident_continue () =
  check_bool "letter" true (Syntax.is_ascii_ident_continue 'a');
  check_bool "digit" true (Syntax.is_ascii_ident_continue '5');
  check_bool "hyphen" true (Syntax.is_ascii_ident_continue '-');
  check_bool "underscore" true (Syntax.is_ascii_ident_continue '_');
  check_bool "punctuation" false (Syntax.is_ascii_ident_continue '!')

let is_hex () =
  check_bool "digit" true (Syntax.is_hex '0');
  check_bool "lower a-f" true (Syntax.is_hex 'a');
  check_bool "upper A-F" true (Syntax.is_hex 'F');
  check_bool "outside" false (Syntax.is_hex 'g');
  check_bool "non-letter" false (Syntax.is_hex '!')

let url_needs_quotes () =
  check_bool "plain" false (Syntax.url_needs_quotes "foo.css");
  check_bool "space" true (Syntax.url_needs_quotes "foo bar.css");
  check_bool "quote" true (Syntax.url_needs_quotes "foo\"bar");
  check_bool "paren" true (Syntax.url_needs_quotes "foo(bar)")

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
    (Syntax.strip_url_suffix "foo.css?v=1#top")

let suite =
  ( "syntax",
    [
      Alcotest.test_case "is_ascii_ident_start" `Quick is_ascii_ident_start;
      Alcotest.test_case "is_ascii_ident_continue" `Quick
        is_ascii_ident_continue;
      Alcotest.test_case "is_hex" `Quick is_hex;
      Alcotest.test_case "url_needs_quotes" `Quick url_needs_quotes;
      Alcotest.test_case "strip_url_suffix" `Quick strip_url_suffix;
    ] )
