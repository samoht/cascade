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
