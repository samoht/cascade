(** Tokenizer smoke tests. Not a full WPT port; that comes later. *)

open Cascade

let tokens_of css =
  let r = Css.Reader.of_string css in
  let rec loop acc =
    match Css.Lexer.next (Css.Lexer.of_reader r) with
    | Css.Token.Eof -> List.rev acc
    | t -> loop (t :: acc)
  in
  loop []

let pp_tokens toks = String.concat " " (List.map Css.Token.to_string toks)

let check input expected_summary =
  let got = pp_tokens (tokens_of input) in
  Alcotest.(check string) (Fmt.str "tokenize %S" input) expected_summary got

let idents () =
  check "foo" "<ident foo>";
  check "foo-bar" "<ident foo-bar>";
  check "--custom" "<ident --custom>";
  check "-foo" "<ident -foo>"

let functions () =
  check "rgb(1,2,3)"
    "<function rgb(> <number 1> <,> <number 2> <,> <number 3> <)>"

let at_keyword () =
  check "@media" "<@media>";
  check "@-webkit-foo" "<@-webkit-foo>"

let hashes () =
  check "#abc" "<#abc>";
  check "#123" "<#123>"

let strings () =
  check {|"hello"|} "<string hello>";
  check "'hello'" "<string hello>";
  check {|"un\"escaped"|} {|<string un"escaped>|}

let delims () = check "* + ~" "<delim '*'> <ws> <delim '+'> <ws> <delim '~'>"

let braces () =
  check "{}" "<{> <}>";
  check "()" "<(> <)>";
  check "[]" "<[> <]>"

let numbers () =
  check "1 2.5 -3 1e2"
    "<number 1> <ws> <number 2.5> <ws> <number -3> <ws> <number 1e2>"

let percentage () = check "50%" "<percentage 50%>"

let dimension () =
  check "10px 1.5rem" "<dimension 10px> <ws> <dimension 1.5rem>"

let comments_skipped () = check "/* hi */ foo" "<ws> <ident foo>"
let cdo_cdc () = check "<!-- foo -->" "<CDO> <ws> <ident foo> <ws> <CDC>"

let url () =
  check "url(a.png)" "<url a.png>";
  check {|url("a.png")|} {|<function url(> <string a.png> <)>|}

(* Per 4.3.11, a newline inside a string produces a <bad-string> token; the
   newline is not consumed and becomes a subsequent <whitespace>. *)
let bad_string () = check "\"oops\n" "<bad-string> <ws>"
let bad_url () = check "url(a b)" "<bad-url>"

let suite =
  ( "token",
    [
      Alcotest.test_case "idents" `Quick idents;
      Alcotest.test_case "functions" `Quick functions;
      Alcotest.test_case "at-keyword" `Quick at_keyword;
      Alcotest.test_case "hashes" `Quick hashes;
      Alcotest.test_case "strings" `Quick strings;
      Alcotest.test_case "delims" `Quick delims;
      Alcotest.test_case "braces" `Quick braces;
      Alcotest.test_case "numbers" `Quick numbers;
      Alcotest.test_case "percentage" `Quick percentage;
      Alcotest.test_case "dimension" `Quick dimension;
      Alcotest.test_case "comments skipped" `Quick comments_skipped;
      Alcotest.test_case "CDO/CDC" `Quick cdo_cdc;
      Alcotest.test_case "url" `Quick url;
      Alcotest.test_case "bad string" `Quick bad_string;
      Alcotest.test_case "bad url" `Quick bad_url;
    ] )
