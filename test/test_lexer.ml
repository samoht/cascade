(** Tokenizer smoke tests. Not a full WPT port; that comes later.

    CSS Syntax section 3.2 byte-stream decoding is intentionally not tested
    here: Cascade's lexer starts from already-decoded UTF-8 text, so BOM,
    transport charset, environment fallback, and [@charset] byte sniffing belong
    to callers or to a future byte-decoding entry point. *)

open Cascade

let tokens_of css =
  let lexer = Css.Lexer.of_string css in
  let rec loop acc =
    let tok = Css.Lexer.next lexer in
    match tok.Css.Token.kind with
    | Css.Token.Eof -> List.rev acc
    | _ -> loop (tok.Css.Token.kind :: acc)
  in
  loop []

let pp_tokens kinds =
  String.concat " " (List.map (Css.Pp.to_string Css.Token.pp_kind) kinds)

let check input expected_summary =
  let got = pp_tokens (tokens_of input) in
  Alcotest.(check string) (Fmt.str "tokenize %S" input) expected_summary got

let spec_preprocessing () =
  (* CSS Syntax Level 3 section 3.3: Cascade starts after byte decoding, then
     preprocesses the code-point stream before tokenization. *)
  check "\xEF\xBB\xBFfoo" "<ident foo>";
  check "a\rb" "<ident a> <ws> <ident b>";
  check "a\r\nb" "<ident a> <ws> <ident b>";
  check "a\012b" "<ident a> <ws> <ident b>";
  check "a\x00b" ("<ident a" ^ "\xEF\xBF\xBD" ^ "b>")

let spec_token_railroad_diagrams () =
  (* CSS Syntax Level 3 section 4.1 token railroad diagrams: one compact pass
     over the concrete token categories Cascade exposes. Unicode-range tokens
     are a tokenizer-layer gap: Cascade currently handles unicode-range syntax
     in the property parser instead of exposing a lexer token for it. *)
  check
    "foo calc( @media #id \"str\" url(a.png) url(a b) ? 1 2% 3px \t <!-- --> : \
     ; , [ ] ( ) { }"
    "<ident foo> <ws> <function calc(> <ws> <@media> <ws> <#id> <ws> <string \
     str> <ws> <url a.png> <ws> <bad-url> <ws> <delim '?'> <ws> <number 1> \
     <ws> <percentage 2%> <ws> <dimension 3px> <ws> <CDO> <ws> <CDC> <ws> <:> \
     <ws> <;> <ws> <,> <ws> <[> <ws> <]> <ws> <(> <ws> <)> <ws> <{> <ws> <}>"

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

let spec_escaping () =
  (* CSS Syntax Level 3 section 2.1: hex escapes are one to six hex digits
     followed by optional whitespace, so both forms spell the ident "&B". *)
  check {|\26 B \000026B|} "<ident &B> <ws> <ident &B>";
  check {|"B\26 W"|} "<string B&W>"

let spec_consume_token () =
  (* CSS Syntax Level 3 section 4.3.1: punctuation and malformed escapes map
     directly to the token taxonomy from section 4. *)
  check "():;,.[]" "<(> <)> <:> <;> <,> <delim '.'> <[> <]>";
  check {|@ # \|} {|<delim '@'> <ws> <delim '#'> <ws> <delim '\'>|}

let spec_comments () =
  (* CSS Syntax Level 3 section 4.3.2: comments are consumed before the next
     token and do not synthesize whitespace. *)
  check "a/*x*/b" "<ident a> <ident b>";
  check "a/*x*/ b" "<ident a> <ws> <ident b>"

let spec_numeric_tokens () =
  (* CSS Syntax Level 3 sections 4.3.3, 4.3.10, and 4.3.13. *)
  check "+10 -2.5 .5 1e-2"
    "<number +10> <ws> <number -2.5> <ws> <number .5> <ws> <number 1e-2>";
  check "10px 5% 1e3ms"
    "<dimension 10px> <ws> <percentage 5%> <ws> <dimension 1e3ms>"

let spec_ident_like_tokens () =
  (* CSS Syntax Level 3 sections 4.3.4, 4.3.9, and 4.3.12. *)
  check "-- <!-- -x \\26 B"
    "<ident --> <ws> <CDO> <ws> <ident -x> <ws> <ident &B>";
  check "calc(1) url(\"a.png\")"
    "<function calc(> <number 1> <)> <ws> <function url(> <string a.png> <)>"

let spec_string_tokens () =
  (* CSS Syntax Level 3 sections 4.3.5, 4.3.7, and 4.3.8. *)
  check "\"a\\\nb\"" "<string ab>";
  check {|"a\26 b"|} "<string a&b>";
  check "\"oops\n" "<bad-string> <ws>"

let url () =
  check "url(a.png)" "<url a.png>";
  check {|url("a.png")|} {|<function url(> <string a.png> <)>|}

let spec_url_tokens () =
  (* CSS Syntax Level 3 sections 4.3.6 and 4.3.15. *)
  check "url(  a.png  )" "<url a.png>";
  check "url(a\\ b.png)" "<url a b.png>";
  check "url(a b) foo" "<bad-url> <ws> <ident foo>"

(* Per 4.3.11, a newline inside a string produces a <bad-string> token; the
   newline is not consumed and becomes a subsequent <whitespace>. *)
let bad_string () = check "\"oops\n" "<bad-string> <ws>"
let bad_url () = check "url(a b)" "<bad-url>"

let suite =
  ( "lexer",
    [
      Alcotest.test_case "spec section 3.3 preprocessing" `Quick
        spec_preprocessing;
      Alcotest.test_case "spec section 4.1 token railroad diagrams" `Quick
        spec_token_railroad_diagrams;
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
      Alcotest.test_case "spec section 2.1 escaping" `Quick spec_escaping;
      Alcotest.test_case "spec section 4.3.1 consume token" `Quick
        spec_consume_token;
      Alcotest.test_case "spec section 4.3.2 comments" `Quick spec_comments;
      Alcotest.test_case "spec section 4.3.3 numeric tokens" `Quick
        spec_numeric_tokens;
      Alcotest.test_case "spec section 4.3.4 ident-like tokens" `Quick
        spec_ident_like_tokens;
      Alcotest.test_case "spec section 4.3.5 string tokens" `Quick
        spec_string_tokens;
      Alcotest.test_case "url" `Quick url;
      Alcotest.test_case "spec section 4.3.6 url tokens" `Quick spec_url_tokens;
      Alcotest.test_case "bad string" `Quick bad_string;
      Alcotest.test_case "bad url" `Quick bad_url;
    ] )
