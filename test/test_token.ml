(** Token type module tests. The Token module is pure data: these tests exercise
    [pp_kind] for the payload variants. End-to-end tokenization (with source
    locations) is tested in test_lexer.ml. *)

open Cascade

let check name kind expected =
  Alcotest.(check string) name expected (Css.Pp.to_string Token.pp_kind kind)

let idents () = check "ident" (Token.Ident "foo") "<ident foo>"
let function_tok () = check "function" (Token.Function "rgb") "<function rgb(>"
let at_keyword () = check "at-keyword" (Token.At_keyword "media") "<@media>"

let hash () =
  check "hash" (Token.Hash { value = "abc"; hash_flag = Token.Id }) "<#abc>"

let brackets () =
  check "open curly" (Token.Open Token.Curly) "<{>";
  check "close curly" (Token.Close Token.Curly) "<}>";
  check "open paren" (Token.Open Token.Paren) "<(>";
  check "close paren" (Token.Close Token.Paren) "<)>";
  check "open square" (Token.Open Token.Square) "<[>";
  check "close square" (Token.Close Token.Square) "<]>"

let simple () =
  check "colon" Token.Colon "<:>";
  check "semicolon" Token.Semicolon "<;>";
  check "comma" Token.Comma "<,>";
  check "whitespace" Token.Whitespace "<ws>";
  check "cdo" Token.Cdo "<CDO>";
  check "cdc" Token.Cdc "<CDC>";
  check "eof" Token.Eof "<eof>"

let bad () =
  check "bad-string" Token.Bad_string "<bad-string>";
  check "bad-url" Token.Bad_url "<bad-url>"

let suite =
  ( "token",
    [
      Alcotest.test_case "idents" `Quick idents;
      Alcotest.test_case "function" `Quick function_tok;
      Alcotest.test_case "at-keyword" `Quick at_keyword;
      Alcotest.test_case "hash" `Quick hash;
      Alcotest.test_case "brackets" `Quick brackets;
      Alcotest.test_case "simple tokens" `Quick simple;
      Alcotest.test_case "bad tokens" `Quick bad;
    ] )
