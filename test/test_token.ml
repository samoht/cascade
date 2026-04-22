(** Token type module tests. The Token module is pure data: these tests exercise
    [pp] / [to_string] for the variants. End-to-end tokenization is tested in
    test_lexer.ml. *)

open Cascade

let check name tok expected =
  Alcotest.(check string) name expected (Css.Token.to_string tok)

let idents () = check "ident" (Css.Token.Ident "foo") "<ident foo>"

let function_tok () =
  check "function" (Css.Token.Function "rgb") "<function rgb(>"

let at_keyword () = check "at-keyword" (Css.Token.At_keyword "media") "<@media>"

let hash () =
  check "hash"
    (Css.Token.Hash { value = "abc"; hash_flag = Css.Token.Id })
    "<#abc>"

let brackets () =
  check "open curly" (Css.Token.Open Css.Token.Curly) "<{>";
  check "close curly" (Css.Token.Close Css.Token.Curly) "<}>";
  check "open paren" (Css.Token.Open Css.Token.Paren) "<(>";
  check "close paren" (Css.Token.Close Css.Token.Paren) "<)>";
  check "open square" (Css.Token.Open Css.Token.Square) "<[>";
  check "close square" (Css.Token.Close Css.Token.Square) "<]>"

let simple () =
  check "colon" Css.Token.Colon "<:>";
  check "semicolon" Css.Token.Semicolon "<;>";
  check "comma" Css.Token.Comma "<,>";
  check "whitespace" Css.Token.Whitespace "<ws>";
  check "cdo" Css.Token.Cdo "<CDO>";
  check "cdc" Css.Token.Cdc "<CDC>";
  check "eof" Css.Token.Eof "<eof>"

let bad () =
  check "bad-string" Css.Token.Bad_string "<bad-string>";
  check "bad-url" Css.Token.Bad_url "<bad-url>"

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
