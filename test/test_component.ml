(** Component IR tests. Component is pure data; these tests exercise [pp] /
    [to_string] on representative values. Round-trip via Parser is covered in
    test_parser.ml. *)

open Cascade

let check name cv expected =
  Alcotest.(check string) name expected (Css.Component.to_string cv)

let preserved_ident () =
  check "preserved ident" (Css.Component.Preserved (Css.Token.Ident "foo"))
    "<ident foo>"

let preserved_delim () =
  check "preserved delim" (Css.Component.Preserved (Css.Token.Delim '+'))
    "<delim '+'>"

let block_curly () =
  check "curly block"
    (Css.Component.Block
       {
         opening = Css.Token.Curly;
         value = [ Css.Component.Preserved (Css.Token.Ident "red") ];
       })
    "{<ident red>}"

let block_paren () =
  check "paren block"
    (Css.Component.Block
       {
         opening = Css.Token.Paren;
         value = [ Css.Component.Preserved (Css.Token.Ident "ok") ];
       })
    "(<ident ok>)"

let block_square () =
  check "square block"
    (Css.Component.Block
       {
         opening = Css.Token.Square;
         value = [ Css.Component.Preserved (Css.Token.Ident "attr") ];
       })
    "[<ident attr>]"

let func_call () =
  check "function call"
    (Css.Component.Func
       {
         name = "rgb";
         arguments = [ Css.Component.Preserved (Css.Token.Ident "x") ];
       })
    "rgb(<ident x>)"

let suite =
  ( "component",
    [
      Alcotest.test_case "preserved ident" `Quick preserved_ident;
      Alcotest.test_case "preserved delim" `Quick preserved_delim;
      Alcotest.test_case "curly block" `Quick block_curly;
      Alcotest.test_case "paren block" `Quick block_paren;
      Alcotest.test_case "square block" `Quick block_square;
      Alcotest.test_case "function call" `Quick func_call;
    ] )
