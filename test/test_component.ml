(** Component IR tests. Component is pure data; these tests exercise [pp] /
    [to_string] on representative values. Round-trip via Parser is covered in
    test_parser.ml. *)

open Cascade

let tok k = Css.Token.synthetic k

let wrap_block (node : Css.Component.block) :
    Css.Component.block Css.Component.node =
  { node; loc = Css.Loc.dummy }

let wrap_func (node : Css.Component.func) :
    Css.Component.func Css.Component.node =
  { node; loc = Css.Loc.dummy }

let check name cv expected =
  Alcotest.(check string) name expected (Css.Component.to_string cv)

let preserved_ident () =
  check "preserved ident"
    (Css.Component.Preserved (tok (Css.Token.Ident "foo")))
    "<ident foo>@[0-0]"

let preserved_delim () =
  check "preserved delim"
    (Css.Component.Preserved (tok (Css.Token.Delim "+")))
    "<delim '+'>@[0-0]"

let block_curly () =
  check "curly block"
    (Css.Component.Block
       (wrap_block
          {
            opening = Css.Token.Curly;
            value = [ Css.Component.Preserved (tok (Css.Token.Ident "red")) ];
          }))
    "{<ident red>@[0-0]}"

let block_paren () =
  check "paren block"
    (Css.Component.Block
       (wrap_block
          {
            opening = Css.Token.Paren;
            value = [ Css.Component.Preserved (tok (Css.Token.Ident "ok")) ];
          }))
    "(<ident ok>@[0-0])"

let block_square () =
  check "square block"
    (Css.Component.Block
       (wrap_block
          {
            opening = Css.Token.Square;
            value = [ Css.Component.Preserved (tok (Css.Token.Ident "attr")) ];
          }))
    "[<ident attr>@[0-0]]"

let func_call () =
  check "function call"
    (Css.Component.Func
       (wrap_func
          {
            name = "rgb";
            arguments = [ Css.Component.Preserved (tok (Css.Token.Ident "x")) ];
          }))
    "rgb(<ident x>@[0-0])"

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
