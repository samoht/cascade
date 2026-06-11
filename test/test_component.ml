(** Component IR tests. Component is pure data; these tests exercise [pp] /
    [to_string] on representative values. Round-trip via Parser is covered in
    test_parser.ml. *)

open Cascade

let tok k = Token.synthetic k

let wrap_block (node : Component.block) : Component.block Component.node =
  { node; loc = Loc.dummy }

let wrap_func (node : Component.func) : Component.func Component.node =
  { node; loc = Loc.dummy }

let check name cv expected =
  Alcotest.(check string) name expected (Component.to_string cv)

let preserved_ident () =
  check "preserved ident"
    (Component.Preserved (tok (Token.Ident "foo")))
    "<ident foo>@[0-0]"

let preserved_delim () =
  check "preserved delim"
    (Component.Preserved (tok (Token.Delim "+")))
    "<delim '+'>@[0-0]"

let block_curly () =
  check "curly block"
    (Component.Block
       (wrap_block
          {
            opening = Token.Curly;
            value = [ Component.Preserved (tok (Token.Ident "red")) ];
            closed = true;
          }))
    "{<ident red>@[0-0]}"

let block_paren () =
  check "paren block"
    (Component.Block
       (wrap_block
          {
            opening = Token.Paren;
            value = [ Component.Preserved (tok (Token.Ident "ok")) ];
            closed = true;
          }))
    "(<ident ok>@[0-0])"

let block_square () =
  check "square block"
    (Component.Block
       (wrap_block
          {
            opening = Token.Square;
            value = [ Component.Preserved (tok (Token.Ident "attr")) ];
            closed = true;
          }))
    "[<ident attr>@[0-0]]"

let func_call () =
  check "function call"
    (Component.Func
       (wrap_func
          {
            name = "rgb";
            arguments = [ Component.Preserved (tok (Token.Ident "x")) ];
            terminated = true;
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
