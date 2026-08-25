(** Component IR tests. Component is pure data; these tests exercise [pp] /
    [to_string] on representative values. Round-trip via Parser is covered in
    test_parser.ml.

    [pp] is the located debug rendering of the stage-3 IR, the {!Token.pp} of a
    whole component tree - not source text. Source text comes from
    {!Parser.string_of_components} and its whitespace-policy variants. A debug
    rendering has to show what {!Component.equal} distinguishes, so every node
    carries its own {!Loc.t} and the [closed] / [terminated] flags are visible,
    and it has to read the same however the printer context was configured. *)

open Cascade

let tok k = Token.synthetic k

let wrap_block (node : Component.block) : Component.block Component.node =
  { node; loc = Loc.dummy }

let wrap_func (node : Component.func) : Component.func Component.node =
  { node; loc = Loc.dummy }

let check name cv expected =
  Alcotest.(check string) name expected (Component.to_string cv)

(* The first component of [src], as the parser produced it: real locations, real
   [closed] / [terminated] flags. *)
let first src =
  match Cursor.remaining (Cursor.of_string src) with
  | cv :: _ -> cv
  | [] -> Alcotest.fail ("no component in " ^ src)

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
    "{<ident red>@[0-0]}@[0-0]"

let block_paren () =
  check "paren block"
    (Component.Block
       (wrap_block
          {
            opening = Token.Paren;
            value = [ Component.Preserved (tok (Token.Ident "ok")) ];
            closed = true;
          }))
    "(<ident ok>@[0-0])@[0-0]"

let block_square () =
  check "square block"
    (Component.Block
       (wrap_block
          {
            opening = Token.Square;
            value = [ Component.Preserved (tok (Token.Ident "attr")) ];
            closed = true;
          }))
    "[<ident attr>@[0-0]]@[0-0]"

let func_call () =
  check "function call"
    (Component.Func
       (wrap_func
          {
            name = "rgb";
            arguments = [ Component.Preserved (tok (Token.Ident "x")) ];
            terminated = true;
          }))
    "rgb(<ident x>@[0-0])@[0-0]"

(* A component knows where it was read from, so the dump says so for every node,
   not only for the preserved tokens at the leaves. *)
let located_nodes () =
  check "located function" (first "rgb(1)") "rgb(<number 1>@[4-5])@[0-6]";
  check "located block" (first "[a]") "[<ident a>@[1-2]]@[0-3]"

(* CSS Syntax 3 sec. 5.4.6 forgives a value that runs to EOF, and the flag that
   records it is what typed validators reject on. Two components that
   {!Component.equal} separates must not dump alike. *)
let unterminated_function () =
  check "unterminated function" (first "rgb(1")
    "rgb(<number 1>@[4-5])<unterminated>@[0-5]"

let unclosed_block () =
  check "unclosed block" (first "[a") "[<ident a>@[1-2]]<unclosed>@[0-2]"

let distinct_values_dump_distinctly () =
  let terminated = first "rgb(1)" and unterminated = first "rgb(1" in
  Alcotest.(check bool)
    "equal separates them" false
    (Component.equal terminated unterminated);
  Alcotest.(check bool)
    "so does the dump" false
    (String.equal
       (Component.to_string terminated)
       (Component.to_string unterminated))

(* [Pp.sp] is layout whitespace and vanishes under minify. A dump has no
   minified form: the children stay apart however the context is configured, or
   [<ident a><ident b>] and a single ident become the same text. *)
let dump_ignores_minify () =
  let cv = first "(a b)" in
  Alcotest.(check string)
    "minified dump" (Component.to_string cv)
    (Pp.to_string ~minify:true Component.pp cv)

let suite =
  ( "component",
    [
      Alcotest.test_case "preserved ident" `Quick preserved_ident;
      Alcotest.test_case "preserved delim" `Quick preserved_delim;
      Alcotest.test_case "curly block" `Quick block_curly;
      Alcotest.test_case "paren block" `Quick block_paren;
      Alcotest.test_case "square block" `Quick block_square;
      Alcotest.test_case "function call" `Quick func_call;
      Alcotest.test_case "located nodes" `Quick located_nodes;
      Alcotest.test_case "unterminated function" `Quick unterminated_function;
      Alcotest.test_case "unclosed block" `Quick unclosed_block;
      Alcotest.test_case "distinct values dump distinctly" `Quick
        distinct_values_dump_distinctly;
      Alcotest.test_case "dump ignores minify" `Quick dump_ignores_minify;
    ] )
