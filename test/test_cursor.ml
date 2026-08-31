(** Cursor module tests. *)

open Cascade

let cursor_of_string s =
  let r = Reader.of_string s in
  let lexer = Lexer.of_reader r in
  let parser = Parser.of_lexer lexer in
  let rec loop acc =
    let cv = Parser.next parser in
    match cv with
    | Component.Preserved { kind = Token.Eof; _ } -> List.rev acc
    | _ -> loop (cv :: acc)
  in
  Cursor.of_components (loop [])

let test_ident () =
  let c = cursor_of_string "foo bar" in
  Alcotest.(check (option string)) "first" (Some "foo") (Cursor.ident_opt c);
  Alcotest.(check (option string)) "second" (Some "bar") (Cursor.ident_opt c);
  Alcotest.(check (option string)) "eof" None (Cursor.ident_opt c)

let test_number () =
  let c = cursor_of_string "1.5 42" in
  Alcotest.(check (option (float 0.001)))
    "first" (Some 1.5) (Cursor.number_opt c);
  Alcotest.(check (option int)) "second" (Some 42) (Cursor.integer_opt c)

(* CSS Values 4 sec. 5 represents an authored integer as an abstract integer.
   The lexer also retains its decimal representation, so converting through the
   rounded float must not change an integer that OCaml can represent. A token
   outside that finite range is not an unrelated machine integer. *)
let test_integer_precision () =
  let exact = cursor_of_string "9007199254740993" in
  Alcotest.(check (option int))
    "past float precision" (Some 9007199254740993) (Cursor.integer_opt exact);
  let unsupported = cursor_of_string "999999999999999999999999999999999999" in
  Alcotest.(check (option int))
    "outside the OCaml int range" None
    (Cursor.integer_opt unsupported)

let test_percentage_dimension () =
  let c = cursor_of_string "50% 10px" in
  Alcotest.(check (option (float 0.001)))
    "pct" (Some 50.0) (Cursor.percentage_opt c);
  match Cursor.dimension_opt c with
  | Some (n, u) ->
      Alcotest.(check (float 0.001)) "dim value" 10.0 n;
      Alcotest.(check string) "dim unit" "px" u
  | None -> Alcotest.fail "expected dimension"

let test_parens () =
  let c = cursor_of_string "(red)" in
  let inner = Cursor.parens (fun inner -> Cursor.ident inner) c in
  Alcotest.(check string) "inner" "red" inner;
  Alcotest.(check bool) "done" true (Cursor.is_done c)

let test_function_call () =
  let c = cursor_of_string "rgb(1, 2, 3)" in
  let triple =
    Cursor.function_call "rgb"
      (fun args ->
        let a = Cursor.int args in
        Cursor.comma args;
        let b = Cursor.int args in
        Cursor.comma args;
        let d = Cursor.int args in
        (a, b, d))
      c
  in
  match triple with
  | Some (1, 2, 3) -> ()
  | _ -> Alcotest.fail "expected rgb(1, 2, 3)"

let test_function_call_miss () =
  let c = cursor_of_string "calc(1)" in
  let r = Cursor.function_call "rgb" (fun _ -> ()) c in
  Alcotest.(check bool) "no match" true (r = None);
  Alcotest.(check (option string))
    "still there" (Some "calc")
    (match Cursor.peek c with
    | Some (Component.Func f) -> Some f.node.name
    | _ -> None)

let test_enum () =
  let c = cursor_of_string "block" in
  let v = Cursor.enum "display" [ ("block", `Block); ("inline", `Inline) ] c in
  Alcotest.(check bool) "block matched" true (v = `Block)

let test_enum_miss () =
  let c = cursor_of_string "weird" in
  match Cursor.enum "display" [ ("block", `Block) ] c with
  | exception Cursor.Parse_error e ->
      Alcotest.(check bool)
        "is bad value" true
        (match e.kind with Error.Bad_value _ -> true | _ -> false)
  | _ -> Alcotest.fail "expected Parse_error"

let test_err_unexpected () =
  let c = cursor_of_string "; " in
  match Cursor.ident c with
  | exception Cursor.Parse_error _ -> ()
  | _ -> Alcotest.fail "expected Parse_error"

(* [pair] and [triple] are backtracking combinators: a failure anywhere inside
   them must leave the cursor exactly where it started, the same contract every
   other failable [Cursor] combinator ([option], [one_of], [try_parse_err],
   [list]) already honours through [atomic]. *)
let boom t = Cursor.err_expected t "a later item"

let test_pair_rewinds_on_failure () =
  let c = cursor_of_string "foo" in
  (match Cursor.pair Cursor.ident boom c with
  | exception Cursor.Parse_error _ -> ()
  | _ -> Alcotest.fail "expected Parse_error");
  Alcotest.(check (option string))
    "cursor rewound past the first item" (Some "foo") (Cursor.ident_opt c)

let test_triple_rewinds_on_failure () =
  let c = cursor_of_string "foo bar" in
  (match Cursor.triple Cursor.ident Cursor.ident boom c with
  | exception Cursor.Parse_error _ -> ()
  | _ -> Alcotest.fail "expected Parse_error");
  Alcotest.(check (option string))
    "cursor rewound past both items" (Some "foo") (Cursor.ident_opt c);
  Alcotest.(check (option string))
    "second still there" (Some "bar") (Cursor.ident_opt c)

(* The one wording for an [~at_least] shortfall, shared with [Reader.list]. *)
let test_list_at_least_message () =
  let c = cursor_of_string "foo" in
  match Cursor.list ~at_least:2 Cursor.ident c with
  | exception Cursor.Parse_error e -> (
      match e.kind with
      | Error.Bad_value { reason; _ } ->
          Alcotest.(check string)
            "at_least message" "expected at least 2 items (got 1)" reason
      | _ -> Alcotest.fail "expected Bad_value")
  | _ -> Alcotest.fail "expected Parse_error"

let test_list_default_cardinality () =
  let required = cursor_of_string "" in
  (match Cursor.list Cursor.ident required with
  | exception Cursor.Parse_error _ -> ()
  | _ -> Alcotest.fail "expected the default list to require an item");
  let optional = cursor_of_string "" in
  Alcotest.(check (list string))
    "an empty list is explicit" []
    (Cursor.list ~at_least:0 Cursor.ident optional)

(* CSS Values 4 sec. 5.7.3: a [#] list's comma never trails the last item. [sep]
   must only commit once another [item] parses after it, or a trailing separator
   is silently dropped and the list looks complete when it is not. *)
let test_list_trailing_separator () =
  let c = cursor_of_string "a,b," in
  let items = Cursor.list ~sep:Cursor.comma Cursor.ident c in
  Alcotest.(check (list string)) "items" [ "a"; "b" ] items;
  Alcotest.(check bool)
    "trailing comma left for the caller, not consumed" false (Cursor.is_done c)

let test_list_no_trailing_separator () =
  let c = cursor_of_string "a,b" in
  let items = Cursor.list ~sep:Cursor.comma Cursor.ident c in
  Alcotest.(check (list string)) "items" [ "a"; "b" ] items;
  Alcotest.(check bool) "fully consumed" true (Cursor.is_done c)

let test_list_interior_whitespace () =
  let c = cursor_of_string "a , b" in
  let items = Cursor.list ~sep:Cursor.comma Cursor.ident c in
  Alcotest.(check (list string)) "items" [ "a"; "b" ] items;
  Alcotest.(check bool) "fully consumed" true (Cursor.is_done c)

let test_list_single_item_no_separator () =
  let c = cursor_of_string "a" in
  let items = Cursor.list ~sep:Cursor.comma Cursor.ident c in
  Alcotest.(check (list string)) "items" [ "a" ] items;
  Alcotest.(check bool) "fully consumed" true (Cursor.is_done c)

(* [call] hands [f] a sub-cursor over the function's argument list, but a reader
   that only recognises a prefix of it (e.g. one built from [option] or
   [one_of]) can return without raising. CSS Syntax 3 sec. 8.2 makes anything
   left over after that an invalid value, not a truncated one, so [call] must
   check the sub-cursor is exhausted itself once [f] returns. *)
let test_call_requires_eof () =
  let c = cursor_of_string "foo(1,2)" in
  match Cursor.call "foo" c (fun inner -> Cursor.number inner) with
  | (_ : float) -> Alcotest.fail "expected Parse_error"
  | exception Cursor.Parse_error _ -> ()

let test_call_full_consumption_succeeds () =
  let c = cursor_of_string "foo(1)" in
  let v = Cursor.call "foo" c (fun inner -> Cursor.number inner) in
  Alcotest.(check (float 0.)) "value" 1. v;
  Alcotest.(check bool)
    "outer cursor advanced past the call" true (Cursor.is_done c)

let test_call_interior_whitespace () =
  let c = cursor_of_string "foo( 1 )" in
  let v =
    Cursor.call "foo" c (fun inner ->
        Cursor.ws inner;
        let n = Cursor.number inner in
        Cursor.ws inner;
        n)
  in
  Alcotest.(check (float 0.)) "value" 1. v;
  Alcotest.(check bool)
    "outer cursor advanced past the call" true (Cursor.is_done c)

(* [parens], [brackets], [braces] and [function_call] build the same kind of
   sub-cursor as [call], so the same rule holds: CSS Syntax 3 (ED) sec. 5.4.1
   matches a grammar against the whole component-value list or returns failure,
   so a reader that stops on a prefix of a block has met an invalid value, not a
   shorter one. Real CSS reaches the hole through [width:calc((1px 2px))],
   [grid-template-columns:[a 1px] 1px] and an [image-set()] option whose
   [type(<string>)] carries a trailing token; browsers drop all three. *)
let test_parens_requires_eof () =
  let c = cursor_of_string "(red green)" in
  match Cursor.parens (fun inner -> Cursor.ident inner) c with
  | (_ : string) -> Alcotest.fail "expected Parse_error"
  | exception Cursor.Parse_error _ -> ()

let test_brackets_requires_eof () =
  let c = cursor_of_string "[red green]" in
  match Cursor.brackets (fun inner -> Cursor.ident inner) c with
  | (_ : string) -> Alcotest.fail "expected Parse_error"
  | exception Cursor.Parse_error _ -> ()

let test_braces_requires_eof () =
  let c = cursor_of_string "{red green}" in
  match Cursor.braces (fun inner -> Cursor.ident inner) c with
  | (_ : string) -> Alcotest.fail "expected Parse_error"
  | exception Cursor.Parse_error _ -> ()

let test_function_call_requires_eof () =
  let c = cursor_of_string "rgb(1 2)" in
  match Cursor.function_call "rgb" (fun inner -> Cursor.int inner) c with
  | (_ : int option) -> Alcotest.fail "expected Parse_error"
  | exception Cursor.Parse_error _ -> ()

let suite =
  ( "cursor",
    [
      Alcotest.test_case "ident" `Quick test_ident;
      Alcotest.test_case "number" `Quick test_number;
      Alcotest.test_case "integer precision" `Quick test_integer_precision;
      Alcotest.test_case "percentage / dimension" `Quick
        test_percentage_dimension;
      Alcotest.test_case "parens" `Quick test_parens;
      Alcotest.test_case "function call" `Quick test_function_call;
      Alcotest.test_case "function call miss" `Quick test_function_call_miss;
      Alcotest.test_case "enum" `Quick test_enum;
      Alcotest.test_case "enum miss" `Quick test_enum_miss;
      Alcotest.test_case "err unexpected" `Quick test_err_unexpected;
      Alcotest.test_case "pair rewinds on failure" `Quick
        test_pair_rewinds_on_failure;
      Alcotest.test_case "triple rewinds on failure" `Quick
        test_triple_rewinds_on_failure;
      Alcotest.test_case "list ~at_least message" `Quick
        test_list_at_least_message;
      Alcotest.test_case "list default cardinality" `Quick
        test_list_default_cardinality;
      Alcotest.test_case "list rejects a trailing separator" `Quick
        test_list_trailing_separator;
      Alcotest.test_case "list without a trailing separator" `Quick
        test_list_no_trailing_separator;
      Alcotest.test_case "list interior whitespace" `Quick
        test_list_interior_whitespace;
      Alcotest.test_case "list single item, no separator" `Quick
        test_list_single_item_no_separator;
      Alcotest.test_case "call requires eof" `Quick test_call_requires_eof;
      Alcotest.test_case "call full consumption succeeds" `Quick
        test_call_full_consumption_succeeds;
      Alcotest.test_case "call interior whitespace" `Quick
        test_call_interior_whitespace;
      Alcotest.test_case "parens requires eof" `Quick test_parens_requires_eof;
      Alcotest.test_case "brackets requires eof" `Quick
        test_brackets_requires_eof;
      Alcotest.test_case "braces requires eof" `Quick test_braces_requires_eof;
      Alcotest.test_case "function call requires eof" `Quick
        test_function_call_requires_eof;
    ] )
