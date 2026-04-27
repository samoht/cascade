(** Fuzz tests for the CSS Syntax lexer. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*\n\
     \r\t"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let rec take_kinds lexer limit acc =
  if limit = 0 then List.rev acc
  else
    let tok = Css.Lexer.next lexer in
    match tok.Css.Token.kind with
    | Css.Token.Eof -> List.rev (Css.Token.Eof :: acc)
    | kind -> take_kinds lexer (limit - 1) (kind :: acc)

let kinds input = take_kinds (Css.Lexer.of_string input) 4096 []
let kind_string kind = Css.Pp.to_string Css.Token.pp_kind kind
let kind_strings input = List.map kind_string (kinds input)
let test_tokenization_crash_safety buf = ignore (kinds (cssish buf))

let test_tokenization_deterministic buf =
  let input = cssish buf in
  let a = kind_strings input in
  let b = kind_strings input in
  if a <> b then fail "lexer tokenization is not deterministic"

let test_peek_next_consistency buf =
  let lexer = Css.Lexer.of_string (cssish buf) in
  let peeked = Css.Lexer.peek lexer in
  let next = Css.Lexer.next lexer in
  if peeked.Css.Token.kind <> next.Css.Token.kind then
    fail "lexer peek and next returned different token kinds"

let test_reconsume_returns_same_token buf =
  let lexer = Css.Lexer.of_string (cssish buf) in
  let tok = Css.Lexer.next lexer in
  Css.Lexer.reconsume lexer tok;
  let again = Css.Lexer.next lexer in
  if tok.Css.Token.kind <> again.Css.Token.kind then
    fail "lexer reconsume did not replay the same token kind"

let test_save_restore_replays_prefix buf =
  let input = cssish buf in
  let lexer = Css.Lexer.of_string input in
  Css.Lexer.save lexer;
  let first = take_kinds lexer 8 [] in
  Css.Lexer.restore lexer;
  let second = take_kinds lexer 8 [] in
  if first <> second then fail "lexer restore did not replay consumed tokens"

let test_commit_preserves_outer_restore buf =
  let input = cssish buf in
  let lexer = Css.Lexer.of_string input in
  Css.Lexer.save lexer;
  let first = Css.Lexer.next lexer in
  Css.Lexer.save lexer;
  ignore (Css.Lexer.next lexer);
  Css.Lexer.commit lexer;
  Css.Lexer.restore lexer;
  let replayed = Css.Lexer.next lexer in
  if first.Css.Token.kind <> replayed.Css.Token.kind then
    fail "lexer commit did not fold replay log into outer save"

let test_eof_stable buf =
  let lexer = Css.Lexer.of_string (cssish buf) in
  ignore (take_kinds lexer 4096 []);
  let a = Css.Lexer.next lexer in
  let b = Css.Lexer.next lexer in
  match (a.Css.Token.kind, b.Css.Token.kind) with
  | Css.Token.Eof, Css.Token.Eof -> ()
  | _ -> fail "lexer EOF is not stable"

let suite =
  ( "lexer",
    [
      test_case "tokenization crash safety" [ bytes ]
        test_tokenization_crash_safety;
      test_case "tokenization deterministic" [ bytes ]
        test_tokenization_deterministic;
      test_case "peek/next consistency" [ bytes ] test_peek_next_consistency;
      test_case "reconsume returns same token" [ bytes ]
        test_reconsume_returns_same_token;
      test_case "save/restore replays prefix" [ bytes ]
        test_save_restore_replays_prefix;
      test_case "commit preserves outer restore" [ bytes ]
        test_commit_preserves_outer_restore;
      test_case "eof stable" [ bytes ] test_eof_stable;
    ] )
