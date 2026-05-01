(** Fuzz tests for the CSS Syntax component-value parser and serializer. *)

open Cascade
open Alcobar

let parse_list input =
  (Css.Parser.parse_list_of_component_values (Css.Reader.of_string input)).value

let parse_comma_list input =
  (Css.Parser.parse_csv_component_values (Css.Reader.of_string input)).value

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let minified input = input |> parse_list |> Css.Parser.to_string_minified
let serialized input = input |> parse_list |> Css.Parser.to_string

let bracket_shape = function
  | Css.Token.Curly -> "{}"
  | Css.Token.Paren -> "()"
  | Css.Token.Square -> "[]"

let rec shape = function
  | Css.Component.Preserved { kind = Css.Token.Whitespace; _ } -> None
  | Css.Component.Preserved tok ->
      Some (Css.Pp.to_string Css.Token.pp_kind tok.kind)
  | Css.Component.Block { node = { opening; value; _ }; _ } ->
      let inner = value |> List.filter_map shape |> String.concat "," in
      Some (Fmt.str "block(%s:%s)" (bracket_shape opening) inner)
  | Css.Component.Func { node = { name; arguments; _ }; _ } ->
      let inner = arguments |> List.filter_map shape |> String.concat "," in
      Some (Fmt.str "function(%s:%s)" name inner)

let shapes cvs = List.filter_map shape cvs
let comma_shapes groups = List.map shapes groups

let assert_same_shape label input output =
  let before = parse_list input |> shapes in
  let after = parse_list output |> shapes in
  if before <> after then
    fail
      (Fmt.str "%s changed non-whitespace component shape for %S: %S -> %S"
         label input output
         (String.concat " -> " after))

let has_quote_or_escape s =
  String.contains s '"' || String.contains s '\'' || String.contains s '\\'

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let repeated c n = String.init n (fun _ -> c)

let string_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])

(** Component-value parsing must not crash on decoded CSS-shaped input. *)
let test_component_value_crash_safety buf =
  let buf = cssish buf in
  ignore (Css.Parser.parse_component_value (Css.Reader.of_string buf));
  ignore (Css.Parser.parse_list_of_component_values (Css.Reader.of_string buf));
  ignore (Css.Parser.parse_csv_component_values (Css.Reader.of_string buf))

(** Minified serialization should be idempotent after reparsing. *)
let test_component_value_minified_idempotent buf =
  let buf = cssish buf in
  let once = minified buf in
  let twice = minified once in
  if once <> twice then
    fail
      (Fmt.str "component-value minified serialization changed for %S: %S -> %S"
         buf once twice)

(** Non-minified serialization should be idempotent after reparsing. *)
let test_component_value_serialized_idempotent buf =
  let buf = cssish buf in
  let once = serialized buf in
  let twice = serialized once in
  if once <> twice then
    fail
      (Fmt.str "component-value serialization changed for %S: %S -> %S" buf once
         twice)

(** Serialization output should always be accepted by the component parser. *)
let test_component_value_serialized_reparse buf =
  let buf = cssish buf in
  let css = serialized buf in
  ignore (parse_list css);
  let css = minified buf in
  ignore (parse_list css)

(** CSS Syntax section 9: serialization may collapse whitespace, but must not
    otherwise change the parsed component-value data structures. *)
let test_component_shape_roundtrip buf =
  let buf = cssish buf in
  assert_same_shape "serialization" buf (serialized buf);
  assert_same_shape "minified serialization" buf (minified buf)

(** CSS Syntax section 9: consecutive token pairs must not be serialized in a
    way that lets the tokenizer merge them into fewer component values. *)
let test_token_boundary_roundtrip left right =
  let left = cssish left in
  let right = cssish right in
  let input = left ^ " " ^ right in
  if not (has_quote_or_escape input) then
    assert_same_shape "token boundary serialization" input (minified input)

let test_csv_group_roundtrip buf =
  let buf = cssish buf in
  if buf <> "" then
    let input = buf ^ ", " ^ cssish (string_rev buf) in
    let before = parse_comma_list input |> comma_shapes in
    let serialized =
      input |> parse_comma_list
      |> List.map Css.Parser.to_string_minified
      |> String.concat ","
    in
    let after = parse_comma_list serialized |> comma_shapes in
    if before <> after then
      fail (Fmt.str "comma-list shape changed for %S: %S" input serialized)

let test_block_commas_stable buf =
  let payload =
    cssish buf
    |> String.map (function
      | '(' | ')' | '[' | ']' | '{' | '}' | '"' | '\'' | '\\' -> 'x'
      | c -> c)
  in
  let input = "fn(" ^ payload ^ ", x), [a,b], {c:d,e:f}" in
  let groups = parse_comma_list input in
  if List.length groups <> 3 then
    fail
      (Fmt.str "top-level comma grouping changed; expected 3 groups, got %d"
         (List.length groups))

let test_bounded_unterminated_nesting_reparse buf =
  let depth = 1 + (byte_at buf 0 mod 256) in
  let opener =
    match byte_at buf 1 mod 3 with 0 -> '(' | 1 -> '[' | _ -> '{'
  in
  let input = repeated opener depth ^ "x" in
  let serialized = serialized input in
  ignore (parse_list serialized);
  let minified = minified input in
  ignore (parse_list minified)

let test_balanced_nesting_shape_roundtrip buf =
  let depth = 1 + (byte_at buf 0 mod 128) in
  let input = repeated '(' depth ^ "x" ^ repeated ')' depth in
  assert_same_shape "balanced deep nesting serialization" input
    (serialized input);
  assert_same_shape "balanced deep nesting minification" input (minified input)

let test_comment_confusion_stable buf =
  let payload =
    cssish buf |> String.map (function '/' | '*' -> 'x' | c -> c)
  in
  let input = "safe/*" ^ payload ^ ".evil{color:red}" in
  let output = minified input in
  if String.contains output '{' || String.contains output '}' then
    fail (Fmt.str "unterminated comment surfaced block syntax: %S" output)

let test_spec_parser_branch_vectors buf =
  let row =
    List.nth Cascade_spec_inventory.Syntax_grammar.parser_rows
      (byte_at buf 0
      mod List.length Cascade_spec_inventory.Syntax_grammar.parser_rows)
  in
  let input =
    if byte_at buf 1 mod 2 = 0 then row.input
    else
      Cascade_spec_inventory.Syntax_grammar.mutate_parser_input row
        (byte_at buf 2)
  in
  let before = parse_list input |> shapes in
  let serialized = serialized input in
  let after = parse_list serialized |> shapes in
  if (not (has_quote_or_escape input)) && before <> after then
    fail
      (Fmt.str "CSS Syntax parser branch vector changed shape: %S -> %S" input
         serialized)

let suite =
  ( "parser",
    [
      test_case "component-value crash safety" [ bytes ]
        test_component_value_crash_safety;
      test_case "component-value minified serialization idempotent" [ bytes ]
        test_component_value_minified_idempotent;
      test_case "component-value serialization idempotent" [ bytes ]
        test_component_value_serialized_idempotent;
      test_case "component-value serialized reparse" [ bytes ]
        test_component_value_serialized_reparse;
      test_case "component-value serialization preserves shape" [ bytes ]
        test_component_shape_roundtrip;
      test_case "component-value token boundaries roundtrip" [ bytes; bytes ]
        test_token_boundary_roundtrip;
      test_case "comma-list group count roundtrip" [ bytes ]
        test_csv_group_roundtrip;
      test_case "comma inside blocks does not split groups" [ bytes ]
        test_block_commas_stable;
      test_case "bounded unterminated nesting reparses" [ bytes ]
        test_bounded_unterminated_nesting_reparse;
      test_case "balanced nesting shape roundtrip" [ bytes ]
        test_balanced_nesting_shape_roundtrip;
      test_case "comment confusion does not surface components" [ bytes ]
        test_comment_confusion_stable;
      test_case "spec parser branch vectors" [ bytes ]
        test_spec_parser_branch_vectors;
    ] )
