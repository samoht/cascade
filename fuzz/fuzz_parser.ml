(** Fuzz tests for the CSS Syntax component-value parser and serializer. *)

open Cascade
open Alcobar

let parse_list input =
  (Css.Parser.parse_list_of_component_values (Css.Reader.of_string input)).value

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
  | Css.Component.Block { node = { opening; value }; _ } ->
      let inner = value |> List.filter_map shape |> String.concat "," in
      Some (Fmt.str "block(%s:%s)" (bracket_shape opening) inner)
  | Css.Component.Func { node = { name; arguments }; _ } ->
      let inner = arguments |> List.filter_map shape |> String.concat "," in
      Some (Fmt.str "function(%s:%s)" name inner)

let shapes cvs = List.filter_map shape cvs

let assert_same_shape label input output =
  let before = parse_list input |> shapes in
  let after = parse_list output |> shapes in
  if before <> after then
    fail
      (Fmt.str "%s changed non-whitespace component shape for %S: %S -> %S"
         label input output
         (String.concat " -> " after))

(** Component-value parsing must not crash on decoded CSS-shaped input. *)
let test_component_value_crash_safety buf =
  let buf = cssish buf in
  ignore (Css.Parser.parse_component_value (Css.Reader.of_string buf));
  ignore (Css.Parser.parse_list_of_component_values (Css.Reader.of_string buf));
  ignore
    (Css.Parser.parse_comma_separated_list_of_component_values
       (Css.Reader.of_string buf))

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
let test_component_value_serialized_shape_roundtrip buf =
  let buf = cssish buf in
  assert_same_shape "serialization" buf (serialized buf);
  assert_same_shape "minified serialization" buf (minified buf)

(** CSS Syntax section 9: consecutive token pairs must not be serialized in a
    way that lets the tokenizer merge them into fewer component values. *)
let test_component_value_token_boundary_roundtrip left right =
  let left = cssish left in
  let right = cssish right in
  let input = left ^ " " ^ right in
  assert_same_shape "token boundary serialization" input (minified input)

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
        test_component_value_serialized_shape_roundtrip;
      test_case "component-value token boundaries roundtrip" [ bytes; bytes ]
        test_component_value_token_boundary_roundtrip;
    ] )
