(** Fuzz tests for the CSS Supports module.

    Tests crash safety of [\@supports] condition parsing and roundtrip. *)

open Cascade
open Alcobar
module Supports_inventory = Cascade_spec_inventory.Supports_grammar

let rec supports_of_expected = function
  | Supports_inventory.Property (property, value) ->
      Css.Supports.property property value
  | Supports_inventory.Func (name, value) -> Css.Supports.func name value
  | Supports_inventory.Not condition ->
      Css.Supports.Not (supports_of_expected condition)
  | Supports_inventory.And (left, right) ->
      Css.Supports.And (supports_of_expected left, supports_of_expected right)
  | Supports_inventory.Or (left, right) ->
      Css.Supports.Or (supports_of_expected left, supports_of_expected right)

(** Supports.of_string -- must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Supports.of_string buf) with Cursor.Parse_error _ -> ()

(** Roundtrip: parse -> to_string -> parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Supports.of_string buf) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some cond -> (
      let s = Css.Supports.to_string cond in
      try ignore (Css.Supports.of_string s)
      with Cursor.Parse_error _ -> fail "supports roundtrip re-parse failed")

let test_serialization_idempotent buf =
  match
    try Some (Css.Supports.of_string buf) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some cond ->
      let once = Css.Supports.to_string cond in
      let twice = Css.Supports.(once |> of_string |> to_string) in
      if once <> twice then fail "supports serialization drifted"

let rec has_mixed_operator = function
  | Css.Supports.And (Css.Supports.Or _, _)
  | Css.Supports.And (_, Css.Supports.Or _)
  | Css.Supports.Or (Css.Supports.And _, _)
  | Css.Supports.Or (_, Css.Supports.And _) ->
      true
  | Css.Supports.And (a, b) | Css.Supports.Or (a, b) ->
      has_mixed_operator a || has_mixed_operator b
  | Css.Supports.Not a -> has_mixed_operator a
  | Css.Supports.Property _ | Css.Supports.Function _
  | Css.Supports.General_enclosed _ ->
      false

let test_mixed_operator_serialization_reparse buf =
  match
    try Some (Css.Supports.of_string buf) with Cursor.Parse_error _ -> None
  with
  | Some cond when has_mixed_operator cond ->
      let serialized = Css.Supports.to_string cond in
      let reparsed =
        try Some (Css.Supports.of_string serialized)
        with Cursor.Parse_error _ -> None
      in
      if Option.is_none reparsed then
        fail "mixed and/or supports serialization did not reparse"
  | _ -> ()

(** pp -- must not crash on any parsed condition. *)
let test_pp buf =
  match
    try Some (Css.Supports.of_string buf) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some cond -> ignore (Css.Supports.to_string cond)

(** compare -- must not crash on any pair of parsed conditions. *)
let test_compare buf1 buf2 =
  match
    ( (try Some (Css.Supports.of_string buf1) with Cursor.Parse_error _ -> None),
      try Some (Css.Supports.of_string buf2) with Cursor.Parse_error _ -> None
    )
  with
  | Some a, Some b ->
      ignore (Css.Supports.compare a b);
      ignore (Css.Supports.equal a b)
  | _ -> ()

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let recovered_css label css =
  match Css.of_string ~strict:false css with
  | Ok parsed -> parsed
  | Error err ->
      failf "%s did not recover leniently: %s" label
        (Cascade.Error.to_string err)

let assert_invalid_supports_contract label input =
  let css = "@supports " ^ input ^ "{.x{color:red}}" in
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      failf "%s parsed strictly as invalid supports condition: %S -> %S" label
        input
        (Css.to_string ~minify:true parsed.stylesheet)
  | Error _ ->
      let { Css.warnings; stylesheet; _ } = recovered_css label css in
      ignore (Css.to_string ~minify:true stylesheet : string);
      if warnings = [] then
        failf "%s recovered without a lenient warning: %S" label input

let generated_condition buf =
  let property =
    pick [ ("display", "grid"); ("gap", "1rem"); ("color", "red") ] buf 0
  in
  let func =
    pick
      [
        Css.Supports.func "selector" ":has(img)";
        Css.Supports.func "font-format" "woff2";
        Css.Supports.func "font-tech" "variations";
        Css.Supports.func "at-rule" "@container";
        Css.Supports.func "at-rule" "@charset";
        Css.Supports.func "named-feature" "--compact";
        Css.Supports.func "env" "safe-area-inset-top";
      ]
      buf 1
  in
  let prop = Css.Supports.property (fst property) (snd property) in
  match byte_at buf 2 mod 6 with
  | 0 -> prop
  | 1 -> func
  | 2 -> Css.Supports.Not prop
  | 3 -> Css.Supports.And (prop, func)
  | 4 -> Css.Supports.Or (Css.Supports.Not prop, func)
  | _ -> Css.Supports.Not (Css.Supports.Or (prop, func))

let test_generated_condition_serialization_idempotent buf =
  let condition = generated_condition buf in
  let once = Css.Supports.to_string condition in
  let twice = Css.Supports.(once |> of_string |> to_string) in
  if once <> twice then
    failf "generated supports serialization changed: %S -> %S" once twice

let test_supports_context_syntax buf =
  let condition = generated_condition buf in
  let serialized = Css.Supports.to_string condition in
  let reparsed = Css.Supports.(serialized |> of_string |> to_string) in
  if serialized <> reparsed then
    failf "supports syntax changed: %S -> %S" serialized reparsed

let spec_supports_vector buf = pick Supports_inventory.rows buf 0

let test_spec_supports_structural_vectors buf =
  let row = spec_supports_vector buf in
  let actual = Css.Supports.of_string row.input in
  if not (Css.Supports.equal (supports_of_expected row.expected) actual) then
    failf "supports vector parsed to wrong AST: %S -> %S" row.input
      (Css.Supports.to_string actual)

let test_spec_supports_invalid_vectors buf =
  let input = pick Supports_inventory.invalid buf 0 in
  assert_invalid_supports_contract "invalid supports vector" input

let test_supports_conditions buf =
  let row = pick Supports_inventory.rows buf 2 in
  let input = row.input in
  let once = Css.Supports.to_string (Css.Supports.of_string input) in
  let twice = Css.Supports.to_string (Css.Supports.of_string once) in
  if once <> twice then
    failf "supports condition family serialization drifted: %S -> %S" once twice

let test_supports_invalid_conditions buf =
  let row = pick Supports_inventory.rows buf 3 in
  let input = Supports_inventory.mutate_invalid row (byte_at buf 4) in
  assert_invalid_supports_contract "invalid supports condition family vector"
    input

let suite =
  ( "supports",
    [
      test_case "of_string crash safety" [ bytes ] test_of_string;
      test_case "roundtrip" [ bytes ] test_roundtrip;
      test_case "serialization idempotent" [ bytes ]
        test_serialization_idempotent;
      test_case "mixed operator serialization reparses" [ bytes ]
        test_mixed_operator_serialization_reparse;
      test_case "pp crash safety" [ bytes ] test_pp;
      test_case "compare crash safety" [ bytes; bytes ] test_compare;
      test_case "generated condition serialization idempotent" [ bytes ]
        test_generated_condition_serialization_idempotent;
      test_case "supports context syntax invariant" [ bytes ]
        test_supports_context_syntax;
      test_case "spec supports structural vectors" [ bytes ]
        test_spec_supports_structural_vectors;
      test_case "spec supports invalid vectors rejected" [ bytes ]
        test_spec_supports_invalid_vectors;
      test_case "spec supports condition family vectors" [ bytes ]
        test_supports_conditions;
      test_case "spec invalid supports condition family vectors rejected"
        [ bytes ] test_supports_invalid_conditions;
    ] )
