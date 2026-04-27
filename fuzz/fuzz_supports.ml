(** Fuzz tests for the CSS Supports module.

    Tests crash safety of [\@supports] condition parsing and roundtrip. *)

open Cascade
open Alcobar

(** Supports.of_string — must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Supports.of_string buf)
  with Failure _ | Invalid_argument _ -> ()

(** Roundtrip: parse → to_string → parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some cond -> (
      let s = Css.Supports.to_string cond in
      try ignore (Css.Supports.of_string s)
      with Failure _ | Invalid_argument _ ->
        fail "supports roundtrip re-parse failed")

let test_serialization_idempotent buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
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
  | Css.Supports.Property _ | Css.Supports.Func _ -> false

let test_mixed_operator_serialization_reparse buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
  with
  | Some cond when has_mixed_operator cond ->
      let serialized = Css.Supports.to_string cond in
      let reparsed =
        try Some (Css.Supports.of_string serialized)
        with Failure _ | Invalid_argument _ -> None
      in
      if Option.is_none reparsed then
        fail "mixed and/or supports serialization did not reparse"
  | _ -> ()

(** pp — must not crash on any parsed condition. *)
let test_pp buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some cond -> ignore (Css.Supports.to_string cond)

(** compare — must not crash on any pair of parsed conditions. *)
let test_compare buf1 buf2 =
  match
    ( (try Some (Css.Supports.of_string buf1)
       with Failure _ | Invalid_argument _ -> None),
      try Some (Css.Supports.of_string buf2)
      with Failure _ | Invalid_argument _ -> None )
  with
  | Some a, Some b ->
      ignore (Css.Supports.compare a b);
      ignore (Css.Supports.equal a b)
  | _ -> ()

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let generated_condition buf =
  let property =
    pick [ ("display", "grid"); ("gap", "1rem"); ("color", "red") ] buf 0
  in
  let func =
    pick
      [
        Css.Supports.Func ("selector", ":has(img)");
        Css.Supports.Func ("font-format", "woff2");
        Css.Supports.Func ("font-tech", "variations");
      ]
      buf 1
  in
  let prop = Css.Supports.Property (fst property, snd property) in
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
    fail
      (Fmt.str "generated supports serialization changed: %S -> %S" once twice)

let test_supports_context_syntax buf =
  let condition = generated_condition buf in
  let serialized = Css.Supports.to_string condition in
  let reparsed = Css.Supports.(serialized |> of_string |> to_string) in
  if serialized <> reparsed then
    fail (Fmt.str "supports syntax changed: %S -> %S" serialized reparsed)

let spec_supports_vector buf =
  pick
    [
      ("(display: grid)", Css.Supports.Property ("display", "grid"));
      ("selector(:has(+ img))", Css.Supports.Func ("selector", ":has(+ img)"));
      ("font-format(woff2)", Css.Supports.Func ("font-format", "woff2"));
      ( "font-tech(color-COLRv1)",
        Css.Supports.Func ("font-tech", "color-COLRv1") );
      ( "not (display: grid)",
        Css.Supports.Not (Css.Supports.Property ("display", "grid")) );
      ( "(display: grid) and selector(:has(img))",
        Css.Supports.And
          ( Css.Supports.Property ("display", "grid"),
            Css.Supports.Func ("selector", ":has(img)") ) );
      ( "font-format(woff2) or font-tech(variations)",
        Css.Supports.Or
          ( Css.Supports.Func ("font-format", "woff2"),
            Css.Supports.Func ("font-tech", "variations") ) );
      ( "((display: grid) and (gap: 1rem)) or (color: red)",
        Css.Supports.Or
          ( Css.Supports.And
              ( Css.Supports.Property ("display", "grid"),
                Css.Supports.Property ("gap", "1rem") ),
            Css.Supports.Property ("color", "red") ) );
    ]
    buf 0

let test_spec_supports_structural_vectors buf =
  let input, expected = spec_supports_vector buf in
  let actual = Css.Supports.of_string input in
  if not (Css.Supports.equal expected actual) then
    fail
      (Fmt.str "supports vector parsed to wrong AST: %S -> %S" input
         (Css.Supports.to_string actual))

let test_spec_supports_invalid_vectors buf =
  let input =
    pick
      [
        "";
        "()";
        "display: grid";
        "(display: grid) and";
        "(display: grid) or";
        "(display: grid) and (gap: 1rem) or selector(:has(img))";
        "selector(:has(img)";
        "(display: grid";
      ]
      buf 0
  in
  match
    try Some (Css.Supports.of_string input)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some actual ->
      fail
        (Fmt.str "invalid supports vector parsed: %S -> %S" input
           (Css.Supports.to_string actual))

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
    ] )
