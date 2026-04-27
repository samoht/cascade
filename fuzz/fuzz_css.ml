(** Fuzz tests for the public Css module surface. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*\n\
     \t"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let generated_stylesheet buf =
  let selector name = Css.Selector.class_ name in
  let color i =
    Css.color (Css.hex (pick [ "#ff0000"; "#00ff00"; "#0000ff" ] buf i))
  in
  Css.v
    [
      Css.rule ~selector:(selector "card") [ color 0 ];
      Css.media ~condition:(Css.Media.Raw "(width >= 40em)")
        [
          Css.supports
            ~condition:(Css.Supports.Property ("display", "grid"))
            [
              Css.container
                ~condition:(Css.Container.Raw "(inline-size > 30em)")
                [ Css.rule ~selector:(selector "inside") [ color 4 ] ];
            ];
        ];
      Css.layer
        ~name:(pick [ "base"; "theme"; "components" ] buf 8)
        [ Css.rule ~selector:(selector "layered") [ color 12 ] ];
    ]

let minified ss = Css.to_string ~minify:true ss |> String.trim

let test_parse_crash_safety buf =
  ignore (Css.parse (cssish buf));
  ignore (Css.of_string (cssish buf))

let test_generated_public_roundtrip buf =
  let sheet = generated_stylesheet buf in
  let once = minified sheet in
  match Css.of_string once with
  | Error err ->
      fail
        (Fmt.str "public generated stylesheet did not parse: %s"
           (Css.pp_parse_error err))
  | Ok parsed ->
      let twice = minified parsed in
      if once <> twice then
        fail
          (Fmt.str "public generated stylesheet changed: %S -> %S" once twice)

let test_parse_partial_stringify_reparse buf =
  let parsed = Css.parse (cssish buf) in
  let serialized = minified parsed.Css.stylesheet in
  match Css.of_string serialized with
  | Ok _ -> ()
  | Error err ->
      fail
        (Fmt.str "Css.parse output did not reparse strictly: %s"
           (Css.pp_parse_error err))

let test_map_preserves_rules buf =
  let sheet = generated_stylesheet buf in
  let before = List.length (Css.rule_statements sheet) in
  let mapped =
    Css.map
      (fun sel _decls ->
        Css.rule ~selector:sel [ Css.color (Css.hex "#000000") ])
      (Css.statements sheet)
    |> Css.v
  in
  let after = List.length (Css.rule_statements mapped) in
  if before <> after then
    fail (Fmt.str "Css.map changed rule count: %d -> %d" before after)

let test_public_sort_idempotent buf =
  let cmp (sel1, _) (sel2, _) =
    String.compare (Css.Selector.to_string sel1) (Css.Selector.to_string sel2)
  in
  let sheet = generated_stylesheet buf in
  let once = Css.sort cmp (Css.statements sheet) |> Css.v |> minified in
  let twice =
    match Css.of_string once with
    | Error err -> fail (Css.pp_parse_error err)
    | Ok parsed -> Css.sort cmp (Css.statements parsed) |> Css.v |> minified
  in
  if once <> twice then
    fail (Fmt.str "Css.sort changed after reparse: %S -> %S" once twice)

let test_public_optimize_idempotent buf =
  let sheet = generated_stylesheet buf in
  let once = Css.to_string ~minify:true ~optimize:true sheet |> String.trim in
  match Css.of_string once with
  | Error err -> fail (Css.pp_parse_error err)
  | Ok parsed ->
      let twice =
        Css.to_string ~minify:true ~optimize:true parsed |> String.trim
      in
      if once <> twice then
        fail
          (Fmt.str "public optimize output changed after reparse: %S -> %S" once
             twice)

let suite =
  ( "css",
    [
      test_case "parse crash safety" [ bytes ] test_parse_crash_safety;
      test_case "generated public roundtrip" [ bytes ]
        test_generated_public_roundtrip;
      test_case "parse partial stringify reparse" [ bytes ]
        test_parse_partial_stringify_reparse;
      test_case "public map preserves rule count" [ bytes ]
        test_map_preserves_rules;
      test_case "public sort idempotent" [ bytes ] test_public_sort_idempotent;
      test_case "public optimize idempotent" [ bytes ]
        test_public_optimize_idempotent;
    ] )
