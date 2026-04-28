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
      Css.media
        ~condition:(Css.Media.of_string "(width >= 40em)")
        [
          Css.supports
            ~condition:(Css.Supports.Property ("display", "grid"))
            [
              Css.container
                ~condition:(Css.Container.of_string "(inline-size > 30em)")
                [ Css.rule ~selector:(selector "inside") [ color 4 ] ];
            ];
        ];
      Css.layer
        ~name:(pick [ "base"; "theme"; "components" ] buf 8)
        [ Css.rule ~selector:(selector "layered") [ color 12 ] ];
    ]

let generated_api_stylesheet buf =
  let selector name = Css.Selector.class_ name in
  let prop i =
    Css.custom_property
      ("--fuzz-" ^ string_of_int (byte_at buf i))
      (pick [ "0"; "1rem"; "red"; "var(--fallback)" ] buf (i + 1))
  in
  Css.v
    [
      Css.layer ~name:"theme"
        [
          Css.rule ~selector:(selector "root") [ prop 0 ];
          Css.media
            ~condition:(Css.Media.of_string "(prefers-color-scheme: dark)")
            [ Css.rule ~selector:(selector "dark") [ prop 2 ] ];
        ];
      Css.layer ~name:"utilities"
        [ Css.rule ~selector:(selector "utility") [ prop 4 ] ];
      Css.rule ~selector:(selector "card")
        ~nested:
          [
            Css.media
              ~condition:(Css.Media.of_string "(width >= 40em)")
              [
                Css.rule ~selector:(selector "wide")
                  [ Css.color (Css.hex "#0000ff") ];
              ];
            Css.declarations [ Css.background_color (Css.hex "#ffffff") ];
          ]
        [ Css.color (Css.hex "#ff0000") ];
      Css.starting_style
        [
          Css.rule ~selector:(selector "entry")
            [
              Css.opacity
                (Css.Opacity_number
                   ((float_of_int (byte_at buf 6) +. 1.) /. 256.));
            ];
        ];
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

let test_public_fold_count buf =
  let sheet = generated_api_stylesheet buf in
  let count =
    Css.fold
      (fun n stmt -> match Css.as_rule stmt with Some _ -> n + 1 | None -> n)
      0 sheet
  in
  if count <> 6 then
    fail (Fmt.str "Css.fold visited %d rules instead of 6" count)

let test_custom_props_scope buf =
  let sheet = generated_api_stylesheet buf in
  let all_props = Css.custom_props sheet in
  let theme_props = Css.custom_props ~layer:"theme" sheet in
  let util_props = Css.custom_props ~layer:"utilities" sheet in
  if List.length all_props < 4 then
    fail
      (Fmt.str "Css.custom_props lost properties: %S"
         (String.concat "," all_props));
  List.iter
    (fun name ->
      if List.mem name util_props then
        fail (Fmt.str "theme property leaked into utilities: %S" name))
    theme_props;
  List.iter
    (fun name ->
      if List.mem name theme_props then
        fail (Fmt.str "utility property leaked into theme: %S" name))
    util_props

let test_public_theme_guard buf =
  let var_name = "fuzz-" ^ string_of_int (byte_at buf 0) in
  let guarded =
    Css.theme_guarded ~var_name
      (Css.color (Css.hex (pick [ "#ff0000"; "#00ff00" ] buf 1)))
  in
  let sheet =
    Css.v
      [
        Css.rule
          ~selector:(Css.Selector.class_ "card")
          [ guarded; Css.background_color (Css.hex "#ffffff") ];
      ]
  in
  let hidden =
    Css.to_string ~minify:true ~theme:Css.Pp.String_set.empty sheet
  in
  let shown =
    let theme = Css.Pp.String_set.add var_name Css.Pp.String_set.empty in
    Css.to_string ~minify:true ~theme sheet
  in
  if
    String.contains hidden '#' && Astring.String.is_infix ~affix:"ff0000" hidden
  then fail (Fmt.str "theme guard emitted without theme: %S" hidden);
  if not (Astring.String.is_infix ~affix:"color:" shown) then
    fail (Fmt.str "theme guard omitted with theme: %S" shown)

let test_public_property_shape buf =
  let name = "--fuzz-" ^ string_of_int (byte_at buf 0) in
  let sheet =
    Css.property ~name Css.Length
      ~initial_value:(Css.Px (float_of_int (byte_at buf 1)))
      ()
  in
  match Css.statements sheet with
  | [ stmt ] -> (
      match Css.as_property stmt with
      | Some (Css.Property_info info) ->
          if info.name <> name then
            fail (Fmt.str "@property name changed: %S -> %S" name info.name)
      | None -> fail "Css.property did not create @property statement")
  | _ -> fail "Css.property did not create one statement"

let test_css2_legacy_minified_vectors buf =
  let input =
    pick
      [
        "body { margin: 0; color: black }";
        "@media print { body { color: black } }";
        "h1:first-letter { color: red }";
        "p::first-line { color: blue }";
        "a:link { color: blue } a:visited { color: purple }";
        "div { page-break-before: always }";
      ]
      buf 0
  in
  match Css.of_string input with
  | Error err ->
      fail (Fmt.str "CSS2 legacy vector rejected: %s" (Css.pp_parse_error err))
  | Ok sheet -> (
      let minified = Css.to_string ~minify:true ~newline:false sheet in
      match Css.of_string minified with
      | Ok _ -> ()
      | Error err ->
          fail
            (Fmt.str "CSS2 legacy minified output rejected: %S (%s)" minified
               (Css.pp_parse_error err)))

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
      test_case "public fold counts nested rules" [ bytes ]
        test_public_fold_count;
      test_case "public custom_props layer scope" [ bytes ]
        test_custom_props_scope;
      test_case "public theme guard rendering" [ bytes ] test_public_theme_guard;
      test_case "public property shape" [ bytes ] test_public_property_shape;
      test_case "CSS2 legacy minified vectors" [ bytes ]
        test_css2_legacy_minified_vectors;
    ] )
