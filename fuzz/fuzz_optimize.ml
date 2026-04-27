(** Fuzz tests for stylesheet optimization invariants. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let selector buf i =
  Css.Selector.class_ (pick [ "card"; "title"; "button"; "panel" ] buf i)

let declaration buf i =
  match byte_at buf i mod 5 with
  | 0 -> Css.Declaration.display Css.Properties.Block
  | 1 -> Css.Declaration.display Css.Properties.Flex
  | 2 -> Css.Declaration.color (Css.Values.hex "#ff0000")
  | 3 -> Css.Declaration.background_color (Css.Values.hex "#0000ff")
  | _ -> Css.Declaration.margin [ Css.Values.Px (Float.of_int (byte_at buf i)) ]

let rule buf i =
  Css.Stylesheet.Rule
    (Css.Stylesheet.rule ~selector:(selector buf i) [ declaration buf (i + 1) ])

let generated_stylesheet buf =
  [
    Css.Stylesheet.Layer_decl [ "reset"; "theme"; "components" ];
    Css.Stylesheet.Import
      {
        url = "theme.css";
        layer = Some "theme";
        supports = Some (Css.Supports.Property ("display", "grid"));
        media = Some (Css.Media.of_string "(width >= 40em)");
      };
    Css.Stylesheet.Namespace (Some "svg", "http://www.w3.org/2000/svg");
    Css.Stylesheet.property ~syntax:Css.Variables.Universal "--fuzz";
    rule buf 0;
    rule buf 0;
    Css.Stylesheet.Media
      ( Css.Media.of_string "(width >= 40em)",
        [
          rule buf 4;
          Css.Stylesheet.Supports
            ( Css.Supports.Property ("display", "grid"),
              [
                Css.Stylesheet.Container
                  ( Some "card",
                    Css.Container.Raw "(inline-size > 30em)",
                    [ rule buf 8 ] );
              ] );
        ] );
    Css.Stylesheet.Layer
      ( Some (pick [ "base"; "theme"; "components" ] buf 12),
        [ rule buf 16; Css.Stylesheet.Layer (None, [ rule buf 20 ]) ] );
    Css.Stylesheet.Scope (Some ".card", Some ".limit", [ rule buf 24 ]);
    Css.Stylesheet.Page
      (Some ":first", [ Css.Declaration.margin [ Css.Values.Px 10. ] ]);
    Css.Stylesheet.Keyframes
      ( "fade",
        [
          {
            keyframe_selector =
              Css.Keyframe.Positions [ Css.Keyframe.Percent 0. ];
            keyframe_declarations =
              [ Css.Declaration.opacity (Css.Properties.Opacity_number 0.) ];
          };
          {
            keyframe_selector =
              Css.Keyframe.Positions [ Css.Keyframe.Percent 100. ];
            keyframe_declarations =
              [ Css.Declaration.opacity (Css.Properties.Opacity_number 1.) ];
          };
        ] );
  ]

let minified ss = Css.Stylesheet.to_string ~minify:true ss |> String.trim

let parse_stylesheet input =
  let r = Css.Cursor.of_string input in
  try Some (Css.Stylesheet.read_stylesheet r)
  with Css.Cursor.Parse_error _ -> None

let rec boundary_shape = function
  | Css.Stylesheet.Rule _ -> [ "rule" ]
  | Declarations _ -> [ "declarations" ]
  | Import { layer; _ } -> [ "import:" ^ Option.value ~default:"<none>" layer ]
  | Namespace _ -> [ "namespace" ]
  | Layer_decl names -> [ "layer-decl:" ^ String.concat "," names ]
  | Layer (name, block) ->
      let name = Option.value ~default:"<anonymous>" name in
      (("layer:" ^ name) :: List.concat_map boundary_shape block) @ [ "/layer" ]
  | Media (_, block) ->
      ("media" :: List.concat_map boundary_shape block) @ [ "/media" ]
  | Supports (_, block) ->
      ("supports" :: List.concat_map boundary_shape block) @ [ "/supports" ]
  | Container (_, _, block) ->
      ("container" :: List.concat_map boundary_shape block) @ [ "/container" ]
  | Scope (_, _, block) ->
      ("scope" :: List.concat_map boundary_shape block) @ [ "/scope" ]
  | Starting_style block ->
      ("starting-style" :: List.concat_map boundary_shape block)
      @ [ "/starting-style" ]
  | Origin (_, block) ->
      ("origin" :: List.concat_map boundary_shape block) @ [ "/origin" ]
  | Charset _ -> [ "charset" ]
  | Keyframes _ -> [ "keyframes" ]
  | Font_face _ -> [ "font-face" ]
  | Page _ -> [ "page" ]
  | Property _ -> [ "property" ]

let boundary_shapes ss = List.concat_map boundary_shape ss

let test_optimize_idempotent buf =
  let ss = generated_stylesheet buf in
  let once = Css.Optimize.stylesheet ss in
  let twice = Css.Optimize.stylesheet once in
  let once_s = minified once in
  let twice_s = minified twice in
  if once_s <> twice_s then
    fail
      (Fmt.str "optimization serialization changed on second pass: %S -> %S"
         once_s twice_s)

let test_optimized_stylesheet_reparses buf =
  let optimized = Css.Optimize.stylesheet (generated_stylesheet buf) in
  let serialized = minified optimized in
  match parse_stylesheet serialized with
  | Some _ -> ()
  | None -> fail (Fmt.str "optimized stylesheet did not reparse: %S" serialized)

let test_optimization_preserves_boundary_shape buf =
  let ss = generated_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let before = boundary_shapes ss in
  let after = boundary_shapes optimized in
  if before <> after then
    fail
      (Fmt.str
         "optimization changed conditional/cascade boundary shape: %S -> %S"
         (String.concat " " before) (String.concat " " after))

let test_optimized_reparse_idempotent buf =
  let optimized = Css.Optimize.stylesheet (generated_stylesheet buf) in
  let serialized = minified optimized in
  match parse_stylesheet serialized with
  | None -> fail "optimized stylesheet did not reparse"
  | Some parsed ->
      let reparsed_optimized = Css.Optimize.stylesheet parsed in
      let serialized2 = minified reparsed_optimized in
      if serialized <> serialized2 then
        fail
          (Fmt.str "optimized reparse serialization changed: %S -> %S"
             serialized serialized2)

let count_imports ss =
  List.fold_left
    (fun acc -> function Css.Stylesheet.Import _ -> acc + 1 | _ -> acc)
    0 ss

let count_namespaces ss =
  List.fold_left
    (fun acc -> function Css.Stylesheet.Namespace _ -> acc + 1 | _ -> acc)
    0 ss

let test_import_namespace_counts buf =
  let ss = generated_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  if count_imports ss <> count_imports optimized then
    fail "optimization changed top-level import count";
  if count_namespaces ss <> count_namespaces optimized then
    fail "optimization changed top-level namespace count"

let count_kind f ss =
  List.fold_left (fun acc stmt -> if f stmt then acc + 1 else acc) 0 ss

let test_atrule_counts_stable buf =
  let ss = generated_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let same label pred =
    let before = count_kind pred ss in
    let after = count_kind pred optimized in
    if before <> after then
      fail
        (Fmt.str "optimization changed %s count: %d -> %d" label before after)
  in
  same "property" (function Css.Stylesheet.Property _ -> true | _ -> false);
  same "page" (function Css.Stylesheet.Page _ -> true | _ -> false);
  same "keyframes" (function Css.Stylesheet.Keyframes _ -> true | _ -> false)

let suite =
  ( "optimize",
    [
      test_case "optimize idempotent" [ bytes ] test_optimize_idempotent;
      test_case "optimized stylesheet reparses" [ bytes ]
        test_optimized_stylesheet_reparses;
      test_case "optimization preserves boundary shape" [ bytes ]
        test_optimization_preserves_boundary_shape;
      test_case "optimized reparse idempotent" [ bytes ]
        test_optimized_reparse_idempotent;
      test_case "import namespace counts" [ bytes ] test_import_namespace_counts;
      test_case "at-rule counts stable" [ bytes ] test_atrule_counts_stable;
    ] )
