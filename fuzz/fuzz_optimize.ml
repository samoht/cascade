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
    Css.Stylesheet.Charset "UTF-8";
    Css.Stylesheet.Layer_decl [ "reset"; "theme"; "components" ];
    Css.Stylesheet.Import
      {
        url = "theme.css";
        layer = Some "theme";
        supports = Some (Css.Supports.property "display" "grid");
        media = Some (Css.Media.of_string "(width >= 40em)");
      };
    Css.Stylesheet.Namespace
      ( Some "svg",
        Css.Stylesheet.Url
          ("http://www.w3.org/2000/svg", Css.Stylesheet.Url_bare) );
    Css.Stylesheet.property ~syntax:Css.Variables.Universal "--fuzz";
    rule buf 0;
    rule buf 0;
    Css.Stylesheet.Media
      ( Css.Media.of_string "(width >= 40em)",
        [
          rule buf 4;
          Css.Stylesheet.Supports
            ( Css.Supports.property "display" "grid",
              [
                Css.Stylesheet.Container
                  ( Some "card",
                    Css.Container.of_string "(inline-size > 30em)",
                    [ rule buf 8 ] );
              ] );
        ] );
    Css.Stylesheet.When
      ( Css.Stylesheet.And
          ( Css.Stylesheet.Media_condition
              (Css.Media.of_string "(width >= 48em)"),
            Css.Stylesheet.Supports_condition_test
              (Css.Supports.property "display" "grid") ),
        [ rule buf 10 ] );
    Css.Stylesheet.Else
      ( Some
          (Css.Stylesheet.Supports_condition_test
             (Css.Supports.property "display" "flex")),
        [ rule buf 11 ] );
    Css.Stylesheet.Supports_condition
      ("--fuzz-condition", [ declaration buf 12 ]);
    Css.Stylesheet.Layer
      ( Some (pick [ "base"; "theme"; "components" ] buf 12),
        [ rule buf 16; Css.Stylesheet.Layer (None, [ rule buf 20 ]) ] );
    Css.Stylesheet.Scope (Some ".card", Some ".limit", [ rule buf 24 ]);
    Css.Stylesheet.Starting_style [ rule buf 28 ];
    Css.Stylesheet.with_origin Css.Stylesheet.Author
      [ rule buf 32; rule buf 32 ];
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
      (("layer:" ^ name) :: shapes_with_rule_runs block) @ [ "/layer" ]
  | Media (_, block) -> ("media" :: shapes_with_rule_runs block) @ [ "/media" ]
  | Supports (_, block) ->
      ("supports" :: shapes_with_rule_runs block) @ [ "/supports" ]
  | Container (_, _, block) ->
      ("container" :: shapes_with_rule_runs block) @ [ "/container" ]
  | When (_, block) -> ("when" :: shapes_with_rule_runs block) @ [ "/when" ]
  | Else (_, block) -> ("else" :: shapes_with_rule_runs block) @ [ "/else" ]
  | Supports_condition (name, _) -> [ "supports-condition:" ^ name ]
  | Scope (_, _, block) ->
      ("scope" :: shapes_with_rule_runs block) @ [ "/scope" ]
  | Starting_style block ->
      ("starting-style" :: shapes_with_rule_runs block) @ [ "/starting-style" ]
  | Origin (_, block) ->
      ("origin" :: shapes_with_rule_runs block) @ [ "/origin" ]
  | Charset _ -> [ "charset" ]
  | Keyframes _ | Webkit_keyframes _ -> [ "keyframes" ]
  | Font_face _ -> [ "font-face" ]
  | Page _ -> [ "page" ]
  | Page_with_margins _ -> [ "page" ]
  | Font_palette_values _ -> [ "font-palette-values" ]
  | View_transition _ -> [ "view-transition" ]
  | Position_try _ -> [ "position-try" ]
  | Property _ -> [ "property" ]

(* The optimizer is allowed to merge a contiguous run of [Rule]s into fewer
   rules (e.g. [combine_identical_rules]). Collapse consecutive [Rule] entries
   into a single [rules] token so the boundary check tracks the at-rule skeleton
   without forcing the optimizer to leave every individual rule intact. *)
and shapes_with_rule_runs ss =
  let rec loop acc seen_rule = function
    | [] -> if seen_rule then List.rev ("rules" :: acc) else List.rev acc
    | Css.Stylesheet.Rule _ :: rest -> loop acc true rest
    | other :: rest ->
        let acc = if seen_rule then "rules" :: acc else acc in
        loop (List.rev_append (boundary_shape other) acc) false rest
  in
  loop [] false ss

let boundary_shapes ss = shapes_with_rule_runs ss

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
  same "charset" (function Css.Stylesheet.Charset _ -> true | _ -> false);
  same "starting-style" (function
    | Css.Stylesheet.Starting_style _ -> true
    | _ -> false);
  same "origin" (function Css.Stylesheet.Origin _ -> true | _ -> false);
  same "page" (function Css.Stylesheet.Page _ -> true | _ -> false);
  same "keyframes" (function Css.Stylesheet.Keyframes _ -> true | _ -> false);
  same "when" (function Css.Stylesheet.When _ -> true | _ -> false);
  same "else" (function Css.Stylesheet.Else _ -> true | _ -> false);
  same "supports-condition" (function
    | Css.Stylesheet.Supports_condition _ -> true
    | _ -> false)

let test_cascade_positive_negative_merge_vectors buf =
  let media_rule selector color =
    Css.Stylesheet.Media
      ( Css.Media.Min_width 48.,
        [
          Css.Stylesheet.Rule
            (Css.Stylesheet.rule
               ~selector:(Css.Selector.class_ selector)
               [ Css.Declaration.color (Css.Values.hex color) ]);
        ] )
  in
  let input =
    pick
      [
        [
          Css.Stylesheet.Rule
            (Css.Stylesheet.rule
               ~selector:(Css.Selector.class_ "box")
               [ Css.Declaration.color (Css.Values.hex "#ff0000") ]);
          Css.Stylesheet.Rule
            (Css.Stylesheet.rule
               ~selector:(Css.Selector.class_ "box")
               [
                 Css.Declaration.display Css.Properties.Flex;
                 Css.Declaration.color (Css.Values.hex "#0000ff");
               ]);
        ];
        [
          Css.Stylesheet.Rule
            (Css.Stylesheet.rule
               ~selector:(Css.Selector.class_ "box")
               [ Css.Declaration.color (Css.Values.hex "#ff0000") ]);
          Css.Stylesheet.Layer_decl [ "reset"; "components" ];
          Css.Stylesheet.Rule
            (Css.Stylesheet.rule
               ~selector:(Css.Selector.class_ "box")
               [ Css.Declaration.display Css.Properties.Flex ]);
        ];
        [
          media_rule "a" "#ff0000";
          Css.Stylesheet.Layer_decl [ "theme" ];
          media_rule "b" "#0000ff";
        ];
      ]
      buf 0
  in
  let optimized = Css.Optimize.stylesheet input in
  let before = boundary_shapes input in
  let after = boundary_shapes optimized in
  if before <> after then
    fail
      (Fmt.str "merge vector changed cascade boundary shape: %S -> %S"
         (String.concat " " before) (String.concat " " after));
  ignore (minified optimized)

let test_cascade_shorthand_importance_vectors buf =
  let declarations =
    pick
      [
        [
          Css.Declaration.important
            (Css.Declaration.margin_left (Css.Values.Px 1.));
          Css.Declaration.margin [ Css.Values.Px 2. ];
        ];
        [
          Css.Declaration.margin [ Css.Values.Px 2. ];
          Css.Declaration.important
            (Css.Declaration.margin_left (Css.Values.Px 1.));
        ];
      ]
      buf 0
  in
  let rule =
    Css.Stylesheet.rule ~selector:(Css.Selector.class_ "box") declarations
  in
  let optimized =
    Css.Optimize.stylesheet [ Css.Stylesheet.Rule rule ] |> minified
  in
  if
    not
      (String.contains optimized '!'
      && Astring.String.is_infix ~affix:"margin:" optimized
      && Astring.String.is_infix ~affix:"margin-left:" optimized)
  then
    fail
      (Fmt.str
         "optimization dropped a required mixed-importance shorthand/longhand: \
          %S"
         optimized)

let test_positive_layer_statement_vectors buf =
  let input =
    [
      Css.Stylesheet.Layer (Some (pick [ "reset"; "base" ] buf 0), []);
      Css.Stylesheet.Layer (Some (pick [ "theme"; "components" ] buf 1), []);
      Css.Stylesheet.Rule
        (Css.Stylesheet.rule
           ~selector:(Css.Selector.class_ "card")
           [ Css.Declaration.display Css.Properties.Flex ]);
    ]
  in
  let optimized = Css.Optimize.stylesheet input |> minified in
  if not (Astring.String.is_prefix ~affix:"@layer " optimized) then
    fail
      (Fmt.str "empty named layers did not canonicalize to layer statement: %S"
         optimized);
  match parse_stylesheet optimized with
  | Some _ -> ()
  | None ->
      fail
        (Fmt.str "positive layer statement optimization did not reparse: %S"
           optimized)

let test_name_defining_atrules_preserved buf =
  let css =
    pick
      [
        "@font-face{font-family:Brand;src:url(brand.woff2)}.x{color:red}";
        "@keyframes fade{from{opacity:0}to{opacity:1}}.x{color:red}";
        "@property \
         --gap{syntax:\"<length>\";inherits:false;initial-value:1rem}.x{padding:var(--gap)}";
        "@view-transition{navigation:auto}.x{view-transition-name:card}";
      ]
      buf 0
  in
  match parse_stylesheet css with
  | None -> fail (Fmt.str "name-defining at-rule vector did not parse: %S" css)
  | Some ss -> (
      let optimized = Css.Optimize.stylesheet ss |> minified in
      let required =
        pick
          [ "@font-face"; "@keyframes"; "@property --gap"; "@view-transition" ]
          buf 0
      in
      if not (Astring.String.is_infix ~affix:required optimized) then
        fail
          (Fmt.str "optimization dropped name-defining at-rule %S from %S -> %S"
             required css optimized);
      match parse_stylesheet optimized with
      | Some _ -> ()
      | None ->
          fail
            (Fmt.str "optimized name-defining at-rule did not reparse: %S"
               optimized))

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
      test_case "cascade positive negative merge vectors" [ bytes ]
        test_cascade_positive_negative_merge_vectors;
      test_case "cascade shorthand importance vectors" [ bytes ]
        test_cascade_shorthand_importance_vectors;
      test_case "positive layer statement vectors" [ bytes ]
        test_positive_layer_statement_vectors;
      test_case "name-defining at-rules preserved" [ bytes ]
        test_name_defining_atrules_preserved;
    ] )
