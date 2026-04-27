(** Fuzz tests for the CSS Stylesheet module.

    Tests crash safety of stylesheet, rule, and declaration parsing. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let segment buf i =
  pick
    [
      "reset";
      "base";
      "framework";
      "theme";
      "layout";
      "components";
      "utilities";
      "overrides";
    ]
    buf i

let layer_name buf i =
  match byte_at buf i mod 4 with
  | 0 -> segment buf (i + 1)
  | 1 -> segment buf (i + 1) ^ "." ^ segment buf (i + 2)
  | 2 ->
      segment buf (i + 1) ^ "." ^ segment buf (i + 2) ^ "." ^ segment buf (i + 3)
  | _ -> "framework.theme"

let selector buf i =
  Css.Selector.class_ (pick [ "card"; "title"; "button"; "inside" ] buf i)

let declaration buf i =
  match byte_at buf i mod 4 with
  | 0 -> Css.Declaration.display Css.Properties.Block
  | 1 -> Css.Declaration.display Css.Properties.Flex
  | 2 -> Css.Declaration.color (Css.Values.hex "#ff0000")
  | _ -> Css.Declaration.background_color (Css.Values.hex "#0000ff")

let rule buf i =
  Css.Stylesheet.Rule
    (Css.Stylesheet.rule ~selector:(selector buf i) [ declaration buf (i + 1) ])

let import_rule ?layer url =
  Css.Stylesheet.Import { url; layer; supports = None; media = None }

let generated_layer_stylesheet buf =
  let primary = layer_name buf 0 in
  let secondary = layer_name buf 4 in
  let nested = layer_name buf 8 in
  [
    Css.Stylesheet.Layer_decl [ primary; secondary ];
    import_rule ~layer:secondary "theme.css";
    import_rule ~layer:"" "anonymous-layer.css";
    import_rule "plain.css";
    Css.Stylesheet.Layer
      ( Some primary,
        [
          rule buf 12;
          Css.Stylesheet.Media
            ( Css.Media.Raw "(min-width:30em)",
              [ Css.Stylesheet.Layer (Some nested, [ rule buf 16 ]) ] );
        ] );
    Css.Stylesheet.Layer
      ( None,
        [
          Css.Stylesheet.Layer (Some "foo", [ rule buf 20 ]);
          Css.Stylesheet.Layer (Some "foo", [ rule buf 24 ]);
        ] );
    Css.Stylesheet.Layer
      (None, [ Css.Stylesheet.Layer (Some "foo", [ rule buf 28 ]) ]);
    rule buf 32;
  ]

let parse_stylesheet input =
  let r = Css.Cursor.of_string input in
  try Some (Css.Stylesheet.read_stylesheet r)
  with Css.Cursor.Parse_error _ -> None

let parse_declaration input =
  let r = Css.Cursor.of_string input in
  try
    match Css.Declaration.read_declaration r with
    | None -> None
    | Some decl ->
        Some (Css.Declaration.string_of_declaration ~minify:true decl)
  with Css.Cursor.Parse_error _ -> None

let minified_stylesheet ss =
  Css.Stylesheet.to_string ~minify:true ss |> String.trim

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let css_wide_keyword buf i =
  pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf i

let shorthand_property buf i =
  pick [ "margin"; "padding"; "background"; "border"; "font"; "all" ] buf i

let invalid_shorthand_keyword_mix buf i =
  let keyword = css_wide_keyword buf i in
  pick
    [
      ("margin", keyword ^ " 1px");
      ("padding", "1px " ^ keyword);
      ("background", "green " ^ keyword);
      ("border", "1px solid " ^ keyword);
      ("font", "bold " ^ keyword ^ " 12pt Helvetica");
      ("all", keyword ^ " color");
    ]
    buf (i + 1)

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
  | Origin (origin, block) ->
      let origin =
        match origin with
        | User_agent -> "ua"
        | User -> "user"
        | Author_presentational_hint -> "author-presentational-hint"
        | Author -> "author"
        | Animation -> "animation"
        | Transition -> "transition"
      in
      (("origin:" ^ origin) :: List.concat_map boundary_shape block)
      @ [ "/origin" ]
  | Charset _ -> [ "charset" ]
  | Keyframes _ -> [ "keyframes" ]
  | Font_face _ -> [ "font-face" ]
  | Page _ -> [ "page" ]
  | Property _ -> [ "property" ]

let boundary_shapes ss = List.concat_map boundary_shape ss

let anonymous_layer_count ss =
  let rec statement = function
    | Css.Stylesheet.Layer (None, block) -> 1 + block_count block
    | Layer (Some _, block)
    | Media (_, block)
    | Supports (_, block)
    | Container (_, _, block)
    | Scope (_, _, block)
    | Starting_style block
    | Origin (_, block) ->
        block_count block
    | Rule _ | Declarations _ | Charset _ | Import _ | Namespace _
    | Layer_decl _ | Keyframes _ | Font_face _ | Page _ | Property _ ->
        0
  and block_count block = List.fold_left (fun n s -> n + statement s) 0 block in
  block_count ss

(** read_stylesheet — must not crash on arbitrary input. *)
let test_read_stylesheet buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_stylesheet r)
  with Css.Cursor.Parse_error _ -> ()

(** read_rule — must not crash. *)
let test_read_rule buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_rule r)
  with Css.Cursor.Parse_error _ | Invalid_argument _ -> ()

(** read_block — must not crash. *)
let test_read_block buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_block r) with Css.Cursor.Parse_error _ -> ()

(** read (legacy) — must not crash. *)
let test_read buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Stylesheet.read r) with Css.Cursor.Parse_error _ -> ()

(** read_import_rule — must not crash. *)
let test_read_import_rule buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_import_rule r)
  with Css.Cursor.Parse_error _ -> ()

(** read_config — must not crash. *)
let test_read_config buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_config r)
  with Css.Cursor.Parse_error _ -> ()

(** read_declaration — must not crash. *)
let test_read_declaration buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Declaration.read_declaration r)
  with Css.Cursor.Parse_error _ -> ()

(** read_declarations — must not crash. *)
let test_read_declarations buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Declaration.read_declarations r)
  with Css.Cursor.Parse_error _ -> ()

(** read_property_name — must not crash. *)
let test_read_property_name buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Declaration.read_property_name r)
  with Css.Cursor.Parse_error _ -> ()

(** read_property_value — must not crash. *)
let test_read_property_value buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Declaration.read_property_value r)
  with Css.Cursor.Parse_error _ -> ()

(** Stylesheet roundtrip: parse → to_string → parse should not crash. *)
let test_stylesheet_roundtrip buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Stylesheet.read_stylesheet r)
    with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some ss -> (
      let s = Css.Stylesheet.to_string ss in
      let r2 = Css.Cursor.of_string s in
      try ignore (Css.Stylesheet.read_stylesheet r2)
      with Css.Cursor.Parse_error _ ->
        fail "stylesheet roundtrip re-parse failed")

(** Minified stylesheet serialization should be idempotent after reparsing
    decoded CSS-shaped input. *)
let test_stylesheet_minified_idempotent buf =
  let buf = cssish buf in
  match parse_stylesheet buf with
  | None -> ()
  | Some ss -> (
      let once = minified_stylesheet ss in
      match parse_stylesheet once with
      | None -> fail (Fmt.str "minified stylesheet did not reparse: %S" once)
      | Some reparsed ->
          let twice = minified_stylesheet reparsed in
          if once <> twice then
            fail
              (Fmt.str "stylesheet minified serialization changed: %S -> %S"
                 once twice))

(** CSS Cascade section 6.4: stylesheets emitted with layer statements, nested
    layer names, imports, anonymous layers, and conditional layer blocks should
    parse back to the same canonical serialization. *)
let test_generated_layer_stylesheet_roundtrip buf =
  let ss = generated_layer_stylesheet buf in
  let once = minified_stylesheet ss in
  match parse_stylesheet once with
  | None -> fail (Fmt.str "generated layer stylesheet did not reparse: %S" once)
  | Some reparsed ->
      let twice = minified_stylesheet reparsed in
      if once <> twice then
        fail
          (Fmt.str "generated layer stylesheet serialization changed: %S -> %S"
             once twice)

(** CSS Cascade section 6.4: optimization must preserve cascade boundaries that
    define layer identity, layer order, import placement, and conditional scope.
*)
let test_layer_boundary_shape_invariant buf =
  let ss = generated_layer_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let before = boundary_shapes ss in
  let after = boundary_shapes optimized in
  if before <> after then
    fail
      (Fmt.str "layer boundary shape changed: %S -> %S"
         (String.concat " " before) (String.concat " " after))

(** CSS Cascade section 6.4.2.1: anonymous layer declarations have unique
    identities and must not be collapsed away by optimization. *)
let test_anonymous_layer_count_invariant buf =
  let ss = generated_layer_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let before = anonymous_layer_count ss in
  let after = anonymous_layer_count optimized in
  if before <> after then
    fail (Fmt.str "anonymous layer count changed: %d -> %d" before after)

(** CSS Cascade section 3: CSS-wide keywords used on shorthands are whole
    declaration values and serialize as the shorthand property plus the keyword.
*)
let test_shorthand_css_wide_keyword_invariant buf =
  let property = shorthand_property buf 0 in
  let keyword = css_wide_keyword buf 1 in
  let input = property ^ ":" ^ keyword in
  match parse_declaration input with
  | None -> fail (Fmt.str "CSS-wide shorthand did not parse: %S" input)
  | Some serialized ->
      let expected = property ^ ":" ^ keyword in
      if serialized <> expected then
        fail (Fmt.str "CSS-wide shorthand changed: %S -> %S" input serialized)

(** CSS Cascade section 3: CSS-wide keywords cannot be combined with other
    component values in a single declaration, including in shorthands. *)
let test_shorthand_css_wide_keyword_mix_rejected buf =
  let property, value = invalid_shorthand_keyword_mix buf 0 in
  let input = property ^ ":" ^ value in
  match parse_declaration input with
  | None -> ()
  | Some serialized ->
      fail
        (Fmt.str "invalid shorthand keyword mix parsed: %S -> %S" input
           serialized)

(** CSS Cascade section 3.1: legacy shorthands are parse-time aliases and are
    not chosen when serializing declarations. *)
let test_legacy_shorthand_alias_serialization_invariant buf =
  let input, expected =
    pick
      [
        ("page-break-before:always", "break-before:page");
        ("page-break-after:always", "break-after:page");
        ("page-break-inside:avoid", "break-inside:avoid");
      ]
      buf 0
  in
  match parse_declaration input with
  | None -> fail (Fmt.str "legacy shorthand alias did not parse: %S" input)
  | Some serialized ->
      if starts_with ~prefix:"page-break-" serialized then
        fail
          (Fmt.str "legacy shorthand serialized with old name: %S" serialized);
      if serialized <> expected then
        fail
          (Fmt.str "legacy shorthand alias changed: %S -> %S" input serialized)

(** CSS Cascade section 4.1: declared values preserve declaration source order
    before cascade sorting. *)
let test_declared_values_source_order_invariant buf =
  let first_color =
    if byte_at buf 0 mod 2 = 0 then Css.Values.hex "#ff0000"
    else Css.Values.hex "#00ff00"
  in
  let second_color =
    if byte_at buf 1 mod 2 = 0 then Css.Values.hex "#0000ff"
    else Css.Values.hex "#ffff00"
  in
  let declarations =
    [
      Css.Declaration.color first_color;
      Css.Declaration.margin [ Css.Values.Px 1. ];
      Css.Declaration.important (Css.Declaration.color second_color);
    ]
  in
  let colors = Css.Stylesheet.declared_values ~property:"color" declarations in
  let orders =
    List.map (fun (d : Css.Stylesheet.declared_value) -> d.source_order) colors
  in
  let important =
    List.map (fun (d : Css.Stylesheet.declared_value) -> d.important) colors
  in
  if orders <> [ 0; 2 ] then
    fail
      (Fmt.str "declared color source order changed: %s"
         (String.concat "," (List.map string_of_int orders)));
  if important <> [ false; true ] then
    fail "declared value importance did not follow declarations"

(** CSS Cascade section 4.3: specified-value defaulting for [unset] depends on
    whether the property is inherited. *)
let test_specified_value_unset_inheritance_invariant buf =
  let inherited = Some (pick [ "blue"; "inside"; "4.2px" ] buf 0) in
  let initial = pick [ "black"; "outside"; "medium" ] buf 1 in
  let inherited_property =
    Css.Stylesheet.specified_value ~inherits:true ~initial ~inherited
      ~cascaded:(Some "unset")
  in
  let non_inherited_property =
    Css.Stylesheet.specified_value ~inherits:false ~initial ~inherited
      ~cascaded:(Some "unset")
  in
  if inherited_property.specified_value <> Option.get inherited then
    fail
      (Fmt.str "unset inherited property did not inherit: %S"
         inherited_property.specified_value);
  if non_inherited_property.specified_value <> initial then
    fail
      (Fmt.str "unset non-inherited property did not use initial: %S"
         non_inherited_property.specified_value)

let suite =
  ( "stylesheet",
    [
      test_case "read_stylesheet crash safety" [ bytes ] test_read_stylesheet;
      test_case "read_rule crash safety" [ bytes ] test_read_rule;
      test_case "read_block crash safety" [ bytes ] test_read_block;
      test_case "read crash safety" [ bytes ] test_read;
      test_case "read_import_rule crash safety" [ bytes ] test_read_import_rule;
      test_case "read_config crash safety" [ bytes ] test_read_config;
      test_case "read_declaration crash safety" [ bytes ] test_read_declaration;
      test_case "read_declarations crash safety" [ bytes ]
        test_read_declarations;
      test_case "read_property_name crash safety" [ bytes ]
        test_read_property_name;
      test_case "read_property_value crash safety" [ bytes ]
        test_read_property_value;
      test_case "roundtrip" [ bytes ] test_stylesheet_roundtrip;
      test_case "minified serialization idempotent" [ bytes ]
        test_stylesheet_minified_idempotent;
      test_case "generated layer stylesheet roundtrip" [ bytes ]
        test_generated_layer_stylesheet_roundtrip;
      test_case "layer boundary shape invariant" [ bytes ]
        test_layer_boundary_shape_invariant;
      test_case "anonymous layer count invariant" [ bytes ]
        test_anonymous_layer_count_invariant;
      test_case "shorthand CSS-wide keyword invariant" [ bytes ]
        test_shorthand_css_wide_keyword_invariant;
      test_case "shorthand CSS-wide keyword mix rejected" [ bytes ]
        test_shorthand_css_wide_keyword_mix_rejected;
      test_case "legacy shorthand alias serialization invariant" [ bytes ]
        test_legacy_shorthand_alias_serialization_invariant;
      test_case "declared values source order invariant" [ bytes ]
        test_declared_values_source_order_invariant;
      test_case "specified value unset inheritance invariant" [ bytes ]
        test_specified_value_unset_inheritance_invariant;
    ] )
