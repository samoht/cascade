(** Tests for explicit CSS transform contexts. *)

open Cascade

let decl_t : Css.Declaration.declaration Alcotest.testable =
  Alcotest.testable
    (fun fmt d -> Format.pp_print_string fmt (Css.Declaration.to_string d))
    ( = )

let stylesheet_t : Css.Stylesheet.t Alcotest.testable =
  Alcotest.testable
    (fun fmt sheet ->
      Format.pp_print_string fmt (Css.Stylesheet.to_string ~minify:true sheet))
    ( = )

let matches pattern text =
  let re = Re.Perl.compile_pat pattern in
  Re.execp re text

let check_matches name pattern text =
  Alcotest.(check bool) name true (matches pattern text)

let px value = Css.Media.Length (Css.Values.Px value)
let rem value = Css.Media.Length (Css.Values.Rem value)
let ident value = Css.Media.Ident (Css.Media.ident_of_string value)
let feature = Css.Media.feature
let container_feature = Css.Container.feature

let test_empty_property_value () =
  let ctx = Css.Context.empty in
  Alcotest.(check (list decl_t)) "custom properties" [] ctx.custom_properties;
  Alcotest.(check (list decl_t)) "inherited values" [] ctx.inherited_values;
  Alcotest.(check (list decl_t)) "initial values" [] ctx.initial_values;
  Alcotest.(check (option string)) "base url" None ctx.base_url;
  Alcotest.(check bool) "root font size" true (ctx.root_font_size = None);
  Alcotest.(check bool) "parent font size" true (ctx.parent_font_size = None);
  Alcotest.(check bool) "current color" true (ctx.current_color = None);
  Alcotest.(check bool) "viewport width" true (ctx.viewport_width = None);
  Alcotest.(check bool) "viewport height" true (ctx.viewport_height = None);
  Alcotest.(check bool) "container width" true (ctx.container_width = None);
  Alcotest.(check bool) "container height" true (ctx.container_height = None)

let test_empty_side_contexts () =
  let document = Css.Context.empty_document in
  Alcotest.(check (option string)) "document root" None document.root;
  Alcotest.(check bool) "document scope" true (Option.is_none document.scope);
  Alcotest.(check (option string)) "document element" None document.element;
  Alcotest.(check (list string)) "document classes" [] document.classes;
  Alcotest.(check (list string)) "document ids" [] document.ids;
  Alcotest.(check bool)
    "empty document has no class" false
    (Css.Context.has_class "card" document);
  Alcotest.(check bool)
    "empty document has no id" false
    (Css.Context.has_id "main" document);
  Alcotest.(check (option (option string)))
    "empty document has no attribute" None
    (Css.Context.attribute "hidden" document);
  let query = Css.Context.empty_query in
  Alcotest.(check (option string)) "query media type" None query.media_type;
  Alcotest.(check int)
    "query media features" 0
    (List.length query.media_features);
  Alcotest.(check int) "query supports" 0 (List.length query.supports);
  Alcotest.(check (option string))
    "query container name" None query.container_name;
  Alcotest.(check int)
    "query container features" 0
    (List.length query.container_features);
  Alcotest.(check bool)
    "empty query supports no declaration" false
    (Css.Context.matches_supports query
       (Css.Supports.property "display" "grid"));
  let loader = Css.Context.empty_loader in
  Alcotest.(check (option string)) "loader base url" None loader.base_url;
  Alcotest.(check (list (pair string string)))
    "loader imports" [] loader.imports;
  Alcotest.(check (option string))
    "empty loader has no import" None
    (Css.Context.import_source "theme.css" loader);
  let animation = Css.Context.empty_animation in
  Alcotest.(check (option string))
    "animation timeline" None animation.timeline_time;
  Alcotest.(check bool) "animation progress" true (animation.progress = None);
  Alcotest.(check (list string))
    "animation properties" [] animation.animated_properties;
  Alcotest.(check bool)
    "empty animation animates no property" false
    (Css.Context.animates_property "opacity" animation)

let test_property_value_lookup () =
  let open Css.Values in
  let gap = Css.Declaration.of_string "--gap: 1rem" in
  let brand = Css.Declaration.of_string "--brand: red" in
  let inherited_color = Css.Declaration.of_string "color: blue" in
  let inherited_font_size = Css.Declaration.of_string "font-size: 16px" in
  let initial_display = Css.Declaration.of_string "display: inline" in
  let initial_width = Css.Declaration.of_string "width: auto" in
  let ctx =
    Css.Context.v ~custom_properties:[ gap; brand ]
      ~inherited_values:[ inherited_color; inherited_font_size ]
      ~initial_values:[ initial_display; initial_width ]
      ~base_url:"https://example.test/css/app.css" ~root_font_size:(Px 16.)
      ~parent_font_size:(Px 14.) ~current_color:(Named Blue)
      ~viewport_width:(Px 1024.) ~viewport_height:(Px 768.)
      ~container_width:(Px 640.) ~container_height:(Px 480.) ()
  in
  Alcotest.(check (option decl_t))
    "custom property" (Some gap)
    (Css.Context.custom_property "--gap" ctx);
  Alcotest.(check (option decl_t))
    "missing custom property" None
    (Css.Context.custom_property "--missing" ctx);
  Alcotest.(check (option decl_t))
    "inherited value" (Some inherited_color)
    (Css.Context.inherited_value "color" ctx);
  Alcotest.(check (option decl_t))
    "initial value" (Some initial_width)
    (Css.Context.initial_value "width" ctx)

let test_property_value_lookup_boundaries () =
  let gap = Css.Declaration.of_string "--gap: 1rem" in
  let inherited = Css.Declaration.of_string "color: blue" in
  let initial = Css.Declaration.of_string "display: inline" in
  let ctx =
    Css.Context.v ~custom_properties:[ gap ] ~inherited_values:[ inherited ]
      ~initial_values:[ initial ] ()
  in
  Alcotest.(check (option decl_t))
    "custom lookup is exact" None
    (Css.Context.custom_property "gap" ctx);
  Alcotest.(check (option decl_t))
    "custom lookup is case-sensitive" None
    (Css.Context.custom_property "--GAP" ctx);
  Alcotest.(check (option decl_t))
    "inherited lookup is exact" None
    (Css.Context.inherited_value "background-color" ctx);
  Alcotest.(check (option decl_t))
    "initial lookup is exact" None
    (Css.Context.initial_value "Display" ctx)

let test_document_context () =
  let ctx =
    Css.Context.document ~root:"html"
      ~scope:(Css.Selector.of_string ".card")
      ~element:"button" ~classes:[ "btn"; "primary" ] ~ids:[ "submit" ]
      ~attributes:[ ("type", Some "submit"); ("disabled", None) ]
      ~pseudo_classes:[ "focus-visible" ] ~pseudo_elements:[ "before" ] ()
  in
  Alcotest.(check bool) "class present" true (Css.Context.has_class "btn" ctx);
  Alcotest.(check bool)
    "class absent" false
    (Css.Context.has_class "missing" ctx);
  Alcotest.(check bool) "id present" true (Css.Context.has_id "submit" ctx);
  Alcotest.(check (option (option string)))
    "attribute with value" (Some (Some "submit"))
    (Css.Context.attribute "type" ctx);
  Alcotest.(check (option (option string)))
    "attribute without value" (Some None)
    (Css.Context.attribute "disabled" ctx)

let test_document_context_boundaries () =
  let ctx =
    Css.Context.document ~root:":root"
      ~scope:(Css.Selector.of_string ".dialog")
      ~element:"input" ~classes:[ "field"; "is-invalid" ] ~ids:[ "email" ]
      ~attributes:
        [
          ("type", Some "email");
          ("required", None);
          ("data-state", Some "invalid");
        ]
      ~pseudo_classes:[ "focus"; "user-invalid" ]
      ~pseudo_elements:[ "placeholder" ] ()
  in
  Alcotest.(check (option string)) "root preserved" (Some ":root") ctx.root;
  Alcotest.(check (option string))
    "scope preserved" (Some ".dialog")
    (Option.map (Css.Selector.to_string ~minify:true) ctx.scope);
  Alcotest.(check (option string))
    "element preserved" (Some "input") ctx.element;
  Alcotest.(check (list string))
    "pseudo-classes preserved"
    [ "focus"; "user-invalid" ]
    ctx.pseudo_classes;
  Alcotest.(check (list string))
    "pseudo-elements preserved" [ "placeholder" ] ctx.pseudo_elements;
  Alcotest.(check bool)
    "class lookup is exact" false
    (Css.Context.has_class "FIELD" ctx);
  Alcotest.(check bool)
    "id lookup is exact" false
    (Css.Context.has_id "Email" ctx);
  Alcotest.(check (option (option string)))
    "attribute lookup is exact" None
    (Css.Context.attribute "TYPE" ctx);
  Alcotest.(check (option (option string)))
    "missing attribute" None
    (Css.Context.attribute "aria-label" ctx)

let test_query_context () =
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:
        [ feature "width" (px 1024.); feature "dynamic-range" (ident "high") ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "container-type" "inline-size";
          Css.Supports.func "selector" ":has(img)";
        ]
      ~container_name:"card"
      ~container_features:
        [
          container_feature "inline-size" (px 640.);
          Css.Container.style ~value:"dark" "--theme";
        ]
      ()
  in
  Alcotest.(check int) "media feature count" 2 (List.length ctx.media_features);
  Alcotest.(check bool)
    "supported declaration" true
    (Css.Context.matches_supports ctx (Css.Supports.property "display" "grid"));
  Alcotest.(check bool)
    "unsupported declaration" false
    (Css.Context.matches_supports ctx (Css.Supports.property "display" "ruby"));
  Alcotest.(check int)
    "container feature count" 2
    (List.length ctx.container_features)

let test_query_context_boundaries () =
  let ctx =
    Css.Context.query ~media_type:"print"
      ~media_features:
        [
          feature "width" (rem 80.);
          feature "prefers-color-scheme" (ident "dark");
        ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "selector" ":has(img)";
          Css.Supports.func "selector" ":has(img)";
          Css.Supports.func "font-tech" "color-COLRv1";
        ]
      ~container_name:"sidebar"
      ~container_features:
        [
          container_feature "inline-size" (rem 42.);
          Css.Container.style ~value:"dark" "--theme";
        ]
      ()
  in
  Alcotest.(check (option string))
    "media type preserved" (Some "print") ctx.media_type;
  Alcotest.(check int) "supports preserved" 4 (List.length ctx.supports);
  Alcotest.(check (option string))
    "container name preserved" (Some "sidebar") ctx.container_name;
  Alcotest.(check bool)
    "supports declaration is exact on value" false
    (Css.Context.matches_supports ctx (Css.Supports.property "display" "flex"));
  (* CSS property names are ASCII case-insensitive (CSS Syntax sec. 8.1), so
     "Display" and "display" name the same property. *)
  Alcotest.(check bool)
    "supports declaration normalises property case" true
    (Css.Context.matches_supports ctx (Css.Supports.property "Display" "grid"))

let test_loader_and_animation_contexts () =
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:[ ("theme.css", ".card{color:red}") ]
      ()
  in
  Alcotest.(check (option string))
    "import source" (Some ".card{color:red}")
    (Css.Context.import_source "theme.css" loader);
  let animation =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.5
      ~animated_properties:[ "opacity"; "transform" ] ()
  in
  Alcotest.(check bool)
    "animates opacity" true
    (Css.Context.animates_property "opacity" animation);
  Alcotest.(check bool)
    "does not animate color" false
    (Css.Context.animates_property "color" animation)

let test_loader_and_animation_boundaries () =
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:
        [
          ("theme.css", "@layer theme{.card{color:red}}");
          ("../base.css", "html{box-sizing:border-box}");
        ]
      ()
  in
  Alcotest.(check (option string))
    "loader base preserved" (Some "https://example.test/css/app.css")
    loader.base_url;
  Alcotest.(check (option string))
    "relative import source" (Some "html{box-sizing:border-box}")
    (Css.Context.import_source "../base.css" loader);
  Alcotest.(check (option string))
    "missing import source" None
    (Css.Context.import_source "missing.css" loader);
  let animation =
    Css.Context.animation ~timeline_time:"1.25s" ~progress:0.75
      ~animated_properties:[ "opacity"; "transform"; "--offset" ]
      ()
  in
  Alcotest.(check (option string))
    "timeline preserved" (Some "1.25s") animation.timeline_time;
  Alcotest.(check (option (float 0.0001)))
    "progress preserved" (Some 0.75) animation.progress;
  Alcotest.(check bool)
    "custom property animation" true
    (Css.Context.animates_property "--offset" animation);
  Alcotest.(check bool)
    "animation lookup is exact" false
    (Css.Context.animates_property "Opacity" animation)

let test_context_debug_printers () =
  let value_ctx =
    Css.Context.v
      ~custom_properties:[ Css.Declaration.of_string "--gap: 1rem" ]
      ~inherited_values:[ Css.Declaration.of_string "color: red" ]
      ~initial_values:[ Css.Declaration.of_string "display: inline" ]
      ~base_url:"https://example.test/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 14.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1024.)
      ~viewport_height:(Css.Values.Px 768.)
      ~container_width:(Css.Values.Px 640.)
      ~container_height:(Css.Values.Px 480.) ()
  in
  let value_dump = Css.Pp.to_string Css.Context.pp value_ctx in
  check_matches "value dump has custom properties" "custom_properties=.*--gap"
    value_dump;
  check_matches "value dump has base url" "base_url=Some https://example"
    value_dump;
  check_matches "value dump has dimensions" "container_height=Some 480px"
    value_dump;
  let document_dump =
    Css.Context.document ~root:"html"
      ~scope:(Css.Selector.of_string ".card")
      ~element:"button" ~classes:[ "btn" ] ~ids:[ "submit" ]
      ~attributes:[ ("disabled", None); ("type", Some "submit") ]
      ~pseudo_classes:[ "focus-visible" ] ~pseudo_elements:[ "before" ] ()
    |> Css.Pp.to_string Css.Context.pp_document
  in
  check_matches "document dump has scope" "scope=Some \\.card" document_dump;
  check_matches "document dump has valueless attr" "attributes=.*disabled"
    document_dump;
  check_matches "document dump has valued attr" "attributes=.*type=submit"
    document_dump;
  let query_dump =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ feature "width" (px 1024.) ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.func "selector" ":has(img)";
        ]
      ~container_name:"card"
      ~container_features:[ container_feature "inline-size" (px 640.) ]
      ()
    |> Css.Pp.to_string Css.Context.pp_query
  in
  check_matches "query dump has supports function" "supports=.*:has" query_dump;
  check_matches "query dump has container name" "container_name=Some card"
    query_dump;
  let loader_dump =
    Css.Context.loader ~base_url:"https://example.test/"
      ~imports:[ ("theme.css", ".card{color:red}") ]
      ()
    |> Css.Pp.to_string Css.Context.pp_loader
  in
  check_matches "loader dump has base" "base_url=Some https://example"
    loader_dump;
  check_matches "loader dump has import" "imports=.*theme\\.css" loader_dump;
  let animation_dump =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.5
      ~animated_properties:[ "opacity"; "transform" ] ()
    |> Css.Pp.to_string Css.Context.pp_animation
  in
  check_matches "animation dump has progress" "progress=Some 0\\.5"
    animation_dump;
  check_matches "animation dump has property" "animated_properties=.*transform"
    animation_dump

let declaration_with_value decl value =
  Css.Declaration.of_string
    (Css.Declaration.property_name decl
    ^ ": " ^ value
    ^ if Css.Declaration.is_important decl then " !important" else "")

let check_eval_value name ~ctx ~expected input =
  let decl = Css.Declaration.of_string input in
  Alcotest.(check decl_t)
    name
    (declaration_with_value decl expected)
    (Css.eval_declaration ctx decl)

let check_eval_preserves name ~ctx input =
  let decl = Css.Declaration.of_string input in
  Alcotest.(check decl_t) name decl (Css.eval_declaration ctx decl)

let check_eval name ~ctx ?layer_order ?layer ~expected input =
  let decl = Css.Declaration.of_string input in
  let expected = Css.Declaration.of_string expected in
  let actual = Css.eval_declaration ctx ?layer_order ?layer decl in
  Alcotest.(check decl_t) name expected actual

let check_eval_idempotent name ~ctx ?layer_order ?layer input =
  let decl = Css.Declaration.of_string input in
  let once = Css.eval_declaration ctx ?layer_order ?layer decl in
  let twice = Css.eval_declaration ctx ?layer_order ?layer once in
  Alcotest.(check decl_t) name once twice

let check_eval_context_extension name ?layer_order ?layer ~weak_ctx ~strong_ctx
    ~expected input =
  let decl = Css.Declaration.of_string input in
  let expected = Css.Declaration.of_string expected in
  let direct = Css.eval_declaration strong_ctx ?layer_order ?layer decl in
  let staged =
    Css.eval_declaration strong_ctx ?layer_order ?layer
      (Css.eval_declaration weak_ctx ?layer_order ?layer decl)
  in
  Alcotest.(check decl_t) (name ^ " direct") expected direct;
  Alcotest.(check decl_t) (name ^ " staged") direct staged

let check_layered_eval_value name ~ctx ~layer_order ?layer ~expected input =
  let decl = Css.Declaration.of_string input in
  Alcotest.(check decl_t)
    name
    (declaration_with_value decl expected)
    (Css.eval_declaration ctx ~layer_order ?layer decl)

let check_layered_eval_preserves name ~ctx ~layer_order ?layer input =
  let decl = Css.Declaration.of_string input in
  Alcotest.(check decl_t)
    name decl
    (Css.eval_declaration ctx ~layer_order ?layer decl)

let stylesheet_of_string input =
  let cursor = Cursor.of_string input in
  try Css.Stylesheet.read cursor
  with Cursor.Parse_error err ->
    Alcotest.failf "expected stylesheet to parse: %s" (Error.to_string err)

let check_eval_stylesheet name ~ctx ?layer_order ?layer ~expected input =
  let expected = stylesheet_of_string expected in
  let actual =
    Css.eval_stylesheet ctx ?layer_order ?layer (stylesheet_of_string input)
  in
  Alcotest.(check stylesheet_t) name expected actual

let declaration_shape decl =
  Css.Declaration.property_name decl
  ^ if Css.Declaration.is_important decl then " !important" else ""

let raw_descriptor_shape (descriptor : Css.Stylesheet.page_descriptor) =
  Css.Declaration.property_name descriptor

let page_selectors_string selectors =
  let pseudo = function
    | Css.Stylesheet.First -> ":first"
    | Left -> ":left"
    | Right -> ":right"
    | Blank -> ":blank"
  in
  let one { Css.Stylesheet.name; pseudos } =
    Option.value ~default:"" name ^ String.concat "" (List.map pseudo pseudos)
  in
  String.concat "," (List.map one selectors)

let rec conditional_shape = function
  | Css.Stylesheet.Media_condition condition ->
      "media(" ^ Css.Pp.to_string ~minify:true Css.Media.pp condition ^ ")"
  | Supports_condition_test condition ->
      "supports("
      ^ Css.Pp.to_string ~minify:true Css.Supports.pp condition
      ^ ")"
  | And (left, right) ->
      "(" ^ conditional_shape left ^ " and " ^ conditional_shape right ^ ")"
  | Or (left, right) ->
      "(" ^ conditional_shape left ^ " or " ^ conditional_shape right ^ ")"

let rec statement_shape stmt =
  let sheet_source stmt = Css.Stylesheet.to_string ~minify:true [ stmt ] in
  let prefixed prefix lines = List.map (fun line -> prefix ^ line) lines in
  let declaration_lines declarations =
    List.map (fun decl -> "decl:" ^ declaration_shape decl) declarations
  in
  let block_lines block =
    block |> List.map statement_shape |> List.concat |> prefixed "  "
  in
  match stmt with
  | Css.Stylesheet.Rule rule ->
      ("rule:" ^ Css.Selector.to_string ~minify:true rule.selector)
      :: declaration_lines rule.declarations
      @ block_lines rule.nested
  | Declarations declarations -> "decls" :: declaration_lines declarations
  | Charset _ -> [ "charset:" ^ sheet_source stmt ]
  | Import _ -> [ "import:" ^ sheet_source stmt ]
  | Namespace _ -> [ "namespace:" ^ sheet_source stmt ]
  | Property _ -> [ "property:" ^ sheet_source stmt ]
  | Layer_decl names ->
      [
        "layer-decl:"
        ^ String.concat "." (List.map Css.Stylesheet.string_of_layer_name names);
      ]
  | Layer (name, block) ->
      ("layer:"
      ^ Option.fold ~none:"" ~some:Css.Stylesheet.string_of_layer_name name)
      :: block_lines block
  | Media (condition, block) ->
      ("media:" ^ Css.Pp.to_string ~minify:true Css.Media.pp condition)
      :: block_lines block
  | Container (name, condition, block) ->
      ("container:"
      ^ Option.value ~default:"" name
      ^ ":"
      ^ Option.fold ~none:"" ~some:Css.Container.to_string condition)
      :: block_lines block
  | Supports (condition, block) ->
      ("supports:" ^ Css.Pp.to_string ~minify:true Css.Supports.pp condition)
      :: block_lines block
  | When (condition, block) ->
      ("when:" ^ conditional_shape condition) :: block_lines block
  | Else (condition, block) ->
      ("else:" ^ Option.fold ~none:"" ~some:conditional_shape condition)
      :: block_lines block
  | Supports_condition (name, declarations) ->
      ("supports-condition:" ^ name) :: declaration_lines declarations
  | Starting_style block -> "starting-style" :: block_lines block
  | Origin (origin, block) ->
      ("origin:"
      ^ string_of_int
          (Css.Stylesheet.origin_importance_rank ~important:false origin))
      :: block_lines block
  | Moz_document (_, block) -> "moz-document" :: block_lines block
  | Scope (start, boundary, block) ->
      let bound =
        Option.fold ~none:"" ~some:(Css.Selector.to_string ~minify:true)
      in
      String.concat ":" [ "scope"; bound start; bound boundary ]
      :: block_lines block
  | Keyframes (name, keyframes) ->
      ("keyframes:" ^ name)
      :: (keyframes
         |> List.map (fun (keyframe : Css.Stylesheet.keyframe) ->
             ("  keyframe:" ^ Css.Keyframe.to_string keyframe.selector)
             :: prefixed "    " (declaration_lines keyframe.declarations))
         |> List.concat)
  | Webkit_keyframes (name, keyframes) ->
      ("webkit-keyframes:" ^ name)
      :: (keyframes
         |> List.map (fun (keyframe : Css.Stylesheet.keyframe) ->
             ("  keyframe:" ^ Css.Keyframe.to_string keyframe.selector)
             :: prefixed "    " (declaration_lines keyframe.declarations))
         |> List.concat)
  | Moz_keyframes (name, keyframes) ->
      ("moz-keyframes:" ^ name)
      :: (keyframes
         |> List.map (fun (keyframe : Css.Stylesheet.keyframe) ->
             ("  keyframe:" ^ Css.Keyframe.to_string keyframe.selector)
             :: prefixed "    " (declaration_lines keyframe.declarations))
         |> List.concat)
  | Font_face descriptors ->
      "font-face" :: List.map (fun _ -> "descriptor") descriptors
  | Page (selector, declarations) ->
      ("page:" ^ page_selectors_string selector)
      :: declaration_lines declarations
  | Page_with_margins (selector, descriptors, margins) ->
      String.concat ":"
        [
          "page-margins";
          page_selectors_string selector;
          String.concat "," (List.map raw_descriptor_shape descriptors);
        ]
      :: List.map
           (fun (margin : Css.Stylesheet.page_margin_rule) ->
             "  margin:" ^ margin.name ^ ":"
             ^ String.concat ","
                 (List.map raw_descriptor_shape margin.descriptors))
           margins
  | Font_palette_values (name, descriptors) ->
      [
        "font-palette-values:" ^ name ^ ":"
        ^ String.concat "," (List.map (fun _ -> "descriptor") descriptors);
      ]
  | View_transition descriptors ->
      [
        "view-transition:"
        ^ String.concat "," (List.map (fun _ -> "descriptor") descriptors);
      ]
  | Position_try (name, declarations) ->
      ("position-try:" ^ name) :: declaration_lines declarations
  | Counter_style (name, _) -> [ "counter-style:" ^ name ]
  | Viewport (prefix, descriptors) ->
      let label =
        match prefix with
        | Standard -> "viewport"
        | Ms_prefixed -> "-ms-viewport"
      in
      [
        label ^ ":"
        ^ String.concat ","
            (List.map
               (fun (d : Css.Stylesheet.viewport_descriptor) -> d.name)
               descriptors);
      ]
  | Unknown_at_rule { name; _ } -> [ "unknown-at-rule:" ^ name ]
  | Font_feature_values (families, _) ->
      [ "font-feature-values:" ^ string_of_int (List.length families) ]
  | Bang_comment _ -> [ "bang-comment" ]

let stylesheet_shape sheet = List.concat_map statement_shape sheet

let check_eval_stylesheet_preserves_structure name ~ctx ?layer_order ?layer
    input =
  let sheet = stylesheet_of_string input in
  let actual = Css.eval_stylesheet ctx ?layer_order ?layer sheet in
  Alcotest.(check (list string))
    name (stylesheet_shape sheet) (stylesheet_shape actual)

let specificity_score selector =
  let specificity = Css.Selector.specificity selector in
  (specificity.ids * 1_000_000)
  + (specificity.classes * 1_000)
  + specificity.elements

let declaration_source decl = Css.Declaration.to_string ~minify:true decl

let declaration_value_source decl =
  let source = declaration_source decl in
  let value =
    match String.index_opt source ':' with
    | None -> source
    | Some i -> String.sub source (i + 1) (String.length source - i - 1)
  in
  match String.index_opt value '!' with
  | None -> value
  | Some i -> String.sub value 0 i

let scope_selector_matches (document : Css.Context.document) = function
  | None -> true
  | Some selector ->
      Option.equal Css.Selector.equal document.scope (Some selector)
      || Css.Context.matches_selector document selector

let scope_boundary_allows document start boundary =
  scope_selector_matches document start
  &&
  match boundary with
  | None -> true
  | Some _ -> not (scope_selector_matches document boundary)

let rec conditional_matches query = function
  | Css.Stylesheet.Media_condition condition ->
      Css.Context.matches_media query condition
  | Supports_condition_test condition ->
      Css.Context.matches_supports query condition
  | And (left, right) ->
      conditional_matches query left && conditional_matches query right
  | Or (left, right) ->
      conditional_matches query left || conditional_matches query right

let add_matching_declarations ~source_order ~property ~origin ~layer
    ~specificity ~scope_hops acc declarations =
  List.fold_left
    (fun acc decl ->
      let order = !source_order in
      incr source_order;
      if Css.Declaration.property_name decl <> property then acc
      else
        let candidate : Css.Stylesheet.cascade_candidate =
          {
            origin;
            important = Css.Declaration.is_important decl;
            layer;
            specificity;
            scope_hops;
            source_order = order;
            value = declaration_value_source decl;
          }
        in
        candidate :: acc)
    acc declarations

let collect_declaration_statement ~source_order ~property ~origin ~layer
    ~current_specificity ~scope_hops acc (stmt : Css.Stylesheet.statement) =
  match stmt with
  | Declarations declarations -> (
      match current_specificity with
      | None -> (acc, None)
      | Some specificity ->
          ( add_matching_declarations ~source_order ~property ~origin ~layer
              ~specificity ~scope_hops acc declarations,
            None ))
  | _ -> (acc, None)

let rec collect_matching_block ~source_order ~property ~document ~query ~origin
    ~layer ~current_specificity ~scope_hops acc block =
  let step (acc, chain_matched) stmt =
    collect_matching_statement ~source_order ~property ~document ~query ~origin
      ~layer ~current_specificity ~scope_hops ~chain_matched acc stmt
  in
  fst (List.fold_left step (acc, None) block)

and collect_matching_statement ~source_order ~property ~document ~query ~origin
    ~layer ~current_specificity ~scope_hops ~chain_matched acc stmt =
  match
    collect_rule_like_statement ~source_order ~property ~document ~query ~origin
      ~layer ~current_specificity ~scope_hops acc stmt
  with
  | Some result -> result
  | None -> (
      match
        collect_conditional_statement ~source_order ~property ~document ~query
          ~origin ~layer ~current_specificity ~scope_hops ~chain_matched acc
          stmt
      with
      | Some result -> result
      | None ->
          collect_declaration_statement ~source_order ~property ~origin ~layer
            ~current_specificity ~scope_hops acc stmt)

and collect_rule_like_statement ~source_order ~property ~document ~query ~origin
    ~layer ~current_specificity ~scope_hops acc = function
  | Css.Stylesheet.Rule rule ->
      Some
        (if Css.Context.matches_selector document rule.selector then
           let specificity = specificity_score rule.selector in
           let acc =
             add_matching_declarations ~source_order ~property ~origin ~layer
               ~specificity ~scope_hops acc rule.declarations
           in
           ( collect_matching_block ~source_order ~property ~document ~query
               ~origin ~layer ~current_specificity:(Some specificity)
               ~scope_hops acc rule.nested,
             None )
         else (acc, None))
  | Layer (name, block) ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query ~origin
            ~layer:(Option.map Css.Stylesheet.string_of_layer_name name)
            ~current_specificity ~scope_hops acc block,
          None )
  | Starting_style block ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query
            ~origin ~layer ~current_specificity ~scope_hops acc block,
          None )
  | Origin (origin, block) ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query
            ~origin ~layer ~current_specificity ~scope_hops acc block,
          None )
  | Moz_document (_, block) ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query
            ~origin ~layer ~current_specificity ~scope_hops acc block,
          None )
  | Scope (start, boundary, block) ->
      Some
        (if scope_boundary_allows document start boundary then
           ( collect_matching_block ~source_order ~property ~document ~query
               ~origin ~layer ~current_specificity ~scope_hops:(Some 0) acc
               block,
             None )
         else (acc, None))
  | _ -> None

and collect_conditional_statement ~source_order ~property ~document ~query
    ~origin ~layer ~current_specificity ~scope_hops ~chain_matched acc =
  let collect_if matched block =
    let acc =
      if matched then
        collect_matching_block ~source_order ~property ~document ~query ~origin
          ~layer ~current_specificity ~scope_hops acc block
      else acc
    in
    (acc, Some matched)
  in
  function
  | Media (condition, block) ->
      Some (collect_if (Css.Context.matches_media query condition) block)
  | Supports (condition, block) ->
      Some (collect_if (Css.Context.matches_supports query condition) block)
  | Container (name, condition, block) ->
      let matches =
        match condition with
        | Some c -> Css.Context.matches_container query ?name c
        | None -> true
      in
      Some (collect_if matches block)
  | When (condition, block) ->
      Some (collect_if (conditional_matches query condition) block)
  | Else (condition, block) ->
      Some
        (match chain_matched with
        | None -> (acc, None)
        | Some previous_matched ->
            let condition_matched =
              match condition with
              | None -> true
              | Some condition -> conditional_matches query condition
            in
            let matched = (not previous_matched) && condition_matched in
            let acc =
              if matched then
                collect_matching_block ~source_order ~property ~document ~query
                  ~origin ~layer ~current_specificity ~scope_hops acc block
              else acc
            in
            (acc, Some (previous_matched || matched)))
  | _ -> None

let cascade_layer_candidate_of (c : Css.Stylesheet.cascade_candidate) :
    Css.Stylesheet.cascade_layer_candidate =
  {
    layer = c.layer;
    important = c.important;
    source_order = c.source_order;
    value = c.value;
  }

let cascade_origin_candidate_of (c : Css.Stylesheet.cascade_candidate) :
    Css.Stylesheet.cascade_origin_candidate =
  {
    origin = c.origin;
    important = c.important;
    source_order = c.source_order;
    value = c.value;
  }

let same_layer_candidate (c : Css.Stylesheet.cascade_candidate)
    (l : Css.Stylesheet.cascade_layer_candidate) =
  l.layer = c.layer && l.important = c.important
  && l.source_order = c.source_order

let same_origin_candidate (c : Css.Stylesheet.cascade_candidate)
    (l : Css.Stylesheet.cascade_origin_candidate) =
  Css.Stylesheet.equal_cascade_origin l.origin c.origin
  && l.important = c.important
  && l.source_order = c.source_order

let lower_layer_candidates ~layer_order
    (candidate : Css.Stylesheet.cascade_candidate) candidates =
  Css.Stylesheet.cascade_revert_layer_candidates ~layer_order
    ~important:candidate.important ~current_layer:candidate.layer
    (List.map cascade_layer_candidate_of candidates)

let lower_origin_candidates (candidate : Css.Stylesheet.cascade_candidate)
    candidates =
  Css.Stylesheet.cascade_revert_origin_candidates ~important:candidate.important
    ~current_origin:candidate.origin
    (List.map cascade_origin_candidate_of candidates)

let rec winning_resolved_candidate ~layer_order candidates =
  Option.bind (Css.Stylesheet.winning_cascade_candidate ~layer_order candidates)
    (fun (candidate : Css.Stylesheet.cascade_candidate) ->
      match candidate.value with
      | "revert-layer" ->
          let lower =
            lower_layer_candidates ~layer_order candidate candidates
          in
          let candidates =
            List.filter
              (fun candidate ->
                List.exists (same_layer_candidate candidate) lower)
              candidates
          in
          winning_resolved_candidate ~layer_order candidates
      | "revert" ->
          let lower = lower_origin_candidates candidate candidates in
          let candidates =
            List.filter
              (fun candidate ->
                List.exists (same_origin_candidate candidate) lower)
              candidates
          in
          winning_resolved_candidate ~layer_order candidates
      | value -> Some (candidate, value))

let resolve_stylesheet_property ?(layer_order = []) ~ctx ~document ~query
    ~property stylesheet =
  let source_order = ref 0 in
  let candidates =
    collect_matching_block ~source_order ~property ~document ~query
      ~origin:Css.Stylesheet.Author ~layer:None ~current_specificity:None
      ~scope_hops:None [] stylesheet
  in
  winning_resolved_candidate ~layer_order candidates
  |> Option.map (fun ((candidate : Css.Stylesheet.cascade_candidate), value) ->
      let decl = Css.Declaration.of_string (property ^ ": " ^ value) in
      match candidate.layer with
      | None -> Css.eval_declaration ctx ~layer_order decl
      | Some layer -> Css.eval_declaration ctx ~layer_order ~layer decl)

let check_resolved_property ?layer_order name ~ctx ~document ~query ~property
    ~expected stylesheet =
  let expected = Css.Declaration.of_string expected in
  let actual =
    resolve_stylesheet_property ?layer_order ~ctx ~document ~query ~property
      (stylesheet_of_string stylesheet)
  in
  Alcotest.(check (option decl_t)) name (Some expected) actual

let check_ast_resolved_property ?layer_order name ~ctx ~document ~query
    ~property ~expected stylesheet =
  let expected = Css.Declaration.of_string expected in
  let actual =
    resolve_stylesheet_property ?layer_order ~ctx ~document ~query ~property
      stylesheet
  in
  Alcotest.(check (option decl_t)) name (Some expected) actual

let check_selector_match name ~ctx ~expected input =
  let selector = Css.Selector.of_string input in
  Alcotest.(check bool)
    name expected
    (Css.Context.matches_selector ctx selector)

let check_media_match name ~ctx ~expected input =
  let query = Css.Media.of_string input in
  Alcotest.(check bool) name expected (Css.Context.matches_media ctx query)

let check_supports_match name ~ctx ~expected input =
  let condition = Css.Supports.of_string input in
  Alcotest.(check bool)
    name expected
    (Css.Context.matches_supports ctx condition)

(* MQ4 sections 2.4.2, 3.1, and 5.1: boolean feature values, unknown
   propagation, and resolution units are independent parts of matching. *)
let media_boolean_values () =
  let ctx =
    Css.Context.query
      ~media_features:
        (List.map Css.Media.of_string
           [
             "(monochrome: 0)";
             "(color-index: 0)";
             "(grid: 0)";
             "(forced-colors: none)";
             "(prefers-reduced-motion: no-preference)";
             "(color: 8)";
           ])
      ()
  in
  List.iter
    (check_media_match "zero or none is false" ~ctx ~expected:false)
    [
      "(monochrome)";
      "(color-index)";
      "(grid)";
      "(forced-colors)";
      "(prefers-reduced-motion)";
    ];
  check_media_match "nonzero is true" ~ctx ~expected:true "(color)"

let media_unknown_logic () =
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ Css.Media.of_string "(width: 800px)" ]
      ()
  in
  List.iter
    (fun (input, expected) -> check_media_match input ~ctx ~expected input)
    [
      ("not (cascade-unknown: 1)", false);
      ("not (orientation: sideways)", false);
      ("not ((orientation: sideways) or (width < 0px))", false);
      ("not ((orientation: sideways) and (width < 0px))", true);
      ("(orientation: sideways) or (width >= 0px)", true);
      ("not screen and (orientation: sideways)", false);
      ("not print and (orientation: sideways)", true);
      ("", true);
    ]

let media_resolution_units () =
  let ctx =
    Css.Context.query
      ~media_features:[ Css.Media.of_string "(resolution: 1dppx)" ]
      ()
  in
  List.iter
    (check_media_match "equivalent resolution units" ~ctx ~expected:true)
    [
      "(resolution: 96dpi)";
      "(resolution: 1dppx)";
      "(resolution >= 37dpcm)";
      "(resolution < 38dpcm)";
    ];
  check_media_match "larger resolution does not match" ~ctx ~expected:false
    "(resolution: 192dpi)"

let check_container_match name ~ctx ?name:container_name ~expected input =
  let condition = Css.Container.of_string input in
  Alcotest.(check bool)
    name expected
    (Css.Context.matches_container ctx ?name:container_name condition)

let check_resolved_url name ~loader ~expected input =
  match Css.Context.resolve_url loader input with
  | Ok actual -> Alcotest.(check string) name expected actual
  | Error _ ->
      Alcotest.failf "%s: expected URL %S to resolve to %S" name input expected

let check_url_error name ~loader input =
  match Css.Context.resolve_url loader input with
  | Ok actual ->
      Alcotest.failf "%s: expected URL %S to be unresolved, got %S" name input
        actual
  | Error _ -> ()

let check_registered_property_valid name ~registry input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.validate_registered_custom_property registry decl with
  | Ok () -> ()
  | Error msg ->
      Alcotest.failf "%s: expected registered property %S to validate, got %S"
        name input msg

let check_registered_property_error name ~registry input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.validate_registered_custom_property registry decl with
  | Ok () ->
      Alcotest.failf "%s: expected registered property %S to fail validation"
        name input
  | Error _ -> ()

let check_import_loaded name ?query ~loader ~expected input =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query loader import_rule with
  | Ok stylesheet ->
      Alcotest.(check string)
        name expected
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ -> Alcotest.failf "%s: expected import %S to load" name input

let check_import_error name ?query ~loader input =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query loader import_rule with
  | Ok stylesheet ->
      Alcotest.failf "%s: expected import %S to be blocked, got %S" name input
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ -> ()

let check_layered_import_loaded name ?query ~loader ~layer_order ~expected input
    =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query ~layer_order loader import_rule with
  | Ok stylesheet ->
      Alcotest.(check string)
        name expected
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ ->
      Alcotest.failf "%s: expected layered import %S to load" name input

let check_layered_import_error name ?query ~loader ~layer_order input =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query ~layer_order loader import_rule with
  | Ok stylesheet ->
      Alcotest.failf "%s: expected layered import %S to be blocked, got %S" name
        input
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ -> ()

let test_computed_value_resolution_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--brand: red";
          Css.Declaration.of_string "--gap: 1rem";
        ]
      ~inherited_values:
        [
          Css.Declaration.of_string "color: blue";
          Css.Declaration.of_string "font-size: 10px";
          Css.Declaration.of_string "line-height: 1.5";
          Css.Declaration.of_string "border-collapse: collapse";
        ]
      ~initial_values:
        [
          Css.Declaration.of_string "display: inline";
          Css.Declaration.of_string "width: auto";
          Css.Declaration.of_string "line-height: normal";
          Css.Declaration.of_string "border-collapse: separate";
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 10.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1024.)
      ~viewport_height:(Css.Values.Px 768.)
      ~container_width:(Css.Values.Px 640.)
      ~container_height:(Css.Values.Px 480.) ()
  in
  check_eval_value "initial keyword uses property initial value" ~ctx
    ~expected:"inline" "display: initial";
  check_eval_value "inherit keyword uses inherited value" ~ctx ~expected:"blue"
    "color: inherit";
  check_eval_value "unset on inherited property uses inherited value" ~ctx
    ~expected:"blue" "color: unset";
  check_eval_value "unset on non-inherited property uses initial value" ~ctx
    ~expected:"auto" "width: unset";
  check_eval_value "line-height inherit uses inherited value" ~ctx
    ~expected:"1.5" "line-height: inherit";
  check_eval_value "line-height unset uses inherited value" ~ctx ~expected:"1.5"
    "line-height: unset";
  check_eval_value "line-height initial uses initial value" ~ctx
    ~expected:"normal" "line-height: initial";
  check_eval_value "border-collapse unset uses inherited value" ~ctx
    ~expected:"collapse" "border-collapse: unset";
  check_eval_value "currentColor uses explicit current color" ~ctx
    ~expected:"red" "border-color: currentColor";
  check_eval_value "custom property var resolves from context" ~ctx
    ~expected:"red" "color: var(--brand)";
  check_eval_value "custom property fallback resolves when missing" ~ctx
    ~expected:"green" "color: var(--missing, green)";
  check_eval_value "custom length var resolves then computes" ~ctx
    ~expected:"16px" "margin-left: var(--gap)";
  check_eval_value "rem resolves against root font size" ~ctx ~expected:"32px"
    "margin-left: 2rem";
  check_eval_value "em resolves against parent font size" ~ctx ~expected:"20px"
    "font-size: 2em";
  check_eval_value "vw resolves against viewport width" ~ctx ~expected:"512px"
    "width: 50vw";
  check_eval_value "vh resolves against viewport height" ~ctx ~expected:"192px"
    "height: 25vh";
  check_eval_value "cqw resolves against container width" ~ctx ~expected:"320px"
    "width: 50cqw";
  check_eval_value "cqh resolves against container height" ~ctx
    ~expected:"120px" "height: 25cqh";
  check_eval_value "relative URL resolves against base URL" ~ctx
    ~expected:"url(https://example.test/img/logo.svg)"
    "background-image: url(../img/logo.svg)";
  check_eval_preserves "missing var without fallback is unresolved" ~ctx
    "color: var(--missing)";
  check_eval_preserves "viewport unit without viewport context is unresolved"
    ~ctx:Css.Context.empty "width: 50vw";
  check_eval_preserves "currentColor without current color is unresolved"
    ~ctx:Css.Context.empty "color: currentColor";
  check_eval_preserves "relative URL without base URL is unresolved"
    ~ctx:Css.Context.empty "background-image: url(../img/logo.svg)"

let computed_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--brand: red";
          Css.Declaration.of_string "--accent: var(--brand)";
          Css.Declaration.of_string "--cycle-a: var(--cycle-b)";
          Css.Declaration.of_string "--cycle-b: var(--cycle-a)";
          Css.Declaration.of_string "--cycle-fb: var(--cycle-fb, red)";
          Css.Declaration.of_string "--space: 2em";
        ]
      ~inherited_values:[ Css.Declaration.of_string "font-size: 12px" ]
      ~initial_values:
        [
          Css.Declaration.of_string "color: canvastext";
          Css.Declaration.of_string "font-size: medium";
          Css.Declaration.of_string "line-height: normal";
          Css.Declaration.of_string "margin-left: 0";
        ]
      ~base_url:"https://example.test/assets/css/theme.css"
      ~root_font_size:(Css.Values.Px 20.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1200.)
      ~viewport_height:(Css.Values.Px 800.)
      ~container_width:(Css.Values.Px 300.)
      ~container_height:(Css.Values.Px 900.) ()
  in
  check_eval_value "inherit on root falls back to property initial" ~ctx
    ~expected:"canvastext" "color: inherit";
  check_eval_value "nested custom property var resolves transitively" ~ctx
    ~expected:"red" "color: var(--accent)";
  check_eval_value "nested var fallback resolves transitively" ~ctx
    ~expected:"red" "color: var(--missing, var(--accent))";
  check_eval_value "custom length var resolves with parent font context" ~ctx
    ~expected:"24px" "margin-left: var(--space)";
  check_eval_value "font-size percentage resolves against parent font" ~ctx
    ~expected:"18px" "font-size: 150%";
  check_eval_value "absolute inches canonicalize to px" ~ctx ~expected:"96px"
    "margin-left: 1in";
  check_eval_value "absolute points canonicalize to px" ~ctx ~expected:"96px"
    "margin-left: 72pt";
  check_eval_value "viewport vmin uses smaller viewport side" ~ctx
    ~expected:"80px" "width: 10vmin";
  check_eval_value "viewport vmax uses larger viewport side" ~ctx
    ~expected:"120px" "width: 10vmax";
  check_eval_value "container cqmin uses smaller container side" ~ctx
    ~expected:"30px" "width: 10cqmin";
  check_eval_value "container cqmax uses larger container side" ~ctx
    ~expected:"90px" "width: 10cqmax";
  check_eval_value "same-directory URL resolves against base URL" ~ctx
    ~expected:"url(https://example.test/assets/css/panel.css)"
    "background-image: url(panel.css)";
  check_eval_value "root-relative URL preserves origin" ~ctx
    ~expected:"url(https://example.test/icons/logo.svg)"
    "background-image: url(/icons/logo.svg)";
  check_eval_preserves "custom property cycle is unresolved" ~ctx
    "color: var(--cycle-a)";
  (* A cyclic custom property is invalid at computed-value time: its own var()
     fallback does not rescue it, so the consumer's fallback wins. *)
  check_eval_value "cyclic var ignores its own fallback for the consumer's" ~ctx
    ~expected:"blue" "color: var(--cycle-fb, blue)";
  check_eval_preserves "width percentage needs layout context" ~ctx "width: 50%";
  check_eval_preserves "em unit without parent font context is unresolved"
    ~ctx:Css.Context.empty "margin-left: 2em";
  check_eval_preserves "rem unit without root font context is unresolved"
    ~ctx:Css.Context.empty "margin-left: 2rem";
  check_eval_preserves "container unit without container context is unresolved"
    ~ctx:Css.Context.empty "width: 50cqw"

let test_document_selector_context_contract () =
  let ctx =
    Css.Context.document ~root:"html"
      ~scope:(Css.Selector.of_string ".card")
      ~element:"button" ~classes:[ "btn"; "primary" ] ~ids:[ "submit" ]
      ~attributes:
        [
          ("type", Some "submit");
          ("disabled", None);
          ("data-state", Some "ready");
        ]
      ~pseudo_classes:[ "focus-visible"; "enabled" ]
      ~pseudo_elements:[ "before" ] ()
  in
  check_selector_match "type selector matches element" ~ctx ~expected:true
    "button";
  check_selector_match "class selector matches class list" ~ctx ~expected:true
    ".primary";
  check_selector_match "id selector matches id list" ~ctx ~expected:true
    "#submit";
  check_selector_match "attribute presence selector matches" ~ctx ~expected:true
    "[disabled]";
  check_selector_match "attribute value selector matches" ~ctx ~expected:true
    "[type=submit]";
  check_selector_match "pseudo-class selector matches explicit state" ~ctx
    ~expected:true ":focus-visible";
  check_selector_match "pseudo-element selector matches explicit context" ~ctx
    ~expected:true "::before";
  check_selector_match "compound selector matches all simple selectors" ~ctx
    ~expected:true "button.btn.primary[type=submit]:focus-visible";
  check_selector_match "wrong element fails" ~ctx ~expected:false "a";
  check_selector_match "missing class fails" ~ctx ~expected:false ".secondary";
  check_selector_match "missing attribute value fails" ~ctx ~expected:false
    "[type=button]";
  check_selector_match "missing pseudo-class fails" ~ctx ~expected:false
    ":hover";
  check_selector_match "missing pseudo-element fails" ~ctx ~expected:false
    "::after"

let test_query_context_evaluation_contract () =
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:
        [
          feature "width" (px 1024.);
          feature "height" (px 768.);
          feature "prefers-color-scheme" (ident "dark");
          feature "dynamic-range" (ident "high");
        ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "container-type" "inline-size";
          Css.Supports.property "color" "oklch(50% 0.1 20)";
          Css.Supports.func "selector" ":has(img)";
        ]
      ~container_name:"card"
      ~container_features:
        [
          container_feature "inline-size" (px 640.);
          container_feature "block-size" (px 480.);
          Css.Container.style ~value:"dark" "--theme";
          Css.Container.scroll_state "stuck" "top";
        ]
      ()
  in
  check_media_match "media type and min-width match" ~ctx ~expected:true
    "screen and (min-width: 48em)";
  check_media_match "media range comparison matches" ~ctx ~expected:true
    "(width >= 1024px)";
  check_media_match "media feature equality matches" ~ctx ~expected:true
    "(prefers-color-scheme: dark)";
  check_media_match "media not operator negates" ~ctx ~expected:false
    "not screen";
  check_media_match "media range comparison fails" ~ctx ~expected:false
    "(width > 1200px)";
  check_supports_match "supported declaration matches" ~ctx ~expected:true
    "(display: grid)";
  check_supports_match "supported selector function matches" ~ctx ~expected:true
    "selector(:has(img))";
  check_supports_match "supports and combines true branches" ~ctx ~expected:true
    "(display: grid) and (container-type: inline-size)";
  check_supports_match "supports not negates unsupported branch" ~ctx
    ~expected:true "not (display: ruby)";
  check_supports_match "unsupported declaration fails" ~ctx ~expected:false
    "(display: ruby)";
  check_container_match "named container and width match" ~ctx ~name:"card"
    ~expected:true "(inline-size >= 40rem)";
  check_container_match "wrong container name fails" ~ctx ~name:"sidebar"
    ~expected:false "(inline-size >= 40rem)";
  check_container_match "style query matches explicit container state" ~ctx
    ~name:"card" ~expected:true "style(--theme: dark)";
  check_container_match "scroll-state query matches explicit container state"
    ~ctx ~name:"card" ~expected:true "scroll-state(stuck: top)";
  check_container_match "container range comparison fails" ~ctx ~name:"card"
    ~expected:false "(inline-size > 900px)"

let test_loader_context_contract () =
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:
        [
          ("https://example.test/css/base.css", ".base{display:block}");
          ("https://example.test/css/theme.css", ".theme{color:red}");
          ("https://example.test/print.css", ".print{display:none}");
        ]
      ()
  in
  check_resolved_url "relative URL uses base directory" ~loader
    ~expected:"https://example.test/css/theme.css" "theme.css";
  check_resolved_url "parent-relative URL normalizes path" ~loader
    ~expected:"https://example.test/img/logo.svg" "../img/logo.svg";
  check_resolved_url "root-relative URL preserves origin" ~loader
    ~expected:"https://example.test/print.css" "/print.css";
  check_resolved_url "absolute URL remains absolute" ~loader
    ~expected:"https://cdn.example.test/site.css"
    "https://cdn.example.test/site.css";
  check_resolved_url "data URL remains absolute" ~loader
    ~expected:"data:image/png;base64,x" "data:image/png;base64,x";
  check_resolved_url "scheme-relative URL inherits scheme" ~loader
    ~expected:"https://cdn.example.test/site.css" "//cdn.example.test/site.css";
  check_resolved_url "query reference keeps base path" ~loader
    ~expected:"https://example.test/css/app.css?v=1" "?v=1";
  check_resolved_url "fragment reference keeps base path" ~loader
    ~expected:"https://example.test/css/app.css#icon" "#icon";
  check_resolved_url "scheme matching is case-insensitive" ~loader
    ~expected:"https://cdn.example.test/site.css"
    "HTTPS://cdn.example.test/site.css";
  check_url_error "relative URL without base is unresolved"
    ~loader:Css.Context.empty_loader "theme.css";
  check_import_loaded "loads same-directory import" ~loader
    ~expected:".theme{color:red}" "@import url(theme.css);";
  check_import_loaded "loads root-relative import" ~loader
    ~expected:".print{display:none}" "@import url(/print.css);";
  check_import_error "missing import is unresolved" ~loader
    "@import url(missing.css);"

let test_animation_context_contract () =
  let ctx =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.25
      ~animated_properties:[ "opacity"; "transform"; "--offset" ]
      ()
  in
  Alcotest.(check bool)
    "standard property animation is detected" true
    (Css.Context.animates_property "opacity" ctx);
  Alcotest.(check bool)
    "custom property animation is detected" true
    (Css.Context.animates_property "--offset" ctx);
  Alcotest.(check bool)
    "unanimated property is absent" false
    (Css.Context.animates_property "color" ctx);
  Alcotest.(check (option string))
    "timeline time is preserved" (Some "250ms") ctx.timeline_time;
  Alcotest.(check (option (float 0.0001)))
    "animation progress is preserved" (Some 0.25) ctx.progress

let test_property_registration_context_contract () =
  let length_reg =
    Css.Context.property_registration "--gap"
      (Css.Variables.Syntax Css.Variables.Length) ~inherits:true
      ~initial_value:"0px"
  in
  let color_reg =
    Css.Context.property_registration "--brand"
      (Css.Variables.Syntax Css.Variables.Color) ~inherits:false
      ~initial_value:"red"
  in
  let universal_reg =
    Css.Context.property_registration "--tokens"
      (Css.Variables.Syntax Css.Variables.Universal) ~inherits:true
  in
  let registry =
    Css.Context.property_registry
      ~property_registrations:[ length_reg; color_reg; universal_reg ]
      ()
  in
  Alcotest.(check (option string))
    "registered property lookup" (Some "--gap")
    (Option.map
       (fun (reg : Css.Context.property_registration) -> reg.name)
       (Css.Context.registered_property "--gap" registry));
  Alcotest.(check bool)
    "registered inherits flag" false
    (let (reg : Css.Context.property_registration) = color_reg in
     reg.inherits);
  Alcotest.(check (option string))
    "registered initial value" (Some "0px")
    (let (reg : Css.Context.property_registration) = length_reg in
     reg.initial_value);
  check_registered_property_valid "registered length accepts length" ~registry
    "--gap: 1rem";
  check_registered_property_valid "registered color accepts color" ~registry
    "--brand: color(display-p3 1 0 0)";
  check_registered_property_valid "universal syntax accepts token stream"
    ~registry "--tokens: { color: red }";
  check_registered_property_valid "unregistered custom property remains valid"
    ~registry "--unknown: red";
  check_registered_property_error "registered length rejects color" ~registry
    "--gap: red";
  check_registered_property_error "registered color rejects length" ~registry
    "--brand: 1rem";
  check_registered_property_error "non-custom declaration is not registered"
    ~registry "color: red"

let computed_calc_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--pad-y: 1em";
          Css.Declaration.of_string "--pad-x: 2rem";
          Css.Declaration.of_string "--brand-alpha: 0.5";
        ]
      ~inherited_values:[ Css.Declaration.of_string "font-size: 12px" ]
      ~initial_values:[ Css.Declaration.of_string "opacity: 1" ]
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~viewport_width:(Css.Values.Px 1000.)
      ~viewport_height:(Css.Values.Px 600.)
      ~container_width:(Css.Values.Px 500.)
      ~container_height:(Css.Values.Px 400.) ()
  in
  check_eval_value "calc resolves compatible absolute and rem units" ~ctx
    ~expected:"18px" "margin-left: calc(1rem + 2px)";
  check_eval_value "calc resolves multiplication with rem" ~ctx ~expected:"32px"
    "margin-left: calc(2 * 1rem)";
  check_eval_value "calc resolves division with em" ~ctx ~expected:"6px"
    "margin-left: calc(1em / 2)";
  check_eval_value "margin shorthand computes each length slot" ~ctx
    ~expected:"12px 32px" "margin: 1em 2rem";
  check_eval_value "padding shorthand computes var-backed slots" ~ctx
    ~expected:"12px 32px" "padding: var(--pad-y) var(--pad-x)";
  check_eval_value "border-width shorthand computes absolute units" ~ctx
    ~expected:"1px 2px 4px 8px" "border-width: 1px 0.125rem 4px 0.5rem";
  check_eval_value "number custom property resolves for opacity" ~ctx
    ~expected:"0.5" "opacity: var(--brand-alpha)";
  check_eval_value "viewport dvw resolves against viewport width" ~ctx
    ~expected:"250px" "width: 25dvw";
  check_eval_value "viewport svh resolves against viewport height" ~ctx
    ~expected:"60px" "height: 10svh";
  check_eval_value "viewport dvh resolves against viewport height" ~ctx
    ~expected:"60px" "height: 10dvh";
  check_eval_value "container cqi resolves against inline size" ~ctx
    ~expected:"125px" "width: 25cqi";
  check_eval_value "container cqb resolves against block size" ~ctx
    ~expected:"100px" "height: 25cqb";
  check_eval "calc partially residualizes known length under unresolved layout"
    ~ctx ~expected:"width: calc(100% - 16px)" "width: calc(100% - 1rem)";
  check_eval_preserves "font metric unit ch needs font metrics" ~ctx
    "width: 4ch";
  check_eval_preserves "font metric unit ex needs font metrics" ~ctx
    "width: 2ex"

let eval_calc_family_contract () =
  (* The reader holds calc unfolded; all-constant reduction is an optimize
     transform, and eval reduces what the supplied context resolves. These
     eval-time tests exercise var-substitution chains and mixed-unit calc that
     needs a viewport / font / container context to fold. *)
  let ctx =
    Css.Context.v
      ~custom_properties:[ Css.Declaration.of_string "--lp: calc(50% + 1rem)" ]
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~viewport_width:(Css.Values.Px 1000.)
      ~container_width:(Css.Values.Px 400.) ()
  in
  check_eval "eval partially folds length-percentage calc" ~ctx
    ~expected:"width: calc(50% + 16px)" "width: var(--lp)";
  (* The context supplies no viewport height, so [dvh] stays a live leaf while
     the [rem] term folds. *)
  check_eval "eval preserves unknown viewport leaf after folding known units"
    ~ctx ~expected:"height: calc(16px + 10dvh)" "height: calc(1rem + 10dvh)"

let eval_ast_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--brand: red";
          Css.Declaration.of_string "--gap: calc(1rem + 2px)";
          Css.Declaration.of_string "--rgb: 10 20 30";
          Css.Declaration.of_string "--alpha: 0.5";
          Css.Declaration.of_string "--shift: 1rem";
          Css.Declaration.of_string "--ring-offset: 2px";
          Css.Declaration.of_string "--ring-color: blue";
        ]
      ~inherited_values:
        [
          Css.Declaration.of_string "color: blue";
          Css.Declaration.of_string "font-size: 12px";
        ]
      ~initial_values:[ Css.Declaration.of_string "width: auto" ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1000.)
      ~container_width:(Css.Values.Px 400.) ()
  in
  check_eval "eval resolves inherited css-wide keyword" ~ctx
    ~expected:"color: blue" "color: inherit";
  check_eval "eval resolves initial css-wide keyword" ~ctx
    ~expected:"width: auto" "width: initial";
  check_eval "eval resolves color variable" ~ctx ~expected:"color: red"
    "color: var(--brand)";
  check_eval "eval resolves fallback variable" ~ctx ~expected:"color: green"
    "color: var(--missing, green)";
  check_eval "eval resolves currentColor leaf" ~ctx
    ~expected:"border-color: red" "border-color: currentColor";
  check_eval "eval resolves relative URL leaf" ~ctx
    ~expected:"background-image: url(https://example.test/img/logo.svg)"
    "background-image: url(../img/logo.svg)";
  (* The reader holds calc unfolded; the eval contract here exercises mixed-unit
     calc that needs the context to fold (rem -> px via root font size). *)
  check_eval "eval folds known length calc subtree" ~ctx
    ~expected:"margin-left: 18px" "margin-left: calc(1rem + 2px)";
  check_eval "eval preserves unresolved viewport leaf in calc AST" ~ctx
    ~expected:"height: calc(16px + 10vh)" "height: calc(1rem + 10vh)";
  check_eval "eval preserves unresolved percentage leaf in calc AST" ~ctx
    ~expected:"width: calc(100% - 16px)" "width: calc(100% - 1rem)";
  check_eval "eval preserves abstract var leaf in calc AST" ~ctx
    ~expected:"margin-left: calc(16px + var(--future-gap))"
    "margin-left: calc(1rem + var(--future-gap))";
  check_eval "eval resolves vars inside color function AST" ~ctx
    ~expected:"background-color: rgb(10 20 30 / .5)"
    "background-color: rgb(var(--rgb) / var(--alpha))";
  check_eval "eval resolves vars inside transform list AST" ~ctx
    ~expected:"transform: translateX(16px) rotate(45deg)"
    "transform: translateX(var(--shift)) rotate(calc(30deg + 15deg))";
  check_eval "eval resolves vars and calc inside shadow list AST" ~ctx
    ~expected:"box-shadow: 0 0 0 5px blue"
    "box-shadow: 0 0 0 calc(3px + var(--ring-offset)) var(--ring-color)";
  check_eval "eval resolves shorthand slots independently" ~ctx
    ~expected:"padding: 18px 36px" "padding: var(--gap) calc(var(--gap) * 2)"

let eval_spec_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--gap: calc(1rem + 2px)";
          Css.Declaration.of_string "--outer: var(--missing, var(--gap))";
          Css.Declaration.of_string
            "--nested-fallback: var(--missing, calc(var(--gap) + 1rem))";
          Css.Declaration.of_string "--rgb: 12 34 56";
          Css.Declaration.of_string "--alpha: calc(0.2 + 0.3)";
        ]
      ~inherited_values:[ Css.Declaration.of_string "color: blue" ]
      ~initial_values:
        [
          Css.Declaration.of_string "margin-left: 0";
          Css.Declaration.of_string "color: canvastext";
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1000.)
      ~viewport_height:(Css.Values.Px 800.)
      ~container_width:(Css.Values.Px 400.)
      ~container_height:(Css.Values.Px 300.) ()
  in
  check_eval "eval resolves nested var fallback chain" ~ctx
    ~expected:"margin-left: 18px" "margin-left: var(--outer)";
  check_eval "eval resolves fallback containing nested var and calc" ~ctx
    ~expected:"margin-left: 34px" "margin-left: var(--nested-fallback)";
  check_eval "eval applies css-wide fallback after var substitution" ~ctx
    ~expected:"color: blue" "color: var(--missing, inherit)";
  check_eval "eval applies initial fallback after var substitution" ~ctx
    ~expected:"margin-left: 0" "margin-left: var(--missing, initial)";
  check_eval "eval preserves unresolved fallback subtree as AST" ~ctx
    ~expected:"margin-left: calc(18px + var(--user-gap))"
    "margin-left: var(--missing, calc(var(--gap) + var(--user-gap)))";
  check_eval "eval preserves mixed percentage and resolved dimension" ~ctx
    ~expected:"width: calc(50% + 18px)" "width: calc(50% + var(--gap))";
  check_eval "eval resolves percentage where property defines computed basis"
    ~ctx ~expected:"font-size: 18px" "font-size: calc(100% + 0.5em)";
  check_eval "eval canonicalizes absolute units inside calc" ~ctx
    ~expected:"margin-left: 0" "margin-left: calc(1in - 72pt)";
  check_eval "eval resolves min with comparable computed lengths" ~ctx
    ~expected:"width: 20px" "width: min(10cqi, calc(1rem + 4px))";
  check_eval "eval resolves clamp with comparable computed lengths" ~ctx
    ~expected:"font-size: 18px" "font-size: clamp(1rem, calc(1rem + 2px), 2rem)";
  check_eval "eval resolves vars inside slash color function" ~ctx
    ~expected:"background-color: rgb(12 34 56 / .5)"
    "background-color: rgb(var(--rgb) / var(--alpha))";
  check_eval "eval resolves currentColor inside list item" ~ctx
    ~expected:"box-shadow: 0 0 0 1px blue, 0 0 0 2px red"
    "box-shadow: 0 0 0 1px currentColor, 0 0 0 2px red";
  check_eval "eval resolves nested URL inside image function" ~ctx
    ~expected:
      "background-image: image-set(url(https://example.test/img/a.png) 1x)"
    "background-image: image-set(url(../img/a.png) 1x)"

let eval_css_spec_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--brand: red";
          Css.Declaration.of_string "--gap: calc(1rem + 2px)";
          Css.Declaration.of_string "--angle: calc(30deg + 15deg)";
          Css.Declaration.of_string "--alpha: 0.5";
          Css.Declaration.of_string "--shadow: 0 0 var(--gap) currentColor";
          Css.Declaration.of_string "--image: url(../img/pattern.svg)";
        ]
      ~inherited_values:
        [
          Css.Declaration.of_string "color: blue";
          Css.Declaration.of_string "display: block";
          Css.Declaration.of_string "font-size: 10px";
        ]
      ~initial_values:
        [
          Css.Declaration.of_string "display: inline";
          Css.Declaration.of_string "margin-left: 0";
          Css.Declaration.of_string "opacity: 1";
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 10.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1200.) ()
  in
  check_eval "eval applies inherited css-wide keyword from var fallback" ~ctx
    ~expected:"display: block" "display: var(--missing, inherit)";
  check_eval "eval applies unset fallback using inherited property default" ~ctx
    ~expected:"color: blue" "color: var(--missing, unset)";
  check_eval "eval applies unset fallback using non-inherited initial default"
    ~ctx ~expected:"margin-left: 0" "margin-left: var(--missing, unset)";
  check_eval "eval applies initial fallback after nested var fallback" ~ctx
    ~expected:"opacity: 1" "opacity: var(--missing, var(--other, initial))";
  check_eval "eval residualizes unresolved nested fallback after known rewrite"
    ~ctx ~expected:"margin-left: calc(18px + var(--fluid-gap))"
    "margin-left: var(--missing, calc(var(--gap) + var(--fluid-gap)))";
  check_eval "eval resolves currentColor and length inside shadow custom prop"
    ~ctx ~expected:"box-shadow: 0 0 18px blue" "box-shadow: var(--shadow)";
  check_eval "eval resolves url custom property after substitution" ~ctx
    ~expected:"background-image: url(https://example.test/img/pattern.svg)"
    "background-image: var(--image)";
  check_eval "eval resolves transform longhand angle variable" ~ctx
    ~expected:"rotate: 45deg" "rotate: var(--angle)";
  check_eval "eval resolves opacity alpha inside color function from var" ~ctx
    ~expected:"background-color: rgb(255 0 0 / .5)"
    "background-color: rgb(255 0 0 / var(--alpha))";
  check_eval "eval preserves division by zero calc as residual" ~ctx
    ~expected:"margin-left: calc(16px / 0)" "margin-left: calc(1rem / 0)";
  check_eval "eval preserves sibling-dependent calc as residual" ~ctx
    ~expected:"z-index: calc(sibling-index() + 1)"
    "z-index: calc(sibling-index() + 1)";
  check_eval
    "eval resolves font-relative percentage only where property has basis" ~ctx
    ~expected:"font-size: 15px" "font-size: calc(100% + 0.5em)";
  check_eval "eval leaves layout percentage abstract while reducing known side"
    ~ctx ~expected:"width: calc(50% + 16px)" "width: calc(50% + 1rem)"

let runtime_boundary_contract () =
  let ctx =
    Css.Context.v ~root_font_size:(Css.Values.Px 16.)
      ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Blue) ()
  in
  check_eval_preserves "layout percentage remains a computed percentage" ~ctx
    "width: 50%";
  check_eval "layout calc partially folds lengths but preserves percentage" ~ctx
    ~expected:"width: calc(50% - 16px)" "width: calc(50% - 1rem)";
  check_eval_preserves "grid fr tracks are layout-only used values" ~ctx
    "grid-template-columns: minmax(min-content, 1fr) fit-content(20%)";
  check_eval_preserves "flex content basis is layout-only" ~ctx
    "flex-basis: content";
  check_eval_preserves "aspect-ratio affects layout, not computed eval" ~ctx
    "aspect-ratio: 16 / 9";
  check_eval_preserves "font metric ch unit needs font metrics" ~ctx
    "width: 12ch";
  check_eval_preserves "font metric cap unit needs font metrics" ~ctx
    "height: 2cap";
  check_eval_preserves "color interpolation space is rendering-time behavior"
    ~ctx "color: color-mix(in oklab, red 40%, blue)";
  check_eval_preserves "gamut mapping is rendering-time behavior" ~ctx
    "color: color(display-p3 1 0.5 0)";
  check_eval_preserves "filter raster effects are rendering-time behavior" ~ctx
    "filter: blur(4px) contrast(120%)";
  check_eval_preserves "mask painting is rendering-time behavior" ~ctx
    "mask-image: linear-gradient(black, transparent)";
  check_eval_preserves "scroll timeline sampling is runtime behavior" ~ctx
    "animation-timeline: scroll()";
  check_eval_preserves "view timeline sampling is runtime behavior" ~ctx
    "view-timeline: --reveal block";
  check_eval_stylesheet "font loading stays a stylesheet syntax boundary" ~ctx
    ~expected:
      "@font-face { font-family: Brand; src: url(brand.woff2) \
       format(\"woff2\"); font-display: swap; }"
    "@font-face { font-family: Brand; src: url(brand.woff2) format(\"woff2\"); \
     font-display: swap; }";
  check_eval_stylesheet "keyframe timing stays runtime, values still eval" ~ctx
    ~expected:
      "@keyframes fade { from { opacity: 0; } to { opacity: 1; transform: \
       translateX(16px); } }"
    "@keyframes fade { from { opacity: 0; } to { opacity: 1; transform: \
     translateX(1rem); } }"

let animation_time_eval_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--duration: calc(500ms * 2)";
          Css.Declaration.of_string "--delay: calc(250ms * 2)";
          Css.Declaration.of_string "--distance: calc(1rem + 2px)";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_eval "eval folds animation-duration time calc" ~ctx
    ~expected:"animation-duration: 1s" "animation-duration: calc(500ms * 2)";
  check_eval "eval folds animation-delay time calc" ~ctx
    ~expected:"animation-delay: 0.5s" "animation-delay: calc(250ms * 2)";
  check_eval "eval folds transition-duration time calc" ~ctx
    ~expected:"transition-duration: 1s" "transition-duration: calc(250ms * 4)";
  check_eval "eval folds transition-delay time var" ~ctx
    ~expected:"transition-delay: 0.5s" "transition-delay: var(--delay)";
  check_eval "eval folds time slots inside animation shorthand" ~ctx
    ~expected:"animation: fade 1s linear 0.5s 2 alternate both running"
    "animation: fade var(--duration) linear var(--delay) 2 alternate both \
     running";
  check_eval "eval folds time slots inside transition shorthand" ~ctx
    ~expected:"transition: opacity 1s ease-in 0.5s"
    "transition: opacity var(--duration) ease-in var(--delay)";
  check_eval_preserves
    "animation-timeline is not sampled without a timeline context" ~ctx
    "animation-timeline: scroll()";
  check_eval_stylesheet "stylesheet eval reaches animation keyframe values" ~ctx
    ~expected:
      "@keyframes slide { from { transform: translateX(0); } to { transform: \
       translateX(18px); } }"
    "@keyframes slide { from { transform: translateX(0); } to { transform: \
     translateX(var(--distance)); } }";
  check_eval_stylesheet "stylesheet eval reaches time declarations in rules"
    ~ctx
    ~expected:
      ".fade{animation-duration:1s;animation-delay:0.5s;transition-duration:1s;transition-delay:0.5s}"
    ".fade{animation-duration:var(--duration);animation-delay:var(--delay);transition-duration:calc(250ms \
     * 4);transition-delay:var(--delay)}"

let eval_observable_matrix_contract () =
  let full_ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--gap: 1rem";
          Css.Declaration.of_string "--angle: 45deg";
          Css.Declaration.of_string "--duration: 500ms";
        ]
      ~inherited_values:
        [
          Css.Declaration.of_string "color: blue";
          Css.Declaration.of_string "font-size: 10px";
        ]
      ~initial_values:
        [
          Css.Declaration.of_string "display: inline";
          Css.Declaration.of_string "margin-left: 0";
          Css.Declaration.of_string "color: canvastext";
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 10.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1000.)
      ~viewport_height:(Css.Values.Px 500.)
      ~container_width:(Css.Values.Px 400.)
      ~container_height:(Css.Values.Px 300.) ()
  in
  let empty_ctx = Css.Context.empty in
  check_eval "observable custom property resolves present var" ~ctx:full_ctx
    ~expected:"margin-left: 16px" "margin-left: var(--gap)";
  check_eval_preserves "observable missing custom property stays residual"
    ~ctx:empty_ctx "margin-left: var(--missing)";
  check_eval "observable inherited value resolves inherit" ~ctx:full_ctx
    ~expected:"color: blue" "color: inherit";
  check_eval_preserves "observable absent inherited value preserves inherit"
    ~ctx:empty_ctx "color: inherit";
  check_eval "observable initial value resolves initial" ~ctx:full_ctx
    ~expected:"display: inline" "display: initial";
  check_eval_preserves "observable absent initial value preserves initial"
    ~ctx:empty_ctx "display: initial";
  check_eval "observable currentColor resolves present color" ~ctx:full_ctx
    ~expected:"border-color: blue" "border-color: currentColor";
  check_eval_preserves "observable absent currentColor preserves currentColor"
    ~ctx:empty_ctx "border-color: currentColor";
  check_eval "observable root font size resolves rem" ~ctx:full_ctx
    ~expected:"margin-left: 16px" "margin-left: 1rem";
  check_eval_preserves "observable absent root font size preserves rem"
    ~ctx:empty_ctx "margin-left: 1rem";
  check_eval "observable parent font size resolves em" ~ctx:full_ctx
    ~expected:"font-size: 20px" "font-size: 2em";
  check_eval_preserves "observable absent parent font size preserves em"
    ~ctx:empty_ctx "font-size: 2em";
  check_eval "observable viewport resolves vh" ~ctx:full_ctx
    ~expected:"height: 50px" "height: 10vh";
  check_eval_preserves "observable absent viewport preserves vh" ~ctx:empty_ctx
    "height: 10vh";
  check_eval "observable container resolves cqi" ~ctx:full_ctx
    ~expected:"width: 100px" "width: 25cqi";
  check_eval_preserves "observable absent container preserves cqi"
    ~ctx:empty_ctx "width: 25cqi";
  check_eval "observable base URL resolves relative URL" ~ctx:full_ctx
    ~expected:"background-image: url(https://example.test/img/a.png)"
    "background-image: url(../img/a.png)";
  check_eval_preserves "observable absent base URL preserves relative URL"
    ~ctx:empty_ctx "background-image: url(../img/a.png)";
  check_eval_preserves "observable absent cascade chain preserves revert-layer"
    ~ctx:empty_ctx "color: revert-layer";
  check_eval_preserves "observable absent cascade chain preserves revert"
    ~ctx:empty_ctx "color: revert";
  check_eval "observable angle calc folds with typed value" ~ctx:full_ctx
    ~expected:"rotate: 45deg" "rotate: var(--angle)";
  check_eval_preserves "observable absent angle var stays residual"
    ~ctx:empty_ctx "rotate: var(--angle)";
  check_eval "observable duration var folds with typed value" ~ctx:full_ctx
    ~expected:"animation-duration: 0.5s" "animation-duration: var(--duration)";
  check_eval_preserves "observable absent duration var stays residual"
    ~ctx:empty_ctx "animation-duration: var(--duration)"

let eval_stylesheet_spec_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--brand: red";
          Css.Declaration.of_string "--gap: calc(1rem + 2px)";
          Css.Declaration.of_string "--alpha: 0.5";
          Css.Declaration.of_string "--angle: calc(30deg + 15deg)";
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1000.) ()
  in
  check_eval_stylesheet
    "stylesheet eval walks nested rules, grouping at-rules, and keyframes" ~ctx
    ~expected:
      {|
        @import url(theme.css);
        @font-face { font-family: Brand; src: url(brand.woff2); }
        .card {
          color: red;
          margin-left: 18px;
          background-image: url(https://example.test/img/card.svg);
          & .title { border-color: red; padding: 18px; }
          @media (min-width: 40rem) { width: calc(100% - 16px); }
          @supports (display: grid) { transform: rotate(45deg); }
        }
        @starting-style { .card { opacity: 0.5; } }
        @keyframes fade {
          from { opacity: 0.5; }
          to { transform: translateX(18px); }
        }
        @page { margin: 18px; }
        @position-try --flip { inset-inline-start: 18px; }
      |}
    {|
        @import url(theme.css);
        @font-face { font-family: Brand; src: url(brand.woff2); }
        .card {
          color: var(--brand);
          margin-left: var(--gap);
          background-image: url(../img/card.svg);
          & .title { border-color: currentColor; padding: var(--gap); }
          @media (min-width: 40rem) { width: calc(100% - 1rem); }
          @supports (display: grid) { transform: rotate(var(--angle)); }
        }
        @starting-style { .card { opacity: var(--alpha); } }
        @keyframes fade {
          from { opacity: var(--alpha); }
          to { transform: translateX(var(--gap)); }
        }
        @page { margin: var(--gap); }
        @position-try --flip { inset-inline-start: var(--gap); }
      |};
  check_eval_stylesheet_preserves_structure
    "stylesheet eval preserves non-declaration structure" ~ctx
    {|
      @import url(theme.css);
      @layer theme, components, utilities;
      @layer components {
        .card {
          color: var(--brand);
          & .title { border-color: currentColor; padding: var(--gap); }
          @media (min-width: 40rem) { width: calc(100% - 1rem); }
          @supports (display: grid) { transform: rotate(var(--angle)); }
        }
      }
      @scope (.card) to (.boundary) {
        .title { color: var(--brand); }
      }
      @starting-style { .card { opacity: var(--alpha); } }
      @keyframes fade {
        from { opacity: var(--alpha); }
        to { transform: translateX(var(--gap)); }
      }
      @page { margin: var(--gap); }
      @position-try --flip { inset-inline-start: var(--gap); }
    |}

let eval_laws_contract () =
  let weak_ctx =
    Css.Context.v ~root_font_size:(Css.Values.Px 16.)
      ~parent_font_size:(Css.Values.Px 12.) ()
  in
  let strong_ctx =
    Css.Context.v ~root_font_size:(Css.Values.Px 16.)
      ~parent_font_size:(Css.Values.Px 12.)
      ~viewport_height:(Css.Values.Px 1000.) ()
  in
  check_eval_idempotent "eval is idempotent on reduced calc AST" ~ctx:weak_ctx
    "height: calc(1rem + 10vh)";
  check_eval_context_extension
    "eval with stronger context equals staged weaker-then-stronger eval"
    ~weak_ctx ~strong_ctx ~expected:"height: 116px" "height: calc(1rem + 10vh)";
  check_eval_context_extension
    "eval context extension recurses through nested calc AST" ~weak_ctx
    ~strong_ctx ~expected:"height: 124px" "height: calc((1rem + 8px) + 10vh)"

let layered_eval_ast_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let custom_properties =
    [
      Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
      Css.Declaration.custom_property ~layer:"base" "--base-gap"
        "calc(var(--spacing) * 2)";
      Css.Declaration.custom_property ~layer:"components" "--component-gap"
        "calc(var(--base-gap) + 1rem)";
      Css.Declaration.custom_property ~layer:"utilities" "--utility-gap"
        "calc(var(--component-gap) + 2vh)";
      Css.Declaration.custom_property ~layer:"utilities" "--future-gap"
        "calc(var(--component-gap) + var(--user-gap))";
      Css.Declaration.custom_property ~layer:"utilities" "--component-gap"
        "revert-layer";
      Css.Declaration.custom_property ~layer:"utilities" "--color-brand"
        "revert-layer";
      Css.Declaration.custom_property ~layer:"base" "--color-brand"
        "oklch(60% 0.12 250)";
      Css.Declaration.custom_property ~layer:"theme" "--color-brand"
        "oklch(70% 0.15 250)";
    ]
  in
  let weak_ctx =
    Css.Context.v ~custom_properties
      ~initial_values:
        [
          Css.Declaration.of_string "color: canvastext";
          Css.Declaration.of_string "margin-left: 0";
        ]
      ~inherited_values:
        [ Css.Declaration.of_string "color: var(--color-brand)" ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  let strong_ctx =
    Css.Context.v ~custom_properties
      ~initial_values:
        [
          Css.Declaration.of_string "color: canvastext";
          Css.Declaration.of_string "margin-left: 0";
        ]
      ~inherited_values:
        [ Css.Declaration.of_string "color: var(--color-brand)" ]
      ~root_font_size:(Css.Values.Px 16.) ~viewport_height:(Css.Values.Px 1000.)
      ()
  in
  check_eval "eval resolves layered calc across all cascade layers"
    ~ctx:strong_ctx ~layer_order ~layer:"utilities"
    ~expected:"margin-left: 44px" "margin-left: var(--utility-gap)";
  check_eval "eval preserves unknown viewport leaf after layered var resolution"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    ~expected:"margin-left: calc(24px + 2vh)" "margin-left: var(--utility-gap)";
  check_eval "eval preserves abstract var leaf after known layers resolve"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    ~expected:"margin-left: calc(24px + var(--user-gap))"
    "margin-left: var(--future-gap)";
  check_eval "eval resolves custom property revert-layer before calc eval"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities" ~expected:"padding: 24px"
    "padding: var(--component-gap)";
  check_eval "eval resolves layered color revert through inherited value"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    ~expected:"color: oklch(60% 0.12 250)" "color: revert-layer";
  check_eval "eval resolves theme revert-layer to property initial value"
    ~ctx:weak_ctx ~layer_order ~layer:"theme" ~expected:"margin-left: 0"
    "margin-left: revert-layer";
  check_eval_idempotent "layered eval is idempotent on symbolic AST"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    "margin-left: var(--utility-gap)";
  check_eval_context_extension
    "layered eval context extension reaches the same final AST" ~layer_order
    ~layer:"utilities" ~weak_ctx ~strong_ctx ~expected:"margin-left: 44px"
    "margin-left: var(--utility-gap)"

let test_document_selector_function_contract () =
  let ctx =
    Css.Context.document ~element:"button" ~classes:[ "btn"; "primary" ]
      ~ids:[ "submit" ]
      ~attributes:[ ("aria-expanded", Some "true"); ("disabled", None) ]
      ~pseudo_classes:[ "enabled"; "focus-visible" ]
      ~pseudo_elements:[ "before" ] ()
  in
  check_selector_match ":is matches one matching branch" ~ctx ~expected:true
    ":is(.link, .btn)";
  check_selector_match ":where matches without changing context semantics" ~ctx
    ~expected:true ":where(button.primary)";
  check_selector_match ":not matches absent selector" ~ctx ~expected:true
    ":not(.secondary)";
  check_selector_match ":not fails when inner selector matches" ~ctx
    ~expected:false ":not(.btn)";
  check_selector_match "compound with :is and attribute matches" ~ctx
    ~expected:true "button:is(.primary)[aria-expanded=true]";
  check_selector_match "compound with :where and pseudo-class matches" ~ctx
    ~expected:true ":where(button.btn):enabled";
  check_selector_match "pseudo-element compound matches explicit element" ~ctx
    ~expected:true "button::before";
  check_selector_match "attribute exact value is case-sensitive by default" ~ctx
    ~expected:false "[aria-expanded=True]"

let test_query_context_boolean_contract () =
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:
        [
          feature "width" (px 1024.);
          feature "height" (px 768.);
          feature "orientation" (ident "landscape");
          feature "hover" (ident "hover");
          feature "pointer" (ident "fine");
        ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "display" "flex";
          Css.Supports.property "color" "lab(50% 0 0)";
          Css.Supports.func "selector" ":is(.a, .b)";
          Css.Supports.func "font-format" "woff2";
        ]
      ~container_name:"card"
      ~container_features:
        [
          container_feature "inline-size" (px 720.);
          container_feature "block-size" (px 360.);
          Css.Container.style ~value:"compact" "--density";
        ]
      ()
  in
  check_media_match "media and chain matches" ~ctx ~expected:true
    "screen and (width >= 60em) and (orientation: landscape)";
  check_media_match "media comma list matches any branch" ~ctx ~expected:true
    "print, screen and (hover: hover)";
  check_media_match "media comma list fails all branches" ~ctx ~expected:false
    "print, speech";
  check_media_match "media pointer feature matches" ~ctx ~expected:true
    "(pointer: fine)";
  check_supports_match "supports or matches one branch" ~ctx ~expected:true
    "(display: ruby) or (display: grid)";
  check_supports_match "supports and fails when one branch fails" ~ctx
    ~expected:false "(display: grid) and (display: ruby)";
  check_supports_match "supports selector function exact match" ~ctx
    ~expected:true "selector(:is(.a, .b))";
  check_supports_match "unsupported function argument fails" ~ctx
    ~expected:false "selector(:has(img))";
  check_container_match "container block axis range matches" ~ctx ~name:"card"
    ~expected:true "(block-size >= 20rem)";
  check_container_match "container boolean and matches" ~ctx ~name:"card"
    ~expected:true "((inline-size >= 40rem) and style(--density: compact))";
  check_container_match "container boolean and fails one branch" ~ctx
    ~name:"card" ~expected:false
    "((inline-size >= 40rem) and style(--density: spacious))"

let test_loader_import_condition_contract () =
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:
        [
          ("https://example.test/css/grid.css", ".grid{display:grid}");
          ("https://example.test/css/print.css", ".print{display:block}");
          ("https://example.test/css/lab.css", ".lab{color:lab(50% 0 0)}");
        ]
      ()
  in
  let query =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ feature "width" (px 1024.) ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "color" "lab(50% 0 0)";
        ]
      ()
  in
  check_import_loaded "import loads when media and supports match" ~query
    ~loader ~expected:".grid{display:grid}"
    "@import url(grid.css) supports(display: grid) screen and (width >= 40em);";
  check_import_loaded "import loads when supports color function matches"
    ~query
      (* check_import_loaded is to_string ~minify with no optimize, so pp holds
         the lab() unfolded; lab(50%0 0) is valid minified output (% is a token
         boundary). Folding lab to #777 is an optimize transform. *)
    ~loader ~expected:".lab{color:lab(50%0 0)}"
    "@import url(lab.css) supports(color: lab(50% 0 0));";
  check_import_error "import blocked by unmatched media" ~query ~loader
    "@import url(print.css) print;";
  check_import_error "import blocked by unmatched supports" ~query ~loader
    "@import url(grid.css) supports(display: ruby);"

let layered_vars_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--color-brand-500"
            "oklch(70% 0.15 250)";
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"theme" "--font-sans"
            "Inter, sans-serif";
          Css.Declaration.custom_property ~layer:"base" "--color-brand-500"
            "oklch(60% 0.12 250)";
          Css.Declaration.custom_property ~layer:"components" "--card-padding"
            "calc(var(--spacing) * 6)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-translate-x"
            "calc(var(--spacing) * 4)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-bg-opacity"
            "0.5";
          Css.Declaration.custom_property "--color-brand-500"
            "oklch(40% 0.10 250)";
          Css.Declaration.important
            (Css.Declaration.custom_property ~layer:"utilities"
               "--tw-ring-color" "red");
          Css.Declaration.important
            (Css.Declaration.custom_property ~layer:"theme" "--tw-ring-color"
               "blue");
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value
    "tailwind theme variable is visible to utility declarations" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"oklch(70% 0.15 250)"
    "color: var(--color-brand-500)";
  check_layered_eval_value
    "normal unlayered app variable overrides tailwind layered tokens" ~ctx
    ~layer_order ~expected:"oklch(40% 0.10 250)" "color: var(--color-brand-500)";
  check_layered_eval_value
    "base layer token overrides theme for base-scoped resolution" ~ctx
    ~layer_order ~layer:"base" ~expected:"oklch(60% 0.12 250)"
    "color: var(--color-brand-500)";
  check_layered_eval_value
    "component token resolves through tailwind theme spacing" ~ctx ~layer_order
    ~layer:"components" ~expected:"24px" "padding: var(--card-padding)";
  check_layered_eval_value
    "utility-local --tw variable resolves through theme spacing" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"16px"
    "translate: var(--tw-translate-x)";
  check_layered_eval_value "utility-local opacity variable resolves as a number"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"0.5"
    "opacity: var(--tw-bg-opacity)";
  check_layered_eval_value
    "important tailwind theme layer beats important utility layer" ~ctx
    ~layer_order ~expected:"blue" "outline-color: var(--tw-ring-color)";
  check_layered_eval_preserves "unknown layer is rejected for scoped resolution"
    ~ctx ~layer_order ~layer:"unknown" "color: var(--color-brand-500)"

let test_layered_computed_revert_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~initial_values:
        [
          Css.Declaration.of_string "color: canvastext";
          Css.Declaration.of_string "margin-left: 0";
        ]
      ~inherited_values:
        [ Css.Declaration.of_string "color: var(--color-brand-500)" ]
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--color-brand-500"
            "oklch(70% 0.15 250)";
          Css.Declaration.custom_property ~layer:"base" "--color-brand-500"
            "oklch(60% 0.12 250)";
          Css.Declaration.custom_property ~layer:"utilities" "--color-brand-500"
            "revert-layer";
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"components" "--card-padding"
            "calc(var(--spacing) * 6)";
          Css.Declaration.custom_property ~layer:"utilities" "--card-padding"
            "revert-layer";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value "revert-layer utility token rolls back to base token"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"oklch(60% 0.12 250)"
    "color: var(--color-brand-500)";
  check_layered_eval_value
    "revert-layer utility spacing rolls back to component token then theme \
     token"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"24px"
    "padding: var(--card-padding)";
  check_layered_eval_value
    "property revert-layer rolls back to lower cascaded layer" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"oklch(60% 0.12 250)" "color: revert-layer";
  check_layered_eval_value
    "property revert-layer in theme falls back to initial" ~ctx ~layer_order
    ~layer:"theme" ~expected:"0" "margin-left: revert-layer";
  check_layered_eval_preserves "revert-layer cycle is unresolved" ~ctx
    ~layer_order ~layer:"utilities" "margin-left: var(--missing, revert-layer)"

let test_loader_import_layer_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:
        [
          ( "https://example.test/css/theme.css",
            ":root{--color-brand-500:oklch(70% 0.15 250);--spacing:0.25rem}" );
          ("https://example.test/css/base.css", "*,::before,::after{margin:0}");
          ("https://example.test/css/components.css", ".card{padding:1rem}");
          ("https://example.test/css/utilities.css", ".text-brand{color:red}");
        ]
      ()
  in
  let query =
    Css.Context.query ~media_type:"screen"
      ~supports:[ Css.Supports.property "display" "grid" ]
      ~media_features:[ feature "width" (px 1024.) ]
      ()
  in
  check_layered_import_loaded "tailwind theme import enters theme layer" ~loader
    ~layer_order
    ~expected:
      "@layer theme{:root{--color-brand-500:oklch(70%.15 \
       250);--spacing:.25rem}}"
    "@import url(theme.css) layer(theme);";
  check_layered_import_loaded "tailwind base import enters base layer" ~loader
    ~layer_order ~expected:"@layer base{*,:before,:after{margin:0}}"
    "@import url(base.css) layer(base);";
  check_layered_import_loaded
    "tailwind components import keeps layer when query context matches" ~query
    ~loader ~layer_order ~expected:"@layer components{.card{padding:1rem}}"
    "@import url(components.css) layer(components) supports(display: grid) \
     screen and (width >= 40em);";
  check_layered_import_loaded "tailwind utilities import enters final layer"
    ~loader ~layer_order ~expected:"@layer utilities{.text-brand{color:red}}"
    "@import url(utilities.css) layer(utilities);";
  check_layered_import_error
    "conditional layered import blocked before layer insertion" ~query ~loader
    ~layer_order
    "@import url(components.css) layer(components) supports(display: flex);";
  check_layered_import_error "unknown layer name is rejected by layer context"
    ~loader ~layer_order "@import url(theme.css) layer(experimental);"

let tw_vars_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--color-red-500"
            "239 68 68";
          Css.Declaration.custom_property ~layer:"theme" "--color-blue-500"
            "59 130 246";
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"theme" "--radius-lg" "0.5rem";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-bg-opacity"
            "0.75";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-border-opacity" "0.5";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-gradient-from" "rgb(var(--color-red-500) / 1)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-gradient-to"
            "rgb(var(--color-blue-500) / 0)";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-gradient-stops"
            "var(--tw-gradient-from), var(--tw-gradient-to)";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-ring-offset-width" "2px";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-ring-offset-color" "#fff";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-ring-color"
            "rgb(var(--color-blue-500) / 0.5)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-translate-x"
            "calc(var(--spacing) * 4)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-rotate"
            "45deg";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-scale-x" "1";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-scale-y" "1";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value
    "tailwind rgb slash alpha resolves theme channel and opacity var" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"rgb(239 68 68/.75)"
    "background-color: rgb(var(--color-red-500) / var(--tw-bg-opacity))";
  check_layered_eval_value "tailwind border opacity var resolves independently"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"rgb(59 130 246/.5)"
    "border-color: rgb(var(--color-blue-500) / var(--tw-border-opacity))";
  check_layered_eval_value
    "tailwind gradient stops resolve chained utility variables" ~ctx
    ~layer_order ~layer:"utilities"
    ~expected:"linear-gradient(to right,rgb(239 68 68/1),rgb(59 130 246/0))"
    "background-image: linear-gradient(to right, var(--tw-gradient-stops))";
  check_layered_eval_value
    "tailwind ring shadow resolves offset and color variables" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"0 0 0 2px #fff,0 0 0 5px rgb(59 130 246/.5)"
    "box-shadow: 0 0 0 var(--tw-ring-offset-width) \
     var(--tw-ring-offset-color), 0 0 0 calc(3px + \
     var(--tw-ring-offset-width)) var(--tw-ring-color)";
  check_layered_eval_value
    "tailwind transform utility variables resolve in order" ~ctx ~layer_order
    ~layer:"utilities"
    ~expected:"translate(16px) rotate(45deg) scaleX(1) scaleY(1)"
    "transform: translate(var(--tw-translate-x)) rotate(var(--tw-rotate)) \
     scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))";
  check_layered_eval_value
    "tailwind radius token resolves through theme variable" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"8px" "border-radius: var(--radius-lg)";
  (* CSS Values 4: once the fallback var() has selected the calc() and --spacing
     has resolved to a rem length, computed-value simplification has enough
     information to reduce the expression to px. *)
  check_layered_eval_value
    "tailwind nested fallback uses theme token when utility var is absent" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"16px"
    "margin-left: var(--tw-space-x, calc(var(--spacing) * 4))";
  check_layered_eval_preserves
    "tailwind utility var without fallback remains unresolved" ~ctx ~layer_order
    ~layer:"utilities" "outline-width: var(--tw-outline-width)"

let selector_scope_contract () =
  let scoped =
    Css.Context.document ~root:"html"
      ~scope:(Css.Selector.of_string ".card")
      ~element:"section"
      ~classes:[ "card"; "group"; "is-open" ]
      ~ids:[ "billing" ]
      ~attributes:[ ("data-theme", Some "dark"); ("dir", Some "ltr") ]
      ~pseudo_classes:[ "focus-within" ] ()
  in
  check_selector_match ":root matches explicit root context" ~ctx:scoped
    ~expected:true ":root";
  check_selector_match ":scope matches explicit scope element" ~ctx:scoped
    ~expected:true ":scope";
  check_selector_match "scoped class compound matches" ~ctx:scoped
    ~expected:true ":scope.card[data-theme=dark]";
  check_selector_match "scope mismatch fails" ~ctx:scoped ~expected:false
    ":scope.modal";
  check_selector_match "dir attribute selector matches explicit context"
    ~ctx:scoped ~expected:true "[dir=ltr]";
  check_selector_match "group state selector matches explicit class/state"
    ~ctx:scoped ~expected:true ".group:focus-within";
  check_selector_match "missing group state fails" ~ctx:scoped ~expected:false
    ".group:hover";
  let rootless =
    Css.Context.document ~element:"section" ~classes:[ "card" ] ()
  in
  check_selector_match ":root fails without root context" ~ctx:rootless
    ~expected:false ":root";
  check_selector_match ":scope fails without scope context" ~ctx:rootless
    ~expected:false ":scope"

let tw_layer_order_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"components" "--spacing"
            "0.5rem";
          Css.Declaration.custom_property ~layer:"utilities" "--spacing" "1rem";
          Css.Declaration.custom_property ~layer:"theme" "--color-brand" "red";
          Css.Declaration.custom_property ~layer:"components" "--color-brand"
            "blue";
          Css.Declaration.custom_property ~layer:"utilities" "--color-brand"
            "green";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value
    "tailwind utility layer token wins within utility layer" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"16px" "margin-left: var(--spacing)";
  check_layered_eval_value
    "tailwind component layer token wins within component layer" ~ctx
    ~layer_order ~layer:"components" ~expected:"8px"
    "margin-left: var(--spacing)";
  check_layered_eval_value "tailwind theme layer token wins within theme layer"
    ~ctx ~layer_order ~layer:"theme" ~expected:"4px"
    "margin-left: var(--spacing)";
  check_layered_eval_value
    "tailwind unscoped normal lookup uses final layered token" ~ctx ~layer_order
    ~expected:"green" "color: var(--color-brand)";
  check_layered_eval_value
    "tailwind component scoped lookup ignores later utility token" ~ctx
    ~layer_order ~layer:"components" ~expected:"blue"
    "color: var(--color-brand)"

let cascade_rule_resolver_contract () =
  let layer_order = [ "reset"; "theme"; "components"; "utilities" ] in
  let value_ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--theme-color: oklch(70% 0.15 250)";
          Css.Declaration.of_string "--app-color: oklch(40% 0.10 250)";
          Css.Declaration.of_string "--scoped-color: green";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  let query =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ feature "width" (px 1024.) ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "container-type" "inline-size";
        ]
      ~container_name:"card"
      ~container_features:[ container_feature "inline-size" (px 640.) ]
      ()
  in
  let primary =
    Css.Context.document ~element:"button" ~classes:[ "btn"; "primary" ] ()
  in
  check_resolved_property "unlayered rule beats matching explicit layers"
    ~layer_order ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: oklch(40% 0.10 250)"
    {|
      @layer reset, theme, components, utilities;
      @layer theme { .btn { color: var(--theme-color); } }
      @layer utilities { .btn { color: orange; } }
      @media screen and (width >= 40rem) { .btn { color: blue; } }
      @supports (display: ruby) { .btn.primary { color: red; } }
      @supports (display: grid) { .btn.primary { color: var(--app-color); } }
    |};
  let id_button =
    Css.Context.document ~element:"button" ~classes:[ "btn"; "primary" ]
      ~ids:[ "submit" ] ()
  in
  check_resolved_property
    "specificity chooses id selector after condition filtering" ~layer_order
    ~ctx:value_ctx ~document:id_button ~query ~property:"color"
    ~expected:"color: purple"
    {|
      .btn.primary { color: var(--app-color); }
      #submit { color: purple; }
      @supports (display: ruby) { #submit { color: red; } }
    |};
  check_resolved_property "important layer order reverses normal layer order"
    ~layer_order ~ctx:value_ctx ~document:primary ~query
    ~property:"border-color" ~expected:"border-color: red"
    {|
      @layer reset, theme, components, utilities;
      .btn { border-color: green !important; }
      @layer reset { .btn { border-color: red !important; } }
      @layer utilities { .btn { border-color: blue !important; } }
    |};
  check_resolved_property
    "revert-layer rolls back to the next lower normal layer" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: green"
    {|
      @layer reset, theme, components, utilities;
      @layer theme { .btn { color: red; } }
      @layer components { .btn { color: green; } }
      @layer utilities { .btn { color: revert-layer; } }
    |};
  let scoped =
    Css.Context.document
      ~scope:(Css.Selector.of_string ".card")
      ~element:"h2" ~classes:[ "title" ] ()
  in
  check_resolved_property
    "scope proximity beats later source order at equal specificity" ~layer_order
    ~ctx:value_ctx ~document:scoped ~query ~property:"color"
    ~expected:"color: green"
    {|
      @scope (.card) { .title { color: var(--scoped-color); } }
      .title { color: blue; }
    |};
  check_resolved_property
    "scope boundary suppresses declarations outside the active scope"
    ~layer_order ~ctx:value_ctx
    ~document:
      (Css.Context.document
         ~scope:(Css.Selector.of_string ".card")
         ~element:"h2" ~classes:[ "title"; "boundary" ] ())
    ~query ~property:"color" ~expected:"color: blue"
    {|
      @scope (.card) to (.boundary) { .title { color: green; } }
      .title { color: blue; }
    |};
  let ast_rule selector declarations =
    Css.rule
      ~selector:(Css.Selector.of_string selector)
      (List.map Css.Declaration.of_string declarations)
  in
  let origin_sheet =
    [
      Css.Stylesheet.with_origin Css.Stylesheet.User_agent
        [ ast_rule ".btn" [ "color: black" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.Author
        [ ast_rule ".btn" [ "color: red" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.User
        [ ast_rule ".btn" [ "color: blue !important" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.Transition
        [ ast_rule ".btn" [ "color: yellow" ] ];
    ]
  in
  check_ast_resolved_property
    "origin and animation/transition ranks are applied" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: yellow" origin_sheet;
  let revert_origin_sheet =
    [
      Css.Stylesheet.with_origin Css.Stylesheet.User_agent
        [ ast_rule ".btn" [ "color: black" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.User
        [ ast_rule ".btn" [ "color: blue" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.Author
        [ ast_rule ".btn" [ "color: revert" ] ];
    ]
  in
  check_ast_resolved_property
    "revert rolls back to the next lower cascade origin" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: blue" revert_origin_sheet;
  check_resolved_property "nested conditional declarations inherit parent rule"
    ~layer_order ~ctx:value_ctx ~document:primary ~query
    ~property:"outline-color" ~expected:"outline-color: oklch(40% 0.10 250)"
    {|
      .btn {
        @media screen and (width >= 40rem) {
          outline-color: var(--app-color);
        }
        @supports (display: ruby) {
          outline-color: red;
        }
      }
    |};
  check_resolved_property
    "source order breaks ties after equal origin layer specificity and scope"
    ~layer_order ~ctx:value_ctx ~document:primary ~query
    ~property:"background-color" ~expected:"background-color: blue"
    {|
      .btn { background-color: red; }
      .btn { background-color: blue; }
    |};
  check_resolved_property
    "normal named layer order follows the declared layer statement" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"outline-color"
    ~expected:"outline-color: blue"
    {|
      @layer reset, theme, components, utilities;
      @layer utilities { .btn { outline-color: blue; } }
      @layer theme { .btn { outline-color: red; } }
    |}

let runtime_var_not_folded_contract () =
  let open Css.Values in
  (* A [~runtime:true] theme var keeps its var() reference even when its default
     is known, so a script can still override --spacing at runtime. *)
  let _decl, spacing = Css.var ~runtime:true "spacing" Length (Px 4.) in
  let decl = Css.width (Calc (Expr (Var spacing, Mul, Num 4.))) in
  Alcotest.(check decl_t)
    "runtime var stays a var() in calc" decl
    (Css.eval_declaration Css.Context.empty decl);
  (* Without [~runtime] the same calc folds to a constant through the
     default. *)
  let _decl, fixed = Css.var "fixed-spacing" Length (Px 4.) in
  let folding = Css.width (Calc (Expr (Var fixed, Mul, Num 4.))) in
  Alcotest.(check bool)
    "non-runtime var folds" false
    (String.equal
       (Css.Declaration.to_string folding)
       (Css.Declaration.to_string
          (Css.eval_declaration Css.Context.empty folding)));
  (* Multiplicative identities never read the variable's value, so they still
     simplify a runtime var; only value resolution ([* n], n >= 2) is
     skipped. *)
  let eval c =
    Css.Declaration.to_string ~minify:true
      (Css.eval_declaration Css.Context.empty (Css.width (Calc c)))
  in
  Alcotest.(check string)
    "runtime var * 0 folds to 0" "width:0"
    (eval (Expr (Var spacing, Mul, Num 0.)));
  Alcotest.(check string)
    "runtime var * 1 folds to the operand" "width:var(--spacing)"
    (eval (Expr (Var spacing, Mul, Num 1.)));
  Alcotest.(check string)
    "runtime var * 4 keeps the var() reference" "width:calc(var(--spacing)*4)"
    (eval (Expr (Var spacing, Mul, Num 4.)));
  (* The spacing utilities multiply the theme var, so the [* 1] case must reach
     the operand through a property shorthand too: [p-N] for N=1 is [padding:
     var(--spacing)], not [calc(var(--spacing) * 1)]. *)
  Alcotest.(check string)
    "runtime var * 1 in the padding shorthand folds to the operand"
    "padding:var(--spacing)"
    (Css.Declaration.to_string ~minify:true
       (Css.eval_declaration Css.Context.empty
          (Css.padding [ Calc (Expr (Var spacing, Mul, Num 1.)) ])))

let suite =
  ( "context",
    [
      Alcotest.test_case "runtime var not folded" `Quick
        runtime_var_not_folded_contract;
      Alcotest.test_case "empty property value context" `Quick
        test_empty_property_value;
      Alcotest.test_case "empty side contexts" `Quick test_empty_side_contexts;
      Alcotest.test_case "property value lookups" `Quick
        test_property_value_lookup;
      Alcotest.test_case "property value lookup boundaries" `Quick
        test_property_value_lookup_boundaries;
      Alcotest.test_case "document context" `Quick test_document_context;
      Alcotest.test_case "document context boundaries" `Quick
        test_document_context_boundaries;
      Alcotest.test_case "query context" `Quick test_query_context;
      Alcotest.test_case "query context boundaries" `Quick
        test_query_context_boundaries;
      Alcotest.test_case "loader and animation contexts" `Quick
        test_loader_and_animation_contexts;
      Alcotest.test_case "loader and animation boundaries" `Quick
        test_loader_and_animation_boundaries;
      Alcotest.test_case "context debug printers" `Quick
        test_context_debug_printers;
      Alcotest.test_case "computed value resolution contract" `Quick
        test_computed_value_resolution_contract;
      Alcotest.test_case "computed value edge contract" `Quick
        computed_edge_contract;
      Alcotest.test_case "document selector context contract" `Quick
        test_document_selector_context_contract;
      Alcotest.test_case "query context evaluation contract" `Quick
        test_query_context_evaluation_contract;
      Alcotest.test_case "media boolean values" `Quick media_boolean_values;
      Alcotest.test_case "media unknown logic" `Quick media_unknown_logic;
      Alcotest.test_case "media resolution units" `Quick media_resolution_units;
      Alcotest.test_case "loader context contract" `Quick
        test_loader_context_contract;
      Alcotest.test_case "animation context contract" `Quick
        test_animation_context_contract;
      Alcotest.test_case "property registration context contract" `Quick
        test_property_registration_context_contract;
      Alcotest.test_case "computed value calc and shorthand contract" `Quick
        computed_calc_contract;
      Alcotest.test_case "eval calc family contract" `Quick
        eval_calc_family_contract;
      Alcotest.test_case "eval AST contract" `Quick eval_ast_contract;
      Alcotest.test_case "eval spec edge contract" `Quick
        eval_spec_edge_contract;
      Alcotest.test_case "eval CSS spec edge contract" `Quick
        eval_css_spec_edge_contract;
      Alcotest.test_case "runtime boundary contract" `Quick
        runtime_boundary_contract;
      Alcotest.test_case "animation time eval contract" `Quick
        animation_time_eval_contract;
      Alcotest.test_case "eval observable matrix contract" `Quick
        eval_observable_matrix_contract;
      Alcotest.test_case "eval stylesheet spec edge contract" `Quick
        eval_stylesheet_spec_edge_contract;
      Alcotest.test_case "eval laws contract" `Quick eval_laws_contract;
      Alcotest.test_case "layered eval AST contract" `Quick
        layered_eval_ast_contract;
      Alcotest.test_case "document selector function contract" `Quick
        test_document_selector_function_contract;
      Alcotest.test_case "query context boolean contract" `Quick
        test_query_context_boolean_contract;
      Alcotest.test_case "loader import condition contract" `Quick
        test_loader_import_condition_contract;
      Alcotest.test_case "layered custom property context contract" `Quick
        layered_vars_contract;
      Alcotest.test_case "layered computed revert contract" `Quick
        test_layered_computed_revert_contract;
      Alcotest.test_case "loader import layer contract" `Quick
        test_loader_import_layer_contract;
      Alcotest.test_case "tailwind variable layer contract" `Quick
        tw_vars_contract;
      Alcotest.test_case "selector scope context contract" `Quick
        selector_scope_contract;
      Alcotest.test_case "tailwind layer order contract" `Quick
        tw_layer_order_contract;
      Alcotest.test_case "cascade rule resolver contract" `Quick
        cascade_rule_resolver_contract;
    ] )
