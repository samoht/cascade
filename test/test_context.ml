(** Tests for explicit CSS transform contexts. *)

open Cascade

let decl_t : Css.Declaration.declaration Alcotest.testable =
  Alcotest.testable
    (fun fmt d ->
      Format.pp_print_string fmt (Css.Declaration.string_of_declaration d))
    ( = )

let matches pattern text =
  let re = Re.Perl.compile_pat pattern in
  Re.execp re text

let check_matches name pattern text =
  Alcotest.(check bool) name true (matches pattern text)

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
  Alcotest.(check (option string)) "document scope" None document.scope;
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
  Alcotest.(check (list (pair string string)))
    "query media features" [] query.media_features;
  Alcotest.(check (list (pair string string)))
    "query supports declarations" [] query.supports_declarations;
  Alcotest.(check (list (pair string string)))
    "query supports functions" [] query.supports_functions;
  Alcotest.(check (option string))
    "query container name" None query.container_name;
  Alcotest.(check (list (pair string string)))
    "query container features" [] query.container_features;
  Alcotest.(check (option string))
    "empty query has no media feature" None
    (Css.Context.media_feature "width" query);
  Alcotest.(check bool)
    "empty query supports no declaration" false
    (Css.Context.supports_declaration ~property:"display" ~value:"grid" query);
  Alcotest.(check (option string))
    "empty query has no container feature" None
    (Css.Context.container_feature "inline-size" query);
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
    Css.Context.document ~root:"html" ~scope:".card" ~element:"button"
      ~classes:[ "btn"; "primary" ] ~ids:[ "submit" ]
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
    Css.Context.document ~root:":root" ~scope:".dialog" ~element:"input"
      ~classes:[ "field"; "is-invalid" ] ~ids:[ "email" ]
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
  Alcotest.(check (option string)) "scope preserved" (Some ".dialog") ctx.scope;
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
      ~media_features:[ ("width", "1024px"); ("dynamic-range", "high") ]
      ~supports_declarations:
        [ ("display", "grid"); ("container-type", "inline-size") ]
      ~supports_functions:[ ("selector", ":has(img)") ]
      ~container_name:"card"
      ~container_features:
        [ ("inline-size", "640px"); ("style(--theme)", "dark") ]
      ()
  in
  Alcotest.(check (option string))
    "media feature" (Some "1024px")
    (Css.Context.media_feature "width" ctx);
  Alcotest.(check bool)
    "supported declaration" true
    (Css.Context.supports_declaration ~property:"display" ~value:"grid" ctx);
  Alcotest.(check bool)
    "unsupported declaration" false
    (Css.Context.supports_declaration ~property:"display" ~value:"ruby" ctx);
  Alcotest.(check (option string))
    "container feature" (Some "640px")
    (Css.Context.container_feature "inline-size" ctx)

let test_query_context_boundaries () =
  let ctx =
    Css.Context.query ~media_type:"print"
      ~media_features:[ ("width", "80rem"); ("prefers-color-scheme", "dark") ]
      ~supports_declarations:[ ("display", "grid"); ("selector", ":has(img)") ]
      ~supports_functions:
        [ ("selector", ":has(img)"); ("font-tech", "color-COLRv1") ]
      ~container_name:"sidebar"
      ~container_features:
        [ ("inline-size", "42rem"); ("style(--theme)", "dark") ]
      ()
  in
  Alcotest.(check (option string))
    "media type preserved" (Some "print") ctx.media_type;
  Alcotest.(check (list (pair string string)))
    "supports functions preserved"
    [ ("selector", ":has(img)"); ("font-tech", "color-COLRv1") ]
    ctx.supports_functions;
  Alcotest.(check (option string))
    "container name preserved" (Some "sidebar") ctx.container_name;
  Alcotest.(check (option string))
    "missing media feature" None
    (Css.Context.media_feature "height" ctx);
  Alcotest.(check bool)
    "supports declaration is exact on value" false
    (Css.Context.supports_declaration ~property:"display" ~value:"flex" ctx);
  Alcotest.(check bool)
    "supports declaration is exact on property" false
    (Css.Context.supports_declaration ~property:"Display" ~value:"grid" ctx);
  Alcotest.(check (option string))
    "style container feature" (Some "dark")
    (Css.Context.container_feature "style(--theme)" ctx);
  Alcotest.(check (option string))
    "missing container feature" None
    (Css.Context.container_feature "block-size" ctx)

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
    Css.Context.document ~root:"html" ~scope:".card" ~element:"button"
      ~classes:[ "btn" ] ~ids:[ "submit" ]
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
      ~media_features:[ ("width", "1024px") ]
      ~supports_declarations:[ ("display", "grid") ]
      ~supports_functions:[ ("selector", ":has(img)") ]
      ~container_name:"card"
      ~container_features:[ ("inline-size", "640px") ]
      ()
    |> Css.Pp.to_string Css.Context.pp_query
  in
  check_matches "query dump has supports function" "supports_functions=.*:has"
    query_dump;
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

let suite =
  ( "context",
    [
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
    ] )
