(** Tests for explicit CSS transform contexts. *)

open Cascade

let decl_t : Css.Declaration.declaration Alcotest.testable =
  Alcotest.testable
    (fun fmt d ->
      Format.pp_print_string fmt (Css.Declaration.string_of_declaration d))
    ( = )

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

let suite =
  ( "context",
    [
      Alcotest.test_case "empty property value context" `Quick
        test_empty_property_value;
      Alcotest.test_case "property value lookups" `Quick
        test_property_value_lookup;
      Alcotest.test_case "document context" `Quick test_document_context;
      Alcotest.test_case "query context" `Quick test_query_context;
      Alcotest.test_case "loader and animation contexts" `Quick
        test_loader_and_animation_contexts;
    ] )
