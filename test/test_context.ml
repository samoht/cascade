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

let check_computed_value name ~ctx ~expected input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.computed_value ctx decl with
  | Ok actual -> Alcotest.(check string) name expected actual
  | Error _ ->
      Alcotest.failf "%s: expected %S to resolve to %S" name input expected

let check_computed_error name ~ctx input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.computed_value ctx decl with
  | Ok actual ->
      Alcotest.failf "%s: expected %S to be unresolved, got %S" name input
        actual
  | Error _ -> ()

let check_layered_computed_value name ~ctx ~layer_order ?layer ~expected input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.computed_value ~layer_order ?layer ctx decl with
  | Ok actual -> Alcotest.(check string) name expected actual
  | Error _ ->
      Alcotest.failf "%s: expected layered %S to resolve to %S" name input
        expected

let check_layered_computed_error name ~ctx ~layer_order ?layer input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.computed_value ~layer_order ?layer ctx decl with
  | Ok actual ->
      Alcotest.failf "%s: expected layered %S to be unresolved, got %S" name
        input actual
  | Error _ -> ()

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

let check_import_loaded name ?query ~loader ~expected input =
  let r = Css.Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query loader import_rule with
  | Ok stylesheet ->
      Alcotest.(check string)
        name expected
        (Css.Stylesheet.to_string ~minify:true ~newline:false stylesheet)
  | Error _ -> Alcotest.failf "%s: expected import %S to load" name input

let check_import_error name ?query ~loader input =
  let r = Css.Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query loader import_rule with
  | Ok stylesheet ->
      Alcotest.failf "%s: expected import %S to be blocked, got %S" name input
        (Css.Stylesheet.to_string ~minify:true ~newline:false stylesheet)
  | Error _ -> ()

let check_layered_import_loaded name ?query ~loader ~layer_order ~expected input
    =
  let r = Css.Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query ~layer_order loader import_rule with
  | Ok stylesheet ->
      Alcotest.(check string)
        name expected
        (Css.Stylesheet.to_string ~minify:true ~newline:false stylesheet)
  | Error _ ->
      Alcotest.failf "%s: expected layered import %S to load" name input

let check_layered_import_error name ?query ~loader ~layer_order input =
  let r = Css.Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query ~layer_order loader import_rule with
  | Ok stylesheet ->
      Alcotest.failf "%s: expected layered import %S to be blocked, got %S" name
        input
        (Css.Stylesheet.to_string ~minify:true ~newline:false stylesheet)
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
        ]
      ~initial_values:
        [
          Css.Declaration.of_string "display: inline";
          Css.Declaration.of_string "width: auto";
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 10.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1024.)
      ~viewport_height:(Css.Values.Px 768.)
      ~container_width:(Css.Values.Px 640.)
      ~container_height:(Css.Values.Px 480.) ()
  in
  check_computed_value "initial keyword uses property initial value" ~ctx
    ~expected:"inline" "display: initial";
  check_computed_value "inherit keyword uses inherited value" ~ctx
    ~expected:"blue" "color: inherit";
  check_computed_value "unset on inherited property uses inherited value" ~ctx
    ~expected:"blue" "color: unset";
  check_computed_value "unset on non-inherited property uses initial value" ~ctx
    ~expected:"auto" "width: unset";
  check_computed_value "currentColor uses explicit current color" ~ctx
    ~expected:"red" "border-color: currentColor";
  check_computed_value "custom property var resolves from context" ~ctx
    ~expected:"red" "color: var(--brand)";
  check_computed_value "custom property fallback resolves when missing" ~ctx
    ~expected:"green" "color: var(--missing, green)";
  check_computed_value "custom length var resolves then computes" ~ctx
    ~expected:"16px" "margin-left: var(--gap)";
  check_computed_value "rem resolves against root font size" ~ctx
    ~expected:"32px" "margin-left: 2rem";
  check_computed_value "em resolves against parent font size" ~ctx
    ~expected:"20px" "font-size: 2em";
  check_computed_value "vw resolves against viewport width" ~ctx
    ~expected:"512px" "width: 50vw";
  check_computed_value "vh resolves against viewport height" ~ctx
    ~expected:"192px" "height: 25vh";
  check_computed_value "cqw resolves against container width" ~ctx
    ~expected:"320px" "width: 50cqw";
  check_computed_value "cqh resolves against container height" ~ctx
    ~expected:"120px" "height: 25cqh";
  check_computed_value "relative URL resolves against base URL" ~ctx
    ~expected:"url(https://example.test/img/logo.svg)"
    "background-image: url(../img/logo.svg)";
  check_computed_error "missing var without fallback is unresolved" ~ctx
    "color: var(--missing)";
  check_computed_error "viewport unit without viewport context is unresolved"
    ~ctx:Css.Context.empty "width: 50vw";
  check_computed_error "currentColor without current color is unresolved"
    ~ctx:Css.Context.empty "color: currentColor";
  check_computed_error "relative URL without base URL is unresolved"
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
  check_computed_value "inherit on root falls back to property initial" ~ctx
    ~expected:"canvastext" "color: inherit";
  check_computed_value "nested custom property var resolves transitively" ~ctx
    ~expected:"red" "color: var(--accent)";
  check_computed_value "nested var fallback resolves transitively" ~ctx
    ~expected:"red" "color: var(--missing, var(--accent))";
  check_computed_value "custom length var resolves with parent font context"
    ~ctx ~expected:"24px" "margin-left: var(--space)";
  check_computed_value "font-size percentage resolves against parent font" ~ctx
    ~expected:"18px" "font-size: 150%";
  check_computed_value "absolute inches canonicalize to px" ~ctx
    ~expected:"96px" "margin-left: 1in";
  check_computed_value "absolute points canonicalize to px" ~ctx
    ~expected:"96px" "margin-left: 72pt";
  check_computed_value "viewport vmin uses smaller viewport side" ~ctx
    ~expected:"80px" "width: 10vmin";
  check_computed_value "viewport vmax uses larger viewport side" ~ctx
    ~expected:"120px" "width: 10vmax";
  check_computed_value "container cqmin uses smaller container side" ~ctx
    ~expected:"30px" "width: 10cqmin";
  check_computed_value "container cqmax uses larger container side" ~ctx
    ~expected:"90px" "width: 10cqmax";
  check_computed_value "same-directory URL resolves against base URL" ~ctx
    ~expected:"url(https://example.test/assets/css/panel.css)"
    "background-image: url(panel.css)";
  check_computed_value "root-relative URL preserves origin" ~ctx
    ~expected:"url(https://example.test/icons/logo.svg)"
    "background-image: url(/icons/logo.svg)";
  check_computed_error "custom property cycle is unresolved" ~ctx
    "color: var(--cycle-a)";
  check_computed_error "width percentage needs layout context" ~ctx "width: 50%";
  check_computed_error "em unit without parent font context is unresolved"
    ~ctx:Css.Context.empty "margin-left: 2em";
  check_computed_error "rem unit without root font context is unresolved"
    ~ctx:Css.Context.empty "margin-left: 2rem";
  check_computed_error "container unit without container context is unresolved"
    ~ctx:Css.Context.empty "width: 50cqw"

let test_document_selector_context_contract () =
  let ctx =
    Css.Context.document ~root:"html" ~scope:".card" ~element:"button"
      ~classes:[ "btn"; "primary" ] ~ids:[ "submit" ]
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
          ("width", "1024px");
          ("height", "768px");
          ("prefers-color-scheme", "dark");
          ("dynamic-range", "high");
        ]
      ~supports_declarations:
        [
          ("display", "grid");
          ("container-type", "inline-size");
          ("color", "oklch(50% 0.1 20)");
        ]
      ~supports_functions:[ ("selector", ":has(img)") ]
      ~container_name:"card"
      ~container_features:
        [
          ("inline-size", "640px");
          ("block-size", "480px");
          ("style(--theme)", "dark");
          ("scroll-state(stuck: top)", "true");
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
  check_computed_value "calc resolves compatible absolute and rem units" ~ctx
    ~expected:"18px" "margin-left: calc(1rem + 2px)";
  check_computed_value "calc resolves multiplication with rem" ~ctx
    ~expected:"32px" "margin-left: calc(2 * 1rem)";
  check_computed_value "calc resolves division with em" ~ctx ~expected:"6px"
    "margin-left: calc(1em / 2)";
  check_computed_value "margin shorthand computes each length slot" ~ctx
    ~expected:"12px 32px" "margin: 1em 2rem";
  check_computed_value "padding shorthand computes var-backed slots" ~ctx
    ~expected:"12px 32px" "padding: var(--pad-y) var(--pad-x)";
  check_computed_value "border-width shorthand computes absolute units" ~ctx
    ~expected:"1px 2px 4px 8px" "border-width: 1px 0.125rem 4px 0.5rem";
  check_computed_value "number custom property resolves for opacity" ~ctx
    ~expected:"0.5" "opacity: var(--brand-alpha)";
  check_computed_value "viewport dvw resolves against viewport width" ~ctx
    ~expected:"250px" "width: 25dvw";
  check_computed_value "viewport svh resolves against viewport height" ~ctx
    ~expected:"60px" "height: 10svh";
  check_computed_value "container cqi resolves against inline size" ~ctx
    ~expected:"125px" "width: 25cqi";
  check_computed_value "container cqb resolves against block size" ~ctx
    ~expected:"100px" "height: 25cqb";
  check_computed_error "calc with unresolved percentage remains unresolved" ~ctx
    "width: calc(100% - 1rem)";
  check_computed_error "font metric unit ch needs font metrics" ~ctx
    "width: 4ch";
  check_computed_error "font metric unit ex needs font metrics" ~ctx
    "width: 2ex"

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
          ("width", "1024px");
          ("height", "768px");
          ("orientation", "landscape");
          ("hover", "hover");
          ("pointer", "fine");
        ]
      ~supports_declarations:
        [ ("display", "grid"); ("display", "flex"); ("color", "lab(50% 0 0)") ]
      ~supports_functions:
        [ ("selector", ":is(.a, .b)"); ("font-format", "woff2") ]
      ~container_name:"card"
      ~container_features:
        [
          ("inline-size", "720px");
          ("block-size", "360px");
          ("style(--density)", "compact");
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
      ~media_features:[ ("width", "1024px") ]
      ~supports_declarations:[ ("display", "grid"); ("color", "lab(50% 0 0)") ]
      ()
  in
  check_import_loaded "import loads when media and supports match" ~query
    ~loader ~expected:".grid{display:grid}"
    "@import url(grid.css) supports(display: grid) screen and (width >= 40em);";
  check_import_loaded "import loads when supports color function matches" ~query
    ~loader ~expected:".lab{color:lab(50% 0 0)}"
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
  check_layered_computed_value
    "tailwind theme variable is visible to utility declarations" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"oklch(70% 0.15 250)"
    "color: var(--color-brand-500)";
  check_layered_computed_value
    "normal unlayered app variable overrides tailwind layered tokens" ~ctx
    ~layer_order ~expected:"oklch(40% 0.10 250)" "color: var(--color-brand-500)";
  check_layered_computed_value
    "base layer token overrides theme for base-scoped resolution" ~ctx
    ~layer_order ~layer:"base" ~expected:"oklch(60% 0.12 250)"
    "color: var(--color-brand-500)";
  check_layered_computed_value
    "component token resolves through tailwind theme spacing" ~ctx ~layer_order
    ~layer:"components" ~expected:"24px" "padding: var(--card-padding)";
  check_layered_computed_value
    "utility-local --tw variable resolves through theme spacing" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"16px"
    "translate: var(--tw-translate-x)";
  check_layered_computed_value
    "utility-local opacity variable resolves as a number" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"0.5" "opacity: var(--tw-bg-opacity)";
  check_layered_computed_value
    "important tailwind theme layer beats important utility layer" ~ctx
    ~layer_order ~expected:"blue" "outline-color: var(--tw-ring-color)";
  check_layered_computed_error "unknown layer is rejected for scoped resolution"
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
      ~inherited_values:[ Css.Declaration.of_string "color: purple" ]
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
  check_layered_computed_value
    "revert-layer utility token rolls back to base token" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"oklch(60% 0.12 250)"
    "color: var(--color-brand-500)";
  check_layered_computed_value
    "revert-layer utility spacing rolls back to component token then theme \
     token"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"24px"
    "padding: var(--card-padding)";
  check_layered_computed_value
    "property revert-layer rolls back to lower cascaded layer" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"oklch(60% 0.12 250)" "color: revert-layer";
  check_layered_computed_value
    "property revert-layer in theme falls back to initial" ~ctx ~layer_order
    ~layer:"theme" ~expected:"0" "margin-left: revert-layer";
  check_layered_computed_error "revert-layer cycle is unresolved" ~ctx
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
      ~supports_declarations:[ ("display", "grid") ]
      ~media_features:[ ("width", "1024px") ]
      ()
  in
  check_layered_import_loaded "tailwind theme import enters theme layer" ~loader
    ~layer_order
    ~expected:
      "@layer theme{:root{--color-brand-500:oklch(70% 0.15 \
       250);--spacing:0.25rem}}"
    "@import url(theme.css) layer(theme);";
  check_layered_import_loaded "tailwind base import enters base layer" ~loader
    ~layer_order ~expected:"@layer base{*,::before,::after{margin:0}}"
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
  check_layered_computed_value
    "tailwind rgb slash alpha resolves theme channel and opacity var" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"rgb(239 68 68/0.75)"
    "background-color: rgb(var(--color-red-500) / var(--tw-bg-opacity))";
  check_layered_computed_value
    "tailwind border opacity var resolves independently" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"rgb(59 130 246/0.5)"
    "border-color: rgb(var(--color-blue-500) / var(--tw-border-opacity))";
  check_layered_computed_value
    "tailwind gradient stops resolve chained utility variables" ~ctx
    ~layer_order ~layer:"utilities"
    ~expected:"linear-gradient(to right,rgb(239 68 68/1),rgb(59 130 246/0))"
    "background-image: linear-gradient(to right, var(--tw-gradient-stops))";
  check_layered_computed_value
    "tailwind ring shadow resolves offset and color variables" ~ctx ~layer_order
    ~layer:"utilities"
    ~expected:"0 0 0 2px #fff,0 0 0 calc(3px + 2px) rgb(59 130 246/0.5)"
    "box-shadow: 0 0 0 var(--tw-ring-offset-width) \
     var(--tw-ring-offset-color), 0 0 0 calc(3px + \
     var(--tw-ring-offset-width)) var(--tw-ring-color)";
  check_layered_computed_value
    "tailwind transform utility variables resolve in order" ~ctx ~layer_order
    ~layer:"utilities"
    ~expected:"translate(16px) rotate(45deg) scaleX(1) scaleY(1)"
    "transform: translate(var(--tw-translate-x)) rotate(var(--tw-rotate)) \
     scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))";
  check_layered_computed_value
    "tailwind radius token resolves through theme variable" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"8px" "border-radius: var(--radius-lg)";
  check_layered_computed_value
    "tailwind nested fallback uses theme token when utility var is absent" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"16px"
    "margin-left: var(--tw-space-x, calc(var(--spacing) * 4))";
  check_layered_computed_error
    "tailwind utility var without fallback remains unresolved" ~ctx ~layer_order
    ~layer:"utilities" "outline-width: var(--tw-outline-width)"

let selector_scope_contract () =
  let scoped =
    Css.Context.document ~root:"html" ~scope:".card" ~element:"section"
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
  check_layered_computed_value
    "tailwind utility layer token wins within utility layer" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"16px" "margin-left: var(--spacing)";
  check_layered_computed_value
    "tailwind component layer token wins within component layer" ~ctx
    ~layer_order ~layer:"components" ~expected:"8px"
    "margin-left: var(--spacing)";
  check_layered_computed_value
    "tailwind theme layer token wins within theme layer" ~ctx ~layer_order
    ~layer:"theme" ~expected:"4px" "margin-left: var(--spacing)";
  check_layered_computed_value
    "tailwind unscoped normal lookup uses final layered token" ~ctx ~layer_order
    ~expected:"green" "color: var(--color-brand)";
  check_layered_computed_value
    "tailwind component scoped lookup ignores later utility token" ~ctx
    ~layer_order ~layer:"components" ~expected:"blue"
    "color: var(--color-brand)"

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
      Alcotest.test_case "computed value resolution contract" `Quick
        test_computed_value_resolution_contract;
      Alcotest.test_case "computed value edge contract" `Quick
        computed_edge_contract;
      Alcotest.test_case "document selector context contract" `Quick
        test_document_selector_context_contract;
      Alcotest.test_case "query context evaluation contract" `Quick
        test_query_context_evaluation_contract;
      Alcotest.test_case "loader context contract" `Quick
        test_loader_context_contract;
      Alcotest.test_case "animation context contract" `Quick
        test_animation_context_contract;
      Alcotest.test_case "computed value calc and shorthand contract" `Quick
        computed_calc_contract;
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
    ] )
