(** Fuzz tests for explicit CSS transform contexts. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let name prefix buf i = prefix ^ string_of_int (byte_at buf i)
let css_value pp value = Css.Pp.to_string ~minify:true pp value

let contains_literal haystack needle =
  let re = Re.compile (Re.str needle) in
  Re.execp re haystack

let expect_contains label haystack needle =
  if not (contains_literal haystack needle) then
    fail (Fmt.str "%s missing %S in %S" label needle haystack)

let test_empty_contexts _buf =
  let value = Css.Context.empty in
  if
    value.custom_properties <> []
    || value.inherited_values <> []
    || value.initial_values <> [] || value.base_url <> None
    || value.root_font_size <> None
    || value.parent_font_size <> None
    || value.current_color <> None
    || value.viewport_width <> None
    || value.viewport_height <> None
    || value.container_width <> None
    || value.container_height <> None
  then fail "empty property-value context changed";
  let document = Css.Context.empty_document in
  if Css.Context.has_class "x" document || Css.Context.has_id "x" document then
    fail "empty document context matched a class or id";
  if Css.Context.attribute "x" document <> None then
    fail "empty document context matched an attribute";
  let query = Css.Context.empty_query in
  if Css.Context.media_feature "width" query <> None then
    fail "empty query context matched a media feature";
  if Css.Context.supports_declaration ~property:"display" ~value:"grid" query
  then fail "empty query context matched a support declaration";
  if Css.Context.container_feature "inline-size" query <> None then
    fail "empty query context matched a container feature";
  if Css.Context.import_source "theme.css" Css.Context.empty_loader <> None then
    fail "empty loader context matched an import";
  if Css.Context.animates_property "opacity" Css.Context.empty_animation then
    fail "empty animation context matched a property"

let test_property_value_context buf =
  let open Css.Values in
  let custom = name "--v" buf 0 in
  let property, value =
    pick
      [
        ("color", css_value pp_color (Named Red));
        ("font-size", css_value pp_length (Rem 1.));
        ("display", "grid");
        ("width", css_value pp_length Auto);
      ]
      buf 1
  in
  let custom_decl = Css.Declaration.of_string (custom ^ ": " ^ value) in
  let inherited_decl = Css.Declaration.of_string (property ^ ": " ^ value) in
  let initial_decl = Css.Declaration.of_string "display: inline" in
  let ctx =
    Css.Context.v ~custom_properties:[ custom_decl ]
      ~inherited_values:[ inherited_decl ] ~initial_values:[ initial_decl ]
      ~base_url:"https://example.test/a.css" ~root_font_size:(Px 16.)
      ~parent_font_size:(Px 14.) ~current_color:(Named Red)
      ~viewport_width:(Px 1024.) ~viewport_height:(Px 768.)
      ~container_width:(Px 640.) ~container_height:(Px 480.) ()
  in
  if Css.Context.custom_property custom ctx <> Some custom_decl then
    fail "custom property lookup changed";
  if Css.Context.inherited_value property ctx <> Some inherited_decl then
    fail "inherited value lookup changed";
  if Css.Context.initial_value "display" ctx <> Some initial_decl then
    fail "initial value lookup changed";
  if Css.Context.custom_property (custom ^ "-missing") ctx <> None then
    fail "custom property lookup stopped being exact";
  if Css.Context.inherited_value (property ^ "-missing") ctx <> None then
    fail "inherited value lookup stopped being exact";
  if ctx.base_url <> Some "https://example.test/a.css" then
    fail "base URL context changed";
  if
    ctx.root_font_size <> Some (Px 16.) || ctx.parent_font_size <> Some (Px 14.)
  then fail "font-size context changed";
  if
    ctx.viewport_width <> Some (Px 1024.)
    || ctx.viewport_height <> Some (Px 768.)
  then fail "viewport context changed";
  if
    ctx.container_width <> Some (Px 640.)
    || ctx.container_height <> Some (Px 480.)
  then fail "container dimension context changed"

let test_document_context buf =
  let class_name = name "c" buf 0 in
  let id = name "id" buf 1 in
  let attr = name "data-" buf 2 in
  let ctx =
    Css.Context.document ~element:"div" ~classes:[ class_name ] ~ids:[ id ]
      ~attributes:[ (attr, Some "x"); ("disabled", None) ]
      ~pseudo_classes:[ "hover" ] ~pseudo_elements:[ "before" ] ()
  in
  if not (Css.Context.has_class class_name ctx) then
    fail "document context lost class";
  if not (Css.Context.has_id id ctx) then fail "document context lost id";
  if Css.Context.attribute attr ctx <> Some (Some "x") then
    fail "document context lost attribute value";
  if Css.Context.attribute "disabled" ctx <> Some None then
    fail "document context lost valueless attribute";
  if Css.Context.has_class (class_name ^ "-missing") ctx then
    fail "document context class lookup stopped being exact";
  if Css.Context.has_id (id ^ "-missing") ctx then
    fail "document context id lookup stopped being exact";
  if Css.Context.attribute (attr ^ "-missing") ctx <> None then
    fail "document context attribute lookup stopped being exact";
  if ctx.element <> Some "div" then fail "document context lost element";
  if ctx.pseudo_classes <> [ "hover" ] then
    fail "document context lost pseudo-class list";
  if ctx.pseudo_elements <> [ "before" ] then
    fail "document context lost pseudo-element list"

let test_query_context buf =
  let media = pick [ "width"; "height"; "dynamic-range" ] buf 0 in
  let feature_value = pick [ "1024px"; "768px"; "high" ] buf 1 in
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ (media, feature_value) ]
      ~supports_declarations:[ ("display", "grid") ]
      ~supports_functions:[ ("selector", ":has(img)") ]
      ~container_name:"card"
      ~container_features:[ ("inline-size", "640px") ]
      ()
  in
  if Css.Context.media_feature media ctx <> Some feature_value then
    fail "query context lost media feature";
  if
    not (Css.Context.supports_declaration ~property:"display" ~value:"grid" ctx)
  then fail "query context lost supported declaration";
  if Css.Context.container_feature "inline-size" ctx <> Some "640px" then
    fail "query context lost container feature";
  if Css.Context.media_feature (media ^ "-missing") ctx <> None then
    fail "query context media lookup stopped being exact";
  if Css.Context.supports_declaration ~property:"display" ~value:"flex" ctx then
    fail "query context support value lookup stopped being exact";
  if Css.Context.supports_declaration ~property:"Display" ~value:"grid" ctx then
    fail "query context support property lookup stopped being exact";
  if Css.Context.container_feature "block-size" ctx <> None then
    fail "query context container lookup stopped being exact";
  if ctx.media_type <> Some "screen" then fail "query context lost media type";
  if ctx.supports_functions <> [ ("selector", ":has(img)") ] then
    fail "query context lost supports function list";
  if ctx.container_name <> Some "card" then
    fail "query context lost container name"

let test_loader_animation_context buf =
  let url = pick [ "theme.css"; "../base.css"; "#inline" ] buf 0 in
  let source = pick [ ".a{color:red}"; "@layer theme{}" ] buf 1 in
  let loader =
    Css.Context.loader ~base_url:"https://example.test/"
      ~imports:[ (url, source) ]
      ()
  in
  if Css.Context.import_source url loader <> Some source then
    fail "loader context lost import source";
  let property = pick [ "opacity"; "transform"; "color" ] buf 2 in
  let animation =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.5
      ~animated_properties:[ property ] ()
  in
  if not (Css.Context.animates_property property animation) then
    fail "animation context lost property";
  if Css.Context.import_source (url ^ "-missing") loader <> None then
    fail "loader context import lookup stopped being exact";
  if Css.Context.animates_property (property ^ "-missing") animation then
    fail "animation context property lookup stopped being exact";
  if loader.base_url <> Some "https://example.test/" then
    fail "loader context lost base URL";
  if animation.timeline_time <> Some "250ms" || animation.progress <> Some 0.5
  then fail "animation context lost timeline fields"

let test_context_printers buf =
  let custom = name "--print" buf 0 in
  let class_name = name "c" buf 1 in
  let media = pick [ "width"; "height"; "dynamic-range" ] buf 2 in
  let property = pick [ "opacity"; "transform"; "color" ] buf 3 in
  let value_ctx =
    Css.Context.v
      ~custom_properties:[ Css.Declaration.of_string (custom ^ ": red") ]
      ~base_url:"https://example.test/print.css"
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  let value_dump = Css.Pp.to_string Css.Context.pp value_ctx in
  expect_contains "property-value context printer" value_dump custom;
  expect_contains "property-value context printer" value_dump
    "https://example.test/print.css";
  let document_dump =
    Css.Context.document ~element:"section" ~classes:[ class_name ]
      ~attributes:[ ("data-state", Some "open") ]
      ()
    |> Css.Pp.to_string Css.Context.pp_document
  in
  expect_contains "document context printer" document_dump class_name;
  expect_contains "document context printer" document_dump "data-state=open";
  let query_dump =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ (media, "1024px") ]
      ~supports_functions:[ ("selector", ":has(img)") ]
      ()
    |> Css.Pp.to_string Css.Context.pp_query
  in
  expect_contains "query context printer" query_dump media;
  expect_contains "query context printer" query_dump "selector=:has(img)";
  let loader_dump =
    Css.Context.loader ~base_url:"https://example.test/"
      ~imports:[ ("theme.css", ".card{color:red}") ]
      ()
    |> Css.Pp.to_string Css.Context.pp_loader
  in
  expect_contains "loader context printer" loader_dump "theme.css";
  let animation_dump =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.5
      ~animated_properties:[ property ] ()
    |> Css.Pp.to_string Css.Context.pp_animation
  in
  expect_contains "animation context printer" animation_dump property;
  expect_contains "animation context printer" animation_dump "progress=Some 0.5"

let suite =
  ( "context",
    [
      test_case "empty contexts" [ bytes ] test_empty_contexts;
      test_case "property value context" [ bytes ] test_property_value_context;
      test_case "document context" [ bytes ] test_document_context;
      test_case "query context" [ bytes ] test_query_context;
      test_case "loader and animation context" [ bytes ]
        test_loader_animation_context;
      test_case "context printers" [ bytes ] test_context_printers;
    ] )
