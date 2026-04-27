(** Fuzz tests for explicit CSS transform contexts. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let name prefix buf i = prefix ^ string_of_int (byte_at buf i)
let css_value pp value = Css.Pp.to_string ~minify:true pp value

let test_property_value_context buf =
  let open Css.Values in
  let custom = name "--v" buf 0 in
  let property = pick [ "color"; "font-size"; "display"; "width" ] buf 1 in
  let value =
    pick
      [
        css_value pp_color (Named Red);
        css_value pp_length (Rem 1.);
        "grid";
        css_value pp_length Auto;
      ]
      buf 2
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
    fail "inherited value lookup changed"

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
    fail "document context lost valueless attribute"

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
    fail "query context lost container feature"

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
    fail "animation context lost property"

let suite =
  ( "context",
    [
      test_case "property value context" [ bytes ] test_property_value_context;
      test_case "document context" [ bytes ] test_document_context;
      test_case "query context" [ bytes ] test_query_context;
      test_case "loader and animation context" [ bytes ]
        test_loader_animation_context;
    ] )
