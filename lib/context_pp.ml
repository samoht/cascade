open Context_intf

(* Pretty-printers emit a debug-style record for inspection, not CSS source. *)

let pp_string_option ctx = function
  | None -> Pp.string ctx "None"
  | Some s ->
      Pp.string ctx "Some ";
      Pp.string ctx s

let pp_pair ctx (k, v) =
  Pp.string ctx k;
  Pp.char ctx '=';
  Pp.string ctx v

let pp_debug_list pp_item ctx items =
  Pp.char ctx '[';
  let first = ref true in
  List.iter
    (fun item ->
      if !first then first := false else Pp.string ctx ", ";
      pp_item ctx item)
    items;
  Pp.char ctx ']'

let pp_string_list ctx items = pp_debug_list Pp.string ctx items
let pp_pair_list ctx items = pp_debug_list pp_pair ctx items

let pp_attr_list ctx items =
  pp_debug_list
    (fun ctx (name, value) ->
      Pp.string ctx name;
      match value with
      | None -> ()
      | Some v ->
          Pp.char ctx '=';
          Pp.string ctx v)
    ctx items

let pp_field ctx ~first label print_value value =
  if not !first then Pp.string ctx "; ";
  first := false;
  Pp.string ctx label;
  Pp.char ctx '=';
  print_value ctx value

let pp_decl_list ctx items = pp_debug_list Declaration.pp ctx items
let pp_bool ctx value = Pp.string ctx (string_of_bool value)
let pp_int ctx value = Pp.string ctx (string_of_int value)

let pp_cascade_rule ctx (rule : cascade_rule) =
  let first = ref true in
  Pp.char ctx '{';
  pp_field ctx ~first "property_name" Pp.string rule.property_name;
  pp_field ctx ~first "important" pp_bool rule.important;
  pp_field ctx ~first "layer" pp_string_option rule.layer;
  pp_field ctx ~first "source_order" pp_int rule.source_order;
  pp_field ctx ~first "declaration" Declaration.pp rule.declaration;
  Pp.char ctx '}'

let pp_cascade_rules ctx = function
  | None -> Pp.string ctx "None"
  | Some rules ->
      Pp.string ctx "Some ";
      Pp.char ctx '[';
      let first = ref true in
      List.iter
        (fun rule ->
          if !first then first := false else Pp.string ctx ", ";
          pp_cascade_rule ctx rule)
        rules;
      Pp.char ctx ']'

let pp_length_option ctx = function
  | None -> Pp.string ctx "None"
  | Some l ->
      Pp.string ctx "Some ";
      Values.pp_length ~always:true ctx l

let pp_color_option ctx = function
  | None -> Pp.string ctx "None"
  | Some c ->
      Pp.string ctx "Some ";
      Values.pp_color ctx c

let pp : t Pp.t =
 fun ctx t ->
  let first = ref true in
  Pp.char ctx '{';
  pp_field ctx ~first "custom_properties" pp_decl_list t.custom_properties;
  pp_field ctx ~first "inherited_values" pp_decl_list t.inherited_values;
  pp_field ctx ~first "initial_values" pp_decl_list t.initial_values;
  pp_field ctx ~first "layer_order" pp_string_list t.layer_order;
  pp_field ctx ~first "layer" pp_string_option t.layer;
  pp_field ctx ~first "cascade_rules" pp_cascade_rules t.cascade_rules;
  pp_field ctx ~first "base_url" pp_string_option t.base_url;
  pp_field ctx ~first "root_font_size" pp_length_option t.root_font_size;
  pp_field ctx ~first "parent_font_size" pp_length_option t.parent_font_size;
  pp_field ctx ~first "current_color" pp_color_option t.current_color;
  pp_field ctx ~first "viewport_width" pp_length_option t.viewport_width;
  pp_field ctx ~first "viewport_height" pp_length_option t.viewport_height;
  pp_field ctx ~first "container_width" pp_length_option t.container_width;
  pp_field ctx ~first "container_height" pp_length_option t.container_height;
  Pp.char ctx '}'

let pp_document : document Pp.t =
 fun ctx d ->
  let first = ref true in
  Pp.char ctx '{';
  pp_field ctx ~first "root" pp_string_option d.root;
  pp_field ctx ~first "scope" pp_string_option
    (Option.map (Selector.to_string ~minify:true) d.scope);
  pp_field ctx ~first "element" pp_string_option d.element;
  pp_field ctx ~first "classes" pp_string_list d.classes;
  pp_field ctx ~first "ids" pp_string_list d.ids;
  pp_field ctx ~first "attributes" pp_attr_list d.attributes;
  pp_field ctx ~first "pseudo_classes" pp_string_list d.pseudo_classes;
  pp_field ctx ~first "pseudo_elements" pp_string_list d.pseudo_elements;
  Pp.char ctx '}'

let pp_query : query Pp.t =
 fun ctx q ->
  let first = ref true in
  Pp.char ctx '{';
  pp_field ctx ~first "media_type" pp_string_option q.media_type;
  let pp_typed_list to_str ctx items =
    Pp.char ctx '[';
    let first = ref true in
    List.iter
      (fun item ->
        if !first then first := false else Pp.string ctx ", ";
        Pp.string ctx (to_str item))
      items;
    Pp.char ctx ']'
  in
  pp_field ctx ~first "media_features"
    (pp_typed_list Media.to_string)
    q.media_features;
  pp_field ctx ~first "supports" (pp_typed_list Supports.to_string) q.supports;
  pp_field ctx ~first "container_name" pp_string_option q.container_name;
  pp_field ctx ~first "container_features"
    (pp_typed_list Container.to_string)
    q.container_features;
  Pp.char ctx '}'

let pp_loader : loader Pp.t =
 fun ctx l ->
  let first = ref true in
  Pp.char ctx '{';
  pp_field ctx ~first "base_url" pp_string_option l.base_url;
  pp_field ctx ~first "imports" pp_pair_list l.imports;
  Pp.char ctx '}'

let pp_animation : animation Pp.t =
 fun ctx a ->
  let first = ref true in
  Pp.char ctx '{';
  pp_field ctx ~first "timeline_time" pp_string_option a.timeline_time;
  pp_field ctx ~first "progress"
    (fun ctx -> function
      | None -> Pp.string ctx "None"
      | Some f ->
          Pp.string ctx "Some ";
          Pp.string ctx (Float.to_string f))
    a.progress;
  pp_field ctx ~first "animated_properties" pp_string_list a.animated_properties;
  Pp.char ctx '}'

let pp_property_registration ctx registration =
  Pp.char ctx '{';
  Pp.string ctx "name=";
  Pp.string ctx registration.name;
  Pp.string ctx "; syntax=";
  Variables.pp_any_syntax ctx registration.syntax;
  Pp.string ctx "; inherits=";
  Pp.string ctx (Bool.to_string registration.inherits);
  Pp.string ctx "; initial_value=";
  pp_string_option ctx registration.initial_value;
  Pp.char ctx '}'

let pp_property_registry : property_registry Pp.t =
 fun ctx registry ->
  pp_debug_list pp_property_registration ctx registry.property_registrations
