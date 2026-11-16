open Context_intf

let equal_property_registration (a : property_registration) b = a = b

let empty =
  {
    custom_properties = [];
    runtime_vars = [];
    inherited_values = [];
    initial_values = [];
    layer_order = [];
    layer = None;
    cascade_rules = None;
    base_url = None;
    root_font_size = None;
    parent_font_size = None;
    current_color = None;
    viewport_width = None;
    viewport_height = None;
    container_width = None;
    container_height = None;
  }

let v ?(custom_properties = []) ?(runtime_vars = []) ?(inherited_values = [])
    ?(initial_values = []) ?(layer_order = []) ?layer ?cascade_rules ?base_url
    ?root_font_size ?parent_font_size ?current_color ?viewport_width
    ?viewport_height ?container_width ?container_height () =
  {
    custom_properties;
    runtime_vars;
    inherited_values;
    initial_values;
    layer_order;
    layer;
    cascade_rules;
    base_url;
    root_font_size;
    parent_font_size;
    current_color;
    viewport_width;
    viewport_height;
    container_width;
    container_height;
  }

let empty_document =
  {
    root = None;
    scope = None;
    element = None;
    classes = [];
    ids = [];
    attributes = [];
    pseudo_classes = [];
    pseudo_elements = [];
  }

let document ?root ?scope ?element ?(classes = []) ?(ids = [])
    ?(attributes = []) ?(pseudo_classes = []) ?(pseudo_elements = []) () =
  {
    root;
    scope;
    element;
    classes;
    ids;
    attributes;
    pseudo_classes;
    pseudo_elements;
  }

let empty_query =
  {
    media_type = None;
    media_features = [];
    media_inapplicable = [];
    supports = [];
    container_name = None;
    container_features = [];
  }

let query ?media_type ?(media_features = []) ?(media_inapplicable = [])
    ?(supports = []) ?container_name ?(container_features = []) () =
  {
    media_type;
    media_features;
    media_inapplicable;
    supports;
    container_name;
    container_features;
  }

let empty_loader = { base_url = None; imports = [] }
let loader ?base_url ?(imports = []) () = { base_url; imports }

let empty_animation =
  { timeline_time = None; progress = None; animated_properties = [] }

let animation ?timeline_time ?progress ?(animated_properties = []) () =
  { timeline_time; progress; animated_properties }

let empty_property_registry = { property_registrations = [] }

let property_registration ?initial_value ~inherits name syntax =
  if not (Custom_property_name.is_valid name) then
    invalid_arg "property_registration: invalid custom-property name";
  { name; syntax; inherits; initial_value }

let property_registry ?(property_registrations = []) () =
  { property_registrations }

let by_name name decls =
  List.find_opt (fun d -> Declaration.property_name d = name) decls

let custom_property name ctx = by_name name ctx.custom_properties
let inherited_value property ctx = by_name property ctx.inherited_values
let initial_value property ctx = by_name property ctx.initial_values

let index_of name items =
  let rec find i = function
    | [] -> None
    | item :: _ when String.equal item name -> Some i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 items

let cascade_layer_precedence_rank ~layer_order ~important layer =
  let layer_count = List.length layer_order in
  match (important, layer) with
  | false, None -> layer_count
  | false, Some name ->
      Option.value ~default:layer_count (index_of name layer_order)
  | true, None -> 0
  | true, Some name ->
      let i = Option.value ~default:layer_count (index_of name layer_order) in
      layer_count - i

let cascade_rule_chain ctx ~property_name ~important =
  match ctx.cascade_rules with
  | None when ctx.layer_order = [] && Option.is_none ctx.layer -> None
  | None -> Some []
  | Some rules ->
      let current_rank =
        cascade_layer_precedence_rank ~layer_order:ctx.layer_order ~important
          ctx.layer
      in
      rules
      |> List.filter (fun (rule : cascade_rule) ->
          String.equal rule.property_name property_name
          && Bool.equal rule.important important
          && cascade_layer_precedence_rank ~layer_order:ctx.layer_order
               ~important rule.layer
             < current_rank)
      |> List.sort (fun (a : cascade_rule) (b : cascade_rule) ->
          match
            compare
              (cascade_layer_precedence_rank ~layer_order:ctx.layer_order
                 ~important b.layer)
              (cascade_layer_precedence_rank ~layer_order:ctx.layer_order
                 ~important a.layer)
          with
          | 0 -> compare b.source_order a.source_order
          | by_layer -> by_layer)
      |> Option.some

let remove_cascade_rule (target : cascade_rule) rules =
  List.filter
    (fun (rule : cascade_rule) -> rule.source_order <> target.source_order)
    rules

let scope ?layer_order ?layer ctx =
  {
    ctx with
    layer_order = Option.value ~default:ctx.layer_order layer_order;
    layer = (match layer with Some _ -> layer | None -> ctx.layer);
  }

(* CSS Cascade 5 sec. 6.4.3 layered custom-property lookup. Important beats
   normal. Normal author: unlayered wins, else later layers beat earlier.
   Important author: the order reverses (earlier layers beat later, unlayered
   ranks below). *)
let custom_layer_index ~layer_order = function
  | None -> max_int
  | Some name ->
      let rec find i = function
        | [] -> -1
        | n :: _ when n = name -> i
        | _ :: rest -> find (i + 1) rest
      in
      find 0 layer_order

let is_revert_layer d =
  let v = Declaration.string_of_value ~minify:true d in
  String.trim v = "revert-layer"

let custom_layered_candidates ~layer_order pool =
  List.filter_map
    (fun d ->
      if is_revert_layer d then None
      else
        match Declaration.custom_declaration_layer d with
        | None -> None
        | Some _ as l -> Some (custom_layer_index ~layer_order l, d))
    pool

let pick_custom_candidate pool ~compare_score =
  match List.sort (fun (a, _) (b, _) -> compare_score a b) pool with
  | [] -> None
  | (_, d) :: _ -> Some d

let pick_latest_custom_layer ~layer_order pool =
  pick_custom_candidate (custom_layered_candidates ~layer_order pool)
    ~compare_score:(fun a b -> compare b a)

let pick_earliest_custom_layer ~layer_order pool =
  pick_custom_candidate
    (custom_layered_candidates ~layer_order pool)
    ~compare_score:compare

let custom_by_layer scope pool =
  List.find_opt (fun d -> Declaration.custom_declaration_layer d = scope) pool

let custom_revert_layer_fallback ~layer_order pool scope =
  let scope_index =
    match scope with
    | None -> max_int
    | Some _ as l -> custom_layer_index ~layer_order l
  in
  let earlier =
    List.filter
      (fun (idx, _) -> idx < scope_index)
      (custom_layered_candidates ~layer_order pool)
  in
  pick_custom_candidate earlier ~compare_score:(fun a b -> compare b a)

let resolve_custom_with_revert ~layer_order pool d scope =
  if is_revert_layer d then custom_revert_layer_fallback ~layer_order pool scope
  else Some d

let pick_normal_custom ?layer ~layer_order pool =
  match layer with
  | None -> (
      match custom_by_layer None pool with
      | Some d -> resolve_custom_with_revert ~layer_order pool d None
      | None -> pick_latest_custom_layer ~layer_order pool)
  | Some scope -> (
      match custom_by_layer (Some scope) pool with
      | Some d -> resolve_custom_with_revert ~layer_order pool d (Some scope)
      | None -> pick_earliest_custom_layer ~layer_order pool)

let pick_important_custom ~layer_order pool =
  let unlayered_rank = max_int in
  let rank_of d =
    match Declaration.custom_declaration_layer d with
    | None -> unlayered_rank
    | Some _ as l -> custom_layer_index ~layer_order l
  in
  pick_custom_candidate
    (List.map (fun d -> (rank_of d, d)) pool)
    ~compare_score:compare

let lookup_custom_property ?layer ?layer_order ctx name =
  let ctx = scope ?layer_order ?layer ctx in
  let layer_order = ctx.layer_order in
  let layer = ctx.layer in
  match layer with
  | Some scope when not (List.mem scope layer_order) -> None
  | _ -> (
      let target = "--" ^ name in
      let candidates =
        List.filter
          (fun d -> Declaration.property_name d = target)
          ctx.custom_properties
      in
      let important, normal =
        List.partition Declaration.is_important candidates
      in
      match pick_important_custom ~layer_order important with
      | Some _ as v -> v
      | None -> pick_normal_custom ?layer ~layer_order normal)

(* The computed winner among a pool of same-property custom-property
   declarations, resolved by CSS Cascade 5 sec. 6.4.3: important beats normal,
   [revert-layer] rolls back to the next layer down, and layer order (reversed
   for important) breaks ties. Layers are read from each declaration's own
   annotation, so the caller must have tagged them. [None] when the pool is
   empty or resolves to unset. *)
let winning_custom_declaration ~layer_order decls =
  let important, normal = List.partition Declaration.is_important decls in
  match pick_important_custom ~layer_order important with
  | Some _ as v -> v
  | None -> pick_normal_custom ~layer_order normal

(* Extract the value bound to feature [name] from a single media feature. A
   [<name>: <value>] plain and an [<name> = <value>] equality range both pin the
   feature to a concrete value; range / boolean / interval features do not. *)
let feature_value name (f : Media.feature) : Media.value option =
  match f with
  | Media.Plain (feature_name, value)
    when String.equal name (Media.string_of_name feature_name) ->
      Some value
  | Media.Range (feature_name, Media.Eq, value)
    when String.equal name (Media.string_of_name feature_name) ->
      Some value
  | _ -> None

let rec condition_feature_value name (c : Media.condition) : Media.value option
    =
  match c with
  | Media.Feature f -> feature_value name f
  | Media.Not c -> condition_feature_value name c
  | Media.And (a, b) | Media.Or (a, b) -> (
      match condition_feature_value name a with
      | Some _ as v -> v
      | None -> condition_feature_value name b)

let rec media_feature_value name : Media.t -> Media.value option = function
  | Media.Cond c -> condition_feature_value name c
  | Media.Type { trailing = Some c; _ } -> condition_feature_value name c
  | Media.Type _ -> None
  | Media.List qs -> List.find_map (media_feature_value name) qs

let media_feature name ctx =
  List.find_map (media_feature_value name) ctx.media_features

let container_feature name ctx =
  List.find_map
    (function
      | Container.Feature_query media -> media_feature_value name media
      | _ -> None)
    ctx.container_features

let has_class name ctx = List.exists (String.equal name) ctx.classes
let has_id name ctx = List.exists (String.equal name) ctx.ids
let attribute name ctx = List.assoc_opt name ctx.attributes
let import_source url ctx = List.assoc_opt url ctx.imports

let animates_property property ctx =
  List.exists (String.equal property) ctx.animated_properties

let registered_property name registry =
  List.find_opt
    (fun registration -> String.equal name registration.name)
    registry.property_registrations

let validate_value_against_syntax (Variables.Syntax syntax) value =
  match syntax with
  | Variables.Universal | Variables.Transform_function
  | Variables.Transform_list | Variables.Resolution ->
      if String.trim value = "" then Error "registered value is empty"
      else Ok ()
  | _ -> (
      let cursor = Cursor.of_string value in
      match
        try
          ignore (Variables.read_value cursor syntax);
          Cursor.ws cursor;
          if Cursor.is_done cursor then Ok ()
          else
            Error
              ("value has trailing tokens: " ^ Cursor.string_of_remaining cursor)
        with Error.Parse_error e -> Error (Error.to_string e)
      with
      | Ok () -> Ok ()
      | Error msg -> Error msg)

let validate_registered_custom_property registry decl =
  let name = Declaration.property_name decl in
  if not (Custom_property_name.is_valid name) then
    Error ("not a custom property: " ^ name)
  else
    match registered_property name registry with
    | None -> Ok ()
    | Some registration ->
        let value = Declaration.string_of_value ~minify:false decl in
        validate_value_against_syntax registration.syntax value
