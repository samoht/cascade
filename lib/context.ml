(** Explicit contexts for CSS AST/value transforms. *)

type cascade_rule = {
  property_name : string;
  important : bool;
  layer : string option;
  source_order : int;
  declaration : Declaration.declaration;
}

type t = {
  custom_properties : Declaration.declaration list;
  runtime_vars : string list;
      (** Custom-property names (no [--] prefix) that must stay live [var()]
          references: a resolver keeps them like a [~runtime] var, so the
          inliner can still apply value-independent simplifications without
          collapsing the reference. *)
  inherited_values : Declaration.declaration list;
  initial_values : Declaration.declaration list;
  layer_order : string list;
  layer : string option;
  cascade_rules : cascade_rule list option;
  base_url : string option;
  root_font_size : Values.length option;
  parent_font_size : Values.length option;
  current_color : Values.color option;
  viewport_width : Values.length option;
  viewport_height : Values.length option;
  container_width : Values.length option;
  container_height : Values.length option;
}

type document = {
  root : string option;
  scope : Selector.t option;
  element : string option;
  classes : string list;
  ids : string list;
  attributes : (string * string option) list;
  pseudo_classes : string list;
  pseudo_elements : string list;
}

type query = {
  media_type : string option;
  media_features : Media.t list;
      (** Media features the rendering environment claims to expose. Build
          entries with [Media.feature] or [Media.boolean]. *)
  media_inapplicable : Media.name list;
  supports : Supports.t list;
      (** Capability flags the environment claims to support, normally a
          [Supports.Property]/[Supports.Func] leaf. Compound forms
          ([And]/[Or]/[Not]) are accepted but match only a structurally
          identical query. *)
  container_name : string option;
  container_features : Container.t list;
      (** Container capabilities exposed by the matching container. Build size
          features with [Container.feature], style queries with
          [Container.style], and scroll-state queries with
          [Container.scroll_state]. *)
}

type loader = { base_url : string option; imports : (string * string) list }

type animation = {
  timeline_time : string option;
  progress : float option;
  animated_properties : string list;
}

type property_registration = {
  name : string;
  syntax : Variables.any_syntax;
  inherits : bool;
  initial_value : string option;
}

let equal_property_registration (a : property_registration) b = a = b

type property_registry = { property_registrations : property_registration list }

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

(** {1 CSS Evaluator Pipeline}

    Browser CSS engines structure rule application as a sequence of stages
    operating on closed inputs:

    + {b Tokenization / parsing} produces structured ASTs (handled by the
      individual module readers).
    + {b Length canonicalisation} ([Length]) maps each unit to absolute pixels
      using the supplied font/viewport/container references.
    + {b Condition evaluation} ([Match_supports], [Match_media],
      [Match_container]) walks the structured query AST against the explicit
      feature/declaration tables in [query].
    + {b Selector matching} ([Match_selector]) decides whether a selector would
      attach to the element described by [document]. The element is not part of
      a tree, so combinators reduce to matching the rightmost compound selector.
    + {b Cascade + computed value} ([eval]) resolves CSS-wide keywords, expands
      [var()], evaluates [calc()], and converts relative lengths against the
      property-value context in the typed AST.
    + {b Loader} ([Import], [Url]) resolves relative URLs and looks up imported
      stylesheets, applying [@import] guards. *)

type (_, _) type_eq = Refl : ('a, 'a) type_eq

let kind_equal : type a b.
    a Properties.kind -> b Properties.kind -> (a, b) type_eq option =
 fun a b ->
  match (a, b) with
  | Properties.Length, Properties.Length -> Some Refl
  | Properties.Length_percentage, Properties.Length_percentage -> Some Refl
  | Properties.Opacity, Properties.Opacity -> Some Refl
  | Properties.Angle, Properties.Angle -> Some Refl
  | Properties.Rotate, Properties.Rotate -> Some Refl
  | Properties.Scale, Properties.Scale -> Some Refl
  | Properties.Duration, Properties.Duration -> Some Refl
  | Properties.Animation, Properties.Animation -> Some Refl
  | Properties.Number, Properties.Number -> Some Refl
  | Properties.Number_percentage, Properties.Number_percentage -> Some Refl
  | Properties.Font_size, Properties.Font_size -> Some Refl
  | Properties.Filter, Properties.Filter -> Some Refl
  | Properties.Shadow, Properties.Shadow -> Some Refl
  | Properties.Color, Properties.Color -> Some Refl
  | Properties.Gradient_stop, Properties.Gradient_stop -> Some Refl
  | Properties.Background_image, Properties.Background_image -> Some Refl
  | Properties.Font_src, Properties.Font_src -> Some Refl
  | _ -> None

let read_full_components read components =
  match
    let cursor = Cursor.of_components components in
    let value = read cursor in
    Cursor.ws cursor;
    Cursor.expect_eof cursor;
    value
  with
  | value -> Some value
  | exception Cursor.Parse_error _ -> None

let read_custom_value : type a.
    a Properties.kind -> (Cursor.t -> a) -> Declaration.declaration -> a option
    =
 fun target_kind read decl ->
  match decl with
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } -> (
      match value with
      | Properties.Typed { kind; value } -> (
          match kind_equal kind target_kind with
          | Some Refl -> Some value
          | None ->
              read_full_components read
                (Properties.components_of_custom_property_value
                   (Properties.Typed { kind; value })))
      | Properties.Tokens components -> read_full_components read components)
  | _ -> None

let read_custom_components read = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } ->
      read_full_components read
        (Properties.components_of_custom_property_value value)
  | _ -> None

let map_var_fallback f = function
  | Values.Empty -> Values.Empty
  | Values.Empty2 -> Values.Empty2
  | Values.None -> Values.None
  | Values.Fallback value -> Values.Fallback (f value)
  | Values.Syntax_fallback value -> Values.Syntax_fallback value
  | Values.Var_fallback name -> Values.Var_fallback name

let combine_numeric_values ~to_number ~of_number left op right =
  let canonical f =
    try float_of_string (Pp.string_of_float f) with Failure _ -> f
  in
  match (to_number left, op, to_number right) with
  | Some a, Values.Add, Some b -> Some (of_number (canonical (a +. b)))
  | Some a, Values.Sub, Some b -> Some (of_number (canonical (a -. b)))
  | _ -> None

let combine_numeric_value_num ~to_number ~of_number value op num =
  let canonical f =
    try float_of_string (Pp.string_of_float f) with Failure _ -> f
  in
  match (to_number value, op) with
  | Some n, Values.Mul -> Some (of_number (canonical (n *. num)))
  | Some n, Values.Div when num <> 0. -> Some (of_number (canonical (n /. num)))
  | _ -> None

let normalize_numeric_value ~to_number ~of_number value =
  match to_number value with
  | Some n -> (
      try of_number (float_of_string (Pp.string_of_float n))
      with Failure _ -> of_number n)
  | None -> value

module Var_residual = struct
  type 'a simplifier = authored:bool -> visited:string list -> 'a -> 'a

  type 'a ops = {
    as_var : 'a -> 'a Values.var option;
    of_var : 'a Values.var -> 'a;
    read_custom : Declaration.declaration -> 'a option;
    simplify_leaf : 'a simplifier -> 'a simplifier;
  }

  let with_resolver (type a) ?(resolve_fallback = fun _ -> true) ?layer_order
      ?layer cascade ~(read_custom : Declaration.declaration -> a option) f =
    let parsed_custom = Hashtbl.create 16 in
    (* The cache key is only [var.name] because [layer] and [layer_order] are
       fixed for one simplification call. If callers ever vary scope inside a
       single call, the key must include that scope. *)
    let simplify_var_record ~(simplify : a simplifier) ~visited
        (var : a Values.var) : a Values.var =
      {
        var with
        Values.fallback =
          map_var_fallback (simplify ~authored:false ~visited) var.fallback;
        default = Option.map (simplify ~authored:false ~visited) var.default;
      }
    in
    let lookup_parsed name =
      match Hashtbl.find_opt parsed_custom name with
      | Some value -> value
      | None ->
          let value =
            Option.bind
              (lookup_custom_property ?layer ?layer_order cascade name)
              read_custom
          in
          Hashtbl.add parsed_custom name value;
          value
    in
    let resolve_fallback_value ~(simplify : a simplifier) ~visited
        (var : a Values.var) : a option =
      match var.fallback with
      | Values.Fallback fallback when resolve_fallback fallback ->
          Some (simplify ~authored:false ~visited fallback)
      | Values.Fallback _ -> None
      | Values.Empty | Values.Empty2 | Values.None | Values.Syntax_fallback _
      | Values.Var_fallback _ ->
          None
    in
    let resolve_var ~(simplify : a simplifier) ~visited (var : a Values.var) :
        a option =
      (* A [runtime] var - either flagged on the value or named in the context's
         [runtime_vars] - stays a [var()] reference so a script can override the
         custom property; never fold it to its theme value. *)
      if
        var.runtime
        || List.mem var.name cascade.runtime_vars
        || List.mem var.name visited
      then None
      else
        match lookup_parsed var.name with
        | Some value ->
            Some (simplify ~authored:false ~visited:(var.name :: visited) value)
        | None -> (
            (* Typed [default] supplied by [Css.var ~default:...] is the
               authoritative compile-time value of the variable, so it wins over
               the var() [Fallback] arm (which is a browser-time hint for when
               the [--name] declaration is missing at runtime). *)
            match var.default with
            | Some d ->
                Some (simplify ~authored:false ~visited:(var.name :: visited) d)
            | None -> resolve_fallback_value ~simplify ~visited var)
    in
    f ~resolve_var ~simplify_var_record

  let simplify (type a) ?layer_order ?layer cascade (ops : a ops) (value : a) :
      a =
    with_resolver ?layer_order ?layer cascade ~read_custom:ops.read_custom
    @@ fun ~resolve_var ~simplify_var_record ->
    let rec on_var_residual ~visited (var : a Values.var) result =
      match var.fallback with
      | Values.Fallback fb -> simplify ~authored:false ~visited fb
      | Values.Syntax_fallback _ | Values.Var_fallback _ ->
          ops.of_var (simplify_var_record ~simplify ~visited var)
      | _ -> result
    and on_var_unresolved ~visited (var : a Values.var) =
      match var.fallback with
      | Values.Fallback fb -> simplify ~authored:false ~visited fb
      | Values.Syntax_fallback _ | Values.Var_fallback _ | Values.Empty
      | Values.Empty2 | Values.None ->
          ops.of_var (simplify_var_record ~simplify ~visited var)
    and on_var ~visited (var : a Values.var) =
      if var.runtime || List.mem var.name cascade.runtime_vars then
        (* A runtime var remains a live browser-time reference, including its
           fallback. Treating it like an ordinary unresolved variable would
           select a typed [Fallback] here and erase the override point. *)
        ops.of_var (simplify_var_record ~simplify ~visited var)
      else if List.mem var.name visited then
        (* A var() forming a cycle is invalid at computed-value time; its own
           fallback does not rescue it (CSS Variables L1 sec 3). Leave it as an
           unresolved residual so an outer consumer's fallback can apply. *)
        ops.of_var (simplify_var_record ~simplify ~visited var)
      else
        match resolve_var ~simplify ~visited var with
        | Some result when ops.as_var result <> None ->
            on_var_residual ~visited var result
        | Some result -> result
        | None -> on_var_unresolved ~visited var
    and simplify ~authored ~visited value =
      match ops.as_var value with
      | Some var -> on_var ~visited var
      | None -> ops.simplify_leaf simplify ~authored ~visited value
    in
    simplify ~authored:true ~visited:[] value
end

module Calc_residual = struct
  type 'a simplifier = visited:string list -> 'a -> 'a

  type 'a calc_simplifier =
    visited:string list -> 'a Values.calc -> 'a Values.calc

  type 'a ops = {
    of_unitless_number : float -> 'a option;
    zero : 'a Values.calc;
        (** The type's canonical unitless zero, returned by [x * 0] when neither
            operand carries a unit. *)
    is_zero : 'a -> bool;
        (** Recognises a literal zero value, so [0 * x] collapses to zero even
            when [x] is an unresolved [var()]. Conservative [false] is always
            safe (keeps the multiplication). *)
    combine_values : 'a -> Values.calc_op -> 'a -> 'a option;
    combine_value_num : 'a -> Values.calc_op -> float -> 'a option;
    normalize_value : 'a -> 'a;
    as_var : 'a -> 'a Values.var option;
    of_var : 'a Values.var -> 'a;
    as_calc : 'a -> 'a Values.calc option;
    of_calc : 'a Values.calc -> 'a;
    read_custom : Declaration.declaration -> 'a option;
    simplify_leaf : 'a simplifier -> 'a calc_simplifier -> 'a simplifier;
  }

  let rec math_arg_contains_var = function
    | Values.Lit _ | Values.Dim _ | Values.Const _ -> false
    | Values.Var_arg _ -> true
    | Values.Op (l, _, r) -> math_arg_contains_var l || math_arg_contains_var r
    | Values.Parens_arg inner -> math_arg_contains_var inner
    | Values.Math_call fn -> math_fn_contains_var fn

  and angle_arg_contains_var : Values.angle_arg -> bool = function
    | Values.Deg _ | Values.Rad _ | Values.Turn _ | Values.Grad _ -> false
    | Values.Numeric_arg arg -> math_arg_contains_var arg
    | Values.Operation (l, _, r) ->
        angle_arg_contains_var l || angle_arg_contains_var r
    | Values.Grouped inner -> angle_arg_contains_var inner

  and math_fn_contains_var = function
    | Values.Sin a | Values.Cos a | Values.Tan a -> angle_arg_contains_var a
    | Values.Asin a
    | Values.Acos a
    | Values.Atan a
    | Values.Sqrt a
    | Values.Exp a
    | Values.Sign_n a
    | Values.Abs_n a ->
        math_arg_contains_var a
    | Values.Atan2 (a, b) | Values.Log (a, Some b) | Values.Pow (a, b) ->
        math_arg_contains_var a || math_arg_contains_var b
    | Values.Log (a, None) -> math_arg_contains_var a
    | Values.Hypot args -> List.exists math_arg_contains_var args

  let rec contains_var : type a. a Values.calc -> bool = function
    | Values.Var _ -> true
    | Values.Nested inner | Values.Parens inner -> contains_var inner
    | Values.Expr (left, _, right) -> contains_var left || contains_var right
    | Values.Math_fn fn -> math_fn_contains_var fn
    | Values.Num _ | Values.Val _ | Values.Math_const _ | Values.Sibling_index
    | Values.Sibling_count ->
        false

  let fold_num_expr : type a.
      a Values.calc -> Values.calc_op -> a Values.calc -> a Values.calc option =
   fun left op right ->
    let open Values in
    match (left, op, right) with
    | Num a, Add, Num b -> Some (Num (a +. b))
    | Num a, Sub, Num b -> Some (Num (a -. b))
    | Num a, Mul, Num b -> Some (Num (a *. b))
    | Num a, Div, Num b when b <> 0. -> Some (Num (a /. b))
    | _ -> None

  let fold_value_expr : type a.
      a ops ->
      a Values.calc ->
      Values.calc_op ->
      a Values.calc ->
      a Values.calc option =
   fun ops left op right ->
    let open Values in
    match (left, op, right) with
    | Val a, _, Val b ->
        Option.map (fun value -> Val value) (ops.combine_values a op b)
    | Val value, _, Num n ->
        Option.map (fun value -> Val value) (ops.combine_value_num value op n)
    | Num n, Mul, Val value ->
        Option.map
          (fun value -> Val value)
          (ops.combine_value_num value Values.Mul n)
    | _ -> None

  (* Value-independent multiplicative identities ([x * 0], [x * 1], ...) live in
     [Values.calc_identity], shared with the AST evaluators so the paths never
     drift. They hold for any operand, so they apply to an unresolved [var()]
     too; [ops.is_zero] collapses a literal zero ([0px], not just [0]). Additive
     terms fold only through the preceding same-type value combination. *)
  let fold_identity_expr ops left op right =
    Values.calc_identity ~zero:ops.zero ~is_zero:ops.is_zero left op right

  let fold_calc_expr ops left op right =
    match fold_num_expr left op right with
    | Some folded -> folded
    | None -> (
        match fold_value_expr ops left op right with
        | Some folded -> folded
        | None -> (
            match fold_identity_expr ops left op right with
            | Some folded -> folded
            | None -> Values.Expr (left, op, right)))

  let fold_calc_wrapper wrap reduced =
    let open Values in
    match reduced with
    | (Val _ | Num _ | Math_const _ | Var _) as leaf -> leaf
    | reduced -> wrap reduced

  let rec fold_calc : type a. a ops -> a Values.calc -> a Values.calc =
   fun ops calc ->
    let open Values in
    match calc with
    | Expr (left, op, right) ->
        let left = fold_calc ops left in
        let right = fold_calc ops right in
        fold_calc_expr ops left op right
    | Nested inner ->
        fold_calc_wrapper (fun v -> Nested v) (fold_calc ops inner)
    | Parens inner ->
        fold_calc_wrapper (fun v -> Parens v) (fold_calc ops inner)
    | leaf -> leaf

  (* Apply only the value-independent identities ([fold_identity_expr]), leaving
     every other subexpression verbatim. Used when a calc keeps an unresolved
     [var()]: the var() reference is preserved, but [x * 0] / [x * 1] still
     simplify because they never read the variable. *)
  let rec fold_calc_identities : type a. a ops -> a Values.calc -> a Values.calc
      =
   fun ops calc ->
    let open Values in
    match calc with
    | Expr (left, op, right) -> (
        let left = fold_calc_identities ops left in
        let right = fold_calc_identities ops right in
        match fold_identity_expr ops left op right with
        | Some folded -> folded
        | None -> Expr (left, op, right))
    | Nested inner ->
        fold_calc_wrapper (fun v -> Nested v) (fold_calc_identities ops inner)
    | Parens inner ->
        fold_calc_wrapper (fun v -> Parens v) (fold_calc_identities ops inner)
    | leaf -> leaf

  let calc_result_to_value (type a) (ops : a ops) simplify ~visited
      (calc : a Values.calc) : a =
    match calc with
    | Values.Val value -> simplify ~authored:false ~visited value
    | Values.Num n -> (
        match ops.of_unitless_number n with
        | Some value -> value
        | None -> ops.of_calc (Values.Num n))
    | Values.Var var -> ops.of_var var
    | calc -> ops.of_calc calc

  let simplify_calc_value ops simplify simplify_calc ~authored ~visited calc =
    let preserve = authored && contains_var calc in
    simplify_calc ~preserve ~visited calc
    |> calc_result_to_value ops simplify ~visited

  let simplify_plain_value ops simplify simplify_calc ~visited value =
    let simplify_authored = simplify ~authored:true in
    let simplify_calc_authored = simplify_calc ~preserve:false in
    let value =
      ops.simplify_leaf simplify_authored simplify_calc_authored ~visited value
    in
    ops.normalize_value value

  let simplify_calc_non_var ops simplify simplify_calc ~authored ~visited value
      =
    match ops.as_calc value with
    | Some calc ->
        simplify_calc_value ops simplify simplify_calc ~authored ~visited calc
    | None -> simplify_plain_value ops simplify simplify_calc ~visited value

  let simplify_calc_var ops resolve_var simplify_var_record simplify_resolved
      ~visited var =
    match resolve_var ~simplify:simplify_resolved ~visited var with
    | Some value -> value
    | None ->
        ops.of_var
          (simplify_var_record ~simplify:simplify_resolved ~visited var)

  let run_simplify ops ~resolve_var ~simplify_var_record value =
    let rec calc_of_value ~visited value =
      match simplify ~authored:false ~visited value |> ops.as_calc with
      | Some inner -> Values.Nested (simplify_calc ~visited inner)
      | None -> Values.Val (simplify ~authored:false ~visited value)
    and walk_calc ~visited calc =
      let simplify_resolved ~authored:_ = simplify ~authored:false in
      match calc with
      | Values.Val value -> calc_of_value ~visited value
      | Values.Var var -> (
          match resolve_var ~simplify:simplify_resolved ~visited var with
          | Some value -> calc_of_value ~visited value
          | None ->
              Values.Var
                (simplify_var_record ~simplify:simplify_resolved ~visited var))
      | Values.Num _ as leaf -> leaf
      | Values.Math_const _ as leaf -> leaf
      | Values.Math_fn _ as leaf -> leaf
      | Values.Sibling_index -> Values.Sibling_index
      | Values.Sibling_count -> Values.Sibling_count
      | Values.Nested inner -> Values.Nested (walk_calc ~visited inner)
      | Values.Parens inner -> Values.Parens (walk_calc ~visited inner)
      | Values.Expr (left, op, right) ->
          Values.Expr (walk_calc ~visited left, op, walk_calc ~visited right)
    and simplify_calc ?(preserve = false) ~visited calc =
      let calc = walk_calc ~visited calc in
      if preserve && contains_var calc then fold_calc_identities ops calc
      else fold_calc ops calc
    and simplify ~authored ~visited value =
      let simplify_resolved ~authored:_ = simplify ~authored:false in
      let run_calc ~preserve ~visited calc =
        simplify_calc ~preserve ~visited calc
      in
      match ops.as_var value with
      | Some var ->
          simplify_calc_var ops resolve_var simplify_var_record
            simplify_resolved ~visited var
      | None ->
          simplify_calc_non_var ops simplify run_calc ~authored ~visited value
    in
    simplify ~authored:true ~visited:[] value

  let simplify (type a) ?resolve_fallback ?layer_order ?layer cascade
      (ops : a ops) (value : a) : a =
    Var_residual.with_resolver ?resolve_fallback ?layer_order ?layer cascade
      ~read_custom:ops.read_custom
    @@ fun ~resolve_var ~simplify_var_record ->
    run_simplify ops ~resolve_var ~simplify_var_record value
end

(** {2 Length canonicalisation (CSS Values 4 sec. 6)}

    All length units convert to pixels. Absolute units come from the fixed 1in =
    96px conversion table; font-relative and viewport-relative units require the
    supplied base size. Returns [None] when a length depends on information not
    present in the context (e.g. [ch]/[ex] need glyph metrics, percentages need
    a containing-block size). *)

module Length = struct
  type ctx = {
    root_font_size : float option;
    parent_font_size : float option;
    viewport_width : float option;
    viewport_height : float option;
    container_width : float option;
    container_height : float option;
  }

  (* CSS Media Queries 4 sec. 1.3: relative units in @media/@container resolve
     against root font-size. Pre-fill [parent_font_size]/[root_font_size] with
     the 16px default so [em]/[rem] in queries always have a reference; a fresh
     [Length.ctx] without these rejects relative units. *)
  let media_default =
    {
      root_font_size = Some 16.;
      parent_font_size = Some 16.;
      viewport_width = None;
      viewport_height = None;
      container_width = None;
      container_height = None;
    }

  let in_to_px = 96.
  let cm_to_px = 96. /. 2.54
  let mm_to_px = cm_to_px /. 10.
  let q_to_px = cm_to_px /. 40.
  let pt_to_px = in_to_px /. 72.
  let pc_to_px = in_to_px /. 6.

  let to_px ctx (l : Values.length) : float option =
    let viewport_v fn =
      match (ctx.viewport_width, ctx.viewport_height) with
      | Some w, Some h -> Some (fn w h)
      | _ -> None
    in
    let container_v fn =
      match (ctx.container_width, ctx.container_height) with
      | Some w, Some h -> Some (fn w h)
      | _ -> None
    in
    match l with
    | Px f -> Some f
    | Cm f -> Some (f *. cm_to_px)
    | Mm f -> Some (f *. mm_to_px)
    | Q f -> Some (f *. q_to_px)
    | In f -> Some (f *. in_to_px)
    | Pt f -> Some (f *. pt_to_px)
    | Pc f -> Some (f *. pc_to_px)
    | Rem f -> Option.map (fun b -> f *. b) ctx.root_font_size
    | Em f -> Option.map (fun b -> f *. b) ctx.parent_font_size
    | Vw f | Lvw f | Svw f ->
        Option.map (fun w -> f *. w /. 100.) ctx.viewport_width
    | Vh f | Lvh f | Svh f ->
        Option.map (fun h -> f *. h /. 100.) ctx.viewport_height
    | Dvw f -> Option.map (fun w -> f *. w /. 100.) ctx.viewport_width
    | Dvh f -> Option.map (fun h -> f *. h /. 100.) ctx.viewport_height
    | Vmin f | Lvmin f | Svmin f | Dvmin f ->
        viewport_v (fun w h -> f *. Float.min w h /. 100.)
    | Vmax f | Lvmax f | Svmax f | Dvmax f ->
        viewport_v (fun w h -> f *. Float.max w h /. 100.)
    | Vi f -> Option.map (fun w -> f *. w /. 100.) ctx.viewport_width
    | Vb f -> Option.map (fun h -> f *. h /. 100.) ctx.viewport_height
    | Cqw f | Cqi f -> Option.map (fun w -> f *. w /. 100.) ctx.container_width
    | Cqh f | Cqb f -> Option.map (fun h -> f *. h /. 100.) ctx.container_height
    | Cqmin f -> container_v (fun w h -> f *. Float.min w h /. 100.)
    | Cqmax f -> container_v (fun w h -> f *. Float.max w h /. 100.)
    | Zero -> Some 0.
    | _ -> None

  let media_to_px (l : Values.length) : float option = to_px media_default l

  let normalize_zero = function
    | Values.Px px when Float.equal px 0. -> Values.Zero
    | value -> value

  let rec all_px ctx = function
    | [] -> Some []
    | value :: values -> (
        match (to_px ctx value, all_px ctx values) with
        | Some px, Some pxs -> Some (px :: pxs)
        | _ -> None)

  let of_t (ctx : t) : ctx =
    let unwrap_px = function Some (Values.Px p) -> Some p | _ -> None in
    {
      root_font_size = unwrap_px ctx.root_font_size;
      parent_font_size = unwrap_px ctx.parent_font_size;
      viewport_width = unwrap_px ctx.viewport_width;
      viewport_height = unwrap_px ctx.viewport_height;
      container_width = unwrap_px ctx.container_width;
      container_height = unwrap_px ctx.container_height;
    }

  (* CSS Values 4 sec. 10.11 simplification of a typed [length calc]: fold every
     subtree the [ctx] can collapse to absolute lengths, leaving
     [Var]/[Sibling_*]/unresolvable subtrees. The result stays a [length calc] -
     [Val (Px _)] when fully reducible, else [Expr] with simplified operands. *)
  let eval_combine_lengths ctx la lb (op : Values.calc_op) :
      Values.length option =
    match (to_px ctx la, to_px ctx lb, op) with
    | Some pa, Some pb, Add -> Some (Px (pa +. pb))
    | Some pa, Some pb, Sub -> Some (Px (pa -. pb))
    | _ -> None

  let eval_combine_length_num ctx l n (op : Values.calc_op) :
      Values.length option =
    match (to_px ctx l, op) with
    | Some p, Mul -> Some (Px (p *. n))
    | Some p, Div when n <> 0. -> Some (Px (p /. n))
    | _ -> None

  let eval_num_expr (l : Values.length Values.calc) op
      (r : Values.length Values.calc) : Values.length Values.calc option =
    let open Values in
    match (l, op, r) with
    | Num a, Add, Num b -> Some (Num (a +. b))
    | Num a, Sub, Num b -> Some (Num (a -. b))
    | Num a, Mul, Num b -> Some (Num (a *. b))
    | Num a, Div, Num b when b <> 0. -> Some (Num (a /. b))
    | _ -> None

  let eval_value_expr ctx (l : Values.length Values.calc) op
      (r : Values.length Values.calc) : Values.length Values.calc option =
    let open Values in
    match (l, op, r) with
    | Val la, _, Val lb ->
        Option.map (fun out -> Val out) (eval_combine_lengths ctx la lb op)
    | Val la, _, Num n ->
        Option.map (fun out -> Val out) (eval_combine_length_num ctx la n op)
    | Num n, Mul, Val lb ->
        (* Multiplication is commutative on length x number. *)
        Option.map (fun out -> Val out) (eval_combine_length_num ctx lb n Mul)
    | _ -> None

  let eval_calc_expr_values ctx (l : Values.length Values.calc) op
      (r : Values.length Values.calc) : Values.length Values.calc =
    let open Values in
    match eval_num_expr l op r with
    | Some folded -> folded
    | None -> (
        match eval_value_expr ctx l op r with
        | Some folded -> folded
        | None -> Expr (l, op, r))

  let eval_calc_wrapper_value wrap reduced =
    let open Values in
    match reduced with
    | (Val _ | Num _ | Math_const _ | Var _) as leaf -> leaf
    | reduced -> wrap reduced

  let eval_calc ctx (calc : Values.length Values.calc) :
      Values.length Values.calc =
    let rec eval (calc : Values.length Values.calc) : Values.length Values.calc
        =
      let open Values in
      match calc with
      | Num _ | Val _ | Var _ | Math_const _ | Math_fn _ | Sibling_index
      | Sibling_count ->
          calc
      | Nested inner -> eval_calc_wrapper_value (fun v -> Nested v) (eval inner)
      | Parens inner -> eval_calc_wrapper_value (fun v -> Parens v) (eval inner)
      | Expr (l, op, r) -> eval_calc_expr_values ctx (eval l) op (eval r)
    in
    eval calc

  let simplify_math_args simplify ~visited args =
    List.map (simplify ~visited) args

  let simplify_length_min ctx simplify ~visited args =
    let args = simplify_math_args simplify ~visited args in
    match all_px ctx args with
    | Some (first :: rest) ->
        normalize_zero (Values.Px (List.fold_left Float.min first rest))
    | _ -> Values.Min args

  let simplify_length_max ctx simplify ~visited args =
    let args = simplify_math_args simplify ~visited args in
    match all_px ctx args with
    | Some (first :: rest) ->
        normalize_zero (Values.Px (List.fold_left Float.max first rest))
    | _ -> Values.Max args

  let simplify_length_clamp ctx simplify ~visited (min, preferred, max) =
    let args = simplify_math_args simplify ~visited [ min; preferred; max ] in
    match (all_px ctx args, args) with
    | Some [ min; preferred; max ], _ ->
        normalize_zero (Values.Px (Float.max min (Float.min preferred max)))
    | _, [ min; preferred; max ] -> Values.Clamp (min, preferred, max)
    | _ -> Values.Clamp (min, preferred, max)

  let simplify ?(preserve_authored_calc = true) ?layer_order ?layer cascade ctx
      value =
    let simplify_leaf simplify simplify_calc ~visited value =
      match value with
      | Values.Min args -> simplify_length_min ctx simplify ~visited args
      | Values.Max args -> simplify_length_max ctx simplify ~visited args
      | Values.Clamp (mn, v, mx) ->
          simplify_length_clamp ctx simplify ~visited (mn, v, mx)
      | Values.Fit_content_arg value ->
          Values.Fit_content_arg (simplify ~visited value)
      | Values.Round (strategy, value, step) ->
          Values.Round
            (strategy, simplify ~visited value, simplify ~visited step)
      | Values.Mod (left, right) ->
          Values.Mod (simplify ~visited left, simplify ~visited right)
      | Values.Rem_fn (left, right) ->
          Values.Rem_fn (simplify ~visited left, simplify ~visited right)
      | Values.Hypot values ->
          Values.Hypot (List.map (simplify ~visited) values)
      | Values.Abs value -> Values.Abs (simplify ~visited value)
      | Values.Sign value -> Values.Sign (simplify ~visited value)
      | Values.Calc_size (basis, calc) ->
          Values.Calc_size (simplify ~visited basis, simplify_calc ~visited calc)
      | Values.Anchor (name, side, fallback) ->
          Values.Anchor (name, side, Option.map (simplify ~visited) fallback)
      | value -> value
    in
    let ops : Values.length Calc_residual.ops =
      let to_number = to_px ctx in
      let of_number px =
        if Float.equal px 0. then Values.Zero else Values.Px px
      in
      {
        of_unitless_number = (fun _ -> None);
        zero = Values.Val Values.Zero;
        is_zero = Values.length_is_zero;
        combine_values = combine_numeric_values ~to_number ~of_number;
        combine_value_num = combine_numeric_value_num ~to_number ~of_number;
        normalize_value = normalize_numeric_value ~to_number ~of_number;
        as_var = (function Values.Var var -> Some var | _ -> None);
        of_var = (fun var -> Values.Var var);
        as_calc = (function Values.Calc calc -> Some calc | _ -> None);
        of_calc = (fun calc -> Values.Calc calc);
        read_custom = read_custom_value Properties.Length Values.read_length;
        simplify_leaf;
      }
    in
    let resolve_fallback (value : Values.length) =
      match value with Values.Revert_layer -> false | _ -> true
    in
    let preserve_authored_calc =
      preserve_authored_calc
      &&
      match (value : Values.length) with
      | Values.Calc calc -> Calc_residual.contains_var calc
      | _ -> false
    in
    let value : Values.length =
      Calc_residual.simplify ~resolve_fallback ?layer_order ?layer cascade ops
        value
    in
    match value with
    | Values.Calc _ when preserve_authored_calc -> value
    | Values.Calc calc -> (
        match eval_calc ctx calc with
        | Values.Val value -> normalize_zero value
        | calc -> Values.Calc calc)
    | value -> normalize_zero value
end

(** {2 Media-feature value comparison (CSS Media Queries 4 sec. 3)}

    [query.media_features] stores typed [Media.feature]s parsed at construction;
    this module is just the numeric comparison glue used when matching range
    queries against those typed values. *)

module Media_value = struct
  let to_number (v : Media.value) : float option =
    match v with
    | Length l -> Length.media_to_px l
    | Integer i -> Some (float_of_int i)
    | Number n -> Some n
    | Ratio (a, b) when b <> 0 -> Some (float_of_int a /. float_of_int b)
    | Ratio _ -> None
    | Resolution_value (n, unit) -> (
        match String.lowercase_ascii unit with
        | "dppx" | "x" -> Some n
        | "dpi" -> Some (n /. 96.)
        | "dpcm" -> Some (n *. 2.54 /. 96.)
        | _ -> None)
    | Ident _ | Function _ -> None

  let cmp_op : Media.cmp -> float -> float -> bool = function
    | Lt -> ( < )
    | Le -> ( <= )
    | Eq -> ( = )
    | Gt -> ( > )
    | Ge -> ( >= )

  (* Compare two media values. [None] when the comparison cannot be decided
     (mixed kinds, missing length context). *)
  let compare_with op (lhs : Media.value) (rhs : Media.value) : bool option =
    match (lhs, rhs) with
    | Ident a, Ident b ->
        Some (op = Media.Eq && Media.string_of_ident a = Media.string_of_ident b)
    | _ -> (
        match (to_number lhs, to_number rhs) with
        | Some la, Some lb -> Some (cmp_op op la lb)
        | _ -> None)
end

(** {2 [@supports] evaluation (CSS Conditional 3 sec. 6)}

    Each [Supports.t] constructor has a dedicated evaluator. The structural
    cases ([Not]/[And]/[Or]) recurse into [eval]; the leaves ([Property]/[Func])
    consult the explicit declaration/function tables. *)

module Match_supports = struct
  (* Leaves match against [q.supports] via the typed [Supports.equal]. *)
  let rec eval q : Supports.t -> bool = function
    | (Property _ | Function _) as leaf ->
        List.exists (Supports.equal leaf) q.supports
    | Not c -> not (eval q c)
    | And (a, b) -> eval q a && eval q b
    | Or (a, b) -> eval q a || eval q b
end

(** {2 [@media] evaluation (CSS Media Queries 4)}

    Each [Media] AST type has its own evaluator: [eval_feature] for
    [Media.feature], [eval_condition] for [Media.condition], [eval_query] for
    [Media.query], [eval_medium] for [Media.medium], and [eval] for the
    typed-shorthand wrapper [Media.t]. Lengths resolve against a 16px base per
    sec. 1.3; [min-]/[max-] feature prefixes desugar to range comparisons. *)

module Match_media = struct
  open Media

  type feature_table = Media.t list
  type truth = True | False | Unknown

  let of_bool = function true -> True | false -> False
  let of_option = function Some value -> of_bool value | None -> Unknown
  let negate = function True -> False | False -> True | Unknown -> Unknown

  let conjunction a b =
    match (a, b) with
    | False, _ | _, False -> False
    | True, True -> True
    | Unknown, _ | _, Unknown -> Unknown

  let disjunction a b =
    match (a, b) with
    | True, _ | _, True -> True
    | False, False -> False
    | Unknown, _ | _, Unknown -> Unknown

  let strip_min_max = function
    | Media.Min name -> Some (`Min, name)
    | Media.Max name -> Some (`Max, name)
    | _ -> None

  let lookup_value (table : feature_table) feature_name : Media.value option =
    List.find_map (media_feature_value feature_name) table

  let boolean_value : Media.value -> bool option = function
    | Ident (None | No_preference) -> Some false
    | Ident _ -> Some true
    | value -> Option.map (fun n -> n <> 0.) (Media_value.to_number value)

  let eval_feature q : Media.feature -> truth =
    let with_lookup name f =
      if List.exists (Media.equal_name name) q.media_inapplicable then False
      else
        match lookup_value q.media_features (Media.string_of_name name) with
        | None -> Unknown
        | Some actual -> of_option (f actual)
    in
    function
    (* MQ4 section 3.1: preserve unknown until the whole query is evaluated. *)
    | General_enclosed _ -> Unknown
    | Boolean name -> with_lookup name boolean_value
    | Plain (name, value) -> (
        match strip_min_max name with
        | Some (`Min, base) ->
            with_lookup base (fun a -> Media_value.compare_with Ge a value)
        | Some (`Max, base) ->
            with_lookup base (fun a -> Media_value.compare_with Le a value)
        | None ->
            with_lookup name (fun a -> Media_value.compare_with Eq a value))
    | Range (name, op, value) ->
        with_lookup name (fun a -> Media_value.compare_with op a value)
    | Range_rev (value, op, name) ->
        with_lookup name (fun a -> Media_value.compare_with op value a)
    | Interval (lo, lo_op, name, hi_op, hi) ->
        with_lookup name (fun a ->
            match
              ( Media_value.compare_with lo_op lo a,
                Media_value.compare_with hi_op a hi )
            with
            | Some x, Some y -> Some (x && y)
            | _ -> None)

  let eval_medium q : Media.medium -> bool = function
    | All -> true
    | Screen -> q.media_type = Some "screen"
    | Print -> q.media_type = Some "print"
    | Other s -> q.media_type = Some s

  let rec eval_condition q : Media.condition -> truth = function
    | Feature f -> eval_feature q f
    | Not c -> negate (eval_condition q c)
    | And (a, b) -> conjunction (eval_condition q a) (eval_condition q b)
    | Or (a, b) -> disjunction (eval_condition q a) (eval_condition q b)

  let rec eval_query q : Media.t -> truth = function
    | Cond c -> eval_condition q c
    | Type { prefix; type_; trailing } -> (
        let body =
          conjunction
            (of_bool (eval_medium q type_))
            (Option.fold ~none:True ~some:(eval_condition q) trailing)
        in
        match prefix with Some Media.Not -> negate body | _ -> body)
    | List [] -> True
    | List qs ->
        List.fold_left
          (fun acc query -> disjunction acc (eval_query q query))
          False qs

  let eval q query = eval_query q query = True
end

(** {2 Container-query evaluation (CSS Conditional 5 sec. 5.4)}

    Container queries reuse the media-feature evaluator applied to the container
    feature table; they additionally guard on the container name (when supplied)
    and accept [style()] / [scroll-state()] queries that the media evaluator
    does not know about. *)

module Match_container = struct
  let name_matches ?name q =
    match (name, q.container_name) with
    | None, _ -> true
    | Some n, Some actual -> String.equal n actual
    | Some _, None -> false

  (* Project [container_features] entries that look like size/range features
     into [Media.t]s so the [Match_media] evaluator can range-compare them.
     [Style _] / [Scroll_state _] are handled directly. *)
  let media_features_of q =
    List.filter_map
      (function Container.Feature_query f -> Some f | _ -> None)
      q.container_features

  let style_value = function
    | Container.Boolean name -> (name, None)
    | Container.Declaration { name; value } ->
        (name, Some (Cursor.string_of_components ~trim:true value))
    | Container.Range _ | Container.All _ | Container.Any _ | Container.Neg _ ->
        ("", None)

  let style_leaf_match q ~query =
    let prop, value = style_value query in
    List.exists
      (function
        | Container.Style { query = Range _ | All _ | Any _ | Neg _; _ } ->
            false
        | Container.Style { query; _ }
          when String.equal (fst (style_value query)) prop -> (
            match (value, snd (style_value query)) with
            | None, _ -> true (* any style(prop) match: present in any form *)
            | Some _, None -> false
            | Some asked, Some actual ->
                String.equal (String.trim asked) (String.trim actual))
        | _ -> false)
      q.container_features

  let rec style_match q ~query =
    match query with
    | Container.Boolean _ | Container.Declaration _ | Container.Range _ ->
        style_leaf_match q ~query
    | Container.All (a, b) -> style_match q ~query:a && style_match q ~query:b
    | Container.Any (a, b) -> style_match q ~query:a || style_match q ~query:b
    | Container.Neg query -> not (style_match q ~query)

  let rec eval_scroll_state_query q = function
    | Container.State { name = prop; value } ->
        List.exists
          (function
            | Container.Scroll_state
                { query = State { name = p; value = v }; _ } ->
                String.equal p prop && String.equal v value
            | _ -> false)
          q.container_features
    | Both (a, b) -> eval_scroll_state_query q a && eval_scroll_state_query q b
    | Either (a, b) ->
        eval_scroll_state_query q a || eval_scroll_state_query q b
    | Negated query -> not (eval_scroll_state_query q query)

  let eval_scroll_state q ~query =
    List.exists
      (function
        | Container.Scroll_state { query = actual; _ } ->
            Container.compare_scroll_state_query actual query = 0
        | _ -> false)
      q.container_features
    || eval_scroll_state_query q query

  let rec feature_available table = function
    | Media.Min name | Media.Max name -> feature_available table name
    | name ->
        Option.is_some
          (Match_media.lookup_value table (Media.string_of_name name))

  let rec media_available table : Media.condition -> bool = function
    | Feature (General_enclosed _) -> false
    | Feature
        ( Plain (name, _)
        | Boolean name
        | Range (name, _, _)
        | Range_rev (_, _, name)
        | Interval (_, _, name, _, _) ) ->
        feature_available table name
    | Not condition -> media_available table condition
    | And (a, b) | Or (a, b) ->
        media_available table a && media_available table b

  (* Conditional Rules 5 section 5.4: all features must select the same eligible
     container, even when an [or] branch would already be true. *)
  let rec eligible q table : Container.t -> bool = function
    | Min_width_rem _ | Min_width_px _ -> feature_available table Media.Width
    | Feature_query (Media.Cond condition) -> media_available table condition
    | Feature_query (Media.Type _ | Media.List _) -> false
    | Named (name, condition) ->
        name_matches ~name q && eligible q table condition
    | And (a, b) | Or (a, b) -> eligible q table a && eligible q table b
    | Not condition -> eligible q table condition
    | Style _ -> true
    | Scroll_state _ ->
        List.exists
          (function Container.Scroll_state _ -> true | _ -> false)
          q.container_features

  let rec eval_condition q : Container.t -> Match_media.truth =
    let media_q =
      { q with media_features = media_features_of q; media_inapplicable = [] }
    in
    fun cond ->
      let min_width l =
        Media.Cond
          (Media.Feature (Media.Plain (Media.Min Media.Width, Media.Length l)))
      in
      match cond with
      | Min_width_rem rem ->
          Match_media.eval_query media_q (min_width (Values.Rem rem))
      | Min_width_px px ->
          Match_media.eval_query media_q
            (min_width (Values.Px (float_of_int px)))
      | Named (_, inner) -> eval_condition q inner
      | Style { query; _ } -> Match_media.of_bool (style_match q ~query)
      | Scroll_state { query; _ } ->
          Match_media.of_bool (eval_scroll_state q ~query)
      | And (a, b) ->
          Match_media.conjunction (eval_condition q a) (eval_condition q b)
      | Or (a, b) ->
          Match_media.disjunction (eval_condition q a) (eval_condition q b)
      | Not c -> Match_media.negate (eval_condition q c)
      | Feature_query media -> Match_media.eval_query media_q media

  let eval q ?name condition =
    name_matches ?name q
    && eligible q (media_features_of q) condition
    && eval_condition q condition = Match_media.True
end

(** {2 Selector matching (CSS Selectors 4)}

    Browsers match selectors right-to-left because the rightmost compound fixes
    the candidate element. Without a tree the matcher reduces to that rightmost
    subject; combinators always succeed against the document so callers should
    only request selector matching when they have already decided which element
    they are testing. *)

module Match_selector = struct
  let attr_name : Selector.attr_name -> string = function
    | Aria a -> Aria.to_string a
    | Data s -> "data-" ^ s
    | Regular s -> s

  let value_match (matcher : Selector.attribute_match) ~flag actual =
    let normalize =
      match flag with
      | Some Selector.Insensitive -> String.lowercase_ascii
      | _ -> Fun.id
    in
    let actual = normalize actual in
    let words s =
      String.split_on_char ' ' s |> List.filter (fun s -> s <> "")
    in
    match matcher with
    | Presence -> true
    | Exact v | Exact_quoted (v, _) -> String.equal actual (normalize v)
    | Whitespace_list v | Whitespace_list_quoted (v, _) ->
        let v = normalize v in
        List.exists (String.equal v) (words actual)
    | Hyphen_list v | Hyphen_list_quoted (v, _) ->
        let v = normalize v in
        String.equal actual v
        || String.length actual > String.length v
           && String.sub actual 0 (String.length v) = v
           && actual.[String.length v] = '-'
    | Prefix v | Prefix_quoted (v, _) ->
        let v = normalize v in
        String.length actual >= String.length v
        && String.sub actual 0 (String.length v) = v
    | Suffix v | Suffix_quoted (v, _) ->
        let v = normalize v in
        let la = String.length actual and lv = String.length v in
        la >= lv && String.sub actual (la - lv) lv = v
    | Substring v | Substring_quoted (v, _) ->
        let v = normalize v in
        let la = String.length actual and lv = String.length v in
        let rec scan i =
          if i + lv > la then false
          else if String.sub actual i lv = v then true
          else scan (i + 1)
        in
        lv = 0 || scan 0

  let rec eval (doc : document) (sel : Selector.t) : bool =
    match sel with
    | Universal _ -> true
    | Element (_, name) -> doc.element = Some name
    | Class name -> List.exists (String.equal name) doc.classes
    | Id name -> List.exists (String.equal name) doc.ids
    | Attribute (_, name, matcher, flag) -> (
        match List.assoc_opt (attr_name name) doc.attributes with
        | None -> false
        | Some None -> matcher = Presence
        | Some (Some v) -> value_match matcher ~flag v)
    | Compound parts -> List.for_all (eval doc) parts
    | List alts -> List.exists (eval doc) alts
    | Is alts | Where alts -> List.exists (eval doc) alts
    | Not alts -> not (List.exists (eval doc) alts)
    | Combined (_, _, right) ->
        (* Combinators need a tree we do not have; collapse to the rightmost
           subject so the matcher answers questions about the [doc] element. *)
        eval doc right
    | Scope -> Option.is_some doc.scope
    | Root -> Option.is_some doc.root
    | Nesting -> true
    | Hover -> List.mem "hover" doc.pseudo_classes
    | Active -> List.mem "active" doc.pseudo_classes
    | Focus -> List.mem "focus" doc.pseudo_classes
    | Focus_visible -> List.mem "focus-visible" doc.pseudo_classes
    | Focus_within -> List.mem "focus-within" doc.pseudo_classes
    | Target -> List.mem "target" doc.pseudo_classes
    | Link -> List.mem "link" doc.pseudo_classes
    | Visited -> List.mem "visited" doc.pseudo_classes
    | Any_link -> List.mem "any-link" doc.pseudo_classes
    | Empty -> List.mem "empty" doc.pseudo_classes
    | First_child -> List.mem "first-child" doc.pseudo_classes
    | Last_child -> List.mem "last-child" doc.pseudo_classes
    | Only_child -> List.mem "only-child" doc.pseudo_classes
    | First_of_type -> List.mem "first-of-type" doc.pseudo_classes
    | Last_of_type -> List.mem "last-of-type" doc.pseudo_classes
    | Only_of_type -> List.mem "only-of-type" doc.pseudo_classes
    | Enabled -> List.mem "enabled" doc.pseudo_classes
    | Disabled -> List.mem "disabled" doc.pseudo_classes
    | Read_only -> List.mem "read-only" doc.pseudo_classes
    | Read_write -> List.mem "read-write" doc.pseudo_classes
    | Placeholder_shown -> List.mem "placeholder-shown" doc.pseudo_classes
    | Default -> List.mem "default" doc.pseudo_classes
    | Checked -> List.mem "checked" doc.pseudo_classes
    | Indeterminate -> List.mem "indeterminate" doc.pseudo_classes
    | Valid -> List.mem "valid" doc.pseudo_classes
    | Invalid -> List.mem "invalid" doc.pseudo_classes
    | In_range -> List.mem "in-range" doc.pseudo_classes
    | Out_of_range -> List.mem "out-of-range" doc.pseudo_classes
    | Required -> List.mem "required" doc.pseudo_classes
    | Optional -> List.mem "optional" doc.pseudo_classes
    | Open -> List.mem "open" doc.pseudo_classes
    | Popover_open -> List.mem "popover-open" doc.pseudo_classes
    | Before _ -> List.mem "before" doc.pseudo_elements
    | After _ -> List.mem "after" doc.pseudo_elements
    | First_letter _ -> List.mem "first-letter" doc.pseudo_elements
    | First_line _ -> List.mem "first-line" doc.pseudo_elements
    | Backdrop -> List.mem "backdrop" doc.pseudo_elements
    | Marker -> List.mem "marker" doc.pseudo_elements
    | Placeholder -> List.mem "placeholder" doc.pseudo_elements
    | Selection -> List.mem "selection" doc.pseudo_elements
    | _ -> false
end

(** {2 URL resolution (RFC 3986)} *)

module Url = struct
  let resolve loader href =
    let reference = Uri.of_string href in
    match (Uri.scheme reference, loader.base_url) with
    | Some _, _ -> Ok (Uri.to_string reference)
    | None, None -> Error ("no base URL to resolve " ^ href)
    | None, Some base ->
        Ok (Uri.resolve "" (Uri.of_string base) reference |> Uri.to_string)
end

(** {2 [@import] loader (CSS Cascade 5 sec. 2)}

    Resolves the import URL through {!Url}, looks up the body in
    [loader.imports], parses it, and applies any media/supports/layer guards on
    the rule. *)

module Import = struct
  let layer_known ~layer_order = function
    | None -> true
    | Some name -> List.mem (Stylesheet.string_of_layer_name name) layer_order

  let supports_ok ?query rule =
    match ((rule : Stylesheet.import_rule).supports, query) with
    | None, _ -> true
    | Some _, None -> false
    | Some cond, Some q -> Match_supports.eval q cond

  let media_ok ?query rule =
    match ((rule : Stylesheet.import_rule).media, query) with
    | None, _ -> true
    | Some _, None -> false
    | Some media, Some q -> Match_media.eval q media

  let wrap_in_layer rule statements =
    match (rule : Stylesheet.import_rule).layer with
    | None -> statements
    | Some name -> [ Stylesheet.Layer (Some name, statements) ]

  let parse_source rule source =
    try
      let cursor = Cursor.of_string source in
      let sheet = Stylesheet.read cursor in
      Ok (wrap_in_layer rule sheet)
    with
    | Failure msg -> Error msg
    | Cursor.Parse_error _ -> Error "stylesheet parse error"

  let load_resolved loader rule resolved =
    match List.assoc_opt resolved loader.imports with
    | None -> Error ("import not in loader table: " ^ resolved)
    | Some source -> parse_source rule source

  let guards_ok ?query ~layer_order rule =
    let layer_name = (rule : Stylesheet.import_rule).layer in
    if not (layer_known ~layer_order layer_name) then
      Error
        ("unknown layer "
        ^ Option.fold ~none:"" ~some:Stylesheet.string_of_layer_name layer_name
        )
    else if not (supports_ok ?query rule) then
      Error "supports() guard rejected the import"
    else if not (media_ok ?query rule) then
      Error "media guard rejected the import"
    else Ok ()

  let load ?query ?(layer_order = []) loader (rule : Stylesheet.import_rule) =
    match guards_ok ?query ~layer_order rule with
    | Error _ as e -> e
    | Ok () -> (
        match Url.resolve loader rule.url with
        | Error _ as e -> e
        | Ok resolved -> load_resolved loader rule resolved)
end

(** {2 Public API surface (forwards to the internal modules)} *)

let matches_supports = Match_supports.eval
let matches_media = Match_media.eval
let matches_container = Match_container.eval
let matches_selector = Match_selector.eval
let resolve_url = Url.resolve
let load_import = Import.load

let simplify_length_percentage ?layer_order ?layer cascade length_ctx value =
  let to_px = function
    | Values.Length length -> Length.to_px length_ctx length
    | _ -> None
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ = function
    | Values.Length length ->
        Values.Length
          (Length.simplify ?layer_order ?layer cascade length_ctx length)
    | value -> value
  in
  let ops =
    let of_number px =
      Values.Length (if Float.equal px 0. then Values.Zero else Values.Px px)
    in
    let combine_values (left : Values.length_percentage) op
        (right : Values.length_percentage) =
      match (left, op, right) with
      | Values.Pct a, Values.Add, Values.Pct b ->
          Some (Values.Pct (a +. b) : Values.length_percentage)
      | Values.Pct a, Values.Sub, Values.Pct b ->
          Some (Values.Pct (a -. b) : Values.length_percentage)
      | _ -> combine_numeric_values ~to_number:to_px ~of_number left op right
    in
    let combine_value_num (value : Values.length_percentage) op num =
      match (value, op) with
      | Values.Pct n, Values.Mul ->
          Some (Values.Pct (n *. num) : Values.length_percentage)
      | Values.Pct n, Values.Div when num <> 0. ->
          Some (Values.Pct (n /. num) : Values.length_percentage)
      | _ -> combine_numeric_value_num ~to_number:to_px ~of_number value op num
    in
    {
      Calc_residual.of_unitless_number = (fun _ -> None);
      zero = Values.Val (Values.Length Values.Zero);
      is_zero = (fun v -> match to_px v with Some n -> n = 0. | None -> false);
      combine_values;
      combine_value_num;
      normalize_value = normalize_numeric_value ~to_number:to_px ~of_number;
      as_var = (function Values.Var var -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc = (function Values.Calc calc -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom =
        read_custom_value Properties.Length_percentage
          (Values.read_length_percentage ~with_keywords:true);
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let simplify_border_width ?layer_order ?layer cascade (length_ctx : Length.ctx)
    value =
  let to_number (value : Properties.border_width) =
    match value with
    | Properties.Px n -> Some n
    | Properties.Rem n -> Option.map (fun b -> n *. b) length_ctx.root_font_size
    | Properties.Em n ->
        Option.map (fun b -> n *. b) length_ctx.parent_font_size
    | Properties.Vw n ->
        Option.map (fun w -> n *. w /. 100.) length_ctx.viewport_width
    | Properties.Vh n ->
        Option.map (fun h -> n *. h /. 100.) length_ctx.viewport_height
    | Properties.Vmin n -> (
        match (length_ctx.viewport_width, length_ctx.viewport_height) with
        | Some w, Some h -> Some (n *. Float.min w h /. 100.)
        | _ -> None)
    | Properties.Vmax n -> (
        match (length_ctx.viewport_width, length_ctx.viewport_height) with
        | Some w, Some h -> Some (n *. Float.max w h /. 100.)
        | _ -> None)
    | Properties.Zero -> Some 0.
    | _ -> None
  in
  let of_number px = (Properties.Px px : Properties.border_width) in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Properties.border_width Calc_residual.ops =
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function
        | (Properties.Var var : Properties.border_width) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      as_calc =
        (function
        | (Properties.Calc calc : Properties.border_width) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Properties.Calc calc);
      read_custom = read_custom_components Properties.read_border_width;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let simplify_font_size ?layer_order ?layer cascade (length_ctx : Length.ctx)
    value =
  let length = Length.simplify ?layer_order ?layer cascade length_ctx in
  let to_number (value : Properties.font_size) =
    match value with
    | Properties.Length (Values.Pct n) ->
        Option.map (fun basis -> basis *. n /. 100.) length_ctx.parent_font_size
    | Properties.Length length -> Length.to_px length_ctx length
    | Properties.Pct n ->
        Option.map (fun basis -> basis *. n /. 100.) length_ctx.parent_font_size
    | _ -> None
  in
  let of_number px =
    (Properties.Length (Length.normalize_zero (Values.Px px))
      : Properties.font_size)
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_
      (value : Properties.font_size) =
    match value with
    | Properties.Length value ->
        (Properties.Length (length value) : Properties.font_size)
    | value -> value
  in
  let ops : Properties.font_size Calc_residual.ops =
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function
        | (Properties.Var var : Properties.font_size) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      as_calc =
        (function
        | (Properties.Calc calc : Properties.font_size) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Properties.Calc calc);
      read_custom =
        read_custom_value
          (Properties.Font_size : Properties.font_size Properties.kind)
          Properties.read_font_size;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

(* Only the <length-percentage> branch carries anything to simplify: a bare
   number is already a user-unit width. *)
let simplify_stroke_width ?layer_order ?layer cascade length_ctx
    (value : Properties.stroke_width) : Properties.stroke_width =
  match value with
  | Length lp ->
      let lp' =
        simplify_length_percentage ?layer_order ?layer cascade length_ctx lp
      in
      if lp' == lp then value else Length lp'
  | _ -> value

let simplify_opacity ?layer_order ?layer cascade value =
  let to_number = function
    | Properties.Opacity_number n -> Some n
    | _ -> None
  in
  let simplify_leaf simplify _simplify_calc ~visited (leaf : Properties.opacity)
      : Properties.opacity =
    match leaf with
    | Abs value -> Abs (simplify ~visited value)
    | Sign value -> Sign (simplify ~visited value)
    | value -> value
  in
  let ops : Properties.opacity Calc_residual.ops =
    let of_number n = Properties.Opacity_number n in
    {
      of_unitless_number = (fun n -> Some (Properties.Opacity_number n));
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function
        | (Properties.Var var : Properties.opacity) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      as_calc =
        (function
        | (Properties.Calc calc : Properties.opacity) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Properties.Calc calc);
      read_custom =
        read_custom_value
          (Properties.Opacity : Properties.opacity Properties.kind)
          Properties.read_opacity;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let degrees_of_angle (value : Values.angle) =
  let pi = 4. *. atan 1. in
  match value with
  | Values.Deg n -> Some n
  | Values.Rad n -> Some (n *. 180. /. pi)
  | Values.Turn n -> Some (n *. 360.)
  | Values.Grad n -> Some (n *. 0.9)
  | _ -> None

let simplify_angle ?layer_order ?layer cascade value =
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.angle Calc_residual.ops =
    let to_number = degrees_of_angle in
    let of_number deg = Values.Deg deg in
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function (Values.Var var : Values.angle) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function (Values.Calc calc : Values.angle) -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_value Properties.Angle Values.read_angle;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let seconds_of_duration (value : Values.duration) =
  match value with
  | Values.S n -> Some n
  | Values.Ms n -> Some (n /. 1000.)
  | _ -> None

let simplify_duration ?layer_order ?layer cascade value =
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.duration Calc_residual.ops =
    let to_number = seconds_of_duration in
    let of_number seconds = Values.S seconds in
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function (Values.Var var : Values.duration) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function
        | (Values.Calc calc : Values.duration) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_value Properties.Duration Values.read_duration;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let number_percentage_num n : Values.number_percentage = Values.Num n
let number_percentage_pct n : Values.number_percentage = Values.Pct n

let combine_number_percentage_values (left : Values.number_percentage) op
    (right : Values.number_percentage) =
  match (left, op, right) with
  | Values.Num a, Values.Add, Values.Num b ->
      Some (number_percentage_num (a +. b))
  | Values.Num a, Values.Sub, Values.Num b ->
      Some (number_percentage_num (a -. b))
  | Values.Pct a, Values.Add, Values.Pct b ->
      Some (number_percentage_pct (a +. b))
  | Values.Pct a, Values.Sub, Values.Pct b ->
      Some (number_percentage_pct (a -. b))
  | _ -> None

let combine_number_percentage_value_num (value : Values.number_percentage) op
    num =
  match (value, op) with
  | Values.Num a, Values.Mul -> Some (number_percentage_num (a *. num))
  | Values.Num a, Values.Div when num <> 0. ->
      Some (number_percentage_num (a /. num))
  | Values.Pct a, Values.Mul -> Some (number_percentage_pct (a *. num))
  | Values.Pct a, Values.Div when num <> 0. ->
      Some (number_percentage_pct (a /. num))
  | _ -> None

let simplify_number_percentage ?layer_order ?layer cascade value =
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.number_percentage Calc_residual.ops =
    {
      Calc_residual.of_unitless_number =
        (fun n -> Some (number_percentage_num n));
      zero = Values.Num 0.;
      is_zero = (fun _ -> false);
      combine_values = combine_number_percentage_values;
      combine_value_num = combine_number_percentage_value_num;
      normalize_value = (fun value -> value);
      as_var =
        (function
        | (Values.Var var : Values.number_percentage) -> Some var
        | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function
        | (Values.Calc calc : Values.number_percentage) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom =
        read_custom_value Properties.Number_percentage
          Values.read_number_percentage;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let simplify_rotate_value ?layer_order ?layer cascade value =
  let angle = simplify_angle ?layer_order ?layer cascade in
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.rotate_value) =
    match value with
    | Properties.Angle value ->
        (Properties.Angle (angle value) : Properties.rotate_value)
    | Properties.X value -> Properties.X (angle value)
    | Properties.Y value -> Properties.Y (angle value)
    | Properties.Z value -> Properties.Z (angle value)
    | Properties.Axis (x, y, z, value) -> Properties.Axis (x, y, z, angle value)
    | value -> value
  in
  let ops : Properties.rotate_value Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.rotate_value) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Rotate : Properties.rotate_value Properties.kind)
          Properties.read_rotate_value;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_translate_value ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let simplify_leaf _simplify ~authored ~visited:_
      (value : Properties.translate_value) =
    match value with
    | Properties.X value ->
        (Properties.X (length authored value) : Properties.translate_value)
    | Properties.XY (x, y) ->
        (Properties.XY (length authored x, length authored y)
          : Properties.translate_value)
    | Properties.XYZ (x, y, z) ->
        (Properties.XYZ (length authored x, length authored y, length authored z)
          : Properties.translate_value)
    | value -> value
  in
  let ops : Properties.translate_value Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.translate_value) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_translate_value;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_scale ?layer_order ?layer cascade value =
  let number_percentage =
    simplify_number_percentage ?layer_order ?layer cascade
  in
  let simplify_leaf _simplify ~authored:_ ~visited:_ (value : Properties.scale)
      =
    match value with
    | Properties.X value ->
        (Properties.X (number_percentage value) : Properties.scale)
    | Properties.XY (x, y) ->
        (Properties.XY (number_percentage x, number_percentage y)
          : Properties.scale)
    | Properties.XYZ (x, y, z) ->
        (Properties.XYZ
           (number_percentage x, number_percentage y, number_percentage z)
          : Properties.scale)
    | value -> value
  in
  let ops : Properties.scale Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.scale) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Scale : Properties.scale Properties.kind)
          Properties.read_scale;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_translate_transform length authored (value : Properties.transform)
    =
  match value with
  | Properties.Translate (x, y) ->
      (Properties.Translate (length authored x, Option.map (length authored) y)
        : Properties.transform)
  | Properties.Translate_x x ->
      (Properties.Translate_x (length authored x) : Properties.transform)
  | Properties.Translate_y y ->
      (Properties.Translate_y (length authored y) : Properties.transform)
  | Properties.Translate_z z ->
      (Properties.Translate_z (length authored z) : Properties.transform)
  | Properties.Translate_3d (x, y, z) ->
      (Properties.Translate_3d
         (length authored x, length authored y, length authored z)
        : Properties.transform)
  | value -> value

let simplify_rotate_transform angle (value : Properties.transform) =
  match value with
  | Properties.Rotate value ->
      (Properties.Rotate (angle value) : Properties.transform)
  | Properties.Rotate_x value ->
      (Properties.Rotate_x (angle value) : Properties.transform)
  | Properties.Rotate_y value ->
      (Properties.Rotate_y (angle value) : Properties.transform)
  | Properties.Rotate_z value ->
      (Properties.Rotate_z (angle value) : Properties.transform)
  | Properties.Rotate_3d (x, y, z, value) ->
      (Properties.Rotate_3d (x, y, z, angle value) : Properties.transform)
  | Properties.Rotate_axis (x, y, z, value) ->
      (Properties.Rotate_axis (x, y, z, angle value) : Properties.transform)
  | value -> value

let simplify_scale_transform number_percentage (value : Properties.transform) =
  match value with
  | Properties.Scale (x, y) ->
      (Properties.Scale (number_percentage x, Option.map number_percentage y)
        : Properties.transform)
  | Properties.Scale_space (x, y) ->
      (Properties.Scale_space (number_percentage x, number_percentage y)
        : Properties.transform)
  | Properties.Scale_x value ->
      (Properties.Scale_x (number_percentage value) : Properties.transform)
  | Properties.Scale_y value ->
      (Properties.Scale_y (number_percentage value) : Properties.transform)
  | Properties.Scale_z value ->
      (Properties.Scale_z (number_percentage value) : Properties.transform)
  | Properties.Scale_3d (x, y, z) ->
      (Properties.Scale_3d
         (number_percentage x, number_percentage y, number_percentage z)
        : Properties.transform)
  | value -> value

let simplify_transform_leaf length angle number_percentage simplify ~authored
    ~visited (value : Properties.transform) =
  match value with
  | Properties.Translate _ | Properties.Translate_x _ | Properties.Translate_y _
  | Properties.Translate_z _ | Properties.Translate_3d _ ->
      simplify_translate_transform length authored value
  | Properties.Rotate _ | Properties.Rotate_x _ | Properties.Rotate_y _
  | Properties.Rotate_z _ | Properties.Rotate_3d _ | Properties.Rotate_axis _ ->
      simplify_rotate_transform angle value
  | Properties.Scale _ | Properties.Scale_space _ | Properties.Scale_x _
  | Properties.Scale_y _ | Properties.Scale_z _ | Properties.Scale_3d _ ->
      simplify_scale_transform number_percentage value
  | Properties.Skew (x, y) ->
      (Properties.Skew (angle x, Option.map angle y) : Properties.transform)
  | Properties.Skew_x value ->
      (Properties.Skew_x (angle value) : Properties.transform)
  | Properties.Skew_y value ->
      (Properties.Skew_y (angle value) : Properties.transform)
  | Properties.Perspective value ->
      (Properties.Perspective (length authored value) : Properties.transform)
  | Properties.List values ->
      (Properties.List (List.map (simplify ~authored ~visited) values)
        : Properties.transform)
  | value -> value

let simplify_transform ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let angle = simplify_angle ?layer_order ?layer cascade in
  let number_percentage =
    simplify_number_percentage ?layer_order ?layer cascade
  in
  let simplify_leaf = simplify_transform_leaf length angle number_percentage in
  let ops : Properties.transform Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.transform) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_transform;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_filter ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let number_percentage =
    simplify_number_percentage ?layer_order ?layer cascade
  in
  let angle = simplify_angle ?layer_order ?layer cascade in
  let simplify_leaf simplify ~authored ~visited (value : Properties.filter) =
    match value with
    | Properties.Blur value ->
        (Properties.Blur (length authored value) : Properties.filter)
    | Properties.Brightness value ->
        (Properties.Brightness (number_percentage value) : Properties.filter)
    | Properties.Contrast value ->
        (Properties.Contrast (number_percentage value) : Properties.filter)
    | Properties.Grayscale value ->
        (Properties.Grayscale (number_percentage value) : Properties.filter)
    | Properties.Hue_rotate value ->
        (Properties.Hue_rotate (angle value) : Properties.filter)
    | Properties.Invert value ->
        (Properties.Invert (number_percentage value) : Properties.filter)
    | Properties.Opacity value ->
        (Properties.Opacity (number_percentage value) : Properties.filter)
    | Properties.Saturate value ->
        (Properties.Saturate (number_percentage value) : Properties.filter)
    | Properties.Sepia value ->
        (Properties.Sepia (number_percentage value) : Properties.filter)
    | Properties.List values ->
        (Properties.List (List.map (simplify ~authored ~visited) values)
          : Properties.filter)
    | value -> value
  in
  let ops : Properties.filter Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.filter) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Filter : Properties.filter Properties.kind)
          Properties.read_filter;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

type css_wide_keyword = Inherit | Initial | Unset | Revert | Revert_layer

(* [pp_property_value] is exhaustive over the sealed property GADT, so this
   generic path cannot silently miss a newly added property. CSS-wide keywords
   are reserved from every property's ordinary grammar, making an exact minified
   rendering unambiguous here. *)
let css_wide_of_property_value property value =
  match
    Pp.to_string ~minify:true Properties.pp_property_value (property, value)
  with
  | "inherit" -> Some Inherit
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | _ -> None

let css_wide_of_length (value : Values.length) =
  match value with
  | Values.Inherit -> Some Inherit
  | Values.Initial -> Some Initial
  | Values.Unset -> Some Unset
  | Values.Revert -> Some Revert
  | Values.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_length_list (value : Values.length list) =
  match value with [ length ] -> css_wide_of_length length | _ -> None

let css_wide_of_length_percentage (value : Values.length_percentage) =
  match value with
  | Values.Length length -> css_wide_of_length length
  | _ -> None

let css_wide_of_border_width (value : Properties.border_width) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_border_widths (value : Properties.border_width list) =
  match value with [ width ] -> css_wide_of_border_width width | _ -> None

let css_wide_of_opacity (value : Properties.opacity) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_stroke_width (value : Properties.stroke_width) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_font_size (value : Properties.font_size) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_font_family (value : Properties.font_family) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | _ -> None

let css_wide_of_rotate_value (value : Properties.rotate_value) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_duration (value : Values.duration) =
  match value with
  | Values.Inherit -> Some Inherit
  | Values.Initial -> Some Initial
  | Values.Unset -> Some Unset
  | Values.Revert -> Some Revert
  | Values.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_display (value : Properties.display) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_position (value : Properties.position) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_visibility (value : Properties.visibility) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_clear (value : Properties.clear) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_float_side (value : Properties.float_side) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_scale (value : Properties.scale) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_translate_value (value : Properties.translate_value) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_transform (value : Properties.transform) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_transforms (value : Properties.transform list) =
  match value with
  | [ transform ] -> css_wide_of_transform transform
  | _ -> None

let css_wide_of_filter (value : Properties.filter) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_shadow (value : Properties.shadow) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_border_radius (value : Properties.border_radius) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_color (value : Values.color) =
  match value with
  | Values.Inherit -> Some Inherit
  | Values.Initial -> Some Initial
  | Values.Unset -> Some Unset
  | Values.Revert -> Some Revert
  | Values.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_colors (value : Values.color list) =
  match value with [ color ] -> css_wide_of_color color | _ -> None

let css_wide_of_background_image (value : Properties.background_image) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_background_images (value : Properties.background_image list) =
  match value with [ image ] -> css_wide_of_background_image image | _ -> None

let css_wide_of_animation (value : Properties.animation) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | _ -> None

let css_wide_of_animations (value : Properties.animation list) =
  match value with
  | [ animation ] -> css_wide_of_animation animation
  | _ -> None

let css_wide_of_transition (value : Properties.transition) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_transitions (value : Properties.transition list) =
  match value with
  | [ transition ] -> css_wide_of_transition transition
  | _ -> None

let rec declaration_with_importance important = function
  | Declaration.Declaration { property; value; important = _; _ } ->
      Declaration.v ~important property value
  | Declaration.Theme_guarded { var_name; decl; _ } ->
      Declaration.theme_guarded ~var_name
        (declaration_with_importance important decl)

let property_name (type a) (property : a Properties.property) =
  Pp.to_string ~minify:true Properties.pp_property property

let simplify_component ?layer_order ?layer ctx value =
  let combine_values (left : Values.component) op (right : Values.component) =
    match (left, op, right) with
    | Values.Num a, Values.Add, Values.Num b ->
        Some (Values.Num (a +. b) : Values.component)
    | Values.Num a, Values.Sub, Values.Num b ->
        Some (Values.Num (a -. b) : Values.component)
    | Values.Pct a, Values.Add, Values.Pct b ->
        Some (Values.Pct (a +. b) : Values.component)
    | Values.Pct a, Values.Sub, Values.Pct b ->
        Some (Values.Pct (a -. b) : Values.component)
    | _ -> None
  in
  let combine_value_num (value : Values.component) op num =
    match (value, op) with
    | Values.Num n, Values.Mul ->
        Some (Values.Num (n *. num) : Values.component)
    | Values.Num n, Values.Div when num <> 0. ->
        Some (Values.Num (n /. num) : Values.component)
    | Values.Pct n, Values.Mul ->
        Some (Values.Pct (n *. num) : Values.component)
    | Values.Pct n, Values.Div when num <> 0. ->
        Some (Values.Pct (n /. num) : Values.component)
    | _ -> None
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.component Calc_residual.ops =
    {
      Calc_residual.of_unitless_number = (fun n -> Some (Values.Num n));
      zero = Values.Num 0.;
      is_zero = (fun _ -> false);
      combine_values;
      combine_value_num;
      normalize_value = Fun.id;
      as_var =
        (function (Values.Var var : Values.component) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc = (function Values.Calc calc -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_components Values.read_component;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer ctx ops value

let simplify_percentage ?layer_order ?layer ctx value =
  let to_number (value : Values.percentage) =
    match value with
    | Values.Pct n -> Some n
    | Values.Num n -> Some n
    | _ -> None
  in
  let of_number n = (Values.Pct n : Values.percentage) in
  (* A [<percentage>] slot also accepts a bare [<number>] (e.g. an oklch
     lightness: [.7] and [70%] are equal, but [.7%] is not [.7]). Canonicalise
     the float spelling without collapsing the [Num] / [Pct] distinction, which
     [of_number] would, since it always reissues a [Pct]. *)
  let normalize_value (value : Values.percentage) : Values.percentage =
    let canon n =
      try float_of_string (Pp.string_of_float n) with Failure _ -> n
    in
    match value with
    | Values.Num n -> Values.Num (canon n)
    | Values.Pct n -> Values.Pct (canon n)
    | other -> other
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.percentage Calc_residual.ops =
    {
      of_unitless_number = (fun n -> Some (Values.Num n));
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value;
      as_var =
        (function (Values.Var var : Values.percentage) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc = (function Values.Calc calc -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_components Values.read_percentage;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer ctx ops value

let simplify_alpha ?layer_order ?layer ctx value =
  let alpha_num n : Values.alpha = Values.Num (Float.max 0. (Float.min 1. n)) in
  let alpha_pct n : Values.alpha =
    Values.Pct (Float.max 0. (Float.min 100. n))
  in
  let combine_values (left : Values.alpha) op (right : Values.alpha) =
    match (left, op, right) with
    | Values.Num a, Values.Add, Values.Num b -> Some (alpha_num (a +. b))
    | Values.Num a, Values.Sub, Values.Num b -> Some (alpha_num (a -. b))
    | Values.Pct a, Values.Add, Values.Pct b -> Some (alpha_pct (a +. b))
    | Values.Pct a, Values.Sub, Values.Pct b -> Some (alpha_pct (a -. b))
    | _ -> None
  in
  let combine_value_num (value : Values.alpha) op num =
    match (value, op) with
    | Values.Num a, Values.Mul -> Some (alpha_num (a *. num))
    | Values.Num a, Values.Div when num <> 0. -> Some (alpha_num (a /. num))
    | Values.Pct a, Values.Mul -> Some (alpha_pct (a *. num))
    | Values.Pct a, Values.Div when num <> 0. -> Some (alpha_pct (a /. num))
    | _ -> None
  in
  let to_number (value : Values.alpha) =
    match value with Values.Num n -> Some n | _ -> None
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.alpha Calc_residual.ops =
    {
      of_unitless_number = (fun n -> Some (alpha_num n));
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values;
      combine_value_num;
      normalize_value = normalize_numeric_value ~to_number ~of_number:alpha_num;
      as_var =
        (function (Values.Var var : Values.alpha) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function (Values.Calc calc : Values.alpha) -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_components Values.read_alpha;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer ctx ops value

let simplify_channel ?layer_order ?layer ctx value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : Values.channel Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.channel) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_components Values.read_channel;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_rgb ?layer_order ?layer ctx value =
  let channel = simplify_channel ?layer_order ?layer ctx in
  let simplify_leaf _simplify ~authored:_ ~visited:_ (value : Values.rgb) =
    match value with
    | Values.Channels { r; g; b } ->
        (Values.Channels { r = channel r; g = channel g; b = channel b }
          : Values.rgb)
    | value -> value
  in
  let ops : Values.rgb Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.rgb) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_components Values.read_rgb;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_hue ?layer_order ?layer ctx value =
  let angle_value = simplify_angle ?layer_order ?layer ctx in
  let simplify_leaf _simplify ~authored:_ ~visited:_ (value : Values.hue) =
    match value with
    | Values.Angle (value : Values.angle) ->
        (Values.Angle (angle_value value) : Values.hue)
    | value -> value
  in
  let ops : Values.hue Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.hue) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_components Values.read_hue;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_rgb_color component percentage alpha_value rgb hue
    (value : Values.color) =
  match value with
  | Values.Rgb value -> Values.Rgb (rgb value)
  | Values.Rgba { rgb = value; a } ->
      Values.Rgba { rgb = rgb value; a = alpha_value a }
  | Values.Hsl { h; s; l; a } ->
      Values.Hsl
        { h = hue h; s = percentage s; l = percentage l; a = alpha_value a }
  | Values.Hwb { h; w; b; a } ->
      Values.Hwb
        { h = hue h; w = percentage w; b = percentage b; a = alpha_value a }
  | Values.Color { space; components; alpha } ->
      Values.Color
        {
          space;
          components = List.map component components;
          alpha = alpha_value alpha;
        }
  | value -> value

let simplify_lab_color percentage alpha_value hue (value : Values.color) =
  match value with
  | Values.Lab { l; a; b; alpha = value } ->
      Values.Lab
        { l = Option.map percentage l; a; b; alpha = alpha_value value }
  | Values.Oklch { l; c; h; alpha = value } ->
      Values.Oklch
        { l = Option.map percentage l; c; h = hue h; alpha = alpha_value value }
  | Values.Oklab { l; a; b; alpha = value } ->
      Values.Oklab
        { l = Option.map percentage l; a; b; alpha = alpha_value value }
  | Values.Lch { l; c; h; alpha = value } ->
      Values.Lch
        { l = Option.map percentage l; c; h = hue h; alpha = alpha_value value }
  | value -> value

let simplify_nested_color ctx percentage simplify ~authored ~visited
    (value : Values.color) =
  match value with
  | Values.Current -> (
      match ctx.current_color with
      | Some color -> color
      | None -> Values.Current)
  | Values.Contrast_color color ->
      Values.Contrast_color (simplify ~authored ~visited color)
  | Values.Light_dark (light, dark) ->
      Values.Light_dark
        (simplify ~authored ~visited light, simplify ~authored ~visited dark)
  | Values.Attribute (name, fallback) ->
      Values.Attribute (name, Option.map (simplify ~authored ~visited) fallback)
  | Values.Mix mix ->
      Values.Mix
        {
          mix with
          color1 = simplify ~authored ~visited mix.color1;
          percent1 = Option.map percentage mix.percent1;
          color2 = simplify ~authored ~visited mix.color2;
          percent2 = Option.map percentage mix.percent2;
        }
  | value -> value

let simplify_color ?(layer_order = []) ?layer ctx (value : Values.color) :
    Values.color =
  let component = simplify_component ~layer_order ?layer ctx in
  let percentage = simplify_percentage ~layer_order ?layer ctx in
  let alpha_value = simplify_alpha ~layer_order ?layer ctx in
  let rgb = simplify_rgb ~layer_order ?layer ctx in
  let hue = simplify_hue ~layer_order ?layer ctx in
  let simplify_leaf simplify ~authored ~visited value =
    value
    |> simplify_rgb_color component percentage alpha_value rgb hue
    |> simplify_lab_color percentage alpha_value hue
    |> simplify_nested_color ctx percentage simplify ~authored ~visited
  in
  let ops : Values.color Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.color) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_value Properties.Color Values.read_color;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value

let simplify_shadow_leaf length color simplify ~authored ~visited
    (value : Properties.shadow) =
  let simplify_body
      ({ h_offset; v_offset; blur; spread; color = shadow_color } :
        Properties.shadow_body) : Properties.shadow_body =
    {
      h_offset = length authored h_offset;
      v_offset = length authored v_offset;
      blur = Option.map (length authored) blur;
      spread = Option.map (length authored) spread;
      color = Option.map color shadow_color;
    }
  in
  match value with
  | Properties.Shadow body ->
      (Properties.Shadow (simplify_body body) : Properties.shadow)
  | Properties.Inset (Properties.Body body) ->
      (Properties.Inset (Properties.Body (simplify_body body))
        : Properties.shadow)
  | Properties.Inset (Properties.Toggle { name; no_fallback; body }) ->
      (Properties.Inset
         (Properties.Toggle { name; no_fallback; body = simplify_body body })
        : Properties.shadow)
  | Properties.List shadows ->
      (Properties.List (List.map (simplify ~authored ~visited) shadows)
        : Properties.shadow)
  | value -> value

let simplify_shadow ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let color = simplify_color ?layer_order ?layer cascade in
  let simplify_leaf = simplify_shadow_leaf length color in
  let ops : Properties.shadow Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.shadow) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Shadow : Properties.shadow Properties.kind)
          Properties.read_shadow;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_border_radius ?layer_order ?layer cascade length_ctx value =
  let length_percentage =
    simplify_length_percentage ?layer_order ?layer cascade length_ctx
  in
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.border_radius) =
    match value with
    | Properties.Radius { horizontal; vertical } ->
        (Properties.Radius
           {
             horizontal = List.map length_percentage horizontal;
             vertical = Option.map (List.map length_percentage) vertical;
           }
          : Properties.border_radius)
    | value -> value
  in
  let ops : Properties.border_radius Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.border_radius) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_border_radius;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let resolve_url_leaf (ctx : t) url =
  let loader = { base_url = ctx.base_url; imports = [] } in
  match Url.resolve loader url with Ok resolved -> resolved | Error _ -> url

let gradient_stop_of_color_var (var : Values.color Values.var) :
    Properties.gradient_stop Values.var =
  {
    Values.name = var.name;
    fallback =
      map_var_fallback
        (fun color -> Properties.Color_percentage (color, None, None))
        var.fallback;
    default =
      Option.map
        (fun color -> Properties.Color_percentage (color, None, None))
        var.default;
    layer = var.layer;
    meta = var.meta;
    runtime = var.runtime;
  }

let simplify_color_var_gradient_stop (color : Values.color -> Values.color)
    simplify ~authored ~visited var =
  let stop_var = gradient_stop_of_color_var var in
  let stop =
    simplify ~authored ~visited
      (Properties.Var stop_var : Properties.gradient_stop)
  in
  match (stop : Properties.gradient_stop) with
  | Properties.Var unresolved when unresolved.Values.name = stop_var.name ->
      Properties.Color_percentage (color (Values.Var var), None, None)
  | stop -> stop

let simplify_gradient_stop_leaf ~layer_order ?layer ctx length length_percentage
    percentage color simplify ~authored ~visited
    (value : Properties.gradient_stop) =
  match value with
  | Properties.Color_percentage (Values.Var var, None, None) ->
      simplify_color_var_gradient_stop color simplify ~authored ~visited var
  | Properties.Color_percentage (color_value, first, second) ->
      (Properties.Color_percentage
         ( color color_value,
           Option.map length_percentage first,
           Option.map length_percentage second )
        : Properties.gradient_stop)
  | Properties.Color_length (color_value, first, second) ->
      (Properties.Color_length
         (color color_value, Option.map length first, Option.map length second)
        : Properties.gradient_stop)
  | Properties.Length value ->
      (Properties.Length (length value) : Properties.gradient_stop)
  | Properties.Channel value ->
      (Properties.Channel (simplify_channel ~layer_order ?layer ctx value)
        : Properties.gradient_stop)
  | Properties.Percentage value ->
      (Properties.Percentage (percentage value) : Properties.gradient_stop)
  | Properties.List values ->
      (Properties.List (List.map (simplify ~authored ~visited) values)
        : Properties.gradient_stop)
  | value -> value

let simplify_gradient_stop ?(layer_order = []) ?layer ctx
    (value : Properties.gradient_stop) =
  let length_ctx = Length.of_t ctx in
  let length = Length.simplify ~layer_order ?layer ctx length_ctx in
  let length_percentage =
    simplify_length_percentage ~layer_order ?layer ctx length_ctx
  in
  let percentage = simplify_percentage ~layer_order ?layer ctx in
  let color = simplify_color ~layer_order ?layer ctx in
  let simplify_leaf =
    simplify_gradient_stop_leaf ~layer_order ?layer ctx length length_percentage
      percentage color
  in
  let ops : Properties.gradient_stop Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.gradient_stop) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value Properties.Gradient_stop
          Properties.read_gradient_stop_list;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value

let resolve_image_set_option ctx (option : Properties.image_set_option) =
  let source =
    match option.Properties.source with
    | Properties.Url url ->
        (Properties.Url (resolve_url_leaf ctx url)
          : Properties.image_set_source)
    | Properties.String _ as source -> source
  in
  { option with Properties.source }

let simplify_background_gradient_stops
    (gradient_stop : Properties.gradient_stop -> Properties.gradient_stop)
    (values : Properties.gradient_stop list) =
  List.concat_map
    (fun value ->
      match gradient_stop value with
      | Properties.List values -> values
      | value -> [ value ])
    values

let simplify_background_gradient_stop_var
    (gradient_stop : Properties.gradient_stop -> Properties.gradient_stop)
    (var : Properties.gradient_stop Values.var) =
  {
    var with
    Values.fallback = map_var_fallback gradient_stop var.Values.fallback;
    default = Option.map gradient_stop var.default;
  }

let simplify_background_image_leaf ctx
    (gradient_stop : Properties.gradient_stop -> Properties.gradient_stop)
    simplify ~authored ~visited (value : Properties.background_image) =
  let gradient_stops = simplify_background_gradient_stops gradient_stop in
  let gradient_stop_var = simplify_background_gradient_stop_var gradient_stop in
  match value with
  | Properties.Url url ->
      (Properties.Url (resolve_url_leaf ctx url) : Properties.background_image)
  | Properties.Quoted (url, quote) ->
      Properties.Quoted (resolve_url_leaf ctx url, quote)
  | Properties.Linear_gradient (direction, stops) ->
      Properties.Linear_gradient (direction, gradient_stops stops)
  | Properties.Radial_gradient (config, stops) ->
      Properties.Radial_gradient (config, gradient_stops stops)
  | Properties.Conic_gradient (config, stops) ->
      Properties.Conic_gradient (config, gradient_stops stops)
  | Properties.Linear_gradient_var var ->
      Properties.Linear_gradient_var (gradient_stop_var var)
  | Properties.Radial_gradient_var var ->
      Properties.Radial_gradient_var (gradient_stop_var var)
  | Properties.Conic_gradient_var var ->
      Properties.Conic_gradient_var (gradient_stop_var var)
  | Properties.Image_set options ->
      Properties.Image_set (List.map (resolve_image_set_option ctx) options)
  | Properties.Cross_fade options ->
      Properties.Cross_fade
        (List.map
           (fun (option : Properties.cross_fade_option) ->
             {
               option with
               Properties.image =
                 simplify ~authored ~visited option.Properties.image;
             })
           options)
  | Properties.List images ->
      Properties.List (List.map (simplify ~authored ~visited) images)
  | value -> value

let simplify_background_image ?(layer_order = []) ?layer ctx
    (value : Properties.background_image) : Properties.background_image =
  let gradient_stop = simplify_gradient_stop ~layer_order ?layer ctx in
  let simplify_leaf = simplify_background_image_leaf ctx gradient_stop in
  let ops : Properties.background_image Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.background_image) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value Properties.Background_image
          Properties.read_background_image;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value

let simplify_background ?(layer_order = []) ?layer ctx
    (value : Properties.background) : Properties.background =
  let color = simplify_color ~layer_order ?layer ctx in
  let image = simplify_background_image ~layer_order ?layer ctx in
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.background) =
    match value with
    | Properties.Shorthand (shorthand : Properties.background_shorthand) ->
        let shorthand : Properties.background_shorthand =
          {
            color = Option.map color shorthand.color;
            image = Option.map image shorthand.image;
            position = shorthand.position;
            size = shorthand.size;
            repeat = shorthand.repeat;
            attachment = shorthand.attachment;
            clip = shorthand.clip;
            origin = shorthand.origin;
          }
        in
        (Properties.Shorthand shorthand : Properties.background)
    | value -> value
  in
  let ops : Properties.background Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.background) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_background;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value

let simplify_transition_property_value ?layer_order ?layer ctx value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : Properties.transition_property_value Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.transition_property_value) ->
            Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_components Properties.read_transition_property_value;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_animation_name ?layer_order ?layer ctx value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : Properties.animation_name Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.animation_name) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_animation_name;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_font_family ?layer_order ?layer ctx value =
  let simplify_leaf simplify ~authored:_ ~visited
      (value : Properties.font_family) : Properties.font_family =
    match value with
    | Properties.List items ->
        (Properties.List (List.map (simplify ~authored:false ~visited) items)
          : Properties.font_family)
    | _ -> value
  in
  let ops : Properties.font_family Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.font_family) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_font_family;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_font_src ?layer_order ?layer ctx value =
  let simplify_leaf simplify ~authored ~visited entries =
    List.concat_map
      (function
        | Font_face.Var var -> (
            match simplify ~authored ~visited [ Font_face.Var var ] with
            | [ (Font_face.Var _ as unresolved) ] -> [ unresolved ]
            | entries -> entries)
        | entry -> [ entry ])
      entries
  in
  let ops : Font_face.src Var_residual.ops =
    {
      Var_residual.as_var =
        (function [ Font_face.Var var ] -> Some var | _ -> None);
      of_var = (fun var -> [ Font_face.Var var ]);
      read_custom = read_custom_value Properties.Font_src Font_face.read_src;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let animation_name_of_timing_var ?layer_order ?layer ctx
    (var : Properties.timing_function Values.var) =
  Option.bind
    (lookup_custom_property ?layer ?layer_order ctx var.Values.name)
    (read_custom_components Properties.read_animation_name)

let resolve_animation_name_from_timing ?layer_order ?layer ctx
    (shorthand : Properties.animation_shorthand) =
  match (shorthand.name, shorthand.timing_function) with
  | None, Some (Properties.Var var) -> (
      match animation_name_of_timing_var ?layer_order ?layer ctx var with
      | Some name -> (Some name, None)
      | None -> (shorthand.name, shorthand.timing_function))
  | _ -> (shorthand.name, shorthand.timing_function)

let rebuild_animation_shorthand ?layer_order ?layer ctx duration
    (shorthand : Properties.animation_shorthand) =
  let name, timing_function =
    resolve_animation_name_from_timing ?layer_order ?layer ctx shorthand
  in
  let shorthand : Properties.animation_shorthand =
    {
      name = Option.map (simplify_animation_name ?layer_order ?layer ctx) name;
      duration = Option.map duration shorthand.duration;
      timing_function;
      delay = Option.map duration shorthand.delay;
      iteration_count = shorthand.iteration_count;
      direction = shorthand.direction;
      fill_mode = shorthand.fill_mode;
      play_state = shorthand.play_state;
      timeline = shorthand.timeline;
    }
  in
  (Properties.Shorthand shorthand : Properties.animation)

let simplify_animation_item ?layer_order ?layer ctx duration value =
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.animation) =
    match value with
    | Properties.Shorthand shorthand ->
        rebuild_animation_shorthand ?layer_order ?layer ctx duration shorthand
    | value -> value
  in
  let ops : Properties.animation Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.animation) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Animation : Properties.animation Properties.kind)
          Properties.read_animation;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_transition_item ?layer_order ?layer ctx duration value =
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.transition) =
    match value with
    | Properties.Shorthand (shorthand : Properties.transition_shorthand) ->
        let shorthand : Properties.transition_shorthand =
          {
            property =
              simplify_transition_property_value ?layer_order ?layer ctx
                shorthand.property;
            duration = Option.map duration shorthand.duration;
            timing_function = shorthand.timing_function;
            delay = Option.map duration shorthand.delay;
            behavior = shorthand.behavior;
          }
        in
        (Properties.Shorthand shorthand : Properties.transition)
    | value -> value
  in
  let ops : Properties.transition Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.transition) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_transition;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_var_value ?layer_order ?layer ctx ~as_var ~of_var ~read_custom
    value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : _ Var_residual.ops =
    { Var_residual.as_var; of_var; read_custom; simplify_leaf }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_display ~layer_order ?layer ctx (value : Properties.display) =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.display) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_display)
    value

let simplify_position ~layer_order ?layer ctx (value : Properties.position) =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.position) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_position)
    value

let simplify_visibility ~layer_order ?layer ctx (value : Properties.visibility)
    =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.visibility) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_visibility)
    value

let simplify_clear ~layer_order ?layer ctx (value : Properties.clear) =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.clear) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_clear)
    value

let simplify_float_side ~layer_order ?layer ctx (value : Properties.float_side)
    =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.float_side) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_float_side)
    value

let simplify_lengths_value ~layer_order ?layer ctx length_ctx value =
  let simplify_one = Length.simplify ~layer_order ?layer ctx length_ctx in
  match value with
  | [ (Values.Var var : Values.length) ]
    when var.Values.runtime || List.mem var.Values.name ctx.runtime_vars ->
      [ simplify_one (Values.Var var) ]
  | [ (Values.Var var : Values.length) ] -> (
      match
        Option.bind
          (lookup_custom_property ?layer ~layer_order ctx var.Values.name)
          (read_custom_components Values.read_margin_shorthand)
      with
      | Some lengths -> List.map simplify_one lengths
      | None -> (
          match var.Values.fallback with
          | Values.Fallback fallback -> [ simplify_one fallback ]
          | _ -> [ simplify_one (Values.Var var) ]))
  | _ -> List.map simplify_one value

(* Every kind resolves the same way - simplify the value in context, then let a
   css-wide keyword in the result take over - so the simplifier and the css-wide
   reader are all that vary. Keeping them in one total table is what makes a new
   kind a compile error rather than a value that falls through unsimplified. *)
let kind_simplifier : type a.
    layer_order:string list ->
    ?layer:string ->
    t ->
    a Properties.property_value_kind ->
    (a -> a) * (a -> css_wide_keyword option) =
 fun ~layer_order ?layer ctx kind ->
  let length_ctx = Length.of_t ctx in
  match kind with
  | Properties.Length ->
      (Length.simplify ~layer_order ?layer ctx length_ctx, css_wide_of_length)
  | Properties.Lengths ->
      ( simplify_lengths_value ~layer_order ?layer ctx length_ctx,
        css_wide_of_length_list )
  | Properties.Length_percentage ->
      ( simplify_length_percentage ~layer_order ?layer ctx length_ctx,
        css_wide_of_length_percentage )
  | Properties.Border_width ->
      ( simplify_border_width ~layer_order ?layer ctx length_ctx,
        css_wide_of_border_width )
  | Properties.Border_widths ->
      ( List.map (simplify_border_width ~layer_order ?layer ctx length_ctx),
        css_wide_of_border_widths )
  | Properties.Font_size ->
      ( simplify_font_size ~layer_order ?layer ctx length_ctx,
        css_wide_of_font_size )
  | Properties.Translate ->
      ( simplify_translate_value ~layer_order ?layer ctx length_ctx,
        css_wide_of_translate_value )
  | Properties.Transform ->
      ( List.map (simplify_transform ~layer_order ?layer ctx length_ctx),
        css_wide_of_transforms )
  | Properties.Filter ->
      (simplify_filter ~layer_order ?layer ctx length_ctx, css_wide_of_filter)
  | Properties.Shadow ->
      (simplify_shadow ~layer_order ?layer ctx length_ctx, css_wide_of_shadow)
  | Properties.Border_radius ->
      ( simplify_border_radius ~layer_order ?layer ctx length_ctx,
        css_wide_of_border_radius )
  | Properties.Opacity ->
      (simplify_opacity ~layer_order ?layer ctx, css_wide_of_opacity)
  | Properties.Rotate ->
      (simplify_rotate_value ~layer_order ?layer ctx, css_wide_of_rotate_value)
  | Properties.Duration ->
      (simplify_duration ~layer_order ?layer ctx, css_wide_of_duration)
  | Properties.Display ->
      (simplify_display ~layer_order ?layer ctx, css_wide_of_display)
  | Properties.Position ->
      (simplify_position ~layer_order ?layer ctx, css_wide_of_position)
  | Properties.Visibility ->
      (simplify_visibility ~layer_order ?layer ctx, css_wide_of_visibility)
  | Properties.Clear ->
      (simplify_clear ~layer_order ?layer ctx, css_wide_of_clear)
  | Properties.Float ->
      (simplify_float_side ~layer_order ?layer ctx, css_wide_of_float_side)
  | Properties.Number_percentage ->
      (simplify_number_percentage ~layer_order ?layer ctx, fun _ -> None)
  | Properties.Scale ->
      (simplify_scale ~layer_order ?layer ctx, css_wide_of_scale)
  | Properties.Animation ->
      let duration = simplify_duration ~layer_order ?layer ctx in
      ( List.map (simplify_animation_item ~layer_order ?layer ctx duration),
        css_wide_of_animations )
  | Properties.Transition ->
      let duration = simplify_duration ~layer_order ?layer ctx in
      ( List.map (simplify_transition_item ~layer_order ?layer ctx duration),
        css_wide_of_transitions )
  | Properties.Color ->
      (simplify_color ~layer_order ?layer ctx, css_wide_of_color)
  | Properties.Colors ->
      (List.map (simplify_color ~layer_order ?layer ctx), css_wide_of_colors)
  | Properties.Animation_name ->
      (simplify_animation_name ~layer_order ?layer ctx, fun _ -> None)
  | Properties.Background ->
      (List.map (simplify_background ~layer_order ?layer ctx), fun _ -> None)
  | Properties.Background_image ->
      ( simplify_background_image ~layer_order ?layer ctx,
        css_wide_of_background_image )
  | Properties.Background_images ->
      ( List.map (simplify_background_image ~layer_order ?layer ctx),
        css_wide_of_background_images )
  | Properties.Font_src ->
      (simplify_font_src ~layer_order ?layer ctx, fun _ -> None)
  | Properties.Font_family ->
      (simplify_font_family ~layer_order ?layer ctx, css_wide_of_font_family)
  | Properties.Stroke_width ->
      ( simplify_stroke_width ~layer_order ?layer ctx length_ctx,
        css_wide_of_stroke_width )

let rec eval_typed ?layer_order ?layer ctx decl =
  let ctx = scope ?layer_order ?layer ctx in
  let layer_order = ctx.layer_order in
  let layer = ctx.layer in
  match decl with
  | Declaration.Theme_guarded { var_name; decl; _ } ->
      Declaration.theme_guarded ~var_name
        (eval_typed ~layer_order ?layer ctx decl)
  | Declaration.Declaration { property; value; important; _ } -> (
      match css_wide_of_property_value property value with
      | Some keyword ->
          Option.value
            (resolve_css_wide_keyword ~layer_order ctx ~important ~property
               keyword)
            ~default:decl
      | None -> (
          match Properties.property_value_kind property with
          | Some kind ->
              eval_kind ~layer_order ?layer ctx kind property important value
          | None -> decl))

and resolve_typed : type b.
    layer_order:string list ->
    t ->
    (b -> b) ->
    (b -> css_wide_keyword option) ->
    b Properties.property ->
    bool ->
    b ->
    Declaration.declaration =
 fun ~layer_order ctx simplify css_wide_of property important value ->
  let value = simplify value in
  Option.value
    (Option.bind (css_wide_of value) (fun keyword ->
         resolve_css_wide_keyword ~layer_order ctx ~important ~property keyword))
    ~default:(Declaration.v ~important property value)

and eval_kind : type a.
    layer_order:string list ->
    ?layer:string ->
    t ->
    a Properties.property_value_kind ->
    a Properties.property ->
    bool ->
    a ->
    Declaration.declaration =
 fun ~layer_order ?layer ctx kind property important value ->
  let simplify, css_wide_of = kind_simplifier ~layer_order ?layer ctx kind in
  resolve_typed ~layer_order ctx simplify css_wide_of property important value

and resolve_css_wide_keyword : type b.
    layer_order:string list ->
    t ->
    important:bool ->
    property:b Properties.property ->
    css_wide_keyword ->
    Declaration.declaration option =
 fun ~layer_order ctx ~important ~property keyword ->
  let property_name = property_name property in
  let eval_source ctx decl = eval_typed ~layer_order ctx decl in
  let inherit_source () =
    match inherited_value property_name ctx with
    | Some decl -> Some decl
    | None -> initial_value property_name ctx
  in
  let initial_source () = initial_value property_name ctx in
  let unset_source () =
    if Properties.property_is_inherited property then inherit_source ()
    else initial_source ()
  in
  let cascade_source () =
    match cascade_rule_chain ctx ~property_name ~important with
    | None -> None
    | Some chain -> (
        match chain with
        | rule :: _ ->
            let cascade_rules =
              Option.map (remove_cascade_rule rule) ctx.cascade_rules
            in
            let ctx = { ctx with layer = rule.layer; cascade_rules } in
            Some (eval_source ctx rule.declaration)
        | [] -> Option.map (eval_source ctx) (unset_source ()))
  in
  match keyword with
  | Revert_layer ->
      Option.map (declaration_with_importance important) (cascade_source ())
  | _ ->
      let source =
        match keyword with
        | Inherit -> inherit_source ()
        | Initial -> initial_source ()
        | Unset -> unset_source ()
        | Revert -> None
        | Revert_layer -> assert false
      in
      Option.map
        (fun decl ->
          eval_source ctx decl |> declaration_with_importance important)
        source

(* [eval] is a declaration-level abstract interpreter: it rewrites a declaration
   to a more-defined one in the same typed AST, keeping unresolved subtrees as
   residual syntax. The typed walker is the only semantic path. *)
let eval ?layer_order ?layer ctx decl = eval_typed ?layer_order ?layer ctx decl
