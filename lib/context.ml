(** Explicit contexts for CSS AST/value transforms. *)

type t = {
  custom_properties : Declaration.declaration list;
  inherited_values : Declaration.declaration list;
  initial_values : Declaration.declaration list;
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
  scope : string option;
  element : string option;
  classes : string list;
  ids : string list;
  attributes : (string * string option) list;
  pseudo_classes : string list;
  pseudo_elements : string list;
}

type query = {
  media_type : string option;
  media_features : (string * string) list;
  supports_declarations : (string * string) list;
  supports_functions : (string * string) list;
  container_name : string option;
  container_features : (string * string) list;
}

type loader = { base_url : string option; imports : (string * string) list }

type animation = {
  timeline_time : string option;
  progress : float option;
  animated_properties : string list;
}

let empty =
  {
    custom_properties = [];
    inherited_values = [];
    initial_values = [];
    base_url = None;
    root_font_size = None;
    parent_font_size = None;
    current_color = None;
    viewport_width = None;
    viewport_height = None;
    container_width = None;
    container_height = None;
  }

let v ?(custom_properties = []) ?(inherited_values = []) ?(initial_values = [])
    ?base_url ?root_font_size ?parent_font_size ?current_color ?viewport_width
    ?viewport_height ?container_width ?container_height () =
  {
    custom_properties;
    inherited_values;
    initial_values;
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
    supports_declarations = [];
    supports_functions = [];
    container_name = None;
    container_features = [];
  }

let query ?media_type ?(media_features = []) ?(supports_declarations = [])
    ?(supports_functions = []) ?container_name ?(container_features = []) () =
  {
    media_type;
    media_features;
    supports_declarations;
    supports_functions;
    container_name;
    container_features;
  }

let empty_loader = { base_url = None; imports = [] }
let loader ?base_url ?(imports = []) () = { base_url; imports }

let empty_animation =
  { timeline_time = None; progress = None; animated_properties = [] }

let animation ?timeline_time ?progress ?(animated_properties = []) () =
  { timeline_time; progress; animated_properties }

let by_name name decls =
  List.find_opt (fun d -> Declaration.property_name d = name) decls

let custom_property name ctx = by_name name ctx.custom_properties
let inherited_value property ctx = by_name property ctx.inherited_values
let initial_value property ctx = by_name property ctx.initial_values
let has_class name ctx = List.exists (String.equal name) ctx.classes
let has_id name ctx = List.exists (String.equal name) ctx.ids
let attribute name ctx = List.assoc_opt name ctx.attributes
let media_feature name ctx = List.assoc_opt name ctx.media_features

let supports_declaration ~property ~value ctx =
  List.exists
    (fun (property', value') ->
      String.equal property property' && String.equal value value')
    ctx.supports_declarations

let container_feature name ctx = List.assoc_opt name ctx.container_features
let import_source url ctx = List.assoc_opt url ctx.imports

let animates_property property ctx =
  List.exists (String.equal property) ctx.animated_properties

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

let pp_string_list ctx items =
  Pp.char ctx '[';
  let first = ref true in
  List.iter
    (fun s ->
      if !first then first := false else Pp.string ctx ", ";
      Pp.string ctx s)
    items;
  Pp.char ctx ']'

let pp_pair_list ctx items =
  Pp.char ctx '[';
  let first = ref true in
  List.iter
    (fun pair ->
      if !first then first := false else Pp.string ctx ", ";
      pp_pair ctx pair)
    items;
  Pp.char ctx ']'

let pp_attr_list ctx items =
  Pp.char ctx '[';
  let first = ref true in
  List.iter
    (fun (name, value) ->
      if !first then first := false else Pp.string ctx ", ";
      Pp.string ctx name;
      match value with
      | None -> ()
      | Some v ->
          Pp.char ctx '=';
          Pp.string ctx v)
    items;
  Pp.char ctx ']'

let pp_field ctx ~first label print_value value =
  if not !first then Pp.string ctx "; ";
  first := false;
  Pp.string ctx label;
  Pp.char ctx '=';
  print_value ctx value

let pp_decl_list ctx items =
  Pp.char ctx '[';
  let first = ref true in
  List.iter
    (fun d ->
      if !first then first := false else Pp.string ctx ", ";
      Declaration.pp_declaration ctx d)
    items;
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
  pp_field ctx ~first "scope" pp_string_option d.scope;
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
  pp_field ctx ~first "media_features" pp_pair_list q.media_features;
  pp_field ctx ~first "supports_declarations" pp_pair_list
    q.supports_declarations;
  pp_field ctx ~first "supports_functions" pp_pair_list q.supports_functions;
  pp_field ctx ~first "container_name" pp_string_option q.container_name;
  pp_field ctx ~first "container_features" pp_pair_list q.container_features;
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

(* TODO: full computed-value resolution. Returns Error for now so callers can
   plumb the API through; concrete cases will be filled in incrementally as the
   test_context contract grows. *)
let computed_value ?layer_order:_ ?layer:_ _ctx _decl =
  Error "Context.computed_value: not implemented"

(* Selector / media / supports / container matching stubs. Conservative: never
   claim a match while the context is a closed record (the library is
   parser/serializer-first, evaluation is the caller's job). *)
let matches_selector _doc _sel = false
let matches_media _q _m = false
let matches_supports _q _cond = false
let matches_container _q ?name:_ _cond = false

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let cut ?(rev = false) ~sep s =
  let sep_len = String.length sep in
  let len = String.length s in
  let matches i = i + sep_len <= len && String.sub s i sep_len = sep in
  let rec forward i =
    if i + sep_len > len then None
    else if matches i then Some i
    else forward (i + 1)
  in
  let rec backward i =
    if i < 0 then None else if matches i then Some i else backward (i - 1)
  in
  match if rev then backward (len - sep_len) else forward 0 with
  | None -> None
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i + sep_len) (len - i - sep_len))

let resolve_url loader href =
  match loader.base_url with
  | None -> Ok href
  | Some _ when starts_with ~prefix:"http://" href -> Ok href
  | Some _ when starts_with ~prefix:"https://" href -> Ok href
  | Some base when starts_with ~prefix:"/" href -> (
      match cut ~sep:"://" base with
      | None -> Ok href
      | Some (scheme, rest) -> (
          match cut ~sep:"/" rest with
          | None -> Ok (scheme ^ "://" ^ rest ^ href)
          | Some (host, _) -> Ok (scheme ^ "://" ^ host ^ href)))
  | Some base -> (
      match cut ~rev:true ~sep:"/" base with
      | None -> Ok href
      | Some (dir, _) -> Ok (dir ^ "/" ^ href))

let load_import ?query:_ ?layer_order:_ _loader _rule =
  Error "Context.load_import: not implemented"
