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
  let value = String.trim value in
  List.exists
    (fun (property', value') ->
      String.equal property property' && String.equal value (String.trim value'))
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

(* Selector / media / supports / container matching evaluates the structured
   condition AST against the closed query record. Every lookup is a string match
   against the explicit feature/declaration tables in [query]; nothing in the
   library reaches outside the supplied context. *)

(* CSS Values 4 absolute-length conversion table. Values that depend on
   font-size or viewport size are resolved with a 16px base when no explicit
   context is supplied; the typical UA initial font-size. *)
let length_to_px ?(base_font_size = 16.) ?(viewport_width = 0.)
    ?(viewport_height = 0.) (l : Values.length) : float option =
  match l with
  | Px f -> Some f
  | Cm f -> Some (f *. 96. /. 2.54)
  | Mm f -> Some (f *. 96. /. 25.4)
  | Q f -> Some (f *. 96. /. (2.54 *. 40.))
  | In f -> Some (f *. 96.)
  | Pt f -> Some (f *. 96. /. 72.)
  | Pc f -> Some (f *. 96. /. 6.)
  | Rem f | Em f -> Some (f *. base_font_size)
  | Vw f | Dvw f | Lvw f | Svw f -> Some (f *. viewport_width /. 100.)
  | Vh f | Dvh f | Lvh f | Svh f -> Some (f *. viewport_height /. 100.)
  | Vmin f | Dvmin f | Lvmin f | Svmin f ->
      Some (f *. Float.min viewport_width viewport_height /. 100.)
  | Vmax f | Dvmax f | Lvmax f | Svmax f ->
      Some (f *. Float.max viewport_width viewport_height /. 100.)
  | Zero -> Some 0.
  | _ -> None

(* Parse a media-feature value (e.g. "1024px", "48em", "16/9") through the
   normal length reader; falls back to a numeric or keyword view when the value
   is not a length. *)
let parse_media_value s : Media.value option =
  let s = String.trim s in
  if s = "" then None
  else
    let try_length () =
      try
        let cursor = Cursor.of_string s in
        let length = Values.read_length cursor in
        Cursor.ws cursor;
        if Cursor.is_done cursor then Some (Media.Length length) else None
      with _ -> None
    in
    let try_ratio () =
      match String.split_on_char '/' s with
      | [ a; b ] -> (
          try
            Some
              (Media.Ratio
                 (int_of_string (String.trim a), int_of_string (String.trim b)))
          with _ -> None)
      | _ -> None
    in
    let try_number () =
      try
        let cursor = Cursor.of_string s in
        let n = Cursor.number cursor in
        Cursor.ws cursor;
        if Cursor.is_done cursor then
          if Float.is_integer n then Some (Media.Integer (int_of_float n))
          else Some (Media.Number n)
        else None
      with _ -> None
    in
    match try_length () with
    | Some _ as v -> v
    | None -> (
        match try_ratio () with
        | Some _ as v -> v
        | None -> (
            match try_number () with
            | Some _ as v -> v
            | None -> Some (Media.Ident s)))

(* Convert a media value to a comparable number when one exists. Ratios collapse
   to width/height to support range comparisons on aspect-ratio. *)
let media_value_to_number ~base_font_size (v : Media.value) : float option =
  match v with
  | Length l -> length_to_px ~base_font_size l
  | Integer i -> Some (float_of_int i)
  | Number n -> Some n
  | Ratio (a, b) when b <> 0 -> Some (float_of_int a /. float_of_int b)
  | Ratio _ -> None
  | Resolution (n, _) -> Some n
  | Ident _ -> None

let cmp_to_float_op : Media.cmp -> float -> float -> bool = function
  | Lt -> ( < )
  | Le -> ( <= )
  | Eq -> ( = )
  | Gt -> ( > )
  | Ge -> ( >= )

let normalize_supports_value v = String.trim v

let supports_function_match table name args =
  let args = normalize_supports_value args in
  List.exists
    (fun (n, a) ->
      String.equal name n && String.equal args (normalize_supports_value a))
    table

let rec matches_supports q (cond : Supports.t) =
  match cond with
  | Property (property, value) ->
      supports_declaration ~property ~value:(normalize_supports_value value) q
  | Func (name, args) -> supports_function_match q.supports_functions name args
  | Not c -> not (matches_supports q c)
  | And (a, b) -> matches_supports q a && matches_supports q b
  | Or (a, b) -> matches_supports q a || matches_supports q b

(* Strip the [min-]/[max-] prefix from a CSS feature name. The prefixed forms
   imply [<= value] and [>= value] respectively per CSS Media Queries 4 §2.4. *)
let strip_min_max_prefix name =
  if String.length name > 4 && String.sub name 0 4 = "min-" then
    Some (`Min, String.sub name 4 (String.length name - 4))
  else if String.length name > 4 && String.sub name 0 4 = "max-" then
    Some (`Max, String.sub name 4 (String.length name - 4))
  else None

(* The base font size used when resolving [em]/[rem] inside a media or container
   query. Per CSS Media Queries 4 §1.3 these resolve against the initial value
   of font-size on the root element. *)
let media_base_font_size = 16.

(* Look up a media feature's parsed value from the context. *)
let lookup_feature_value table feature_name =
  match List.assoc_opt feature_name table with
  | None -> None
  | Some raw -> parse_media_value raw

let compare_with_op ~base_font_size (op : Media.cmp) (lhs : Media.value)
    (rhs : Media.value) : bool option =
  match (lhs, rhs) with
  | Ident a, Ident b ->
      Some (op = Media.Eq && String.equal (String.trim a) (String.trim b))
  | _ -> (
      match
        ( media_value_to_number ~base_font_size lhs,
          media_value_to_number ~base_font_size rhs )
      with
      | Some la, Some lb -> Some (cmp_to_float_op op la lb)
      | _ -> None)

(* CSS Media Queries 4 §3 evaluates each feature against the explicit query
   table. An unknown feature is treated as not matching ([Some false] would be a
   stronger claim, but here [None] means "cannot decide"). *)
let eval_feature ~base_font_size table (feature : Media.feature) : bool =
  let with_lookup name f =
    match lookup_feature_value table name with
    | None -> false
    | Some actual -> Option.value ~default:false (f actual)
  in
  match feature with
  | Boolean name -> List.mem_assoc name table
  | Plain (name, value) -> (
      match strip_min_max_prefix name with
      | Some (`Min, base) ->
          with_lookup base (fun actual ->
              compare_with_op ~base_font_size Media.Ge actual value)
      | Some (`Max, base) ->
          with_lookup base (fun actual ->
              compare_with_op ~base_font_size Media.Le actual value)
      | None ->
          with_lookup name (fun actual ->
              compare_with_op ~base_font_size Media.Eq actual value))
  | Range (name, op, value) ->
      with_lookup name (fun actual ->
          compare_with_op ~base_font_size op actual value)
  | Range_rev (value, op, name) ->
      with_lookup name (fun actual ->
          compare_with_op ~base_font_size op value actual)
  | Interval (lo, lo_op, name, hi_op, hi) ->
      with_lookup name (fun actual ->
          match
            ( compare_with_op ~base_font_size lo_op lo actual,
              compare_with_op ~base_font_size hi_op actual hi )
          with
          | Some a, Some b -> Some (a && b)
          | _ -> None)

let rec eval_condition ~base_font_size table (c : Media.condition) =
  match c with
  | Feature f -> eval_feature ~base_font_size table f
  | Not c -> not (eval_condition ~base_font_size table c)
  | And (a, b) ->
      eval_condition ~base_font_size table a
      && eval_condition ~base_font_size table b
  | Or (a, b) ->
      eval_condition ~base_font_size table a
      || eval_condition ~base_font_size table b

let medium_to_string = function
  | Media.All -> "all"
  | Screen -> "screen"
  | Print -> "print"
  | Other s -> s

let media_type_matches q (medium : Media.medium) =
  match (medium, q.media_type) with
  | All, _ -> true
  | _, None -> medium = All
  | _, Some t -> String.equal (medium_to_string medium) t

let rec eval_query ~base_font_size q (query : Media.query) =
  match query with
  | Cond c -> eval_condition ~base_font_size q.media_features c
  | Type { prefix; type_; trailing } -> (
      let head = media_type_matches q type_ in
      let body =
        match trailing with
        | None -> head
        | Some c -> head && eval_condition ~base_font_size q.media_features c
      in
      match prefix with Some Media.Not -> not body | _ -> body)
  | List qs -> List.exists (eval_query ~base_font_size q) qs

let rec matches_media q (m : Media.t) =
  let base_font_size = media_base_font_size in
  let table = q.media_features in
  let bool_feature name expected =
    match lookup_feature_value table name with
    | Some (Ident s) -> String.equal s expected
    | _ -> false
  in
  match m with
  | Min_width px | Max_width px -> (
      let op = match m with Min_width _ -> Media.Ge | _ -> Media.Le in
      match lookup_feature_value table "width" with
      | None -> false
      | Some actual -> (
          match compare_with_op ~base_font_size op actual (Length (Px px)) with
          | Some b -> b
          | None -> false))
  | Not_min_width px -> not (matches_media q (Min_width px))
  | Min_width_rem rem -> matches_media q (Min_width (rem *. base_font_size))
  | Not_min_width_rem rem ->
      matches_media q (Not_min_width (rem *. base_font_size))
  | Min_width_length l -> (
      match length_to_px ~base_font_size l with
      | Some px -> matches_media q (Min_width px)
      | None -> false)
  | Not_min_width_length l -> not (matches_media q (Min_width_length l))
  | Prefers_reduced_motion `No_preference ->
      bool_feature "prefers-reduced-motion" "no-preference"
  | Prefers_reduced_motion `Reduce ->
      bool_feature "prefers-reduced-motion" "reduce"
  | Prefers_contrast `More -> bool_feature "prefers-contrast" "more"
  | Prefers_contrast `Less -> bool_feature "prefers-contrast" "less"
  | Prefers_color_scheme `Dark -> bool_feature "prefers-color-scheme" "dark"
  | Prefers_color_scheme `Light -> bool_feature "prefers-color-scheme" "light"
  | Forced_colors `Active -> bool_feature "forced-colors" "active"
  | Forced_colors `None -> bool_feature "forced-colors" "none"
  | Inverted_colors `Inverted -> bool_feature "inverted-colors" "inverted"
  | Inverted_colors `None -> bool_feature "inverted-colors" "none"
  | Pointer `None -> bool_feature "pointer" "none"
  | Pointer `Coarse -> bool_feature "pointer" "coarse"
  | Pointer `Fine -> bool_feature "pointer" "fine"
  | Any_pointer `None -> bool_feature "any-pointer" "none"
  | Any_pointer `Coarse -> bool_feature "any-pointer" "coarse"
  | Any_pointer `Fine -> bool_feature "any-pointer" "fine"
  | Scripting `None -> bool_feature "scripting" "none"
  | Scripting `Initial_only -> bool_feature "scripting" "initial-only"
  | Scripting `Enabled -> bool_feature "scripting" "enabled"
  | Hover -> bool_feature "hover" "hover"
  | Print -> ( match q.media_type with Some t -> t = "print" | None -> false)
  | Orientation `Portrait -> bool_feature "orientation" "portrait"
  | Orientation `Landscape -> bool_feature "orientation" "landscape"
  | Custom q' -> eval_query ~base_font_size q q'
  | Negated m -> not (matches_media q m)

let matches_selector _doc _sel = false

(* Container queries reuse the media-feature/condition machinery applied to
   [q.container_features], with optional named-container guarding. *)
let rec matches_container q ?name (cond : Container.t) =
  let base_font_size = media_base_font_size in
  let name_matches expected =
    match (name, q.container_name) with
    | None, _ -> true
    | Some n, Some actual -> String.equal n actual
    | Some _, None -> false || expected = ""
  in
  let _ = name_matches in
  let check_named =
    match (name, q.container_name) with
    | None, _ -> true
    | Some n, Some actual -> String.equal n actual
    | Some _, None -> false
  in
  if not check_named then false
  else
    match cond with
    | Min_width_rem rem -> (
        let target = rem *. base_font_size in
        match lookup_feature_value q.container_features "inline-size" with
        | None -> false
        | Some actual -> (
            match
              compare_with_op ~base_font_size Media.Ge actual
                (Length (Px target))
            with
            | Some b -> b
            | None -> false))
    | Min_width_px px -> (
        match lookup_feature_value q.container_features "inline-size" with
        | None -> false
        | Some actual -> (
            match
              compare_with_op ~base_font_size Media.Ge actual
                (Length (Px (float_of_int px)))
            with
            | Some b -> b
            | None -> false))
    | Named (n, inner) -> matches_container q ~name:n inner
    | Style (prop, value) -> (
        let key =
          match value with
          | None -> "style(" ^ prop ^ ")"
          | Some v -> "style(" ^ prop ^ ": " ^ v ^ ")"
        in
        List.mem_assoc key q.container_features
        ||
        match value with
        | None -> List.mem_assoc ("style(" ^ prop ^ ")") q.container_features
        | Some v ->
            List.exists
              (fun (k, vv) ->
                String.equal k ("style(" ^ prop ^ ")")
                && String.equal (String.trim vv) (String.trim v))
              q.container_features)
    | Scroll_state (prop, value) ->
        let key = "scroll-state(" ^ prop ^ ": " ^ value ^ ")" in
        List.mem_assoc key q.container_features
    | Feature_query raw -> (
        (* Parse the raw feature back as a media condition and evaluate. *)
        try
          let media = Media.of_string raw in
          matches_media { q with media_features = q.container_features } media
        with _ -> false)
    | Custom media ->
        matches_media { q with media_features = q.container_features } media

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
