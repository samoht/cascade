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

(* Re-tokenise a value string and reserialise it minified. Used to bring
   user-provided table entries into the same canonical form as query inputs,
   which always run through [Parser.to_string_minified] /
   [Declaration.string_of_value ~minify:true] before comparison. Falls back to
   [String.trim] on parse failure so callers never get a surprise exception. *)
let canon_value s =
  let s = String.trim s in
  try
    let p = Parser.of_string s in
    let rec collect acc =
      match Parser.next p with
      | Component.Preserved { kind = Token.Eof; _ } -> List.rev acc
      | c -> collect (c :: acc)
    in
    Parser.to_string_minified (collect [])
  with _ -> s

let query ?media_type ?(media_features = []) ?(supports_declarations = [])
    ?(supports_functions = []) ?container_name ?(container_features = []) () =
  let supports_declarations =
    List.map
      (fun (prop, value) -> (prop, canon_value value))
      supports_declarations
  in
  let supports_functions =
    List.map (fun (name, args) -> (name, canon_value args)) supports_functions
  in
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
  (* [ctx.supports_declarations] was canonicalised at [query] construction;
     canonicalise the caller's [value] the same way before comparing. *)
  let value = canon_value value in
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

(** {1 CSS Evaluator Pipeline}

    Browser CSS engines structure rule application as a sequence of stages
    operating on closed inputs:

    + {b Tokenization / parsing} produces structured ASTs (handled by the
      individual module readers).
    + {b Length canonicalisation} ([Length]) maps each unit to absolute pixels
      using the supplied font/viewport/container references.
    + {b Condition evaluation} ([Supports_eval], [Media_eval], [Container_eval])
      walks the structured query AST against the explicit feature/declaration
      tables in [query].
    + {b Selector matching} ([Selector_eval]) decides whether a selector would
      attach to the element described by [document]. The element is not part of
      a tree, so combinators reduce to matching the rightmost compound selector.
    + {b Cascade + computed value} ([Computed_value]) resolves CSS-wide
      keywords, expands [var()], evaluates [calc()], and converts relative
      lengths against the property-value context.
    + {b Loader} ([Import], [Url]) resolves relative URLs and looks up imported
      stylesheets, applying [@import] guards. *)

(** {2 Length canonicalisation (CSS Values 4 §6)}

    All length units convert to pixels. Absolute units come from the fixed 1in =
    96px conversion table; font-relative and viewport-relative units require the
    supplied base size. Returns [None] when a length depends on information not
    present in the context (e.g. [ch]/[ex] need glyph metrics, percentages need
    a containing-block size). *)

module Length = struct
  type ctx = {
    base_font_size : float;
    root_font_size : float option;
    parent_font_size : float option;
    viewport_width : float option;
    viewport_height : float option;
    container_width : float option;
    container_height : float option;
  }

  (* CSS Media Queries 4 §1.3: relative units in @media/@container resolve
     against the initial value of font-size on the root element. Pre-fill
     [parent_font_size] / [root_font_size] with the 16px default so [em] / [rem]
     in queries always have a reference, while a fresh user-supplied
     [Length.ctx] without these fields will reject relative units. *)
  let media_default =
    {
      base_font_size = 16.;
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

  let of_t (ctx : t) : ctx =
    let unwrap_px = function Some (Values.Px p) -> Some p | _ -> None in
    {
      base_font_size = 16.;
      root_font_size = unwrap_px ctx.root_font_size;
      parent_font_size = unwrap_px ctx.parent_font_size;
      viewport_width = unwrap_px ctx.viewport_width;
      viewport_height = unwrap_px ctx.viewport_height;
      container_width = unwrap_px ctx.container_width;
      container_height = unwrap_px ctx.container_height;
    }
end

(** {2 Media-feature value parsing and comparison (CSS Media Queries 4 §3)}

    Feature values arrive in [query.media_features] / [query.container_features]
    as raw strings. Parsing produces a {!Media.value} so length, ratio, integer
    and ident comparisons share the same code path with the typed feature AST.
*)

module Media_value = struct
  let parse s : Media.value option =
    let s = String.trim s in
    if s = "" then None
    else
      let try_length () =
        try
          let cursor = Cursor.of_string s in
          let length = Values.read_length cursor in
          Cursor.ws cursor;
          if Cursor.is_done cursor then Some (Media.Length length) else None
        with Cursor.Parse_error _ | Reader.Parse_error _ -> None
      in
      let try_ratio () =
        match String.split_on_char '/' s with
        | [ a; b ] -> (
            match
              ( int_of_string_opt (String.trim a),
                int_of_string_opt (String.trim b) )
            with
            | Some n, Some d -> Some (Media.Ratio (n, d))
            | _ -> None)
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
        with Cursor.Parse_error _ | Reader.Parse_error _ -> None
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

  let to_number (v : Media.value) : float option =
    match v with
    | Length l -> Length.media_to_px l
    | Integer i -> Some (float_of_int i)
    | Number n -> Some n
    | Ratio (a, b) when b <> 0 -> Some (float_of_int a /. float_of_int b)
    | Ratio _ -> None
    | Resolution (n, _) -> Some n
    | Ident _ -> None

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
        Some (op = Media.Eq && String.equal (String.trim a) (String.trim b))
    | _ -> (
        match (to_number lhs, to_number rhs) with
        | Some la, Some lb -> Some (cmp_op op la lb)
        | _ -> None)
end

(** {2 [@supports] evaluation (CSS Conditional 4 §3)}

    Each [Supports.t] constructor has a dedicated evaluator. The structural
    cases ([Not]/[And]/[Or]) recurse into [eval]; the leaves ([Property]/[Func])
    consult the explicit declaration/function tables. *)

module Supports_eval = struct
  (* [q.supports_declarations] and [q.supports_functions] are already in
     minified canonical form (applied at [query] construction), and the query
     side of each comparison is always minified at the call site, so the leaves
     can use plain [String.equal]. *)
  let eval_property table ~property ~value =
    List.exists
      (fun (p, v) -> String.equal property p && String.equal value v)
      table

  let eval_func table ~name ~args =
    List.exists (fun (n, a) -> String.equal name n && String.equal args a) table

  let rec eval q : Supports.t -> bool = function
    | Property decl ->
        let property = Declaration.property_name decl in
        let value = Declaration.string_of_value ~minify:true decl in
        eval_property q.supports_declarations ~property ~value
    | Func (name, args) ->
        eval_func q.supports_functions ~name
          ~args:(Parser.to_string_minified args)
    | Not c -> not (eval q c)
    | And (a, b) -> eval q a && eval q b
    | Or (a, b) -> eval q a || eval q b
end

(** {2 [@media] evaluation (CSS Media Queries 4)}

    Each [Media] AST type has its own evaluator: [eval_feature] for
    [Media.feature], [eval_condition] for [Media.condition], [eval_query] for
    [Media.query], [eval_medium] for [Media.medium], and [eval] for the
    typed-shorthand wrapper [Media.t]. Lengths resolve against a 16px base per
    §1.3; [min-]/[max-] feature prefixes desugar to range comparisons. *)

module Media_eval = struct
  open Media

  type feature_table = (string * string) list

  let strip_min_max name =
    let len = String.length name in
    if len > 4 && String.sub name 0 4 = "min-" then
      Some (`Min, String.sub name 4 (len - 4))
    else if len > 4 && String.sub name 0 4 = "max-" then
      Some (`Max, String.sub name 4 (len - 4))
    else None

  let lookup_value (table : feature_table) feature_name : Media.value option =
    match List.assoc_opt feature_name table with
    | None -> None
    | Some raw -> Media_value.parse raw

  let eval_feature (table : feature_table) : Media.feature -> bool =
    let with_lookup name f =
      match lookup_value table name with
      | None -> false
      | Some actual -> Option.value ~default:false (f actual)
    in
    function
    | Boolean name -> List.mem_assoc name table
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

  let rec eval_condition (table : feature_table) : Media.condition -> bool =
    function
    | Feature f -> eval_feature table f
    | Not c -> not (eval_condition table c)
    | And (a, b) -> eval_condition table a && eval_condition table b
    | Or (a, b) -> eval_condition table a || eval_condition table b

  let eval_medium q : Media.medium -> bool = function
    | All -> true
    | Screen -> q.media_type = Some "screen"
    | Print -> q.media_type = Some "print"
    | Other s -> q.media_type = Some s

  let rec eval_query q : Media.query -> bool = function
    | Cond c -> eval_condition q.media_features c
    | Type { prefix; type_; trailing } -> (
        let head = eval_medium q type_ in
        let body =
          match trailing with
          | None -> head
          | Some c -> head && eval_condition q.media_features c
        in
        match prefix with Some Media.Not -> not body | _ -> body)
    | List qs -> List.exists (eval_query q) qs

  let bool_feature q name expected =
    match lookup_value q.media_features name with
    | Some (Ident s) -> String.equal s expected
    | _ -> false

  (* CSS Media Queries 4 typed shorthands map onto specific [Plain]/[Range]
     evaluations against the explicit feature table. *)
  let rec eval q (m : Media.t) : bool =
    match m with
    | Min_width px | Max_width px -> (
        let op : Media.cmp = match m with Min_width _ -> Ge | _ -> Le in
        match lookup_value q.media_features "width" with
        | None -> false
        | Some actual -> (
            match Media_value.compare_with op actual (Length (Px px)) with
            | Some b -> b
            | None -> false))
    | Not_min_width px -> not (eval q (Min_width px))
    | Min_width_rem rem ->
        eval q (Min_width (rem *. Length.media_default.base_font_size))
    | Not_min_width_rem rem ->
        eval q (Not_min_width (rem *. Length.media_default.base_font_size))
    | Min_width_length l -> (
        match Length.media_to_px l with
        | Some px -> eval q (Min_width px)
        | None -> false)
    | Not_min_width_length l -> not (eval q (Min_width_length l))
    | Prefers_reduced_motion `No_preference ->
        bool_feature q "prefers-reduced-motion" "no-preference"
    | Prefers_reduced_motion `Reduce ->
        bool_feature q "prefers-reduced-motion" "reduce"
    | Prefers_contrast `More -> bool_feature q "prefers-contrast" "more"
    | Prefers_contrast `Less -> bool_feature q "prefers-contrast" "less"
    | Prefers_color_scheme `Dark -> bool_feature q "prefers-color-scheme" "dark"
    | Prefers_color_scheme `Light ->
        bool_feature q "prefers-color-scheme" "light"
    | Forced_colors `Active -> bool_feature q "forced-colors" "active"
    | Forced_colors `None -> bool_feature q "forced-colors" "none"
    | Inverted_colors `Inverted -> bool_feature q "inverted-colors" "inverted"
    | Inverted_colors `None -> bool_feature q "inverted-colors" "none"
    | Pointer `None -> bool_feature q "pointer" "none"
    | Pointer `Coarse -> bool_feature q "pointer" "coarse"
    | Pointer `Fine -> bool_feature q "pointer" "fine"
    | Any_pointer `None -> bool_feature q "any-pointer" "none"
    | Any_pointer `Coarse -> bool_feature q "any-pointer" "coarse"
    | Any_pointer `Fine -> bool_feature q "any-pointer" "fine"
    | Scripting `None -> bool_feature q "scripting" "none"
    | Scripting `Initial_only -> bool_feature q "scripting" "initial-only"
    | Scripting `Enabled -> bool_feature q "scripting" "enabled"
    | Hover -> bool_feature q "hover" "hover"
    | Print -> q.media_type = Some "print"
    | Orientation `Portrait -> bool_feature q "orientation" "portrait"
    | Orientation `Landscape -> bool_feature q "orientation" "landscape"
    | Custom q' -> eval_query q q'
    | Negated m -> not (eval q m)
end

(** {2 Container-query evaluation (CSS Containment 3 §3.4)}

    Container queries reuse the media-feature evaluator applied to the container
    feature table; they additionally guard on the container name (when supplied)
    and accept [style()] / [scroll-state()] queries that the media evaluator
    does not know about. *)

module Container_eval = struct
  let name_matches ?name q =
    match (name, q.container_name) with
    | None, _ -> true
    | Some n, Some actual -> String.equal n actual
    | Some _, None -> false

  let style_match q ~prop ~value =
    let key_with_value v = "style(" ^ prop ^ ": " ^ v ^ ")" in
    let key_bool = "style(" ^ prop ^ ")" in
    match value with
    | None ->
        List.mem_assoc key_bool q.container_features
        || List.exists
             (fun (k, _) ->
               String.length k >= String.length key_bool
               && String.sub k 0 (String.length key_bool) = key_bool)
             q.container_features
    | Some v ->
        List.mem_assoc (key_with_value v) q.container_features
        || List.exists
             (fun (k, vv) ->
               String.equal k key_bool
               && String.equal (String.trim vv) (String.trim v))
             q.container_features

  let eval_scroll_state q ~prop ~value =
    let key = "scroll-state(" ^ prop ^ ": " ^ value ^ ")" in
    List.mem_assoc key q.container_features

  let outer_parens_wrap_all s =
    let len = String.length s in
    let rec loop depth i =
      if i >= len - 1 then true
      else
        match s.[i] with
        | '(' -> loop (depth + 1) (i + 1)
        | ')' when depth = 0 -> false
        | ')' -> loop (depth - 1) (i + 1)
        | _ -> loop depth (i + 1)
    in
    len >= 2 && s.[0] = '(' && s.[len - 1] = ')' && loop 0 1

  (* Strip a single outer paren pair if it wraps the entire input. *)
  let strip_outer_parens s =
    let s = String.trim s in
    if outer_parens_wrap_all s then
      String.sub s 1 (String.length s - 2) |> String.trim
    else s

  (* Find the position of [keyword] at parenthesis depth 0 inside [s]. The
     keyword is matched literally, so callers pass it with surrounding spaces
     ([" and "], [" or "]) to avoid catching identifier substrings. *)
  let find_top_level_keyword s keyword =
    let len = String.length s in
    let klen = String.length keyword in
    let depth = ref 0 in
    let result = ref None in
    let i = ref 0 in
    while !result = None && !i + klen <= len do
      (match s.[!i] with '(' -> incr depth | ')' -> decr depth | _ -> ());
      if !depth = 0 && String.sub s !i klen = keyword then result := Some !i;
      incr i
    done;
    !result

  let parse_style_call body =
    let body = String.trim body in
    if String.contains body ':' then
      match String.split_on_char ':' body with
      | [ name; value ] when String.trim name <> "" && String.trim value <> ""
        ->
          Some (String.trim name, Some (String.trim value))
      | _ -> None
    else if String.length body >= 2 && body.[0] = '-' && body.[1] = '-' then
      Some (body, None)
    else None

  let parse_scroll_state_call body =
    match String.split_on_char ':' (String.trim body) with
    | [ name; value ] -> Some (String.trim name, String.trim value)
    | _ -> None

  let extract_func_call s prefix =
    let s = String.trim s in
    let plen = String.length prefix in
    if String.length s < plen + 1 || String.sub s 0 plen <> prefix then None
    else if s.[String.length s - 1] <> ')' then None
    else Some (String.sub s plen (String.length s - plen - 1))

  let rec eval_raw q raw =
    let s = strip_outer_parens raw in
    match find_top_level_keyword s " or " with
    | Some i ->
        let lhs = String.sub s 0 i in
        let rhs = String.sub s (i + 4) (String.length s - i - 4) in
        eval_raw q lhs || eval_raw q rhs
    | None -> (
        match find_top_level_keyword s " and " with
        | Some i ->
            let lhs = String.sub s 0 i in
            let rhs = String.sub s (i + 5) (String.length s - i - 5) in
            eval_raw q lhs && eval_raw q rhs
        | None -> eval_leaf q s)

  and eval_leaf q s =
    let s = strip_outer_parens s in
    match extract_func_call s "style(" with
    | Some body -> (
        match parse_style_call body with
        | Some (prop, value) -> style_match q ~prop ~value
        | None -> false)
    | None -> (
        match extract_func_call s "scroll-state(" with
        | Some body -> (
            match parse_scroll_state_call body with
            | Some (prop, value) -> eval_scroll_state q ~prop ~value
            | None -> false)
        | None -> (
            (* Fall back to media-feature evaluation for size queries. *)
            let media_q = { q with media_features = q.container_features } in
            try Media_eval.eval media_q (Media.of_string ("(" ^ s ^ ")"))
            with Failure _ | Cursor.Parse_error _ | Reader.Parse_error _ ->
              false))

  let rec eval q ?name : Container.t -> bool =
    let media_q = { q with media_features = q.container_features } in
    fun cond ->
      if not (name_matches ?name q) then false
      else
        match cond with
        | Min_width_rem rem -> Media_eval.eval media_q (Min_width_rem rem)
        | Min_width_px px ->
            Media_eval.eval media_q (Min_width (float_of_int px))
        | Named (n, inner) -> eval q ~name:n inner
        | Style (prop, value) -> style_match q ~prop ~value
        | Scroll_state (prop, value) -> eval_scroll_state q ~prop ~value
        | Feature_query raw -> eval_raw q raw
        | Custom media -> Media_eval.eval media_q media
end

(** {2 Selector matching (CSS Selectors 4)}

    Browsers match selectors right-to-left because the rightmost compound fixes
    the candidate element. Without a tree the matcher reduces to that rightmost
    subject; combinators always succeed against the document so callers should
    only request selector matching when they have already decided which element
    they are testing. *)

module Selector_eval = struct
  let attr_name : Selector.attr_name -> string = function
    | Aria a ->
        let suffix =
          match a with
          | Busy -> "busy"
          | Checked -> "checked"
          | Disabled -> "disabled"
          | Expanded -> "expanded"
          | Hidden -> "hidden"
          | Pressed -> "pressed"
          | Readonly -> "readonly"
          | Required -> "required"
          | Selected -> "selected"
          | Custom s -> s
        in
        "aria-" ^ suffix
    | Data s -> "data-" ^ s
    | Regular s -> s

  let value_match (matcher : Selector.attribute_match) ~flag actual =
    let normalize =
      match flag with
      | Some Selector.Case_insensitive -> String.lowercase_ascii
      | _ -> Fun.id
    in
    let actual = normalize actual in
    let words s =
      String.split_on_char ' ' s |> List.filter (fun s -> s <> "")
    in
    match matcher with
    | Presence -> true
    | Exact v -> String.equal actual (normalize v)
    | Whitespace_list v ->
        let v = normalize v in
        List.exists (String.equal v) (words actual)
    | Hyphen_list v ->
        let v = normalize v in
        String.equal actual v
        || String.length actual > String.length v
           && String.sub actual 0 (String.length v) = v
           && actual.[String.length v] = '-'
    | Prefix v ->
        let v = normalize v in
        String.length actual >= String.length v
        && String.sub actual 0 (String.length v) = v
    | Suffix v ->
        let v = normalize v in
        let la = String.length actual and lv = String.length v in
        la >= lv && String.sub actual (la - lv) lv = v
    | Substring v ->
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
    | Before -> List.mem "before" doc.pseudo_elements
    | After -> List.mem "after" doc.pseudo_elements
    | First_letter -> List.mem "first-letter" doc.pseudo_elements
    | First_line -> List.mem "first-line" doc.pseudo_elements
    | Backdrop -> List.mem "backdrop" doc.pseudo_elements
    | Marker -> List.mem "marker" doc.pseudo_elements
    | Placeholder -> List.mem "placeholder" doc.pseudo_elements
    | Selection -> List.mem "selection" doc.pseudo_elements
    | _ -> false
end

(** {2 URL resolution (RFC 3986)} *)

module Url = struct
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

  (* Collapse a single [..]/[.] segment in [path]. Returns [None] when the path
     has no segments left to normalise. *)
  let normalise_path path =
    let parts = String.split_on_char '/' path in
    let rec loop acc = function
      | [] -> List.rev acc
      | "." :: rest -> loop acc rest
      | ".." :: rest -> (
          match acc with [] -> loop acc rest | _ :: tl -> loop tl rest)
      | seg :: rest -> loop (seg :: acc) rest
    in
    String.concat "/" (loop [] parts)

  let resolve loader href =
    match loader.base_url with
    | None when starts_with ~prefix:"http://" href -> Ok href
    | None when starts_with ~prefix:"https://" href -> Ok href
    | None -> Error ("no base URL to resolve " ^ href)
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
        | Some (dir, _) -> (
            let combined = dir ^ "/" ^ href in
            match cut ~sep:"://" combined with
            | None -> Ok (normalise_path combined)
            | Some (scheme, rest) -> (
                match cut ~sep:"/" rest with
                | None -> Ok (scheme ^ "://" ^ normalise_path rest)
                | Some (host, path) ->
                    Ok (scheme ^ "://" ^ host ^ "/" ^ normalise_path path))))
end

(** {2 [@import] loader (CSS Cascade 5 §6)}

    Resolves the import URL through {!Url}, looks up the body in
    [loader.imports], parses it, and applies any media/supports/layer guards on
    the rule. *)

module Import = struct
  let layer_known ~layer_order = function
    | None -> true
    | Some name -> List.mem name layer_order

  let supports_ok ?query rule =
    match ((rule : Stylesheet.import_rule).supports, query) with
    | None, _ -> true
    | Some _, None -> false
    | Some cond, Some q -> Supports_eval.eval q cond

  let media_ok ?query rule =
    match ((rule : Stylesheet.import_rule).media, query) with
    | None, _ -> true
    | Some _, None -> false
    | Some media, Some q -> Media_eval.eval q media

  let wrap_in_layer rule statements =
    match (rule : Stylesheet.import_rule).layer with
    | None -> statements
    | Some name -> [ Stylesheet.Layer (Some name, statements) ]

  let load ?query ?(layer_order = []) loader (rule : Stylesheet.import_rule) =
    let layer_name = rule.layer in
    if not (layer_known ~layer_order layer_name) then
      Error ("unknown layer " ^ Option.value layer_name ~default:"")
    else if not (supports_ok ?query rule) then
      Error "supports() guard rejected the import"
    else if not (media_ok ?query rule) then
      Error "media guard rejected the import"
    else
      match Url.resolve loader rule.url with
      | Error _ as e -> e
      | Ok resolved -> (
          match List.assoc_opt resolved loader.imports with
          | None -> Error ("import not in loader table: " ^ resolved)
          | Some source -> (
              try
                let cursor = Cursor.of_string source in
                let sheet = Stylesheet.read_stylesheet cursor in
                Ok (wrap_in_layer rule sheet)
              with
              | Failure msg -> Error msg
              | Cursor.Parse_error _ -> Error "stylesheet parse error"))
end

(** {2 Public API surface (forwards to the internal modules)} *)

let matches_supports = Supports_eval.eval
let matches_media = Media_eval.eval
let matches_container = Container_eval.eval
let matches_selector = Selector_eval.eval
let resolve_url = Url.resolve
let load_import = Import.load

(** {2 Computed-value resolution (CSS Cascade 5 §4)}

    Walks a declaration's value, applying CSS-wide keywords, expanding [var()]
    references with cycle detection, evaluating [calc()] arithmetic, resolving
    [currentColor]/[url(...)] against the explicit context, and converting
    relative lengths into absolute pixels. The pipeline operates on the
    declaration's serialised value string; per-property AST is not used so the
    resolver stays small. *)

module Computed_value = struct
  (* CSS Cascade 5 §6.4 lists inherited properties; the rest default to the
     property's initial value when no value is supplied. The list here covers
     the test surface; extend as new test cases reach properties that need a
     different default. *)
  let is_inherited = function
    | "color" | "cursor" | "direction" | "font-family" | "font-feature-settings"
    | "font-kerning" | "font-language-override" | "font-optical-sizing"
    | "font-size" | "font-size-adjust" | "font-stretch" | "font-style"
    | "font-synthesis" | "font-variant" | "font-variant-alternates"
    | "font-variant-caps" | "font-variant-east-asian" | "font-variant-emoji"
    | "font-variant-ligatures" | "font-variant-numeric"
    | "font-variant-position" | "font-weight" | "font" | "hyphens"
    | "letter-spacing" | "line-height" | "list-style" | "list-style-image"
    | "list-style-position" | "list-style-type" | "orphans" | "quotes"
    | "tab-size" | "text-align" | "text-align-last" | "text-decoration-skip-ink"
    | "text-emphasis" | "text-emphasis-color" | "text-emphasis-position"
    | "text-emphasis-style" | "text-indent" | "text-justify"
    | "text-orientation" | "text-rendering" | "text-shadow" | "text-transform"
    | "text-underline-position" | "visibility" | "white-space" | "widows"
    | "word-break" | "word-spacing" | "word-wrap" | "writing-mode" ->
        true
    | _ -> false

  (* The cascade keyword [unset] resolves to [inherit] on inherited properties
     and [initial] otherwise (CSS Cascade 5 §7.5). *)
  let unset_target property =
    if is_inherited property then `Inherit else `Initial

  let trim = String.trim

  (* Walk a value string and replace each [var(...)] expression by calling
     [resolve_var name fallback] which returns either a substitution string or
     [None] when the lookup fails. The walker is paren-aware and supports nested
     calls. *)
  let expand_vars ~resolve_var (s : string) : (string, string) result =
    let buf = Buffer.create (String.length s) in
    let len = String.length s in
    let exception Unresolved of string in
    let rec scan i =
      if i >= len then ()
      else if i + 4 <= len && String.sub s i 4 = "var(" then (
        let body_start = i + 4 in
        let rec find_close depth j =
          if j >= len then j
          else
            match s.[j] with
            | '(' -> find_close (depth + 1) (j + 1)
            | ')' when depth = 0 -> j
            | ')' -> find_close (depth - 1) (j + 1)
            | _ -> find_close depth (j + 1)
        in
        let close = find_close 0 body_start in
        if close >= len then raise (Unresolved "unterminated var()")
        else
          let body = String.sub s body_start (close - body_start) in
          let name, fallback =
            match String.index_opt body ',' with
            | None -> (trim body, None)
            | Some idx ->
                ( trim (String.sub body 0 idx),
                  Some
                    (trim
                       (String.sub body (idx + 1)
                          (String.length body - idx - 1))) )
          in
          let stripped =
            if String.length name >= 2 && String.sub name 0 2 = "--" then
              String.sub name 2 (String.length name - 2)
            else name
          in
          (match resolve_var stripped fallback with
          | Some v -> Buffer.add_string buf v
          | None -> raise (Unresolved ("var(--" ^ stripped ^ ")")));
          scan (close + 1))
      else (
        Buffer.add_char buf s.[i];
        scan (i + 1))
    in
    try
      scan 0;
      Ok (Buffer.contents buf)
    with Unresolved msg -> Error msg

  (* Resolve a [currentColor] keyword by substituting it for the value of the
     [current_color] context field rendered via {!Values.pp_color}. *)
  (* CSS Values 4 §10 [calc()] over absolute/length operands. The evaluator is
     a small Pratt-style parser: terms are either a length (resolved against
     [length_ctx]) or a unitless number, joined by [+ - * /] with the usual
     precedence. Parenthesised sub-expressions are supported. Returns [None]
     when any operand is unresolvable, mirroring the [Unresolved] path of
     [resolve_lengths]. *)
  module Calc = struct
    type token = Num of float | Op of char | Lparen | Rparen

    (* End of a [calc()] term: stop at whitespace, parenthesis, or operator. *)
    let term_terminator = function
      | ' ' | '\t' | '\n' | '+' | '-' | '*' | '/' | '(' | ')' -> true
      | _ -> false

    let read_term_end s i =
      let len = String.length s in
      let rec scan j =
        if j >= len then j else if term_terminator s.[j] then j else scan (j + 1)
      in
      scan i

    let parse_length length_ctx term =
      try
        let cursor = Cursor.of_string term in
        let length = Values.read_length cursor in
        Cursor.ws cursor;
        if Cursor.is_done cursor then Length.to_px length_ctx length else None
      with Cursor.Parse_error _ | Reader.Parse_error _ -> None

    let parse_number term =
      try
        let cursor = Cursor.of_string term in
        let n = Cursor.number cursor in
        Cursor.ws cursor;
        if Cursor.is_done cursor then Some n else None
      with Cursor.Parse_error _ | Reader.Parse_error _ -> None

    let parse_term length_ctx term =
      match parse_length length_ctx term with
      | Some _ as v -> v
      | None -> parse_number term

    let prev_is_operator = function
      | [] | Op _ :: _ | Lparen :: _ -> true
      | _ -> false

    let tokenise length_ctx s =
      let len = String.length s in
      let consume_term i acc =
        let stop = read_term_end s i in
        let term = String.sub s i (stop - i) in
        match parse_term length_ctx term with
        | None -> raise Exit
        | Some v -> (stop, Num v :: acc)
      in
      let rec loop i acc =
        if i >= len then List.rev acc
        else
          match s.[i] with
          | ' ' | '\t' | '\n' -> loop (i + 1) acc
          | '(' -> loop (i + 1) (Lparen :: acc)
          | ')' -> loop (i + 1) (Rparen :: acc)
          | ('+' | '-') as c
            when i + 1 < len
                 && prev_is_operator acc
                 && (s.[i + 1] = '.' || (s.[i + 1] >= '0' && s.[i + 1] <= '9'))
            ->
              (* A leading [+]/[-] is part of a numeric literal when it sits
                 between operators or at the start of an expression. *)
              let _ = c in
              let stop, acc = consume_term i acc in
              loop stop acc
          | ('+' | '-' | '*' | '/') as c -> loop (i + 1) (Op c :: acc)
          | _ ->
              let stop, acc = consume_term i acc in
              loop stop acc
      in
      try Some (loop 0 []) with Exit -> None

    let rec parse_expr tokens = parse_addsub tokens

    and parse_addsub tokens =
      let rec loop lhs tokens =
        match tokens with
        | Op ('+' as op) :: rest | Op ('-' as op) :: rest ->
            let rhs, tokens = parse_muldiv rest in
            let v = if op = '+' then lhs +. rhs else lhs -. rhs in
            loop v tokens
        | _ -> (lhs, tokens)
      in
      let lhs, tokens = parse_muldiv tokens in
      loop lhs tokens

    and parse_muldiv tokens =
      let rec loop lhs tokens =
        match tokens with
        | Op '*' :: rest ->
            let rhs, tokens = parse_atom rest in
            loop (lhs *. rhs) tokens
        | Op '/' :: rest ->
            let rhs, tokens = parse_atom rest in
            if rhs = 0. then raise Exit else loop (lhs /. rhs) tokens
        | _ -> (lhs, tokens)
      in
      let lhs, tokens = parse_atom tokens in
      loop lhs tokens

    and parse_atom tokens =
      match tokens with
      | Num n :: rest -> (n, rest)
      | Lparen :: rest -> (
          let v, tokens = parse_expr rest in
          match tokens with Rparen :: rest -> (v, rest) | _ -> raise Exit)
      | _ -> raise Exit

    let evaluate length_ctx s : float option =
      match tokenise length_ctx s with
      | None -> None
      | Some tokens -> (
          try
            let v, remaining = parse_expr tokens in
            if remaining = [] then Some v else None
          with Exit -> None)
  end

  (* Replace each [calc(...)] expression in [s] with its evaluated px value. *)
  let resolve_calc length_ctx (s : string) : (string, string) result =
    let len = String.length s in
    let buf = Buffer.create len in
    let exception Unresolved of string in
    let rec scan i =
      if i >= len then ()
      else if i + 5 <= len && String.sub s i 5 = "calc(" then (
        let body_start = i + 5 in
        let rec find_close depth j =
          if j >= len then j
          else
            match s.[j] with
            | '(' -> find_close (depth + 1) (j + 1)
            | ')' when depth = 0 -> j
            | ')' -> find_close (depth - 1) (j + 1)
            | _ -> find_close depth (j + 1)
        in
        let close = find_close 0 body_start in
        if close >= len then raise (Unresolved "unterminated calc()")
        else
          let body = String.sub s body_start (close - body_start) in
          (match Calc.evaluate length_ctx body with
          | None -> raise (Unresolved ("calc(" ^ body ^ ")"))
          | Some px ->
              Buffer.add_string buf
                (Pp.to_string ~minify:true
                   (Values.pp_length ~always:true)
                   (Px px)));
          scan (close + 1))
      else (
        Buffer.add_char buf s.[i];
        scan (i + 1))
    in
    try
      scan 0;
      Ok (Buffer.contents buf)
    with Unresolved msg -> Error msg

  let resolve_current_color ctx (s : string) : (string, string) result =
    if
      not
        (String.length s >= 12
        && String.lowercase_ascii (String.sub s 0 12) = "currentcolor")
    then Ok s
    else
      match ctx.current_color with
      | None -> Error "no current_color in context"
      | Some color ->
          let rendered = Pp.to_string ~minify:true Values.pp_color color in
          let rest = String.sub s 12 (String.length s - 12) in
          Ok (rendered ^ rest)

  (* Distinguish between a token that parses as a length but cannot be resolved
     ([`Unresolved]), one that resolves to a [px] string ([`Resolved]), or one
     that is not a length-shaped token at all ([`Not_length]). *)
  let try_resolve_length length_ctx (s : string) :
      [ `Resolved of string | `Unresolved | `Not_length ] =
    let s = String.trim s in
    let parsed =
      try
        let cursor = Cursor.of_string s in
        let length = Values.read_length cursor in
        Cursor.ws cursor;
        if Cursor.is_done cursor then Some length else None
      with Cursor.Parse_error _ | Reader.Parse_error _ -> None
    in
    match parsed with
    | None -> `Not_length
    | Some (Pct _) ->
        (* Defer to the property-aware percentage resolver. *)
        `Not_length
    | Some length -> (
        match Length.to_px length_ctx length with
        | Some px ->
            `Resolved
              (Pp.to_string ~minify:true
                 (Values.pp_length ~always:true)
                 (Px px))
        | None -> `Unresolved)

  (* Resolve a [url(...)] reference against the base URL, if any. *)
  let resolve_url_in_value (ctx : t) (s : string) : (string, string) result =
    let len = String.length s in
    if len < 5 || String.lowercase_ascii (String.sub s 0 4) <> "url(" then Ok s
    else
      let close =
        let rec find i =
          if i >= len then i else if s.[i] = ')' then i else find (i + 1)
        in
        find 4
      in
      if close >= len then Error "unterminated url()"
      else
        let raw_url = String.trim (String.sub s 4 (close - 4)) in
        let raw_url =
          let n = String.length raw_url in
          if
            n >= 2
            && (raw_url.[0] = '"' || raw_url.[0] = '\'')
            && raw_url.[n - 1] = raw_url.[0]
          then String.sub raw_url 1 (n - 2)
          else raw_url
        in
        match ctx.base_url with
        | None
          when not
                 (Url.starts_with ~prefix:"http://" raw_url
                 || Url.starts_with ~prefix:"https://" raw_url) ->
            Error ("no base URL for " ^ raw_url)
        | _ -> (
            let loader = { base_url = ctx.base_url; imports = [] } in
            match Url.resolve loader raw_url with
            | Error msg -> Error msg
            | Ok resolved ->
                let rest = String.sub s (close + 1) (len - close - 1) in
                Ok ("url(" ^ resolved ^ ")" ^ rest))

  (* Resolve every length-shaped token in [s] against [length_ctx]. Tokens that
     parse as lengths but fail to resolve return [Error]; non-length tokens
     (idents, keywords, colors) pass through unchanged. *)
  let resolve_lengths length_ctx (s : string) : (string, string) result =
    let parts = String.split_on_char ' ' s |> List.filter (fun s -> s <> "") in
    let exception Unresolved of string in
    try
      let resolved =
        List.map
          (fun part ->
            match try_resolve_length length_ctx part with
            | `Resolved px -> px
            | `Unresolved -> raise (Unresolved part)
            | `Not_length -> part)
          parts
      in
      Ok (String.concat " " resolved)
    with Unresolved token -> Error ("unresolved length: " ^ token)

  (* CSS Cascade 5 §6.4.3 layered custom-property lookup.

     Important-flagged declarations beat normal ones. For normal author rules
     the unlayered declaration wins, otherwise later layers beat earlier ones.
     For important author rules the order reverses: earlier layers beat later
     ones, and unlayered ranks below them.

     [layer_order] supplies the cascade order over layer names; the lookup picks
     the candidate with the highest priority per the rules above. *)
  let layer_index ~layer_order = function
    | None -> max_int (* unlayered, highest in normal cascade *)
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

  (* Layered candidates extracted from a [custom_properties] pool, paired with
     their [layer_order] index so callers can pick the highest- or lowest-ranked
     entry without re-scanning. Revert-layer placeholders are dropped because
     they do not contribute their own value. *)
  let layered_candidates ~layer_order pool =
    List.filter_map
      (fun d ->
        if is_revert_layer d then None
        else
          match Declaration.custom_declaration_layer d with
          | None -> None
          | Some _ as l -> Some (layer_index ~layer_order l, d))
      pool

  let pick_first_in pool ~compare_score =
    match List.sort (fun (a, _) (b, _) -> compare_score a b) pool with
    | [] -> None
    | (_, d) :: _ -> Some d

  let pick_latest_layer ~layer_order pool =
    pick_first_in (layered_candidates ~layer_order pool)
      ~compare_score:(fun a b -> compare b a)

  let pick_earliest_layer ~layer_order pool =
    pick_first_in (layered_candidates ~layer_order pool) ~compare_score:compare

  let by_layer scope pool =
    List.find_opt (fun d -> Declaration.custom_declaration_layer d = scope) pool

  (* CSS Cascade 5 §7.4: rolling [revert-layer] back to the next earlier layer
     for the same custom property. *)
  let revert_layer_fallback ~layer_order pool scope =
    let scope_index =
      match scope with
      | None -> max_int
      | Some _ as l -> layer_index ~layer_order l
    in
    let earlier =
      List.filter
        (fun (idx, _) -> idx < scope_index)
        (layered_candidates ~layer_order pool)
    in
    pick_first_in earlier ~compare_score:(fun a b -> compare b a)

  let resolve_with_revert ~layer_order pool d scope =
    if is_revert_layer d then revert_layer_fallback ~layer_order pool scope
    else Some d

  let pick_normal ?layer ~layer_order pool =
    match layer with
    | None -> (
        match by_layer None pool with
        | Some d -> resolve_with_revert ~layer_order pool d None
        | None -> pick_latest_layer ~layer_order pool)
    | Some scope -> (
        match by_layer (Some scope) pool with
        | Some d -> resolve_with_revert ~layer_order pool d (Some scope)
        | None -> pick_earliest_layer ~layer_order pool)

  (* Important reverses cascade order: earlier layers win, unlayered ranks below
     all named layers. *)
  let pick_important ~layer_order pool =
    let unlayered_rank = max_int in
    let rank_of d =
      match Declaration.custom_declaration_layer d with
      | None -> unlayered_rank
      | Some _ as l -> layer_index ~layer_order l
    in
    pick_first_in
      (List.map (fun d -> (rank_of d, d)) pool)
      ~compare_score:compare

  let lookup_custom ?layer ?(layer_order = []) ctx name =
    let target = "--" ^ name in
    let candidates =
      List.filter
        (fun d -> Declaration.property_name d = target)
        ctx.custom_properties
    in
    let important, normal =
      List.partition Declaration.is_important candidates
    in
    match pick_important ~layer_order important with
    | Some _ as v -> v
    | None -> pick_normal ?layer ~layer_order normal

  let initial_value_str ctx property =
    match
      List.find_opt
        (fun d -> Declaration.property_name d = property)
        ctx.initial_values
    with
    | None -> None
    | Some d -> Some (Declaration.string_of_value ~minify:true d)

  let inherited_value_str ctx property =
    match
      List.find_opt
        (fun d -> Declaration.property_name d = property)
        ctx.inherited_values
    with
    | None -> None
    | Some d -> Some (Declaration.string_of_value ~minify:true d)

  (* Resolve a custom property var() reference with cycle detection. *)
  let resolve_var_chain ?layer ?layer_order ctx ~visited name fallback =
    if List.mem name visited then None
    else
      match lookup_custom ?layer ?layer_order ctx name with
      | None -> fallback
      | Some d -> Some (Declaration.string_of_value ~minify:true d)

  let rec expand_value ?layer ?layer_order ctx ~visited s :
      (string, string) result =
    let resolve_var name fallback =
      match
        resolve_var_chain ?layer ?layer_order ctx ~visited name fallback
      with
      | None -> None
      | Some raw -> (
          (* Recursively expand vars in the resolved value using the updated
             [visited] list to detect cycles. *)
          match
            expand_value ?layer ?layer_order ctx ~visited:(name :: visited) raw
          with
          | Ok expanded -> Some expanded
          | Error _ -> None)
    in
    expand_vars ~resolve_var s

  (* CSS Values 4 §6.3: percentages compute against a property-specific
     reference. The resolver here only handles font-size (parent font-size);
     other properties either keep the percentage as-is or reject when the
     reference is missing. *)
  let resolve_percentage ~property ~parent_font_size_px (s : string) :
      (string, string) result =
    let parts = String.split_on_char ' ' s |> List.filter (fun s -> s <> "") in
    let pct_to_px part =
      let n = String.length part in
      if n > 1 && part.[n - 1] = '%' then
        match float_of_string_opt (String.sub part 0 (n - 1)) with
        | Some pct -> Some pct
        | None -> None
      else None
    in
    let exception Unresolved of string in
    try
      let resolved =
        List.map
          (fun part ->
            match pct_to_px part with
            | None -> part
            | Some pct ->
                if String.equal property "font-size" then
                  match parent_font_size_px with
                  | None -> raise (Unresolved part)
                  | Some base ->
                      Pp.to_string ~minify:true
                        (Values.pp_length ~always:true)
                        (Px (pct /. 100. *. base))
                else raise (Unresolved part))
          parts
      in
      Ok (String.concat " " resolved)
    with Unresolved token -> Error ("unresolved percentage: " ^ token)

  let resolve ?layer ?layer_order ctx ~property ~value =
    let value = trim value in
    let layer_known =
      match (layer, layer_order) with
      | None, _ -> true
      | Some name, Some order -> List.mem name order
      | Some _, None -> true
    in
    if not layer_known then
      Error
        ("unknown layer "
        ^ Option.value layer ~default:""
        ^ " for scoped resolution")
    else
      match value with
      | "initial" -> (
          match initial_value_str ctx property with
          | Some v -> Ok v
          | None -> Ok "initial")
      | "inherit" -> (
          match inherited_value_str ctx property with
          | Some v -> Ok v
          | None -> (
              match initial_value_str ctx property with
              | Some v -> Ok v
              | None -> Ok "inherit"))
      | "unset" -> (
          let target = unset_target property in
          match target with
          | `Inherit -> (
              match inherited_value_str ctx property with
              | Some v -> Ok v
              | None -> (
                  match initial_value_str ctx property with
                  | Some v -> Ok v
                  | None -> Ok "unset"))
          | `Initial -> (
              match initial_value_str ctx property with
              | Some v -> Ok v
              | None -> Ok "unset"))
      | _ -> (
          match expand_value ?layer ?layer_order ctx ~visited:[] value with
          | Error msg -> Error msg
          | Ok expanded -> (
              let length_ctx = Length.of_t ctx in
              match resolve_current_color ctx expanded with
              | Error msg -> Error msg
              | Ok with_color -> (
                  match resolve_url_in_value ctx with_color with
                  | Error msg -> Error msg
                  | Ok with_url -> (
                      match resolve_calc length_ctx with_url with
                      | Error msg -> Error msg
                      | Ok with_calc -> (
                          match resolve_lengths length_ctx with_calc with
                          | Error msg -> Error msg
                          | Ok with_lengths ->
                              resolve_percentage ~property
                                ~parent_font_size_px:length_ctx.parent_font_size
                                with_lengths)))))
end

let computed_value ?layer_order ?layer ctx decl =
  let property = Declaration.property_name decl in
  let value = Declaration.string_of_value ~minify:true decl in
  Computed_value.resolve ?layer ?layer_order ctx ~property ~value
