(** CSS variables and variable extraction utilities *)

open Values
open Properties
open Declaration
include Variables_intf

(** {1 Custom Property Support} *)

(** Pretty-print a syntax descriptor to CSS syntax string *)
let rec pp_syntax_inner : type a. a syntax Pp.t =
 fun ctx syn ->
  match syn with
  | Length -> Pp.string ctx "<length>"
  | Color -> Pp.string ctx "<color>"
  | Number -> Pp.string ctx "<number>"
  | Integer -> Pp.string ctx "<integer>"
  | Percentage -> Pp.string ctx "<percentage>"
  | Length_percentage -> Pp.string ctx "<length-percentage>"
  | Angle -> Pp.string ctx "<angle>"
  | Time -> Pp.string ctx "<time>"
  | Resolution -> Pp.string ctx "<resolution>"
  | Custom_ident -> Pp.string ctx "<custom-ident>"
  | String -> Pp.string ctx "<string>"
  | Url -> Pp.string ctx "<url>"
  | Image -> Pp.string ctx "<image>"
  | Transform_function -> Pp.string ctx "<transform-function>"
  | Transform_list -> Pp.string ctx "<transform-list>"
  | Universal -> Pp.string ctx "*"
  | Or (syn1, syn2) ->
      pp_syntax_inner ctx syn1;
      Pp.string ctx " | ";
      pp_syntax_inner ctx syn2
  | Plus syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "+"
  | Hash syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "#"
  | Ident_keyword name -> Pp.string ctx name

and pp_syntax : type a. a syntax Pp.t =
 fun ctx syn ->
  (* Syntax descriptors should be printed with quotes per CSS spec *)
  Pp.char ctx '"';
  pp_syntax_inner ctx syn;
  Pp.char ctx '"'

(** Pretty-print a value according to its syntax type *)
let rec pp_value : type a. a syntax -> a Pp.t =
 fun syntax ctx value ->
  match syntax with
  | Length -> Values.pp_length ~always:true ctx value
  | Color -> Values.pp_color ctx value
  | Number -> Pp.float ctx value
  | Integer -> Pp.int ctx value
  | Percentage -> Values.pp_percentage ~always:true ctx value
  | Length_percentage -> Values.pp_length_percentage ~always:true ctx value
  | Angle -> Values.pp_angle ctx value
  | Time -> Values.pp_duration ctx value
  | Resolution -> Pp.string ctx value
  | Custom_ident -> Pp.string ctx value
  | String -> Pp.quoted ctx value
  | Url ->
      Pp.string ctx "url(";
      Pp.quoted ctx value;
      Pp.string ctx ")"
  | Image -> Pp.string ctx value
  | Transform_function -> Pp.string ctx value
  | Transform_list -> Pp.string ctx value
  | Universal -> Pp.string ctx value
  | Or (syn1, syn2) -> (
      match value with
      | Left v -> pp_value syn1 ctx v
      | Right v -> pp_value syn2 ctx v)
  | Plus syn ->
      List.iteri
        (fun i v ->
          if i > 0 then Pp.sp ctx ();
          pp_value syn ctx v)
        value
  | Hash syn ->
      List.iteri
        (fun i v ->
          if i > 0 then Pp.string ctx ", ";
          pp_value syn ctx v)
        value
  | Ident_keyword name -> Pp.string ctx name

(* CSS Properties and Values API 1 §2 lists the named [<...>] type references.
   Bare ident keywords match the [<custom-ident>] shape so a leading letter
   followed by ident-cont characters counts; this rejects stray punctuation. *)
let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '-'

let is_ident_cont c = is_ident_start c || (c >= '0' && c <= '9')

let is_ident_keyword s =
  String.length s > 0
  && is_ident_start s.[0]
  &&
  let rec loop i =
    if i = String.length s then true
    else if is_ident_cont s.[i] then loop (i + 1)
    else false
  in
  loop 1

(** Read a CSS syntax descriptor from input *)
let read_simple_syntax_component r s : any_syntax =
  match s with
  | "<length>" -> Syntax Length
  | "<color>" -> Syntax Color
  | "<number>" -> Syntax Number
  | "<integer>" -> Syntax Integer
  | "<percentage>" -> Syntax Percentage
  | "<length-percentage>" -> Syntax Length_percentage
  | "<angle>" -> Syntax Angle
  | "<time>" -> Syntax Time
  | "<resolution>" -> Syntax Resolution
  | "<custom-ident>" -> Syntax Custom_ident
  | "<string>" -> Syntax String
  | "<url>" -> Syntax Url
  | "<image>" -> Syntax Image
  | "<transform-function>" -> Syntax Transform_function
  | "<transform-list>" -> Syntax Transform_list
  | "*" -> Syntax Universal
  | s when is_ident_keyword s -> Syntax (Ident_keyword s)
  | s -> Cursor.err_invalid r ("Unsupported CSS syntax: " ^ s)

(* CSS Properties and Values API 1 §2: only [+] and [#] are valid syntax
   multipliers. *)
let apply_syntax_modifier r (Syntax inner) (modifier : char option) : any_syntax
    =
  match modifier with
  | None -> Syntax inner
  | Some '+' -> Syntax (Plus inner)
  | Some '#' -> Syntax (Hash inner)
  | Some c ->
      Cursor.err_invalid r
        (String.concat ""
           [ "Unsupported CSS syntax modifier: '"; String.make 1 c; "'" ])

let split_syntax_modifier s : string * char option =
  let n = String.length s in
  if n = 0 then (s, None)
  else
    match s.[n - 1] with
    | ('+' | '#') as m -> (String.sub s 0 (n - 1), Some m)
    | _ -> (s, None)

let read_syntax (r : Cursor.t) : any_syntax =
  (* CSS @property syntax values must be quoted strings per spec *)
  let s = Cursor.string r in
  let read_component part =
    let body, modifier = split_syntax_modifier (String.trim part) in
    if body = "" then Cursor.err_invalid r "empty CSS syntax component";
    apply_syntax_modifier r (read_simple_syntax_component r body) modifier
  in
  if String.contains s '|' then
    let parts = String.split_on_char '|' s in
    let components = List.map read_component parts in
    match components with
    | [] | [ _ ] ->
        (* split_on_char never returns empty list; the single-element case
           cannot reach here because '|' is in [s]. *)
        Cursor.err_invalid r "invalid CSS syntax disjunction"
    | first :: rest ->
        List.fold_left
          (fun (Syntax left) (Syntax right) -> Syntax (Or (left, right)))
          first rest
  else read_component s

(** Read a value according to its syntax type *)
let rec read_value : type a. Cursor.t -> a syntax -> a =
 fun reader syntax ->
  match syntax with
  | Universal ->
      (* For universal syntax "*", accept any CSS value — serialise the
         remaining components back to source text. *)
      Cursor.remaining_to_string ~trim:true reader
  | String -> Cursor.string ~trim:true reader
  | Custom_ident -> Cursor.ident ~keep_case:true reader
  | Url -> Cursor.url reader
  | Image -> Cursor.remaining_to_string ~trim:true reader
  | Transform_function -> Cursor.remaining_to_string ~trim:true reader
  | Transform_list -> Cursor.remaining_to_string ~trim:true reader
  | Resolution -> Cursor.remaining_to_string ~trim:true reader
  | Length -> Values.read_length reader
  | Color -> Values.read_color reader
  | Number -> Cursor.number reader
  | Integer -> int_of_float (Cursor.number reader)
  | Percentage -> Values.read_percentage reader
  | Length_percentage -> Values.read_length_percentage reader
  | Angle -> Values.read_angle reader
  | Time -> Values.read_duration reader
  | Or (syn1, syn2) -> (
      (* Try the left branch with backtracking; fall back to the right. *)
      match Cursor.option (fun r -> read_value r syn1) reader with
      | Some v -> Either.Left v
      | None -> Either.Right (read_value reader syn2))
  | Plus syn ->
      (* Read space-separated list - use Cursor.many for proper error
         handling *)
      let values, _error_opt = Cursor.many (fun r -> read_value r syn) reader in
      if values = [] then
        Cursor.err_invalid reader "expected at least one value for '+' syntax"
      else values
  | Hash syn ->
      (* Read comma-separated list - use Cursor.list for proper parsing *)
      let values =
        Cursor.list ~sep:Cursor.comma ~at_least:1
          (fun r -> read_value r syn)
          reader
      in
      values
  | Ident_keyword name ->
      let got = Cursor.ident reader in
      if got <> name then
        Cursor.err_invalid reader
          (String.concat ""
             [ "expected keyword '"; name; "', got '"; got; "'" ])

(** {1 Meta handling} *)

let meta (type t) () =
  let module M = struct
    type meta += V : t -> meta
  end in
  let inj x = M.V x in
  let proj = function M.V v -> Some v | _ -> None in
  (inj, proj)

(** {1 Variable creation} *)

let var : type a.
    ?default:a ->
    ?fallback:a fallback ->
    ?layer:string ->
    ?meta:meta ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?default ?fallback ?layer ?meta name kind value ->
  (* Create the declaration directly with the value *)
  let decl =
    Declaration.v
      (Custom_property (String.concat "" [ "--"; name ]))
      (Custom_value { kind; value; layer; meta })
  in
  let fallback : a fallback =
    match fallback with None -> None | Some v -> v
  in
  (* Use the value as default if no explicit default provided *)
  let default_value =
    match default with Some d -> Some d | None -> Some value
  in
  let var_handle = { name; fallback; default = default_value; layer; meta } in
  (decl, var_handle)

(** {1 Variable extraction} *)

let rec vars_of_calc : type a. a calc -> any_var list = function
  | Val _ -> []
  | Var v -> [ V v ]
  | Num _ -> []
  | Sibling_index -> []
  | Sibling_count -> []
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right
  | Nested inner -> vars_of_calc inner
  | Parens inner -> vars_of_calc inner

let rec vars_of_length (value : Values.length) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | Round (_, value, step) -> vars_of_length value @ vars_of_length step
  | Mod (a, b) | Rem_fn (a, b) | Hypot (a, b) ->
      vars_of_length a @ vars_of_length b
  | Abs value | Sign value -> vars_of_length value
  | Calc_size (basis, calc) -> vars_of_length basis @ vars_of_calc calc
  | Anchor (_, _, Some fallback) -> vars_of_length fallback
  | _ -> []

let vars_of_length_list (values : Values.length list) : any_var list =
  List.concat_map vars_of_length values

(** Helper for types that share Length/Var/Calc constructors with a wildcard
    fallback (e.g., length_percentage, font_size). The caller decomposes the
    value into one of three cases. *)
let vars_of_lvc = function
  | `Length l -> vars_of_length l
  | `Var v -> [ v ]
  | `Calc calc -> vars_of_calc calc
  | `Other -> []

let vars_of_length_percentage (value : Values.length_percentage) : any_var list
    =
  vars_of_lvc
    (match value with
    | Length l -> `Length l
    | Var v -> `Var (V v)
    | Calc calc -> `Calc calc
    | _ -> `Other)

let vars_of_font_size (value : Properties.font_size) : any_var list =
  vars_of_lvc
    (match value with
    | Length l -> `Length l
    | Var v -> `Var (V v)
    | Calc calc -> `Calc calc
    | _ -> `Other)

let vars_of_angle (value : Values.angle) : any_var list =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_rotate_value (value : Properties.rotate_value) : any_var list =
  match value with
  | Angle a | X a | Y a | Z a -> vars_of_angle a
  | Axis (_, _, _, a) -> vars_of_angle a
  | Var v -> [ V v ]
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_channel (value : Values.channel) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_rgb (value : Values.rgb) : any_var list =
  match value with
  | Channels { r; g; b } ->
      vars_of_channel r @ vars_of_channel g @ vars_of_channel b
  | Var v -> [ V v ]

let vars_of_alpha (value : Values.alpha) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_hue (value : Values.hue) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Angle angle -> vars_of_angle angle
  | _ -> []

let vars_of_component (value : Values.component) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | Angle hue -> vars_of_hue hue
  | Component_none -> []
  | _ -> []

let vars_of_percentage (value : Values.percentage) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let rec vars_of_color (value : Values.color) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Rgb rgb -> vars_of_rgb rgb
  | Rgba { rgb; a } -> vars_of_rgb rgb @ vars_of_alpha a
  | Hsl { h; s; l; a } ->
      vars_of_hue h @ vars_of_percentage s @ vars_of_percentage l
      @ vars_of_alpha a
  | Hwb { h; w; b; a } ->
      vars_of_hue h @ vars_of_percentage w @ vars_of_percentage b
      @ vars_of_alpha a
  | Color { components; alpha; _ } ->
      List.concat_map vars_of_component components @ vars_of_alpha alpha
  | Relative_rgb _ -> []
  | Contrast_color color -> vars_of_color color
  | Light_dark (light, dark) -> vars_of_color light @ vars_of_color dark
  | Lab { l; alpha; _ } -> vars_of_percentage l @ vars_of_alpha alpha
  | Oklch { l; h; alpha; _ } ->
      vars_of_percentage l @ vars_of_hue h @ vars_of_alpha alpha
  | Oklab { l; alpha; _ } -> vars_of_percentage l @ vars_of_alpha alpha
  | Lch { l; h; alpha; _ } ->
      vars_of_percentage l @ vars_of_hue h @ vars_of_alpha alpha
  | Mix { color1; percent1; color2; percent2; _ } ->
      let c1_vars = vars_of_color color1 in
      let c2_vars = vars_of_color color2 in
      let p1_vars =
        match percent1 with Some p -> vars_of_percentage p | None -> []
      in
      let p2_vars =
        match percent2 with Some p -> vars_of_percentage p | None -> []
      in
      c1_vars @ c2_vars @ p1_vars @ p2_vars
  | _ -> []

let vars_of_duration (value : Values.duration) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_border_width (value : Properties.border_width) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_border_width_list values =
  List.concat_map vars_of_border_width values

let vars_of_line_height (value : Properties.line_height) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

(* Helper for optional length properties in Gap *)
let vars_of_optional_length : Values.length option -> any_var list = function
  | Some (Var v) -> [ V v ]
  | Some (Calc calc) -> vars_of_calc calc
  | _ -> []

(* Complex type extractors *)
let vars_of_font_weight (value : Properties.font_weight) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_family (value : Properties.font_family) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform (value : Properties.transform) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_list (value : Properties.transform list) : any_var list =
  List.concat_map vars_of_transform value

let rec vars_of_shadow (value : Properties.shadow) : any_var list =
  match value with
  | Var v -> [ V v ]
  | List shadows -> List.concat_map vars_of_shadow shadows
  | Shadow { color; _ } -> (
      match color with Some c -> vars_of_color c | None -> [])
  | _ -> []

let vars_of_content (value : Properties.content) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_blend_mode (value : Properties.blend_mode) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_style (value : Properties.border_style) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration (value : Properties.text_decoration) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_transform (value : Properties.text_transform) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scale (value : Properties.scale) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_translate_value (value : Properties.translate_value) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | X len -> vars_of_length len
  | XY (len1, len2) -> vars_of_length len1 @ vars_of_length len2
  | XYZ (len1, len2, len3) ->
      vars_of_length len1 @ vars_of_length len2 @ vars_of_length len3
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_quotes (value : Properties.quotes) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation (value : Properties.animation) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transition (value : Properties.transition) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_filter (value : Properties.filter) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background (value : Properties.background) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_content_visibility (value : Properties.content_visibility) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_feature_settings (value : Properties.font_feature_settings) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variation_settings (value : Properties.font_variation_settings)
    : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_numeric (value : Properties.font_variant_numeric) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_strictness (value : Properties.scroll_snap_strictness) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_axis (value : Properties.scroll_snap_axis) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_type (value : Properties.scroll_snap_type) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Axis axis -> vars_of_scroll_snap_axis axis
  | Axis_with_strictness (axis, strictness) ->
      vars_of_scroll_snap_axis axis @ vars_of_scroll_snap_strictness strictness
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_aspect_ratio (value : Properties.aspect_ratio) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background_image (value : Properties.background_image) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background_size (value : Properties.background_size) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_columns_value (value : Properties.columns_value) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Width l -> vars_of_length l
  | Both (l, _) -> vars_of_length l
  | _ -> []

let vars_of_contain (value : Properties.contain) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_cursor (value : Properties.cursor) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Url (_, _, fallback) -> vars_of_cursor fallback
  | _ -> []

let rec vars_of_grid_template (value : Properties.grid_template) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | Fit_content l -> vars_of_length l
  | Min_max (a, b) -> vars_of_grid_template a @ vars_of_grid_template b
  | Repeat (_, ts) -> List.concat_map vars_of_grid_template ts
  | Tracks ts -> List.concat_map vars_of_grid_template ts
  | Split (rows, columns) ->
      vars_of_grid_template rows @ vars_of_grid_template columns
  | Named_tracks ts ->
      List.concat_map (fun (_, t) -> vars_of_grid_template t) ts
  | _ -> []

let vars_of_grid_line (value : Properties.grid_line) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_image (value : Properties.list_style_image) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_type (value : Properties.list_style_type) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_value (value : Properties.position_value) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Single l -> vars_of_length l
  | XY (l1, l2) -> vars_of_length l1 @ vars_of_length l2
  | Edge_offset_axis (_, lp, _) -> vars_of_length_percentage lp
  | Edge_offset_edge_offset (_, lp1, _, lp2) ->
      vars_of_length_percentage lp1 @ vars_of_length_percentage lp2
  | _ -> []

let vars_of_outline_style (value : Properties.outline_style) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_shadow (value : Properties.text_shadow) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Text_shadow { h_offset; v_offset; blur; color; _ } -> (
      vars_of_length h_offset @ vars_of_length v_offset
      @ (match blur with Some l -> vars_of_length l | None -> [])
      @ match color with Some c -> vars_of_color c | None -> [])
  | _ -> []

let vars_of_text_shadow_list (values : Properties.text_shadow list) :
    any_var list =
  List.concat_map vars_of_text_shadow values

let vars_of_transform_origin (value : Properties.transform_origin) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | X l -> vars_of_length l
  | XY (l1, l2) -> vars_of_length l1 @ vars_of_length l2
  | XYZ (l1, l2, l3) ->
      vars_of_length l1 @ vars_of_length l2 @ vars_of_length l3
  | _ -> []

let vars_of_vertical_align (value : Properties.vertical_align) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_will_change (value : Properties.will_change) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_opacity (value : Properties.opacity) : any_var list =
  match value with
  | Opacity_number _ -> []
  | Abs v | Sign v -> vars_of_opacity v
  | Var v -> [ V v ]
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_tab_size (value : Properties.tab_size) : any_var list =
  match value with
  | Int _ -> []
  | Length len -> vars_of_length len
  | Var v -> [ V v ]
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name

(** {1 Variable name utilities} *)

let any_var_name (V v) = String.concat "" [ "--"; v.name ]

(** Extract variables from timing function *)
let vars_of_timing_function = function
  | Ease | Linear | Ease_in | Ease_out | Ease_in_out | Step_start | Step_end
  | Steps _ | Cubic_bezier _ | Linear_function _ | Inherit | Initial | Unset
  | Revert | Revert_layer ->
      []
  | Var v -> [ V v ]

let vars_of_display (value : Properties.display) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position (value : Properties.position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow (value : Properties.overflow) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_direction (value : Properties.flex_direction) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_wrap (value : Properties.flex_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_factor (value : Properties.flex_factor) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_align_content (value : Properties.align_content) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_align_items (value : Properties.align_items) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_align_self (value : Properties.align_self) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_content (value : Properties.justify_content) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_items (value : Properties.justify_items) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_self (value : Properties.justify_self) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_place_content (value : Properties.place_content) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_place_items (value : Properties.place_items) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_auto_flow (value : Properties.grid_auto_flow) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_container_name (value : Properties.container_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_anchor_name (value : Properties.anchor_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_anchor (value : Properties.position_anchor) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_try_fallbacks (value : Properties.position_try_fallbacks) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow_anchor (value : Properties.overflow_anchor) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scrollbar_width (value : Properties.scrollbar_width) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scrollbar_color (value : Properties.scrollbar_color) =
  match value with
  | Var v -> [ V v ]
  | Colors (thumb, track) -> vars_of_color thumb @ vars_of_color track
  | Auto | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_scrollbar_gutter (value : Properties.scrollbar_gutter) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_palette (value : Properties.font_palette) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis (value : Properties.font_synthesis) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation_timeline (value : Properties.animation_timeline) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation_range_item (value : Properties.animation_range_item) =
  match value with
  | Normal -> []
  | Offset lp | Named (_, lp) -> vars_of_length_percentage lp

let vars_of_animation_range (value : Properties.animation_range) =
  match value with
  | Var v -> [ V v ]
  | Range (first, second) ->
      vars_of_animation_range_item first
      @ Option.fold ~none:[] ~some:vars_of_animation_range_item second
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_view_transition_name (value : Properties.view_transition_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_image_orientation (value : Properties.image_orientation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_contain_intrinsic_size_item
    (value : Properties.contain_intrinsic_size_item) =
  match value with Length len | Auto len -> vars_of_length len

let vars_of_contain_intrinsic_size (value : Properties.contain_intrinsic_size) =
  match value with
  | Var v -> [ V v ]
  | Intrinsic (first, second) ->
      vars_of_contain_intrinsic_size_item first
      @ Option.fold ~none:[] ~some:vars_of_contain_intrinsic_size_item second
  | None | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_margin_trim (value : Properties.margin_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ray (value : Properties.ray) =
  vars_of_angle value.angle
  @ Option.fold ~none:[] ~some:vars_of_position_value value.position

let vars_of_offset_path (value : Properties.offset_path) =
  match value with
  | Var v -> [ V v ]
  | Ray ray -> vars_of_ray ray
  | None | Url _ | Path _ | Initial | Inherit | Unset | Revert | Revert_layer ->
      []

let vars_of_flex (value : Properties.flex) =
  match value with
  | Var v -> [ V v ]
  | Basis b | Full (_, _, b) -> (
      match b with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> [])
  | _ -> []

let vars_of_font_style (value : Properties.font_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_align (value : Properties.text_align) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_visibility (value : Properties.visibility) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration_line (value : Properties.text_decoration_line) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration_style (value : Properties.text_decoration_style) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_text_overflow (value : Properties.text_overflow) =
  match value with
  | Var v -> [ V v ]
  | Pair (first, second) ->
      vars_of_text_overflow first @ vars_of_text_overflow second
  | _ -> []

let vars_of_text_wrap (value : Properties.text_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_wrap_style (value : Properties.text_wrap_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_box_trim (value : Properties.text_box_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_spacing_trim (value : Properties.text_spacing_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_hyphenate_limit_chars (value : Properties.hyphenate_limit_chars) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_initial_letter (value : Properties.initial_letter) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_white_space (value : Properties.white_space) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_word_break (value : Properties.word_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow_wrap (value : Properties.overflow_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_position (value : Properties.list_style_position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border (value : Properties.border) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { width; style; color } ->
      Option.value ~default:[] (Option.map vars_of_border_width width)
      @ Option.value ~default:[] (Option.map vars_of_border_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let vars_of_outline (value : Properties.outline) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { width; style; color } ->
      Option.value ~default:[] (Option.map vars_of_length width)
      @ Option.value ~default:[] (Option.map vars_of_outline_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let vars_of_background_attachment (value : Properties.background_attachment) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background_box (value : Properties.background_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background_repeat (value : Properties.background_repeat) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_collapse (value : Properties.border_collapse) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_box_sizing (value : Properties.box_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_style (value : Properties.transform_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_box (value : Properties.transform_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_backface_visibility (value : Properties.backface_visibility) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation_direction (value : Properties.animation_direction) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation_fill_mode (value : Properties.animation_fill_mode) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation_iteration_count
    (value : Properties.animation_iteration_count) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transition_behavior (value : Properties.transition_behavior) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_appearance (value : Properties.appearance) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_appearance (value : Properties.webkit_appearance) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_color_scheme (value : Properties.color_scheme) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_print_color_adjust (value : Properties.print_color_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_box_decoration_break (value : Properties.box_decoration_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_clear (value : Properties.clear) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_float_side (value : Properties.float_side) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration_skip_ink
    (value : Properties.text_decoration_skip_ink) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_forced_color_adjust (value : Properties.forced_color_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_pointer_events (value : Properties.pointer_events) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_resize (value : Properties.resize) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_object_fit (value : Properties.object_fit) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_container_type (value : Properties.container_type) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_container_shorthand (value : Properties.container_shorthand) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { ctype = Some ctype; _ } -> vars_of_container_type ctype
  | _ -> []

let vars_of_break_value (value : Properties.break_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_break_inside_value (value : Properties.break_inside_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size_name (value : Properties.page_size_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size_orientation (value : Properties.page_size_orientation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size (value : Properties.page_size) =
  match value with
  | Var v -> [ V v ]
  | Single l -> vars_of_length l
  | Pair (a, b) -> vars_of_length a @ vars_of_length b
  | Named name -> vars_of_page_size_name name
  | Named_oriented (name, orientation) ->
      vars_of_page_size_name name @ vars_of_page_size_orientation orientation
  | Oriented orientation -> vars_of_page_size_orientation orientation
  | _ -> []

let rec vars_of_scroll_snap_align (value : Properties.scroll_snap_align) =
  match value with
  | Var v -> [ V v ]
  | Snap_align_pair (a, b) ->
      vars_of_scroll_snap_align a @ vars_of_scroll_snap_align b
  | _ -> []

let vars_of_scroll_snap_stop (value : Properties.scroll_snap_stop) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overscroll_behavior (value : Properties.overscroll_behavior) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_svg_paint (value : Properties.svg_paint) =
  match value with
  | Var v -> [ V v ]
  | Color c -> vars_of_color c
  | Url (_, fallback) ->
      Option.value ~default:[] (Option.map vars_of_svg_paint fallback)
  | _ -> []

let vars_of_unicode_bidi (value : Properties.unicode_bidi) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_writing_mode (value : Properties.writing_mode) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_font_smoothing (value : Properties.webkit_font_smoothing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_moz_osx_font_smoothing (value : Properties.moz_osx_font_smoothing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_box_orient (value : Properties.webkit_box_orient) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_moz_orient (value : Properties.moz_orient) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_stretch (value : Properties.font_stretch) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_size_adjust (value : Properties.font_size_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_emoji (value : Properties.font_variant_emoji) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_size_adjust (value : Properties.text_size_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_touch_action (value : Properties.touch_action) =
  match value with
  | Var v -> [ V v ]
  | Vars vars -> List.map (fun v -> V v) vars
  | _ -> []

let vars_of_clip (value : Properties.clip) =
  match value with
  | Var v -> [ V v ]
  | Clip_rect (a, b, c, d) ->
      vars_of_length a @ vars_of_length b @ vars_of_length c @ vars_of_length d
  | _ -> []

let vars_of_border_radius (value : Properties.border_radius) =
  let from_list = List.concat_map vars_of_length_percentage in
  match value with
  | Var v -> [ V v ]
  | Radius { horizontal; vertical } ->
      from_list horizontal
      @ Option.value ~default:[] (Option.map from_list vertical)
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_perspective_origin (value : Properties.perspective_origin) =
  vars_of_position_value value

let vars_of_clip_path (value : Properties.clip_path) =
  match value with
  | Var v -> [ V v ]
  | Clip_path_inset (a, b, c, d) ->
      vars_of_length a
      @ Option.value ~default:[] (Option.map vars_of_length b)
      @ Option.value ~default:[] (Option.map vars_of_length c)
      @ Option.value ~default:[] (Option.map vars_of_length d)
  | Clip_path_circle l -> vars_of_length l
  | Clip_path_ellipse (a, b) -> vars_of_length a @ vars_of_length b
  | Clip_path_polygon points | Clip_path_polygon_spaced points ->
      List.concat_map (fun (a, b) -> vars_of_length a @ vars_of_length b) points
  | Clip_path_xywh { x; y; width; height; rounded }
  | Clip_path_rect
      { top = x; right = y; bottom = width; left = height; rounded } ->
      vars_of_length_percentage x
      @ vars_of_length_percentage y
      @ vars_of_length_percentage width
      @ vars_of_length_percentage height
      @ Option.value ~default:[] (Option.map vars_of_border_radius rounded)
  | _ -> []

let vars_of_mask_box (value : Properties.mask_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_mask_composite (value : Properties.webkit_mask_composite) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_mask_composite (value : Properties.mask_composite) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_mask_source_type (value : Properties.webkit_mask_source_type)
    =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_mask_mode (value : Properties.mask_mode) =
  match value with
  | Var v -> [ V v ]
  | Modes modes -> List.concat_map vars_of_mask_mode modes
  | Alpha | Luminance | Match_source | Initial | Inherit | Unset | Revert
  | Revert_layer ->
      []

let vars_of_user_select (value : Properties.user_select) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_axis (value : Properties.timeline_axis) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_name (value : Properties.timeline_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_shorthand (value : Properties.timeline_shorthand) =
  vars_of_timeline_axis value.timeline_axis

let vars_of_direction (value : Properties.direction) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_css_wide (value : Properties.css_wide) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_behavior (value : Properties.scroll_behavior) =
  match value with Var v -> [ V v ] | _ -> []

(** {1 Advanced variable extraction} *)

(* Extract variables from CSS property values using type-specific extraction
   functions *)
let vars_of_property : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Width, value -> vars_of_length_percentage value
  | Height, value -> vars_of_length_percentage value
  | Min_width, value -> vars_of_length_percentage value
  | Min_height, value -> vars_of_length_percentage value
  | Max_width, value -> vars_of_length_percentage value
  | Max_height, value -> vars_of_length_percentage value
  | Padding, values -> vars_of_length_list values
  | Padding_top, value -> vars_of_length value
  | Padding_right, value -> vars_of_length value
  | Padding_bottom, value -> vars_of_length value
  | Padding_left, value -> vars_of_length value
  | Padding_inline, value -> vars_of_length value
  | Padding_inline_start, value -> vars_of_length value
  | Padding_inline_end, value -> vars_of_length value
  | Padding_block, value -> vars_of_length value
  | Padding_block_start, value -> vars_of_length value
  | Padding_block_end, value -> vars_of_length value
  | Margin, values -> vars_of_length_list values
  | Margin_top, value -> vars_of_length value
  | Margin_right, value -> vars_of_length value
  | Margin_bottom, value -> vars_of_length value
  | Margin_left, value -> vars_of_length value
  | Margin_inline, value -> vars_of_length_list value
  | Margin_inline_start, value -> vars_of_length value
  | Margin_inline_end, value -> vars_of_length value
  | Margin_block, value -> vars_of_length_list value
  | Margin_block_start, value -> vars_of_length value
  | Margin_block_end, value -> vars_of_length value
  | Top, value -> vars_of_length_list value
  | Right, value -> vars_of_length_list value
  | Bottom, value -> vars_of_length_list value
  | Left, value -> vars_of_length_list value
  | Font_size, value -> vars_of_font_size value
  | Letter_spacing, value -> vars_of_length value
  | Line_height, value -> vars_of_line_height value
  | Border_width, value -> vars_of_border_width_list value
  | Border_top_width, value -> vars_of_border_width value
  | Border_right_width, value -> vars_of_border_width value
  | Border_bottom_width, value -> vars_of_border_width value
  | Border_left_width, value -> vars_of_border_width value
  | Border_inline_start_width, value -> vars_of_border_width value
  | Border_inline_end_width, value -> vars_of_border_width value
  | Border_block_start_width, value -> vars_of_border_width value
  | Border_block_end_width, value -> vars_of_border_width value
  | Outline_width, value -> vars_of_length value
  | Column_gap, value -> vars_of_length value
  | Row_gap, value -> vars_of_length value
  | Gap, Lengths { row_gap; column_gap } ->
      vars_of_optional_length row_gap @ vars_of_optional_length column_gap
  | Gap, Var v -> [ V v ]
  | Gap, (Inherit | Initial | Unset | Revert | Revert_layer) -> []
  (* Color properties *)
  | Background_color, value -> vars_of_color value
  | Color, value -> vars_of_color value
  | Border_color, value -> vars_of_color value
  | Border_top_color, value -> vars_of_color value
  | Border_right_color, value -> vars_of_color value
  | Border_bottom_color, value -> vars_of_color value
  | Border_left_color, value -> vars_of_color value
  | Border_inline_start_color, value -> vars_of_color value
  | Border_inline_end_color, value -> vars_of_color value
  | Border_inline_style, value -> vars_of_border_style value
  | Border_block_style, value -> vars_of_border_style value
  | Border_start_start_radius, value -> vars_of_length value
  | Border_start_end_radius, value -> vars_of_length value
  | Border_end_start_radius, value -> vars_of_length value
  | Border_end_end_radius, value -> vars_of_length value
  | Text_decoration_color, value -> vars_of_color value
  | Webkit_text_decoration_color, value -> vars_of_color value
  | Webkit_tap_highlight_color, value -> vars_of_color value
  | Outline_color, value -> vars_of_color value
  (* Border radius *)
  | Border_radius, value -> vars_of_border_radius value
  | Border_top_left_radius, value -> vars_of_length value
  | Border_top_right_radius, value -> vars_of_length value
  | Border_bottom_left_radius, value -> vars_of_length value
  | Border_bottom_right_radius, value -> vars_of_length value
  | Border_image, _ -> []
  (* Outline offset *)
  | Outline_offset, value -> vars_of_length value
  | Flex_basis, value -> vars_of_length value
  (* Text and font properties *)
  | Text_indent, value -> vars_of_length value
  | Text_decoration_thickness, value -> vars_of_length value
  | Word_spacing, value -> vars_of_length value
  (* Other length properties *)
  | Border_spacing, values -> List.concat_map vars_of_length values
  | Perspective, value -> vars_of_length value
  | Stroke_width, value -> vars_of_length value
  | Scroll_margin, value -> List.concat_map vars_of_length value
  | Scroll_margin_top, value -> vars_of_length value
  | Scroll_margin_right, value -> vars_of_length value
  | Scroll_margin_bottom, value -> vars_of_length value
  | Scroll_margin_left, value -> vars_of_length value
  | Scroll_padding, value -> List.concat_map vars_of_length value
  | Scroll_padding_top, value -> vars_of_length value
  | Scroll_padding_right, value -> vars_of_length value
  | Scroll_padding_bottom, value -> vars_of_length value
  | Scroll_padding_left, value -> vars_of_length value
  (* Color properties *)
  | Accent_color, value -> vars_of_color value
  | Caret_color, value -> vars_of_color value
  (* Rotate property *)
  | Rotate, value -> vars_of_rotate_value value
  (* Duration properties *)
  | Transition_duration, value -> vars_of_duration value
  | Transition_delay, value -> vars_of_duration value
  | Animation_duration, value -> vars_of_duration value
  | Animation_delay, value -> vars_of_duration value
  (* Transform properties *)
  | Transform, value -> vars_of_transform_list value
  | Webkit_transform, value -> vars_of_transform_list value
  | Translate, value -> vars_of_translate_value value
  (* Border style properties *)
  | Border_style, value -> vars_of_border_style value
  | Border_top_style, value -> vars_of_border_style value
  | Border_right_style, value -> vars_of_border_style value
  | Border_bottom_style, value -> vars_of_border_style value
  | Border_left_style, value -> vars_of_border_style value
  (* Font properties *)
  | Font_weight, value -> vars_of_font_weight value
  | Font_family, value -> vars_of_font_family value
  | Font_feature_settings, value -> vars_of_font_feature_settings value
  | Font_size_adjust, value -> vars_of_font_size_adjust value
  | Font_stretch, value -> vars_of_font_stretch value
  | Font_variant_emoji, value -> vars_of_font_variant_emoji value
  | Font_variation_settings, value -> vars_of_font_variation_settings value
  | Font_variant_numeric, value -> vars_of_font_variant_numeric value
  (* Text properties *)
  | Text_decoration, value -> vars_of_text_decoration value
  | Webkit_text_decoration, value -> vars_of_text_decoration value
  | Text_transform, value -> vars_of_text_transform value
  (* Content and visibility *)
  | Content, value -> vars_of_content value
  | Content_visibility, value -> vars_of_content_visibility value
  (* Blend mode properties *)
  | Mix_blend_mode, value -> vars_of_blend_mode value
  | Background_blend_mode, values -> List.concat_map vars_of_blend_mode values
  (* Filter properties *)
  | Filter, value -> vars_of_filter value
  | Backdrop_filter, value -> vars_of_filter value
  | Webkit_backdrop_filter, value -> vars_of_filter value
  | Webkit_filter, value -> vars_of_filter value
  | Ms_filter, value -> vars_of_filter value
  (* Transition properties *)
  | Transition, values -> List.concat_map vars_of_transition values
  | Webkit_transition, values -> List.concat_map vars_of_transition values
  | O_transition, values -> List.concat_map vars_of_transition values
  (* Animation properties *)
  | Animation, values -> List.concat_map vars_of_animation values
  (* Background properties *)
  | Background, values -> List.concat_map vars_of_background values
  (* Shadow properties *)
  | Box_shadow, value -> vars_of_shadow value
  (* Scale properties *)
  | Scale, value -> vars_of_scale value
  (* Scroll snap properties *)
  | Scroll_snap_type, value -> vars_of_scroll_snap_type value
  (* Timing function properties *)
  | Animation_timing_function, value -> vars_of_timing_function value
  | Transition_timing_function, value -> vars_of_timing_function value
  (* Quotes property *)
  | Quotes, value -> vars_of_quotes value
  (* Aspect ratio *)
  | Aspect_ratio, value -> vars_of_aspect_ratio value
  (* Background image/size *)
  | Background_image, values -> List.concat_map vars_of_background_image values
  | Background_size, value -> vars_of_background_size value
  (* Columns *)
  | Columns, value -> vars_of_columns_value value
  (* Contain *)
  | Contain, value -> vars_of_contain value
  (* Cursor *)
  | Cursor, value -> vars_of_cursor value
  (* Grid template *)
  | Grid_auto_columns, value -> vars_of_grid_template value
  | Grid_auto_rows, value -> vars_of_grid_template value
  | Grid_template, value -> vars_of_grid_template value
  | Grid_template_columns, value -> vars_of_grid_template value
  | Grid_template_rows, value -> vars_of_grid_template value
  (* Grid line *)
  | Grid_column_end, value -> vars_of_grid_line value
  | Grid_column_start, value -> vars_of_grid_line value
  | Grid_row_end, value -> vars_of_grid_line value
  | Grid_row_start, value -> vars_of_grid_line value
  (* List style *)
  | List_style_image, value -> vars_of_list_style_image value
  | List_style_type, value -> vars_of_list_style_type value
  (* Mask image/size *)
  | Mask_image, value -> vars_of_background_image value
  | Mask_size, value -> vars_of_background_size value
  | Webkit_mask_image, value -> vars_of_background_image value
  | Webkit_mask_size, value -> vars_of_background_size value
  (* Object position *)
  | Object_position, value -> vars_of_position_value value
  (* Outline style *)
  | Outline_style, value -> vars_of_outline_style value
  (* Text shadow *)
  | Text_shadow, value -> vars_of_text_shadow_list value
  (* Transform origin *)
  | Transform_origin, value -> vars_of_transform_origin value
  (* Vertical align *)
  | Vertical_align, value -> vars_of_vertical_align value
  (* Will change *)
  | Will_change, value -> vars_of_will_change value
  (* Opacity (typed math) *)
  | Opacity, value -> vars_of_opacity value
  | Tab_size, value -> vars_of_tab_size value
  | Align_content, value -> vars_of_align_content value
  | Align_items, value -> vars_of_align_items value
  | Align_self, value -> vars_of_align_self value
  | Animation_direction, value -> vars_of_animation_direction value
  | Animation_fill_mode, value -> vars_of_animation_fill_mode value
  | Animation_iteration_count, value -> vars_of_animation_iteration_count value
  | Appearance, value -> vars_of_appearance value
  | Backface_visibility, value -> vars_of_backface_visibility value
  | Background_attachment, value -> vars_of_background_attachment value
  | Background_clip, value -> vars_of_background_box value
  | Background_origin, value -> vars_of_background_box value
  | Background_repeat, value -> vars_of_background_repeat value
  | Border, value -> vars_of_border value
  | Border_collapse, value -> vars_of_border_collapse value
  | Box_sizing, value -> vars_of_box_sizing value
  | Box_decoration_break, value -> vars_of_box_decoration_break value
  | Break_after, value -> vars_of_break_value value
  | Break_before, value -> vars_of_break_value value
  | Break_inside, value -> vars_of_break_inside_value value
  | Clear, value -> vars_of_clear value
  | Clip, value -> vars_of_clip value
  | Clip_path, value -> vars_of_clip_path value
  | Color_scheme, value -> vars_of_color_scheme value
  | Container_type, value -> vars_of_container_type value
  | Container, value -> vars_of_container_shorthand value
  | Container_name, value -> vars_of_container_name value
  | Anchor_name, value -> vars_of_anchor_name value
  | Position_anchor, value -> vars_of_position_anchor value
  | Position_try_fallbacks, value -> vars_of_position_try_fallbacks value
  | Overflow_anchor, value -> vars_of_overflow_anchor value
  | Scrollbar_width, value -> vars_of_scrollbar_width value
  | Scrollbar_color, value -> vars_of_scrollbar_color value
  | Scrollbar_gutter, value -> vars_of_scrollbar_gutter value
  | Font_palette, value -> vars_of_font_palette value
  | Font_synthesis, value -> vars_of_font_synthesis value
  | Animation_timeline, value -> vars_of_animation_timeline value
  | Animation_range, value -> vars_of_animation_range value
  | View_transition_name, value -> vars_of_view_transition_name value
  | Image_orientation, value -> vars_of_image_orientation value
  | Contain_intrinsic_size, value -> vars_of_contain_intrinsic_size value
  | Margin_trim, value -> vars_of_margin_trim value
  | Offset_path, value -> vars_of_offset_path value
  | All, value -> vars_of_css_wide value
  | Direction, value -> vars_of_direction value
  | Display, value -> vars_of_display value
  | Fill, value -> vars_of_svg_paint value
  | Flex, value -> vars_of_flex value
  | Flex_direction, value -> vars_of_flex_direction value
  | Flex_wrap, value -> vars_of_flex_wrap value
  | Flex_grow, value -> vars_of_flex_factor value
  | Flex_shrink, value -> vars_of_flex_factor value
  | Float, value -> vars_of_float_side value
  | Font_style, value -> vars_of_font_style value
  | Forced_color_adjust, value -> vars_of_forced_color_adjust value
  | Grid_auto_flow, value -> vars_of_grid_auto_flow value
  | Justify_content, value -> vars_of_justify_content value
  | Justify_items, value -> vars_of_justify_items value
  | Justify_self, value -> vars_of_justify_self value
  | List_style_position, value -> vars_of_list_style_position value
  | Mask_clip, value -> vars_of_mask_box value
  | Mask_composite, value -> vars_of_mask_composite value
  | Mask_mode, value -> vars_of_mask_mode value
  | Mask_origin, value -> vars_of_mask_box value
  | Mask_repeat, value -> vars_of_background_repeat value
  | Moz_appearance, value -> vars_of_appearance value
  | Moz_orient, value -> vars_of_moz_orient value
  | Moz_osx_font_smoothing, value -> vars_of_moz_osx_font_smoothing value
  | Object_fit, value -> vars_of_object_fit value
  | Outline, value -> vars_of_outline value
  | Overflow, value -> vars_of_overflow value
  | Overflow_wrap, value -> vars_of_overflow_wrap value
  | Overflow_x, value -> vars_of_overflow value
  | Overflow_y, value -> vars_of_overflow value
  | Overflow_block, value -> vars_of_overflow value
  | Overflow_inline, value -> vars_of_overflow value
  | Overscroll_behavior, values ->
      List.concat_map vars_of_overscroll_behavior values
  | Overscroll_behavior_x, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_y, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_block, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_inline, value -> vars_of_overscroll_behavior value
  | Page_size, value -> vars_of_page_size value
  | Perspective_origin, value -> vars_of_perspective_origin value
  | Place_content, value -> vars_of_place_content value
  | Place_items, value -> vars_of_place_items value
  | Pointer_events, value -> vars_of_pointer_events value
  | Position, value -> vars_of_position value
  | Print_color_adjust, value -> vars_of_print_color_adjust value
  | Resize, value -> vars_of_resize value
  | Scroll_snap_align, value -> vars_of_scroll_snap_align value
  | Scroll_snap_stop, value -> vars_of_scroll_snap_stop value
  | Scroll_behavior, value -> vars_of_scroll_behavior value
  | Scroll_timeline, value -> vars_of_timeline_shorthand value
  | Stroke, value -> vars_of_svg_paint value
  | Source, _ -> []
  | Text_align, value -> vars_of_text_align value
  | Text_decoration_line, values ->
      List.concat_map vars_of_text_decoration_line values
  | Text_decoration_skip_ink, value -> vars_of_text_decoration_skip_ink value
  | Text_decoration_style, value -> vars_of_text_decoration_style value
  | Text_overflow, value -> vars_of_text_overflow value
  | Text_size_adjust, value -> vars_of_text_size_adjust value
  | Text_wrap, value -> vars_of_text_wrap value
  | Text_wrap_style, value -> vars_of_text_wrap_style value
  | Text_box_trim, value -> vars_of_text_box_trim value
  | Text_spacing_trim, value -> vars_of_text_spacing_trim value
  | Hyphenate_limit_chars, value -> vars_of_hyphenate_limit_chars value
  | Initial_letter, value -> vars_of_initial_letter value
  | Touch_action, value -> vars_of_touch_action value
  | Transform_box, value -> vars_of_transform_box value
  | Transform_style, value -> vars_of_transform_style value
  | Transition_behavior, value -> vars_of_transition_behavior value
  | Unicode_bidi, value -> vars_of_unicode_bidi value
  | User_select, value -> vars_of_user_select value
  | Visibility, value -> vars_of_visibility value
  | View_timeline_name, value -> vars_of_timeline_name value
  | View_timeline, value -> vars_of_timeline_shorthand value
  | Timeline_scope, value -> vars_of_timeline_name value
  | Webkit_appearance, value -> vars_of_webkit_appearance value
  | Webkit_background_clip, value -> vars_of_background_box value
  | Webkit_box_decoration_break, value -> vars_of_box_decoration_break value
  | Webkit_box_orient, value -> vars_of_webkit_box_orient value
  | Webkit_font_smoothing, value -> vars_of_webkit_font_smoothing value
  | Webkit_mask_clip, value -> vars_of_mask_box value
  | Webkit_mask_composite, value -> vars_of_webkit_mask_composite value
  | Webkit_mask_origin, value -> vars_of_mask_box value
  | Webkit_mask_repeat, value -> vars_of_background_repeat value
  | Webkit_mask_source_type, value -> vars_of_webkit_mask_source_type value
  | Webkit_text_size_adjust, value -> vars_of_text_size_adjust value
  | Webkit_user_select, value -> vars_of_user_select value
  | White_space, value -> vars_of_white_space value
  | Word_break, value -> vars_of_word_break value
  | Writing_mode, value -> vars_of_writing_mode value
  (* Default case for all other properties *)
  | _ -> []

let rec extract_vars_from_declaration : declaration -> any_var list = function
  | Declaration { property = Custom_property _; _ } -> []
  | Declaration { property; value; _ } -> vars_of_property property value
  | Theme_guarded { decl; _ } -> extract_vars_from_declaration decl

(* Stable dedup: preserves first occurrence of each var, removes later
   duplicates *)
let stable_dedup_vars vars =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (V v) ->
      if Hashtbl.mem seen v.name then false
      else (
        Hashtbl.add seen v.name ();
        true))
    vars

let vars_of_declarations properties =
  List.concat_map extract_vars_from_declaration properties |> stable_dedup_vars

(* Extract only custom property declarations (variable definitions) *)
let custom_declarations ?layer (decls : declaration list) : declaration list =
  List.filter
    (function
      | Declaration
          {
            property = Custom_property _;
            value = Custom_value { layer = decl_layer; _ };
            _;
          } -> (
          match layer with None -> true | Some l -> decl_layer = Some l)
      | _ -> false)
    decls

(* Extract the variable name from a custom declaration *)
let rec custom_declaration_name (decl : declaration) : string option =
  match decl with
  | Declaration { property = Custom_property name; _ } -> Some name
  | Theme_guarded { decl; _ } -> custom_declaration_name decl
  | _ -> None

(* Pretty-printer for any_syntax *)
let pp_any_syntax : any_syntax Pp.t = fun ctx (Syntax syn) -> pp_syntax ctx syn

(* Reader for any_syntax *)
let read_any_syntax (r : Cursor.t) : any_syntax =
  (* Reuse the main read_syntax function *)
  read_syntax r

(* CSS Custom Properties §3 requires [var()] fallbacks to round-trip including
   author-written comments. The token stream silently drops comments, so slice
   the original source between the first and last fallback component instead of
   re-serialising tokens. Falls back to component-based serialisation when the
   source is not retained on the cursor. *)
let fallback_to_string inner =
  let cvs = Cursor.remaining inner in
  match (cvs, Cursor.source inner) with
  | [], _ -> ""
  | _, None -> Cursor.remaining_to_string ~trim:true inner
  | first :: _, Some src ->
      let start_pos = (Component.source_loc first).start_pos in
      let last = List.nth cvs (List.length cvs - 1) in
      let end_pos = (Component.source_loc last).end_pos in
      let len = max 0 (end_pos - start_pos) in
      let slice =
        String.sub src start_pos (min len (String.length src - start_pos))
      in
      String.trim slice

(** Parse a CSS variable reference with optional fallback value. This creates a
    variable handle for parsing purposes only - it doesn't have type or layer
    information which would need to be resolved from a variable registry or
    context. *)
let parse_var_reference (r : Cursor.t) : string * string option =
  (* CSS Syntax 3 §4.3.6: EOF inside a function is a parse error. We tolerate it
     only when the fallback list was opened with a comma — the trailing
     [<string-token>] from §4.3.5 may have eaten the function's closing [)] — so
     the declaration still carries a recoverable name + fallback pair. Without a
     fallback there is no recovery signal and the malformed var() is
     rejected. *)
  let terminated =
    match Cursor.peek r with
    | Some (Component.Func fn) -> fn.node.terminated
    | _ -> true
  in
  let result =
    Cursor.call "var" r (fun inner ->
        let raw_name = Cursor.ident ~keep_case:true inner in
        (* css-variables-1: a <custom-property-name> is a <dashed-ident> other
           than [--]. The [--] prefix must be followed by an ident-continue code
           point that is not itself [-], otherwise the trailing dashes are
           ambiguous with the reserved [--] keyword. *)
        if
          not
            (String.length raw_name >= 3
            && raw_name.[0] = '-'
            && raw_name.[1] = '-'
            && raw_name.[2] <> '-')
        then Cursor.err_invalid inner ("not a custom property: " ^ raw_name);
        let name = String.sub raw_name 2 (String.length raw_name - 2) in
        Cursor.ws inner;
        let fallback =
          if Cursor.comma_opt inner then Some (fallback_to_string inner)
          else None
        in
        (name, fallback))
  in
  (match (terminated, snd result) with
  | false, None -> Cursor.err_invalid r "unterminated var()"
  | _ -> ());
  Cursor.ws r;
  if not (Cursor.is_done r) then
    Cursor.err_invalid r "trailing tokens after var()";
  result
