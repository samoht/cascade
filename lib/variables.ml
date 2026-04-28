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
    Declaration.custom_declaration ?layer ?meta
      (String.concat "" [ "--"; name ])
      kind value
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
  | None -> []

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
  | None -> []

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
  | Inherit -> []

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
  | Both (_, l) -> vars_of_length l
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

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name

(** {1 Variable name utilities} *)

let any_var_name (V v) = String.concat "" [ "--"; v.name ]

(** Extract variables from timing function *)
let vars_of_timing_function = function
  | Ease | Linear | Ease_in | Ease_out | Ease_in_out | Step_start | Step_end
  | Steps _ | Cubic_bezier _ ->
      []
  | Var v -> [ V v ]

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
  | Margin_inline, value -> vars_of_length value
  | Margin_inline_start, value -> vars_of_length value
  | Margin_inline_end, value -> vars_of_length value
  | Margin_block, value -> vars_of_length value
  | Margin_block_start, value -> vars_of_length value
  | Margin_block_end, value -> vars_of_length value
  | Top, value -> vars_of_length_list value
  | Right, value -> vars_of_length_list value
  | Bottom, value -> vars_of_length_list value
  | Left, value -> vars_of_length_list value
  | Font_size, value -> vars_of_font_size value
  | Letter_spacing, value -> vars_of_length value
  | Line_height, value -> vars_of_line_height value
  | Border_width, value -> vars_of_border_width value
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
  | Gap, { row_gap; column_gap } ->
      vars_of_optional_length row_gap @ vars_of_optional_length column_gap
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
  | Border_radius, { horizontal; vertical } ->
      let from_list = List.concat_map vars_of_length_percentage in
      from_list horizontal
      @ Option.value ~default:[] (Option.map from_list vertical)
  | Border_top_left_radius, value -> vars_of_length value
  | Border_top_right_radius, value -> vars_of_length value
  | Border_bottom_left_radius, value -> vars_of_length value
  | Border_bottom_right_radius, value -> vars_of_length value
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
  | Scroll_margin, value -> vars_of_length value
  | Scroll_margin_top, value -> vars_of_length value
  | Scroll_margin_right, value -> vars_of_length value
  | Scroll_margin_bottom, value -> vars_of_length value
  | Scroll_margin_left, value -> vars_of_length value
  | Scroll_padding, value -> vars_of_length value
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
  (* Default case for all other properties *)
  | _ -> []

let rec extract_vars_from_declaration : declaration -> any_var list = function
  | Custom_declaration _ -> [] (* Custom properties don't have typed vars *)
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
      | Custom_declaration { layer = decl_layer; _ } -> (
          match layer with None -> true | Some l -> decl_layer = Some l)
      | _ -> false)
    decls

(* Extract the variable name from a custom declaration *)
let rec custom_declaration_name (decl : declaration) : string option =
  match decl with
  | Custom_declaration { name; _ } -> Some name
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
  let result =
    Cursor.call "var" r (fun inner ->
        let raw_name = Cursor.ident ~keep_case:true inner in
        (* Per css-variables-1, custom-property names start with [--]; anything
           else is rejected. *)
        if
          not
            (String.length raw_name >= 3
            && raw_name.[0] = '-'
            && raw_name.[1] = '-')
        then Cursor.err_invalid inner ("not a custom property: " ^ raw_name);
        let name = String.sub raw_name 2 (String.length raw_name - 2) in
        Cursor.ws inner;
        let fallback =
          if Cursor.comma_opt inner then Some (fallback_to_string inner)
          else None
        in
        (name, fallback))
  in
  Cursor.ws r;
  if not (Cursor.is_done r) then
    Cursor.err_invalid r "trailing tokens after var()";
  result
