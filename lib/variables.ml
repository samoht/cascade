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
  | Custom_ident -> Pp.string ctx "<custom-ident>"
  | String -> Pp.string ctx "<string>"
  | Url -> Pp.string ctx "<url>"
  | Image -> Pp.string ctx "<image>"
  | Transform_function -> Pp.string ctx "<transform-function>"
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
  | Question syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "?"
  | Brackets s ->
      Pp.string ctx "[";
      Pp.string ctx s;
      Pp.string ctx "]"

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
  | Custom_ident -> Pp.string ctx value
  | String -> Pp.quoted ctx value
  | Url ->
      Pp.string ctx "url(";
      Pp.quoted ctx value;
      Pp.string ctx ")"
  | Image -> Pp.string ctx value
  | Transform_function -> Pp.string ctx value
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
  | Question syn -> (
      match value with None -> () | Some v -> pp_value syn ctx v)
  | Brackets _ -> Pp.string ctx value

(** Read a CSS syntax descriptor from input *)
let read_syntax (r : Reader.t) : any_syntax =
  (* CSS @property syntax values must be quoted strings per spec *)
  let s = Reader.string r in
  match s with
  | "<length>" -> Syntax Length
  | "<color>" -> Syntax Color
  | "<number>" -> Syntax Number
  | "<integer>" -> Syntax Integer
  | "<percentage>" -> Syntax Percentage
  | "<length-percentage>" -> Syntax Length_percentage
  | "<angle>" -> Syntax Angle
  | "<time>" -> Syntax Time
  | "<custom-ident>" -> Syntax Custom_ident
  | "<string>" -> Syntax String
  | "<url>" -> Syntax Url
  | "<image>" -> Syntax Image
  | "<transform-function>" -> Syntax Transform_function
  | "*" -> Syntax Universal
  | s when String.length s > 2 && s.[0] = '[' && s.[String.length s - 1] = ']'
    ->
      Syntax (Brackets (String.sub s 1 (String.length s - 2)))
  | s when String.contains s '|' -> (
      (* Handle composite syntax like "<length> | <percentage>" *)
      match List.map String.trim (String.split_on_char '|' s) with
      | [ "<length>"; "<percentage>" ] | [ "<percentage>"; "<length>" ] ->
          Syntax (Or (Length, Percentage))
      | _ -> Reader.err_invalid r ("Unsupported CSS composite syntax: " ^ s))
  | s -> Reader.err_invalid r ("Unsupported CSS syntax: " ^ s)

(** Read a value according to its syntax type *)
let rec read_value : type a. Reader.t -> a syntax -> a =
 fun reader syntax ->
  match syntax with
  | Universal ->
      (* For universal syntax "*", accept any CSS value - not just quoted
         strings *)
      Reader.ws reader;
      Reader.token reader
  | String -> Reader.string ~trim:true reader
  | Custom_ident -> Reader.string ~trim:true reader
  | Url -> Reader.string ~trim:true reader
  | Image -> Reader.string ~trim:true reader
  | Transform_function -> Reader.string ~trim:true reader
  | Brackets _desc -> Reader.string ~trim:true reader
  | Length -> Values.read_length reader
  | Color -> Values.read_color reader
  | Number -> Reader.number reader
  | Integer -> int_of_float (Reader.number reader)
  | Percentage -> Values.read_percentage reader
  | Length_percentage -> Values.read_length_percentage reader
  | Angle -> Values.read_angle reader
  | Time -> Values.read_duration reader
  | Or (syn1, _syn2) ->
      (* For now, only try the first syntax - proper backtracking would require
         a seekable reader *)
      Either.Left (read_value reader syn1)
  | Plus syn ->
      (* Read space-separated list - use Reader.many for proper error
         handling *)
      let values, _error_opt = Reader.many (fun r -> read_value r syn) reader in
      if values = [] then
        Reader.err_invalid reader "expected at least one value for '+' syntax"
      else values
  | Hash syn ->
      (* Read comma-separated list - use Reader.list for proper parsing *)
      let values =
        Reader.list ~sep:Reader.comma ~at_least:1
          (fun r -> read_value r syn)
          reader
      in
      values
  | Question syn ->
      (* Optional value - use Reader.option for safe parsing *)
      Reader.option (fun r -> read_value r syn) reader

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
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right
  | Nested inner -> vars_of_calc inner

let vars_of_length (value : Values.length) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_length_list (values : Values.length list) : any_var list =
  List.concat_map vars_of_length values

let vars_of_length_percentage (value : Values.length_percentage) : any_var list
    =
  match value with
  | Length l -> vars_of_length l
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | _ -> []

let vars_of_font_size (value : Properties.font_size) : any_var list =
  match value with
  | Length l -> vars_of_length l
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | _ -> []

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
  match value with Var v -> [ V v ] | _ -> []

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
  | Top, value -> vars_of_length value
  | Right, value -> vars_of_length value
  | Bottom, value -> vars_of_length value
  | Left, value -> vars_of_length value
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
  | Border_start_start_radius, value -> vars_of_length value
  | Border_start_end_radius, value -> vars_of_length value
  | Border_end_start_radius, value -> vars_of_length value
  | Border_end_end_radius, value -> vars_of_length value
  | Text_decoration_color, value -> vars_of_color value
  | Webkit_text_decoration_color, value -> vars_of_color value
  | Webkit_tap_highlight_color, value -> vars_of_color value
  | Outline_color, value -> vars_of_color value
  (* Border radius *)
  | Border_radius, value -> vars_of_length value
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
  | Border_spacing, value -> vars_of_length value
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
  (* Default case for all other properties *)
  | _ -> []

let extract_vars_from_declaration : declaration -> any_var list = function
  | Custom_declaration _ -> [] (* Custom properties don't have typed vars *)
  | Declaration { property; value; _ } -> vars_of_property property value

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
let custom_declaration_name (decl : declaration) : string option =
  match decl with Custom_declaration { name; _ } -> Some name | _ -> None

(* Pretty-printer for any_syntax *)
let pp_any_syntax : any_syntax Pp.t = fun ctx (Syntax syn) -> pp_syntax ctx syn

(* Reader for any_syntax *)
let read_any_syntax (r : Reader.t) : any_syntax =
  (* Reuse the main read_syntax function *)
  read_syntax r

(** Parse a CSS variable reference with optional fallback value. This creates a
    variable handle for parsing purposes only - it doesn't have type or layer
    information which would need to be resolved from a variable registry or
    context. *)
let parse_var_reference (r : Reader.t) : string * string option =
  if Reader.looking_at r "var(" then (
    for _ = 1 to 4 do
      Reader.skip r
    done;
    (* skip "var(" *)
    Reader.expect '-' r;
    Reader.expect '-' r;
    let content = Reader.until r ')' in
    let name, fallback =
      match String.index_opt content ',' with
      | Some i ->
          let name = String.sub content 0 i |> String.trim in
          let fallback =
            String.sub content (i + 1) (String.length content - i - 1)
            |> String.trim
          in
          (name, Some fallback)
      | None -> (String.trim content, None)
    in
    (* CSS spec requires variable names to have at least one character after
       -- *)
    if name = "" then Reader.err_invalid r "CSS variable name";
    Reader.expect ')' r;
    (name, fallback))
  else Reader.err_invalid r "Expected var() function"
