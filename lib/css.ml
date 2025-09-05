(** CSS generation utilities *)

(* Re-expose the CSS printer module as Css.Pp *)
module Pp = Pp

(* Error handling helpers *)
let err_invalid_identifier name reason =
  invalid_arg (String.concat "" [ "CSS identifier '"; name; "' "; reason ])

type meta = ..

type 'a var = {
  name : string;
  fallback : 'a option;
  default : 'a option;
  layer : string option;
  meta : meta option;
}

let var_name v = v.name
let var_layer v = v.layer

type any_var = V : 'a var -> any_var
type mode = Variables | Inline
type config = { minify : bool; mode : mode; optimize : bool }

let var_ref ?fallback ?default ?layer ?meta name =
  { name; fallback; default; layer; meta }

let meta (type t) () =
  let module M = struct
    type meta += V : t -> meta
  end in
  let inj x = M.V x in
  let proj = function M.V v -> Some v | _ -> None in
  (inj, proj)

(** CSS length values *)
type calc_op = Add | Sub | Mult | Div

type 'a calc =
  | Var of 'a var (* CSS variable *)
  | Val of 'a
  | Expr of 'a calc * calc_op * 'a calc

type length =
  | Px of int
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Ch of float (* Character units *)
  | Lh of float (* Line height units *)
  | Num of float (* Unitless numbers, e.g., line-height multipliers *)
  | Auto
  | Zero
  | Inherit
  | Fit_content (* fit-content keyword *)
  | Max_content (* max-content keyword *)
  | Min_content (* min-content keyword *)
  | From_font (* from-font keyword for text-decoration-thickness *)
  | Var of length var (* CSS variable reference *)
  | Calc of length calc (* Calculated expressions *)

type color_space =
  | Srgb
  | Srgb_linear
  | Display_p3
  | A98_rgb
  | Prophoto_rgb
  | Rec2020
  | Lab
  | Oklab
  | Xyz
  | Xyz_d50
  | Xyz_d65
  | Lch
  | Oklch
  | Hsl
  | Hwb

(** CSS named colors *)
type color_name =
  | Red
  | Blue
  | Green
  | White
  | Black
  | Yellow
  | Cyan
  | Magenta
  | Gray
  | Grey
  | Orange
  | Purple
  | Pink
  | Silver
  | Maroon
  | Fuchsia
  | Lime
  | Olive
  | Navy
  | Teal
  | Aqua

(** CSS color values *)
type color =
  | Hex of { hash : bool; value : string } (* hash indicates if # was present *)
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : float }
  | Oklch of { l : float; c : float; h : float }
  | Named of color_name
  | Var of color var (* Use typed var instead of string *)
  | Current
  | Transparent
  | Inherit
  | Mix of {
      in_space : color_space;  (** Color space for mixing *)
      color1 : color;  (** First color *)
      percent1 : int option;  (** Optional percentage for first color *)
      color2 : color;  (** Second color *)
      percent2 : int option;  (** Optional percentage for second color *)
    }

let hex s =
  let len = String.length s in
  if len > 0 && s.[0] = '#' then
    Hex { hash = true; value = String.sub s 1 (len - 1) }
  else Hex { hash = false; value = s }

let rgb r g b = Rgb { r; g; b }
let rgba r g b a = Rgba { r; g; b; a }
let oklch l c h = Oklch { l; c; h }
let named n = Named n
let current_color = Current
let transparent = Transparent

let color_mix ?(in_space = Srgb) ?percent1 ?percent2 color1 color2 =
  Mix { in_space; color1; percent1; color2; percent2 }

(** CSS display values *)
type display =
  | Block
  | Inline
  | Inline_block
  | Flex
  | Inline_flex
  | Grid
  | Inline_grid
  | None
  | Flow_root
  | Table
  | Table_row
  | Table_cell
  | Table_caption
  | Table_column
  | Table_column_group
  | Table_footer_group
  | Table_header_group
  | Table_row_group
  | Inline_table
  | List_item
  | Contents
  | Webkit_box

(** CSS position values *)
type position = Static | Relative | Absolute | Fixed | Sticky

(** CSS visibility values *)
type visibility = Visible | Hidden | Collapse

(** CSS z-index values *)
type z_index = Auto | Index of int

(** CSS font weight values *)
type font_weight =
  | Weight of int
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Inherit
  | Var of font_weight var (* CSS variable reference *)

(** CSS text align values *)
type text_align = Left | Right | Center | Justify | Start | End | Inherit

(** CSS overflow values *)
type overflow = Visible | Hidden | Scroll | Auto | Clip

(** CSS flex direction values *)
type flex_direction = Row | Row_reverse | Column | Column_reverse

(** CSS align/justify values *)
type align =
  | Flex_start
  | Flex_end
  | Center
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Start
  | End
  | Baseline
  | Auto

(** CSS text decoration values *)
type text_decoration =
  | None
  | Underline
  | Overline
  | Line_through
  | Inherit
  | Underline_dotted (* underline dotted *)

(** CSS font style values *)
type font_style = Normal | Italic | Oblique | Inherit

(** CSS list style type values *)
type list_style_type =
  | None
  | Disc
  | Circle
  | Square
  | Decimal
  | Lower_alpha
  | Upper_alpha
  | Lower_roman
  | Upper_roman

(** CSS list-style-position values *)
type list_style_position = Inside | Outside | Inherit

(** CSS list-style-image values *)
type list_style_image = None_img | Url of string | Inherit

(** CSS border style values *)
type border_style =
  | None
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Hidden
  | Var of border_style var (* CSS variable reference *)

(** CSS outline style values *)
type outline_style =
  | None
  | Auto
  | Dotted
  | Dashed
  | Solid
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Inherit

type forced_color_adjust = Auto | None | Inherit

(** CSS cursor values *)
type cursor =
  | Auto
  | Default
  | Pointer
  | Wait
  | Text
  | Move
  | Help
  | Not_allowed
  | None
  | Context_menu
  | Progress
  | Cell
  | Crosshair
  | Vertical_text
  | Alias
  | Copy
  | No_drop
  | Grab
  | Grabbing
  | All_scroll
  | Col_resize
  | Row_resize
  | N_resize
  | E_resize
  | S_resize
  | W_resize
  | Ne_resize
  | Nw_resize
  | Se_resize
  | Sw_resize
  | Ew_resize
  | Ns_resize
  | Nesw_resize
  | Nwse_resize
  | Zoom_in
  | Zoom_out

(** CSS user-select values *)
type user_select = None | Auto | Text | All | Contain

(** CSS flex wrap values *)
type flex_wrap = Nowrap | Wrap | Wrap_reverse

type align_items =
  | Normal
  | Stretch
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Baseline
  | First_baseline
  | Last_baseline
  | Safe_center
  | Unsafe_center
  | Inherit_align
  | Initial
  | Unset
  | Revert
  | Revert_layer

type place_items =
  | Normal
  | Auto
  | Stretch
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Baseline
  | First_baseline
  | Last_baseline
  | Inherit

type justify_content =
  | Normal
  | Flex_start
  | Flex_end
  | Center
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Start
  | End
  | Left
  | Right

type align_self = Auto | Flex_start | Flex_end | Center | Baseline | Stretch
type border_collapse = Collapse | Separate | Inherit
type grid_auto_flow = Row | Column | Row_dense | Column_dense
type isolation = Auto | Isolate | Inherit
type clear = None | Left | Right | Both
type transform_style = Flat | Preserve_3d | Inherit
type backface_visibility = Visible | Hidden | Inherit
type resize = None | Both | Horizontal | Vertical | Block | Inline | Inherit
type object_fit = Fill | Contain | Cover | None | Scale_down | Inherit
type box_sizing = Border_box | Content_box | Inherit
type scroll_behavior = Auto | Smooth | Inherit
type scroll_snap_stop = Normal | Always | Inherit

type scroll_snap_strictness =
  | Mandatory
  | Proximity
  | Var of scroll_snap_strictness var

type scroll_snap_align = None | Start | End | Center
type scroll_snap_axis = X | Y | Block | Inline | Both
type webkit_box_orient = Horizontal | Vertical | Inherit

type scroll_snap_type =
  | None
  | Axis of scroll_snap_axis * scroll_snap_strictness option
  | Inherit

type touch_action = Auto | None | Pan_x | Pan_y | Manipulation | Inherit

(** CSS overscroll-behavior values *)
type overscroll_behavior = Auto | Contain | None | Inherit

(** SVG paint values *)
type svg_paint = None | Current_color | Color of color

(** CSS text transform values *)
type text_transform =
  | None
  | Uppercase
  | Lowercase
  | Capitalize
  | Full_width
  | Full_size_kana
  | Inherit

(** CSS white space values *)
type white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Inherit

(** CSS table layout values *)
type table_layout = Auto | Fixed | Inherit

(** CSS resize values *)

(** CSS appearance values *)
type appearance = None | Auto | Button | Textfield | Menulist | Inherit

type vertical_align =
  | Baseline
  | Top
  | Middle
  | Bottom
  | Text_top
  | Text_bottom
  | Sub
  | Super
  | Length of length
  | Percentage of float
  | Inherit

type text_overflow = Clip | Ellipsis | String of string | Inherit
type text_wrap = Wrap | No_wrap | Balance | Pretty | Inherit
type word_break = Normal | Break_all | Keep_all | Break_word | Inherit
type overflow_wrap = Normal | Break_word | Anywhere | Inherit
type hyphens = None | Manual | Auto | Inherit

type content =
  | String of string
  | None
  | Normal
  | Open_quote
  | Close_quote
  | Var of content var

type font_stretch =
  | Pct of float (* CSS spec: accepts percentage values from 50% to 200% *)
  | Ultra_condensed (* 50% *)
  | Extra_condensed (* 62.5% *)
  | Condensed (* 75% *)
  | Semi_condensed (* 87.5% *)
  | Normal (* 100% *)
  | Semi_expanded (* 112.5% *)
  | Expanded (* 125% *)
  | Extra_expanded (* 150% *)
  | Ultra_expanded (* 200% *)
  | Inherit

type font_variant_numeric_token =
  | Normal
  | Lining_nums
  | Oldstyle_nums
  | Proportional_nums
  | Tabular_nums
  | Diagonal_fractions
  | Stacked_fractions
  | Ordinal
  | Slashed_zero
  | Var of font_variant_numeric_token var

type font_variant_numeric =
  | Normal
  | Tokens of font_variant_numeric_token list
  | Var of font_variant_numeric var
  | Composed of {
      ordinal : font_variant_numeric_token option;
      slashed_zero : font_variant_numeric_token option;
      numeric_figure : font_variant_numeric_token option;
      numeric_spacing : font_variant_numeric_token option;
      numeric_fraction : font_variant_numeric_token option;
    }

type pointer_events =
  | Auto
  | None
  | Visible_painted
  | Visible_fill
  | Visible_stroke
  | Visible
  | Painted
  | Fill
  | Stroke
  | All
  | Inherit

(** CSS text-decoration-style values *)
type text_decoration_style = Solid | Double | Dotted | Dashed | Wavy | Inherit

(** CSS text-decoration-skip-ink values *)
type text_decoration_skip_ink = Auto | None | All | Inherit

(** CSS direction values *)
type direction = Ltr | Rtl | Inherit

(** CSS unicode-bidi values *)
type unicode_bidi =
  | Normal
  | Embed
  | Bidi_override
  | Isolate
  | Isolate_override
  | Plaintext
  | Inherit

(** CSS writing-mode values *)
type writing_mode = Horizontal_tb | Vertical_rl | Vertical_lr | Inherit

(** CSS webkit-font-smoothing values *)
type webkit_font_smoothing =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit

(** CSS webkit-appearance values *)
type webkit_appearance =
  | None  (** No appearance styling *)
  | Auto  (** Default browser styling *)
  | Button  (** Button appearance *)
  | Textfield  (** Text field appearance *)
  | Menulist  (** Select/dropdown appearance *)
  | Listbox  (** List box appearance *)
  | Checkbox  (** Checkbox appearance *)
  | Radio  (** Radio button appearance *)
  | Push_button  (** Push button appearance *)
  | Square_button  (** Square button appearance *)

(** CSS -moz-osx-font-smoothing values *)
type moz_osx_font_smoothing = Auto | Grayscale | Inherit

(** CSS transform-style values *)

(** CSS backface-visibility values *)

(** CSS scroll-behavior values *)

(** CSS scroll-snap-type values *)

(** CSS scroll-snap-align values *)

(** CSS scroll-snap-stop values *)

(** CSS isolation values *)

(** CSS background-repeat values *)
type background_repeat =
  | Repeat
  | Repeat_x
  | Repeat_y
  | No_repeat
  | Space
  | Round
  | Inherit

(** CSS background-size values. *)
type background_size =
  | Auto
  | Cover
  | Contain
  | Length of length
  | Percentage of float
  | Size of length * length
  | Inherit

(** CSS background-attachment values. *)
type background_attachment = Scroll | Fixed | Local | Inherit

(** CSS container-type values *)
type container_type = Normal | Size | Inline_size | Inherit

(** CSS contain values. *)
type contain =
  | None  (** No containment *)
  | Layout  (** Layout containment *)
  | Style  (** Style containment *)
  | Paint  (** Paint containment *)
  | Size  (** Size containment *)
  | Inline_size  (** Inline-size containment *)
  | Block_size  (** Block-size containment *)
  | Strict  (** Strict containment (layout + style + paint + size) *)
  | Content  (** Content containment (layout + style + paint) *)
  | Inherit  (** Inherit from parent *)
  | Var of contain var  (** CSS variable reference *)

(** CSS content-visibility values. *)
type content_visibility =
  | Visible  (** Content is visible and rendered *)
  | Hidden  (** Content is hidden from rendering *)
  | Auto  (** Browser determines visibility based on relevance *)
  | Inherit  (** Inherit from parent *)
  | Var of content_visibility var  (** CSS variable reference *)

(** CSS aspect-ratio values *)
type aspect_ratio = Auto | Ratio of float * float | Inherit

(** CSS justify values *)
type justify = Auto | Start | End | Center | Stretch | Flex_start | Flex_end

(** CSS place-content values *)
type place_content =
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly

(** CSS flex shorthand values *)
type flex =
  | Initial (* 0 1 auto *)
  | Auto (* 1 1 auto *)
  | None (* 0 0 auto *)
  | Grow of float (* grow 1 0% *)
  | Basis of length (* 1 1 basis *)
  | Grow_shrink of float * float (* grow shrink 0% *)
  | Full of float * float * length (* grow shrink basis *)

(** CSS timing function values *)
type timing_function =
  | Ease
  | Linear
  | Ease_in
  | Ease_out
  | Ease_in_out
  | Step_start
  | Step_end
  | Steps of int * [ `Start | `End ]
  | Cubic_bezier of float * float * float * float

(** CSS duration values *)
and duration =
  | Ms of int
  (* milliseconds *)
  | S of float (* seconds *)
  | Var of duration var (* CSS variable reference *)

(** CSS animation-fill-mode values *)
type animation_fill_mode = None | Forwards | Backwards | Both | Inherit

(** CSS animation-direction values *)
type animation_direction =
  | Normal
  | Reverse
  | Alternate
  | Alternate_reverse
  | Inherit

(** CSS animation-play-state values *)
type animation_play_state = Running | Paused | Inherit

(** CSS animation-iteration-count values *)
type animation_iteration_count = Count of int | Infinite | Inherit

type animation = {
  name : string option;
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  iteration_count : animation_iteration_count option;
  direction : animation_direction option;
  fill_mode : animation_fill_mode option;
  play_state : animation_play_state option;
}
(** CSS animation type *)

(** CSS blend mode values *)
type blend_mode =
  | Normal
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | Color_dodge
  | Color_burn
  | Hard_light
  | Soft_light
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity

(** CSS angle values *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var (* CSS variable reference *)

(** CSS number values (unitless numbers for filters, transforms, etc.). *)
type number =
  | Float of float  (** Floating point number *)
  | Int of int  (** Integer number *)
  | Pct of float  (** Percentage value *)
  | Var of number var  (** CSS variable reference *)

type font_family =
  (* Generic CSS font families *)
  | Sans_serif
  | Serif
  | Monospace
  | Cursive
  | Fantasy
  | System_ui
  | Ui_sans_serif
  | Ui_serif
  | Ui_monospace
  | Ui_rounded
  | Emoji
  | Math
  | Fangsong
  (* Popular web fonts *)
  | Inter
  | Roboto
  | Open_sans
  | Lato
  | Montserrat
  | Poppins
  | Source_sans_pro
  | Raleway
  | Oswald
  | Noto_sans
  | Ubuntu
  | Playfair_display
  | Merriweather
  | Lora
  | PT_sans
  | PT_serif
  | Nunito
  | Nunito_sans
  | Work_sans
  | Rubik
  | Fira_sans
  | Fira_code
  | JetBrains_mono
  | IBM_plex_sans
  | IBM_plex_serif
  | IBM_plex_mono
  | Source_code_pro
  | Space_mono
  | DM_sans
  | DM_serif_display
  | Bebas_neue
  | Barlow
  | Mulish
  | Josefin_sans
  (* Platform-specific fonts *)
  | Helvetica
  | Helvetica_neue
  | Arial
  | Verdana
  | Tahoma
  | Trebuchet_ms
  | Times_new_roman
  | Times
  | Georgia
  | Cambria
  | Garamond
  | Courier_new
  | Courier
  | Lucida_console
  | SF_pro
  | SF_pro_display
  | SF_pro_text
  | SF_mono
  | NY
  | Segoe_ui
  | Segoe_ui_emoji
  | Segoe_ui_symbol
  | Apple_color_emoji
  | Noto_color_emoji
  | Android_emoji
  | Twemoji_mozilla
  (* Developer fonts *)
  | Menlo
  | Monaco
  | Consolas
  | Liberation_mono
  | SFMono_regular
  | Cascadia_code
  | Cascadia_mono
  | Victor_mono
  | Inconsolata
  | Hack
  (* CSS keywords *)
  | Inherit
  | Initial
  | Unset
  (* CSS variables *)
  | Var of font_family list var

type font_feature_settings =
  | Normal
  | Feature_list of string
  | Inherit
  | String of string
  | Var of font_feature_settings var

type line_height =
  | Normal
  | Length of length
  | Number of float
  | Percentage of float
  | Inherit
  | Var of line_height var

type font_variation_settings =
  | Normal
  | Axis_list of string
  | Inherit
  | String of string
  | Var of font_variation_settings var

(** CSS transition property values *)
type transition_property = All | None | Property of string

(** CSS transition values *)
type transition =
  | Simple of transition_property * duration
  | With_timing of transition_property * duration * timing_function
  | With_delay of transition_property * duration * timing_function * duration
  | Multiple of transition list

(** CSS grid track sizing *)
type grid_track_size =
  | Fr of float
  | Min_max of length * grid_track_size
  | Grid_auto
  | Max_content
  | Min_content
  | Fit_content of length
  | Grid_length of length

(** CSS grid template values *)
type grid_template =
  | Tracks of grid_track_size list
  | Repeat of int * grid_track_size
  | Repeat_auto_fill of grid_track_size
  | Repeat_auto_fit of grid_track_size
  | None
  | Inherit

(** CSS grid line values *)
type grid_line =
  | Line_number of int (* 1, 2, 3, ... or -1, -2, ... *)
  | Line_name of string (* "header-start", "main-end", etc. *)
  | Span of int (* span 2, span 3, etc. *)
  | Auto (* auto *)

(** CSS transform scale values *)
type transform_scale =
  | Num of float
  | Pct of float
  | Var of transform_scale var

(** CSS scale property values *)
type scale =
  | String of string
  | Num of float
  | Pct of float
  | None
  | Vars of transform_scale var list

(** CSS transform values *)
type transform =
  | Translate of length * length option
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate3d of length * length * length
  | Rotate of angle
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate3d of float * float * float * angle
  | Scale of transform_scale * transform_scale option
  | Scale_x of transform_scale
  | Scale_y of transform_scale
  | Scale_z of transform_scale
  | Scale3d of transform_scale * transform_scale * transform_scale
  | Skew of angle * angle option
  | Skew_x of angle
  | Skew_y of angle
  | Matrix of (float * float * float * float * float * float)
  | Matrix3d of
      (float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float)
  | Perspective of length
  | None
  | Inherit
  | Var of transform list var

type shadow =
  | Shadow of {
      inset : bool;
      h_offset : length;
      v_offset : length;
      blur : length;
      spread : length;
      color : color;
    }
  | Var of shadow var

let shadow ?(inset = false) ?(h_offset = Px 0) ?(v_offset = Px 0) ?(blur = Px 0)
    ?(spread = Px 0) ?(color = Transparent) () =
  Shadow { inset; h_offset; v_offset; blur; spread; color }

let inset_ring_shadow ?(h_offset = Px 0) ?(v_offset = Px 0) ?(blur = Px 0)
    ?(spread = Px 0) ?(color = Transparent) () =
  shadow ~inset:true ~h_offset ~v_offset ~blur ~spread ~color ()

type box_shadow =
  | Shadow of shadow
  | Shadows of shadow list
  | None
  | Var of box_shadow var

let box_shadows lst = Shadows lst

type text_shadow =
  | Shadow of shadow
  | Shadows of shadow list  (** Multiple text shadows *)
  | None  (** No shadow *)
  | Var of text_shadow var  (** Composed shadow variable *)

(** Filter value for visual effects. *)
type filter =
  | None  (** No filter *)
  | Blur of length  (** blur(px) *)
  | Brightness of number  (** brightness(%) *)
  | Contrast of number  (** contrast(%) *)
  | Drop_shadow of shadow  (** drop-shadow(...) *)
  | Grayscale of number  (** grayscale(%) *)
  | Hue_rotate of angle  (** hue-rotate(deg) *)
  | Invert of number  (** invert(%) *)
  | Opacity of number  (** opacity(%) *)
  | Saturate of number  (** saturate(%) *)
  | Sepia of number  (** sepia(%) *)
  | Url of string  (** url(...) *)
  | List of filter list  (** Multiple filters *)
  | Var of filter var  (** Custom filter variable *)

type float_side = None | Left | Right

(** Value kind GADT for typed custom properties *)
type _ kind =
  | Length : length kind
  | Color : color kind
  | Int : int kind
  | Float : float kind
  | String : string kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Font_weight : font_weight kind
  | Font_family : font_family list kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Font_variant_numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Shadow : shadow kind
  | Transform_scale : transform_scale kind
  | Box_shadow : box_shadow kind
  | Content : content kind

let pp_op ctx = function
  | Add -> Pp.string ctx " + "
  | Sub -> Pp.string ctx " - "
  | Mult -> Pp.string ctx " * "
  | Div -> Pp.string ctx " / "

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  (* TODO: Add proper CSS variable inlining phase *)
  if ctx.inline && v.default <> None then
    (* When inlining is enabled, output the default value if available *)
    match v.default with
    | Some value -> pp_value ctx value
    | None -> assert false (* unreachable due to condition above *)
  else
    (* Standard var() reference output *)
    Pp.string ctx "var(--";
  Pp.string ctx v.name;
  match v.fallback with
  | None -> Pp.char ctx ')'
  | Some value ->
      Pp.string ctx ", ";
      pp_value ctx value;
      Pp.char ctx ')'

(** Helper to format function calls: name(args) *)
let pp_fun name pp_args ctx args =
  Pp.string ctx name;
  Pp.char ctx '(';
  pp_args ctx args;
  Pp.char ctx ')'

(** Helper to format function calls with comma-separated list: name(arg1, arg2,
    ...) *)
let pp_fun' name pp_item ctx items =
  pp_fun name (Pp.list ~sep:Pp.comma pp_item) ctx items

let rec pp_calc : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  match calc with
  | Val v -> pp_value ctx v
  | Var v -> pp_var pp_value ctx v
  | Expr (left, op, right) ->
      pp_calc pp_value ctx left;
      pp_op ctx op;
      pp_calc pp_value ctx right

let rec pp_length : length Pp.t =
 fun ctx -> function
  | Px n when n = 0 -> Pp.char ctx '0'
  | Rem f when f = 0. -> Pp.char ctx '0'
  | Em f when f = 0. -> Pp.char ctx '0'
  | Pct f when f = 0. -> Pp.char ctx '0'
  | Vw f when f = 0. -> Pp.char ctx '0'
  | Vh f when f = 0. -> Pp.char ctx '0'
  | Ch f when f = 0. -> Pp.char ctx '0'
  | Lh f when f = 0. -> Pp.char ctx '0'
  | Zero -> Pp.char ctx '0'
  | Px n ->
      Pp.int ctx n;
      Pp.string ctx "px"
  | Rem f ->
      Pp.float ctx f;
      Pp.string ctx "rem"
  | Em f ->
      Pp.float ctx f;
      Pp.string ctx "em"
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Vw f ->
      Pp.float ctx f;
      Pp.string ctx "vw"
  | Vh f ->
      Pp.float ctx f;
      Pp.string ctx "vh"
  | Ch f ->
      Pp.float ctx f;
      Pp.string ctx "ch"
  | Lh f ->
      Pp.float ctx f;
      Pp.string ctx "lh"
  | Num f -> Pp.float ctx f
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Fit_content -> Pp.string ctx "fit-content"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | From_font -> Pp.string ctx "from-font"
  | Var v -> pp_var pp_length ctx v
  | Calc cv -> (
      (* Optimize calc(infinity * 1px) to 3.40282e38px for minification *)
      match cv with
      | Expr (Val (Num f), Mult, Val (Px 1)) when f = infinity ->
          Pp.string ctx "3.40282e38px"
      | _ -> pp_fun "calc" (pp_calc pp_length) ctx cv)

(* Calc module for building calc() expressions *)
module Calc = struct
  let add left right = Expr (left, Add, right)
  let sub left right = Expr (left, Sub, right)
  let mul left right = Expr (left, Mult, right)
  let div left right = Expr (left, Div, right)

  (* Operators *)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div

  (* Value constructors *)
  let length len = Val len

  let var : ?default:'a -> ?fallback:'a -> string -> 'a calc =
   fun ?default ?fallback name -> Var (var_ref ?default ?fallback name)

  let float f : length calc = Val (Num f)
  let infinity : length calc = Val (Num infinity)
  let px n = Val (Px n)
  let rem f = Val (Rem f)
  let em f = Val (Em f)
  let pct f : length calc = Val (Pct f)
end

let rec pp_content : content Pp.t =
 fun ctx -> function
  | String s ->
      Pp.char ctx '"';
      Pp.string ctx s;
      Pp.char ctx '"'
  | None -> Pp.string ctx "none"
  | Normal -> Pp.string ctx "normal"
  | Open_quote -> Pp.string ctx "open-quote"
  | Close_quote -> Pp.string ctx "close-quote"
  | Var v -> pp_var pp_content ctx v

let rec pp_color_in_mix : color Pp.t =
 fun ctx -> function
  | Current -> Pp.string ctx "currentcolor" (* lowercase in color-mix *)
  | c -> pp_color ctx c

and pp_color_name : color_name Pp.t =
 fun ctx -> function
  | Red -> Pp.string ctx "red"
  | Blue -> Pp.string ctx "blue"
  | Green -> Pp.string ctx "green"
  | White -> Pp.string ctx "white"
  | Black -> Pp.string ctx "black"
  | Yellow -> Pp.string ctx "yellow"
  | Cyan -> Pp.string ctx "cyan"
  | Magenta -> Pp.string ctx "magenta"
  | Gray -> Pp.string ctx "gray"
  | Grey -> Pp.string ctx "grey"
  | Orange -> Pp.string ctx "orange"
  | Purple -> Pp.string ctx "purple"
  | Pink -> Pp.string ctx "pink"
  | Silver -> Pp.string ctx "silver"
  | Maroon -> Pp.string ctx "maroon"
  | Fuchsia -> Pp.string ctx "fuchsia"
  | Lime -> Pp.string ctx "lime"
  | Olive -> Pp.string ctx "olive"
  | Navy -> Pp.string ctx "navy"
  | Teal -> Pp.string ctx "teal"
  | Aqua -> Pp.string ctx "aqua"

(* RGB helper function *)
and pp_rgb ctx r g b alpha =
  Pp.string ctx "rgb(";
  Pp.int ctx r;
  Pp.space ctx ();
  Pp.int ctx g;
  Pp.space ctx ();
  Pp.int ctx b;
  (match alpha with
  | Some a ->
      Pp.string ctx " / ";
      Pp.float ctx a
  | None -> ());
  Pp.char ctx ')'

(* OKLCH helper function *)
and pp_oklch ctx l c h =
  Pp.string ctx "oklch(";
  Pp.float_n 1 ctx l;
  Pp.string ctx "% ";
  Pp.float_n 3 ctx c;
  Pp.space ctx ();
  Pp.float_n 3 ctx h;
  Pp.char ctx ')'

(* Color-mix helper function *)
and pp_color_mix ctx in_space color1 percent1 color2 percent2 =
  Pp.string ctx "color-mix(in ";
  pp_color_space ctx in_space;
  Pp.string ctx ", ";
  pp_color_in_mix ctx color1;
  (match percent1 with
  | Some p ->
      Pp.space ctx ();
      Pp.int ctx p;
      Pp.char ctx '%'
  | None -> ());
  Pp.string ctx ", ";
  pp_color_in_mix ctx color2;
  (match percent2 with
  | Some p ->
      Pp.space ctx ();
      Pp.int ctx p;
      Pp.char ctx '%'
  | None -> ());
  Pp.char ctx ')'

(* Convert to Pp-based color formatter *)
and pp_color : color Pp.t =
 fun ctx -> function
  | Hex { hash = _; value } ->
      Pp.char ctx '#';
      Pp.string ctx value
  | Rgb { r; g; b } -> pp_rgb ctx r g b None
  | Rgba { r; g; b; a } -> pp_rgb ctx r g b (Some a)
  | Oklch { l; c; h } -> pp_oklch ctx l c h
  | Named name -> pp_color_name ctx name
  | Var v -> pp_var pp_color ctx v
  | Current -> Pp.string ctx "currentcolor"
  | Transparent -> Pp.string ctx "transparent"
  | Inherit -> Pp.string ctx "inherit"
  | Mix { in_space; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space color1 percent1 color2 percent2

and pp_color_space : color_space Pp.t =
 fun ctx -> function
  | Srgb -> Pp.string ctx "srgb"
  | Srgb_linear -> Pp.string ctx "srgb-linear"
  | Display_p3 -> Pp.string ctx "display-p3"
  | A98_rgb -> Pp.string ctx "a98-rgb"
  | Prophoto_rgb -> Pp.string ctx "prophoto-rgb"
  | Rec2020 -> Pp.string ctx "rec2020"
  | Lab -> Pp.string ctx "lab"
  | Oklab -> Pp.string ctx "oklab"
  | Xyz -> Pp.string ctx "xyz"
  | Xyz_d50 -> Pp.string ctx "xyz-d50"
  | Xyz_d65 -> Pp.string ctx "xyz-d65"
  | Lch -> Pp.string ctx "lch"
  | Oklch -> Pp.string ctx "oklch"
  | Hsl -> Pp.string ctx "hsl"
  | Hwb -> Pp.string ctx "hwb"

let pp_svg_paint : svg_paint Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Current_color -> Pp.string ctx "currentColor"
  | Color c -> pp_color ctx c

let rec pp_shadow : shadow Pp.t =
 fun ctx -> function
  | Shadow { inset; h_offset; v_offset; blur; spread; color } ->
      if inset then Pp.string ctx "inset ";
      pp_length ctx h_offset;
      Pp.space ctx ();
      pp_length ctx v_offset;
      (* Only include blur if it's not zero *)
      if blur <> Zero then (
        Pp.space ctx ();
        pp_length ctx blur;
        (* Only include spread if blur is present and spread is not zero *)
        if spread <> Zero then (
          Pp.space ctx ();
          pp_length ctx spread));
      (* Only include color if it's not currentcolor and some values were
         specified, or if color is explicitly different *)
      if color <> Current && (blur <> Zero || spread <> Zero) then (
        Pp.space ctx ();
        pp_color ctx color)
  | Var v -> pp_var pp_shadow ctx v

let rec pp_box_shadow : box_shadow Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Shadow shadow -> pp_shadow ctx shadow
  | Shadows shadows -> Pp.(list ~sep:comma) pp_shadow ctx shadows
  | Var v -> pp_var pp_box_shadow ctx v

let pp_blend_mode : blend_mode Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Multiply -> Pp.string ctx "multiply"
  | Screen -> Pp.string ctx "screen"
  | Overlay -> Pp.string ctx "overlay"
  | Darken -> Pp.string ctx "darken"
  | Lighten -> Pp.string ctx "lighten"
  | Color_dodge -> Pp.string ctx "color-dodge"
  | Color_burn -> Pp.string ctx "color-burn"
  | Hard_light -> Pp.string ctx "hard-light"
  | Soft_light -> Pp.string ctx "soft-light"
  | Difference -> Pp.string ctx "difference"
  | Exclusion -> Pp.string ctx "exclusion"
  | Hue -> Pp.string ctx "hue"
  | Saturation -> Pp.string ctx "saturation"
  | Color -> Pp.string ctx "color"
  | Luminosity -> Pp.string ctx "luminosity"

let pp_display : display Pp.t =
 fun ctx -> function
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inline_block -> Pp.string ctx "inline-block"
  | Flex -> Pp.string ctx "flex"
  | Inline_flex -> Pp.string ctx "inline-flex"
  | Grid -> Pp.string ctx "grid"
  | Inline_grid -> Pp.string ctx "inline-grid"
  | None -> Pp.string ctx "none"
  | Flow_root -> Pp.string ctx "flow-root"
  | Table -> Pp.string ctx "table"
  | Table_row -> Pp.string ctx "table-row"
  | Table_cell -> Pp.string ctx "table-cell"
  | Table_caption -> Pp.string ctx "table-caption"
  | Table_column -> Pp.string ctx "table-column"
  | Table_column_group -> Pp.string ctx "table-column-group"
  | Table_footer_group -> Pp.string ctx "table-footer-group"
  | Table_header_group -> Pp.string ctx "table-header-group"
  | Table_row_group -> Pp.string ctx "table-row-group"
  | Inline_table -> Pp.string ctx "inline-table"
  | List_item -> Pp.string ctx "list-item"
  | Contents -> Pp.string ctx "contents"
  | Webkit_box -> Pp.string ctx "-webkit-box"

let pp_position : position Pp.t =
 fun ctx -> function
  | Static -> Pp.string ctx "static"
  | Relative -> Pp.string ctx "relative"
  | Absolute -> Pp.string ctx "absolute"
  | Fixed -> Pp.string ctx "fixed"
  | Sticky -> Pp.string ctx "sticky"

let pp_visibility : visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Collapse -> Pp.string ctx "collapse"

let pp_z_index : z_index Pp.t =
 fun ctx -> function Auto -> Pp.string ctx "auto" | Index n -> Pp.int ctx n

let rec pp_font_weight : font_weight Pp.t =
 fun ctx -> function
  | Weight n -> Pp.int ctx n
  | Normal -> Pp.string ctx "normal"
  | Bold -> Pp.string ctx "bold"
  | Bolder -> Pp.string ctx "bolder"
  | Lighter -> Pp.string ctx "lighter"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_font_weight ctx v

let pp_text_align : text_align Pp.t =
 fun ctx -> function
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Center -> Pp.string ctx "center"
  | Justify -> Pp.string ctx "justify"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Inherit -> Pp.string ctx "inherit"

let pp_overflow : overflow Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Scroll -> Pp.string ctx "scroll"
  | (Auto : overflow) -> Pp.string ctx "auto"
  | Clip -> Pp.string ctx "clip"

let pp_flex_direction : flex_direction Pp.t =
 fun ctx -> function
  | Row -> Pp.string ctx "row"
  | Row_reverse -> Pp.string ctx "row-reverse"
  | Column -> Pp.string ctx "column"
  | Column_reverse -> Pp.string ctx "column-reverse"

let pp_flex_wrap : flex_wrap Pp.t =
 fun ctx -> function
  | Nowrap -> Pp.string ctx "nowrap"
  | Wrap -> Pp.string ctx "wrap"
  | Wrap_reverse -> Pp.string ctx "wrap-reverse"

let pp_align_items : align_items Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Safe_center -> Pp.string ctx "safe center"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Inherit_align -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_justify_content : justify_content Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Center -> Pp.string ctx "center"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Stretch -> Pp.string ctx "stretch"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"

let pp_align_self : align_self Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Center -> Pp.string ctx "center"
  | Baseline -> Pp.string ctx "baseline"
  | Stretch -> Pp.string ctx "stretch"

let pp_border_collapse : border_collapse Pp.t =
 fun ctx -> function
  | Collapse -> Pp.string ctx "collapse"
  | Separate -> Pp.string ctx "separate"
  | Inherit -> Pp.string ctx "inherit"

let pp_grid_auto_flow : grid_auto_flow Pp.t =
 fun ctx -> function
  | Row -> Pp.string ctx "row"
  | Column -> Pp.string ctx "column"
  | Row_dense -> Pp.string ctx "row dense"
  | Column_dense -> Pp.string ctx "column dense"

let rec pp_contain : contain Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Layout -> Pp.string ctx "layout"
  | Style -> Pp.string ctx "style"
  | Paint -> Pp.string ctx "paint"
  | Size -> Pp.string ctx "size"
  | Inline_size -> Pp.string ctx "inline-size"
  | Block_size -> Pp.string ctx "block-size"
  | Strict -> Pp.string ctx "strict"
  | Content -> Pp.string ctx "content"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_contain ctx v

let pp_isolation : isolation Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Isolate -> Pp.string ctx "isolate"
  | Inherit -> Pp.string ctx "inherit"

let pp_clear : clear Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Both -> Pp.string ctx "both"

let pp_float_side : float_side Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"

let pp_transform_style : transform_style Pp.t =
 fun ctx -> function
  | Flat -> Pp.string ctx "flat"
  | Preserve_3d -> Pp.string ctx "preserve-3d"
  | Inherit -> Pp.string ctx "inherit"

let pp_backface_visibility : backface_visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Inherit -> Pp.string ctx "inherit"

let pp_appearance : appearance Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Button -> Pp.string ctx "button"
  | Textfield -> Pp.string ctx "textfield"
  | Menulist -> Pp.string ctx "menulist"
  | Inherit -> Pp.string ctx "inherit"

let pp_resize : resize Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Both -> Pp.string ctx "both"
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inherit -> Pp.string ctx "inherit"

let pp_webkit_box_orient : webkit_box_orient Pp.t =
 fun ctx -> function
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Inherit -> Pp.string ctx "inherit"

let pp_object_fit : object_fit Pp.t =
 fun ctx -> function
  | Fill -> Pp.string ctx "fill"
  | Contain -> Pp.string ctx "contain"
  | Cover -> Pp.string ctx "cover"
  | None -> Pp.string ctx "none"
  | Scale_down -> Pp.string ctx "scale-down"
  | Inherit -> Pp.string ctx "inherit"

let pp_box_sizing : box_sizing Pp.t =
 fun ctx -> function
  | Border_box -> Pp.string ctx "border-box"
  | Content_box -> Pp.string ctx "content-box"
  | Inherit -> Pp.string ctx "inherit"

let pp_scroll_behavior : scroll_behavior Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Smooth -> Pp.string ctx "smooth"
  | Inherit -> Pp.string ctx "inherit"

let pp_scroll_snap_stop : scroll_snap_stop Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Always -> Pp.string ctx "always"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_scroll_snap_strictness : scroll_snap_strictness Pp.t =
 fun ctx -> function
  | Mandatory -> Pp.string ctx "mandatory"
  | Proximity -> Pp.string ctx "proximity"
  | Var v -> pp_var pp_scroll_snap_strictness ctx v

let pp_scroll_snap_align : scroll_snap_align Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"

let pp_scroll_snap_axis : scroll_snap_axis Pp.t =
 fun ctx -> function
  | X -> Pp.string ctx "x"
  | Y -> Pp.string ctx "y"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Both -> Pp.string ctx "both"

let pp_scroll_snap_type : scroll_snap_type Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Axis (axis, strictness) -> (
      pp_scroll_snap_axis ctx axis;
      match strictness with
      | None -> ()
      | Some s ->
          Pp.space ctx ();
          pp_scroll_snap_strictness ctx s)
  | Inherit -> Pp.string ctx "inherit"

let pp_touch_action : touch_action Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Pan_x -> Pp.string ctx "pan-x"
  | Pan_y -> Pp.string ctx "pan-y"
  | Manipulation -> Pp.string ctx "manipulation"
  | Inherit -> Pp.string ctx "inherit"

let pp_text_transform : text_transform Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Uppercase -> Pp.string ctx "uppercase"
  | Lowercase -> Pp.string ctx "lowercase"
  | Capitalize -> Pp.string ctx "capitalize"
  | Full_width -> Pp.string ctx "full-width"
  | Full_size_kana -> Pp.string ctx "full-size-kana"
  | Inherit -> Pp.string ctx "inherit"

let pp_white_space : white_space Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Nowrap -> Pp.string ctx "nowrap"
  | Pre -> Pp.string ctx "pre"
  | Pre_wrap -> Pp.string ctx "pre-wrap"
  | Pre_line -> Pp.string ctx "pre-line"
  | Break_spaces -> Pp.string ctx "break-spaces"
  | Inherit -> Pp.string ctx "inherit"

let pp_table_layout : table_layout Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Fixed -> Pp.string ctx "fixed"
  | Inherit -> Pp.string ctx "inherit"

let pp_vertical_align : vertical_align Pp.t =
 fun ctx -> function
  | Baseline -> Pp.string ctx "baseline"
  | Top -> Pp.string ctx "top"
  | Middle -> Pp.string ctx "middle"
  | Bottom -> Pp.string ctx "bottom"
  | Text_top -> Pp.string ctx "text-top"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Length l -> pp_length ctx l
  | Percentage p ->
      Pp.float ctx p;
      Pp.char ctx '%'
  | Inherit -> Pp.string ctx "inherit"

let pp_pointer_events : pointer_events Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Visible_painted -> Pp.string ctx "visiblePainted"
  | Visible_fill -> Pp.string ctx "visibleFill"
  | Visible_stroke -> Pp.string ctx "visibleStroke"
  | Visible -> Pp.string ctx "visible"
  | Painted -> Pp.string ctx "painted"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"

let pp_text_decoration_style : text_decoration_style Pp.t =
 fun ctx -> function
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Wavy -> Pp.string ctx "wavy"
  | Inherit -> Pp.string ctx "inherit"

let pp_text_decoration_skip_ink : text_decoration_skip_ink Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"

let pp_direction : direction Pp.t =
 fun ctx -> function
  | Ltr -> Pp.string ctx "ltr"
  | Rtl -> Pp.string ctx "rtl"
  | Inherit -> Pp.string ctx "inherit"

let pp_unicode_bidi : unicode_bidi Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Embed -> Pp.string ctx "embed"
  | Bidi_override -> Pp.string ctx "bidi-override"
  | Isolate -> Pp.string ctx "isolate"
  | Isolate_override -> Pp.string ctx "isolate-override"
  | Plaintext -> Pp.string ctx "plaintext"
  | Inherit -> Pp.string ctx "inherit"

let pp_writing_mode : writing_mode Pp.t =
 fun ctx -> function
  | Horizontal_tb -> Pp.string ctx "horizontal-tb"
  | Vertical_rl -> Pp.string ctx "vertical-rl"
  | Vertical_lr -> Pp.string ctx "vertical-lr"
  | Inherit -> Pp.string ctx "inherit"

let pp_animation_fill_mode : animation_fill_mode Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Forwards -> Pp.string ctx "forwards"
  | Backwards -> Pp.string ctx "backwards"
  | Both -> Pp.string ctx "both"
  | Inherit -> Pp.string ctx "inherit"

let pp_animation_direction : animation_direction Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Reverse -> Pp.string ctx "reverse"
  | Alternate -> Pp.string ctx "alternate"
  | Alternate_reverse -> Pp.string ctx "alternate-reverse"
  | Inherit -> Pp.string ctx "inherit"

let pp_animation_play_state : animation_play_state Pp.t =
 fun ctx -> function
  | Running -> Pp.string ctx "running"
  | Paused -> Pp.string ctx "paused"
  | Inherit -> Pp.string ctx "inherit"

let pp_animation_iteration_count : animation_iteration_count Pp.t =
 fun ctx -> function
  | Count n -> Pp.int ctx n
  | Infinite -> Pp.string ctx "infinite"
  | Inherit -> Pp.string ctx "inherit"

let pp_overscroll_behavior : overscroll_behavior Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Contain -> Pp.string ctx "contain"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"

let pp_webkit_font_smoothing : webkit_font_smoothing Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Antialiased -> Pp.string ctx "antialiased"
  | Subpixel_antialiased -> Pp.string ctx "subpixel-antialiased"
  | Inherit -> Pp.string ctx "inherit"

let pp_webkit_appearance : webkit_appearance Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Button -> Pp.string ctx "button"
  | Textfield -> Pp.string ctx "textfield"
  | Menulist -> Pp.string ctx "menulist"
  | Listbox -> Pp.string ctx "listbox"
  | Checkbox -> Pp.string ctx "checkbox"
  | Radio -> Pp.string ctx "radio"
  | Push_button -> Pp.string ctx "push-button"
  | Square_button -> Pp.string ctx "square-button"

let pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Grayscale -> Pp.string ctx "grayscale"
  | Inherit -> Pp.string ctx "inherit"

let pp_background_repeat : background_repeat Pp.t =
 fun ctx -> function
  | Repeat -> Pp.string ctx "repeat"
  | Repeat_x -> Pp.string ctx "repeat-x"
  | Repeat_y -> Pp.string ctx "repeat-y"
  | No_repeat -> Pp.string ctx "no-repeat"
  | Space -> Pp.string ctx "space"
  | Round -> Pp.string ctx "round"
  | Inherit -> Pp.string ctx "inherit"

let pp_background_size : background_size Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Cover -> Pp.string ctx "cover"
  | Contain -> Pp.string ctx "contain"
  | Length l -> pp_length ctx l
  | Percentage p -> Pp.string ctx (Printf.sprintf "%g%%" p)
  | Size (w, h) ->
      pp_length ctx w;
      Pp.space ctx ();
      pp_length ctx h
  | Inherit -> Pp.string ctx "inherit"

let pp_container_type : container_type Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Size -> Pp.string ctx "size"
  | Inline_size -> Pp.string ctx "inline-size"
  | Inherit -> Pp.string ctx "inherit"

let pp_flex : flex Pp.t =
 fun ctx -> function
  | Initial -> Pp.string ctx "0 1 auto"
  | Auto -> Pp.string ctx "1 1 auto"
  | None -> Pp.string ctx "0 0 auto"
  | Grow f -> Pp.float ctx f
  | Basis len ->
      Pp.string ctx "1 1 ";
      pp_length ctx len
  | Grow_shrink (g, s) ->
      Pp.float ctx g;
      Pp.space ctx ();
      Pp.float ctx s;
      Pp.string ctx " 0%"
  | Full (g, s, b) ->
      Pp.float ctx g;
      Pp.space ctx ();
      Pp.float ctx s;
      Pp.space ctx ();
      pp_length ctx b

let rec pp_duration : duration Pp.t =
 fun ctx -> function
  | Ms n ->
      Pp.int ctx n;
      Pp.string ctx "ms"
  | S f ->
      Pp.float ctx f;
      Pp.char ctx 's'
  | Var v -> pp_var pp_duration ctx v

let pp_timing_function : timing_function Pp.t =
 fun ctx -> function
  | Ease -> Pp.string ctx "ease"
  | Linear -> Pp.string ctx "linear"
  | Ease_in -> Pp.string ctx "ease-in"
  | Ease_out -> Pp.string ctx "ease-out"
  | Ease_in_out -> Pp.string ctx "ease-in-out"
  | Step_start -> Pp.string ctx "step-start"
  | Step_end -> Pp.string ctx "step-end"
  | Steps (n, dir) ->
      Pp.string ctx "steps(";
      Pp.int ctx n;
      Pp.comma ctx ();
      let dir_str = match dir with `Start -> "start" | `End -> "end" in
      Pp.string ctx dir_str;
      Pp.char ctx ')'
  | Cubic_bezier (a, b, c, d) ->
      Pp.string ctx "cubic-bezier(";
      Pp.float ctx a;
      Pp.comma ctx ();
      Pp.float ctx b;
      Pp.comma ctx ();
      Pp.float ctx c;
      Pp.comma ctx ();
      Pp.float ctx d;
      Pp.char ctx ')'

let pp_animation : animation Pp.t =
 fun ctx anim ->
  let parts = [] in
  let parts = match anim.name with Some n -> n :: parts | None -> parts in
  let parts =
    match anim.duration with
    | Some d ->
        let s = Pp.to_string ~minify:ctx.minify pp_duration d in
        s :: parts
    | None -> parts
  in
  let parts =
    match anim.timing_function with
    | Some tf ->
        let s = Pp.to_string ~minify:ctx.minify pp_timing_function tf in
        s :: parts
    | None -> parts
  in
  let parts =
    match anim.delay with
    | Some d ->
        let s = Pp.to_string ~minify:ctx.minify pp_duration d in
        s :: parts
    | None -> parts
  in
  let parts =
    match anim.iteration_count with
    | Some ic ->
        let s =
          Pp.to_string ~minify:ctx.minify pp_animation_iteration_count ic
        in
        s :: parts
    | None -> parts
  in
  let parts =
    match anim.direction with
    | Some d ->
        let s = Pp.to_string ~minify:ctx.minify pp_animation_direction d in
        s :: parts
    | None -> parts
  in
  let parts =
    match anim.fill_mode with
    | Some fm ->
        let s = Pp.to_string ~minify:ctx.minify pp_animation_fill_mode fm in
        s :: parts
    | None -> parts
  in
  let parts =
    match anim.play_state with
    | Some ps ->
        let s = Pp.to_string ~minify:ctx.minify pp_animation_play_state ps in
        s :: parts
    | None -> parts
  in
  Pp.string ctx (String.concat " " (List.rev parts))

let pp_transition_property : transition_property Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | None -> Pp.string ctx "none"
  | Property p -> Pp.string ctx p

let rec pp_transition : transition Pp.t =
 fun ctx -> function
  | Simple (prop, dur) ->
      pp_transition_property ctx prop;
      Pp.space ctx ();
      pp_duration ctx dur
  | With_timing (prop, dur, timing) ->
      pp_transition_property ctx prop;
      Pp.space ctx ();
      pp_duration ctx dur;
      Pp.space ctx ();
      pp_timing_function ctx timing
  | With_delay (prop, dur, timing, delay) ->
      pp_transition_property ctx prop;
      Pp.space ctx ();
      pp_duration ctx dur;
      Pp.space ctx ();
      pp_timing_function ctx timing;
      Pp.space ctx ();
      pp_duration ctx delay
  | Multiple transitions -> Pp.(list ~sep:comma) pp_transition ctx transitions

let pp_align : align Pp.t =
 fun ctx -> function
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Center -> Pp.string ctx "center"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Stretch -> Pp.string ctx "stretch"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Baseline -> Pp.string ctx "baseline"
  | Auto -> Pp.string ctx "auto"

let pp_justify : justify Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"

let pp_aspect_ratio : aspect_ratio Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Ratio (w, h) ->
      Pp.float ctx w;
      Pp.slash ctx ();
      Pp.float ctx h
  | Inherit -> Pp.string ctx "inherit"

let pp_background_attachment : background_attachment Pp.t =
 fun ctx -> function
  | Scroll -> Pp.string ctx "scroll"
  | Fixed -> Pp.string ctx "fixed"
  | Local -> Pp.string ctx "local"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_text_shadow : text_shadow Pp.t =
 fun ctx -> function
  | Shadow s -> pp_shadow ctx s
  | Shadows shadows -> Pp.(list ~sep:comma) pp_shadow ctx shadows
  | None -> Pp.string ctx "none"
  | Var v -> pp_var pp_text_shadow ctx v

let rec pp_content_visibility : content_visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_content_visibility ctx v

let pp_place_items : place_items Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Auto -> Pp.string ctx "auto"
  | Stretch -> Pp.string ctx "stretch"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Inherit -> Pp.string ctx "inherit"

let pp_place_content : place_content Pp.t =
 fun ctx -> function
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"

let pp_text_decoration : text_decoration Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Underline -> Pp.string ctx "underline"
  | Overline -> Pp.string ctx "overline"
  | Line_through -> Pp.string ctx "line-through"
  | Inherit -> Pp.string ctx "inherit"
  | Underline_dotted -> Pp.string ctx "underline dotted"

let pp_font_style : font_style Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Italic -> Pp.string ctx "italic"
  | Oblique -> Pp.string ctx "oblique"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_line_height : line_height Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Length l -> pp_length ctx l
  | Number n -> Pp.float ctx n
  | Percentage p ->
      Pp.float ctx p;
      Pp.char ctx '%'
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_line_height ctx v

let pp_list_style_type : list_style_type Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Disc -> Pp.string ctx "disc"
  | Circle -> Pp.string ctx "circle"
  | Square -> Pp.string ctx "square"
  | Decimal -> Pp.string ctx "decimal"
  | Lower_alpha -> Pp.string ctx "lower-alpha"
  | Upper_alpha -> Pp.string ctx "upper-alpha"
  | Lower_roman -> Pp.string ctx "lower-roman"
  | Upper_roman -> Pp.string ctx "upper-roman"

let pp_list_style_position : list_style_position Pp.t =
 fun ctx -> function
  | Inside -> Pp.string ctx "inside"
  | Outside -> Pp.string ctx "outside"
  | Inherit -> Pp.string ctx "inherit"

let pp_list_style_image : list_style_image Pp.t =
 fun ctx -> function
  | None_img -> Pp.string ctx "none"
  | Url u -> pp_fun "url" Pp.string ctx u
  | Inherit -> Pp.string ctx "inherit"

let rec pp_border_style : border_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Solid -> Pp.string ctx "solid"
  | Dashed -> Pp.string ctx "dashed"
  | Dotted -> Pp.string ctx "dotted"
  | Double -> Pp.string ctx "double"
  | Groove -> Pp.string ctx "groove"
  | Ridge -> Pp.string ctx "ridge"
  | Inset -> Pp.string ctx "inset"
  | Outset -> Pp.string ctx "outset"
  | Hidden -> Pp.string ctx "hidden"
  | Var v -> pp_var pp_border_style ctx v

let pp_outline_style : outline_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Groove -> Pp.string ctx "groove"
  | Ridge -> Pp.string ctx "ridge"
  | Inset -> Pp.string ctx "inset"
  | Outset -> Pp.string ctx "outset"
  | Inherit -> Pp.string ctx "inherit"

let pp_forced_color_adjust : forced_color_adjust Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"

let pp_cursor : cursor Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Default -> Pp.string ctx "default"
  | Pointer -> Pp.string ctx "pointer"
  | Wait -> Pp.string ctx "wait"
  | Text -> Pp.string ctx "text"
  | Move -> Pp.string ctx "move"
  | Help -> Pp.string ctx "help"
  | Not_allowed -> Pp.string ctx "not-allowed"
  | None -> Pp.string ctx "none"
  | Context_menu -> Pp.string ctx "context-menu"
  | Progress -> Pp.string ctx "progress"
  | Cell -> Pp.string ctx "cell"
  | Crosshair -> Pp.string ctx "crosshair"
  | Vertical_text -> Pp.string ctx "vertical-text"
  | Alias -> Pp.string ctx "alias"
  | Copy -> Pp.string ctx "copy"
  | No_drop -> Pp.string ctx "no-drop"
  | Grab -> Pp.string ctx "grab"
  | Grabbing -> Pp.string ctx "grabbing"
  | All_scroll -> Pp.string ctx "all-scroll"
  | Col_resize -> Pp.string ctx "col-resize"
  | Row_resize -> Pp.string ctx "row-resize"
  | N_resize -> Pp.string ctx "n-resize"
  | E_resize -> Pp.string ctx "e-resize"
  | S_resize -> Pp.string ctx "s-resize"
  | W_resize -> Pp.string ctx "w-resize"
  | Ne_resize -> Pp.string ctx "ne-resize"
  | Nw_resize -> Pp.string ctx "nw-resize"
  | Se_resize -> Pp.string ctx "se-resize"
  | Sw_resize -> Pp.string ctx "sw-resize"
  | Ew_resize -> Pp.string ctx "ew-resize"
  | Ns_resize -> Pp.string ctx "ns-resize"
  | Nesw_resize -> Pp.string ctx "nesw-resize"
  | Nwse_resize -> Pp.string ctx "nwse-resize"
  | Zoom_in -> Pp.string ctx "zoom-in"
  | Zoom_out -> Pp.string ctx "zoom-out"

let pp_user_select : user_select Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Text -> Pp.string ctx "text"
  | All -> Pp.string ctx "all"
  | Contain -> Pp.string ctx "contain"

let rec pp_grid_track_size : grid_track_size Pp.t =
 fun ctx -> function
  | Fr f ->
      Pp.float ctx f;
      Pp.string ctx "fr"
  | Min_max (min, max) ->
      Pp.string ctx "minmax(";
      pp_length ctx min;
      Pp.comma ctx ();
      pp_grid_track_size ctx max;
      Pp.char ctx ')'
  | Grid_auto -> Pp.string ctx "auto"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | Fit_content l ->
      Pp.string ctx "fit-content(";
      pp_length ctx l;
      Pp.char ctx ')'
  | Grid_length l -> pp_length ctx l

let pp_grid_template : grid_template Pp.t =
 fun ctx -> function
  | Tracks sizes -> Pp.(list ~sep:space) pp_grid_track_size ctx sizes
  | Repeat (count, size) ->
      Pp.string ctx "repeat(";
      Pp.int ctx count;
      Pp.comma ctx ();
      pp_grid_track_size ctx size;
      Pp.char ctx ')'
  | Repeat_auto_fill size ->
      Pp.string ctx "repeat(auto-fill, ";
      pp_grid_track_size ctx size;
      Pp.char ctx ')'
  | Repeat_auto_fit size ->
      Pp.string ctx "repeat(auto-fit, ";
      pp_grid_track_size ctx size;
      Pp.char ctx ')'
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"

let pp_grid_line : grid_line Pp.t =
 fun ctx -> function
  | Line_number n -> Pp.int ctx n
  | Line_name name -> Pp.string ctx name
  | Span n ->
      Pp.string ctx "span ";
      Pp.int ctx n
  | Auto -> Pp.string ctx "auto"

let rec pp_angle : angle Pp.t =
 fun ctx -> function
  | Deg f ->
      Pp.float ctx f;
      Pp.string ctx "deg"
  | Rad f ->
      Pp.float ctx f;
      Pp.string ctx "rad"
  | Turn f ->
      Pp.float ctx f;
      Pp.string ctx "turn"
  | Grad f ->
      Pp.float ctx f;
      Pp.string ctx "grad"
  | Var v -> pp_var pp_angle ctx v

let rec pp_transform_scale : transform_scale Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_transform_scale ctx v

let pp_scale : scale Pp.t =
 fun ctx -> function
  | String s -> Pp.string ctx s
  | Num f -> Pp.float ctx f
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | None -> Pp.string ctx "none"
  | Vars vars -> Pp.(list ~sep:comma) (pp_var pp_transform_scale) ctx vars

let rec pp_font_feature_settings : font_feature_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Feature_list s -> Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | String s -> Pp.string ctx s
  | Var v -> pp_var pp_font_feature_settings ctx v

let rec pp_font_variation_settings : font_variation_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Axis_list s -> Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | String s -> Pp.string ctx s
  | Var v -> pp_var pp_font_variation_settings ctx v

let pp_translate : (length * length option * length option) Pp.t =
 fun ctx (x, y_opt, z_opt) ->
  match (y_opt, z_opt) with
  | Some y, None ->
      Pp.string ctx "translate(";
      pp_length ctx x;
      Pp.comma ctx ();
      pp_length ctx y;
      Pp.char ctx ')'
  | None, Some z ->
      Pp.string ctx "translate3d(";
      pp_length ctx x;
      Pp.comma ctx ();
      Pp.string ctx "0";
      Pp.comma ctx ();
      pp_length ctx z;
      Pp.char ctx ')'
  | Some y, Some z ->
      Pp.string ctx "translate3d(";
      pp_length ctx x;
      Pp.comma ctx ();
      pp_length ctx y;
      Pp.comma ctx ();
      pp_length ctx z;
      Pp.char ctx ')'
  | None, None ->
      Pp.string ctx "translate(";
      pp_length ctx x;
      Pp.char ctx ')'

let pp_rotate :
    (angle * [ `None | `X | `Y | `Z | `Vec3d of float * float * float ]) Pp.t =
 fun ctx (angle, axis) ->
  match axis with
  | `None ->
      Pp.string ctx "rotate(";
      pp_angle ctx angle;
      Pp.char ctx ')'
  | `X ->
      Pp.string ctx "rotateX(";
      pp_angle ctx angle;
      Pp.char ctx ')'
  | `Y ->
      Pp.string ctx "rotateY(";
      pp_angle ctx angle;
      Pp.char ctx ')'
  | `Z ->
      Pp.string ctx "rotateZ(";
      pp_angle ctx angle;
      Pp.char ctx ')'
  | `Vec3d (x, y, z) ->
      Pp.string ctx "rotate3d(";
      Pp.float ctx x;
      Pp.comma ctx ();
      Pp.float ctx y;
      Pp.comma ctx ();
      Pp.float ctx z;
      Pp.comma ctx ();
      pp_angle ctx angle;
      Pp.char ctx ')'

let pp_scale_transform :
    (transform_scale * transform_scale option * transform_scale option) Pp.t =
 fun ctx (x, y_opt, z_opt) ->
  match (y_opt, z_opt) with
  | Some y, None ->
      Pp.string ctx "scale(";
      pp_transform_scale ctx x;
      Pp.comma ctx ();
      pp_transform_scale ctx y;
      Pp.char ctx ')'
  | None, Some z ->
      Pp.string ctx "scale3d(";
      pp_transform_scale ctx x;
      Pp.comma ctx ();
      Pp.string ctx "1";
      Pp.comma ctx ();
      pp_transform_scale ctx z;
      Pp.char ctx ')'
  | Some y, Some z ->
      Pp.string ctx "scale3d(";
      pp_transform_scale ctx x;
      Pp.comma ctx ();
      pp_transform_scale ctx y;
      Pp.comma ctx ();
      pp_transform_scale ctx z;
      Pp.char ctx ')'
  | None, None ->
      Pp.string ctx "scale(";
      pp_transform_scale ctx x;
      Pp.char ctx ')'

let pp_skew : (angle * angle option) Pp.t =
 fun ctx (x, y_opt) ->
  match y_opt with
  | Some y -> pp_fun "skew" (Pp.list ~sep:Pp.comma pp_angle) ctx [ x; y ]
  | None -> pp_fun "skewX" pp_angle ctx x

let pp_matrix_2d : (float * float * float * float * float * float) Pp.t =
 fun ctx (a, b, c, d, e, f) ->
  let values = [ a; b; c; d; e; f ] in
  pp_fun' "matrix" Pp.float ctx values

let pp_matrix_3d : _ Pp.t =
 fun ctx
     (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16) ->
  let values =
    [ m1; m2; m3; m4; m5; m6; m7; m8; m9; m10; m11; m12; m13; m14; m15; m16 ]
  in
  Pp.string ctx "matrix3d(";
  Pp.(list ~sep:comma float) ctx values;
  Pp.char ctx ')'

let rec pp_transform : transform Pp.t =
 fun ctx -> function
  | Translate (x, None) -> pp_translate ctx (x, None, None)
  | Translate (x, Some y) -> pp_translate ctx (x, Some y, None)
  | Translate_x l -> pp_fun "translateX" pp_length ctx l
  | Translate_y l -> pp_fun "translateY" pp_length ctx l
  | Translate_z l -> pp_fun "translateZ" pp_length ctx l
  | Translate3d (x, y, z) -> pp_translate ctx (x, Some y, Some z)
  | Rotate a -> pp_rotate ctx (a, `None)
  | Rotate_x a -> pp_rotate ctx (a, `X)
  | Rotate_y a -> pp_rotate ctx (a, `Y)
  | Rotate_z a -> pp_rotate ctx (a, `Z)
  | Rotate3d (x, y, z, angle) -> pp_rotate ctx (angle, `Vec3d (x, y, z))
  | Scale (x, None) -> pp_scale_transform ctx (x, None, None)
  | Scale (x, Some y) -> pp_scale_transform ctx (x, Some y, None)
  | Scale_x s -> pp_fun "scaleX" pp_transform_scale ctx s
  | Scale_y s -> pp_fun "scaleY" pp_transform_scale ctx s
  | Scale_z s -> pp_fun "scaleZ" pp_transform_scale ctx s
  | Scale3d (x, y, z) -> pp_scale_transform ctx (x, Some y, Some z)
  | Skew (x, None) -> pp_skew ctx (x, None)
  | Skew (x, Some y) -> pp_skew ctx (x, Some y)
  | Skew_x a -> pp_fun "skewX" pp_angle ctx a
  | Skew_y a -> pp_fun "skewY" pp_angle ctx a
  | Matrix m -> pp_matrix_2d ctx m
  | Matrix3d m -> pp_matrix_3d ctx m
  | Perspective l -> pp_fun "perspective" pp_length ctx l
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var (Pp.list ~sep:Pp.space pp_transform) ctx v

let rec pp_font_variant_token : font_variant_numeric_token Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Lining_nums -> Pp.string ctx "lining-nums"
  | Oldstyle_nums -> Pp.string ctx "oldstyle-nums"
  | Proportional_nums -> Pp.string ctx "proportional-nums"
  | Tabular_nums -> Pp.string ctx "tabular-nums"
  | Diagonal_fractions -> Pp.string ctx "diagonal-fractions"
  | Stacked_fractions -> Pp.string ctx "stacked-fractions"
  | Ordinal -> Pp.string ctx "ordinal"
  | Slashed_zero -> Pp.string ctx "slashed-zero"
  | Var v -> pp_var pp_font_variant_token ctx v

let rec pp_font_variant_numeric : font_variant_numeric Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Tokens tokens -> Pp.list ~sep:Pp.space pp_font_variant_token ctx tokens
  | Var v -> pp_var pp_font_variant_numeric ctx v
  | Composed
      {
        ordinal;
        slashed_zero;
        numeric_figure;
        numeric_spacing;
        numeric_fraction;
      } ->
      Option.iter (pp_font_variant_token ctx) ordinal;
      Option.iter (pp_font_variant_token ctx) slashed_zero;
      Option.iter (pp_font_variant_token ctx) numeric_figure;
      Option.iter (pp_font_variant_token ctx) numeric_spacing;
      Option.iter (pp_font_variant_token ctx) numeric_fraction

let pp_text_overflow : text_overflow Pp.t =
 fun ctx -> function
  | Clip -> Pp.string ctx "clip"
  | Ellipsis -> Pp.string ctx "ellipsis"
  | String s -> Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"

let pp_text_wrap : text_wrap Pp.t =
 fun ctx -> function
  | Wrap -> Pp.string ctx "wrap"
  | No_wrap -> Pp.string ctx "nowrap"
  | Balance -> Pp.string ctx "balance"
  | Pretty -> Pp.string ctx "pretty"
  | Inherit -> Pp.string ctx "inherit"

let pp_word_break : word_break Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Break_all -> Pp.string ctx "break-all"
  | Keep_all -> Pp.string ctx "keep-all"
  | Break_word -> Pp.string ctx "break-word"
  | Inherit -> Pp.string ctx "inherit"

let pp_overflow_wrap : overflow_wrap Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Anywhere -> Pp.string ctx "anywhere"
  | Break_word -> Pp.string ctx "break-word"
  | Inherit -> Pp.string ctx "inherit"

let pp_hyphens : hyphens Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Manual -> Pp.string ctx "manual"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"

let pp_font_stretch : font_stretch Pp.t =
 fun ctx -> function
  | Pct p ->
      Pp.float ctx p;
      Pp.char ctx '%'
  | Ultra_condensed -> Pp.string ctx "ultra-condensed"
  | Extra_condensed -> Pp.string ctx "extra-condensed"
  | Condensed -> Pp.string ctx "condensed"
  | Semi_condensed -> Pp.string ctx "semi-condensed"
  | Normal -> Pp.string ctx "normal"
  | Semi_expanded -> Pp.string ctx "semi-expanded"
  | Expanded -> Pp.string ctx "expanded"
  | Extra_expanded -> Pp.string ctx "extra-expanded"
  | Ultra_expanded -> Pp.string ctx "ultra-expanded"
  | Inherit -> Pp.string ctx "inherit"

type gradient_direction =
  | To_top
  | To_top_right
  | To_right
  | To_bottom_right
  | To_bottom
  | To_bottom_left
  | To_left
  | To_top_left
  | Angle of angle

type gradient_stop =
  | Color_stop of color
  | Color_position of color * length
  | Var of color var
  | Computed_stops of
      string (* For complex computed values like --tw-gradient-stops *)

type background_image =
  | Url of string
  | Linear_gradient of gradient_direction * gradient_stop list
  | Radial_gradient of gradient_stop list
  | None

type 'a property =
  | Background_color : color property
  | Color : color property
  | Border_color : color property
  | Border_style : border_style property
  | Border_top_style : border_style property
  | Border_right_style : border_style property
  | Border_bottom_style : border_style property
  | Border_left_style : border_style property
  | Padding : length property
  | Padding_left : length property
  | Padding_right : length property
  | Padding_bottom : length property
  | Padding_top : length property
  | Padding_inline : length property
  | Padding_inline_start : length property
  | Padding_inline_end : length property
  | Padding_block : length property
  | Margin : length property
  | Margin_inline_end : length property
  | Margin_left : length property
  | Margin_right : length property
  | Margin_top : length property
  | Margin_bottom : length property
  | Margin_inline : length property
  | Margin_block : length property
  | Gap : length property
  | Column_gap : length property
  | Row_gap : length property
  | Width : length property
  | Height : length property
  | Min_width : length property
  | Min_height : length property
  | Max_width : length property
  | Max_height : length property
  | Font_size : length property
  | Line_height : line_height property
  | Font_weight : font_weight property
  | Font_style : font_style property
  | Text_align : text_align property
  | Text_decoration : text_decoration property
  | Text_decoration_style : text_decoration_style property
  | Text_decoration_color : color property
  | Text_underline_offset : string property
  | Text_transform : text_transform property
  | Letter_spacing : length property
  | List_style_type : list_style_type property
  | List_style_position : list_style_position property
  | List_style_image : list_style_image property
  | Display : display property
  | Position : position property
  | Visibility : visibility property
  | Flex_direction : flex_direction property
  | Flex_wrap : flex_wrap property
  | Flex : flex property
  | Flex_grow : float property
  | Flex_shrink : float property
  | Flex_basis : length property
  | Order : int property
  | Align_items : align_items property
  | Justify_content : justify_content property
  | Justify_items : justify property
  | Justify_self : justify property
  | Align_content : align property
  | Align_self : align_self property
  | Place_content : place_content property
  | Place_items : place_items property
  | Place_self : align_self property
  | Grid_template_columns : grid_template property
  | Grid_template_rows : grid_template property
  | Grid_template_areas : string property
  | Grid_template : grid_template property
  | Grid_area : string property
  | Grid_auto_flow : grid_auto_flow property
  | Grid_auto_columns : grid_template property
  | Grid_auto_rows : grid_template property
  | Grid_column : string property
  | Grid_row : string property
  | Grid_column_start : grid_line property
  | Grid_column_end : grid_line property
  | Grid_row_start : grid_line property
  | Grid_row_end : grid_line property
  | Border_width : length property
  | Border_top_width : length property
  | Border_right_width : length property
  | Border_bottom_width : length property
  | Border_left_width : length property
  | Border_inline_start_width : length property
  | Border_inline_end_width : length property
  | Border_radius : length property
  | Border_top_color : color property
  | Border_right_color : color property
  | Border_bottom_color : color property
  | Border_left_color : color property
  | Border_inline_start_color : color property
  | Border_inline_end_color : color property
  | Opacity : float property
  | Mix_blend_mode : blend_mode property
  | Transform : transform list property
  | Cursor : cursor property
  | Table_layout : table_layout property
  | Border_collapse : border_collapse property
  | Border_spacing : length property
  | User_select : user_select property
  | Pointer_events : pointer_events property
  | Overflow : overflow property
  | Top : length property
  | Right : length property
  | Bottom : length property
  | Left : length property
  | Z_index : z_index property
  | Outline : string property
  | Outline_style : outline_style property
  | Outline_width : length property
  | Outline_color : color property
  | Outline_offset : length property
  | Forced_color_adjust : forced_color_adjust property
  | Scroll_snap_type : scroll_snap_type property
  | White_space : white_space property
  | Border : string property
  | Tab_size : int property
  | Webkit_text_size_adjust : string property
  | Font_feature_settings : font_feature_settings property
  | Font_variation_settings : font_variation_settings property
  | Webkit_tap_highlight_color : color property
  | Webkit_text_decoration : text_decoration property
  | Webkit_text_decoration_color : color property
  | Text_indent : length property
  | List_style : string property
  | Font : string property
  | Webkit_appearance : webkit_appearance property
  | Container_type : container_type property
  | Container_name : string property
  | Perspective : length property
  | Perspective_origin : string property
  | Transform_style : transform_style property
  | Backface_visibility : backface_visibility property
  | Object_position : string property
  | Rotate : angle property
  | Transition_duration : duration property
  | Transition_timing_function : timing_function property
  | Transition_delay : duration property
  | Will_change : string property
  | Contain : contain property
  | Isolation : isolation property
  | Word_spacing : length property
  | Background_attachment : background_attachment property
  | Border_top : string property
  | Border_right : string property
  | Border_bottom : string property
  | Border_left : string property
  | Transform_origin : string property
  | Text_shadow : text_shadow property
  | Clip_path : string property
  | Mask : string property
  | Content_visibility : content_visibility property
  | Filter : filter property
  | Background_image : background_image property
  | Animation : animation property
  | Aspect_ratio : aspect_ratio property
  | Overflow_x : overflow property
  | Overflow_y : overflow property
  | Vertical_align : vertical_align property
  | Font_family : font_family list property
  | Background_position : string property
  | Background_repeat : background_repeat property
  | Background_size : background_size property
  | Webkit_font_smoothing : webkit_font_smoothing property
  | Moz_osx_font_smoothing : moz_osx_font_smoothing property
  | Webkit_line_clamp : int property
  | Webkit_box_orient : webkit_box_orient property
  | Text_overflow : text_overflow property
  | Text_wrap : text_wrap property
  | Word_break : word_break property
  | Overflow_wrap : overflow_wrap property
  | Hyphens : hyphens property
  | Webkit_hyphens : hyphens property
  | Font_stretch : font_stretch property
  | Font_variant_numeric : font_variant_numeric property
  | Backdrop_filter : filter property
  | Scroll_snap_align : scroll_snap_align property
  | Scroll_snap_stop : scroll_snap_stop property
  | Scroll_behavior : scroll_behavior property
  | Box_sizing : box_sizing property
  | Resize : resize property
  | Object_fit : object_fit property
  | Appearance : appearance property
  | Content : content property
  | Quotes : string property
  | Text_decoration_thickness : length property
  | Text_size_adjust : string property
  | Touch_action : touch_action property
  | Clip : string property
  | Clear : clear property
  | Float : float_side property
  | Scale : scale property
  | Transition : transition property
  | Box_shadow : box_shadow property
  | Fill : svg_paint property
  | Stroke : svg_paint property
  | Stroke_width : length property
  | Direction : direction property
  | Unicode_bidi : unicode_bidi property
  | Writing_mode : writing_mode property
  | Text_decoration_skip_ink : text_decoration_skip_ink property
  | Animation_name : string property
  | Animation_duration : duration property
  | Animation_timing_function : timing_function property
  | Animation_delay : duration property
  | Animation_iteration_count : animation_iteration_count property
  | Animation_direction : animation_direction property
  | Animation_fill_mode : animation_fill_mode property
  | Animation_play_state : animation_play_state property
  | Background_blend_mode : blend_mode property
  | Scroll_margin : length property
  | Scroll_margin_top : length property
  | Scroll_margin_right : length property
  | Scroll_margin_bottom : length property
  | Scroll_margin_left : length property
  | Scroll_padding : length property
  | Scroll_padding_top : length property
  | Scroll_padding_right : length property
  | Scroll_padding_bottom : length property
  | Scroll_padding_left : length property
  | Overscroll_behavior : overscroll_behavior property
  | Overscroll_behavior_x : overscroll_behavior property
  | Overscroll_behavior_y : overscroll_behavior property
  | Accent_color : color property
  | Caret_color : color property

type declaration =
  | Declaration : 'a property * 'a -> declaration
      (** A CSS property-value pair with typed value using existential type *)
  | Custom_declaration : {
      name : string;
      kind : 'a kind;
      value : 'a;
      layer : string option;
      meta : meta option;
    }
      -> declaration
      (** Custom property with dynamic name, typed value via value kind, and
          optional layer info *)
  | Important_declaration : 'a property * 'a -> declaration
      (** A CSS property-value pair marked as !important *)

(* Extract metadata from a declaration *)
let declaration_meta : declaration -> meta option = function
  | Custom_declaration { meta; _ } -> meta
  | Declaration _ -> None
  | Important_declaration _ -> None

(* Helper to mark a declaration as important - needs special handling for
   GADT *)
let important = function
  | Declaration (prop, value) -> Important_declaration (prop, value)
  | Custom_declaration { name; kind; value; layer; meta } ->
      Custom_declaration { name; kind; value; layer; meta }
  (* Custom properties remain as-is; we don't attach !important here *)
  | Important_declaration (prop, value) -> Important_declaration (prop, value)
(* Already important *)

(* Helper for raw custom properties - primarily for internal use *)
let custom_property ?layer name value =
  (* Validate that this is a proper CSS variable name *)
  if not (String.length name > 2 && String.sub name 0 2 = "--") then
    failwith
      (String.concat ""
         [
           "custom_property: ";
           name;
           " is not a valid CSS variable name (must start with --)";
         ]);
  Custom_declaration { name; kind = String; value; layer; meta = None }

(* Access the layer associated with a custom declaration, if any *)
let custom_declaration_layer = function
  | Custom_declaration { layer; _ } -> layer
  | Declaration _ -> None
  | Important_declaration _ -> None

let pp_gradient_direction : gradient_direction Pp.t =
 fun ctx -> function
  | To_top -> Pp.string ctx "to top"
  | To_top_right -> Pp.string ctx "to top right"
  | To_right -> Pp.string ctx "to right"
  | To_bottom_right -> Pp.string ctx "to bottom right"
  | To_bottom -> Pp.string ctx "to bottom"
  | To_bottom_left -> Pp.string ctx "to bottom left"
  | To_left -> Pp.string ctx "to left"
  | To_top_left -> Pp.string ctx "to top left"
  | Angle a -> pp_angle ctx a

let pp_gradient_stop : gradient_stop Pp.t =
 fun ctx -> function
  | Color_stop c -> pp_color ctx c
  | Color_position (c, len) ->
      pp_color ctx c;
      Pp.space ctx ();
      pp_length ctx len
  | Var v -> pp_var pp_color ctx v
  | Computed_stops s -> Pp.string ctx s

let rec pp_number : number Pp.t =
 fun ctx -> function
  | Float f -> Pp.float ctx f
  | Int i -> Pp.int ctx i
  | Pct p ->
      Pp.float ctx p;
      Pp.char ctx '%'
  | Var v -> pp_var pp_number ctx v

let rec pp_filter : filter Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Blur l -> pp_fun "blur" pp_length ctx l
  | Brightness n -> pp_fun "brightness" pp_number ctx n
  | Contrast n -> pp_fun "contrast" pp_number ctx n
  | Drop_shadow s -> pp_fun "drop-shadow" pp_shadow ctx s
  | Grayscale n -> pp_fun "grayscale" pp_number ctx n
  | Hue_rotate a -> pp_fun "hue-rotate" pp_angle ctx a
  | Invert n -> pp_fun "invert" pp_number ctx n
  | Opacity n -> pp_fun "opacity" pp_number ctx n
  | Saturate n -> pp_fun "saturate" pp_number ctx n
  | Sepia n -> pp_fun "sepia" pp_number ctx n
  | Url url -> pp_fun "url" Pp.string ctx url
  | List filters -> Pp.list ~sep:Pp.space pp_filter ctx filters
  | Var v -> pp_var pp_filter ctx v

let pp_background_image : background_image Pp.t =
 fun ctx -> function
  | Url url ->
      Pp.string ctx "url(\"";
      Pp.string ctx url;
      Pp.string ctx "\")"
  | Linear_gradient (dir, stops) ->
      Pp.string ctx "linear-gradient(";
      pp_gradient_direction ctx dir;
      (match stops with
      | [] -> ()
      | _ ->
          Pp.string ctx ", ";
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops);
      Pp.char ctx ')'
  | Radial_gradient stops ->
      Pp.string ctx "radial-gradient(";
      (match stops with
      | [] -> ()
      | _ -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops);
      Pp.char ctx ')'
  | None -> Pp.string ctx "none"

let rec pp_font_family : font_family Pp.t =
 fun ctx -> function
  (* Generic CSS font families *)
  | Sans_serif -> Pp.string ctx "sans-serif"
  | Serif -> Pp.string ctx "serif"
  | Monospace -> Pp.string ctx "monospace"
  | Cursive -> Pp.string ctx "cursive"
  | Fantasy -> Pp.string ctx "fantasy"
  | System_ui -> Pp.string ctx "system-ui"
  | Ui_sans_serif -> Pp.string ctx "ui-sans-serif"
  | Ui_serif -> Pp.string ctx "ui-serif"
  | Ui_monospace -> Pp.string ctx "ui-monospace"
  | Ui_rounded -> Pp.string ctx "ui-rounded"
  | Emoji -> Pp.string ctx "emoji"
  | Math -> Pp.string ctx "math"
  | Fangsong -> Pp.string ctx "fangsong"
  (* Popular web fonts *)
  | Inter -> Pp.string ctx "Inter"
  | Roboto -> Pp.string ctx "Roboto"
  | Open_sans -> Pp.string ctx "\"Open Sans\""
  | Lato -> Pp.string ctx "Lato"
  | Montserrat -> Pp.string ctx "Montserrat"
  | Poppins -> Pp.string ctx "Poppins"
  | Source_sans_pro -> Pp.string ctx "\"Source Sans Pro\""
  | Raleway -> Pp.string ctx "Raleway"
  | Oswald -> Pp.string ctx "Oswald"
  | Noto_sans -> Pp.string ctx "\"Noto Sans\""
  | Ubuntu -> Pp.string ctx "Ubuntu"
  | Playfair_display -> Pp.string ctx "\"Playfair Display\""
  | Merriweather -> Pp.string ctx "Merriweather"
  | Lora -> Pp.string ctx "Lora"
  | PT_sans -> Pp.string ctx "\"PT Sans\""
  | PT_serif -> Pp.string ctx "\"PT Serif\""
  | Nunito -> Pp.string ctx "Nunito"
  | Nunito_sans -> Pp.string ctx "\"Nunito Sans\""
  | Work_sans -> Pp.string ctx "\"Work Sans\""
  | Rubik -> Pp.string ctx "Rubik"
  | Fira_sans -> Pp.string ctx "\"Fira Sans\""
  | Fira_code -> Pp.string ctx "\"Fira Code\""
  | JetBrains_mono -> Pp.string ctx "\"JetBrains Mono\""
  | IBM_plex_sans -> Pp.string ctx "\"IBM Plex Sans\""
  | IBM_plex_serif -> Pp.string ctx "\"IBM Plex Serif\""
  | IBM_plex_mono -> Pp.string ctx "\"IBM Plex Mono\""
  | Source_code_pro -> Pp.string ctx "\"Source Code Pro\""
  | Space_mono -> Pp.string ctx "\"Space Mono\""
  | DM_sans -> Pp.string ctx "\"DM Sans\""
  | DM_serif_display -> Pp.string ctx "\"DM Serif Display\""
  | Bebas_neue -> Pp.string ctx "\"Bebas Neue\""
  | Barlow -> Pp.string ctx "Barlow"
  | Mulish -> Pp.string ctx "Mulish"
  | Josefin_sans -> Pp.string ctx "\"Josefin Sans\""
  (* Platform-specific fonts *)
  | Helvetica -> Pp.string ctx "Helvetica"
  | Helvetica_neue -> Pp.string ctx "\"Helvetica Neue\""
  | Arial -> Pp.string ctx "Arial"
  | Verdana -> Pp.string ctx "Verdana"
  | Tahoma -> Pp.string ctx "Tahoma"
  | Trebuchet_ms -> Pp.string ctx "\"Trebuchet MS\""
  | Times_new_roman -> Pp.string ctx "\"Times New Roman\""
  | Times -> Pp.string ctx "Times"
  | Georgia -> Pp.string ctx "Georgia"
  | Cambria -> Pp.string ctx "Cambria"
  | Garamond -> Pp.string ctx "Garamond"
  | Courier_new -> Pp.string ctx "\"Courier New\""
  | Courier -> Pp.string ctx "Courier"
  | Lucida_console -> Pp.string ctx "\"Lucida Console\""
  | SF_pro -> Pp.string ctx "\"SF Pro\""
  | SF_pro_display -> Pp.string ctx "\"SF Pro Display\""
  | SF_pro_text -> Pp.string ctx "\"SF Pro Text\""
  | SF_mono -> Pp.string ctx "\"SF Mono\""
  | NY -> Pp.string ctx "\"New York\""
  | Segoe_ui -> Pp.string ctx "\"Segoe UI\""
  | Segoe_ui_emoji -> Pp.string ctx "\"Segoe UI Emoji\""
  | Segoe_ui_symbol -> Pp.string ctx "\"Segoe UI Symbol\""
  | Apple_color_emoji -> Pp.string ctx "\"Apple Color Emoji\""
  | Noto_color_emoji -> Pp.string ctx "\"Noto Color Emoji\""
  | Android_emoji -> Pp.string ctx "\"Android Emoji\""
  | Twemoji_mozilla -> Pp.string ctx "\"Twemoji Mozilla\""
  (* Developer fonts *)
  | Menlo -> Pp.string ctx "Menlo"
  | Monaco -> Pp.string ctx "Monaco"
  | Consolas -> Pp.string ctx "Consolas"
  | Liberation_mono -> Pp.string ctx "\"Liberation Mono\""
  | SFMono_regular -> Pp.string ctx "SFMono-Regular"
  | Cascadia_code -> Pp.string ctx "\"Cascadia Code\""
  | Cascadia_mono -> Pp.string ctx "\"Cascadia Mono\""
  | Victor_mono -> Pp.string ctx "\"Victor Mono\""
  | Inconsolata -> Pp.string ctx "Inconsolata"
  | Hack -> Pp.string ctx "Hack"
  (* CSS keywords *)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Var v -> pp_var (Pp.list ~sep:Pp.comma pp_font_family) ctx v

let pp_value : type a. (a kind * a) Pp.t =
 fun ctx (kind, value) ->
  let pp pp_a = pp_a ctx value in
  match kind with
  | Length -> pp pp_length
  | Color -> pp pp_color
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | String -> pp Pp.string
  | Shadow -> pp pp_shadow
  | Duration -> pp pp_duration
  | Aspect_ratio -> pp pp_aspect_ratio
  | Border_style -> pp pp_border_style
  | Font_weight -> pp pp_font_weight
  | Font_family -> pp (Pp.list ~sep:Pp.comma pp_font_family)
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Font_variant_numeric -> pp pp_font_variant_numeric
  | Font_variant_numeric_token -> pp pp_font_variant_token
  | Blend_mode -> pp pp_blend_mode
  | Scroll_snap_strictness -> pp pp_scroll_snap_strictness
  | Angle -> pp pp_angle
  | Transform_scale -> pp pp_transform_scale
  | Box_shadow -> pp pp_box_shadow
  | Content -> pp pp_content

let pp_property_value : type a. (a property * a) Pp.t =
 fun ctx (prop, value) ->
  let pp pp_a = pp_a ctx value in
  match prop with
  | Background_color -> pp pp_color
  | Color -> pp pp_color
  | Border_color -> pp pp_color
  | Border_style -> pp pp_border_style
  | Border_top_style -> pp pp_border_style
  | Border_right_style -> pp pp_border_style
  | Border_bottom_style -> pp pp_border_style
  | Border_left_style -> pp pp_border_style
  | Padding -> pp pp_length
  | Padding_left -> pp pp_length
  | Padding_right -> pp pp_length
  | Padding_bottom -> pp pp_length
  | Padding_top -> pp pp_length
  | Padding_inline -> pp pp_length
  | Padding_inline_start -> pp pp_length
  | Padding_inline_end -> pp pp_length
  | Padding_block -> pp pp_length
  | Margin -> pp pp_length
  | Margin_inline_end -> pp pp_length
  | Margin_left -> pp pp_length
  | Margin_right -> pp pp_length
  | Margin_top -> pp pp_length
  | Margin_bottom -> pp pp_length
  | Margin_inline -> pp pp_length
  | Margin_block -> pp pp_length
  | Gap -> pp pp_length
  | Column_gap -> pp pp_length
  | Row_gap -> pp pp_length
  | Width -> pp pp_length
  | Height -> pp pp_length
  | Min_width -> pp pp_length
  | Min_height -> pp pp_length
  | Max_width -> pp pp_length
  | Max_height -> pp pp_length
  | Font_size -> pp pp_length
  | Line_height -> pp pp_line_height
  | Font_weight -> pp pp_font_weight
  | Display -> pp pp_display
  | Position -> pp pp_position
  | Visibility -> pp pp_visibility
  | Align_items -> pp pp_align_items
  | Justify_content -> pp pp_justify_content
  | Justify_items -> pp pp_justify
  | Align_self -> pp pp_align_self
  | Border_collapse -> pp pp_border_collapse
  | Table_layout -> pp pp_table_layout
  | Grid_auto_flow -> pp pp_grid_auto_flow
  | Opacity -> pp Pp.float
  | Mix_blend_mode -> pp pp_blend_mode
  | Z_index -> pp pp_z_index
  | Tab_size -> pp Pp.int
  | Webkit_line_clamp -> pp Pp.int
  | Webkit_box_orient -> pp pp_webkit_box_orient
  | Top -> pp pp_length
  | Right -> pp pp_length
  | Bottom -> pp pp_length
  | Left -> pp pp_length
  | Border_width -> pp pp_length
  | Border_top_width -> pp pp_length
  | Border_right_width -> pp pp_length
  | Border_bottom_width -> pp pp_length
  | Border_left_width -> pp pp_length
  | Border_inline_start_width -> pp pp_length
  | Border_inline_end_width -> pp pp_length
  | Border_radius -> pp pp_length
  | Border_top_color -> pp pp_color
  | Border_right_color -> pp pp_color
  | Border_bottom_color -> pp pp_color
  | Border_left_color -> pp pp_color
  | Border_inline_start_color -> pp pp_color
  | Border_inline_end_color -> pp pp_color
  | Text_decoration_color -> pp pp_color
  | Webkit_text_decoration_color -> pp pp_color
  | Webkit_tap_highlight_color -> pp pp_color
  | Text_indent -> pp pp_length
  | Border_spacing -> pp pp_length
  | Outline_offset -> pp pp_length
  | Perspective -> pp pp_length
  | Transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Isolation -> pp pp_isolation
  | Transform_style -> pp pp_transform_style
  | Backface_visibility -> pp pp_backface_visibility
  | Scroll_snap_align -> pp pp_scroll_snap_align
  | Scroll_snap_stop -> pp pp_scroll_snap_stop
  | Scroll_behavior -> pp pp_scroll_behavior
  | Box_sizing -> pp pp_box_sizing
  | Resize -> pp pp_resize
  | Object_fit -> pp pp_object_fit
  | Appearance -> pp pp_appearance
  | Flex_grow -> pp Pp.float
  | Flex_shrink -> pp Pp.float
  | Order -> pp Pp.int
  | Flex_direction -> pp pp_flex_direction
  | Flex_wrap -> pp pp_flex_wrap
  | Font_style -> pp pp_font_style
  | Text_align -> pp pp_text_align
  | Text_decoration -> pp pp_text_decoration
  | Text_decoration_style -> pp pp_text_decoration_style
  | Text_transform -> pp pp_text_transform
  | List_style_type -> pp pp_list_style_type
  | List_style_position -> pp pp_list_style_position
  | List_style_image -> pp pp_list_style_image
  | Overflow -> pp pp_overflow
  | Overflow_x -> pp pp_overflow
  | Overflow_y -> pp pp_overflow
  | Vertical_align -> pp pp_vertical_align
  | Text_overflow -> pp pp_text_overflow
  | Text_wrap -> pp pp_text_wrap
  | Word_break -> pp pp_word_break
  | Overflow_wrap -> pp pp_overflow_wrap
  | Hyphens -> pp pp_hyphens
  | Webkit_hyphens -> pp pp_hyphens
  | Font_stretch -> pp pp_font_stretch
  | Font_variant_numeric -> pp pp_font_variant_numeric
  | Webkit_font_smoothing -> pp pp_webkit_font_smoothing
  | Scroll_snap_type -> pp pp_scroll_snap_type
  | Container_type -> pp pp_container_type
  | White_space -> pp pp_white_space
  | Grid_template_columns -> pp pp_grid_template
  | Grid_template_rows -> pp pp_grid_template
  | Grid_template_areas -> pp Pp.string
  | Grid_template -> pp pp_grid_template
  | Grid_area -> pp Pp.string
  | Grid_auto_columns -> pp pp_grid_template
  | Grid_auto_rows -> pp pp_grid_template
  (* String properties *)
  | Flex -> pp pp_flex
  | Flex_basis -> pp pp_length
  | Align_content -> pp pp_align
  | Justify_self -> pp pp_justify
  | Place_content -> pp pp_place_content
  | Place_items -> pp pp_place_items
  | Place_self -> pp pp_align_self
  | Grid_column -> pp Pp.string
  | Grid_row -> pp Pp.string
  | Grid_column_start -> pp pp_grid_line
  | Grid_column_end -> pp pp_grid_line
  | Grid_row_start -> pp pp_grid_line
  | Grid_row_end -> pp pp_grid_line
  | Text_underline_offset -> pp Pp.string
  | Background_position -> pp Pp.string
  | Background_repeat -> pp pp_background_repeat
  | Background_size -> pp pp_background_size
  | Moz_osx_font_smoothing -> pp pp_moz_osx_font_smoothing
  | Backdrop_filter -> pp pp_filter
  | Container_name -> pp Pp.string
  | Perspective_origin -> pp Pp.string
  | Object_position -> pp Pp.string
  | Rotate -> pp pp_angle
  | Transition_duration -> pp pp_duration
  | Transition_timing_function -> pp pp_timing_function
  | Transition_delay -> pp pp_duration
  | Will_change -> pp Pp.string
  | Contain -> pp pp_contain
  | Word_spacing -> pp pp_length
  | Background_attachment -> pp pp_background_attachment
  | Border_top -> pp Pp.string
  | Border_right -> pp Pp.string
  | Border_bottom -> pp Pp.string
  | Border_left -> pp Pp.string
  | Transform_origin -> pp Pp.string
  | Text_shadow -> pp pp_text_shadow
  | Clip_path -> pp Pp.string
  | Mask -> pp Pp.string
  | Content_visibility -> pp pp_content_visibility
  | Filter -> pp pp_filter
  | Background_image -> pp pp_background_image
  | Animation -> pp pp_animation
  | Aspect_ratio -> pp pp_aspect_ratio
  | Content -> pp pp_content
  | Quotes -> pp Pp.string
  | Box_shadow -> pp pp_box_shadow
  | Fill -> pp pp_svg_paint
  | Stroke -> pp pp_svg_paint
  | Stroke_width -> pp pp_length
  | Transition -> pp pp_transition
  | Scale -> pp pp_scale
  | Outline -> pp Pp.string
  | Outline_style -> pp pp_outline_style
  | Outline_width -> pp pp_length
  | Outline_color -> pp pp_color
  | Forced_color_adjust -> pp pp_forced_color_adjust
  | Clip -> pp Pp.string
  | Clear -> pp pp_clear
  | Float -> pp pp_float_side
  | Border -> pp Pp.string
  | Text_decoration_thickness -> pp pp_length
  | Text_size_adjust -> pp Pp.string
  | Touch_action -> pp pp_touch_action
  | Direction -> pp pp_direction
  | Unicode_bidi -> pp pp_unicode_bidi
  | Writing_mode -> pp pp_writing_mode
  | Text_decoration_skip_ink -> pp pp_text_decoration_skip_ink
  | Animation_name -> pp Pp.string
  | Animation_duration -> pp pp_duration
  | Animation_timing_function -> pp pp_timing_function
  | Animation_delay -> pp pp_duration
  | Animation_iteration_count -> pp pp_animation_iteration_count
  | Animation_direction -> pp pp_animation_direction
  | Animation_fill_mode -> pp pp_animation_fill_mode
  | Animation_play_state -> pp pp_animation_play_state
  | Background_blend_mode -> pp pp_blend_mode
  | Scroll_margin -> pp pp_length
  | Scroll_margin_top -> pp pp_length
  | Scroll_margin_right -> pp pp_length
  | Scroll_margin_bottom -> pp pp_length
  | Scroll_margin_left -> pp pp_length
  | Scroll_padding -> pp pp_length
  | Scroll_padding_top -> pp pp_length
  | Scroll_padding_right -> pp pp_length
  | Scroll_padding_bottom -> pp pp_length
  | Scroll_padding_left -> pp pp_length
  | Overscroll_behavior -> pp pp_overscroll_behavior
  | Overscroll_behavior_x -> pp pp_overscroll_behavior
  | Overscroll_behavior_y -> pp pp_overscroll_behavior
  | Accent_color -> pp pp_color
  | Caret_color -> pp pp_color
  | List_style -> pp Pp.string
  | Font -> pp Pp.string
  | Webkit_appearance -> pp pp_webkit_appearance
  | Letter_spacing -> pp pp_length
  | Cursor -> pp pp_cursor
  | Pointer_events -> pp pp_pointer_events
  | User_select -> pp pp_user_select
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Webkit_text_decoration -> pp pp_text_decoration
  | Webkit_text_size_adjust -> pp Pp.string
  | Font_family -> pp (Pp.list ~sep:Pp.comma pp_font_family)

(* Typed variable setters *)
let var : type a.
    ?fallback:a ->
    ?layer:string ->
    ?meta:meta ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?fallback ?layer ?meta name kind value ->
  let declaration =
    Custom_declaration
      { name = String.concat "" [ "--"; name ]; kind; value; layer; meta }
  in
  let var_handle = { name; fallback; default = Some value; layer; meta } in
  (declaration, var_handle)

(* Property constructors with typed values *)
let background_color c = Declaration (Background_color, c)
let color c = Declaration (Color, c)
let border_color c = Declaration (Border_color, c)
let border_style bs = Declaration (Border_style, bs)
let border_top_style bs = Declaration (Border_top_style, bs)
let border_right_style bs = Declaration (Border_right_style, bs)
let border_bottom_style bs = Declaration (Border_bottom_style, bs)
let border_left_style bs = Declaration (Border_left_style, bs)
let text_decoration td = Declaration (Text_decoration, td)
let font_style fs = Declaration (Font_style, fs)
let list_style_type lst = Declaration (List_style_type, lst)
let list_style_position v = Declaration (List_style_position, v)
let list_style_image v = Declaration (List_style_image, v)
let padding len = Declaration (Padding, len)
let padding_left len = Declaration (Padding_left, len)
let padding_right len = Declaration (Padding_right, len)
let padding_bottom len = Declaration (Padding_bottom, len)
let padding_top len = Declaration (Padding_top, len)

(* Remove deprecated string-based versions *)
let margin len = Declaration (Margin, len)
let margin_left len = Declaration (Margin_left, len)
let margin_right len = Declaration (Margin_right, len)
let margin_top len = Declaration (Margin_top, len)
let margin_bottom len = Declaration (Margin_bottom, len)

(* Remove deprecated string-based versions *)
let gap len = Declaration (Gap, len)
let column_gap len = Declaration (Column_gap, len)
let row_gap len = Declaration (Row_gap, len)

(* Grid functions *)
let grid_template_areas template = Declaration (Grid_template_areas, template)
let grid_template template = Declaration (Grid_template, template)
let grid_auto_columns size = Declaration (Grid_auto_columns, size)
let grid_auto_rows size = Declaration (Grid_auto_rows, size)
let grid_row_start value = Declaration (Grid_row_start, value)
let grid_row_end value = Declaration (Grid_row_end, value)
let grid_column_start value = Declaration (Grid_column_start, value)
let grid_column_end value = Declaration (Grid_column_end, value)

let grid_row (start, end_) =
  let grid_line_to_string = function
    | Auto -> "auto"
    | Line_number n -> string_of_int n
    | Line_name s -> s
    | Span n -> Printf.sprintf "span %d" n
  in
  let value =
    Printf.sprintf "%s / %s"
      (grid_line_to_string start)
      (grid_line_to_string end_)
  in
  Declaration (Grid_row, value)

let grid_column (start, end_) =
  let grid_line_to_string = function
    | Auto -> "auto"
    | Line_number n -> string_of_int n
    | Line_name s -> s
    | Span n -> Printf.sprintf "span %d" n
  in
  let value =
    Printf.sprintf "%s / %s"
      (grid_line_to_string start)
      (grid_line_to_string end_)
  in
  Declaration (Grid_column, value)

let grid_area value = Declaration (Grid_area, value)
let width len = Declaration (Width, len)
let height len = Declaration (Height, len)

(* Remove deprecated string-based versions *)
let min_width len = Declaration (Min_width, len)
let min_height len = Declaration (Min_height, len)
let max_width len = Declaration (Max_width, len)
let max_height len = Declaration (Max_height, len)
let font_size len = Declaration (Font_size, len)
let line_height len = Declaration (Line_height, len)
let font_weight w = Declaration (Font_weight, w)
let text_align a = Declaration (Text_align, a)
let text_decoration_style value = Declaration (Text_decoration_style, value)
let text_underline_offset value = Declaration (Text_underline_offset, value)
let text_transform value = Declaration (Text_transform, value)
let letter_spacing len = Declaration (Letter_spacing, len)
let white_space value = Declaration (White_space, value)
let display d = Declaration (Display, d)
let position p = Declaration (Position, p)
let visibility v = Declaration (Visibility, v)
let top len = Declaration (Top, len)
let right len = Declaration (Right, len)
let bottom len = Declaration (Bottom, len)
let left len = Declaration (Left, len)
let opacity value = Declaration (Opacity, value)

(* Remove deprecated string-based versions *)
let flex_direction d = Declaration (Flex_direction, d)
let flex value = Declaration (Flex, value)
let flex_grow value = Declaration (Flex_grow, value)
let flex_shrink value = Declaration (Flex_shrink, value)
let flex_basis value = Declaration (Flex_basis, value)
let flex_wrap value = Declaration (Flex_wrap, value)
let order value = Declaration (Order, value)
let align_items a = Declaration (Align_items, a)
let align_content a = Declaration (Align_content, a)
let align_self a = Declaration (Align_self, a)
let justify_content a = Declaration (Justify_content, a)
let justify_items a = Declaration (Justify_items, a)
let justify_self a = Declaration (Justify_self, a)
let place_content value = Declaration (Place_content, value)
let place_items value = Declaration (Place_items, value)
let place_self value = Declaration (Place_self, value)
let border_width len = Declaration (Border_width, len)
let border_radius len = Declaration (Border_radius, len)
let box_shadow value = Declaration (Box_shadow, value)
let fill value = Declaration (Fill, value)
let stroke value = Declaration (Stroke, value)
let stroke_width value = Declaration (Stroke_width, value)
let outline_style v = Declaration (Outline_style, v)
let outline_width len = Declaration (Outline_width, len)
let outline_color c = Declaration (Outline_color, c)
let forced_color_adjust v = Declaration (Forced_color_adjust, v)
let table_layout value = Declaration (Table_layout, value)
let border_spacing len = Declaration (Border_spacing, len)
let overflow o = Declaration (Overflow, o)
let object_fit value = Declaration (Object_fit, value)
let clip value = Declaration (Clip, value)
let clear value = Declaration (Clear, value)
let float value = Declaration (Float, value)
let touch_action value = Declaration (Touch_action, value)
let direction value = Declaration (Direction, value)
let unicode_bidi value = Declaration (Unicode_bidi, value)
let writing_mode value = Declaration (Writing_mode, value)

let text_decoration_skip_ink value =
  Declaration (Text_decoration_skip_ink, value)

let animation_name value = Declaration (Animation_name, value)
let animation_duration value = Declaration (Animation_duration, value)

let animation_timing_function value =
  Declaration (Animation_timing_function, value)

let animation_delay value = Declaration (Animation_delay, value)

let animation_iteration_count value =
  Declaration (Animation_iteration_count, value)

let animation_direction value = Declaration (Animation_direction, value)
let animation_fill_mode value = Declaration (Animation_fill_mode, value)
let animation_play_state value = Declaration (Animation_play_state, value)
let background_blend_mode value = Declaration (Background_blend_mode, value)
let scroll_margin value = Declaration (Scroll_margin, value)
let scroll_margin_top value = Declaration (Scroll_margin_top, value)
let scroll_margin_right value = Declaration (Scroll_margin_right, value)
let scroll_margin_bottom value = Declaration (Scroll_margin_bottom, value)
let scroll_margin_left value = Declaration (Scroll_margin_left, value)
let scroll_padding value = Declaration (Scroll_padding, value)
let scroll_padding_top value = Declaration (Scroll_padding_top, value)
let scroll_padding_right value = Declaration (Scroll_padding_right, value)
let scroll_padding_bottom value = Declaration (Scroll_padding_bottom, value)
let scroll_padding_left value = Declaration (Scroll_padding_left, value)
let overscroll_behavior value = Declaration (Overscroll_behavior, value)
let overscroll_behavior_x value = Declaration (Overscroll_behavior_x, value)
let overscroll_behavior_y value = Declaration (Overscroll_behavior_y, value)
let accent_color value = Declaration (Accent_color, value)
let caret_color value = Declaration (Caret_color, value)
let text_decoration_color value = Declaration (Text_decoration_color, value)

let text_decoration_thickness value =
  Declaration (Text_decoration_thickness, value)

let text_size_adjust value = Declaration (Text_size_adjust, value)
let aspect_ratio v = Declaration (Aspect_ratio, v)
let filter value = Declaration (Filter, value)

(* Gradient direction values *)
(* Background image values *)

let background_image value = Declaration (Background_image, value)

(* Helper functions for background images *)
let url path = Url path
let linear_gradient dir stops = Linear_gradient (dir, stops)
let radial_gradient stops = Radial_gradient stops

(* Helper functions for gradient stops *)
let color_stop c = Color_stop c
let color_position c pos = Color_position (c, pos)

let make_animation ?name ?duration ?timing_function ?delay ?iteration_count
    ?direction ?fill_mode ?play_state () =
  {
    name;
    duration;
    timing_function;
    delay;
    iteration_count;
    direction;
    fill_mode;
    play_state;
  }

let animation value = Declaration (Animation, value)

(* Blend modes *)

let mix_blend_mode v = Declaration (Mix_blend_mode, v)
let grid_template_columns value = Declaration (Grid_template_columns, value)
let grid_template_rows value = Declaration (Grid_template_rows, value)
let grid_auto_flow value = Declaration (Grid_auto_flow, value)
let pointer_events value = Declaration (Pointer_events, value)
let z_index value = Declaration (Z_index, value)
let z_index_auto = Declaration (Z_index, Auto)
let appearance value = Declaration (Appearance, value)
let overflow_x o = Declaration (Overflow_x, o)
let overflow_y o = Declaration (Overflow_y, o)
let resize value = Declaration (Resize, value)
let vertical_align value = Declaration (Vertical_align, value)
let box_sizing value = Declaration (Box_sizing, value)
let font_family fonts = Declaration (Font_family, fonts)

(* Additional missing functions *)
let word_spacing value = Declaration (Word_spacing, value)
let background_attachment value = Declaration (Background_attachment, value)
let border_top value = Declaration (Border_top, value)
let border_right value = Declaration (Border_right, value)
let border_bottom value = Declaration (Border_bottom, value)
let border_left value = Declaration (Border_left, value)
let transform_origin value = Declaration (Transform_origin, value)
let text_shadow value = Declaration (Text_shadow, value)
let clip_path value = Declaration (Clip_path, value)
let mask value = Declaration (Mask, value)
let content_visibility value = Declaration (Content_visibility, value)
let moz_osx_font_smoothing value = Declaration (Moz_osx_font_smoothing, value)
let webkit_line_clamp value = Declaration (Webkit_line_clamp, value)
let webkit_box_orient value = Declaration (Webkit_box_orient, value)
let text_overflow v = Declaration (Text_overflow, v)
let text_wrap v = Declaration (Text_wrap, v)
let word_break v = Declaration (Word_break, v)
let overflow_wrap v = Declaration (Overflow_wrap, v)
let hyphens v = Declaration (Hyphens, v)
let webkit_hyphens v = Declaration (Webkit_hyphens, v)
let font_stretch v = Declaration (Font_stretch, v)
let font_variant_numeric v = Declaration (Font_variant_numeric, v)
let font_variant_numeric_tokens tokens = Tokens tokens

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

let backdrop_filter value = Declaration (Backdrop_filter, value)
let background_position value = Declaration (Background_position, value)
let background_repeat value = Declaration (Background_repeat, value)
let background_size value = Declaration (Background_size, value)

(* Additional property constructors *)
let content value = Declaration (Content, value)
let border_left_width len = Declaration (Border_left_width, len)
let border_inline_start_width len = Declaration (Border_inline_start_width, len)
let border_inline_end_width len = Declaration (Border_inline_end_width, len)
let border_bottom_width len = Declaration (Border_bottom_width, len)
let border_top_width len = Declaration (Border_top_width, len)
let border_right_width len = Declaration (Border_right_width, len)
let border_top_color c = Declaration (Border_top_color, c)
let border_right_color c = Declaration (Border_right_color, c)
let border_bottom_color c = Declaration (Border_bottom_color, c)
let border_left_color c = Declaration (Border_left_color, c)
let border_inline_start_color c = Declaration (Border_inline_start_color, c)
let border_inline_end_color c = Declaration (Border_inline_end_color, c)
let transition value = Declaration (Transition, value)
let quotes value = Declaration (Quotes, value)

(* New property constructors for tw.ml *)
let border value = Declaration (Border, value)
let tab_size value = Declaration (Tab_size, value)
let webkit_text_size_adjust value = Declaration (Webkit_text_size_adjust, value)
let font_feature_settings value = Declaration (Font_feature_settings, value)
let font_variation_settings value = Declaration (Font_variation_settings, value)

let webkit_tap_highlight_color value =
  Declaration (Webkit_tap_highlight_color, value)

let webkit_text_decoration value = Declaration (Webkit_text_decoration, value)

let webkit_text_decoration_color value =
  Declaration (Webkit_text_decoration_color, value)

let text_indent len = Declaration (Text_indent, len)
let border_collapse value = Declaration (Border_collapse, value)
let list_style value = Declaration (List_style, value)
let font value = Declaration (Font, value)
let webkit_appearance value = Declaration (Webkit_appearance, value)
let webkit_font_smoothing value = Declaration (Webkit_font_smoothing, value)
let cursor c = Declaration (Cursor, c)
let user_select u = Declaration (User_select, u)
let container_type value = Declaration (Container_type, value)
let container_name value = Declaration (Container_name, value)
let transform values = Declaration (Transform, values)
let rotate a = Declaration (Rotate, a)
let scale value = Declaration (Scale, value)
let perspective len = Declaration (Perspective, len)
let perspective_origin value = Declaration (Perspective_origin, value)
let transform_style value = Declaration (Transform_style, value)
let backface_visibility value = Declaration (Backface_visibility, value)
let object_position value = Declaration (Object_position, value)
let transition_duration value = Declaration (Transition_duration, value)

let transition_timing_function value =
  Declaration (Transition_timing_function, value)

let transition_delay value = Declaration (Transition_delay, value)
let will_change value = Declaration (Will_change, value)
let contain value = Declaration (Contain, value)
let isolation value = Declaration (Isolation, value)
let padding_inline len = Declaration (Padding_inline, len)
let padding_inline_start len = Declaration (Padding_inline_start, len)
let padding_inline_end len = Declaration (Padding_inline_end, len)
let padding_block len = Declaration (Padding_block, len)
let margin_inline len = Declaration (Margin_inline, len)
let margin_block len = Declaration (Margin_block, len)
let margin_inline_end len = Declaration (Margin_inline_end, len)
let outline value = Declaration (Outline, value)
let outline_offset len = Declaration (Outline_offset, len)
let scroll_snap_type value = Declaration (Scroll_snap_type, value)
let scroll_snap_align value = Declaration (Scroll_snap_align, value)
let scroll_snap_stop value = Declaration (Scroll_snap_stop, value)
let scroll_behavior value = Declaration (Scroll_behavior, value)

(* Selector module for structured selector representation *)
module Selector = struct
  type attribute_match =
    | Presence
    | Exact of string
    | Whitespace_list of string
    | Hyphen_list of string
    | Prefix of string
    | Suffix of string
    | Substring of string

  type combinator = Descendant | Child | Next_sibling | Subsequent_sibling

  type t =
    | Element of string
    | Class of string
    | Id of string
    | Universal
    | Attribute of string * attribute_match
    | Pseudo_class of string
    | Pseudo_element of string
    | Where of t list
    | Not of t list
    | Fun of string * t list
    | Compound of t list (* Compound selector like div.class#id *)
    | Combined of t * combinator * t (* Selectors with combinators *)
    | List of t list (* Comma-separated list of selectors *)

  let combinator_to_string ~minify = function
    | Descendant -> " "
    | Child -> if minify then ">" else " > "
    | Next_sibling -> if minify then "+" else " + "
    | Subsequent_sibling -> if minify then "~" else " ~ "

  (* CSS identifier validation functions *)
  let is_valid_nmstart c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || c = '_'
    || Char.code c > 127
    || c = '\\'

  let is_valid_nmchar c =
    is_valid_nmstart c || (c >= '0' && c <= '9') || c = '-'

  let needs_quotes value =
    String.length value = 0
    || (not (is_valid_nmstart value.[0]))
    || not (String.for_all is_valid_nmchar value)

  let pp_attribute_value ~minify value =
    (* Deterministic rule: minified = no quotes unless needed; pretty = always
       quotes *)
    if minify && not (needs_quotes value) then value
    else String.concat "" [ "\""; value; "\"" ]

  let pp_attribute_match ~minify name = function
    | Presence -> name
    | Exact value ->
        String.concat "" [ name; "="; pp_attribute_value ~minify value ]
    | Whitespace_list value ->
        String.concat "" [ name; "~="; pp_attribute_value ~minify value ]
    | Hyphen_list value ->
        String.concat "" [ name; "|="; pp_attribute_value ~minify value ]
    | Prefix value ->
        String.concat "" [ name; "^="; pp_attribute_value ~minify value ]
    | Suffix value ->
        String.concat "" [ name; "$="; pp_attribute_value ~minify value ]
    | Substring value ->
        String.concat "" [ name; "*="; pp_attribute_value ~minify value ]

  let rec to_string ?(minify = false) = function
    | Element e -> e
    | Class c -> String.concat "" [ "."; c ]
    | Id i -> String.concat "" [ "#"; i ]
    | Universal -> "*"
    | Attribute (name, match_type) ->
        String.concat ""
          [ "["; pp_attribute_match ~minify name match_type; "]" ]
    | Pseudo_class pc -> String.concat "" [ ":"; pc ]
    | Pseudo_element pe -> String.concat "" [ "::"; pe ]
    | Where selectors ->
        let sep = if minify then "," else ", " in
        String.concat ""
          [
            ":where(";
            String.concat sep (List.map (to_string ~minify) selectors);
            ")";
          ]
    | Not selectors ->
        let sep = if minify then "," else ", " in
        String.concat ""
          [
            ":not(";
            String.concat sep (List.map (to_string ~minify) selectors);
            ")";
          ]
    | Fun (name, selectors) ->
        let sep = if minify then "," else ", " in
        String.concat ""
          [
            ":";
            name;
            "(";
            String.concat sep (List.map (to_string ~minify) selectors);
            ")";
          ]
    | Compound selectors ->
        String.concat "" (List.map (to_string ~minify) selectors)
    | Combined (left, comb, right) ->
        let comb_str = combinator_to_string ~minify comb in
        String.concat ""
          [ to_string ~minify left; comb_str; to_string ~minify right ]
    | List selectors ->
        let sep = if minify then "," else ", " in
        String.concat sep (List.map (to_string ~minify) selectors)

  let validate_css_identifier name =
    if String.length name = 0 then err_invalid_identifier name "cannot be empty";

    let first_char = name.[0] in

    (* Check for invalid starting patterns *)
    if first_char >= '0' && first_char <= '9' then
      err_invalid_identifier name "cannot start with digit";

    if String.length name >= 2 then (
      if name.[0] = '-' && name.[1] = '-' then
        err_invalid_identifier name
          "cannot start with '--' (reserved for custom properties)";
      if name.[0] = '-' && name.[1] >= '0' && name.[1] <= '9' then
        err_invalid_identifier name "cannot start with '-' followed by digit");

    (* Validate characters with CSS escape support *)
    let len = String.length name in
    let i = ref 0 in
    while !i < len do
      let c = name.[!i] in
      if c = '\\' then (
        (* Skip escaped sequence payload: either next char or up to 6 hex digits
           optionally followed by a space *)
        incr i;
        if !i < len then (
          let is_hex c =
            (c >= '0' && c <= '9')
            || (c >= 'a' && c <= 'f')
            || (c >= 'A' && c <= 'F')
          in
          let start = !i in
          let rec consume_hex n =
            if n = 6 || !i >= len then ()
            else if is_hex name.[!i] then (
              incr i;
              consume_hex (n + 1))
            else ()
          in
          consume_hex 0;
          if !i = start then incr i (* single escaped char *)
          else if !i < len && name.[!i] = ' ' then incr i))
      else
        let idx = !i in
        let is_valid =
          if idx = 0 then
            (* Allow - at start for vendor prefixes, but other rules still
               apply *)
            is_valid_nmstart c || c = '-'
          else is_valid_nmchar c
        in
        if (not is_valid) && Char.code c <= 127 then
          (* Only validate ASCII, allow non-ASCII *)
          err_invalid_identifier name
            (String.concat ""
               [
                 "contains invalid character '";
                 String.make 1 c;
                 "' at position ";
                 Int.to_string idx;
               ]);
        incr i
    done

  let element name =
    validate_css_identifier name;
    Element name

  let class_ name =
    validate_css_identifier name;
    Class name

  let id name =
    validate_css_identifier name;
    Id name

  let universal = Universal

  let attribute name match_type =
    validate_css_identifier name;
    Attribute (name, match_type)

  let pseudo_class name =
    (* Skip validation for functional pseudo-classes that contain parentheses *)
    if not (String.contains name '(') then validate_css_identifier name;
    Pseudo_class name

  let pseudo_element name =
    validate_css_identifier name;
    Pseudo_element name

  let rec combine s1 comb s2 =
    match s2 with
    | List selectors ->
        (* When combining with a List, distribute the combinator over each
           element *)
        List (List.map (combine s1 comb) selectors)
    | _ ->
        (* For all other cases, create a Combined node *)
        Combined (s1, comb, s2)

  let ( ++ ) s1 s2 = combine s1 Descendant s2
  let ( >> ) s1 s2 = combine s1 Child s2
  let where selectors = Where selectors
  let not selectors = Not selectors
  let fun_ name selectors = Fun (name, selectors)
  let list selectors = List selectors
  let is_compound_list = function List _ -> true | _ -> false
  let compound selectors = Compound selectors
  let ( && ) sel1 sel2 = compound [ sel1; sel2 ]
  let ( || ) sel1 sel2 = list [ sel1; sel2 ]

  (* Pretty printer for selectors *)
  let rec pp : t Pp.t =
   fun ctx -> function
    | Element name -> Pp.string ctx name
    | Class name ->
        Pp.char ctx '.';
        Pp.string ctx name
    | Id name ->
        Pp.char ctx '#';
        Pp.string ctx name
    | Universal -> Pp.char ctx '*'
    | Attribute (name, match_type) ->
        Pp.char ctx '[';
        Pp.string ctx name;
        (match match_type with
        | Presence -> ()
        | Exact value ->
            Pp.char ctx '=';
            if needs_quotes value then (
              Pp.char ctx '"';
              Pp.string ctx value;
              Pp.char ctx '"')
            else Pp.string ctx value
        | Whitespace_list value ->
            Pp.string ctx "~=";
            Pp.char ctx '"';
            Pp.string ctx value;
            Pp.char ctx '"'
        | Hyphen_list value ->
            Pp.string ctx "|=";
            Pp.char ctx '"';
            Pp.string ctx value;
            Pp.char ctx '"'
        | Prefix value ->
            Pp.string ctx "^=";
            Pp.char ctx '"';
            Pp.string ctx value;
            Pp.char ctx '"'
        | Suffix value ->
            Pp.string ctx "$=";
            Pp.char ctx '"';
            Pp.string ctx value;
            Pp.char ctx '"'
        | Substring value ->
            Pp.string ctx "*=";
            Pp.char ctx '"';
            Pp.string ctx value;
            Pp.char ctx '"');
        Pp.char ctx ']'
    | Pseudo_class name ->
        Pp.char ctx ':';
        Pp.string ctx name
    | Pseudo_element name ->
        Pp.string ctx "::";
        Pp.string ctx name
    | Where selectors ->
        Pp.string ctx ":where(";
        Pp.list ~sep:Pp.comma pp ctx selectors;
        Pp.char ctx ')'
    | Not selectors ->
        Pp.string ctx ":not(";
        Pp.list ~sep:Pp.comma pp ctx selectors;
        Pp.char ctx ')'
    | Fun (name, selectors) ->
        Pp.char ctx ':';
        Pp.string ctx name;
        Pp.char ctx '(';
        Pp.list ~sep:Pp.comma pp ctx selectors;
        Pp.char ctx ')'
    | Compound selectors -> List.iter (pp ctx) selectors
    | Combined (left, comb, right) ->
        pp ctx left;
        pp_combinator ctx comb;
        pp ctx right
    | List selectors -> Pp.list ~sep:Pp.comma pp ctx selectors

  and pp_combinator ctx = function
    | Descendant -> Pp.space ctx ()
    | Child -> if ctx.minify then Pp.char ctx '>' else Pp.string ctx " > "
    | Next_sibling ->
        if ctx.minify then Pp.char ctx '+' else Pp.string ctx " + "
    | Subsequent_sibling ->
        if ctx.minify then Pp.char ctx '~' else Pp.string ctx " ~ "
end

type rule = { selector : Selector.t; declarations : declaration list }
type media_rule = { media_condition : string; media_rules : rule list }

type container_rule = {
  container_name : string option;
  container_condition : string;
  container_rules : rule list;
}

type starting_style_rule = { starting_rules : rule list }

type supports_content =
  | Support_rules of rule list
  | Support_nested of rule list * supports_rule list

and supports_rule = {
  supports_condition : string;
  supports_content : supports_content;
}

type nested_rule = Rule of rule | Supports of supports_rule

type property_rule = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string option;
}

type layer_rule = {
  layer : string;
  rules : nested_rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  supports_queries : supports_rule list;
}

type t = {
  layers : layer_rule list;
  rules : rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  starting_styles : starting_style_rule list;
  supports_queries : supports_rule list;
  at_properties : property_rule list;
}

type sheet_item =
  | Rule of rule
  | Media of media_rule
  | Supports of supports_rule
  | Container of container_rule
  | Layer of layer_rule
  | Property of property_rule
  | Starting_style of starting_style_rule

(** Stylesheet accessors *)
let stylesheet_rules t = t.rules

let stylesheet_layers t = t.layers
let stylesheet_media_queries t = t.media_queries
let stylesheet_container_queries t = t.container_queries

(** {1 Creation} *)

let pp_property : type a. a property Pp.t =
 fun ctx -> function
  | Background_color -> Pp.string ctx "background-color"
  | Color -> Pp.string ctx "color"
  | Border_color -> Pp.string ctx "border-color"
  | Border_style -> Pp.string ctx "border-style"
  | Border_top_style -> Pp.string ctx "border-top-style"
  | Border_right_style -> Pp.string ctx "border-right-style"
  | Border_bottom_style -> Pp.string ctx "border-bottom-style"
  | Border_left_style -> Pp.string ctx "border-left-style"
  | Padding -> Pp.string ctx "padding"
  | Padding_left -> Pp.string ctx "padding-left"
  | Padding_right -> Pp.string ctx "padding-right"
  | Padding_bottom -> Pp.string ctx "padding-bottom"
  | Padding_top -> Pp.string ctx "padding-top"
  | Padding_inline -> Pp.string ctx "padding-inline"
  | Padding_inline_start -> Pp.string ctx "padding-inline-start"
  | Padding_inline_end -> Pp.string ctx "padding-inline-end"
  | Padding_block -> Pp.string ctx "padding-block"
  | Margin -> Pp.string ctx "margin"
  | Margin_inline_end -> Pp.string ctx "margin-inline-end"
  | Margin_left -> Pp.string ctx "margin-left"
  | Margin_right -> Pp.string ctx "margin-right"
  | Margin_top -> Pp.string ctx "margin-top"
  | Margin_bottom -> Pp.string ctx "margin-bottom"
  | Margin_inline -> Pp.string ctx "margin-inline"
  | Margin_block -> Pp.string ctx "margin-block"
  | Gap -> Pp.string ctx "gap"
  | Column_gap -> Pp.string ctx "column-gap"
  | Row_gap -> Pp.string ctx "row-gap"
  | Width -> Pp.string ctx "width"
  | Height -> Pp.string ctx "height"
  | Min_width -> Pp.string ctx "min-width"
  | Min_height -> Pp.string ctx "min-height"
  | Max_width -> Pp.string ctx "max-width"
  | Max_height -> Pp.string ctx "max-height"
  | Font_size -> Pp.string ctx "font-size"
  | Line_height -> Pp.string ctx "line-height"
  | Font_weight -> Pp.string ctx "font-weight"
  | Font_style -> Pp.string ctx "font-style"
  | Text_align -> Pp.string ctx "text-align"
  | Text_decoration -> Pp.string ctx "text-decoration"
  | Text_decoration_style -> Pp.string ctx "text-decoration-style"
  | Text_decoration_color -> Pp.string ctx "text-decoration-color"
  | Text_decoration_thickness -> Pp.string ctx "text-decoration-thickness"
  | Text_underline_offset -> Pp.string ctx "text-underline-offset"
  | Text_transform -> Pp.string ctx "text-transform"
  | Letter_spacing -> Pp.string ctx "letter-spacing"
  | List_style_type -> Pp.string ctx "list-style-type"
  | List_style_position -> Pp.string ctx "list-style-position"
  | List_style_image -> Pp.string ctx "list-style-image"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Flex_direction -> Pp.string ctx "flex-direction"
  | Flex_wrap -> Pp.string ctx "flex-wrap"
  | Flex -> Pp.string ctx "flex"
  | Flex_grow -> Pp.string ctx "flex-grow"
  | Flex_shrink -> Pp.string ctx "flex-shrink"
  | Flex_basis -> Pp.string ctx "flex-basis"
  | Order -> Pp.string ctx "order"
  | Align_items -> Pp.string ctx "align-items"
  | Justify_content -> Pp.string ctx "justify-content"
  | Justify_items -> Pp.string ctx "justify-items"
  | Align_content -> Pp.string ctx "align-content"
  | Align_self -> Pp.string ctx "align-self"
  | Justify_self -> Pp.string ctx "justify-self"
  | Place_content -> Pp.string ctx "place-content"
  | Place_items -> Pp.string ctx "place-items"
  | Place_self -> Pp.string ctx "place-self"
  | Grid_template_columns -> Pp.string ctx "grid-template-columns"
  | Grid_template_rows -> Pp.string ctx "grid-template-rows"
  | Grid_template_areas -> Pp.string ctx "grid-template-areas"
  | Grid_template -> Pp.string ctx "grid-template"
  | Grid_area -> Pp.string ctx "grid-area"
  | Grid_auto_flow -> Pp.string ctx "grid-auto-flow"
  | Grid_auto_columns -> Pp.string ctx "grid-auto-columns"
  | Grid_auto_rows -> Pp.string ctx "grid-auto-rows"
  | Grid_column -> Pp.string ctx "grid-column"
  | Grid_row -> Pp.string ctx "grid-row"
  | Grid_column_start -> Pp.string ctx "grid-column-start"
  | Grid_column_end -> Pp.string ctx "grid-column-end"
  | Grid_row_start -> Pp.string ctx "grid-row-start"
  | Grid_row_end -> Pp.string ctx "grid-row-end"
  | Border_width -> Pp.string ctx "border-width"
  | Border_top_width -> Pp.string ctx "border-top-width"
  | Border_right_width -> Pp.string ctx "border-right-width"
  | Border_bottom_width -> Pp.string ctx "border-bottom-width"
  | Border_left_width -> Pp.string ctx "border-left-width"
  | Border_inline_start_width -> Pp.string ctx "border-inline-start-width"
  | Border_inline_end_width -> Pp.string ctx "border-inline-end-width"
  | Border_radius -> Pp.string ctx "border-radius"
  | Border_top_color -> Pp.string ctx "border-top-color"
  | Border_right_color -> Pp.string ctx "border-right-color"
  | Border_bottom_color -> Pp.string ctx "border-bottom-color"
  | Border_left_color -> Pp.string ctx "border-left-color"
  | Border_inline_start_color -> Pp.string ctx "border-inline-start-color"
  | Border_inline_end_color -> Pp.string ctx "border-inline-end-color"
  | Box_shadow -> Pp.string ctx "box-shadow"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | Stroke_width -> Pp.string ctx "stroke-width"
  | Opacity -> Pp.string ctx "opacity"
  | Mix_blend_mode -> Pp.string ctx "mix-blend-mode"
  | Transition -> Pp.string ctx "transition"
  | Transform -> Pp.string ctx "transform"
  | Scale -> Pp.string ctx "scale"
  | Cursor -> Pp.string ctx "cursor"
  | Table_layout -> Pp.string ctx "table-layout"
  | Border_collapse -> Pp.string ctx "border-collapse"
  | Border_spacing -> Pp.string ctx "border-spacing"
  | User_select -> Pp.string ctx "user-select"
  | Pointer_events -> Pp.string ctx "pointer-events"
  | Overflow -> Pp.string ctx "overflow"
  | Top -> Pp.string ctx "top"
  | Right -> Pp.string ctx "right"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Z_index -> Pp.string ctx "z-index"
  | Outline -> Pp.string ctx "outline"
  | Outline_style -> Pp.string ctx "outline-style"
  | Outline_width -> Pp.string ctx "outline-width"
  | Outline_color -> Pp.string ctx "outline-color"
  | Outline_offset -> Pp.string ctx "outline-offset"
  | Forced_color_adjust -> Pp.string ctx "forced-color-adjust"
  | Scroll_snap_type -> Pp.string ctx "scroll-snap-type"
  | Clip -> Pp.string ctx "clip"
  | Clear -> Pp.string ctx "clear"
  | Float -> Pp.string ctx "float"
  | White_space -> Pp.string ctx "white-space"
  | Border -> Pp.string ctx "border"
  | Tab_size -> Pp.string ctx "tab-size"
  | Webkit_text_size_adjust -> Pp.string ctx "-webkit-text-size-adjust"
  | Font_feature_settings -> Pp.string ctx "font-feature-settings"
  | Font_variation_settings -> Pp.string ctx "font-variation-settings"
  | Webkit_tap_highlight_color -> Pp.string ctx "-webkit-tap-highlight-color"
  | Webkit_text_decoration -> Pp.string ctx "-webkit-text-decoration"
  | Webkit_text_decoration_color ->
      Pp.string ctx "-webkit-text-decoration-color"
  | Text_indent -> Pp.string ctx "text-indent"
  | List_style -> Pp.string ctx "list-style"
  | Font -> Pp.string ctx "font"
  | Webkit_appearance -> Pp.string ctx "-webkit-appearance"
  | Container_type -> Pp.string ctx "container-type"
  | Container_name -> Pp.string ctx "container-name"
  | Perspective -> Pp.string ctx "perspective"
  | Perspective_origin -> Pp.string ctx "perspective-origin"
  | Transform_style -> Pp.string ctx "transform-style"
  | Backface_visibility -> Pp.string ctx "backface-visibility"
  | Object_position -> Pp.string ctx "object-position"
  | Rotate -> Pp.string ctx "rotate"
  | Transition_duration -> Pp.string ctx "transition-duration"
  | Transition_timing_function -> Pp.string ctx "transition-timing-function"
  | Transition_delay -> Pp.string ctx "transition-delay"
  | Will_change -> Pp.string ctx "will-change"
  | Contain -> Pp.string ctx "contain"
  | Isolation -> Pp.string ctx "isolation"
  | Word_spacing -> Pp.string ctx "word-spacing"
  | Background_attachment -> Pp.string ctx "background-attachment"
  | Border_top -> Pp.string ctx "border-top"
  | Border_right -> Pp.string ctx "border-right"
  | Border_bottom -> Pp.string ctx "border-bottom"
  | Border_left -> Pp.string ctx "border-left"
  | Transform_origin -> Pp.string ctx "transform-origin"
  | Text_shadow -> Pp.string ctx "text-shadow"
  | Clip_path -> Pp.string ctx "clip-path"
  | Mask -> Pp.string ctx "mask"
  | Content_visibility -> Pp.string ctx "content-visibility"
  | Filter -> Pp.string ctx "filter"
  | Background_image -> Pp.string ctx "background-image"
  | Animation -> Pp.string ctx "animation"
  | Aspect_ratio -> Pp.string ctx "aspect-ratio"
  | Overflow_x -> Pp.string ctx "overflow-x"
  | Overflow_y -> Pp.string ctx "overflow-y"
  | Vertical_align -> Pp.string ctx "vertical-align"
  | Font_family -> Pp.string ctx "font-family"
  | Background_position -> Pp.string ctx "background-position"
  | Background_repeat -> Pp.string ctx "background-repeat"
  | Background_size -> Pp.string ctx "background-size"
  | Webkit_font_smoothing -> Pp.string ctx "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> Pp.string ctx "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> Pp.string ctx "-webkit-line-clamp"
  | Webkit_box_orient -> Pp.string ctx "-webkit-box-orient"
  | Text_overflow -> Pp.string ctx "text-overflow"
  | Text_wrap -> Pp.string ctx "text-wrap"
  | Word_break -> Pp.string ctx "word-break"
  | Overflow_wrap -> Pp.string ctx "overflow-wrap"
  | Hyphens -> Pp.string ctx "hyphens"
  | Webkit_hyphens -> Pp.string ctx "-webkit-hyphens"
  | Font_stretch -> Pp.string ctx "font-stretch"
  | Font_variant_numeric -> Pp.string ctx "font-variant-numeric"
  | Backdrop_filter -> Pp.string ctx "backdrop-filter"
  | Scroll_snap_align -> Pp.string ctx "scroll-snap-align"
  | Scroll_snap_stop -> Pp.string ctx "scroll-snap-stop"
  | Scroll_behavior -> Pp.string ctx "scroll-behavior"
  | Box_sizing -> Pp.string ctx "box-sizing"
  | Resize -> Pp.string ctx "resize"
  | Object_fit -> Pp.string ctx "object-fit"
  | Appearance -> Pp.string ctx "appearance"
  | Content -> Pp.string ctx "content"
  | Quotes -> Pp.string ctx "quotes"
  | Text_size_adjust -> Pp.string ctx "text-size-adjust"
  | Touch_action -> Pp.string ctx "touch-action"
  | Direction -> Pp.string ctx "direction"
  | Unicode_bidi -> Pp.string ctx "unicode-bidi"
  | Writing_mode -> Pp.string ctx "writing-mode"
  | Text_decoration_skip_ink -> Pp.string ctx "text-decoration-skip-ink"
  | Animation_name -> Pp.string ctx "animation-name"
  | Animation_duration -> Pp.string ctx "animation-duration"
  | Animation_timing_function -> Pp.string ctx "animation-timing-function"
  | Animation_delay -> Pp.string ctx "animation-delay"
  | Animation_iteration_count -> Pp.string ctx "animation-iteration-count"
  | Animation_direction -> Pp.string ctx "animation-direction"
  | Animation_fill_mode -> Pp.string ctx "animation-fill-mode"
  | Animation_play_state -> Pp.string ctx "animation-play-state"
  | Background_blend_mode -> Pp.string ctx "background-blend-mode"
  | Scroll_margin -> Pp.string ctx "scroll-margin"
  | Scroll_margin_top -> Pp.string ctx "scroll-margin-top"
  | Scroll_margin_right -> Pp.string ctx "scroll-margin-right"
  | Scroll_margin_bottom -> Pp.string ctx "scroll-margin-bottom"
  | Scroll_margin_left -> Pp.string ctx "scroll-margin-left"
  | Scroll_padding -> Pp.string ctx "scroll-padding"
  | Scroll_padding_top -> Pp.string ctx "scroll-padding-top"
  | Scroll_padding_right -> Pp.string ctx "scroll-padding-right"
  | Scroll_padding_bottom -> Pp.string ctx "scroll-padding-bottom"
  | Scroll_padding_left -> Pp.string ctx "scroll-padding-left"
  | Overscroll_behavior -> Pp.string ctx "overscroll-behavior"
  | Overscroll_behavior_x -> Pp.string ctx "overscroll-behavior-x"
  | Overscroll_behavior_y -> Pp.string ctx "overscroll-behavior-y"
  | Accent_color -> Pp.string ctx "accent-color"
  | Caret_color -> Pp.string ctx "caret-color"

let rule ~selector declarations = { selector; declarations }
let selector rule = rule.selector
let declarations rule = rule.declarations

let media ~condition rules =
  { media_condition = condition; media_rules = rules }

let media_condition media = media.media_condition
let media_rules media = media.media_rules

let supports ~condition rules =
  { supports_condition = condition; supports_content = Support_rules rules }

let supports_nested ~condition rules nested_queries =
  {
    supports_condition = condition;
    supports_content = Support_nested (rules, nested_queries);
  }

let container ?(name = Option.none) ~condition rules =
  {
    container_name = name;
    container_condition = condition;
    container_rules = rules;
  }

let property ~syntax ?initial_value ?(inherits = false) name =
  { name; syntax; inherits; initial_value }

let property_rule_name r = r.name
let property_rule_initial r = r.initial_value

let default_decl_of_property_rule r =
  match r.initial_value with
  | Some v -> custom_property r.name v
  | None -> custom_property r.name ""

let rule_to_nested rule : nested_rule = Rule rule
let supports_to_nested supports : nested_rule = Supports supports

let layer ~name ?(media = []) ?(container = []) ?(supports = []) rules =
  {
    layer = name;
    rules;
    media_queries = media;
    container_queries = container;
    supports_queries = supports;
  }

let layer_name (layer : layer_rule) = layer.layer
let layer_rules (layer : layer_rule) : nested_rule list = layer.rules

let empty =
  {
    layers = [];
    rules = [];
    media_queries = [];
    container_queries = [];
    starting_styles = [];
    supports_queries = [];
    at_properties = [];
  }

let concat stylesheets =
  List.fold_left
    (fun acc sheet ->
      {
        layers = acc.layers @ sheet.layers;
        rules = acc.rules @ sheet.rules;
        media_queries = acc.media_queries @ sheet.media_queries;
        container_queries = acc.container_queries @ sheet.container_queries;
        starting_styles = acc.starting_styles @ sheet.starting_styles;
        supports_queries = acc.supports_queries @ sheet.supports_queries;
        at_properties = acc.at_properties @ sheet.at_properties;
      })
    empty stylesheets

let stylesheet items =
  List.fold_left
    (fun acc item ->
      match item with
      | Rule r -> { acc with rules = acc.rules @ [ r ] }
      | Media m -> { acc with media_queries = acc.media_queries @ [ m ] }
      | Container c ->
          { acc with container_queries = acc.container_queries @ [ c ] }
      | Starting_style s ->
          { acc with starting_styles = acc.starting_styles @ [ s ] }
      | Supports s ->
          { acc with supports_queries = acc.supports_queries @ [ s ] }
      | Property a -> { acc with at_properties = acc.at_properties @ [ a ] }
      | Layer l -> { acc with layers = acc.layers @ [ l ] })
    empty items

let stylesheet_items t =
  let rules = List.map (fun r -> Rule r) t.rules in
  let media = List.map (fun m -> Media m) t.media_queries in
  let containers = List.map (fun c -> Container c) t.container_queries in
  let supports = List.map (fun s -> Supports s) t.supports_queries in
  let layers = List.map (fun l -> Layer l) t.layers in
  let properties = List.map (fun p -> Property p) t.at_properties in
  let starting = List.map (fun s -> Starting_style s) t.starting_styles in
  rules @ media @ containers @ supports @ layers @ properties @ starting

(** {1 Utilities} *)

let rec vars_of_calc : type a. a calc -> any_var list = function
  | Val _ -> []
  | Var v -> [ V v ]
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right

(* Extract variables from any property value *)
let vars_of_property : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Width, Var v -> [ V v ]
  | Width, Calc calc -> vars_of_calc calc
  | Height, Var v -> [ V v ]
  | Height, Calc calc -> vars_of_calc calc
  | Min_width, Var v -> [ V v ]
  | Min_width, Calc calc -> vars_of_calc calc
  | Min_height, Var v -> [ V v ]
  | Min_height, Calc calc -> vars_of_calc calc
  | Max_width, Var v -> [ V v ]
  | Max_width, Calc calc -> vars_of_calc calc
  | Max_height, Var v -> [ V v ]
  | Max_height, Calc calc -> vars_of_calc calc
  | Padding, Var v -> [ V v ]
  | Padding, Calc calc -> vars_of_calc calc
  | Padding_top, Var v -> [ V v ]
  | Padding_top, Calc calc -> vars_of_calc calc
  | Padding_right, Var v -> [ V v ]
  | Padding_right, Calc calc -> vars_of_calc calc
  | Padding_bottom, Var v -> [ V v ]
  | Padding_bottom, Calc calc -> vars_of_calc calc
  | Padding_left, Var v -> [ V v ]
  | Padding_left, Calc calc -> vars_of_calc calc
  | Padding_inline, Var v -> [ V v ]
  | Padding_inline, Calc calc -> vars_of_calc calc
  | Padding_inline_start, Var v -> [ V v ]
  | Padding_inline_start, Calc calc -> vars_of_calc calc
  | Padding_inline_end, Var v -> [ V v ]
  | Padding_inline_end, Calc calc -> vars_of_calc calc
  | Padding_block, Var v -> [ V v ]
  | Padding_block, Calc calc -> vars_of_calc calc
  | Margin, Var v -> [ V v ]
  | Margin, Calc calc -> vars_of_calc calc
  | Margin_top, Var v -> [ V v ]
  | Margin_top, Calc calc -> vars_of_calc calc
  | Margin_right, Var v -> [ V v ]
  | Margin_right, Calc calc -> vars_of_calc calc
  | Margin_bottom, Var v -> [ V v ]
  | Margin_bottom, Calc calc -> vars_of_calc calc
  | Margin_left, Var v -> [ V v ]
  | Margin_left, Calc calc -> vars_of_calc calc
  | Margin_inline, Var v -> [ V v ]
  | Margin_inline, Calc calc -> vars_of_calc calc
  | Margin_block, Var v -> [ V v ]
  | Margin_block, Calc calc -> vars_of_calc calc
  | Top, Var v -> [ V v ]
  | Top, Calc calc -> vars_of_calc calc
  | Right, Var v -> [ V v ]
  | Right, Calc calc -> vars_of_calc calc
  | Bottom, Var v -> [ V v ]
  | Bottom, Calc calc -> vars_of_calc calc
  | Left, Var v -> [ V v ]
  | Left, Calc calc -> vars_of_calc calc
  | Font_size, Var v -> [ V v ]
  | Font_size, Calc calc -> vars_of_calc calc
  | Letter_spacing, Var v -> [ V v ]
  | Letter_spacing, Calc calc -> vars_of_calc calc
  | Line_height, Normal -> []
  | Line_height, Length (Var v) -> [ V v ]
  | Line_height, Length (Calc calc) -> vars_of_calc calc
  | Line_height, Length _ -> []
  | Line_height, Number _ -> []
  | Line_height, Percentage _ -> []
  | Line_height, Inherit -> []
  | Line_height, Var v -> [ V v ]
  | Border_width, Var v -> [ V v ]
  | Border_width, Calc calc -> vars_of_calc calc
  | Border_top_width, Var v -> [ V v ]
  | Border_top_width, Calc calc -> vars_of_calc calc
  | Border_right_width, Var v -> [ V v ]
  | Border_right_width, Calc calc -> vars_of_calc calc
  | Border_bottom_width, Var v -> [ V v ]
  | Border_bottom_width, Calc calc -> vars_of_calc calc
  | Border_left_width, Var v -> [ V v ]
  | Border_left_width, Calc calc -> vars_of_calc calc
  | Border_inline_start_width, Var v -> [ V v ]
  | Border_inline_start_width, Calc calc -> vars_of_calc calc
  | Border_inline_end_width, Var v -> [ V v ]
  | Border_inline_end_width, Calc calc -> vars_of_calc calc
  | Outline_width, Var v -> [ V v ]
  | Outline_width, Calc calc -> vars_of_calc calc
  | Column_gap, Var v -> [ V v ]
  | Column_gap, Calc calc -> vars_of_calc calc
  | Row_gap, Var v -> [ V v ]
  | Row_gap, Calc calc -> vars_of_calc calc
  | Gap, Var v -> [ V v ]
  | Gap, Calc calc -> vars_of_calc calc
  (* Color properties *)
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Border_inline_start_color, Var v -> [ V v ]
  | Border_inline_end_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Outline_color, Var v -> [ V v ]
  (* Border radius *)
  | Border_radius, Var v -> [ V v ]
  | Border_radius, Calc calc -> vars_of_calc calc
  (* Outline offset *)
  | Outline_offset, Var v -> [ V v ]
  | Outline_offset, Calc calc -> vars_of_calc calc
  (* Other properties don't support Var *)
  (* All other cases *)
  | _ -> []

let rec vars_of_value : type a. a kind -> a -> any_var list =
 fun kind value ->
  match (kind, value) with
  | Length, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Duration, Var v -> [ V v ]
  | Blend_mode, _ -> []
  | Scroll_snap_strictness, Var v -> [ V v ]
  | Scroll_snap_strictness, _ -> []
  | Angle, Var v -> [ V v ]
  | Angle, _ -> []
  | Length, Calc calc -> vars_of_calc calc
  | Color, Mix _ -> [] (* TODO: extend to extract from color mix *)
  | Int, _ -> []
  | Float, _ -> []
  | Aspect_ratio, _ -> []
  | Border_style, _ -> []
  | Font_weight, _ -> []
  | String, _ -> []
  | Font_variant_numeric, Var v -> [ V v ]
  | ( Font_variant_numeric,
      Composed
        {
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        } ) ->
      vars_of_values_opt
        [
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        ]
  | Font_variant_numeric, _ -> []
  | Font_variant_numeric_token, Var v -> [ V v ]
  | Font_variant_numeric_token, _ -> []
  | Box_shadow, Var v -> [ V v ]
  | Box_shadow, _ -> []
  | _ -> []

and vars_of_values_opt values =
  let collect_vars (opt_fv : font_variant_numeric_token option) =
    match opt_fv with None -> [] | Some (Var v) -> [ V v ] | Some _token -> []
  in
  List.concat_map collect_vars values

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name

(** Extract all CSS variables referenced in properties (for theme layer) *)
let vars_of_declarations properties =
  List.concat_map
    (function
      | Declaration (prop, value) -> vars_of_property prop value
      | Important_declaration (prop, value) -> vars_of_property prop value
      | Custom_declaration { kind; value; _ } -> vars_of_value kind value)
    properties
  |> List.sort_uniq compare_vars_by_name

(* Helper function to check if a property should allow duplicates Some webkit
   properties need to be duplicated for browser compatibility. This is a
   workaround for older WebKit/Safari versions that had bugs with certain CSS
   properties.

   See: https://bugs.webkit.org/show_bug.cgi?id=101180 and:
   https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color#browser_compatibility

   Modern CSS libraries include these duplicates for maximum compatibility. *)

(* Duplicate known buggy properties for browser compatibility *)
let duplicate_buggy_properties decls =
  List.concat_map
    (fun decl ->
      match decl with
      | Declaration (Webkit_text_decoration, Inherit)
      | Important_declaration (Webkit_text_decoration, Inherit) ->
          [ decl; decl; decl ] (* Triplicate only when inherit *)
      | Declaration (Webkit_text_decoration_color, _)
      | Important_declaration (Webkit_text_decoration_color, _) ->
          [ decl; decl ] (* Always duplicate webkit-text-decoration-color *)
      | _ -> [ decl ])
    decls

let deduplicate_declarations props =
  (* Keep last occurrence of each property while preserving order *)
  let seen = Hashtbl.create 16 in
  let deduped =
    List.fold_right
      (fun decl acc ->
        let prop_name =
          match decl with
          | Declaration (prop, _) -> Pp.to_string ~minify:true pp_property prop
          | Important_declaration (prop, _) ->
              Pp.to_string ~minify:true pp_property prop
          | Custom_declaration { name; _ } -> name
        in
        if Hashtbl.mem seen prop_name then acc
        else (
          Hashtbl.add seen prop_name ();
          decl :: acc))
      props []
  in
  (* Apply buggy property duplication after deduplication *)
  duplicate_buggy_properties deduped

(* Get the name of a variable *)
let any_var_name (V v) = String.concat "" [ "--"; v.name ]

(* Extract variables from a typed value - needs to handle each property type *)
let extract_vars_from_prop_value : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Border_inline_start_color, Var v -> [ V v ]
  | Border_inline_end_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Webkit_text_decoration_color, Var v -> [ V v ]
  | Webkit_tap_highlight_color, Var v -> [ V v ]
  | Padding, Var v -> [ V v ]
  | Padding_left, Var v -> [ V v ]
  | Padding_right, Var v -> [ V v ]
  | Padding_top, Var v -> [ V v ]
  | Padding_bottom, Var v -> [ V v ]
  | Margin, Var v -> [ V v ]
  | Margin_left, Var v -> [ V v ]
  | Margin_right, Var v -> [ V v ]
  | Margin_top, Var v -> [ V v ]
  | Margin_bottom, Var v -> [ V v ]
  | Gap, Var v -> [ V v ]
  | Column_gap, Var v -> [ V v ]
  | Row_gap, Var v -> [ V v ]
  | Width, Var v -> [ V v ]
  | Height, Var v -> [ V v ]
  | Min_width, Var v -> [ V v ]
  | Min_height, Var v -> [ V v ]
  | Max_width, Var v -> [ V v ]
  | Max_height, Var v -> [ V v ]
  | Font_size, Var v -> [ V v ]
  | Line_height, Var v -> [ V v ]
  | Letter_spacing, Var v -> [ V v ]
  | Top, Var v -> [ V v ]
  | Right, Var v -> [ V v ]
  | Bottom, Var v -> [ V v ]
  | Left, Var v -> [ V v ]
  | Border_radius, Var v -> [ V v ]
  | Border_width, Var v -> [ V v ]
  | Outline_offset, Var v -> [ V v ]
  | _ -> [] (* No variables in this value *)

let extract_vars_from_declaration : declaration -> any_var list = function
  | Custom_declaration _ -> [] (* Custom properties don't have typed vars *)
  | Declaration (prop, value) -> extract_vars_from_prop_value prop value
  | Important_declaration (prop, value) ->
      extract_vars_from_prop_value prop value

(* Analyze declarations to find all variable references *)
let analyze_declarations (decls : declaration list) : any_var list =
  List.concat_map extract_vars_from_declaration decls

(* Extract only custom property declarations (variable definitions) *)
let extract_custom_declarations (decls : declaration list) : declaration list =
  List.filter (function Custom_declaration _ -> true | _ -> false) decls

(* Extract the variable name from a custom declaration *)
let custom_declaration_name (decl : declaration) : string option =
  match decl with Custom_declaration { name; _ } -> Some name | _ -> None

let inline_style_of_declarations ?(optimize = false) ?(minify = false)
    ?(mode : mode = Inline) props =
  let config = { mode; minify; optimize } in
  props
  |> List.map (function
       | Declaration (prop, value) ->
           let name = Pp.to_string ~minify:true pp_property prop in
           let value_str =
             let pp ctx v = pp_property_value ctx v in
             Pp.to_string ~minify:config.minify pp (prop, value)
           in
           if config.minify then String.concat "" [ name; ":"; value_str ]
           else String.concat "" [ name; ": "; value_str ]
       | Important_declaration (prop, value) ->
           let name = Pp.to_string ~minify:true pp_property prop in
           let value_str =
             let pp ctx v = pp_property_value ctx v in
             Pp.to_string ~minify:config.minify pp (prop, value)
           in
           if config.minify then
             String.concat "" [ name; ":"; value_str; "!important" ]
           else String.concat "" [ name; ": "; value_str; " !important" ]
       | Custom_declaration { name; kind; value; _ } ->
           let value_str =
             let pp ctx v = pp_value ctx v in
             Pp.to_string ~minify:config.minify pp (kind, value)
           in
           if config.minify then String.concat "" [ name; ":"; value_str ]
           else String.concat "" [ name; ": "; value_str ])
  |> String.concat "; "

let merge_rules rules =
  (* Only merge truly adjacent rules with the same selector to preserve cascade
     order. This is safe because we don't reorder rules - we only combine
     immediately adjacent rules with identical selectors, which maintains
     cascade semantics. *)
  let rec merge_adjacent acc prev_rule = function
    | [] -> List.rev (match prev_rule with Some r -> r :: acc | None -> acc)
    | rule :: rest -> (
        match prev_rule with
        | None ->
            (* First rule - just store it *)
            merge_adjacent acc (Some rule) rest
        | Some prev ->
            if prev.selector = rule.selector then
              (* Same selector immediately following - safe to merge *)
              let merged =
                {
                  selector = prev.selector;
                  declarations =
                    deduplicate_declarations
                      (prev.declarations @ rule.declarations);
                }
              in
              merge_adjacent acc (Some merged) rest
            else
              (* Different selector - emit previous rule and continue with
                 current *)
              merge_adjacent (prev :: acc) (Some rule) rest)
  in
  merge_adjacent [] None rules

(* Check if a selector should not be combined with others *)
let should_not_combine selector =
  (* Already a list selector - don't combine *)
  Selector.is_compound_list selector
  ||
  (* Check string representation for specific prefixes *)
  let s = Selector.to_string selector in
  String.starts_with ~prefix:"::file-selector-button" s
  || String.starts_with ~prefix:"::-webkit-" s

(* Convert group of selectors to a rule *)
let group_to_rule = function
  | [ (sel, decls) ] -> Some { selector = sel; declarations = decls }
  | [] -> None
  | group ->
      let selector_list = List.map fst (List.rev group) in
      let decls = snd (List.hd group) in
      (* Create a List selector from all the selectors *)
      let combined_selector =
        if List.length selector_list = 1 then List.hd selector_list
        else Selector.List selector_list
      in
      Some { selector = combined_selector; declarations = decls }

(* Flush current group to accumulator *)
let flush_group acc group =
  match group_to_rule group with Some rule -> rule :: acc | None -> acc

(* Combine consecutive rules with identical declarations into comma-separated
   selectors *)
let combine_identical_rules rules =
  (* Only combine consecutive rules to preserve cascade semantics *)
  let rec combine_consecutive acc current_group = function
    | [] -> List.rev (flush_group acc current_group)
    | rule :: rest -> (
        if should_not_combine rule.selector then
          (* Don't combine this selector, flush current group and start fresh *)
          let acc' = rule :: flush_group acc current_group in
          combine_consecutive acc' [] rest
        else
          match current_group with
          | [] ->
              (* Start a new group *)
              combine_consecutive acc
                [ (rule.selector, rule.declarations) ]
                rest
          | (_prev_sel, prev_decls) :: _ ->
              if prev_decls = rule.declarations then
                (* Same declarations, add to current group *)
                combine_consecutive acc
                  ((rule.selector, rule.declarations) :: current_group)
                  rest
              else
                (* Different declarations, flush current group and start new
                   one *)
                let acc' = flush_group acc current_group in
                combine_consecutive acc'
                  [ (rule.selector, rule.declarations) ]
                  rest)
  in
  combine_consecutive [] [] rules

(** {1 Rendering} *)

(* Forward declarations for mutually recursive functions *)
let rec pp_rule : rule Pp.t =
 fun ctx rule ->
  Selector.pp ctx rule.selector;
  Pp.sp ctx ();
  let pp_body ctx () =
    match rule.declarations with
    | [] -> ()
    | decls ->
        Pp.cut ctx ();
        Pp.nest 2
          (Pp.list
             ~sep:(fun ctx () ->
               Pp.semicolon ctx ();
               Pp.cut ctx ())
             pp_declaration)
          ctx decls;
        Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

and pp_declaration : declaration Pp.t =
 fun ctx -> function
  | Declaration (prop, value) ->
      pp_property ctx prop;
      Pp.colon ctx ();
      Pp.space_if_pretty ctx ();
      pp_property_value ctx (prop, value)
  | Important_declaration (prop, value) ->
      pp_property ctx prop;
      Pp.colon ctx ();
      Pp.space_if_pretty ctx ();
      pp_property_value ctx (prop, value);
      Pp.space_if_pretty ctx ();
      Pp.string ctx "!important"
  | Custom_declaration { name; kind; value; _ } ->
      Pp.string ctx name;
      Pp.colon ctx ();
      Pp.space_if_pretty ctx ();
      pp_value ctx (kind, value)

and pp_supports_content : supports_content Pp.t =
 fun ctx -> function
  | Support_rules rules -> Pp.list ~sep:Pp.cut pp_rule ctx rules
  | Support_nested (rules, nested_queries) ->
      Pp.list ~sep:Pp.cut pp_rule ctx rules;
      if rules <> [] && nested_queries <> [] then Pp.cut ctx ();
      Pp.list ~sep:Pp.cut pp_supports_query ctx nested_queries

and pp_supports_query : supports_rule Pp.t =
 fun ctx sq ->
  Pp.string ctx "@supports ";
  Pp.string ctx sq.supports_condition;
  Pp.sp ctx ();
  Pp.braces pp_supports_content ctx sq.supports_content

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  String.concat ""
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

let pp_nested_rule : nested_rule Pp.t =
 fun ctx -> function
  | Rule r ->
      let r =
        { r with declarations = deduplicate_declarations r.declarations }
      in
      pp_rule ctx r
  | Supports sq -> pp_supports_query ctx sq

let pp_layer_rules : nested_rule list Pp.t =
 fun ctx rules -> Pp.list ~sep:Pp.cut pp_nested_rule ctx rules

(* Generic at-rule pretty printer *)
let pp_at_rules ~at_rule ~condition ~name_part : rule list Pp.t =
 fun ctx rules ->
  Pp.char ctx '@';
  Pp.string ctx at_rule;
  Pp.space ctx ();
  if name_part <> "" then Pp.string ctx name_part;
  Pp.string ctx condition;
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx rules

let pp_media_rule : media_rule Pp.t =
 fun ctx mq ->
  pp_at_rules ~at_rule:"media" ~condition:mq.media_condition ~name_part:"" ctx
    mq.media_rules

let pp_layer_media : media_rule list Pp.t =
 fun ctx media_queries -> Pp.list ~sep:Pp.cut pp_media_rule ctx media_queries

let pp_container_rule : container_rule Pp.t =
 fun ctx cq ->
  let name_part =
    match cq.container_name with None -> "" | Some name -> name ^ " "
  in
  pp_at_rules ~at_rule:"container" ~condition:cq.container_condition ~name_part
    ctx cq.container_rules

let pp_layer_containers : container_rule list Pp.t =
 fun ctx container_queries ->
  Pp.list ~sep:Pp.cut pp_container_rule ctx container_queries

let pp_layer_supports : supports_rule list Pp.t =
 fun ctx supports_queries ->
  Pp.list ~sep:Pp.cut pp_supports_query ctx supports_queries

let pp_stylesheet_rules : rule list Pp.t =
 fun ctx rules -> Pp.list ~sep:Pp.cut pp_rule ctx rules

let pp_stylesheet_media : media_rule list Pp.t =
 fun ctx media_queries -> Pp.list ~sep:Pp.cut pp_media_rule ctx media_queries

let pp_stylesheet_containers : container_rule list Pp.t =
 fun ctx container_queries ->
  Pp.list ~sep:Pp.cut pp_container_rule ctx container_queries

let pp_stylesheet_supports : supports_rule list Pp.t =
 fun ctx supports_queries ->
  Pp.list ~sep:Pp.cut pp_supports_query ctx supports_queries

let pp_starting_style_rule ~optimize : starting_style_rule Pp.t =
 fun ctx ss ->
  let optimized =
    if optimize then ss.starting_rules |> merge_rules else ss.starting_rules
  in
  Pp.string ctx "@starting-style";
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx optimized

let pp_starting_styles ~optimize : starting_style_rule list Pp.t =
 fun ctx starting_styles ->
  Pp.list ~sep:Pp.cut (pp_starting_style_rule ~optimize) ctx starting_styles

let pp_property_rule : property_rule Pp.t =
 fun ctx at ->
  Pp.string ctx "@property ";
  Pp.string ctx at.name;
  Pp.sp ctx ();
  let pp_body ctx () =
    Pp.cut ctx ();
    Pp.nest 2
      (fun ctx () ->
        Pp.string ctx "syntax: \"";
        Pp.string ctx at.syntax;
        Pp.string ctx "\"";
        Pp.semicolon ctx ();
        Pp.cut ctx ();
        Pp.string ctx "inherits: ";
        Pp.string ctx (if at.inherits then "true" else "false");
        (match at.initial_value with
        | None | Some "" | Some "initial" -> ()
        | Some v ->
            Pp.semicolon ctx ();
            Pp.cut ctx ();
            Pp.string ctx "initial-value: ";
            Pp.string ctx v);
        Pp.semicolon ctx ())
      ctx ();
    Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

let pp_at_properties : property_rule list Pp.t =
 fun ctx at_properties -> Pp.list ~sep:Pp.cut pp_property_rule ctx at_properties

(* Helper functions for to_string *)
let is_layer_empty (layer : layer_rule) =
  layer.rules = [] && layer.media_queries = []
  && layer.container_queries = []
  && layer.supports_queries = []

let pp_layer : layer_rule Pp.t =
 fun ctx layer_rules ->
  let layer_name = layer_rules.layer in
  Pp.string ctx "@layer ";
  Pp.string ctx layer_name;
  if is_layer_empty layer_rules then Pp.semicolon ctx ()
  else (
    Pp.sp ctx ();
    let pp_body ctx () =
      let sep_if_needed prev_empty current =
        if (not prev_empty) && current <> [] then Pp.cut ctx ()
      in
      let rules_empty = layer_rules.rules = [] in
      pp_layer_rules ctx layer_rules.rules;
      sep_if_needed rules_empty layer_rules.media_queries;
      let media_empty = layer_rules.media_queries = [] in
      pp_layer_media ctx layer_rules.media_queries;
      sep_if_needed (rules_empty && media_empty) layer_rules.container_queries;
      let container_empty = layer_rules.container_queries = [] in
      pp_layer_containers ctx layer_rules.container_queries;
      sep_if_needed
        (rules_empty && media_empty && container_empty)
        layer_rules.supports_queries;
      pp_layer_supports ctx layer_rules.supports_queries
    in
    Pp.braces pp_body ctx ())

let pp_stylesheet_sections ~optimize : t Pp.t =
 fun ctx stylesheet ->
  let sep_if_needed prev_empty current =
    if (not prev_empty) && current <> [] then Pp.cut ctx ()
  in

  let rules_empty = stylesheet.rules = [] in
  pp_stylesheet_rules ctx stylesheet.rules;

  sep_if_needed rules_empty stylesheet.starting_styles;
  let starting_empty = stylesheet.starting_styles = [] in
  pp_starting_styles ~optimize ctx stylesheet.starting_styles;

  sep_if_needed (rules_empty && starting_empty) stylesheet.container_queries;
  let container_empty = stylesheet.container_queries = [] in
  pp_stylesheet_containers ctx stylesheet.container_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty)
    stylesheet.supports_queries;
  let supports_empty = stylesheet.supports_queries = [] in
  pp_stylesheet_supports ctx stylesheet.supports_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty && supports_empty)
    stylesheet.media_queries;
  let media_empty = stylesheet.media_queries = [] in
  pp_stylesheet_media ctx stylesheet.media_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty && supports_empty
   && media_empty)
    stylesheet.at_properties;
  pp_at_properties ctx stylesheet.at_properties

let pp_layers ~minify : layer_rule list Pp.t =
 fun ctx layers ->
  if not minify then Pp.list ~sep:Pp.cut pp_layer ctx layers
  else
    (* Merge consecutive empty layers into single @layer declarations *)
    let rec process_layers prev_empty current_empty_group = function
      | [] ->
          (* Finish any remaining empty group *)
          if current_empty_group = [] then ()
          else (
            if not prev_empty then Pp.cut ctx ();
            Pp.string ctx "@layer ";
            Pp.string ctx (String.concat "," (List.rev current_empty_group));
            Pp.semicolon ctx ())
      | layer :: rest ->
          if is_layer_empty layer then
            (* Collect empty layer names *)
            process_layers prev_empty (layer.layer :: current_empty_group) rest
          else (
            (* Non-empty layer, flush any empty group and add this layer *)
            if current_empty_group <> [] then (
              if not prev_empty then Pp.cut ctx ();
              Pp.string ctx "@layer ";
              Pp.string ctx (String.concat "," (List.rev current_empty_group));
              Pp.semicolon ctx ();
              Pp.cut ctx ())
            else if not prev_empty then Pp.cut ctx ();
            pp_layer ctx layer;
            process_layers false [] rest)
    in
    process_layers true [] layers

(* ======================================================================== *)
(* CSS Optimization *)
(* ======================================================================== *)

type layer_stats = {
  name : string;
  rules : int;
  selectors : string list; (* First few selectors as examples *)
}

(* Optimize a single rule by deduplicating its declarations *)
let optimize_single_rule (rule : rule) : rule =
  { rule with declarations = deduplicate_declarations rule.declarations }

(* Optimize a list of plain CSS rules *)
let optimize_rule_list (rules : rule list) : rule list =
  let deduped = List.map optimize_single_rule rules in
  let merged = merge_rules deduped in
  combine_identical_rules merged

(* Optimize nested rules (Rule | Supports) while preserving order *)
let optimize_nested_rules (rules : nested_rule list) : nested_rule list =
  (* Process rules in batches separated by non-Rule items *)
  let rec process_nested (acc : nested_rule list) (remaining : nested_rule list)
      : nested_rule list =
    match remaining with
    | [] -> List.rev acc
    | Rule r :: rest ->
        (* Collect consecutive Rule items *)
        let rec collect_rules (rules_acc : rule list) :
            nested_rule list -> rule list * nested_rule list = function
          | Rule r :: rest -> collect_rules (r :: rules_acc) rest
          | rest -> (List.rev rules_acc, rest)
        in
        let plain_rules, rest = collect_rules [ r ] rest in
        (* Optimize this batch of consecutive rules *)
        let optimized = optimize_rule_list plain_rules in
        let as_nested = List.map rule_to_nested optimized in
        process_nested (List.rev_append as_nested acc) rest
    | hd :: rest ->
        (* Non-Rule item (e.g., Supports) - keep as-is *)
        process_nested (hd :: acc) rest
  in
  process_nested [] rules

(* Optimize a layer_rule *)
let optimize_layer (layer : layer_rule) : layer_rule =
  let optimized_rules = optimize_nested_rules layer.rules in
  { layer with rules = optimized_rules }

(* Optimize a media rule *)
let optimize_media_rule (mq : media_rule) : media_rule =
  { mq with media_rules = optimize_rule_list mq.media_rules }

(* Optimize a container rule *)
let optimize_container_rule (cq : container_rule) : container_rule =
  { cq with container_rules = optimize_rule_list cq.container_rules }

let rec optimize_supports_rule (sq : supports_rule) : supports_rule =
  let optimized_content =
    match sq.supports_content with
    | Support_rules rules -> Support_rules (optimize_rule_list rules)
    | Support_nested (rules, nested) ->
        Support_nested
          (optimize_rule_list rules, List.map optimize_supports_rule nested)
  in
  { sq with supports_content = optimized_content }

let optimize (stylesheet : t) : t =
  (* Apply CSS optimizations while preserving cascade semantics *)
  let optimized_layers = List.map optimize_layer stylesheet.layers in
  (* When @supports blocks are present alongside top-level rules, we cannot
     safely merge the top-level rules because the stylesheet structure separates
     rules from @supports blocks into different lists, losing their relative
     ordering.

     However, we can still optimize if there are no top-level rules (everything
     is in layers/@supports/@media), or if there are no @supports blocks. *)
  let optimized_rules =
    if stylesheet.supports_queries = [] || stylesheet.rules = [] then
      (* Safe to optimize: either no @supports or no top-level rules to
         interfere *)
      optimize_rule_list stylesheet.rules
    else
      (* Both top-level rules and @supports exist - can't merge safely *)
      List.map optimize_single_rule stylesheet.rules
  in
  {
    stylesheet with
    layers = optimized_layers;
    rules = optimized_rules;
    media_queries = List.map optimize_media_rule stylesheet.media_queries;
    container_queries =
      List.map optimize_container_rule stylesheet.container_queries;
    supports_queries =
      List.map optimize_supports_rule stylesheet.supports_queries;
  }

let to_string ?(minify = false) ?optimize:(opt = false) ?(mode = Variables)
    stylesheet =
  let optimized_stylesheet = if opt then optimize stylesheet else stylesheet in
  let pp ctx () =
    (* Add header if there are layers *)
    if List.length stylesheet.layers > 0 then (
      Pp.string ctx header;
      Pp.cut ctx ());

    (* Render layers with merging for minified mode *)
    pp_layers ~minify ctx optimized_stylesheet.layers;

    (* Add separator if needed *)
    if
      optimized_stylesheet.layers <> []
      && (optimized_stylesheet.rules <> []
         || optimized_stylesheet.starting_styles <> []
         || optimized_stylesheet.container_queries <> []
         || optimized_stylesheet.supports_queries <> []
         || optimized_stylesheet.media_queries <> []
         || optimized_stylesheet.at_properties <> [])
    then Pp.cut ctx ();

    (* Render stylesheet sections *)
    pp_stylesheet_sections ~optimize:opt ctx optimized_stylesheet
  in
  Pp.to_string ~minify ~inline:(mode = Inline) pp ()

(** Extract all CSS variables from different input types *)

let vars_of_rules rules =
  List.concat_map (fun rule -> vars_of_declarations rule.declarations) rules

let vars_of_media_queries media_queries =
  List.concat_map (fun mq -> vars_of_rules mq.media_rules) media_queries

let vars_of_container_queries container_queries =
  List.concat_map (fun cq -> vars_of_rules cq.container_rules) container_queries

let vars_of_stylesheet (ss : t) =
  vars_of_rules ss.rules
  @ vars_of_media_queries ss.media_queries
  @ vars_of_container_queries ss.container_queries

let pp ?minify ?optimize ?mode stylesheet =
  to_string ?minify ?optimize ?mode stylesheet
