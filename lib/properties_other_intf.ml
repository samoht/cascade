(** Direction and writing-mode, vendor-prefixed, and remaining value types:
    colour scheme, appearance, floats, transform origin and box, will-change and
    clip. *)

open Values
open Properties_background_intf

(* Direction Types *)

(** CSS direction values *)
type direction =
  | Ltr
  | Rtl
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of direction var

(** CSS unicode-bidi values *)
type unicode_bidi =
  | Normal
  | Embed
  | Isolate
  | Bidi_override
  | Isolate_override
  | Plaintext
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of unicode_bidi var

(** CSS writing-mode values *)
type writing_mode =
  | Horizontal_tb
  | Vertical_rl
  | Vertical_lr
  | Sideways_lr
  | Sideways_rl
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of writing_mode var

type text_combine_upright =
  | None
  | All
  | Digits of int option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_combine_upright var

(* Webkit & Mozilla Specific Types *)

(** [-webkit-appearance] is the alias Chrome keeps for [appearance], so it takes
    the [base-select] CSS Basic User Interface 4 sec. 5.1 gives that property
    along with the compatibility keywords of its own. *)
type webkit_appearance =
  | None  (** No appearance styling *)
  | Auto  (** Default browser styling *)
  | Button  (** Button appearance *)
  | Textfield  (** Text field appearance *)
  | Menulist  (** Select/dropdown appearance *)
  | Base_select  (** The base appearance of a select (Chrome alias) *)
  | Listbox  (** List box appearance *)
  | Checkbox  (** Checkbox appearance *)
  | Radio  (** Radio button appearance *)
  | Push_button  (** Push button appearance *)
  | Square_button  (** Square button appearance *)
  | Apple_pay_button  (** Apple Pay button appearance *)
  | Inherit  (** Inherit from parent *)
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_appearance var

(** CSS -webkit-font-smoothing values. *)
type webkit_font_smoothing =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_font_smoothing var

(** CSS -moz-osx-font-smoothing values. *)
type moz_osx_font_smoothing =
  | Auto
  | Grayscale
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of moz_osx_font_smoothing var

(** CSS webkit-box-orient values. *)
type webkit_box_orient =
  | Horizontal
  | Vertical
  | Inline_axis
  | Block_axis
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_box_orient var

type moz_orient =
  | Inline
  | Block
  | Horizontal
  | Vertical
  | Inherit
  | Var of moz_orient var

(** CSS -webkit-line-clamp values. *)
type webkit_line_clamp =
  | None
  | Lines of int
  | Calc of webkit_line_clamp calc
      (** CSS Values 4 sec. 10 allows a math function wherever an [<integer>] is
          allowed, and sec. 10.12 rounds its result, so a fractional call is a
          line count where the bare fraction is not. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_line_clamp var

(** CSS text-size-adjust values (including vendor prefixes). *)
type text_size_adjust =
  | None
  | Auto
  | Pct of float
  | Calc of text_size_adjust calc
      (** CSS Values 4 sec. 10.1 puts a math function where the [<percentage>]
          stands, and sec. 10.12 checks the [0,inf] range on what it resolves
          to. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_size_adjust var

(* Other Types *)

(** CSS forced-color-adjust values. *)
type forced_color_adjust =
  | Auto
  | None
  | Preserve_parent_color
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of forced_color_adjust var

type color_scheme =
  | Normal
  | Light
  | Dark
  | Light_dark
  | Only_light
  | Only_dark
  | Only_light_dark
  | Custom of string list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of color_scheme var

type appearance =
  | None
  | Auto
  | Button
  | Textfield
  | Menulist
  | Base_select
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of appearance var

(** CSS print-color-adjust values. *)
type print_color_adjust =
  | Economy
  | Exact
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of print_color_adjust var

type box_decoration_break =
  | Clone
  | Slice
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of box_decoration_break var

(** CSS clear values. *)
type clear =
  | None
  | Left
  | Right
  | Both
  | Inline_start
  | Inline_end
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of clear var

(** CSS float side values. *)
type float_side =
  | None
  | Left
  | Right
  | Inline_start
  | Inline_end
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of float_side var

(** CSS text-decoration-skip-ink values *)
type text_decoration_skip_ink =
  | Auto
  | None
  | All
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_ink var

type transform_origin =
  | Center
  | Center_center
  | Left
  | Right
  | Top
  | Bottom
  | Left_top
  | Left_center
  | Left_bottom
  | Right_top
  | Right_center
  | Right_bottom
  | Center_top
  | Center_bottom
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Position of position_value
  | X of length  (** Single x-offset, y defaults to 50% *)
  | XY of length * length
  | XYZ of length * length * length
  | Position_z of position_value * length
  | Initial
  | Inherit  (** Transform origin (2D or 3D). *)
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_origin var

(* transform-box property: establishes the reference box for transform *)

(** CSS transform-box property values *)
type transform_box =
  | Content_box
  | Border_box
  | Fill_box
  | Stroke_box
  | View_box
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_box var

(* will-change property: which properties will animate *)

(** CSS will-change property values for performance optimization hints. *)
type will_change =
  | Will_change_auto
  | Scroll_position
  | Contents
  | Transform
  | Opacity
  | Properties of string list  (** Custom CSS property names *)
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of will_change var

(* perspective-origin is the CSS <position> grammar. *)

type perspective_origin = position_value
(** CSS perspective-origin values for 3D transforms. *)

(** CSS clip property values (deprecated, but needed for sr-only). *)
type clip =
  | Clip_auto
  | Clip_rect of length * length * length * length
      (** top, right, bottom, left *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of clip var
