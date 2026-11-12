(** Background, gradient, image, mask, border-image and gap value types. *)

open Values
open Properties_border_intf

(* Background Types *)
type background_attachment =
  | Scroll
  | Fixed
  | Local
  | Layers of background_attachment list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of background_attachment var

type background_box =
  | Border_box
  | Padding_box
  | Content_box
  | Text
  | Layers of background_box list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_box var

type background_repeat =
  | Repeat
  | Space
  | Round
  | No_repeat
  | Repeat_x
  | Repeat_y
  | Layers of background_repeat list
  | Repeat_repeat
  | Repeat_space
  | Repeat_round
  | Repeat_no_repeat
  | Space_repeat
  | Space_space
  | Space_round
  | Space_no_repeat
  | Round_repeat
  | Round_space
  | Round_round
  | Round_no_repeat
  | No_repeat_repeat
  | No_repeat_space
  | No_repeat_round
  | No_repeat_no_repeat
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_repeat var

type background_size =
  | Auto
  | Cover
  | Contain
  | Length of length
  | Size of length * length
  | Layers of background_size list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_size var

(** CSS Color 5 section 9.1: optional hue-interpolation method that follows a
    polar color space (lch / oklch / hsl / hwb). *)
type hue_interpolation_method = Shorter | Longer | Increasing | Decreasing

(** CSS Color 5 sec. 9 spells one [<color-interpolation-method>] wherever one
    appears, over the same fifteen spaces [color-mix()] takes, so the space is
    carried rather than named in a constructor of its own. Sec. 9.1 puts the
    [<hue-interpolation-method>] after a polar space only, which is why the
    reader answers [None] for every other space. *)
type color_interpolation =
  | In of color_space * hue_interpolation_method option
  | Var of color_interpolation var

type gradient_direction =
  | Default_direction
  | To_top
  | To_top_right
  | To_right
  | To_bottom_right
  | To_bottom
  | To_bottom_left
  | To_left
  | To_top_left
  | Angle of angle
  | With_interpolation of gradient_direction * color_interpolation
  | Var of gradient_direction var

type radial_shape = Circle | Ellipse | Var of radial_shape var

type radial_size =
  | Closest_side
  | Farthest_side
  | Closest_corner
  | Farthest_corner
  | Circle_radius of length
  | Ellipse_radii of length_percentage * length_percentage
  | Var of radial_size var

type position_value =
  | Center
  | Top
  | Bottom
  | Left
  | Right
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
  | XY of length * length
  | Single of length
      (** Single length/percentage value for background-position *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  (* 3-value syntax: edge offset axis (e.g., "right 0.5rem center") *)
  | Edge_offset_axis of string * length_percentage * string
  | Axis_edge_offset of string * string * length_percentage
  (* 4-value syntax: edge1 offset1 edge2 offset2 *)
  | Edge_offset_edge_offset of
      string * length_percentage * string * length_percentage
  | Var of position_value var

(* CSS Backgrounds 4 sec. 3.3: [background-position-x] and
   [background-position-y] each take [center | [<edge>? <length-percentage>]].
   One type serves both, each edge keyword naming exactly one axis, and the
   per-property reader refuses the other axis's keywords. *)
type position_axis_edge = Left | Right | Top | Bottom

type background_position_axis =
  | Center
  | Edge of position_axis_edge
  | Offset of length_percentage
  | Edge_offset of position_axis_edge * length_percentage
  | Layers of background_position_axis list
      (** CSS Backgrounds 4 sec. 3.6 spells the axis longhand with the same [#]
          the pair carries, so it names one position per background layer. A
          single position is the bare constructor, the way [mask] holds one
          layer beside its list. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_position_axis var

type radial_gradient_config = {
  shape : radial_shape option;
  size : radial_size option;
  position : position_value option;
  interpolation : color_interpolation option;
}

type border_radius =
  | Radius of {
      horizontal : length_percentage list;
          (** 1-4 horizontal radii (top-left, top-right, bottom-right,
              bottom-left). *)
      vertical : length_percentage list option;
          (** Optional 1-4 vertical radii after [/]; when [None] the horizontal
              values are used for both axes. *)
    }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_radius var  (** Per CSS Backgrounds and Borders 3 sec. 4.1. *)

type conic_gradient_config = {
  angle : angle option;  (** [from <angle>] starting angle *)
  position : position_value option;  (** [at <position>] center *)
  interpolation : color_interpolation option;
      (** Optional [in <color-interpolation-method>] clause. *)
}

type gradient_position =
  | Linear_position of gradient_direction
  | Radial_position of radial_gradient_config
  | Conic_position of conic_gradient_config
  | Var of gradient_position var

type gradient_stop =
  | Color_percentage of
      color * length_percentage option * length_percentage option
    (* Color with optional length-percentage position *)
  | Color_length of
      color
      * length option
      * length option (* Color with optional length position *)
  | Length of length (* Interpolation hint with length, e.g., "50px" *)
  | Channel of channel
      (** Residual numeric channel token from custom-property substitution. *)
  | List of
      gradient_stop list (* Multiple gradient stops - used for var fallbacks *)
  | Percentage of
      percentage (* Interpolation hint with percentage, e.g., "50%" *)
  | Position of gradient_position
  | Direction of gradient_direction
  | Var of gradient_stop var
(* Gradient direction for stops, e.g., "to right" or Var *)

module Webkit_gradient = struct
  type point = Left_top | Left_bottom | Center | Position of position_value
  type stop = From of color | Color_stop of percentage * color | To of color

  type t =
    | Linear of { start : point; finish : point; stops : stop list }
    | Radial of {
        inner_center : point;
        inner_radius : float;
        outer_center : point;
        outer_radius : float;
        stops : stop list;
      }
end

type background_image =
  | Url of string
  | Quoted of string * char
  | Linear_gradient of gradient_direction * gradient_stop list
  | Linear_gradient_var of gradient_stop var
      (** Linear gradient using a single variable for all stops including
          position. Outputs: linear-gradient(var(--tw-gradient-stops)) *)
  | Radial_gradient of radial_gradient_config * gradient_stop list
  | Radial_gradient_var of gradient_stop var
      (** Radial gradient using a single variable for all stops. Outputs:
          radial-gradient(var(--tw-gradient-stops)) *)
  | Conic_gradient of conic_gradient_config * gradient_stop list
  | Conic_gradient_var of gradient_stop var
      (** Conic gradient using a single variable for all stops. Outputs:
          conic-gradient(var(--tw-gradient-stops)) *)
  | Repeating_linear_gradient of gradient_direction * gradient_stop list
  | Repeating_radial_gradient of radial_gradient_config * gradient_stop list
  | Repeating_conic_gradient of conic_gradient_config * gradient_stop list
      (** [repeating-{linear,radial,conic}-gradient()] CSS Images 4 sec. 3. *)
  | Webkit_linear_gradient of gradient_direction * gradient_stop list
  | Webkit_repeating_linear_gradient of gradient_direction * gradient_stop list
  | Webkit_radial_gradient of radial_gradient_config * gradient_stop list
  | Webkit_repeating_radial_gradient of
      radial_gradient_config * gradient_stop list
  | Moz_linear_gradient of gradient_direction * gradient_stop list
  | Moz_repeating_linear_gradient of gradient_direction * gradient_stop list
  | Moz_radial_gradient of radial_gradient_config * gradient_stop list
  | Moz_repeating_radial_gradient of radial_gradient_config * gradient_stop list
  | O_linear_gradient of gradient_direction * gradient_stop list
  | O_repeating_linear_gradient of gradient_direction * gradient_stop list
  | O_radial_gradient of radial_gradient_config * gradient_stop list
  | O_repeating_radial_gradient of radial_gradient_config * gradient_stop list
  | Image_set of image_set_option list
      (** [image-set(<source>#)] CSS Images 4 *)
  | Webkit_image_set of image_set_option list
      (** [-webkit-image-set(<source>#)] legacy spelling *)
  | Cross_fade of cross_fade_option list
      (** [cross-fade(<cf-mixing-image>#)] CSS Images 4 *)
  | Webkit_gradient of Webkit_gradient.t
  | List of background_image list
      (** Comma-separated list of background images *)
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of background_image var
      (** CSS variable reference: var(--my-gradient) *)

and image_set_option = {
  source : image_set_source;
  resolution : string option;  (** [<resolution>] like ["1x"] or ["300dpi"] *)
  mime_type : string option;  (** [type("image/avif")] *)
}

and image_set_source = Url of string | String of string

and cross_fade_option = {
  image : background_image;
  percent : percentage option;
}

(* CSS Lists 3 sec. 3.5: [list-style-image] is a [<image>], which is the
   [background_image] vocabulary without its comma list, or [none]. *)
type list_style_image =
  | None
  | Image of background_image
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style_image var

(* CSS Lists 3 sec. 3.6: [list-style] is the shorthand for [list-style-type],
   [list-style-position], and [list-style-image]. All components are optional;
   omitted ones reset to the longhand initial ([disc] / [outside] / [none]). The
   single bare [none] keyword in the source sets both [type_] and [image] to
   [None]. *)
type list_style_shorthand = {
  type_ : list_style_type option;
  position : list_style_position option;
  image : list_style_image option;
}

type list_style =
  | Shorthand of list_style_shorthand
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style var

(* Background position can be complex with 1-4 values mixing keywords and
   lengths *)
type background_position = position_value list

(* Structured background type for the shorthand property *)
type background_shorthand = {
  color : color option;
  image : background_image option;
  position : position_value option;
  size : background_size option;
  repeat : background_repeat option;
  attachment : background_attachment option;
  clip : background_box option;
  origin : background_box option;
}

type background =
  | Inherit
  | Initial
  | Unset
  | None
  | Shorthand of background_shorthand
  | Var of background var
  | Vars of background var list
(* Mask Types *)

(** Webkit-prefixed mask-composite values *)
type webkit_mask_composite =
  | Source_over
  | Source_in
  | Source_out
  | Source_atop
  | Destination_over
  | Destination_in
  | Destination_out
  | Destination_atop
  | Xor
  | Plus_lighter
  | Clear
  | Copy
  | Composites of webkit_mask_composite list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_mask_composite var
      (** Standard mask-composite values (different from webkit) *)

type mask_composite =
  | Add
  | Subtract
  | Intersect
  | Exclude
  | Composites of mask_composite list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_composite var  (** Webkit mask-source-type values *)

type webkit_mask_source_type =
  | Alpha
  | Luminance
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_mask_source_type var
      (** Standard mask-mode values (different from webkit) *)

type mask_mode =
  | Alpha
  | Luminance
  | Match_source
  | Modes of mask_mode list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_mode var
      (** mask-type property values (only alpha and luminance) *)

type mask_type =
  | Alpha
  | Luminance
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_type var

type mask_box =
  | Border_box
  | Content_box
  | Fill_box
  | Padding_box
  | Stroke_box
  | View_box
  | No_clip  (** Only valid for mask-clip *)
  | Layers of mask_box list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_box var

(** The box vocabulary of [-webkit-mask-origin] and [-webkit-mask-clip], which
    is WebKit's older set rather than the [<coord-box>] CSS Masking 1 sec. 6.4
    and 6.5 give the unprefixed pair: the three CSS box names in both their bare
    and [-box] spellings, none of the SVG boxes, and [text] on the clip. *)
type webkit_mask_box =
  | Border
  | Border_box
  | Content
  | Content_box
  | Padding
  | Padding_box
  | Text  (** Only valid for -webkit-mask-clip *)
  | Layers of webkit_mask_box list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_mask_box var

type mask_layer = {
  image : background_image option;
  position : position_value option;
  size : background_size option;
  repeat : background_repeat option;
  origin : mask_box option;
  clip : mask_box option;
  mode : mask_mode option;
  composite : mask_composite option;
}

type mask =
  | None
  | Layer of mask_layer
  | Layers of mask_layer list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of mask var

(* CSS Backgrounds 3 sec. 5.2 gives each offset a [<number [0,inf]>] or a
   [<percentage [0,inf]>]. [Number] carries the math on the number side; [Calc]
   carries it on the percentage side, which has no other home. *)

(** CSS Backgrounds 3 sec. 5.2 to 5.4 write the numeric halves of the
    border-image slots as [<number [0,inf]>], which a [calc()] satisfies, so
    each carries a {!type-number} rather than a float. *)
type border_image_slice_item =
  | Number of number
  | Pct of float
  | Calc of border_image_slice_item calc

type border_image_slice_offsets = {
  offsets : border_image_slice_item list;
  fill : bool;
}

(* CSS Backgrounds 3 sec. 5.2: [border-image-slice] is one to four offsets with
   an optional [fill]; the longhand also takes the CSS-wide keywords. *)
type border_image_slice =
  | Slices of border_image_slice_offsets
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_slice var

type border_image_width_item =
  | Number of number
  | Pct of float
  | Length of length
  | Auto

type border_image_outset_item = Number of number | Length of length
type border_image_repeat_keyword = Stretch | Repeat | Round | Space

(* CSS Backgrounds 3 sec. 5.5: [border-image-repeat] is one or two keywords
   (block then inline); the longhand also takes the CSS-wide keywords. *)
type border_image_repeat =
  | Repeats of border_image_repeat_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_repeat var

(* CSS Backgrounds 3 sec. 5.3: [border-image-width] is one to four items; the
   longhand also takes the CSS-wide keywords. *)
type border_image_width =
  | Widths of border_image_width_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_width var

(* CSS Backgrounds 3 sec. 5.4: [border-image-outset] is one to four items; the
   longhand also takes the CSS-wide keywords. *)
type border_image_outset =
  | Outsets of border_image_outset_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_outset var

(** CSS Masking 1 sec. 8.2 [<mask-border-mode>]: shared with the [border_image]
    record because [mask-border] is otherwise the same shorthand as
    [border-image] (the mode is always [None] for the latter). *)
type mask_border_mode = Alpha | Luminance

type border_image = {
  source : background_image option;
  slice : border_image_slice_offsets option;
  width : border_image_width_item list option;
  outset : border_image_outset_item list option;
  repeat : border_image_repeat_keyword list option;
  mode : mask_border_mode option;
}

(* Gap shorthand type *)
type gap =
  | Lengths of { row_gap : length option; column_gap : length option }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of gap var
