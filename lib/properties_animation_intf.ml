(** Transform, transition, animation, blend, shadow and filter value types. *)

open Values

(* Transform & Animation Types *)

(** CSS transform values *)
type transform =
  | Translate of length * length option
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate_3d of length * length * length
  | Rotate of angle
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate_3d of float * float * float * angle
  | Rotate_axis of float * float * float * angle
  | Scale of number_percentage * number_percentage option
  | Scale_space of number_percentage * number_percentage
  | Scale_x of number_percentage
  | Scale_y of number_percentage
  | Scale_z of number_percentage
  | Scale_3d of number_percentage * number_percentage * number_percentage
  | Skew of angle * angle option
  | Skew_x of angle
  | Skew_y of angle
  | Matrix of float * float * float * float * float * float
  | Matrix_3d of
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
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | List of transform list
  | Var of transform var

type transforms = transform list

(** CSS transform-style values *)
type transform_style =
  | Flat
  | Preserve_3d
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_style var

(** CSS backface-visibility values *)
type backface_visibility =
  | Visible
  | Hidden
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of backface_visibility var

(** CSS scale property values *)
type scale =
  | X of number_percentage
  | XY of number_percentage * number_percentage
  | XYZ of number_percentage * number_percentage * number_percentage
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scale var

type translate_value =
  | X of length
  | XY of length * length
  | XYZ of length * length * length
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of translate_value var

(** CSS rotate property values *)
type rotate_value =
  | Angle of angle  (** z-axis rotation *)
  | X of angle  (** x-axis rotation *)
  | Y of angle  (** y-axis rotation *)
  | Z of angle  (** z-axis rotation (explicit) *)
  | Axis of float * float * float * angle  (** custom axis rotation *)
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of rotate_value var

(** CSS steps direction values. *)
type steps_direction =
  | Jump_start
  | Jump_end
  | Jump_none
  | Jump_both
  | Start
  | End
  | Var of steps_direction var

(** CSS animation timing function values. *)
type timing_function =
  | Ease
  | Linear
  | Ease_in
  | Ease_out
  | Ease_in_out
  | Step_start
  | Step_end
  | Steps of int * steps_direction option
  | Cubic_bezier of float * float * float * float
  | Linear_function of string
  | Timing_functions of timing_function list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of timing_function var

(** CSS transition property value. *)
type transition_property_value =
  | All
  | None
  | Property of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transition_property_value var

type transition_property = transition_property_value list
(** CSS transition property (list of property values). *)

(** CSS transition-behavior values (Transitions Level 2). *)
type transition_behavior =
  | Normal
  | Allow_discrete
  | Behaviors of transition_behavior list
      (** The [<transition-behavior-value>#] list of two or more behaviours. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of transition_behavior var

(** CSS Positioned Layout 4
    {{:https://drafts.csswg.org/css-position-4/#propdef-overlay} [overlay]}:
    whether the box is in the top layer. *)
type overlay =
  | Auto
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of overlay var

type transition_shorthand = {
  property : transition_property_value;
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  behavior : transition_behavior option;
}
(** CSS transition shorthand values. *)

type transition =
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | None
  | Shorthand of transition_shorthand  (** CSS transition values. *)
  | Var of transition var

(** CSS animation direction values *)
type animation_direction =
  | Normal
  | Reverse
  | Alternate
  | Alternate_reverse
  | Directions of animation_direction list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_direction var

(** CSS animation fill mode values *)
type animation_fill_mode =
  | None
  | Forwards
  | Backwards
  | Both
  | Fill_modes of animation_fill_mode list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_fill_mode var

(** CSS animation iteration count values *)
type animation_iteration_count =
  | Count of number
  | Infinite
  | Counts of animation_iteration_count list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_iteration_count var

(** CSS animation play state values *)
type animation_play_state =
  | Running
  | Paused
  | States of animation_play_state list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_play_state var

(** How one animation composes with the value beneath it, for CSS Animations 2
    {{:https://drafts.csswg.org/css-animations-2/#propdef-animation-composition}
     [animation-composition]}. *)
type animation_composition_item = Replace | Add | Accumulate

(** CSS Animations 2
    {{:https://drafts.csswg.org/css-animations-2/#propdef-animation-composition}
     [animation-composition]}. *)
type animation_composition =
  | Compositions of animation_composition_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_composition var

type animation_name =
  | None
  | Name of string
  | Ambiguous of string
  | Quoted of string
  | Names of animation_name list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_name var

type animation_shorthand = {
  name : animation_name option; (* Optional animation name, defaults to None *)
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  iteration_count : animation_iteration_count option;
  direction : animation_direction option;
  fill_mode : animation_fill_mode option;
  play_state : animation_play_state option;
  timeline : animation_timeline option;
}
(** CSS animation shorthand values *)

and animation_timeline =
  | None
  | Auto
  | Name of string
  | Scroll of string
  | View of string
  | Timelines of animation_timeline list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_timeline var

type animation =
  | Inherit
  | Initial
  | None (* Special case for "animation: none" *)
  | Shorthand of animation_shorthand (* Requires a name *)
  | Var of animation var

(* Visual Effects Types *)

(** CSS blend-mode values *)
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
  | Plus_darker
  | Plus_lighter
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of blend_mode var

type shadow =
  | Shadow of shadow_body  (** A non-inset shadow. *)
  | Inset of inset  (** An inset shadow. *)
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | List of shadow list
  | Var of shadow var

and shadow_body = {
  h_offset : length;
  v_offset : length;
  blur : length option;
  spread : length option;
  color : color option;
}
(** The [<length>{2,4} && <color>?] body of a single [<shadow>]. *)

and inset =
  | Var of shadow var  (** [inset var(--x)]: the whole body from one var. *)
  | Body of shadow_body  (** [inset 2px 4px red]: a concrete inset body. *)
  | Toggle of { name : string; no_fallback : bool; body : shadow_body }
      (** [var(--name) <body>]: a dynamic inset toggle (Tailwind's ring system).
      *)

(** CSS text-shadow values *)
type text_shadow =
  | None
  | Text_shadow of {
      h_offset : length;
      v_offset : length;
      blur : length option;
      color : color option;
    }
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of text_shadow var

(** Filter functions with an optional argument. *)
type filter_function =
  | Blur_function
  | Brightness_function
  | Contrast_function
  | Grayscale_function
  | Hue_rotate_function
  | Invert_function
  | Opacity_function
  | Saturate_function
  | Sepia_function

(** CSS filter values *)
type filter =
  | None  (** No filter *)
  | Omitted of filter_function  (** Function with its argument omitted. *)
  | Blur of length  (** blur(px) *)
  | Brightness of number_percentage  (** brightness(%) *)
  | Contrast of number_percentage  (** contrast(%) *)
  | Drop_shadow of shadow  (** drop-shadow(...) *)
  | Grayscale of number_percentage  (** grayscale(%) *)
  | Hue_rotate of angle  (** hue-rotate(deg) *)
  | Invert of number_percentage  (** invert(%) *)
  | Opacity of number_percentage  (** opacity(%) *)
  | Saturate of number_percentage  (** saturate(%) *)
  | Sepia of number_percentage  (** sepia(%) *)
  | Url of string  (** url(...) *)
  | List of filter list  (** Multiple filters *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of filter var
