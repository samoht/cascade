(** User-interaction and box-model value types. *)

open Values
open Properties_background_intf

(* User Interaction Types *)
type cursor =
  | Auto
  | Default
  | None
  | Context_menu
  | Help
  | Pointer
  | Progress
  | Wait
  | Cell
  | Crosshair
  | Text
  | Vertical_text
  | Alias
  | Copy
  | Move
  | No_drop
  | Not_allowed
  | Grab
  | Grabbing
  | E_resize
  | N_resize
  | Ne_resize
  | Nw_resize
  | S_resize
  | Se_resize
  | Sw_resize
  | W_resize
  | Ew_resize
  | Ns_resize
  | Nesw_resize
  | Nwse_resize
  | Col_resize
  | Row_resize
  | All_scroll
  | Zoom_in
  | Zoom_out
  | Url of string * (float * float) option * cursor
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of cursor var

type interactivity =
  | Auto
  | Inert
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of interactivity var

type caret_animation =
  | Auto
  | Manual
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caret_animation var

type caret_shape =
  | Auto
  | Bar
  | Block
  | Underscore
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caret_shape var

type caret =
  | Auto
  | Caret of color option * caret_animation option * caret_shape option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caret var

(** One slot of an interest delay: CSS UI 4 sec. 6.4 gives
    [interest-delay-start] and [interest-delay-end] the value [normal | <time>].
*)
type interest_delay_item = Normal | Time of duration

type interest_delay =
  | Delays of interest_delay_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of interest_delay var

type nav_scope = Current | Root | Named of string

type nav =
  | Auto
  | Target of string * nav_scope option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of nav var

type user_select =
  | None
  | Auto
  | Text
  | All
  | Contain
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of user_select var

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
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of pointer_events var

type touch_action =
  | Auto
  | None
  | Pan_x
  | Pan_y
  | Pan_left
  | Pan_right
  | Pan_up
  | Pan_down
  | Pinch_zoom
  | Manipulation
  | Actions of touch_action list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Vars of touch_action var list
      (** Variable references like var(--tw-pan-x,) var(--tw-pan-y,) *)
  | Var of touch_action var

type resize =
  | None
  | Both
  | Horizontal
  | Vertical
  | Block
  | Inline
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of resize var

(* Box Model Types *)
type box_sizing =
  | Border_box
  | Content_box
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of box_sizing var

type field_sizing =
  | Content
  | Fixed
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of field_sizing var

type caption_side =
  | Top
  | Bottom
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caption_side var

type object_fit =
  | Fill
  | Contain
  | Cover
  | None
  | Scale_down
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of object_fit var

type object_view_box =
  | None
  | Inset of length * length option * length option * length option
  | Xywh of {
      x : length_percentage;
      y : length_percentage;
      width : length_percentage;
      height : length_percentage;
      rounded : border_radius option;
    }
  | Rect of {
      top : length_percentage;
      right : length_percentage;
      bottom : length_percentage;
      left : length_percentage;
      rounded : border_radius option;
    }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of object_view_box var
