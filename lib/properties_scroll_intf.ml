(** Multi-column layout, scroll snapping, scroll timeline and overscroll value
    types. *)

open Values

(** CSS columns values for multi-column layout. *)
type columns_value =
  | Auto
  | Count of int
  | Width of length
  | Both of length * int
      (** [<column-width> <column-count>] per CSS Multicol 2 sec. 4.5. The two
          components can appear in either order in the source; we canonicalise
          to [<width>, <count>] internally so the printer always emits the width
          first. *)
  | Auto_count of int
      (** [auto <column-count>]: an explicit [auto] column-width paired with a
          count, e.g. [columns: auto 3]. Distinct from [Count] (bare
          [columns: 3]) so the explicit-[auto] spelling round-trips. *)
  | Count_calc of columns_value calc
      (** A count given as a math function with no width beside it. CSS Values 4
          sec. 10.12 checks the [1,inf] range on what it resolves to, so a call
          the literal grammar refuses is still a declaration. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of columns_value var

(* CSS Multicol 2 sec. 4.1: [column-width] is [auto | <length [0,inf]>]. *)

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-width}
     [column-width]}: [auto | <length [0,inf]>]. *)
type column_width =
  | Auto
  | Width of length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_width var

(* CSS Multicol 2 sec. 4.3: [column-count] is [auto | <integer [1,inf]>]. *)

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-count}
     [column-count]}: [auto | <integer [1,inf]>]. *)
type column_count =
  | Auto
  | Count of int
  | Calc of column_count calc
      (** CSS Values 4 sec. 10.1 puts a math function where the [<integer>]
          stands, and sec. 10.12 checks the [1,inf] range on what it resolves
          to, so a call the literal grammar refuses is still a declaration. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_count var

(* CSS Multicol 2 sec. 4.2: [column-height] is [auto | <length [0,inf]>]; it
   takes no percentage, which Chrome 146 refuses. Sec. 4.4 gives [column-wrap]
   the three keywords below. Both start at [auto]. *)

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-height}
     [column-height]}: [auto | <length [0,inf]>]. *)
type column_height =
  | Auto
  | Height of length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_height var

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-wrap}
     [column-wrap]}: [auto | nowrap | wrap]. *)
type column_wrap =
  | Auto
  | Nowrap
  | Wrap
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_wrap var

type column_span =
  | None
  | All
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_span var

(* Scroll Types *)

(** CSS scroll behavior values *)
type scroll_behavior =
  | Auto
  | Smooth
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_behavior var

(** CSS scroll-snap-align values *)
type scroll_snap_align =
  | None
  | Start
  | End
  | Center
  | Snap_align_pair of scroll_snap_align * scroll_snap_align
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_snap_align var

(** CSS scroll-snap-stop values *)
type scroll_snap_stop =
  | Normal
  | Always
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_snap_stop var

(** CSS scroll-snap-strictness values *)
type scroll_snap_strictness =
  | Mandatory
  | Proximity
  | Var of scroll_snap_strictness var

(** CSS scroll-snap axis values *)
type scroll_snap_axis =
  | None
  | X
  | Y
  | Block
  | Inline
  | Both
  | Var of scroll_snap_axis var

(** CSS scroll-snap-type values *)
type scroll_snap_type =
  | Axis of scroll_snap_axis (* Just the axis, no strictness *)
  | Axis_with_strictness of
      scroll_snap_axis
      * scroll_snap_strictness (* Axis with explicit strictness or var *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_snap_type var

type timeline_axis =
  | Block
  | Inline
  | X
  | Y
  | Axes of timeline_axis list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_axis var

(** Scroll-driven Animations 1 secs. 4.1 and 5.1: one timeline name, [none] or a
    [<dashed-ident>]. *)
type timeline_ident = None | Name of string

type timeline_name =
  | Names of timeline_ident list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_name var

(** Scroll-driven Animations 1 sec. 6 [timeline-scope]: [none] or a list of the
    names an element makes visible to its descendants. [none] stands for the
    whole value here, unlike the name of one timeline. *)
type timeline_scope =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_scope var

type timeline_shorthand_item = {
  name : timeline_ident;
  axis : timeline_axis option;
}

type timeline_shorthand =
  | Timelines of timeline_shorthand_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_shorthand var

(** One edge of Scroll-driven Animations 1
    {{:https://drafts.csswg.org/scroll-animations-1/#propdef-view-timeline-inset}
     [view-timeline-inset]}. *)
type timeline_inset_item = Auto | Length of length_percentage

(** Sec. 5.2 [view-timeline-inset]: the start edge then the end edge. *)
type timeline_inset =
  | Inset of timeline_inset_item * timeline_inset_item option
  | Insets of timeline_inset list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_inset var

type view_timeline_shorthand_item = {
  name : timeline_ident;
  axis : timeline_axis option;
  inset : timeline_inset option;
}

type view_timeline_shorthand =
  | Timelines of view_timeline_shorthand_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_timeline_shorthand var

(** CSS overscroll behavior values *)
type overscroll_behavior =
  | Auto
  | Contain
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of overscroll_behavior var
