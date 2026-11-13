(** Generated content, container, anchor positioning, scrollbar, image
    rendering, intrinsic sizing, clip-path, motion path, containment,
    fragmentation and page value types. *)

open Values
open Properties_background_intf

(* Content Types *)

(** CSS Generated Content 3 sec. 2 [content]: the replacement and list forms
    both take an [<image>], which is the {!type-background_image} vocabulary
    without its comma list. *)
type content =
  | String of string
  | Quoted of { value : string; quote : char; repr : string option }
  | Image of background_image
      (** The [<image>] of {!type-background_image}, aliased below. *)
  | None
  | Normal
  | Open_quote
  | Close_quote
  | Attr of content attr_call
  | Counter of string
  | Counters of string * string
  | String_ref of string
  | Content_list of content list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of content var

type counter_item = { name : string; value : int option }

type counter_set =
  | None
  | Counters of counter_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of counter_set var

(** CSS content-visibility values. *)
type content_visibility =
  | Visible  (** Content is visible and rendered *)
  | Hidden  (** Content is hidden from rendering *)
  | Auto  (** Browser determines visibility based on relevance *)
  | Initial
  | Inherit  (** Inherit from parent *)
  | Unset
  | Revert
  | Revert_layer
  | Var of content_visibility var
      (** The CSS quotes property - defines quotation marks *)

(** CSS quotes property values - defines quotation marks for q and blockquote.
*)
type quotes =
  | Auto  (** Browser default based on language *)
  | None  (** No quotation marks *)
  | Pairs of (string * string) list  (** One or more open/close pairs *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of quotes var

(* Container Types *)

(** CSS container-type values *)
type container_type =
  | Size
  | Inline_size
  | Scroll_state
  | Normal
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of container_type var

type container_name =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of container_name var

(** Sec. 2.1 [anchor-name]: [none | <dashed-ident>#]. *)
type anchor_name =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of anchor_name var

(** Sec. 3.1.2 [<position-area>]: one of the grid keywords naming a region
    around the anchor. *)
type position_area_keyword =
  | Top
  | Bottom
  | Left
  | Right
  | Center
  | Span_top
  | Span_bottom
  | Span_left
  | Span_right
  | X_start
  | X_end
  | Y_start
  | Y_end
  | Span_x_start
  | Span_x_end
  | Span_y_start
  | Span_y_end
  | Inline_start
  | Inline_end
  | Block_start
  | Block_end
  | Span_inline_start
  | Span_inline_end
  | Span_block_start
  | Span_block_end
  | Start
  | End
  | Span_start
  | Span_end
  | Self_start
  | Self_end
  | Span_self_start
  | Span_self_end
  | Self_x_start
  | Self_x_end
  | Self_y_start
  | Self_y_end
  | Span_self_x_start
  | Span_self_x_end
  | Span_self_y_start
  | Span_self_y_end
  | Self_block_start
  | Self_block_end
  | Self_inline_start
  | Self_inline_end
  | Span_self_block_start
  | Span_self_block_end
  | Span_self_inline_start
  | Span_self_inline_end
  | Span_all

(** CSS Anchor Positioning 1 sec. 4.1 [position-anchor]:
    [normal | none | auto | <anchor-name>]. *)
type position_anchor =
  | Normal
  | None
  | Auto
  | Anchor of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_anchor var

(** Sec. 6.1 [<try-tactic>] and the [<dashed-ident>] naming a [@position-try]
    rule. *)
type position_try_fallback =
  | Flip_block
  | Flip_inline
  | Flip_start
  | Name of string

(** CSS Anchor Positioning 1 sec. 6.1: each comma-separated entry is
    [[<dashed-ident> || <try-tactic>] | <position-area>], so the two branches
    never mix inside one entry. *)
type position_try_fallback_entry =
  | Tactics of position_try_fallback list
  | Area of position_area_keyword * position_area_keyword option

(** Sec. 6.1 [position-try-fallbacks]. *)
type position_try_fallbacks =
  | None
  | Fallbacks of position_try_fallback_entry list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try_fallbacks var

(** Sec. 6.2 [position-try-order]: [normal | <try-size>]. *)
type position_try_order =
  | Normal
  | Most_width
  | Most_height
  | Most_block_size
  | Most_inline_size
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try_order var

(* CSS Anchor Positioning 1 sec. 6.3: [position-try] is [<'position-try-order'>
   || <'position-try-fallbacks'>]. A [Normal] order is the initial value and is
   omitted from the serialisation. *)

(** Sec. 6.3 [position-try]:
    [<'position-try-order'>? <'position-try-fallbacks'>]. *)
type position_try =
  | Try of position_try_order * position_try_fallbacks
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try var

(** Sec. 7 [<anchor-visibility>]: one condition that hides the box. *)
type position_visibility_condition = Anchors_visible | No_overflow

(** Sec. 7 [position-visibility]. *)
type position_visibility =
  | Always
  | Conditions of position_visibility_condition list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_visibility var

(** Sec. 3.1.2 [position-area]: one or two keywords from a single branch of the
    grammar. *)
type position_area =
  | None
  | Area of position_area_keyword * position_area_keyword option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_area var

(** CSS Scroll Anchoring 1
    {{:https://drafts.csswg.org/css-scroll-anchoring-1/#propdef-overflow-anchor}
     [overflow-anchor]}. *)
type overflow_anchor =
  | Auto
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of overflow_anchor var

type scrollbar_width =
  | Auto
  | Thin
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of scrollbar_width var

type scrollbar_color =
  | Auto
  | Colors of color * color
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of scrollbar_color var

type scrollbar_gutter =
  | Auto
  | Stable
  | Stable_both_edges
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of scrollbar_gutter var

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-palette}
     [font-palette]}. *)
type font_palette =
  | Normal
  | Light
  | Dark
  | Palette of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of font_palette var

(** One face the browser may synthesise, for CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-synthesis}
     [font-synthesis]}. *)
type font_synthesis_feature = Weight | Style | Small_caps | Position

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-synthesis}
     [font-synthesis]}. *)
type font_synthesis =
  | None
  | Features of font_synthesis_feature list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis var

(** Sec. 6.2 [<timeline-range-name>]: the named part of a view progress
    timeline. *)
type animation_range_name =
  | Cover
  | Contain
  | Entry
  | Exit
  | Entry_crossing
  | Exit_crossing

(** Sec. 6.2: one end of [animation-range]. *)
type animation_range_item =
  | Normal
  | Items of animation_range_item list
  | Offset of length_percentage
  | Named of animation_range_name * length_percentage option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_range_item var

(** Sec. 6.2 [animation-range]: the start then the end. *)
type animation_range =
  | Range of animation_range_item * animation_range_item option
  | Ranges of animation_range list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_range var

(** View Transitions 1
    {{:https://drafts.csswg.org/css-view-transitions-1/#propdef-view-transition-name}
     [view-transition-name]}, with the [match-element] of Level 2. *)
type view_transition_name =
  | None
  | Match_element
  | Name of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_transition_name var

(** View Transitions 2
    {{:https://drafts.csswg.org/css-view-transitions-2/#propdef-view-transition-class}
     [view-transition-class]}: [none | <custom-ident>+]. *)
type view_transition_class =
  | None
  | Classes of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_transition_class var

(** CSS Images 3
    {{:https://drafts.csswg.org/css-images-3/#propdef-image-orientation}
     [image-orientation]}. *)
type image_orientation =
  | None
  | From_image
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_orientation var

(** CSS Images 3
    {{:https://drafts.csswg.org/css-images-3/#propdef-image-rendering}
     [image-rendering]}. *)
type image_rendering =
  | Auto
  | Smooth
  | High_quality
  | Crisp_edges
  | Pixelated
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_rendering var

(** CSS Values 4
    {{:https://drafts.csswg.org/css-values-4/#resolution} [<resolution>]}. *)
type resolution = Dpi of float | Dpcm of float | Dppx of float | X of float

(** CSS Images 4
    {{:https://drafts.csswg.org/css-images-4/#propdef-image-resolution}
     [image-resolution]}: [[ from-image || <resolution> ] && snap?]. *)
type image_resolution =
  | Resolution of resolution
  | From_image
  | From_image_resolution of resolution
  | Snap of resolution
  | From_image_snap
  | From_image_snap_resolution of resolution
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_resolution var

(** CSS Sizing 4 sec. 5.2 spells one slot [auto? [ none | <length [0,inf]> ]],
    so the keyword takes the [auto] prefix as a length does. *)
type contain_intrinsic_size_item =
  | None
  | Length of length
  | Auto_none
  | Auto of length

(** CSS Sizing 4
    {{:https://drafts.csswg.org/css-sizing-4/#propdef-contain-intrinsic-size}
     [contain-intrinsic-size]}: one axis or both. *)
type contain_intrinsic_size =
  | None
  | Intrinsic of
      contain_intrinsic_size_item * contain_intrinsic_size_item option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of contain_intrinsic_size var

(** One axis longhand of {!type-contain_intrinsic_size}.

    @see <https://drafts.csswg.org/css-sizing-4/#propdef-contain-intrinsic-width>
      contain-intrinsic-width *)
type contain_intrinsic_longhand =
  | None
  | Size of contain_intrinsic_size_item
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of contain_intrinsic_longhand var

(** One edge whose margin CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#propdef-margin-trim} [margin-trim]}
    trims. *)
type margin_trim_edge = Block_start | Inline_start | Block_end | Inline_end

(** One axis whose margins CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#propdef-margin-trim} [margin-trim]}
    trims. *)
type margin_trim_axis = Block | Inline

(** CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#propdef-margin-trim} [margin-trim]}.
*)
type margin_trim =
  | None
  | Block
  | Inline
  | Axes of margin_trim_axis list
  | Edges of margin_trim_edge list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of margin_trim var

(** [ray()] size: the [<radial-extent>] keywords plus [sides]. The [<length>]
    and two-radii branches of [<radial-size>] have no [ray()] syntax. *)
type ray_size =
  | Closest_side
  | Closest_corner
  | Farthest_side
  | Farthest_corner
  | Sides

type ray = {
  angle : angle;
  size : ray_size option;
  contain : bool;
  position : position_value option;
}
(** Sec. 3.2 [ray()]: an angle, a size, whether the path is contained, and the
    position it starts from. *)

(** CSS Masking 1 sec. 5.1 [<geometry-box>] reference box for [clip-path]: the
    [<shape-box>] from CSS Shapes 1 plus the SVG-specific boxes. *)
type clip_geometry_box =
  | Margin_box
  | Border_box
  | Padding_box
  | Content_box
  | Fill_box
  | Stroke_box
  | View_box

(** CSS Shapes 1 sec. 3.1 [<shape-radius>] for [circle()] / [ellipse()]: a
    [<length-percentage>] or one of the extent keywords. *)
type clip_path_extent = Extent_length of length | Closest_side | Farthest_side

type clip_path_fill_rule = Nonzero | Evenodd

(** CSS clip-path property values for clipping regions. *)
type clip_path =
  | Clip_path_none
  | Clip_path_url of string
  | Clip_path_inset of {
      top : length_percentage;
      right : length_percentage option;
      bottom : length_percentage option;
      left : length_percentage option;
      rounded : border_radius option;
    }  (** [inset(<length-percentage>{1,4} [round <border-radius>]?)] *)
  | Clip_path_circle of {
      radius : clip_path_extent option;
      position : position_value option;
    }  (** [circle(<shape-radius>? [at <position>]?)] *)
  | Clip_path_ellipse of {
      rx : clip_path_extent option;
      ry : clip_path_extent option;
      position : position_value option;
    }  (** [ellipse(<shape-radius>{2}? [at <position>]?)] *)
  | Clip_path_polygon of {
      fill_rule : clip_path_fill_rule option;
      points : (length * length) list;
      spaced : bool;
          (** [true] if the source emitted points without explicit commas. *)
    }
  | Clip_path_path of string  (** SVG path data *)
  | Clip_path_shape of string
  | Clip_path_box of clip_geometry_box
      (** Bare reference box, e.g. [clip-path: margin-box]. *)
  | Clip_path_with_box of {
      shape : clip_path;
      box : clip_geometry_box;
      box_first : bool;
          (** Source order: [true] if the box appeared before the shape
              ([padding-box circle(...)]), [false] for
              [circle(...) padding-box]. *)
    }
  | Clip_path_xywh of {
      x : length_percentage;
      y : length_percentage;
      width : length_percentage;
      height : length_percentage;
      rounded : border_radius option;
    }
      (** [xywh(<length-percentage>{4} [round <border-radius>]?)] - CSS Shapes
          2. *)
  | Clip_path_rect of {
      top : length_percentage;
      right : length_percentage;
      bottom : length_percentage;
      left : length_percentage;
      rounded : border_radius option;
    }
      (** [rect(<length-percentage>{4} [round <border-radius>]?)] - CSS Shapes
          2. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of clip_path var
  | Invalid of invalid_value
      (** Spec-invalid [<basic-shape>] preserved verbatim - e.g.
          [ellipse(50px 60px at 0 10% 20%)] with a 3-value [<position>] tail.
          The pretty-printer round-trips the captured tokens; the
          [Optimize.drop_invalid] pass removes the declaration on every
          serialisation. *)

(** Sec. 2.1 [offset-path]: [none | <offset-path> || <coord-box>], where the
    shape branch reuses {!type-clip_path}. *)
type offset_path =
  | None
  | Url of string
  | Path of string
  | Ray of ray
  | Shape of clip_path
      (** CSS Motion Path 1 sec. 2.1 [<basic-shape> || <coord-box>], which
          {!type-clip_path} already models for [clip-path]: the same shape
          functions and the same reference boxes. *)
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_path var

(** Sec. 2.4 [offset-anchor]: [auto | <position>]. *)
type offset_anchor =
  | Auto
  | Position of position_value
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_anchor var

(** Sec. 2.5 [offset-position]: [normal | auto | <position>]. *)
type offset_position =
  | Normal
  | Auto
  | Position of position_value
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_position var

(** Sec. 2.3 [offset-rotate]: which of [auto] and [reverse] an explicit angle is
    measured from. *)
type offset_rotate_mode = Auto | Reverse

(** Sec. 2.3 [offset-rotate]: [[ auto | reverse ] || <angle>]. *)
type offset_rotate =
  | Auto
  | Reverse
  | Angle of angle
  | With_angle of offset_rotate_mode * angle
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_rotate var

(* Motion Path 1 (ED) sec. 2.6 gives the [offset] shorthand

   [ <'offset-position'>? [ <'offset-path'> [ <'offset-distance'> ||
   <'offset-rotate'> ]? ]? ]! [ / <'offset-anchor'> ]?

   The [!] makes the leading group required: a declaration writes a position, a
   path, or both, and never nothing. [offset-distance] and [offset-rotate] sit
   inside the path branch, so [With_path] is the only shape that carries them
   and neither can be written without a path in front. *)

(** Sec. 2.6: the leading group of the [offset] shorthand, which is either a
    position on its own or a path with the slots that follow it. *)
type offset_target =
  | Position_only of offset_position
  | With_path of {
      position : offset_position option;
      path : offset_path;
      distance : length_percentage option;
      rotate : offset_rotate option;
    }

(** Sec. 2.6 [offset]:
    [[ <'offset-position'>? [ <'offset-path'> [ <'offset-distance'> ||
     <'offset-rotate'> ]? ]? ]! [ / <'offset-anchor'> ]?]. *)
type offset =
  | Shorthand of { target : offset_target; anchor : offset_anchor option }
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset var

(** CSS Conditional 5 sec. 3.3 [container]:
    [<'container-name'> [ / <'container-type'> ]?]. The name half is required,
    and only the [None] and [Names] arms of {!type-container_name} reach it. *)
type container_shorthand =
  | Shorthand of { name : container_name; ctype : container_type option }
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of container_shorthand var

(* Containment Types *)

(** CSS contain values *)
type contain =
  | None
  | Strict
  | Content
  | Size
  | Layout
  | Style
  | Paint
  | Inline_size
  | List of contain list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of contain var

(** CSS isolation values *)
type isolation =
  | Auto
  | Isolate
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of isolation var

(** CSS break-before/break-after values for page/column/region breaks. *)
type break_value =
  | Auto
  | Avoid
  | All
  | Avoid_page
  | Page
  | Left
  | Right
  | Recto
  | Verso
  | Avoid_column
  | Column
  | Avoid_region
  | Region
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of break_value var

(** CSS break-inside values. *)
type break_inside_value =
  | Auto
  | Avoid
  | Avoid_page
  | Avoid_column
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of break_inside_value var

(** CSS Fragmentation 3 sec. 3.4 deprecated [page-break-before / -after] alias
    vocabulary; the shorter value list makes these their own type rather than
    overload {!type-break_value}. *)
type page_break_value =
  | Auto
  | Always (* maps to [break-before/after: page] *)
  | Avoid
  | Left
  | Right
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of page_break_value var
      (** CSS Fragmentation 3 sec. 3.4 deprecated [page-break-inside]
          vocabulary. *)

type page_break_inside_value =
  | Auto
  | Avoid
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of page_break_inside_value var

(* CSS Paged Media 3 sec. 7.1 [size] descriptor: optional page size keyword
   (paper sheet name), explicit dimensions, [auto], or a page size combined with
   an orientation. *)
type page_size_name =
  | A5
  | A4
  | A3
  | B5
  | B4
  | Jis_b5
  | Jis_b4
  | Letter
  | Legal
  | Ledger
  | Var of page_size_name var

type page_size_orientation =
  | Portrait
  | Landscape
  | Var of page_size_orientation var

(** CSS paged-media [size] descriptor values. *)
type page_size =
  | Auto
  | Single of length
  | Pair of length * length
  | Named of page_size_name
  | Named_oriented of page_size_name * page_size_orientation
  | Oriented of page_size_orientation
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of page_size var
