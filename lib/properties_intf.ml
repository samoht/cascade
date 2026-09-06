(** Property type shared between properties.ml and properties.mli.

    {2 Keyword constructors}

    A CSS keyword carries its own name as a constructor in every value type
    whose grammar admits it: [None] for [none], [Auto] for [auto], [Normal] for
    [normal]. A new value type follows the same rule, so the name stays shared
    rather than prefixed with the property it belongs to.

    The expected type selects among them. An argument to a property setter
    ([Css.display None]), an annotated binding ([let d : Css.display = None]),
    an annotated parameter and a [match] on a scrutinee that already has a type
    all resolve without warning 41. A binding with no expected type
    ([let d = None]) and a [function] whose argument type comes from its own
    branches do not; annotate the binding or the parameter there. *)

open Values

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
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_style var

type line_height =
  | Normal
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Num of float
  | Number of { value : float; unit : string option; repr : string }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Calc of line_height calc
  | Var of line_height var

type font_weight =
  | Weight of float
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_weight var

(* Display & Layout Types *)
type display =
  | Block
  | Inline
  | Inline_block
  | Flex
  | Inline_flex
  | Grid
  | Inline_grid
  | Grid_lanes
  | Inline_grid_lanes
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
  | Run_in
  | Ruby
  | Ruby_base
  | Ruby_text
  | Ruby_base_container
  | Ruby_text_container
  | Math
  | Webkit_flex
  | Webkit_inline_flex
  | Ms_flexbox
  | Webkit_box
  | Moz_box
  | Moz_inline_box
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Multi of display * display
      (** Two-value [<display-outside> <display-inside>] syntax per CSS Display
          3 sec. 2.1, e.g. [inline flow-root] or [list-item flow-root]. *)
  | Var of display var

type position =
  | Static
  | Relative
  | Absolute
  | Fixed
  | Sticky
  | Webkit_sticky
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position var

type visibility =
  | Visible
  | Hidden
  | Collapse
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of visibility var

type baseline_source =
  | Auto
  | First
  | Last
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of baseline_source var

type alignment_baseline =
  | Baseline
  | Text_bottom
  | Middle
  | Central
  | Text_top
  | Ideographic
  | Alphabetic
  | Hanging
  | Mathematical
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of alignment_baseline var

type baseline_shift =
  | Shift of length_percentage
  | Sub
  | Super
  | Top
  | Center
  | Bottom
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of baseline_shift var

type z_index =
  | Auto
  | Index of int
  | Calc of z_index calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of z_index var

type opacity =
  | Opacity_number of float
  | Calc of opacity calc
  | Abs of opacity  (** [abs(<opacity>)] *)
  | Sign of opacity  (** [sign(<opacity>)] *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of opacity var

type shape_image_threshold =
  | Number of float
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of shape_image_threshold var

type tab_size =
  | Int of int
  | Length of length
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of tab_size var

type zoom =
  | Normal
  | Reset
  | Num of float
  | Pct of float
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of zoom var

type order =
  | Int of int
  | Calc of order calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of order var

type overflow =
  | Visible
  | Hidden
  | Scroll
  | Auto
  | Clip
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Overflow_pair of overflow * overflow
  | Var of overflow var

type border_spacing = Lengths of length list | Var of border_spacing var
type overflow_clip_box = Content_box | Padding_box | Border_box

type overflow_clip_margin =
  | Clip_margin of overflow_clip_box option * length option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of overflow_clip_margin var

(* Flexbox Types *)
type flex_direction =
  | Row
  | Row_reverse
  | Column
  | Column_reverse
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_direction var

(** CSS Flexbox 2 sec. 5.2 [flex-wrap]:
    [nowrap | [ wrap | wrap-reverse ] || balance]. [wrap] is the direction
    [balance] carries on its own, so [Balance] is both [balance] and the
    [wrap balance] that means the same thing. *)
type flex_wrap =
  | Nowrap
  | Wrap
  | Wrap_reverse
  | Balance
  | Wrap_reverse_balance
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_wrap var

type flex_flow =
  | Flow of flex_direction option * flex_wrap option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_flow var

type flex_factor =
  | Number of float
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Calc of flex_factor calc
  | Var of flex_factor var

(** CSS Box Alignment 3 sec. 4.2 [align-content]:
    [normal | <baseline-position> | <content-distribution> |
     <overflow-position>? <content-position>], where [<content-position>] is
    [center | start | end | flex-start | flex-end]. The [left] and [right] of
    {!type-justify_content} belong to the inline axis alone. *)
type align_content =
  | Normal
  | Baseline
  | First_baseline
  | Last_baseline
  (* Content position values - safe by default *)
  | Center
  | Start
  | End
  | Flex_start
  | Flex_end
  (* Safe content position values *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  (* Unsafe content position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  (* Content distribution *)
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of align_content var

type align_items =
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  (* Self position values - safe by default *)
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  (* Safe self position values (explicit safe keyword) *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Anchor_center
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of align_items var

type align_self =
  | Auto
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  (* Self position values - safe by default *)
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  (* Safe self position values (explicit safe keyword) *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of align_self var

type justify_content =
  | Normal
  (* Content position values - safe by default *)
  | Center
  | Start
  | End
  | Flex_start
  | Flex_end
  | Left
  | Right
  (* Safe content position values (explicit safe keyword) *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  (* Unsafe content position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  (* Content distribution *)
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of justify_content var

type justify_items =
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  (* Self position values - safe by default *)
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Left
  | Right
  (* Safe self position values *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_self_start
  | Safe_self_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Anchor_center
  | Legacy
  | Legacy_center
  | Legacy_left
  | Legacy_right
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of justify_items var

type justify_self =
  | Auto
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  (* Self position values - safe by default *)
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Left
  | Right
  (* Safe self position values *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_self_start
  | Safe_self_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Anchor_center
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of justify_self var

type flex_basis =
  | Auto
  | Content
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Ric of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Ch of float
  | Lh of float
  | Num of float
  | Zero
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Fit_content
  | Fit_content_arg of length
  | Max_content
  | Min_content
  (* Math functions over [<length-percentage>] (CSS Values 4 sec. 10). Args
     reuse [length] (which already carries [Pct]) and mirror the [length]
     constructors; [flex_basis_of_length] carries them across and the printer
     delegates to [pp_length]. *)
  | Clamp of length * length * length
  | Min of length list
  | Max of length list
  | Round of string * length * length
  | Mod of length * length
  | Rem_fn of length * length
  | Hypot of length list
  | Abs of length
  | Dimension of { value : float; unit : string; repr : string }
      (** A dimension whose authored spelling the typed constructors above do
          not carry, e.g. [1e3px] or [10.0px]. {!type-length} holds one the same
          way. *)
  | Calc of flex_basis calc
  | Var of flex_basis var

type border_width =
  | Thin
  | Medium
  | Thick
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Ric of float
  | Rlh of float
  | Ch of float
  | Lh of float
  | Vh of float
  | Vw of float
  | Vmin of float
  | Vmax of float
  | Pct of float
  | Dimension of { value : float; unit : string; repr : string }
  | Zero
  | Auto
  | Max_content
  | Min_content
  | Fit_content
  | From_font
  | Calc of border_width calc
  | Min of border_width calc list
  | Max of border_width calc list
  | Clamp of border_width calc * border_width calc * border_width calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_width var

type flex =
  | Initial (* 0 1 auto *)
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Auto (* 1 1 auto *)
  | None (* 0 0 auto *)
  | Grow of flex_factor (* Single grow value *)
  | Basis of flex_basis (* 1 1 <flex-basis> *)
  | Grow_shrink of flex_factor * flex_factor (* grow shrink 0% *)
  | Full of flex_factor * flex_factor * flex_basis (* grow shrink basis *)
  | Var of flex var

type font_size =
  | Length of length
  | Pct of float
  | Calc of font_size calc
  (* Absolute size keywords *)
  | Xx_small
  | X_small
  | Small
  | Medium
  | Large
  | X_large
  | Xx_large
  | Xxx_large
  (* Relative size keywords *)
  | Larger
  | Smaller
  (* Math keyword *)
  | Math
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_size var

type place_content =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_stretch
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_stretch
  | Align_justify of align_content * justify_content
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of place_content var

type place_items =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Start_safe
  | End_safe
  | Center_safe
  | Stretch_stretch  (** Explicit stretch on both axes *)
  | Align_justify of align_items * justify_items
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of place_items var

(* Grid Types *)
type grid_auto_flow_component =
  | Axis of [ `Row | `Column ]
  | Dense
  | Var of grid_auto_flow_component var

type grid_auto_flow =
  | Row
  | Column
  | Dense
  | Row_dense
  | Column_dense
  | Components of grid_auto_flow_component list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of grid_auto_flow var

(** [repeat()] count argument: an explicit integer, one of the auto-track-list
    keywords (CSS Grid 1 sec. 7.2.3.1 / 2), or a [var()] standing in for the
    count. *)
type repeat_count =
  | Count of int
  | Auto_fill
  | Auto_fit
  | Var of repeat_count var

(** A CSS Values 4 math function whose result is a Grid [<flex>] track breadth.
    Kept separate from [length] because [fr] is not a length unit. *)
type grid_flex_math =
  | Calc_flex of math_arg
  | Min_flex of math_arg list
  | Max_flex of math_arg list
  | Clamp_flex of math_arg * math_arg * math_arg

type grid_template =
  | None
  (* Single track values *)
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Zero
  | Length of length
      (** A track sized by a general [<length-percentage>] that the unit-
          specific cases above do not carry: a [calc()], a [var()] inside a
          [calc()], or a less common unit (e.g. [cm], [ch]). *)
  | Fr of float
  | Flex_math of grid_flex_math
  | Auto
  | Min_content
  | Max_content
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  (* Complex track values *)
  | Min_max of grid_template * grid_template
  | Fit_content of length
  | Repeat of repeat_count * grid_template list
  | Tracks of grid_template list
  | Split of grid_template * grid_template
  | Auto_flow_columns of grid_template * grid_auto_flow * grid_template option
      (** [<grid-template-rows> / auto-flow [dense]? <grid-auto-columns>?]. *)
  | Auto_flow_rows of grid_auto_flow * grid_template option * grid_template
      (** [auto-flow [dense]? <grid-auto-rows>? / <grid-template-columns>]. *)
  | Named_tracks of (string option * grid_template) list
  | Line_names of string list
      (** Square-bracket line-names block ([[col-start]], [[a b]]). Stored as
          its own track-list element so the printer can place it before /
          between / after the surrounding track sizes. *)
  | Template of string
  | Subgrid
  | Masonry
  | Var of grid_template var

type grid_template_areas =
  | No_areas
  | Areas of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of grid_template_areas var

type grid_line =
  | Auto
  | Num of int
  | Name of string
  | Num_name of int * string
  | Span of int
  | Span_name of string
  | Span_num_name of int * string
  | Calc of grid_line calc
  | Var of grid_line var

type grid_line_pair =
  | Lines of grid_line * grid_line
  | Var of grid_line_pair var

(* CSS Grid 2 sec. 8.4: [grid-area: <grid-line> [/ <grid-line>]{0,3}] -
   row-start / column-start / row-end / column-end. The 1/2/3-value source forms
   are defaulting per the spec; they all canonicalise to the four-line record so
   the printer can pick the shortest equivalent spelling. *)
type grid_area =
  | Lines of {
      row_start : grid_line;
      column_start : grid_line;
      row_end : grid_line;
      column_end : grid_line;
    }
  | Var of grid_area var
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer

type aspect_ratio =
  | Auto
  | Auto_ratio of float * float
  | Ratio of float * float
  | Auto_ratio_calc of number * number
  | Ratio_calc of number * number
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of aspect_ratio var

type font_style =
  | Normal
  | Italic
  | Oblique
  | Oblique_angle of angle
  | Oblique_range of angle * angle
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_style var

type text_align =
  | Left
  | Right
  | Center
  | Justify
  | Start
  | End
  | Match_parent
  | Webkit_match_parent
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_align var

type text_decoration_line =
  | None
  | Underline
  | Overline
  | Line_through
  | Blink
      (** CSS Text Decoration 4 sec. 2.1: deprecated but still part of the
          [<text-decoration-line>] grammar; UAs typically render it as no-op. *)
  | Spelling_error
      (** CSS Text Decoration 4 sec. 2.1 [spelling-error]: lets authors style
          the UA's spelling-error mark via [text-decoration]. *)
  | Grammar_error
      (** CSS Text Decoration 4 sec. 2.1 [grammar-error]: parallel to
          [spelling-error]. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_line var

type text_decoration_style =
  | Solid
  | Double
  | Dotted
  | Dashed
  | Wavy
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_style var

type text_decoration_shorthand = {
  lines : text_decoration_line list;
  style : text_decoration_style option;
  color : color option;
  thickness : length option;
}

type text_decoration =
  | None
  | Shorthand of text_decoration_shorthand
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration var

type text_decoration_skip =
  | None
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip var

type text_decoration_skip_self =
  | None
  | Objects
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_self var

type text_decoration_skip_box =
  | All
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_box var

type text_decoration_skip_inset =
  | None
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_inset var

type text_decoration_skip_space = All | Start | End

type text_decoration_skip_spaces =
  | Spaces of text_decoration_skip_space list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_spaces var

type text_emphasis_fill = Filled | Open
type text_emphasis_shape = Dot | Circle | Double_circle | Triangle | Sesame

type text_emphasis_style =
  | None
  | Mark of text_emphasis_fill option * text_emphasis_shape option
  | String of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis_style var

type text_emphasis =
  | Emphasis of text_emphasis_style option * color option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis var

type text_emphasis_line = Over | Under
type text_emphasis_side = Left | Right
type text_emphasis_skip_keyword = Spaces | Punctuation | Symbols | Narrow

type text_emphasis_skip =
  | Skip of text_emphasis_skip_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis_skip var

type text_emphasis_position =
  | Position of text_emphasis_line * text_emphasis_side option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis_position var

(** CSS Text 4 sec. 6.1
    [text-indent: <length-percentage> && hanging? && each-line?]. Each component
    is optional except the length, but the three may appear in any order. *)
type text_indent_value =
  | Indent of { length : length_percentage; hanging : bool; each_line : bool }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_indent_value var

type text_underline_position_keyword = Under | Left | Right

type text_underline_position =
  | Auto
  | From_font
  | Position of text_underline_position_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_underline_position var

type text_orientation =
  | Mixed
  | Upright
  | Sideways
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_orientation var

type glyph_orientation_vertical =
  | Auto
  | Angle of angle
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of glyph_orientation_vertical var

(** CSS Text 4 sec. 6.3
    [text-transform = none | [capitalize | uppercase | lowercase] || full-width
     || full-size-kana]: case + width + kana width are independent and can
    combine. *)
type text_transform_case = Capitalize | Uppercase | Lowercase

type text_transform =
  | None
  | Case of text_transform_case
      (** Single case keyword without [full-width] / [full-size-kana]. *)
  | Combo of {
      case : text_transform_case option;
      full_width : bool;
      full_size_kana : bool;
    }  (** Multi-keyword form, e.g. [uppercase full-width full-size-kana]. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_transform var

type line_break =
  | Auto
  | Loose
  | Normal
  | Strict
  | Anywhere
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of line_break var

type font_optical_sizing =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_optical_sizing var

type font_kerning =
  | Auto
  | Normal
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_kerning var

type font_language_override =
  | Normal
  | String of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_language_override var

type font_synthesis_style =
  | Auto
  | None
  | Oblique_only
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_style var

type font_synthesis_weight =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_weight var

type font_synthesis_small_caps =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_small_caps var

type font_synthesis_position =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_position var

type font_variant_ligature =
  | Common_ligatures
  | No_common_ligatures
  | Discretionary_ligatures
  | No_discretionary_ligatures
  | Historical_ligatures
  | No_historical_ligatures
  | Contextual
  | No_contextual

type font_variant_ligatures =
  | Normal
  | None
  | Ligatures of font_variant_ligature list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_ligatures var

type font_variant_caps =
  | Normal
  | Small_caps
  | All_small_caps
  | Petite_caps
  | All_petite_caps
  | Unicase
  | Titling_caps
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_caps var

(* CSS Fonts 4 (ED) sec. 6.6: [font-variant-alternates] is [normal | [
   stylistic(<font-feature-value-name>) || historical-forms ||
   styleset(<font-feature-value-name>#) ||
   character-variant(<font-feature-value-name>#) ||
   swash(<font-feature-value-name>) || ornaments(<font-feature-value-name>) ||
   annotation(<font-feature-value-name>) ]]. A feature value name is a
   [<custom-ident>] naming a block of the [@font-feature-values] rule. *)
type font_variant_alternates_item =
  | Stylistic of string
  | Historical_forms
  | Styleset of string list
  | Character_variant of string list
  | Swash of string
  | Ornaments of string
  | Annotation of string

type font_variant_alternates =
  | Normal
  | Alternates of font_variant_alternates_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_alternates var

type font_variant_position =
  | Normal
  | Sub
  | Super
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_position var

type east_asian_feature =
  | Jis78
  | Jis83
  | Jis90
  | Jis04
  | Simplified
  | Traditional
  | Full_width
  | Proportional_width
  | Ruby

type font_variant_east_asian =
  | Normal
  | Features of east_asian_feature list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_east_asian var

type text_overflow =
  | Clip
  | Ellipsis
  | String of string
  | Pair of text_overflow * text_overflow
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_overflow var

(* CSS Text 4 sec. 5.5: [text-wrap] is [<'text-wrap-mode'> ||
   <'text-wrap-style'>]. A single component names the arm it came from;
   [Mode_style] carries both, and prints mode-first whatever order it was
   written in. *)
type text_wrap =
  | Wrap
  | No_wrap
  | Auto
  | Balance
  | Stable
  | Pretty
  | Mode_style of [ `Wrap | `No_wrap ] * [ `Auto | `Balance | `Stable | `Pretty ]
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_wrap var

type text_wrap_mode =
  | Wrap
  | No_wrap
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_wrap_mode var

type text_wrap_style =
  | Auto
  | Balance
  | Pretty
  | Stable
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_wrap_style var

type text_box_trim =
  | None
  | Trim_start
  | Trim_end
  | Trim_both
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_box_trim var

type text_box_edge_keyword =
  | Text
  | Cap
  | Ex
  | Alphabetic
  | Ideographic
  | Ideographic_ink

type text_box_edge =
  | Auto
  | Edge of text_box_edge_keyword * text_box_edge_keyword option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_box_edge var

type text_box =
  | Normal
  | Box of text_box_trim option * text_box_edge option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_box var

type inline_sizing =
  | Normal
  | Stretch
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of inline_sizing var

type line_fit_edge_keyword =
  | Leading
  | Text
  | Cap
  | Ex
  | Alphabetic
  | Ideographic
  | Ideographic_ink

type line_fit_edge =
  | Edge of line_fit_edge_keyword * line_fit_edge_keyword option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of line_fit_edge var

type interpolate_size =
  | Numeric_only
  | Allow_keywords
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of interpolate_size var

type min_intrinsic_sizing_keyword =
  | Legacy
  | Zero_if_scroll
  | Zero_if_extrinsic

type min_intrinsic_sizing =
  | Sizing of min_intrinsic_sizing_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of min_intrinsic_sizing var

type text_spacing_trim =
  | Normal
  | Space_all
  | Trim_start
  | Space_first
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_spacing_trim var

(** CSS Text 4 sec. 6.3.4: one slot of [hyphenate-limit-chars], a minimum
    character count or [auto] for the count the UA picks. CSS Values 4 sec. 10.1
    puts a math function where the count is. *)
type hyphenate_limit_chars_item = Auto | Chars of number

(** CSS Text 4 sec. 6.3.4 [hyphenate-limit-chars]: the minimum characters in a
    hyphenated word, then before the hyphen, then after it. A missing third slot
    repeats the second and a missing second is [auto], so [One Auto] is the
    initial value. *)
type hyphenate_limit_chars =
  | One of hyphenate_limit_chars_item
  | Two of hyphenate_limit_chars_item * hyphenate_limit_chars_item
  | Three of
      hyphenate_limit_chars_item
      * hyphenate_limit_chars_item
      * hyphenate_limit_chars_item
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of hyphenate_limit_chars var

type initial_letter =
  | Normal
  | Drop
  | Raise
  | Size of float
  | Size_sink of float * int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter var

type initial_letter_align_keyword =
  | Alphabetic
  | Ideographic
  | Hanging
  | Leading
  | Border_box

type initial_letter_align =
  | Align of initial_letter_align_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter_align var

type initial_letter_wrap =
  | None
  | First
  | All
  | Grid
  | Length of length_percentage
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter_wrap var

type ruby_merge =
  | Separate
  | Merge
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_merge var

type ruby_align =
  | Start
  | Center
  | Space_between
  | Space_around
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_align var

type ruby_overhang =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_overhang var

type ruby_position_keyword = Alternate | Over | Under | Inter_character

type ruby_position =
  | Position of ruby_position_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_position var

type dominant_baseline =
  | Auto
  | Alphabetic
  | Ideographic
  | Mathematical
  | Central
  | Middle
  | Text_top
  | Text_bottom
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of dominant_baseline var

type white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Collapse
  | Preserve_nowrap
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of white_space var

(* CSS Text 4 sec. 3.1: [white-space-collapse] says how white space and segment
   breaks in the source collapse. *)
type white_space_collapse =
  | Collapse
  | Discard
  | Preserve
  | Preserve_breaks
  | Preserve_spaces
  | Break_spaces
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of white_space_collapse var

type word_break =
  | Normal
  | Break_all
  | Keep_all
  | Break_word
  | Auto_phrase
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of word_break var

type overflow_wrap =
  | Normal
  | Break_word
  | Anywhere
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of overflow_wrap var

type hyphens =
  | None
  | Manual
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of hyphens var

(* List Types *)
type symbols_type = Cyclic | Numeric | Alphabetic | Symbolic | Fixed
type list_style_symbol = String of string | Url of string

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
  | Decimal_leading_zero
  | Arabic_indic
  | Armenian
  | Upper_armenian
  | Lower_armenian
  | Bengali
  | Cambodian
  | Khmer
  | Cjk_decimal
  | Devanagari
  | Georgian
  | Gujarati
  | Gurmukhi
  | Hebrew
  | Kannada
  | Lao
  | Malayalam
  | Mongolian
  | Myanmar
  | Oriya
  | Persian
  | Tamil
  | Telugu
  | Thai
  | Tibetan
  | Lower_latin
  | Upper_latin
  | Cjk_earthly_branch
  | Cjk_heavenly_stem
  | Lower_greek
  | Hiragana
  | Hiragana_iroha
  | Katakana
  | Katakana_iroha
  | Disclosure_open
  | Disclosure_closed
  | Cjk_ideographic
  | Japanese_informal
  | Japanese_formal
  | Korean_hangul_formal
  | Korean_hanja_informal
  | Korean_hanja_formal
  | Simp_chinese_informal
  | Simp_chinese_formal
  | Trad_chinese_informal
  | Trad_chinese_formal
  | Ethiopic_numeric
  | Name of string
  | String of string
  | Symbols of symbols_type option * list_style_symbol list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style_type var

type list_style_position =
  | Inside
  | Outside
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style_position var

(* Table Types *)
type table_layout =
  | Auto
  | Fixed
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of table_layout var

type vertical_align =
  | Baseline
  | Top
  | Middle
  | Bottom
  | Text_top
  | Text_bottom
  | Sub
  | Super
  | Zero
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Calc of vertical_align calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of vertical_align var

(* Border Types *)
type border_collapse =
  | Collapse
  | Separate
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_collapse var

(* Border shorthand type *)
type border_shorthand = {
  width : border_width option;
  style : border_style option;
  color : color option;
}

type border =
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | None
  | Shorthand of border_shorthand
  | Var of border var

type logical_border_color =
  | Single of color
  | Pair of color * color
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of logical_border_color var

(* border-inline-width / border-block-width take one or two <line-width> values,
   mirroring logical_border_color. *)
type logical_border_width =
  | Single of border_width
  | Pair of border_width * border_width
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of logical_border_width var

(* border-inline-style / border-block-style take one or two <line-style> values,
   mirroring logical_border_width. *)
type logical_border_style =
  | Single of border_style
  | Pair of border_style * border_style
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of logical_border_style var

(* [-webkit-text-stroke] is [<line-width> || <color>] over its two longhands. It
   is not in a CSS specification; the shape is what WebKit and Blink accept and
   what their CSSOM reports back. *)
type webkit_text_stroke = { width : border_width option; color : color option }

type outline_style =
  | None
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of outline_style var

(* Outline shorthand type *)
type outline_shorthand = {
  width : border_width option;
  style : outline_style option;
  color : color option;
}

type outline =
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | None
  | Shorthand of outline_shorthand
  | Var of outline var

(* Font Types *)
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
  (* Named families, for building a font stack in OCaml. CSS Fonts 4 sec. 2.1
     makes these [<custom-ident>]s rather than keywords, so the reader never
     produces one: an authored name is read as [Name] and printed back
     verbatim. Each constructor prints exactly as the [Name] carrying its
     spelling. *)
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
  | Revert
  | Revert_layer
  (* Custom font family name *)
  | Name of string
  (* CSS variables *)
  (* List of fonts for composition *)
  | List of font_family list
  | Var of font_family var
  | Invalid of invalid_value
      (** CSS Cascade 5 sec. 7.3: a CSS-wide keyword (e.g. [inherit]) is only
          valid as a sole top-level value. [font-family: Arial, inherit] mixes
          it inside a [<custom-ident>#] list and is therefore invalid. Cascade
          preserves the source verbatim for round-trip; [Optimize.drop_invalid]
          removes the declaration on every serialisation. *)

type font_stretch =
  | Pct of float
  | Ultra_condensed
  | Extra_condensed
  | Condensed
  | Semi_condensed
  | Normal
  | Semi_expanded
  | Expanded
  | Extra_expanded
  | Ultra_expanded
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_stretch var

(* CSS Fonts 4 sec. 2.7: [font] shorthand body is [<style>? <variant-css21>?
   <weight>? <stretch>? <size>[/<line-height>]? <family>+]. Required: [size] and
   [family]; the rest default to the longhand initial. *)
type font_variant_css21 = Normal | Small_caps

type font_shorthand = {
  style : font_style option;
  variant : font_variant_css21 option;
  weight : font_weight option;
  stretch : font_stretch option;
  size : font_size;
  line_height : line_height option;
  family : font_family;
}

(* CSS Fonts 4 sec. 2.7: [font] is either a structured shorthand, one of the six
   system-font keywords, a CSS-wide keyword, or a [var()] reference. *)
type font =
  | Shorthand of font_shorthand
  | Caption
  | Icon
  | Menu
  | Message_box
  | Small_caption
  | Status_bar
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font var

type font_size_adjust_metric =
  | Ex_height
  | Cap_height
  | Ch_width
  | Ic_width
  | Ic_height

type font_size_adjust =
  | None
  | Number of float
  | From_font
  | Metric_number of font_size_adjust_metric * float
  | Metric_from_font of font_size_adjust_metric
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_size_adjust var

type font_variant_emoji =
  | Normal
  | Text
  | Emoji
  | Unicode
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_emoji var

type font_display =
  | Auto
  | Block
  | Swap
  | Fallback
  | Optional
  | Var of font_display var

type unicode_range =
  | Single of int  (** U+xxxx *)
  | Range of int * int  (** U+xxxx-yyyy *)
  | Padded_single of int * int
  | Padded_range of {
      start : int;
      end_ : int;
      start_width : int;
      end_width : int;
    }
  | Wildcard of { prefix : int; prefix_width : int; wildcards : int }
  | Var of unicode_range var

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
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Tokens of font_variant_numeric_token list
  | Composed of {
      ordinal : font_variant_numeric_token option;
      slashed_zero : font_variant_numeric_token option;
      numeric_figure : font_variant_numeric_token option;
      numeric_spacing : font_variant_numeric_token option;
      numeric_fraction : font_variant_numeric_token option;
    }
  | Var of font_variant_numeric var

(* CSS Fonts 4 (ED) sec. 6.10: [font-variant] is [normal | none | [ <ligatures>
   || <alternates> || <caps> || <numeric> || <east-asian> || <position> ||
   <emoji> ]] over its seven longhands, each written at most once. A slot the
   value leaves out is reset to that longhand's initial. *)
type font_variant_shorthand = {
  ligatures : font_variant_ligature list;
  alternates : font_variant_alternates_item list;
  caps : font_variant_caps option;
  numeric : font_variant_numeric_token list;
  east_asian : east_asian_feature list;
  position : font_variant_position option;
  emoji : font_variant_emoji option;
}

type font_variant =
  | Normal
  | None
  | Shorthand of font_variant_shorthand
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant var

type font_feature_value = On | Off | Index of int
type font_feature_setting = { tag : string; value : font_feature_value option }

type font_feature_settings =
  | Normal
  | Feature_list of font_feature_setting list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_feature_settings var

type font_variation_setting = { tag : string; value : float }

type font_variation_settings =
  | Normal
  | Axis_list of font_variation_setting list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variation_settings var

(* Transform & Animation Types *)
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

type transform_style =
  | Flat
  | Preserve_3d
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_style var

type backface_visibility =
  | Visible
  | Hidden
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of backface_visibility var

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

type steps_direction =
  | Jump_start
  | Jump_end
  | Jump_none
  | Jump_both
  | Start
  | End
  | Var of steps_direction var

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

(* Transition behavior for discrete transitions (CSS Transitions Level 2) *)
type transition_behavior =
  | Normal
  | Allow_discrete
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of transition_behavior var

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

type transition =
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | None
  | Shorthand of transition_shorthand
  | Var of transition var

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

type animation_composition_item = Replace | Add | Accumulate

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

and animation_timeline =
  | None
  | Auto
  | Name of string
  | Scroll of string
  | View of string
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
  | Shadow of shadow_body
  | Inset of inset
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
  | Var of shadow var (* inset var(--x): whole body from one var *)
  | Body of shadow_body (* inset 2px 4px red *)
  | Toggle of { name : string; no_fallback : bool; body : shadow_body }
(* var(--name) <body>: ring inset toggle *)

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

type filter =
  | None
  | Omitted of filter_function
  | Blur of length
  | Brightness of number_percentage
  | Contrast of number_percentage
  | Drop_shadow of shadow
  | Grayscale of number_percentage
  | Hue_rotate of angle
  | Invert of number_percentage
  | Opacity of number_percentage
  | Saturate of number_percentage
  | Sepia of number_percentage
  | Url of string
  | List of filter list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of filter var

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

type color_interpolation =
  | In_oklab
  | In_oklch of hue_interpolation_method option
  | In_srgb
  | In_hsl of hue_interpolation_method option
  | In_lab
  | In_lch of hue_interpolation_method option
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
  | Xor
  | Source_in
  | Source_out
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

(** CSS Backgrounds 3 sec. 5.2 to 5.4 write the numeric halves of the
    border-image slots as [<number [0,inf]>], which a [calc()] satisfies, so
    each carries a {!type-number} rather than a float. *)
type border_image_slice_item = Number of number | Pct of float

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

type interest_delay =
  | Normal
  | Durations of duration list
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

(* Content Types *)

(** CSS Generated Content 3 sec. 2 [content]: the replacement and list forms
    both take an [<image>], which is the {!type-background_image} vocabulary
    without its comma list. *)
type content =
  | String of string
  | Quoted of { value : string; quote : char; repr : string option }
  | Image of background_image
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

type content_visibility =
  | Visible
  | Hidden
  | Auto
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of content_visibility var
      (** The CSS quotes property - defines quotation marks *)

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

type anchor_name =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of anchor_name var

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

type position_try_fallbacks =
  | None
  | Fallbacks of position_try_fallback_entry list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try_fallbacks var

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
type position_try =
  | Try of position_try_order * position_try_fallbacks
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try var

type position_visibility_condition = Anchors_visible | No_overflow

type position_visibility =
  | Always
  | Conditions of position_visibility_condition list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_visibility var

type position_area =
  | None
  | Area of position_area_keyword * position_area_keyword option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_area var

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

type font_synthesis_feature = Weight | Style | Small_caps | Position

type font_synthesis =
  | None
  | Features of font_synthesis_feature list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis var

type animation_range_name =
  | Cover
  | Contain
  | Entry
  | Exit
  | Entry_crossing
  | Exit_crossing

type animation_range_item =
  | Normal
  | Offset of length_percentage
  | Named of animation_range_name * length_percentage option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_range_item var

type animation_range =
  | Range of animation_range_item * animation_range_item option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_range var

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

type view_transition_class =
  | None
  | Classes of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_transition_class var

type image_orientation =
  | None
  | From_image
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_orientation var

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

type resolution = Dpi of float | Dpcm of float | Dppx of float | X of float

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

type contain_intrinsic_size_item = Length of length | Auto of length

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

type contain_intrinsic_longhand =
  | None
  | Size of contain_intrinsic_size_item
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of contain_intrinsic_longhand var

type margin_trim_edge = Block_start | Inline_start | Block_end | Inline_end
type margin_trim_axis = Block | Inline

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

(* clip-path property for clipping regions *)
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

type offset_anchor =
  | Auto
  | Position of position_value
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_anchor var

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

type offset_rotate_mode = Auto | Reverse

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
type offset_target =
  | Position_only of offset_position
  | With_path of {
      position : offset_position option;
      path : offset_path;
      distance : length_percentage option;
      rotate : offset_rotate option;
    }

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

type isolation =
  | Auto
  | Isolate
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of isolation var

(* Break Types - for page/column/region breaks *)
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

(* CSS Fragmentation 3 sec. 3.4 deprecated [page-break-before / -after /
   -inside] aliases. The shorter value vocabulary makes them their own type
   rather than overload [break_value]. *)
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

(* Multi-column Layout Types *)
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
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of columns_value var

(* CSS Multicol 2 sec. 4.1: [column-width] is [auto | <length [0,inf]>]. *)
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
type column_count =
  | Auto
  | Count of int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_count var

(* CSS Multicol 2 sec. 4.2: [column-height] is [auto | <length [0,inf]>]; it
   takes no percentage, which Chrome 146 refuses. Sec. 4.4 gives [column-wrap]
   the three keywords below. Both start at [auto]. *)
type column_height =
  | Auto
  | Height of length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_height var

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
type scroll_behavior =
  | Auto
  | Smooth
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_behavior var

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

type scroll_snap_stop =
  | Normal
  | Always
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_snap_stop var

type scroll_snap_strictness =
  | Mandatory
  | Proximity
  | Var of scroll_snap_strictness var

type scroll_snap_axis =
  | None
  | X
  | Y
  | Block
  | Inline
  | Both
  | Var of scroll_snap_axis var

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
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_axis var

type timeline_name =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_name var

type timeline_shorthand_item = { name : string; axis : timeline_axis option }

type timeline_shorthand =
  | None
  | Timelines of timeline_shorthand_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_shorthand var

type timeline_inset_item = Auto | Length of length_percentage

type timeline_inset =
  | Inset of timeline_inset_item * timeline_inset_item option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_inset var

type view_timeline_shorthand_item = {
  name : string;
  axis : timeline_axis option;
  inset : timeline_inset option;
}

type view_timeline_shorthand =
  | None
  | Timelines of view_timeline_shorthand_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_timeline_shorthand var

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

(* SVG Types *)
(* SVG paint servers allow url(#id) with optional fallback (none/currentcolor/color). *)
type svg_paint =
  | None
  | Inherit
  | Current_color
  | Color of color
  | Url of string * svg_paint option
  | Context_fill
      (** SVG2 sec. 13.2 [context-fill] - inherits the fill paint of the context
          element, used in marker / pattern / use trees. *)
  | Context_stroke
      (** SVG2 sec. 13.2 [context-stroke] - mirror of [Context_fill]. *)
  | Var of svg_paint var

(** SVG 2 sec. 13.4.2 and CSS Masking 1 sec. 6.2 [<fill-rule>]: which points
    count as inside a shape when its subpaths overlap. Shared by [fill-rule] and
    [clip-rule]; the argument form inside [polygon()] is {!clip_path_fill_rule},
    which carries no CSS-wide keywords. *)
type fill_rule =
  | Nonzero
  | Evenodd
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of fill_rule var

(** SVG 2 sec. 13.5.3 [stroke-width]: [<length-percentage> | <number>], where a
    bare [<number>] is a width in user units. That number is not a CSS
    [<length>], which is why this is not plain [length_percentage]; sec. 13.5.6
    gives the dash lengths the same shape. A negative width is invalid. *)
type stroke_width =
  | Number of float
  | Length of length_percentage
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_width var

(** SVG 2 sec. 13.5.4 [stroke-linecap]: the shape drawn at the ends of an open
    subpath and at the ends of each dash. *)
type stroke_linecap =
  | Butt
  | Round
  | Square
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_linecap var

(** SVG 2 sec. 13.5.5 [stroke-linejoin]: the shape drawn where two path segments
    meet. [miter_clip] and [arcs] are the Level 2 additions. *)
type stroke_linejoin =
  | Miter
  | Miter_clip
  | Round
  | Bevel
  | Arcs
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_linejoin var

(** SVG 2 sec. 13.5.5 [stroke-miterlimit]: the ratio at which a miter join is
    converted to a bevel. Only a negative value is illegal. *)
type stroke_miterlimit =
  | Number of float
  | Calc of stroke_miterlimit calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_miterlimit var

(** SVG 2 sec. 13.5.6: one dash length. A bare [<number>] is in user units,
    which is why this is not plain [length_percentage]. CSS Values 4 sec. 10.1
    puts a math function wherever a number is, so that half is a {!type-number}.
*)
type dash_length = Number of number | Length of length_percentage

(** SVG 2 sec. 13.5.6 [stroke-dashoffset]. *)
type stroke_dashoffset =
  | Dash of dash_length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_dashoffset var

(** SVG 2 sec. 13.5.6 [stroke-dasharray]: [none] or a dash pattern. The grammar
    separates entries by comma and/or whitespace and the rendered pattern is the
    flat sequence either way, so both spellings read to one list. *)
type stroke_dasharray =
  | None
  | Dashes of dash_length list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_dasharray var

(** SVG 2 sec. 13.8 [paint-order] operand. *)
type paint_order_keyword = Fill | Stroke | Markers

(** SVG 2 sec. 13.8 [paint-order]: [normal | [ fill || stroke || markers ]]. The
    written order is the paint order, and any keyword left out is painted last
    in the order [normal] would use, so a trailing run that already matches
    [normal] is redundant. Entries are distinct: [||] takes each operand at most
    once. *)
type paint_order =
  | Normal
  | Order of paint_order_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of paint_order var

(** SVG 2 sec. 8.13 [vector-effect] operand. *)
type vector_effect_keyword =
  | Non_scaling_stroke
  | Non_scaling_size
  | Non_rotation
  | Fixed_position

(** SVG 2 sec. 8.13 host coordinate space for a vector effect. [viewport] is
    what an omitted space means. *)
type vector_effect_space = Viewport | Screen

(** SVG 2 sec. 8.13 [vector-effect]:
    [none | [ non-scaling-stroke | non-scaling-size | non-rotation |
     fixed-position ]+ [ viewport | screen ]?]. *)
type vector_effect =
  | None
  | Effects of vector_effect_keyword list * vector_effect_space option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of vector_effect var

(* Direction Types *)
type direction =
  | Ltr
  | Rtl
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of direction var

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
  | None
  | Auto
  | Button
  | Textfield
  | Menulist
  | Base_select
  | Listbox
  | Checkbox
  | Radio
  | Push_button
  | Square_button
  | Apple_pay_button
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_appearance var

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

type moz_osx_font_smoothing =
  | Auto
  | Grayscale
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of moz_osx_font_smoothing var

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

type webkit_line_clamp =
  | None
  | Lines of int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_line_clamp var

type text_size_adjust =
  | None
  | Auto
  | Pct of float
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_size_adjust var

(* Other Types *)
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
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_origin var

(* transform-box property: establishes the reference box for transform *)
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

(* clip property (deprecated, but needed for sr-only) *)
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

type _ kind =
  | Length : length kind
  | Color : color kind
  | Rgb : rgb kind
  | Number : number kind
  | Int : int kind
  | Float : float kind
  | Percentage : percentage kind
  | Length_percentage : length_percentage kind
  | Number_percentage : number_percentage kind
  | Opacity : opacity kind
  | Value : custom_value kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Outline_style : outline_style kind
  | Border : border kind
  | Font_weight : font_weight kind
  | Font_size : font_size kind
  | Line_height : line_height kind
  | Font_family : font_family kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Rotate : rotate_value kind
  | Scale : scale kind
  | Shadow : shadow kind
  | Content : content kind
  | Gradient_stop : gradient_stop kind
  | Gradient_direction : gradient_direction kind
  | Gradient_position : gradient_position kind
  | Radial_shape : radial_shape kind
  | Radial_size : radial_size kind
  | Position_value : position_value kind
  | Animation : animation kind
  | Timing_function : timing_function kind
  | Transform : transform kind
  | Touch_action : touch_action kind
  | Transition_property_value : transition_property_value kind
  | Background_image : background_image kind
  | Z_index : z_index kind
  | Filter : filter kind
  | Font_src : Font_face.src kind

type custom_property =
  | Custom_value : {
      value : custom_property_value;
      layer : string option;
      meta : meta option;
    }
      -> custom_property

and custom_property_value =
  | Typed : { kind : 'a kind; value : 'a } -> custom_property_value
  | Tokens of custom_value

(** [all] shorthand value (CSS Cascade 5 sec. 3.2). The [all] property only
    accepts CSS-wide keywords - no other syntax is valid. *)
type css_wide =
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of css_wide var

(* Property type definition *)
type 'a property =
  | Custom_property : string -> custom_property property
  | Unknown_property : string -> custom_value property
  | All : css_wide property
  | Background_color : color property
  | Color : color property
  | Border_color : color list property
  | Border_style : border_style list property
  | Border_top_style : border_style property
  | Border_right_style : border_style property
  | Border_bottom_style : border_style property
  | Border_left_style : border_style property
  | Border_inline_start_style : border_style property
  | Border_inline_end_style : border_style property
  | Border_block_start_style : border_style property
  | Border_block_end_style : border_style property
  | Padding : length list property
  | Padding_left : length property
  | Padding_right : length property
  | Padding_bottom : length property
  | Padding_top : length property
  | Padding_inline : length list property
  | Padding_inline_start : length property
  | Padding_inline_end : length property
  | Padding_block : length list property
  | Padding_block_start : length property
  | Padding_block_end : length property
  | Margin : length list property
  | Margin_inline_end : length property
  | Margin_inline_start : length property
  | Margin_left : length property
  | Margin_right : length property
  | Margin_top : length property
  | Margin_bottom : length property
  | Margin_inline : length list property
  | Margin_block : length list property
  | Margin_block_start : length property
  | Margin_block_end : length property
  | Gap : gap property
  | Column_gap : length property
  | Row_gap : length property
  | Width : length_percentage property
  | Height : length_percentage property
  | Min_width : length_percentage property
  | Min_height : length_percentage property
  | Max_width : length_percentage property
  | Max_height : length_percentage property
  | Inline_size : length_percentage property
  | Min_inline_size : length_percentage property
  | Max_inline_size : length_percentage property
  | Block_size : length_percentage property
  | Min_block_size : length_percentage property
  | Max_block_size : length_percentage property
  | Font_size : font_size property
  | Line_height : line_height property
  | Font_weight : font_weight property
  | Font_style : font_style property
  | Text_align : text_align property
  | Text_decoration : text_decoration property
  | Text_decoration_line : text_decoration_line list property
  | Text_decoration_style : text_decoration_style property
  | Text_decoration_color : color property
  | Text_underline_offset : length property
  | Text_decoration_skip : text_decoration_skip property
  | Text_decoration_skip_self : text_decoration_skip_self property
  | Text_decoration_skip_box : text_decoration_skip_box property
  | Text_decoration_skip_inset : text_decoration_skip_inset property
  | Text_decoration_skip_spaces : text_decoration_skip_spaces property
  | Text_emphasis : text_emphasis property
  | Text_emphasis_style : text_emphasis_style property
  | Text_emphasis_color : color property
  | Text_emphasis_position : text_emphasis_position property
  | Text_emphasis_skip : text_emphasis_skip property
  | Text_orientation : text_orientation property
  | Text_transform : text_transform property
  | Letter_spacing : length property
  | List_style_type : list_style_type property
  | List_style_position : list_style_position property
  | List_style_image : list_style_image property
  | Display : display property
  | Position : position property
  | Visibility : visibility property
  | Baseline_source : baseline_source property
  | Alignment_baseline : alignment_baseline property
  | Baseline_shift : baseline_shift property
  | Flex_direction : flex_direction property
  | Flex_wrap : flex_wrap property
  | Flex_flow : flex_flow property
  | Flex : flex property
  | Flex_grow : flex_factor property
  | Flex_shrink : flex_factor property
  | Flex_basis : flex_basis property
  | Order : order property
  | Align_items : align_items property
  | Justify_content : justify_content property
  | Justify_items : justify_items property
  | Justify_self : justify_self property
  | Align_content : align_content property
  | Align_self : align_self property
  | Place_content : place_content property
  | Place_items : place_items property
  | Place_self : (align_self * justify_self) property
  | Grid_template_columns : grid_template property
  | Grid_template_rows : grid_template property
  | Grid_template_areas : grid_template_areas property
  | Grid_template : grid_template property
  | Grid : grid_template property
      (** CSS Grid 1 sec. 7.8 [grid] shorthand. Cascade treats it as a free-form
          [grid_template] for now: the simple cases (track-list, grid-template
          syntax with area strings) round-trip through the same AST as
          [grid-template], and inputs that exercise the auto-flow branches fall
          back to the raw [Template] preservation arm. *)
  | Grid_area : grid_area property
  | Grid_auto_flow : grid_auto_flow property
  | Grid_auto_columns : grid_template property
  | Grid_auto_rows : grid_template property
  | Grid_column : grid_line_pair property
  | Grid_row : grid_line_pair property
  | Grid_column_start : grid_line property
  | Grid_column_end : grid_line property
  | Grid_row_start : grid_line property
  | Grid_row_end : grid_line property
  | Border_width : border_width list property
  | Border_top_width : border_width property
  | Border_right_width : border_width property
  | Border_bottom_width : border_width property
  | Border_left_width : border_width property
  | Border_inline_start_width : border_width property
  | Border_inline_end_width : border_width property
  | Border_block_start_width : border_width property
  | Border_block_end_width : border_width property
  | Border_inline_width : logical_border_width property
  | Border_block_width : logical_border_width property
  | Border_image : border_image property
  | Border_image_source : background_image property
  | Border_image_slice : border_image_slice property
  | Border_image_repeat : border_image_repeat property
  | Border_image_width : border_image_width property
  | Border_image_outset : border_image_outset property
  | Border_radius : border_radius property
  | Border_top_left_radius : length list property
  | Border_top_right_radius : length list property
  | Border_bottom_left_radius : length list property
  | Border_bottom_right_radius : length list property
  | Border_top_color : color property
  | Border_right_color : color property
  | Border_bottom_color : color property
  | Border_left_color : color property
  | Border_inline_start_color : color property
  | Border_inline_end_color : color property
  | Border_block_start_color : color property
  | Border_block_end_color : color property
  | Border_inline_color : logical_border_color property
  | Border_block_color : logical_border_color property
  | Border_inline_style : logical_border_style property
  | Border_block_style : logical_border_style property
  | Border_start_start_radius : length list property
  | Border_start_end_radius : length list property
  | Border_end_start_radius : length list property
  | Border_end_end_radius : length list property
  | Opacity : opacity property
  | Fill_opacity : opacity property
  | Stroke_opacity : opacity property
  | Stop_opacity : opacity property
  | Flood_opacity : opacity property
  | Mix_blend_mode : blend_mode property
  | Transform : transform list property
  | Translate : translate_value property
  | Cursor : cursor property
  | Interactivity : interactivity property
  | Caret_animation : caret_animation property
  | Caret_shape : caret_shape property
  | Caret : caret property
  | Interest_delay : interest_delay property
  | Interest_delay_start : interest_delay property
  | Interest_delay_end : interest_delay property
  | Nav_up : nav property
  | Nav_right : nav property
  | Nav_down : nav property
  | Nav_left : nav property
  | Table_layout : table_layout property
  | Border_collapse : border_collapse property
  | Border_spacing : border_spacing property
  | User_select : user_select property
  | Pointer_events : pointer_events property
  | Overflow : overflow property
  | Inset : length list property
  | Inset_inline : length list property
  | Inset_inline_start : length list property
  | Inset_inline_end : length list property
  | Inset_block : length list property
  | Inset_block_start : length list property
  | Inset_block_end : length list property
  | Top : length list property
  | Right : length list property
  | Bottom : length list property
  | Left : length list property
  | Z_index : z_index property
  | Outline : outline property
  | Outline_style : outline_style property
  | Outline_width : border_width property
  | Outline_color : color property
  | Outline_offset : length property
  | Forced_color_adjust : forced_color_adjust property
  | Scroll_snap_type : scroll_snap_type property
  | White_space : white_space property
  | White_space_collapse : white_space_collapse property
  | Border : border property
  | Border_block : border property
  | Border_block_start : border property
  | Border_block_end : border property
  | Border_inline : border property
  | Border_inline_start : border property
  | Border_inline_end : border property
  | Background : background list property
  | Tab_size : tab_size property
  | Zoom : zoom property
  | Webkit_text_size_adjust : text_size_adjust property
  | Font_feature_settings : font_feature_settings property
  | Font_variation_settings : font_variation_settings property
  | Webkit_tap_highlight_color : color property
  | Webkit_user_select : user_select property
  | Moz_user_select : user_select property
  | Ms_user_select : user_select property
  | Webkit_text_decoration : text_decoration property
  | Webkit_text_decoration_color : color property
  | Webkit_text_fill_color : color property
  | Webkit_text_stroke : webkit_text_stroke property
  | Webkit_text_stroke_width : border_width property
  | Webkit_text_stroke_color : color property
  | Text_indent : text_indent_value property
  | List_style : list_style property
  | Font : font property
  | Source : Font_face.src property
  | Webkit_appearance : webkit_appearance property
  | Webkit_transform : transform list property
  | Moz_transform : transform list property
  | Ms_transform : transform list property
  | O_transform : transform list property
  | Webkit_transition : transition list property
  | Webkit_transition_delay : duration property
  | Webkit_transition_duration : duration property
  | Webkit_transition_property : transition_property property
  | Webkit_transition_timing_function : timing_function property
  | Webkit_animation : animation list property
  | Webkit_animation_delay : duration property
  | Webkit_animation_duration : duration property
  | Webkit_animation_direction : animation_direction property
  | Webkit_animation_iteration_count : animation_iteration_count property
  | Webkit_animation_name : animation_name property
  | Webkit_animation_timing_function : timing_function property
  | Webkit_animation_fill_mode : animation_fill_mode property
  | Webkit_animation_play_state : animation_play_state property
  | Webkit_flex_direction : flex_direction property
  | Webkit_flex_wrap : flex_wrap property
  | Webkit_flex_flow : flex_flow property
  | Webkit_justify_content : justify_content property
  | Webkit_align_items : align_items property
  | Webkit_align_content : align_content property
  | Webkit_align_self : align_self property
  | Webkit_border_radius : border_radius property
  | Webkit_box_sizing : box_sizing property
  | Moz_box_sizing : box_sizing property
  | Webkit_box_shadow : shadow property
  | Webkit_background_size : background_size property
  | Webkit_filter : filter property
  | Moz_appearance : appearance property
  | Moz_animation : animation list property
  | Moz_animation_delay : duration property
  | Moz_animation_duration : duration property
  | Moz_animation_direction : animation_direction property
  | Moz_animation_iteration_count : animation_iteration_count property
  | Moz_animation_name : animation_name property
  | Moz_animation_timing_function : timing_function property
  | Moz_animation_fill_mode : animation_fill_mode property
  | Moz_animation_play_state : animation_play_state property
  | Moz_transition : transition list property
  | Moz_transition_delay : duration property
  | Moz_transition_duration : duration property
  | Moz_transition_property : transition_property property
  | Moz_transition_timing_function : timing_function property
  | Moz_border_radius : border_radius property
  | Moz_box_shadow : shadow property
  | Ms_filter : filter property
  | O_transition : transition list property
  | Container_type : container_type property
  | Container_name : container_name property
  | Container : container_shorthand property
  | Anchor_name : anchor_name property
  | Position_anchor : position_anchor property
  | Position_try_fallbacks : position_try_fallbacks property
  | Position_try_order : position_try_order property
  | Position_try : position_try property
  | Position_visibility : position_visibility property
  | Position_area : position_area property
  | Shape_outside : string property
  | Shape_margin : length_percentage property
  | Shape_image_threshold : shape_image_threshold property
  | Overflow_clip_margin : overflow_clip_margin property
  | Overflow_anchor : overflow_anchor property
  | Scrollbar_width : scrollbar_width property
  | Scrollbar_color : scrollbar_color property
  | Scrollbar_gutter : scrollbar_gutter property
  | Line_height_step : length property
  | Font_palette : font_palette property
  | Font_synthesis : font_synthesis property
  | Text_wrap_mode : text_wrap_mode property
  | Text_wrap_style : text_wrap_style property
  | Text_box_trim : text_box_trim property
  | Text_underline_position : text_underline_position property
  | Text_box_edge : text_box_edge property
  | Text_box : text_box property
  | Inline_sizing : inline_sizing property
  | Line_fit_edge : line_fit_edge property
  | Interpolate_size : interpolate_size property
  | Min_intrinsic_sizing : min_intrinsic_sizing property
  | Ruby_align : ruby_align property
  | Ruby_merge : ruby_merge property
  | Ruby_overhang : ruby_overhang property
  | Ruby_position : ruby_position property
  | Glyph_orientation_vertical : glyph_orientation_vertical property
  | Text_combine_upright : text_combine_upright property
  | Animation_timeline : animation_timeline property
  | Animation_range : animation_range property
  | Animation_range_start : animation_range_item property
  | Animation_range_end : animation_range_item property
  | Scroll_timeline : timeline_shorthand property
  | Scroll_timeline_name : timeline_name property
  | Scroll_timeline_axis : timeline_axis property
  | View_transition_name : view_transition_name property
  | View_transition_class : view_transition_class property
  | Image_orientation : image_orientation property
  | Image_rendering : image_rendering property
  | Image_resolution : image_resolution property
  | Contain_intrinsic_size : contain_intrinsic_size property
  | Contain_intrinsic_width : contain_intrinsic_longhand property
  | Contain_intrinsic_height : contain_intrinsic_longhand property
  | Contain_intrinsic_block_size : contain_intrinsic_longhand property
  | Contain_intrinsic_inline_size : contain_intrinsic_longhand property
  | Margin_trim : margin_trim property
  | Offset_path : offset_path property
  | Offset_distance : length_percentage property
  | Offset_rotate : offset_rotate property
  | Font_size_adjust : font_size_adjust property
  | Font_variant_emoji : font_variant_emoji property
  | Text_spacing_trim : text_spacing_trim property
  | Hyphenate_limit_chars : hyphenate_limit_chars property
  | Initial_letter : initial_letter property
  | Initial_letter_align : initial_letter_align property
  | Initial_letter_wrap : initial_letter_wrap property
  | Dominant_baseline : dominant_baseline property
  | View_timeline_name : timeline_name property
  | View_timeline_axis : timeline_axis property
  | View_timeline_inset : timeline_inset property
  | View_timeline : view_timeline_shorthand property
  | Timeline_scope : timeline_name property
  | Perspective : length property
  | Perspective_origin : perspective_origin property
  | Transform_style : transform_style property
  | Backface_visibility : backface_visibility property
  | Object_position : position_value property
  | Rotate : rotate_value property
  | Transition_duration : duration property
  | Transition_timing_function : timing_function property
  | Transition_delay : duration property
  | Transition_property : transition_property property
  | Transition_behavior : transition_behavior property
  | Overlay : overlay property
  | Will_change : will_change property
  | Contain : contain property
  | Isolation : isolation property
  | Break_before : break_value property
  | Break_after : break_value property
  | Break_inside : break_inside_value property
  | Page_break_before : page_break_value property
  | Page_break_after : page_break_value property
  | Page_break_inside : page_break_inside_value property
  | Page_size : page_size property
  | Columns : columns_value property
  | Column_width : column_width property
  | Column_height : column_height property
  | Column_wrap : column_wrap property
  | Column_count : column_count property
  | Column_rule : border property
      (** CSS Gaps 1 sec. 4 gives the three longhands below a comma-separated
          list, one entry per gap decoration line. *)
  | Column_rule_width : border_width list property
  | Column_rule_style : border_style list property
  | Column_rule_color : color list property
  | Column_span : column_span property
  | Word_spacing : length property
  | Background_attachment : background_attachment property
  | Border_top : border property
  | Border_right : border property
  | Border_bottom : border property
  | Border_left : border property
  | Transform_origin : transform_origin property
  | Transform_box : transform_box property
  | Text_shadow : text_shadow list property
  | Clip_path : clip_path property
  | Mask : mask property
  | Mask_border : border_image property
  | Content_visibility : content_visibility property
  | Filter : filter property
  | Background_image : background_image list property
  | Background_origin : background_box property
  | Background_clip : background_box property
  | Webkit_background_clip : background_box property
  | Animation : animation list property
  | Aspect_ratio : aspect_ratio property
  | Overflow_x : overflow property
  | Overflow_y : overflow property
  | Overflow_block : overflow property
  | Overflow_inline : overflow property
  | Vertical_align : vertical_align property
  | Font_family : font_family property
  | Background_position : background_position property
  | Background_position_x : background_position_axis property
  | Background_position_y : background_position_axis property
  | Webkit_mask_position_x : background_position_axis property
  | Webkit_mask_position_y : background_position_axis property
  | Background_repeat : background_repeat property
  | Background_size : background_size property
  | Webkit_font_smoothing : webkit_font_smoothing property
  | Moz_osx_font_smoothing : moz_osx_font_smoothing property
  | Webkit_line_clamp : webkit_line_clamp property
  | Webkit_box_orient : webkit_box_orient property
  | Moz_orient : moz_orient property
  | Text_overflow : text_overflow property
  | Text_wrap : text_wrap property
  | Word_break : word_break property
  | Overflow_wrap : overflow_wrap property
  | Line_break : line_break property
  | Hyphens : hyphens property
  | Webkit_hyphens : hyphens property
  | Font_stretch : font_stretch property
  | Font_optical_sizing : font_optical_sizing property
  | Font_kerning : font_kerning property
  | Font_language_override : font_language_override property
  | Font_synthesis_style : font_synthesis_style property
  | Font_synthesis_weight : font_synthesis_weight property
  | Font_synthesis_small_caps : font_synthesis_small_caps property
  | Font_synthesis_position : font_synthesis_position property
  | Font_variant_ligatures : font_variant_ligatures property
  | Caps : font_variant_caps property
  | Numeric : font_variant_numeric property
  | Font_variant_position : font_variant_position property
  | Font_variant_alternates : font_variant_alternates property
  | Font_variant : font_variant property
  | East_asian : font_variant_east_asian property
  | Backdrop_filter : filter property
  | Webkit_backdrop_filter : filter property
  | Webkit_mask_image : background_image property
  | Webkit_mask_composite : webkit_mask_composite property
  | Webkit_mask_source_type : webkit_mask_source_type property
  | Webkit_mask_size : background_size property
  | Webkit_mask_position : background_position property
  | Webkit_mask_repeat : background_repeat property
  | Webkit_mask_clip : mask_box property
  | Webkit_mask_origin : mask_box property
  | Mask_image : background_image property
  | Mask_composite : mask_composite property
  | Mask_mode : mask_mode property
  | Mask_size : background_size property
  | Mask_position : background_position property
  | Mask_repeat : background_repeat property
  | Mask_clip : mask_box property
  | Mask_origin : mask_box property
  | Mask_type : mask_type property
  | Scroll_snap_align : scroll_snap_align property
  | Scroll_snap_stop : scroll_snap_stop property
  | Scroll_behavior : scroll_behavior property
  | Box_sizing : box_sizing property
  | Field_sizing : field_sizing property
  | Caption_side : caption_side property
  | Resize : resize property
  | Object_fit : object_fit property
  | Object_view_box : object_view_box property
  | Appearance : appearance property
  | Color_scheme : color_scheme property
  | Print_color_adjust : print_color_adjust property
  | Webkit_print_color_adjust : print_color_adjust property
  | Box_decoration_break : box_decoration_break property
  | Webkit_box_decoration_break : box_decoration_break property
  | Content : content property
  | Counter_reset : counter_set property
  | Counter_increment : counter_set property
  | Quotes : quotes property
  | Text_decoration_thickness : length property
  | Text_size_adjust : text_size_adjust property
  | Touch_action : touch_action property
  | Clip : clip property
  | Clear : clear property
  | Float : float_side property
  | Scale : scale property
  | Transition : transition list property
  | Box_shadow : shadow property
  | Fill : svg_paint property
  | Stroke : svg_paint property
  | Stroke_width : stroke_width property
  | Fill_rule : fill_rule property
  | Clip_rule : fill_rule property
  | Stroke_linecap : stroke_linecap property
  | Stroke_linejoin : stroke_linejoin property
  | Stroke_miterlimit : stroke_miterlimit property
  | Stroke_dashoffset : stroke_dashoffset property
  | Stroke_dasharray : stroke_dasharray property
  | Paint_order : paint_order property
  | Vector_effect : vector_effect property
  | Stop_color : color property
  | Flood_color : color property
  | Lighting_color : color property
  | Direction : direction property
  | Unicode_bidi : unicode_bidi property
  | Writing_mode : writing_mode property
  | Text_decoration_skip_ink : text_decoration_skip_ink property
  | Animation_name : animation_name property
  | Animation_duration : duration property
  | Animation_timing_function : timing_function property
  | Animation_delay : duration property
  | Animation_iteration_count : animation_iteration_count property
  | Animation_direction : animation_direction property
  | Animation_fill_mode : animation_fill_mode property
  | Animation_play_state : animation_play_state property
  | Animation_composition : animation_composition property
  | Background_blend_mode : blend_mode list property
  | Scroll_margin : length list property
  | Scroll_margin_top : length property
  | Scroll_margin_right : length property
  | Scroll_margin_bottom : length property
  | Scroll_margin_left : length property
  | Scroll_margin_inline : length list property
  | Scroll_margin_inline_start : length property
  | Scroll_margin_inline_end : length property
  | Scroll_margin_block : length list property
  | Scroll_margin_block_start : length property
  | Scroll_margin_block_end : length property
  | Scroll_padding : length list property
  | Scroll_padding_top : length property
  | Scroll_padding_right : length property
  | Scroll_padding_bottom : length property
  | Scroll_padding_left : length property
  | Scroll_padding_inline : length list property
  | Scroll_padding_inline_start : length property
  | Scroll_padding_inline_end : length property
  | Scroll_padding_block : length list property
  | Scroll_padding_block_start : length property
  | Scroll_padding_block_end : length property
  | Overscroll_behavior : overscroll_behavior list property
  | Overscroll_behavior_x : overscroll_behavior property
  | Overscroll_behavior_y : overscroll_behavior property
  | Overscroll_behavior_block : overscroll_behavior property
  | Overscroll_behavior_inline : overscroll_behavior property
  | Accent_color : color property
  | Caret_color : color property
  | Offset_anchor : offset_anchor property
  | Offset_position : offset_position property
  | Offset : offset property

type any_property = Prop : 'a property -> any_property

type _ property_value_kind =
  | Length : length property_value_kind
  | Lengths : length list property_value_kind
  | Length_percentage : length_percentage property_value_kind
  | Border_width : border_width property_value_kind
  | Border_widths : border_width list property_value_kind
  | Opacity : opacity property_value_kind
  | Rotate : rotate_value property_value_kind
  | Duration : duration property_value_kind
  | Number_percentage : number_percentage property_value_kind
  | Font_size : font_size property_value_kind
  | Display : display property_value_kind
  | Position : position property_value_kind
  | Visibility : visibility property_value_kind
  | Clear : clear property_value_kind
  | Float : float_side property_value_kind
  | Scale : scale property_value_kind
  | Translate : translate_value property_value_kind
  | Transform : transform list property_value_kind
  | Animation : animation list property_value_kind
  | Transition : transition list property_value_kind
  | Filter : filter property_value_kind
  | Shadow : shadow property_value_kind
  | Border_radius : border_radius property_value_kind
  | Color : color property_value_kind
  | Colors : color list property_value_kind
  | Animation_name : animation_name property_value_kind
  | Background : background list property_value_kind
  | Background_image : background_image property_value_kind
  | Background_images : background_image list property_value_kind
  | Font_src : Font_face.src property_value_kind
  | Font_family : font_family property_value_kind
  | Stroke_width : stroke_width property_value_kind

let equal_overflow (a : overflow) b = a = b
let equal_grid_line (a : grid_line) b = a = b
let equal_text_emphasis_skip_keyword (a : text_emphasis_skip_keyword) b = a = b

let equal_min_intrinsic_sizing_keyword (a : min_intrinsic_sizing_keyword) b =
  a = b

let equal_initial_letter_align_keyword (a : initial_letter_align_keyword) b =
  a = b

let equal_ruby_position_keyword (a : ruby_position_keyword) b = a = b
let equal_background_shorthand (a : background_shorthand) b = a = b
let equal_background_box (a : background_box) b = a = b
let equal_mask_layer (a : mask_layer) b = a = b
let equal_position_area_keyword (a : position_area_keyword) b = a = b

let equal_position_visibility_condition (a : position_visibility_condition) b =
  a = b

let equal_animation_range_name (a : animation_range_name) b = a = b
let equal_container_shorthand (a : container_shorthand) b = a = b
let equal_paint_order_keyword (a : paint_order_keyword) b = a = b
let equal_paint_order (a : paint_order) b = a = b
let equal_border_width (a : border_width) b = a = b
let equal_overscroll_behavior (a : overscroll_behavior) b = a = b

let equal_contain_intrinsic_size_item (a : contain_intrinsic_size_item) b =
  a = b

let equal_border_style (a : border_style) b = a = b
