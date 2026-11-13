(** Display, positioning, overflow, flexbox, box alignment and grid value types,
    with the border-style, line-height and font-weight types they share. *)

open Values

(** CSS border style values. *)
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

(** CSS line-height values *)
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
  (* CSS Values 4 sec. 10.2 comparison functions over the [<length-percentage>]
     half of the grammar. A comparison answers with one of its arguments, and a
     length beside a percentage resolves only at used-value time, so the call
     stands here rather than folding; the arguments reuse [line_height], which
     already carries [Pct], the way [length] carries its own. *)
  | Min of line_height list
  | Max of line_height list
  | Clamp of line_height * line_height * line_height
      (** CSS Values 4 sec. 10.2 comparison functions over the
          [<length-percentage>] half of the grammar. A length beside a
          percentage resolves only at used-value time, so the call stands here
          rather than folding to one of its arguments. *)
  | Calc of line_height calc
  | Var of line_height var

(** CSS font weight values. *)
type font_weight =
  | Weight of float
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Calc of font_weight calc
      (** CSS Values 4 sec. 10 allows a math function wherever a [<number>] is
          allowed, and one that does not fold to a constant stays here. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_weight var

(* Display & Layout Types *)

(** CSS display values. *)
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

(** CSS position values. *)
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

(** CSS visibility values. *)
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

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-baseline-source}
     [baseline-source]}: which line box baseline an inline block aligns on. *)
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

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/text.html#AlignmentBaselineProperty}
     [alignment-baseline]}: the baseline of the box aligned against its parent's
    dominant baseline. *)
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

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-baseline-shift}
     [baseline-shift]}:
    [<length-percentage> | sub | super | top | center | bottom]. *)
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

(** CSS z-index values. *)
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

(** CSS opacity values. *)
type opacity =
  | Opacity_number of float
  | Calc of opacity calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of opacity var

(** CSS Shapes 1
    {{:https://drafts.csswg.org/css-shapes-1/#propdef-shape-image-threshold}
     [shape-image-threshold]}: the alpha above which a pixel of the shape image
    is inside the shape. *)
type shape_image_threshold =
  | Number of float
  | Calc of shape_image_threshold calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of shape_image_threshold var

(** CSS Text 4 sec. 4.4 [tab-size]: [<number [0,inf]> | <length [0,inf]>], the
    tab stop measured in advance widths or in a length. *)
type tab_size =
  | Number of number
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
  | Calc of zoom calc
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of zoom var

(** CSS order values (flexbox order). *)
type order =
  | Int of int
  | Calc of order calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of order var

(** CSS overflow values. *)
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

(** CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#typedef-visual-box} [<visual-box>]}:
    the box edge an overflow clip margin is measured from. *)
type overflow_clip_box = Content_box | Padding_box | Border_box

(** CSS Overflow 4
    {{:https://drafts.csswg.org/css-overflow-4/#propdef-overflow-clip-margin}
     [overflow-clip-margin]}: [<visual-box> || <length>]. *)
type overflow_clip_margin =
  | Clip_margin of overflow_clip_box option * length option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of overflow_clip_margin var

(* Flexbox Types *)

(** CSS flex direction values. *)
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

(** CSS align-items values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items} MDN:
     align-items} *)
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

(** CSS align-self values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self} MDN:
     align-self} *)
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

(** CSS justify-content values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content} MDN:
     justify-content} *)
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

(** CSS justify-items values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items} MDN:
     justify-items} *)
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

(** CSS justify-self values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self} MDN:
     justify-self} *)
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

(** CSS flex basis values. *)
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
      (** A length in a unit [border_width] does not name, carrying the authored
          spelling in [repr] the way {!length} does. *)
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

(** CSS flex shorthand values. *)
type flex =
  | Initial  (** 0 1 auto *)
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Auto  (** 1 1 auto *)
  | None  (** 0 0 auto *)
  | Grow of flex_factor  (** Single grow value *)
  | Basis of flex_basis  (** 1 1 <flex-basis> *)
  | Grow_shrink of flex_factor * flex_factor  (** grow shrink 0% *)
  | Full of flex_factor * flex_factor * flex_basis  (** grow shrink basis *)
  | Var of flex var

(** CSS font-size values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-size} MDN:
     font-size} *)
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

(** CSS place-content values *)
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

(** CSS place-items values *)
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

(** One component in a [grid-auto-flow] value. *)
type grid_auto_flow_component =
  | Axis of [ `Row | `Column ]
  | Dense
  | Var of grid_auto_flow_component var

(** CSS grid-auto-flow values *)
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
      (** CSS Grid 3 (ED) REMOVED this value, replacing the masonry track
          vocabulary with a single [flow-tolerance] property, and web-features
          records no compat key for it, so no target set can arbitrate it
          either. It stays because Firefox ships it behind a pref: refusing it
          would drop a declaration a shipping browser renders, which is the
          destructive direction. The four [masonry-*] and [item-flow] properties
          of the same draft are not modelled here at all, and survive on the
          generic unknown-property path that keeps any property cascade does not
          know. *)
  | Var of grid_template var

(** CSS grid-template-areas values *)
type grid_template_areas =
  | No_areas
  | Areas of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of grid_template_areas var

(** CSS grid line values *)
type grid_line =
  | Auto  (** auto *)
  | Num of int  (** 1, 2, 3, ... or -1, -2, ... *)
  | Name of string  (** "header-start", "main-end", etc. *)
  | Num_name of int * string  (** <integer> <custom-ident> *)
  | Span of int  (** span 2, span 3, etc. *)
  | Span_name of string  (** span <custom-ident> *)
  | Span_num_name of int * string  (** span <integer> <custom-ident> *)
  | Calc of grid_line calc  (** calc(12 * -1), etc. *)
  | Calc_name of grid_line calc * string
      (** CSS Values 4 sec. 10 allows a math function wherever an [<integer>] is
          allowed, so the index of a named line may be one. *)
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

(** CSS aspect-ratio values *)
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
