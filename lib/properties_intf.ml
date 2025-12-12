(** Property type shared between properties.ml and properties.mli *)

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
  | Var of border_style var

type line_height =
  | Normal
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Num of float
  | Inherit
  | Var of line_height var
  | Calc of line_height calc

type font_weight =
  | Weight of int
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Inherit
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
  | Inherit
  | Initial
  | Unset

type position = Static | Relative | Absolute | Fixed | Sticky
type visibility = Visible | Hidden | Collapse
type z_index = Auto | Index of int
type overflow = Visible | Hidden | Scroll | Auto | Clip

(* Flexbox Types *)
type flex_direction = Row | Row_reverse | Column | Column_reverse
type flex_wrap = Nowrap | Wrap | Wrap_reverse

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
  | Left
  | Right
  (* Safe content position values *)
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
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Anchor_center

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
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end

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
  | Max_content
  | Min_content
  | From_font
  | Var of flex_basis var
  | Calc of flex_basis calc

type border_width =
  | Thin
  | Medium
  | Thick
  | Px of float
  | Rem of float
  | Em of float
  | Ch of float
  | Vh of float
  | Vw of float
  | Vmin of float
  | Vmax of float
  | Pct of float
  | Zero
  | Auto
  | Max_content
  | Min_content
  | Fit_content
  | From_font
  | Calc of border_width calc
  | Var of border_width var
  | Inherit

type flex =
  | Initial (* 0 1 auto *)
  | Auto (* 1 1 auto *)
  | None (* 0 0 auto *)
  | Grow of float (* Single grow value *)
  | Basis of flex_basis (* 1 1 <flex-basis> *)
  | Grow_shrink of float * float (* grow shrink 0% *)
  | Full of float * float * flex_basis (* grow shrink basis *)

type place_content =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly
  | Align_justify of align_content * justify_content
  | Inherit

type place_items =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Align_justify of align_items * justify_items
  | Inherit

(* Grid Types *)
type grid_auto_flow = Row | Column | Dense | Row_dense | Column_dense

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
  | Fr of float
  | Auto
  | Min_content
  | Max_content
  | Inherit
  (* Complex track values *)
  | Min_max of grid_template * grid_template
  | Fit_content of length
  | Repeat of int * grid_template list
  | Tracks of grid_template list
  | Named_tracks of (string option * grid_template) list
  | Subgrid
  | Masonry

type grid_line = Auto | Num of int | Name of string | Span of int
type aspect_ratio = Auto | Ratio of float * float | Inherit
type font_style = Normal | Italic | Oblique | Inherit

type text_align =
  | Left
  | Right
  | Center
  | Justify
  | Start
  | End
  | Match_parent
  | Inherit

type text_decoration_line = Underline | Overline | Line_through
type text_decoration_style = Solid | Double | Dotted | Dashed | Wavy | Inherit

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
  | Var of text_decoration var

type text_transform =
  | None
  | Capitalize
  | Uppercase
  | Lowercase
  | Full_width
  | Full_size_kana
  | Inherit
  | Var of text_transform var

type text_overflow = Clip | Ellipsis | String of string | Inherit
type text_wrap = Wrap | No_wrap | Balance | Pretty | Inherit

type white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Inherit

type word_break = Normal | Break_all | Keep_all | Break_word | Inherit
type overflow_wrap = Normal | Break_word | Anywhere | Inherit
type hyphens = None | Manual | Auto | Inherit

(* List Types *)
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

type list_style_position = Inside | Outside | Inherit
type list_style_image = None | Url of string | Inherit

(* Table Types *)
type table_layout = Auto | Fixed | Inherit

type vertical_align =
  | Baseline
  | Top
  | Middle
  | Bottom
  | Text_top
  | Text_bottom
  | Sub
  | Super
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Inherit

(* Border Types *)
type border_collapse = Collapse | Separate | Inherit

(* Border shorthand type *)
type border_shorthand = {
  width : border_width option;
  style : border_style option;
  color : color option;
}

type border = Inherit | Initial | None | Shorthand of border_shorthand

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

(* Outline shorthand type *)
type outline_shorthand = {
  width : length option;
  style : outline_style option;
  color : color option;
}

type outline = Inherit | Initial | None | Shorthand of outline_shorthand

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
  (* Arbitrary font family name *)
  | Name of string
  (* CSS variables *)
  | Var of font_family var
  (* List of fonts for composition *)
  | List of font_family list

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

type font_display = Auto | Block | Swap | Fallback | Optional

type unicode_range =
  | Single of int  (** U+xxxx *)
  | Range of int * int  (** U+xxxx-yyyy *)

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

type font_feature_settings =
  | Normal
  | Feature_list of string
  | Inherit
  | String of string
  | Var of font_feature_settings var

type font_variation_settings =
  | Normal
  | Axis_list of string
  | Inherit
  | String of string
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
  | Scale of float * float option
  | Scale_x of float
  | Scale_y of float
  | Scale_z of float
  | Scale_3d of float * float * float
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
  | Var of transform var
  | List of transform list

type transforms = transform list
type transform_style = Flat | Preserve_3d | Inherit
type backface_visibility = Visible | Hidden | Inherit

type scale =
  | X of number_percentage
  | XY of number_percentage * number_percentage
  | XYZ of number_percentage * number_percentage * number_percentage
  | None
  | Var of scale var

type steps_direction =
  | Jump_start
  | Jump_end
  | Jump_none
  | Jump_both
  | Start
  | End

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
  | Var of timing_function var

type transition_property_value = All | None | Property of string
type transition_property = transition_property_value list

(* Transition behavior for discrete transitions (CSS Transitions Level 2) *)
type transition_behavior = Normal | Allow_discrete | Inherit

type transition_shorthand = {
  property : transition_property_value;
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
}

type transition =
  | Inherit
  | Initial
  | None
  | Var of transition var
  | Shorthand of transition_shorthand

type animation_direction = Normal | Reverse | Alternate | Alternate_reverse
type animation_fill_mode = None | Forwards | Backwards | Both
type animation_iteration_count = Num of float | Infinite
type animation_play_state = Running | Paused

type animation_shorthand = {
  name : string option; (* Optional animation name, defaults to None *)
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  iteration_count : animation_iteration_count option;
  direction : animation_direction option;
  fill_mode : animation_fill_mode option;
  play_state : animation_play_state option;
}

type animation =
  | Inherit
  | Initial
  | None (* Special case for "animation: none" *)
  | Var of animation var
  | Shorthand of animation_shorthand (* Requires a name *)

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
  | Var of blend_mode var

type shadow =
  | Shadow of {
      inset : bool;
      inset_var : string option;
          (** If set, outputs var(--<name>) before shadow values. Used by
              Tailwind's ring system for dynamic inset toggle. *)
      h_offset : length;
      v_offset : length;
      blur : length option;
      spread : length option;
      color : color option;
    }
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of shadow var
  | List of shadow list

type text_shadow =
  | None
  | Text_shadow of {
      h_offset : length;
      v_offset : length;
      blur : length option;
      color : color option;
    }
  | Inherit

type filter =
  | None
  | Blur of length
  | Brightness of number
  | Contrast of number
  | Drop_shadow of shadow
  | Grayscale of number
  | Hue_rotate of angle
  | Invert of number
  | Opacity of number
  | Saturate of number
  | Sepia of number
  | Url of string
  | List of filter list
  | Var of filter var

(* Background Types *)
type background_attachment = Scroll | Fixed | Local | Inherit
type background_box = Border_box | Padding_box | Content_box | Text | Inherit

type background_repeat =
  | Repeat
  | Space
  | Round
  | No_repeat
  | Repeat_x
  | Repeat_y
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

type background_size =
  | Auto
  | Cover
  | Contain
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Size of length * length
  | Inherit
  | Initial
  | Unset

type color_interpolation =
  | In_oklab
  | In_oklch
  | In_srgb
  | In_hsl
  | In_lab
  | In_lch

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
  | With_interpolation of gradient_direction * color_interpolation
  | Var of gradient_direction var

type gradient_stop =
  | Var of gradient_stop var
  | Color_percentage of
      color
      * percentage option
      * percentage option (* Color with optional percentage position *)
  | Color_length of
      color
      * length option
      * length option (* Color with optional length position *)
  | Length of length (* Interpolation hint with length, e.g., "50px" *)
  | List of
      gradient_stop list (* Multiple gradient stops - used for var fallbacks *)
  | Percentage of
      percentage (* Interpolation hint with percentage, e.g., "50%" *)
  | Direction of gradient_direction
(* Gradient direction for stops, e.g., "to right" or Var *)

type background_image =
  | Url of string
  | Linear_gradient of gradient_direction * gradient_stop list
  | Linear_gradient_var of gradient_stop var
      (** Linear gradient using a single variable for all stops including
          position. Outputs: linear-gradient(var(--tw-gradient-stops)) *)
  | Radial_gradient of gradient_stop list
  | None
  | Initial
  | Inherit

type position_value =
  | Center
  | Left_top
  | Left_center
  | Left_bottom
  | Right_top
  | Right_center
  | Right_bottom
  | Center_top
  | Center_bottom
  | XY of length * length
  | Inherit
  | Initial
  (* 3-value syntax: edge offset axis (e.g., "right 0.5rem center") *)
  | Edge_offset_axis of string * length * string
  (* 4-value syntax: edge1 offset1 edge2 offset2 *)
  | Edge_offset_edge_offset of string * length * string * length

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
  | Var of background var
  | Shorthand of background_shorthand

(* Gap shorthand type *)
type gap = { row_gap : length option; column_gap : length option }

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

type user_select = None | Auto | Text | All | Contain

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

type touch_action = Auto | None | Pan_x | Pan_y | Manipulation | Inherit
type resize = None | Both | Horizontal | Vertical | Block | Inline | Inherit

(* Box Model Types *)
type box_sizing = Border_box | Content_box | Inherit
type object_fit = Fill | Contain | Cover | None | Scale_down | Inherit

(* Content Types *)
type content =
  | String of string
  | None
  | Normal
  | Open_quote
  | Close_quote
  | Var of content var

type content_visibility =
  | Visible
  | Hidden
  | Auto
  | Inherit
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
type container_type = Size | Inline_size | Scroll_state | Normal

(* Container shorthand: name / type *)
type container_shorthand = {
  name : string option;
  ctype : container_type option;
}

(* Containment Types *)
type contain =
  | None
  | Strict
  | Content
  | Size
  | Layout
  | Style
  | Paint
  | List of contain list

type isolation = Auto | Isolate | Inherit

(* Scroll Types *)
type scroll_behavior = Auto | Smooth | Inherit
type scroll_snap_align = None | Start | End | Center
type scroll_snap_stop = Normal | Always | Inherit

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
  | Var of scroll_snap_type var

type overscroll_behavior = Auto | Contain | None | Inherit

(* SVG Types *)
(* SVG paint servers allow url(#id) with optional fallback (none/currentcolor/color). *)
type svg_paint =
  | None
  | Current_color
  | Color of color
  | Url of string * svg_paint option

(* Direction Types *)
type direction = Ltr | Rtl | Inherit

type unicode_bidi =
  | Normal
  | Embed
  | Isolate
  | Bidi_override
  | Isolate_override
  | Plaintext
  | Inherit

type writing_mode =
  | Horizontal_tb
  | Vertical_rl
  | Vertical_lr
  | Sideways_lr
  | Sideways_rl
  | Inherit

(* Webkit & Mozilla Specific Types *)
type webkit_appearance =
  | None
  | Auto
  | Button
  | Textfield
  | Menulist
  | Listbox
  | Checkbox
  | Radio
  | Push_button
  | Square_button
  | Inherit

type webkit_font_smoothing =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit

type moz_osx_font_smoothing = Auto | Grayscale | Inherit
type webkit_box_orient = Horizontal | Vertical | Inherit
type text_size_adjust = None | Auto | Pct of float | Inherit

(* Other Types *)
type forced_color_adjust = Auto | None | Inherit
type appearance = None | Auto | Button | Textfield | Menulist | Inherit
type print_color_adjust = Economy | Exact | Initial | Inherit | Unset
type clear = None | Left | Right | Both | Inline_start | Inline_end
type float_side = None | Left | Right | Inline_start | Inline_end | Inherit
type text_decoration_skip_ink = Auto | None | All | Inherit

type transform_origin =
  | Center
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
  | XYZ of length * length * length
  | Inherit

(* will-change property: which properties will animate *)
type will_change =
  | Will_change_auto
  | Scroll_position
  | Contents
  | Transform
  | Opacity
  | Properties of string list  (** Custom CSS property names *)

(* perspective-origin: origin point for 3D perspective *)
type perspective_origin =
  | Perspective_center
  | Perspective_top
  | Perspective_bottom
  | Perspective_left
  | Perspective_right
  | Perspective_top_left
  | Perspective_top_right
  | Perspective_bottom_left
  | Perspective_bottom_right
  | Perspective_xy of length * length

(* clip property (deprecated, but needed for sr-only) *)
type clip =
  | Clip_auto
  | Clip_rect of length * length * length * length
      (** top, right, bottom, left *)

(* clip-path property for clipping regions *)
type clip_path =
  | Clip_path_none
  | Clip_path_url of string
  | Clip_path_inset of length * length * length * length
  | Clip_path_circle of length  (** Circle with radius *)
  | Clip_path_ellipse of length * length  (** Ellipse with rx, ry *)
  | Clip_path_polygon of (length * length) list
  | Clip_path_path of string  (** SVG path data *)

(* Property type definition *)
type 'a property =
  | Background_color : color property
  | Color : color property
  | Border_color : color property
  | Border_style : border_style property
  | Border_top_style : border_style property
  | Border_right_style : border_style property
  | Border_bottom_style : border_style property
  | Border_left_style : border_style property
  | Padding : length list property
  | Padding_left : length property
  | Padding_right : length property
  | Padding_bottom : length property
  | Padding_top : length property
  | Padding_inline : length property
  | Padding_inline_start : length property
  | Padding_inline_end : length property
  | Padding_block : length property
  | Margin : length list property
  | Margin_inline_end : length property
  | Margin_inline_start : length property
  | Margin_left : length property
  | Margin_right : length property
  | Margin_top : length property
  | Margin_bottom : length property
  | Margin_inline : length property
  | Margin_block : length property
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
  | Font_size : length_percentage property
  | Line_height : line_height property
  | Font_weight : font_weight property
  | Font_style : font_style property
  | Text_align : text_align property
  | Text_decoration : text_decoration property
  | Text_decoration_style : text_decoration_style property
  | Text_decoration_color : color property
  | Text_underline_offset : length property
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
  | Justify_items : justify_items property
  | Justify_self : justify_self property
  | Align_content : align_content property
  | Align_self : align_self property
  | Place_content : place_content property
  | Place_items : place_items property
  | Place_self : (align_self * justify_self) property
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
  | Border_width : border_width property
  | Border_top_width : border_width property
  | Border_right_width : border_width property
  | Border_bottom_width : border_width property
  | Border_left_width : border_width property
  | Border_inline_start_width : border_width property
  | Border_inline_end_width : border_width property
  | Border_radius : length property
  | Border_top_left_radius : length property
  | Border_top_right_radius : length property
  | Border_bottom_left_radius : length property
  | Border_bottom_right_radius : length property
  | Border_top_color : color property
  | Border_right_color : color property
  | Border_bottom_color : color property
  | Border_left_color : color property
  | Border_inline_start_color : color property
  | Border_inline_end_color : color property
  | Opacity : float property
  | Mix_blend_mode : blend_mode property
  | Transform : transform list property
  | Translate : string property
  | Cursor : cursor property
  | Table_layout : table_layout property
  | Border_collapse : border_collapse property
  | Border_spacing : length property
  | User_select : user_select property
  | Pointer_events : pointer_events property
  | Overflow : overflow property
  | Inset : length property
  | Top : length property
  | Right : length property
  | Bottom : length property
  | Left : length property
  | Z_index : z_index property
  | Outline : outline property
  | Outline_style : outline_style property
  | Outline_width : length property
  | Outline_color : color property
  | Outline_offset : length property
  | Forced_color_adjust : forced_color_adjust property
  | Scroll_snap_type : scroll_snap_type property
  | White_space : white_space property
  | Border : border property
  | Background : background list property
  | Tab_size : int property
  | Webkit_text_size_adjust : text_size_adjust property
  | Font_feature_settings : font_feature_settings property
  | Font_variation_settings : font_variation_settings property
  | Webkit_tap_highlight_color : color property
  | Webkit_user_select : user_select property
  | Webkit_text_decoration : text_decoration property
  | Webkit_text_decoration_color : color property
  | Text_indent : length property
  | List_style : string property
  | Font : string property
  | Webkit_appearance : webkit_appearance property
  | Webkit_transform : transform list property
  | Webkit_transition : transition list property
  | Webkit_filter : filter property
  | Moz_appearance : appearance property
  | Ms_filter : filter property
  | O_transition : transition list property
  | Container_type : container_type property
  | Container_name : string property
  | Container : container_shorthand property
  | Perspective : length property
  | Perspective_origin : perspective_origin property
  | Transform_style : transform_style property
  | Backface_visibility : backface_visibility property
  | Object_position : position_value property
  | Rotate : angle property
  | Transition_duration : duration property
  | Transition_timing_function : timing_function property
  | Transition_delay : duration property
  | Transition_property : transition_property property
  | Transition_behavior : transition_behavior property
  | Will_change : will_change property
  | Contain : contain property
  | Isolation : isolation property
  | Word_spacing : length property
  | Background_attachment : background_attachment property
  | Border_top : string property
  | Border_right : string property
  | Border_bottom : string property
  | Border_left : string property
  | Transform_origin : transform_origin property
  | Text_shadow : text_shadow list property
  | Clip_path : clip_path property
  | Mask : string property
  | Content_visibility : content_visibility property
  | Filter : filter property
  | Background_image : background_image list property
  | Background_origin : background_box property
  | Animation : animation list property
  | Aspect_ratio : aspect_ratio property
  | Overflow_x : overflow property
  | Overflow_y : overflow property
  | Vertical_align : vertical_align property
  | Font_family : font_family property
  | Background_position : background_position property
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
  | Webkit_backdrop_filter : filter property
  | Scroll_snap_align : scroll_snap_align property
  | Scroll_snap_stop : scroll_snap_stop property
  | Scroll_behavior : scroll_behavior property
  | Box_sizing : box_sizing property
  | Resize : resize property
  | Object_fit : object_fit property
  | Appearance : appearance property
  | Print_color_adjust : print_color_adjust property
  | Content : content property
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
  | Background_blend_mode : blend_mode list property
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

type any_property = Prop : 'a property -> any_property
