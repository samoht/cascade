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
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Calc of line_height calc
  | Var of line_height var

type font_weight =
  | Weight of int
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
  | Webkit_box
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Multi of display * display
      (** Two-value [<display-outside> <display-inside>] syntax per CSS Display
          3 §2.1, e.g. [inline flow-root] or [list-item flow-root]. *)
  | Var of display var

type position =
  | Static
  | Relative
  | Absolute
  | Fixed
  | Sticky
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

type z_index =
  | Auto
  | Index of int
  | Calc of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of z_index var

type opacity =
  | Opacity_number of float
  | Abs of opacity  (** [abs(<opacity>)] *)
  | Sign of opacity  (** [sign(<opacity>)] *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of opacity var

type tab_size =
  | Int of int
  | Length of length
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of tab_size var

type order =
  | Int of int
  | Calc of string
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

type flex_wrap =
  | Nowrap
  | Wrap
  | Wrap_reverse
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_wrap var

type flex_factor =
  | Number of float
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_factor var

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
  | Calc of flex_basis calc
  | Var of flex_basis var

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
  | Grow of float (* Single grow value *)
  | Basis of flex_basis (* 1 1 <flex-basis> *)
  | Grow_shrink of float * float (* grow shrink 0% *)
  | Full of float * float * flex_basis (* grow shrink basis *)
  | Var of flex var  (** Font-size values including relative keywords *)

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
type grid_auto_flow =
  | Row
  | Column
  | Dense
  | Row_dense
  | Column_dense
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of grid_auto_flow var

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
  | Split of grid_template * grid_template
  | Named_tracks of (string option * grid_template) list
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
  | Span of int
  | Span_name of string
  | Span_num_name of int * string
  | Calc of string
  | Var of grid_line var

type aspect_ratio =
  | Auto
  | Auto_ratio of float * float
  | Ratio of float * float
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

type text_transform =
  | None
  | Capitalize
  | Uppercase
  | Lowercase
  | Full_width
  | Full_size_kana
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_transform var

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

type text_wrap =
  | Wrap
  | No_wrap
  | Balance
  | Pretty
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_wrap var

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

type hyphenate_limit_chars =
  | Auto
  | One of int
  | Two of int * int
  | Three of int * int * int
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

type white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Preserve_nowrap
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of white_space var

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
  | Var of overflow_wrap var

type hyphens = None | Manual | Auto | Inherit | Var of hyphens var

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
  | String of string
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

type list_style_image =
  | None
  | Url of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style_image var

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
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Inherit
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
  width : length option;
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
  (* Custom font family name *)
  | Name of string
  (* CSS variables *)
  (* List of fonts for composition *)
  | List of font_family list
  | Var of font_family var

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

type font_feature_settings =
  | Normal
  | Feature_list of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | String of string
  | Var of font_feature_settings var

type font_variation_settings =
  | Normal
  | Axis_list of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
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
  | Rotate_axis of float * float * float * angle
  | Scale of float * float option
  | Scale_space of float * float
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
  | Var of animation_direction var

type animation_fill_mode =
  | None
  | Forwards
  | Backwards
  | Both
  | Var of animation_fill_mode var

type animation_iteration_count =
  | Num of float
  | Infinite
  | Var of animation_iteration_count var

type animation_play_state = Running | Paused | Var of animation_play_state var

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
  | Shadow of {
      inset : bool;
      inset_var : string option;
          (** If set, outputs var(--<name>) before shadow values. Used by
              Tailwind's ring system for dynamic inset toggle. *)
      inset_var_no_fallback : bool;
          (** When true, emits [var(--name)] without fallback instead of
              [var(--name,)] with empty fallback. Used by the forms plugin where
              the variable is always set in the same rule. *)
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
  | List of shadow list
  | Var of shadow var

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

type filter =
  | None
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
  | Revert
  | Revert_layer
  | Var of background_size var

type color_interpolation =
  | In_oklab
  | In_oklch
  | In_srgb
  | In_hsl
  | In_lab
  | In_lch
  | Var of color_interpolation var

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
  | Color_percentage of
      color * length_percentage option * length_percentage option
    (* Color with optional length-percentage position *)
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
  | Var of gradient_stop var
(* Gradient direction for stops, e.g., "to right" or Var *)

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
  (* 4-value syntax: edge1 offset1 edge2 offset2 *)
  | Edge_offset_edge_offset of
      string * length_percentage * string * length_percentage
  | Var of position_value var

type radial_gradient_config = {
  shape : radial_shape option;
  size : radial_size option;
  position : position_value option;
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
  | Var of border_radius var  (** Per CSS Backgrounds and Borders 3 §5. *)

type conic_gradient_config = {
  from_angle : angle option;  (** [from <angle>] starting angle *)
  conic_position : position_value option;  (** [at <position>] center *)
}

type background_image =
  | Url of string
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
  | Image_set of image_set_option list
      (** [image-set(<source>#)] CSS Images 4 *)
  | Cross_fade of cross_fade_option list
      (** [cross-fade(<cf-mixing-image>#)] CSS Images 4 *)
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
  image_set_source : image_set_source;
  image_set_resolution : string option;
      (** [<resolution>] like ["1x"] or ["300dpi"] *)
  image_set_mime_type : string option;  (** [type("image/avif")] *)
}

and image_set_source = Image_set_url of string | Image_set_string of string

and cross_fade_option = {
  cross_fade_image : background_image;
  cross_fade_percent : percentage option;
}

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
(* Mask Types *)

(** Webkit-prefixed mask-composite values *)
type webkit_mask_composite =
  | Source_over
  | Xor
  | Source_in
  | Source_out
  | Inherit
  | Var of webkit_mask_composite var
      (** Standard mask-composite values (different from webkit) *)

type mask_composite =
  | Add
  | Subtract
  | Intersect
  | Exclude
  | Inherit
  | Var of mask_composite var  (** Webkit mask-source-type values *)

type webkit_mask_source_type =
  | Alpha
  | Luminance
  | Auto
  | Inherit
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

type mask_type = Alpha | Luminance | Inherit | Var of mask_type var

type mask_box =
  | Border_box
  | Content_box
  | Fill_box
  | Padding_box
  | Stroke_box
  | View_box
  | No_clip  (** Only valid for mask-clip *)
  | Inherit
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
  | Inherit
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

type field_sizing = Content | Fixed | Inherit | Var of field_sizing var
type caption_side = Top | Bottom | Inherit | Var of caption_side var

type object_fit =
  | Fill
  | Contain
  | Cover
  | None
  | Scale_down
  | Inherit
  | Var of object_fit var

(* Content Types *)
type content =
  | String of string
  | None
  | Normal
  | Open_quote
  | Close_quote
  | Attr of string
  | Counter of string
  | Counters of string * string
  | Content_list of content list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
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

type position_anchor =
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

type position_try_fallbacks =
  | None
  | Fallbacks of position_try_fallback list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try_fallbacks var

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

type animation_timeline =
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
  | Named of animation_range_name * length_percentage

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

type image_orientation =
  | None
  | From_image
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_orientation var

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

type margin_trim_edge = Block_start | Inline_start | Block_end | Inline_end

type margin_trim =
  | None
  | Block
  | Inline
  | Edges of margin_trim_edge list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of margin_trim var

type ray_size = Radial of radial_size | Sides

type ray = {
  angle : angle;
  size : ray_size option;
  contain : bool;
  position : position_value option;
}

type offset_path =
  | None
  | Url of string
  | Path of string
  | Ray of ray
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_path var

(* Container shorthand: name / type *)
type container_shorthand =
  | Shorthand of { name : string option; ctype : container_type option }
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

(* CSS Fragmentation 3 §6 deprecated [page-break-before / -after / -inside]
   aliases. The shorter value vocabulary makes them their own type rather than
   overload [break_value]. *)
type page_break_value =
  | Auto
  | Always (* maps to [break-before/after: page] *)
  | Avoid
  | Left
  | Right
  | Inherit
  | Var of page_break_value var

type page_break_inside_value =
  | Auto
  | Avoid
  | Inherit
  | Var of page_break_inside_value var

(* CSS Paged Media 3 §6.1 [size] descriptor: optional page size keyword (paper
   sheet name), explicit dimensions, [auto], or a page size combined with an
   orientation. *)
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
  | Inherit
  | Var of page_size var

(* Multi-column Layout Types *)
type columns_value =
  | Auto
  | Count of int
  | Width of length
  | Both of length * int
      (** [<column-width> <column-count>] per CSS Multicol 2 §6.1. The two
          components can appear in either order in the source; we canonicalise
          to [<width>, <count>] internally so the printer always emits the width
          first. *)
  | Inherit
  | Var of columns_value var

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

type timeline_shorthand = {
  timeline_name : string;
  timeline_axis : timeline_axis;
}

type overscroll_behavior =
  | Auto
  | Contain
  | None
  | Inherit
  | Var of overscroll_behavior var

(* SVG Types *)
(* SVG paint servers allow url(#id) with optional fallback (none/currentcolor/color). *)
type svg_paint =
  | None
  | Inherit
  | Current_color
  | Color of color
  | Url of string * svg_paint option
  | Var of svg_paint var

(* Direction Types *)
type direction = Ltr | Rtl | Inherit | Var of direction var

type unicode_bidi =
  | Normal
  | Embed
  | Isolate
  | Bidi_override
  | Isolate_override
  | Plaintext
  | Inherit
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
  | Var of webkit_font_smoothing var

type moz_osx_font_smoothing =
  | Auto
  | Grayscale
  | Inherit
  | Var of moz_osx_font_smoothing var

type webkit_box_orient =
  | Horizontal
  | Vertical
  | Inherit
  | Var of webkit_box_orient var

type moz_orient =
  | Inline
  | Block
  | Horizontal
  | Vertical
  | Inherit
  | Var of moz_orient var

type webkit_line_clamp = Lines of int | Unset | Var of webkit_line_clamp var

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
  | Var of print_color_adjust var

type box_decoration_break = Clone | Slice | Var of box_decoration_break var

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
  | Var of text_decoration_skip_ink var

type transform_origin =
  | Center
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

(* clip-path property for clipping regions *)
type clip_path =
  | Clip_path_none
  | Clip_path_url of string
  | Clip_path_inset of length * length option * length option * length option
      (** inset(top, right?, bottom?, left?) - supports 1-4 values *)
  | Clip_path_circle of length  (** Circle with radius *)
  | Clip_path_ellipse of length * length  (** Ellipse with rx, ry *)
  | Clip_path_polygon of (length * length) list
  | Clip_path_polygon_spaced of (length * length) list
  | Clip_path_path of string  (** SVG path data *)
  | Clip_path_shape of string
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

type _ kind =
  | Length : length kind
  | Color : color kind
  | Rgb : rgb kind
  | Int : int kind
  | Float : float kind
  | Percentage : percentage kind
  | Length_percentage : length_percentage kind
  | Number_percentage : number_percentage kind
  | Value : Component.t list kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Outline_style : outline_style kind
  | Border : border kind
  | Font_weight : font_weight kind
  | Line_height : line_height kind
  | Font_family : font_family kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Font_variant_numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Shadow : shadow kind
  | Box_shadow : shadow kind
  | Content : content kind
  | Gradient_stop : gradient_stop kind
  | Gradient_direction : gradient_direction kind
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
      kind : 'a kind;
      value : 'a;
      layer : string option;
      meta : meta option;
    }
      -> custom_property

(** [all] shorthand value (CSS Cascade 5 §3.2). The [all] property only accepts
    CSS-wide keywords - no other syntax is valid. *)
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
  | All : css_wide property
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
  | Flex_grow : flex_factor property
  | Flex_shrink : flex_factor property
  | Flex_basis : length property
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
  | Grid_area : string property
  | Grid_auto_flow : grid_auto_flow property
  | Grid_auto_columns : grid_template property
  | Grid_auto_rows : grid_template property
  | Grid_column : (grid_line * grid_line) property
  | Grid_row : (grid_line * grid_line) property
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
  | Border_image : string property
  | Border_radius : border_radius property
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
  | Border_inline_style : border_style property
  | Border_block_style : border_style property
  | Border_start_start_radius : length property
  | Border_start_end_radius : length property
  | Border_end_start_radius : length property
  | Border_end_end_radius : length property
  | Opacity : opacity property
  | Mix_blend_mode : blend_mode property
  | Transform : transform list property
  | Translate : translate_value property
  | Cursor : cursor property
  | Table_layout : table_layout property
  | Border_collapse : border_collapse property
  | Border_spacing : length list property
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
  | Outline_width : length property
  | Outline_color : color property
  | Outline_offset : length property
  | Forced_color_adjust : forced_color_adjust property
  | Scroll_snap_type : scroll_snap_type property
  | White_space : white_space property
  | Border : border property
  | Background : background list property
  | Tab_size : tab_size property
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
  | Source : Font_face.src property
  | Webkit_appearance : webkit_appearance property
  | Webkit_transform : transform list property
  | Webkit_transition : transition list property
  | Webkit_filter : filter property
  | Moz_appearance : appearance property
  | Ms_filter : filter property
  | O_transition : transition list property
  | Container_type : container_type property
  | Container_name : container_name property
  | Container : container_shorthand property
  | Anchor_name : anchor_name property
  | Position_anchor : position_anchor property
  | Position_try_fallbacks : position_try_fallbacks property
  | Shape_outside : string property
  | Shape_margin : length_percentage property
  | Overflow_clip_margin : length property
  | Overflow_anchor : overflow_anchor property
  | Scrollbar_width : scrollbar_width property
  | Scrollbar_color : scrollbar_color property
  | Scrollbar_gutter : scrollbar_gutter property
  | Line_height_step : length property
  | Font_palette : font_palette property
  | Font_synthesis : font_synthesis property
  | Text_wrap_style : text_wrap_style property
  | Text_box_trim : text_box_trim property
  | Animation_timeline : animation_timeline property
  | Animation_range : animation_range property
  | Scroll_timeline : timeline_shorthand property
  | View_transition_name : view_transition_name property
  | Image_orientation : image_orientation property
  | Contain_intrinsic_size : contain_intrinsic_size property
  | Contain_intrinsic_width : string property
  | Contain_intrinsic_height : string property
  | Contain_intrinsic_block_size : string property
  | Contain_intrinsic_inline_size : string property
  | Margin_trim : margin_trim property
  | Offset_path : offset_path property
  | Offset_distance : length_percentage property
  | Font_size_adjust : font_size_adjust property
  | Font_variant_emoji : font_variant_emoji property
  | Text_spacing_trim : text_spacing_trim property
  | Hyphenate_limit_chars : hyphenate_limit_chars property
  | Initial_letter : initial_letter property
  | View_timeline_name : timeline_name property
  | View_timeline_axis : timeline_axis property
  | View_timeline : timeline_shorthand property
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
  | Will_change : will_change property
  | Contain : contain property
  | Isolation : isolation property
  | Break_before : break_value property
  | Break_after : break_value property
  | Break_inside : break_inside_value property
  | Page_size : page_size property
  | Columns : columns_value property
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
  | Hyphens : hyphens property
  | Webkit_hyphens : hyphens property
  | Font_stretch : font_stretch property
  | Font_variant_numeric : font_variant_numeric property
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
  | Appearance : appearance property
  | Color_scheme : color_scheme property
  | Print_color_adjust : print_color_adjust property
  | Box_decoration_break : box_decoration_break property
  | Webkit_box_decoration_break : box_decoration_break property
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
  | Scroll_margin : length list property
  | Scroll_margin_top : length property
  | Scroll_margin_right : length property
  | Scroll_margin_bottom : length property
  | Scroll_margin_left : length property
  | Scroll_margin_inline : length property
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
  | Scroll_padding_inline : length property
  | Scroll_padding_inline_start : length property
  | Scroll_padding_inline_end : length property
  | Scroll_padding_block : length property
  | Scroll_padding_block_start : length property
  | Scroll_padding_block_end : length property
  | Overscroll_behavior : overscroll_behavior list property
  | Overscroll_behavior_x : overscroll_behavior property
  | Overscroll_behavior_y : overscroll_behavior property
  | Overscroll_behavior_block : overscroll_behavior property
  | Overscroll_behavior_inline : overscroll_behavior property
  | Accent_color : color property
  | Caret_color : color property

type any_property = Prop : 'a property -> any_property
