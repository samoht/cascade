(** Text, text decoration, text emphasis, font feature, ruby and white-space
    value types. *)

open Values

(** CSS font style values. *)
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

(** CSS text align values. *)
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

(** CSS text decoration values. *)
type text_decoration =
  | None
  | Shorthand of text_decoration_shorthand
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration var

(** CSS Text Decoration 4
    {{:https://drafts.csswg.org/css-text-decor-4/#propdef-text-decoration-skip}
     [text-decoration-skip]}: the shorthand over the four longhands below. *)
type text_decoration_skip =
  | None
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip var

(** Sec. 2.5.1 [text-decoration-skip-self]: whether the box's own decoration
    skips it. *)
type text_decoration_skip_self =
  | None
  | Objects
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_self var

(** Sec. 2.5.2 [text-decoration-skip-box]: whether an ancestor's decoration
    skips the box's edges. *)
type text_decoration_skip_box =
  | All
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_box var

(** Sec. 2.5.3 [text-decoration-skip-inset]: whether the decoration is inset
    from the glyph edges. *)
type text_decoration_skip_inset =
  | None
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_inset var

(** Sec. 2.5.4: one span of spaces a decoration skips. *)
type text_decoration_skip_space = All | Start | End

(** Sec. 2.5.4 [text-decoration-skip-spaces]. *)
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

(** One class of character the emphasis marks skip, for CSS Text Decoration 4
    {{:https://drafts.csswg.org/css-text-decor-4/#propdef-text-emphasis-skip}
     [text-emphasis-skip]}. *)
type text_emphasis_skip_keyword = Spaces | Punctuation | Symbols | Narrow

(** Sec. 4.3 [text-emphasis-skip]. *)
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

(** CSS text transform values. *)
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

(** One feature of CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant-alternates}
     [font-variant-alternates]}. *)
type font_variant_alternates_item =
  | Stylistic of string
  | Historical_forms
  | Styleset of string list
  | Character_variant of string list
  | Swash of string
  | Ornaments of string
  | Annotation of string

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant-alternates}
     [font-variant-alternates]}. *)
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

(** CSS text-overflow values *)
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

(** CSS text-wrap values *)
type text_wrap =
  | Wrap
  | No_wrap
  | Auto
  | Balance
  | Stable
  | Pretty
  | Mode_style of [ `Wrap | `No_wrap ] * [ `Auto | `Balance | `Stable | `Pretty ]
      (** both components, printed mode-first *)
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

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-text-box} [text-box]}:
    [normal | <'text-box-trim'> || <'text-box-edge'>]. *)
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
  | Calc of initial_letter calc * int option
      (** A math function in the size slot. CSS Values 4 sec. 10.12 checks the
          [1,inf] range on what it resolves to, so a call the literal grammar
          refuses is still a declaration. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter var

(** One alignment point of CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-initial-letter-align}
     [initial-letter-align]}. *)
type initial_letter_align_keyword =
  | Alphabetic
  | Ideographic
  | Hanging
  | Leading
  | Border_box

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-initial-letter-align}
     [initial-letter-align]}. *)
type initial_letter_align =
  | Align of initial_letter_align_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter_align var

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-initial-letter-wrap}
     [initial-letter-wrap]}. *)
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

(* CSS Ruby 1 sec. 5.1: [auto | spaces]. [None] is the older spelling Chrome and
   Firefox still read, and serialise as [spaces]. *)
type ruby_overhang =
  | Auto
  | Spaces
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

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-dominant-baseline}
     [dominant-baseline]}: [auto | <baseline-metric>]. *)
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

(** CSS white-space values *)
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

(** CSS Text 4
    {{:https://drafts.csswg.org/css-text-4/#propdef-white-space-collapse}
     [white-space-collapse]}: how white space and line breaks collapse. *)
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

(** CSS word-break values *)
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

(** CSS overflow-wrap values *)
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

(** CSS hyphens values *)
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
