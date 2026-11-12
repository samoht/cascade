(** List-marker, table, border and outline value types. *)

open Values
open Properties_layout_intf

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

(** CSS Inline 3 sec. 4.2 [vertical-align]: the keywords, or a
    [<length-percentage>] raising the box by that much. Every length unit is one
    of them, which is why this is not a list of the four the property is usually
    written with. *)
type vertical_align =
  | Baseline
  | Top
  | Middle
  | Bottom
  | Text_top
  | Text_bottom
  | Sub
  | Super
  | Length of length_percentage
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
