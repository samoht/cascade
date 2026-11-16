(** Font family, font shorthand and font variant value types. *)

open Values
open Properties_layout_intf
open Properties_text_intf

(* Font Types *)

(** CSS font-family values *)
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

(** CSS font-stretch values *)
type font_stretch =
  | Pct of float  (** Percentage values from 50% to 200% *)
  | Calc of font_stretch calc
      (** CSS Values 4 sec. 10.1 puts a math function where the [<percentage>]
          stands, and sec. 10.12 checks the [0,inf] range on what it resolves
          to. *)
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

(** CSS font-size-adjust metric keywords *)
type font_size_adjust_metric =
  | Ex_height
  | Cap_height
  | Ch_width
  | Ic_width
  | Ic_height

(** CSS font-size-adjust values *)
type font_size_adjust =
  | None
  | Number of float
  | Calc of font_size_adjust calc
      (** CSS Values 4 sec. 10.1 puts a math function where the [<number>]
          stands, and sec. 10.12 checks the [0,inf] range on what it resolves
          to, so a call the literal grammar refuses is still a declaration. *)
  | From_font
  | Metric_number of font_size_adjust_metric * float
  | Metric_from_font of font_size_adjust_metric
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_size_adjust var

(** CSS font-variant-emoji values *)
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

(** CSS font-variant-numeric token values *)
type font_variant_numeric_token =
  | Normal  (** Reset to normal font variant *)
  | Lining_nums
  | Oldstyle_nums
  | Proportional_nums
  | Tabular_nums
  | Diagonal_fractions
  | Stacked_fractions
  | Ordinal
  | Slashed_zero
  | Var of font_variant_numeric_token var

(** CSS font-variant-numeric values *)
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
(** The slots of the CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant}
     [font-variant]} shorthand. *)

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant}
     [font-variant]}. *)
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

(** The optional value paired with an OpenType feature tag. *)
type font_feature_value = On | Off | Index of int

type font_feature_setting = { tag : string; value : font_feature_value option }
(** One OpenType feature tag and its optional value. *)

(** CSS font-feature-settings values. *)
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
(** One OpenType variation axis and its numeric value. *)

(** CSS font-variation-settings values. *)
type font_variation_settings =
  | Normal
  | Axis_list of font_variation_setting list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variation_settings var
