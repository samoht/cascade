(** The font-variant family and the OpenType controls next to it: the
    [font-variant] shorthand and its longhands, font-feature-settings,
    font-variation-settings, font-language-override, font-kerning,
    font-optical-sizing, font-palette and font-synthesis. *)

val pp_font_optical_sizing : Properties_intf.font_optical_sizing Pp.t
(** [pp_font_optical_sizing] prints a [font-optical-sizing] value. *)

val read_font_optical_sizing : Cursor.t -> Properties_intf.font_optical_sizing
(** [read_font_optical_sizing t] reads a [font-optical-sizing] value from [t].
*)

val pp_font_kerning : Properties_intf.font_kerning Pp.t
(** [pp_font_kerning] prints a [font-kerning] value. *)

val read_font_kerning : Cursor.t -> Properties_intf.font_kerning
(** [read_font_kerning t] reads a [font-kerning] value from [t]. *)

val pp_font_language_override : Properties_intf.font_language_override Pp.t
(** [pp_font_language_override] prints a [font-language-override] value. *)

val read_font_language_override :
  Cursor.t -> Properties_intf.font_language_override
(** [read_font_language_override t] reads a [font-language-override] value from
    [t]. *)

val pp_font_synthesis_style : Properties_intf.font_synthesis_style Pp.t
(** [pp_font_synthesis_style] prints a [font-synthesis-style] value. *)

val read_font_synthesis_style : Cursor.t -> Properties_intf.font_synthesis_style
(** [read_font_synthesis_style t] reads a [font-synthesis-style] value from [t].
*)

val pp_font_synthesis_weight : Properties_intf.font_synthesis_weight Pp.t
(** [pp_font_synthesis_weight] prints a [font-synthesis-weight] value. *)

val read_font_synthesis_weight :
  Cursor.t -> Properties_intf.font_synthesis_weight
(** [read_font_synthesis_weight t] reads a [font-synthesis-weight] value from
    [t]. *)

val pp_font_synthesis_small_caps :
  Properties_intf.font_synthesis_small_caps Pp.t
(** [pp_font_synthesis_small_caps] prints a [font-synthesis-small-caps] value.
*)

val read_font_synthesis_small_caps :
  Cursor.t -> Properties_intf.font_synthesis_small_caps
(** [read_font_synthesis_small_caps t] reads a [font-synthesis-small-caps] value
    from [t]. *)

val pp_font_synthesis_position : Properties_intf.font_synthesis_position Pp.t
(** [pp_font_synthesis_position] prints a [font-synthesis-position] value. *)

val read_font_synthesis_position :
  Cursor.t -> Properties_intf.font_synthesis_position
(** [read_font_synthesis_position t] reads a [font-synthesis-position] value
    from [t]. *)

val pp_font_variant_ligature :
  Pp.ctx -> Properties_intf.font_variant_ligature -> unit
(** [pp_font_variant_ligature ctx v] prints the ligature keyword [v]. *)

val pp_font_variant_ligatures : Properties_intf.font_variant_ligatures Pp.t
(** [pp_font_variant_ligatures] prints a [font-variant-ligatures] value. *)

val read_font_variant_ligature :
  Cursor.t -> Properties_intf.font_variant_ligature
(** [read_font_variant_ligature t] reads a [font-variant-ligature] value from
    [t]. *)

val read_font_variant_ligatures :
  Cursor.t -> Properties_intf.font_variant_ligatures
(** [read_font_variant_ligatures t] reads a [font-variant-ligatures] value from
    [t]. *)

val pp_font_variant_caps : Properties_intf.font_variant_caps Pp.t
(** [pp_font_variant_caps] prints a [font-variant-caps] value. *)

val pp_font_variant_alternates : Properties_intf.font_variant_alternates Pp.t
(** [pp_font_variant_alternates] prints a [font-variant-alternates] value. *)

val read_font_variant_alternates :
  Cursor.t -> Properties_intf.font_variant_alternates
(** [read_font_variant_alternates t] reads a [font-variant-alternates] value
    from [t]. *)

val read_font_variant_caps : Cursor.t -> Properties_intf.font_variant_caps
(** [read_font_variant_caps t] reads a [font-variant-caps] value from [t]. *)

val pp_font_variant_position : Properties_intf.font_variant_position Pp.t
(** [pp_font_variant_position] prints a [font-variant-position] value. *)

val read_font_variant_position :
  Cursor.t -> Properties_intf.font_variant_position
(** [read_font_variant_position t] reads a [font-variant-position] value from
    [t]. *)

val pp_east_asian_feature : Pp.ctx -> Properties_intf.east_asian_feature -> unit
(** [pp_east_asian_feature ctx v] prints the east-asian keyword [v]. *)

val pp_font_variant_east_asian : Properties_intf.font_variant_east_asian Pp.t
(** [pp_font_variant_east_asian] prints a [font-variant-east-asian] value. *)

val read_east_asian_feature : Cursor.t -> Properties_intf.east_asian_feature
(** [read_east_asian_feature t] reads a [east-asian-feature] value from [t]. *)

val read_font_variant_east_asian :
  Cursor.t -> Properties_intf.font_variant_east_asian
(** [read_font_variant_east_asian t] reads a [font-variant-east-asian] value
    from [t]. *)

val pp_font_feature_value : Pp.ctx -> Properties_intf.font_feature_value -> unit
(** [pp_font_feature_value ctx v] prints the value of one feature setting. *)

val pp_font_feature_setting :
  Pp.ctx -> Properties_intf.font_feature_setting -> unit
(** [pp_font_feature_setting ctx v] prints one tag-value feature setting. *)

val pp_font_feature_settings : Properties_intf.font_feature_settings Pp.t
(** [pp_font_feature_settings] prints a [font-feature-settings] value. *)

val pp_font_variation_setting :
  Pp.ctx -> Properties_intf.font_variation_setting -> unit
(** [pp_font_variation_setting ctx v] prints one axis-value variation setting.
*)

val pp_font_variation_settings : Properties_intf.font_variation_settings Pp.t
(** [pp_font_variation_settings] prints a [font-variation-settings] value. *)

val pp_font_palette : Properties_intf.font_palette Pp.t
(** [pp_font_palette] prints a [font-palette] value. *)

val pp_font_synthesis_feature : Properties_intf.font_synthesis_feature Pp.t
(** [pp_font_synthesis_feature] prints a [font-synthesis-feature] value. *)

val pp_font_synthesis : Properties_intf.font_synthesis Pp.t
(** [pp_font_synthesis] prints a [font-synthesis] value. *)

val pp_font_variant_emoji : Properties_intf.font_variant_emoji Pp.t
(** [pp_font_variant_emoji] prints a [font-variant-emoji] value. *)

val pp_font_variant_numeric_token :
  Properties_intf.font_variant_numeric_token Pp.t
(** [pp_font_variant_numeric_token] prints a [font-variant-numeric-token] value.
*)

val pp_font_variant_numeric : Properties_intf.font_variant_numeric Pp.t
(** [pp_font_variant_numeric] prints a [font-variant-numeric] value. *)

val read_font_palette : Cursor.t -> Properties_intf.font_palette
(** [read_font_palette t] reads a [font-palette] value from [t]. *)

val read_font_synthesis_feature :
  Cursor.t -> Properties_intf.font_synthesis_feature
(** [read_font_synthesis_feature t] reads a [font-synthesis-feature] value from
    [t]. *)

val read_font_synthesis : Cursor.t -> Properties_intf.font_synthesis
(** [read_font_synthesis t] reads a [font-synthesis] value from [t]. *)

val read_font_variant_numeric_token :
  Cursor.t -> Properties_intf.font_variant_numeric_token
(** [read_font_variant_numeric_token t] reads a [font-variant-numeric-token]
    value from [t]. *)

val read_font_variant_numeric : Cursor.t -> Properties_intf.font_variant_numeric
(** [read_font_variant_numeric t] reads a [font-variant-numeric] value from [t].
*)

val read_font_variant : Cursor.t -> Properties_intf.font_variant
(** [read_font_variant t] reads a [font-variant] value from [t]. *)

val pp_font_variant : Properties_intf.font_variant Pp.t
(** [pp_font_variant] prints a [font-variant] value. *)

val read_font_feature_value : Cursor.t -> Properties_intf.font_feature_value
(** [read_font_feature_value t] reads a [font-feature-value] value from [t]. *)

val read_font_feature_setting : Cursor.t -> Properties_intf.font_feature_setting
(** [read_font_feature_setting t] reads a [font-feature-setting] value from [t].
*)

val read_font_feature_settings :
  Cursor.t -> Properties_intf.font_feature_settings
(** [read_font_feature_settings t] reads a [font-feature-settings] value from
    [t]. *)

val read_font_variation_setting :
  Cursor.t -> Properties_intf.font_variation_setting
(** [read_font_variation_setting t] reads a [font-variation-setting] value from
    [t]. *)

val read_font_variation_settings :
  Cursor.t -> Properties_intf.font_variation_settings
(** [read_font_variation_settings t] reads a [font-variation-settings] value
    from [t]. *)

val read_font_variant_emoji : Cursor.t -> Properties_intf.font_variant_emoji
(** [read_font_variant_emoji t] reads a [font-variant-emoji] value from [t]. *)
