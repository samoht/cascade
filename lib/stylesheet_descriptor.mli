(** Readers for the descriptor at-rules: [@font-face], [@counter-style], [@page]
    and its margin rules, [@font-palette-values], [@font-feature-values],
    [@view-transition], [@position-try] and [@viewport]. Each validates its
    descriptors against their grammar and keeps the last valid one of a name. *)

open Stylesheet_intf

val resolve_font_face_var :
  src:(Font_face.src -> Font_face.src) ->
  unicode_range:(Properties.unicode_range list -> Properties.unicode_range list) ->
  font_family:(Properties.font_family list -> Properties.font_family list) ->
  font_style:(Properties.font_style -> Properties.font_style) ->
  font_weight:(Properties.font_weight -> Properties.font_weight) ->
  font_stretch:(Properties.font_stretch -> Properties.font_stretch) ->
  font_display:(Properties.font_display -> Properties.font_display) ->
  font_variant:(font_variant_descriptor -> font_variant_descriptor) ->
  font_feature_settings:
    (Properties.font_feature_settings -> Properties.font_feature_settings) ->
  font_variation_settings:
    (Properties.font_variation_settings -> Properties.font_variation_settings) ->
  metric_override:(Font_face.metric_override -> Font_face.metric_override) ->
  size_adjust:(Font_face.size_adjust -> Font_face.size_adjust) ->
  font_face_descriptor ->
  font_face_descriptor option
(** [resolve_font_face_var] is {!Stylesheet.resolve_font_face_var}. *)

val read_font_variant_descriptor : Cursor.t -> font_variant_descriptor
(** [read_font_variant_descriptor r] reads the [font-variant] descriptor of an
    [@font-face], or a [var()] standing for one. *)

val read_font_face : Cursor.t -> statement
(** [read_font_face r] reads an [@font-face] rule. *)

val read_counter_style : Cursor.t -> statement
(** [read_counter_style r] reads an [@counter-style] rule. *)

val read_page : Cursor.t -> statement
(** [read_page r] reads an [@page] rule with its margin rules. *)

val read_font_palette_values : Cursor.t -> statement
(** [read_font_palette_values r] reads an [@font-palette-values] rule. *)

val read_font_feature_values : Cursor.t -> statement
(** [read_font_feature_values r] reads an [@font-feature-values] rule. *)

val read_view_transition : Cursor.t -> statement
(** [read_view_transition r] reads an [@view-transition] rule. *)

val read_position_try : Cursor.t -> statement
(** [read_position_try r] reads an [@position-try] rule. *)

val read_viewport : Cursor.t -> statement
(** [read_viewport r] reads an [@viewport] rule. *)

val read_ms_viewport : Cursor.t -> statement
(** [read_ms_viewport r] reads an [@-ms-viewport] rule. *)
