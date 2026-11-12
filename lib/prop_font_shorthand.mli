(** The [font] shorthand and the longhands its body spells: font-style,
    font-weight, font-stretch, font-size, line-height and font-size-adjust. *)

val read_font_weight : Cursor.t -> Properties_intf.font_weight
(** [read_font_weight t] reads a [font-weight] value from [t]. *)

val read_font_style : Cursor.t -> Properties_intf.font_style
(** [read_font_style t] reads a [font-style] value from [t]. *)

val read_font_size : Cursor.t -> Properties_intf.font_size
(** [read_font_size t] reads a [font-size] value from [t]. *)

val normalize_font_style :
  Properties_intf.font_style -> Properties_intf.font_style
(** [normalize_font_style v] is [v] in canonical form. *)

val pp_font_style : Properties_intf.font_style Pp.t
(** [pp_font_style] prints a [font-style] value. *)

val pp_font_stretch : Properties_intf.font_stretch Pp.t
(** [pp_font_stretch] prints a [font-stretch] value. *)

val pp_font_size_adjust_metric : Properties_intf.font_size_adjust_metric Pp.t
(** [pp_font_size_adjust_metric] prints the font metric a [font-size-adjust]
    value names. *)

val pp_font_size_adjust : Properties_intf.font_size_adjust Pp.t
(** [pp_font_size_adjust] prints a [font-size-adjust] value. *)

val pp_font_size : Properties_intf.font_size Pp.t
(** [pp_font_size] prints a [font-size] value. *)

val pp_line_height : Properties_intf.line_height Pp.t
(** [pp_line_height] prints a [line-height] value. *)

val pp_font_weight : Properties_intf.font_weight Pp.t
(** [pp_font_weight] prints a [font-weight] value. *)

val pp_font_variant_css21 : Pp.ctx -> Properties_intf.font_variant_css21 -> unit
(** [pp_font_variant_css21 ctx v] prints the CSS 2.1 font-variant keyword [v].
*)

val read_font_variant_css21 : Cursor.t -> Properties_intf.font_variant_css21
(** [read_font_variant_css21 t] reads the CSS 2.1 font-variant keyword the
    [font] shorthand accepts. *)

val font_width_css3 :
  Properties_intf.font_stretch -> Properties_intf.font_stretch option
(** [font_width_css3 v] is the width keyword [v] stands for, or [None] when no
    keyword does. *)

val pp_font_shorthand : Properties_intf.font_shorthand Pp.t
(** [pp_font_shorthand] prints the body of a [font] shorthand. *)

val pp_font : Properties_intf.font Pp.t
(** [pp_font] prints a [font] value. *)

val read_line_height : Cursor.t -> Properties_intf.line_height
(** [read_line_height t] reads a [line-height] value from [t]. *)

val read_font_shorthand : Cursor.t -> Properties_intf.font_shorthand
(** [read_font_shorthand t] reads the body of a [font] shorthand from [t]. *)

val read_font : Cursor.t -> Properties_intf.font
(** [read_font t] reads a [font] value from [t]. *)

val read_font_stretch : Cursor.t -> Properties_intf.font_stretch
(** [read_font_stretch t] reads a [font-stretch] value from [t]. *)

val normalize_font_size : Properties_intf.font_size -> Properties_intf.font_size
(** [normalize_font_size v] is [v] in canonical form. *)

val normalize_line_height :
  ?lossless:bool -> Properties_intf.line_height -> Properties_intf.line_height
(** [normalize_line_height ?lossless v] is [v] in canonical form. *)

val normalize_font_weight :
  Properties_intf.font_weight -> Properties_intf.font_weight
(** [normalize_font_weight v] is [v] in canonical form. *)

val normalize_font_size_adjust :
  Properties_intf.font_size_adjust -> Properties_intf.font_size_adjust
(** [normalize_font_size_adjust v] is [v] in canonical form. *)

val normalize_font_stretch :
  Properties_intf.font_stretch -> Properties_intf.font_stretch
(** [normalize_font_stretch v] is [v] in canonical form. *)

val normalize_font : Properties_intf.font -> Properties_intf.font
(** [normalize_font v] is [v] in canonical form. *)

val read_font_size_adjust_metric :
  Cursor.t -> Properties_intf.font_size_adjust_metric
(** [read_font_size_adjust_metric t] reads the font metric keyword of a
    [font-size-adjust] value. *)

val read_font_size_adjust : Cursor.t -> Properties_intf.font_size_adjust
(** [read_font_size_adjust t] reads a [font-size-adjust] value from [t]. *)
