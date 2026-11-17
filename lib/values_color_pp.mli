(** Colour printers. *)

open Values_intf

val pp_system_color : system_color Pp.t
(** [pp_system_color] is the pretty-printer for [system_color]. *)

val pp_channel : channel Pp.t
(** [pp_channel] pretty-prints {!Values_intf.channel} values. *)

val pp_hue : hue Pp.t
(** [pp_hue] pretty-prints {!Values_intf.hue} values. *)

val pp_alpha : alpha Pp.t
(** [pp_alpha] pretty-prints {!Values_intf.alpha} values. *)

val color_numeric_channels : component list -> (float * float * float) option
(** [color_numeric_channels cs] is the three numeric [color()] channels [cs],
    percentages scaled to [0, 1]. *)

val color_channel_decimals :
  color_space -> (float * float * float) option -> int
(** [color_channel_decimals space channels] is the fewest decimals a [color()]
    channel in [space] needs within the Oklab budget. *)

val pp_component : component Pp.t
(** [pp_component] pretty-prints {!Values_intf.component} values. *)

val pp_hue_interpolation : hue_interpolation Pp.t
(** [pp_hue_interpolation] pretty-prints {!Values_intf.hue_interpolation}
    values. *)

val pp_rgb : rgb Pp.t
(** [pp_rgb] pretty-prints {!Values_intf.type-rgb} values. *)

val pp_color_space : color_space Pp.t
(** [pp_color_space] pretty-prints {!Values_intf.color_space} values. *)

val canonical_color_name : color_name -> color_name
(** [canonical_color_name n] is the [gray] spelling of a [grey] colour name, and
    [n] otherwise. *)

val pp_color_in_mix : color Pp.t
(** [pp_color_in_mix] pretty-prints color values for use inside color-mix
    functions, using lowercase [currentcolor]. *)

val pp_color : color Pp.t
(** [pp_color] pretty-prints {!Values_intf.color} values. *)

val pp_specified_color : color Pp.t
(** [pp_specified_color] pretty-prints {!Values_intf.color} values without
    crossing the computed-value color-space boundary. *)
