(** CSS Images 4 gradients: linear, radial and conic gradients, their repeating
    and vendor-prefixed spellings, colour stops and colour interpolation. *)

open Properties_intf

val pp_hue_interpolation_method : Pp.ctx -> hue_interpolation_method -> unit
(** [pp_hue_interpolation_method] prints a [<hue-interpolation-method>] with its
    trailing [hue]. *)

val pp_color_interpolation : color_interpolation Pp.t
(** [pp_color_interpolation] prints a [color_interpolation]. *)

val read_hue_interpolation_method : Cursor.t -> hue_interpolation_method option
(** [read_hue_interpolation_method t] reads
    [shorter|longer|increasing|decreasing hue] from [t]; [None], with [t]
    restored, when absent. *)

val read_color_interpolation : Cursor.t -> color_interpolation
(** [read_color_interpolation t] reads a [color_interpolation] from [t]. *)

val pp_gradient_direction : gradient_direction Pp.t
(** [pp_gradient_direction] prints a [gradient_direction]. *)

val pp_radial_shape : radial_shape Pp.t
(** [pp_radial_shape] prints a [radial_shape]. *)

val pp_radial_size : radial_size Pp.t
(** [pp_radial_size] prints a [radial_size]. *)

val pp_radial_gradient_config : radial_gradient_config Pp.t
(** [pp_radial_gradient_config] prints a [radial_gradient_config]. *)

val pp_conic_gradient_config : conic_gradient_config Pp.t
(** [pp_conic_gradient_config] prints a [conic_gradient_config]. *)

val pp_gradient_position : gradient_position Pp.t
(** [pp_gradient_position] prints a [gradient_position]. *)

val pp_gradient_stop : gradient_stop Pp.t
(** [pp_gradient_stop] prints a [gradient_stop]. *)

val pp_webkit_gradient : Webkit_gradient.t Pp.t
(** [pp_webkit_gradient] prints a legacy [-webkit-gradient()]. *)

val pp_conic_gradient_named :
  string -> Pp.ctx -> conic_gradient_config * gradient_stop list -> unit
(** [pp_conic_gradient_named name ctx (config, stops)] prints a conic gradient
    as the function [name], eliding a default [config]. *)

val pp_linear_gradient_named :
  string -> Pp.ctx -> gradient_direction * gradient_stop list -> unit
(** [pp_linear_gradient_named name ctx (dir, stops)] prints a linear gradient as
    the function [name], eliding a default [dir]. *)

val pp_webkit_linear_gradient_named :
  string -> Pp.ctx -> gradient_direction * gradient_stop list -> unit
(** [pp_webkit_linear_gradient_named name ctx (dir, stops)] prints a prefixed
    linear gradient as the function [name], [dir] in the legacy syntax. *)

val pp_radial_gradient_named :
  string -> Pp.ctx -> radial_gradient_config * gradient_stop list -> unit
(** [pp_radial_gradient_named name ctx (config, stops)] prints a radial gradient
    as the function [name], eliding a default [config]. *)

val normalize_gradient_direction : gradient_direction -> gradient_direction
(** [normalize_gradient_direction v] is [v] in its canonical spelling. *)

val normalize_gradient_stop : ?lossless:bool -> gradient_stop -> gradient_stop
(** [normalize_gradient_stop ?lossless s] is [s] with its colour and positions
    in canonical spelling; [lossless] keeps colour channels unrounded. *)

val normalize_webkit_gradient :
  ?lossless:bool -> Webkit_gradient.t -> Webkit_gradient.t
(** [normalize_webkit_gradient ?lossless g] is [g] with its stop colours in
    canonical spelling; [lossless] keeps colour channels unrounded. *)

val normalize_radial_config : radial_gradient_config -> radial_gradient_config
(** [normalize_radial_config v] is [v] in its canonical spelling. *)

val normalize_conic_config : conic_gradient_config -> conic_gradient_config
(** [normalize_conic_config v] is [v] in its canonical spelling. *)

val fold_double_position_stops : gradient_stop list -> gradient_stop list
(** [fold_double_position_stops stops] merges each pair of adjacent
    single-position stops of one colour into one double-position stop (CSS
    Images 4 sec. 3.5.1). *)

val drop_to_top_direction :
  gradient_direction ->
  gradient_stop list ->
  (gradient_direction * gradient_stop list) option
(** [drop_to_top_direction dir stops] is
    [Some (Default_direction, List.rev stops)] when [dir] is [0deg] and no stop
    carries a position; [None] otherwise. *)

val read_gradient_direction : Cursor.t -> gradient_direction
(** [read_gradient_direction t] reads a [gradient_direction] from [t]. *)

val read_gradient_stop_list : Cursor.t -> gradient_stop
(** [read_gradient_stop_list t] reads a [gradient_stop] from [t]. *)

val read_gradient_stop : Cursor.t -> gradient_stop
(** [read_gradient_stop t] reads a [gradient_stop] from [t]. *)

val read_radial_shape : Cursor.t -> radial_shape
(** [read_radial_shape t] reads a [radial_shape] from [t]. *)

val read_radial_size : Cursor.t -> radial_size
(** [read_radial_size t] reads a [radial_size] from [t]. *)

val read_radial_gradient_config : Cursor.t -> radial_gradient_config
(** [read_radial_gradient_config t] reads a [radial_gradient_config] from [t].
*)

val read_conic_gradient_config : Cursor.t -> conic_gradient_config
(** [read_conic_gradient_config t] reads a [conic_gradient_config] from [t]. *)

val read_gradient_prelude : Cursor.t -> gradient_direction
(** [read_gradient_prelude t] reads a [gradient_direction] from [t]. *)

val read_gradient_position : Cursor.t -> gradient_position
(** [read_gradient_position t] reads a [gradient_position] from [t]. *)

val read_linear_gradient_body : Cursor.t -> background_image
(** [read_linear_gradient_body t] reads a [background_image] from [t]. *)

val read_webkit_linear_gradient_body : Cursor.t -> background_image
(** [read_webkit_linear_gradient_body t] reads a [background_image] from [t]. *)

val read_radial_gradient_body_stops : Cursor.t -> background_image
(** [read_radial_gradient_body_stops t] reads a [background_image] from [t]. *)

val read_radial_gradient_body : Cursor.t -> background_image
(** [read_radial_gradient_body t] reads a [background_image] from [t]. *)

val read_conic_gradient_body : Cursor.t -> background_image
(** [read_conic_gradient_body t] reads a [background_image] from [t]. *)

val read_webkit_gradient_body : Cursor.t -> background_image
(** [read_webkit_gradient_body t] reads a [background_image] from [t]. *)

val read_repeating_linear_gradient : Cursor.t -> background_image
(** [read_repeating_linear_gradient t] reads a [background_image] from [t]. *)

val read_repeating_radial_gradient : Cursor.t -> background_image
(** [read_repeating_radial_gradient t] reads a [background_image] from [t]. *)

val read_repeating_conic_gradient : Cursor.t -> background_image
(** [read_repeating_conic_gradient t] reads a [background_image] from [t]. *)

val read_webkit_repeating_linear_gradient : Cursor.t -> background_image
(** [read_webkit_repeating_linear_gradient t] reads a [background_image] from
    [t]. *)

val webkit_radial_gradient_of_radial : background_image -> background_image
(** [webkit_radial_gradient_of_radial i] is [i] respelt as
    [-webkit-radial-gradient()] when it is a [radial-gradient()], else [i]. *)

val webkit_repeat_radial_of_radial : background_image -> background_image
(** [webkit_repeat_radial_of_radial i] is [i] respelt as
    [-webkit-repeating-radial-gradient()] when it is a [radial-gradient()], else
    [i]. *)

val moz_linear_gradient_of_webkit : background_image -> background_image
(** [moz_linear_gradient_of_webkit i] is [i] respelt as [-moz-linear-gradient()]
    when it is a [-webkit-linear-gradient()], else [i]. *)

val moz_repeat_linear_of_webkit : background_image -> background_image
(** [moz_repeat_linear_of_webkit i] is [i] respelt as
    [-moz-repeating-linear-gradient()] when it is a [-webkit-linear-gradient()],
    else [i]. *)

val moz_radial_gradient_of_radial : background_image -> background_image
(** [moz_radial_gradient_of_radial i] is [i] respelt as [-moz-radial-gradient()]
    when it is a [radial-gradient()], else [i]. *)

val moz_repeat_radial_of_radial : background_image -> background_image
(** [moz_repeat_radial_of_radial i] is [i] respelt as
    [-moz-repeating-radial-gradient()] when it is a [radial-gradient()], else
    [i]. *)

val o_linear_gradient_of_webkit : background_image -> background_image
(** [o_linear_gradient_of_webkit i] is [i] respelt as [-o-linear-gradient()]
    when it is a [-webkit-linear-gradient()], else [i]. *)

val o_repeat_linear_of_webkit : background_image -> background_image
(** [o_repeat_linear_of_webkit i] is [i] respelt as
    [-o-repeating-linear-gradient()] when it is a [-webkit-linear-gradient()],
    else [i]. *)

val o_radial_gradient_of_radial : background_image -> background_image
(** [o_radial_gradient_of_radial i] is [i] respelt as [-o-radial-gradient()]
    when it is a [radial-gradient()], else [i]. *)

val o_repeat_radial_of_radial : background_image -> background_image
(** [o_repeat_radial_of_radial i] is [i] respelt as
    [-o-repeating-radial-gradient()] when it is a [radial-gradient()], else [i].
*)

val minify_gradient_stop : gradient_stop -> gradient_stop
(** [minify_gradient_stop s] is [s] with a percentage stop's colour minified. *)

val minify_webkit_gradient_stop : Webkit_gradient.stop -> Webkit_gradient.stop
(** [minify_webkit_gradient_stop s] is [s] with its colour minified. *)

val minify_conic_gradient_config :
  conic_gradient_config -> conic_gradient_config
(** [minify_conic_gradient_config c] is [c]. *)

val linear_gradient :
  gradient_direction -> gradient_stop list -> background_image
(** [linear_gradient dir stops] is [linear-gradient(dir, stops)]. *)

val radial_gradient :
  ?config:radial_gradient_config -> gradient_stop list -> background_image
(** [radial_gradient ?config stops] is [radial-gradient(config, stops)];
    [config] defaults to no shape, size, position or interpolation. *)

val color_stop : Values.color -> gradient_stop
(** [color_stop c] is the gradient stop [c] with no position. *)

val color_position : Values.color -> Values.length -> gradient_stop
(** [color_position c l] is the gradient stop [c l]. *)
