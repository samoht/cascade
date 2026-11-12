(** Numeric value simplifiers: lengths, angles, durations, opacity and numbers,
    and the transform and filter values built from them. Each one resolves the
    [var()] references of a value against a context and folds its [calc()] as
    far as the context allows, keeping what does not resolve as residual syntax.
*)

open Context_intf
open Context_length

val simplify_length_percentage :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Values.length_percentage ->
  Values.length_percentage
(** [simplify_length_percentage ctx lctx v] is [v] simplified against [ctx],
    with lengths resolved against [lctx]. *)

val simplify_border_width :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.border_width ->
  Properties.border_width
(** [simplify_border_width ctx lctx v] is [v] simplified against [ctx], with
    lengths resolved against [lctx]. *)

val simplify_font_size :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.font_size ->
  Properties.font_size
(** [simplify_font_size ctx lctx v] is [v] simplified against [ctx], with
    lengths resolved against [lctx]. *)

val simplify_stroke_width :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.stroke_width ->
  Properties.stroke_width
(** [simplify_stroke_width ctx lctx v] is [v] simplified against [ctx], with
    lengths resolved against [lctx]. *)

val simplify_opacity :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.opacity ->
  Properties.opacity
(** [simplify_opacity ctx v] is [v] simplified against [ctx]. *)

val simplify_angle :
  ?layer_order:string list -> ?layer:string -> t -> Values.angle -> Values.angle
(** [simplify_angle ctx v] is [v] simplified against [ctx]. *)

val simplify_duration :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Values.duration ->
  Values.duration
(** [simplify_duration ctx v] is [v] simplified against [ctx]. *)

val simplify_number_percentage :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Values.number_percentage ->
  Values.number_percentage
(** [simplify_number_percentage ctx v] is [v] simplified against [ctx]. *)

val simplify_rotate_value :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.rotate_value ->
  Properties.rotate_value
(** [simplify_rotate_value ctx v] is [v] simplified against [ctx]. *)

val simplify_translate_value :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.translate_value ->
  Properties.translate_value
(** [simplify_translate_value ctx lctx v] is [v] simplified against [ctx], with
    lengths resolved against [lctx]. *)

val simplify_scale :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.scale ->
  Properties.scale
(** [simplify_scale ctx v] is [v] simplified against [ctx]. *)

val simplify_transform :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.transform ->
  Properties.transform
(** [simplify_transform ctx lctx v] is [v] simplified against [ctx], with
    lengths resolved against [lctx]. *)

val simplify_filter :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.filter ->
  Properties.filter
(** [simplify_filter ctx lctx v] is [v] simplified against [ctx], with lengths
    resolved against [lctx]. *)
