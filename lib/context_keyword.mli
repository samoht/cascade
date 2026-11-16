(** Simplifiers for values whose only residual form is a [var()] reference:
    keyword properties, names, font sources, length lists, and the animation and
    transition items. *)

open Context_intf
open Context_length

val simplify_animation_name :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.animation_name ->
  Properties.animation_name
(** [simplify_animation_name ctx v] is [v] simplified against [ctx]. *)

val simplify_font_family :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.font_family ->
  Properties.font_family
(** [simplify_font_family ctx v] is [v] simplified against [ctx]. *)

val simplify_font_src :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Font_face.src ->
  Font_face.src
(** [simplify_font_src ctx v] is [v] simplified against [ctx]. *)

val simplify_animation_item :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  (Values.duration -> Values.duration) ->
  Properties.animation ->
  Properties.animation
(** [simplify_animation_item ctx duration v] is [v] simplified against [ctx],
    with its durations simplified by [duration]. *)

val simplify_transition_item :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  (Values.duration -> Values.duration) ->
  Properties.transition ->
  Properties.transition
(** [simplify_transition_item ctx duration v] is [v] simplified against [ctx],
    with its durations simplified by [duration]. *)

val simplify_display :
  layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.display ->
  Properties.display
(** [simplify_display ~layer_order ctx v] is [v] simplified against [ctx]. *)

val simplify_position :
  layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.position ->
  Properties.position
(** [simplify_position ~layer_order ctx v] is [v] simplified against [ctx]. *)

val simplify_visibility :
  layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.visibility ->
  Properties.visibility
(** [simplify_visibility ~layer_order ctx v] is [v] simplified against [ctx]. *)

val simplify_clear :
  layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.clear ->
  Properties.clear
(** [simplify_clear ~layer_order ctx v] is [v] simplified against [ctx]. *)

val simplify_float_side :
  layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.float_side ->
  Properties.float_side
(** [simplify_float_side ~layer_order ctx v] is [v] simplified against [ctx]. *)

val simplify_lengths_value :
  layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Values.length list ->
  Values.length list
(** [simplify_lengths_value ~layer_order ctx lctx v] is [v] simplified against
    [ctx], with lengths resolved against [lctx]; a lone reference may expand to
    the list its custom property holds. *)
