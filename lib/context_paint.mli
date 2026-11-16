(** Colour, shadow, border-radius and background simplifiers: each resolves the
    [var()] references of a value against a context, folds the colour arithmetic
    and [calc()] it can, and resolves image URLs against the base URL. *)

open Context_intf
open Context_length

val simplify_color :
  ?layer_order:string list -> ?layer:string -> t -> Values.color -> Values.color
(** [simplify_color ctx v] is [v] simplified against [ctx]. *)

val simplify_shadow :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.shadow ->
  Properties.shadow
(** [simplify_shadow ctx lctx v] is [v] simplified against [ctx], with lengths
    resolved against [lctx]. *)

val simplify_border_radius :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Length.ctx ->
  Properties.border_radius ->
  Properties.border_radius
(** [simplify_border_radius ctx lctx v] is [v] simplified against [ctx], with
    lengths resolved against [lctx]. *)

val simplify_background_image :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.background_image ->
  Properties.background_image
(** [simplify_background_image ctx v] is [v] simplified against [ctx]. *)

val simplify_background :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Properties.background ->
  Properties.background
(** [simplify_background ctx v] is [v] simplified against [ctx]. *)
