(** Layered shorthands: [background] and [mask]. *)

val compose_background_shorthand :
  ctx:Ctx.t ->
  ('a * Declaration.declaration) list ->
  (int * Declaration.declaration) list
(** [compose_background_shorthand ~ctx decls] composes the [background] longhand
    runs of [decls] into [background] and renumbers the result. *)

val reorder_mask_border_before_mask :
  ('a * Declaration.declaration) list -> ('a * Declaration.declaration) list
(** [reorder_mask_border_before_mask decls] moves a [mask-border] declaration
    after the following run of mask layer longhands when that run carries a
    [mask-image], so the composed [mask] does not reset it. *)

val compose_mask_via_index : ctx:Ctx.t -> Rule_index.t -> unit
(** [compose_mask_via_index ~ctx idx] absorbs every [mask] layer longhand run of
    [idx] into [mask]. *)
