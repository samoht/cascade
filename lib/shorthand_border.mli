(** The [border] family: [outline], the [border] and [border-<edge>] lines,
    [column-rule], the whole [border] from its side longhands, and
    [border-image]. *)

val compose_outline_via_index : Rule_index.t -> unit
(** [compose_outline_via_index idx] absorbs every
    [outline-width]/[-style]/[-color] run of [idx] into [outline]. *)

val compose_border_via_index :
  bi_hazard:(int -> important:bool -> bool) -> Rule_index.t -> unit
(** [compose_border_via_index ~bi_hazard idx] absorbs every run of the twelve
    side longhands of [idx] that agree on all sides into [border].
    [bi_hazard i ~important] is [true] when the [border-image] reset at [i]
    would drop a held value. *)

val compose_line_via_index : Rule_index.t -> unit
(** [compose_line_via_index idx] absorbs every width/style/colour run of [idx]
    into its line shorthand ([border-<edge>], [border-block], [border-inline],
    [column-rule]). *)

val reorder_border_image_before_border :
  ('a * Declaration.declaration) list -> ('a * Declaration.declaration) list
(** [reorder_border_image_before_border decls] moves a run of [border-image]
    longhands after the [border-width]/[-style]/[-color] run that follows it, so
    the composed [border] does not reset it. *)

val compose_border_whole_via_index :
  bi_hazard:(int -> important:bool -> bool) -> ctx:Ctx.t -> Rule_index.t -> unit
(** [compose_border_whole_via_index ~bi_hazard ~ctx idx] absorbs every
    single-valued [border-width]/[-style]/[-color] run of [idx] into [border],
    as {!val-compose_border_via_index}. *)

val drop_bimg_shadowed_by_border :
  ('a * Declaration.declaration) list -> ('a * Declaration.declaration) list
(** [drop_bimg_shadowed_by_border decls] drops a [border-image*] declaration
    that a later [border] of at least equal importance resets, when no later
    [border-image*] re-establishes it. *)

val compose_border_image_shorthand :
  ctx:Ctx.t ->
  ('a * Declaration.declaration) list ->
  (int * Declaration.declaration) list
(** [compose_border_image_shorthand ~ctx decls] composes the [border-image]
    longhand runs of [decls] into [border-image] and renumbers the result. *)
