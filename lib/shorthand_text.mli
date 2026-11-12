(** Font and text shorthands: [font] and the reorder that lets it compose,
    [font-synthesis], [font-variant], [list-style] and [text-decoration]. *)

val compose_font_via_index : ctx:Ctx.t -> Rule_index.t -> unit
(** [compose_font_via_index ~ctx idx] absorbs every [font] longhand run of [idx]
    into [font]. A run leaving a slot that no later declaration writes composes
    only under the [`Stylesheet] scope. *)

val reorder_font_resets_before_font :
  ('a * Declaration.declaration) list -> ('a * Declaration.declaration) list
(** [reorder_font_resets_before_font decls] moves a subproperty [font] resets
    after the foldable [font] longhand run that follows it, so the composed
    [font] does not reset it. *)

val compose_list_style_via_index : Rule_index.t -> unit
(** [compose_list_style_via_index idx] absorbs every [list-style] longhand run
    of [idx] into [list-style]. *)

val compose_font_synthesis_via_index : Rule_index.t -> unit
(** [compose_font_synthesis_via_index idx] absorbs every [font-synthesis-*] run
    of [idx] into [font-synthesis]. *)

val compose_text_decoration_via_index :
  held:Shorthand_held.held -> Rule_index.t -> unit
(** [compose_text_decoration_via_index ~held idx] absorbs every
    [text-decoration] longhand run of [idx] into [text-decoration], unless
    [held] or an earlier declaration holds a slot the run leaves out. *)

val compose_font_variant_via_index : Rule_index.t -> unit
(** [compose_font_variant_via_index idx] absorbs every [font-variant-*] run of
    [idx] into [font-variant]. *)
