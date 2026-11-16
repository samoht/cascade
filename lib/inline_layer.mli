(** Cascade layers: the layer order a stylesheet declares, whether dropping its
    [\@layer] wrappers keeps every winner, and the folding of a custom property
    whose definitions across layers have a statically decided winner. *)

val layer_order : Stylesheet.t -> string list option
(** [layer_order stylesheet] is the cascade layer order [stylesheet] declares,
    weakest first, or [None] when static analysis cannot settle it. *)

val flattening_layers_is_safe : Stylesheet.t -> bool
(** [flattening_layers_is_safe stylesheet] is [true] when dropping every
    [\@layer] wrapper leaves the same declaration winning each cascade slot. *)

val collapse_layer_decided : keep:string list -> Stylesheet.t -> Stylesheet.t
(** [collapse_layer_decided ~keep stylesheet] replaces the definitions of each
    custom property whose winner across layers is decided with one unlayered
    definition of that winner. A name in [keep] is left alone. *)
