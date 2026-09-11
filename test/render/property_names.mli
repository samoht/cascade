(** The CSS property names cascade's reader accepts.

    Generated from the dispatch table in [lib/properties.ml], so a property
    added there reaches the render differential without a second edit. *)

val all : string list
(** [all] is every property name the reader dispatches on, in source order. *)
