(** The typed property-value printer. *)

open Properties_intf

val pp_property_value : ('a property * 'a) Pp.t
(** [pp_property_value] is the pretty-printer for a property and its typed
    value. *)
