(** Debug-style dumps of the context records, for inspection and logging; not
    CSS source. *)

open Context_intf

val pp : t Pp.t
(** [pp] dumps a property-value context. *)

val pp_document : document Pp.t
(** [pp_document] dumps a document context. *)

val pp_query : query Pp.t
(** [pp_query] dumps a query context. *)

val pp_loader : loader Pp.t
(** [pp_loader] dumps a loader context. *)

val pp_animation : animation Pp.t
(** [pp_animation] dumps an animation context. *)

val pp_property_registry : property_registry Pp.t
(** [pp_property_registry] dumps a registered-property context. *)
