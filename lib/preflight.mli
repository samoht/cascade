(** Cheap gate for the expensive global factoring fixpoint. *)

type t
(** Summary of the factoring opportunity surface. *)

val small_declaration_threshold : int
(** Declaration count below which factoring always runs. *)

val summarize : Stylesheet.rule list -> t
(** [summarize rules] computes the cheap opportunity summary for [rules]. *)

val declaration_count : t -> int
(** [declaration_count t] is the total number of declarations seen. *)

val source_units : t -> int
(** [source_units t] is a cheap weighted source-size estimate. *)

val estimated_gain : t -> int
(** [estimated_gain t] is the cheap byte-saving proxy. *)

val useful : t -> bool
(** [useful t] is [true] when the estimated gain justifies running the full
    factoring fixpoint. Small inputs always return [true]. *)
