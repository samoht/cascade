(** Cached rule summary for global factoring. *)

module Props : sig
  type t

  val empty : t
  (** Empty property set. *)

  val add : Declaration.prop_key -> t -> t
  (** [add prop t] returns [t] with [prop]. *)

  val mem : Declaration.prop_key -> t -> bool
  (** [mem prop t] is [true] when [prop] is in [t]. *)

  val inter : t -> t -> t
  (** [inter a b] is the intersection of [a] and [b]. *)
end

module Map : sig
  type 'a t

  val empty : 'a t
  (** Empty property-keyed map. *)

  val add : Declaration.prop_key -> 'a -> 'a t -> 'a t
  (** [add prop v t] returns [t] with [prop] bound to [v]. *)

  val mem : Declaration.prop_key -> 'a t -> bool
  (** [mem prop t] is [true] when [prop] is bound in [t]. *)

  val find_opt : Declaration.prop_key -> 'a t -> 'a option
  (** [find_opt prop t] returns the value bound to [prop], if any. *)
end

type ids = int array
(** Sorted compact property-id set. *)

type bloom
(** Compact declaration-hash prefilter. *)

type t
(** Summary of one rule used by factoring passes. *)

val reset : unit -> unit
(** [reset ()] clears the process-local property-id table. *)

val prop : Declaration.t -> Declaration.prop_key
(** [prop decl] returns the comparable property key for [decl]. *)

val ids_empty : ids -> bool
(** [ids_empty ids] is [true] when [ids] has no property ids. *)

val ids_mem : int -> ids -> bool
(** [ids_mem id ids] tests membership by binary search. *)

val ids_disjoint : ids -> ids -> bool
(** [ids_disjoint a b] is [true] when the sorted id sets do not intersect. *)

val ids_subset : ids -> ids -> bool
(** [ids_subset a b] is [true] when every id in [a] is present in [b]. *)

val ids_inter : ids -> ids -> ids
(** [ids_inter a b] returns the sorted intersection of [a] and [b]. *)

val v :
  rule_size:(Stylesheet.rule -> int) ->
  decl_size:(Declaration.t -> int) ->
  selector_size:(Selector.t -> int) ->
  Stylesheet.rule ->
  t
(** [v ~rule_size ~decl_size ~selector_size rule] builds the cached summary for
    [rule]. *)

val rule : t -> Stylesheet.rule
(** [rule t] is the summarized rule, preserving physical identity. *)

val size : t -> int
(** [size t] is the cached minified rule size. *)

val selector_size : t -> int
(** [selector_size t] is the cached minified selector size. *)

val decl_sizes : t -> int list
(** [decl_sizes t] are cached minified declaration sizes in source order. *)

val decl_pp_size : t -> int
(** [decl_pp_size t] is the sum of [decl_sizes t]. *)

val decl_count : t -> int
(** [decl_count t] is the number of declarations in [rule t]. *)

val prop_set : t -> Props.t
(** [prop_set t] is the set of properties declared by [rule t]. *)

val prop_ids : t -> ids
(** [prop_ids t] is the compact sorted id set for [prop_set t]. *)

val decl_prop_ids : t -> ids
(** [decl_prop_ids t] are property ids aligned with declarations in source
    order. *)

val selector_summary : t -> Selector_summary.t Lazy.t
(** [selector_summary t] is the lazily computed selector summary. *)

val same_bloom : t -> t -> bool
(** [same_bloom a b] is [true] when both declaration-hash blooms match. *)

val bloom : t -> bloom
(** [bloom t] is [t]'s declaration-hash bloom. *)

val bloom_of_decls : Declaration.t list -> bloom
(** [bloom_of_decls decls] builds a bloom from declaration hashes. *)

val may_share_bloom : t -> bloom -> bool
(** [may_share_bloom t bloom] is a constant-time possible-overlap test. *)

val may_share_decl_hash : t -> t -> bool
(** [may_share_decl_hash a b] is a constant-time possible declaration-hash
    overlap test. *)

val declares_all : t -> Declaration.prop_key list -> bool
(** [declares_all t props] is [true] when [t] declares every property in
    [props]. *)

val declares_ids : t -> ids -> bool
(** [declares_ids t ids] is [true] when [t] declares every property id in [ids].
*)

val decl_for_prop : t -> Declaration.prop_key -> Declaration.t option
(** [decl_for_prop t prop] returns the first declaration for [prop], if any. *)

val decl_size_for_prop : t -> Declaration.prop_key -> int option
(** [decl_size_for_prop t prop] returns the cached size of
    [decl_for_prop t prop], if any. *)

val contains :
  same:(Declaration.t -> Declaration.t -> bool) -> t -> Declaration.t -> bool
(** [contains ~same t decl] checks whether [t]'s rule has [decl], using a
    declaration-hash bloom prefilter before [same]. *)
