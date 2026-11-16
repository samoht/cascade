(** The index that finds the rules that may conflict with one another.

    Two rules conflict only when some branches of theirs tie on specificity and
    may match one element, and some declarations of theirs write a common slot
    differently at one importance. The index buckets what it holds by the
    overlap keys a rule's declarations write and the specificity of each of its
    branches, so a query enumerates only what can meet it on both. A candidate
    is not yet a conflict: the caller tests it. *)

(** Tables keyed by a packed specificity. *)
module Spec_table : sig
  include Hashtbl.S with type key = int

  val push : 'a list t -> key -> 'a -> unit
  (** [push tbl key value] prepends [value] to the list bound to [key]. *)
end

module Overlap_key_table : Hashtbl.S with type key = Shorthand.overlap_key
(** Tables keyed by an overlap key. *)

val spec_key : Selector.specificity -> int
(** [spec_key s] is [s] packed into an immediate, saturating each component. *)

val distinct_spec_keys : int list -> Selector.specificity list -> int list
(** [distinct_spec_keys acc specs] adds the packed keys of [specs] to [acc],
    each once. *)

val spec_bucket :
  'a Spec_table.t Overlap_key_table.t ->
  Shorthand.overlap_key ->
  'a Spec_table.t
(** [spec_bucket table key] is the specificity table under [key], added empty
    when absent. *)

val add_spec_buckets : 'a list Spec_table.t -> 'a -> int list -> unit
(** [add_spec_buckets table value specs] pushes [value] under each of [specs].
*)

val add_bucket :
  'a list Spec_table.t Overlap_key_table.t ->
  Shorthand.overlap_key ->
  int list ->
  'a ->
  unit
(** [add_bucket table key specs value] pushes [value] under [key] and each of
    [specs]. *)

type 'a positioned
(** Values, each at a position, bucketed by the specificities and overlap keys
    of the rule each stands for. *)

val positioned : unit -> 'a positioned
(** [positioned ()] is an empty index. *)

val add_at :
  'a positioned ->
  at:int ->
  specs:int list ->
  keys:Shorthand.overlap_key list ->
  'a ->
  unit
(** [add_at index ~at ~specs ~keys v] adds [v] at position [at], which is no
    less than any position added before, for a rule with the packed branch
    specificities [specs] and the distinct overlap [keys]. *)

val exists_after :
  'a positioned ->
  after:int ->
  specs:int list ->
  keys:Shorthand.overlap_key list ->
  ('a -> bool) ->
  bool
(** [exists_after index ~after ~specs ~keys test] is [true] when [test v] holds
    of some [v] added at a position greater than [after] that shares a
    specificity of [specs] and an overlap key of [keys], or the broad key on
    either side. Each such [v] is tested at most once. *)
