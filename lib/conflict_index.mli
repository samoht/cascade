(** The index that finds the rules that may conflict with one another.

    Two rules conflict only when some branches of theirs tie on specificity and
    may match one element, and some declarations of theirs write a common slot
    differently at one importance. The index buckets what it holds by the
    overlap keys a rule's declarations write and the specificity of each of its
    branches, and the graph's index of nodes further by the element and IDs the
    subject requires and the importance of the declaration, so a query
    enumerates only what can meet it on every one of those. A candidate is not
    yet a conflict: the caller tests it. *)

module Decl_tbl : Hashtbl.S with type key = Declaration.declaration
(** Tables keyed by a declaration, compared by its minified text. *)

(** Tables keyed by a packed specificity. *)
module Spec_table : sig
  include Hashtbl.S with type key = int

  val push : 'a list t -> key -> 'a -> unit
  (** [push tbl key value] prepends [value] to the list bound to [key]. *)
end

type slot = { key : Shorthand.overlap_key; spec : int }
(** An overlap key at a packed specificity. *)

module Slot_table : sig
  include Hashtbl.S with type key = slot

  val push : 'a list t -> key -> 'a -> unit
  (** [push tbl key value] prepends [value] to the list bound to [key]. *)
end

type decl_overlap = {
  decl : Declaration.declaration;
  footprint : Shorthand.overlap_key list;
}
(** A declaration and the overlap keys it writes. *)

val distinct_spec_keys : int list -> Selector.specificity list -> int list
(** [distinct_spec_keys acc specs] adds the packed keys of [specs] to [acc],
    each once. *)

val add_spec_buckets : 'a list Spec_table.t -> 'a -> int list -> unit
(** [add_spec_buckets table value specs] pushes [value] under each of [specs].
*)

val add_bucket :
  'a list Slot_table.t -> Shorthand.overlap_key -> int list -> 'a -> unit
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
(** [add_at index ~at ~specs ~keys x] adds [x] at position [at], which is no
    less than any position added before, for a rule with the packed branch
    specificities [specs] and the distinct overlap [keys]. *)

val exists_after :
  'a positioned ->
  after:int ->
  specs:int list ->
  keys:Shorthand.overlap_key list ->
  ('a -> bool) ->
  bool
(** [exists_after index ~after ~specs ~keys test] is [true] when [test x] holds
    of some [x] added at a position greater than [after] that shares a
    specificity of [specs] and an overlap key of [keys], or the broad key on
    either side. Each such [x] is tested at most once. *)

val add_candidates : int -> int array -> int list -> int list -> int list
(** [add_candidates stamp seen acc nodes] adds to [acc] each of [nodes] whose
    [seen] entry is not [stamp] yet, and stamps it. *)

val collect_specs :
  int list Spec_table.t -> int list -> int -> int array -> int list -> int list
(** [collect_specs table specs stamp seen acc] adds the nodes under each of
    [specs], as {!add_candidates} does. *)

val collect_bucket :
  int list Slot_table.t ->
  Shorthand.overlap_key ->
  int list ->
  int ->
  int array ->
  int list ->
  int list
(** [collect_bucket table key specs stamp seen acc] adds the nodes under [key]
    and each of [specs], as {!add_candidates} does. *)

type t
(** An index of nodes, numbered from [0]. *)

val v : int -> t
(** [v n] is an empty index with room for nodes [0] to [n - 1]. *)

val grow : t -> int -> unit
(** [grow t n] makes room in [t] for nodes [0] to [n - 1]. *)

val seen : t -> int array
(** [seen t] is the stamp each node carries from the last query that reached it.
*)

val candidates :
  t ->
  relay:bool ->
  keys:Shorthand.overlap_key list ->
  decl_overlap list ->
  Selector.t list ->
  Selector_summary.t list ->
  Selector.specificity list ->
  int ->
  int list
(** [candidates t ~relay ~keys decls branches summaries specificities node] is
    the nodes of [t] that may conflict with [node], which writes [decls] with
    the distinct overlap [keys] and has the given selector branches, their
    summaries and their specificities. [node] stamps the query, so it must be
    new to [t]. A [relay] node is answered through the compact runs {!insert}
    keeps, which carry the order of every node inserted before it; any other is
    answered with every node the index holds. *)

val insert :
  t ->
  relay:bool ->
  decl_overlap list ->
  Selector.t list ->
  Selector_summary.t list ->
  Selector.specificity list ->
  int ->
  unit
(** [insert t ~relay decls branches summaries specificities node] adds [node] to
    [t]. A [relay] node that writes one declaration through a simple subject
    joins the compact runs, which a later relay reads in place of the nodes the
    runs carry; only a node that stays in the order may relay it. *)
