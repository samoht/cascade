(** The subsets of a bucket's members the candidate search tries.

    A bucket holds the rules that share a declaration, or write one property
    once, in source order. Trying every subset is exponential, so a budget
    bounds the search by the size of the run it belongs to. *)

type t
(** A budget. *)

val exact : aggressive:bool -> int -> t
(** [exact ~aggressive nodes] is the budget of exact-shared factoring in a run
    of [nodes] rules: every small bucket whole, less one member, and as runs of
    neighbours up to 128 rules or under [aggressive], and runs of up to 24
    neighbours spanning at most 128 source positions above. *)

val default : aggressive:bool -> int -> t
(** [default ~aggressive nodes] is {!exact} for default factoring, which tries
    one run of a large bucket where exact sharing tries 32, unless [aggressive].
*)

val shared : t -> int option
(** [shared b] is the number of subsets all the buckets of a run share, taken in
    the order the caller ranks them, when [b] bounds the run as a whole. *)

val of_bucket :
  t ->
  origin:(Rule_graph.node_id -> int) ->
  Rule_graph.node_id list ->
  Rule_graph.node_id list list
(** [of_bucket b ~origin ids] is the subsets of the bucket [ids], given in
    source order, that [b] tries, in the order it tries them. [origin] is each
    rule's source position. *)
