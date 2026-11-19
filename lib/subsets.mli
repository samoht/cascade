(** The subsets of a bucket's members the candidate search tries.

    A bucket holds the rules that share a declaration, or write one property
    once, in the order they print. Trying every subset is exponential, so a
    budget bounds the search. *)

type t
(** A budget. *)

val max_span : int
(** [max_span] is how far an identical-body merge reaches under the transfer
    objective: [128] printed rules. *)

val v : aggressive:bool -> t
(** [v ~aggressive] is the budget of a factoring kind: the runs of up to 24
    neighbouring members of a bucket, none of a bucket with more than 256 such
    runs, and one budget of 256 subsets shared by the buckets of a run. Under
    [aggressive] it tries a bucket of up to 12 members whole, less one member
    and as runs of neighbours, and a larger one as up to 32 runs of neighbours.
*)

val shared : t -> int option
(** [shared b] is the number of subsets all the buckets of a run share, taken in
    the order the caller ranks them, when [b] bounds the run as a whole. *)

val of_bucket : t -> Rule_graph.node_id list -> Rule_graph.node_id list list
(** [of_bucket b ids] is the subsets of the bucket [ids], given in the order
    they print, that [b] tries, in the order it tries them. *)

val take :
  t -> Rule_graph.node_id list list list -> Rule_graph.node_id list list
(** [take b buckets] is the subsets [b] takes from [buckets], each the subsets
    {!of_bucket} offers for one bucket, given in the order a shared budget is
    spent in: all of them when [b] does not bound the run as a whole, else the
    first {!shared} [b] of them in that order. *)
