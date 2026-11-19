(** Exact-shared and default factoring for {!Rule_candidate}.

    Rules sharing a declaration, or writing one property once, form a bucket.
    The candidates are the subsets of a bucket's members {!Subsets} takes, each
    making a group rule, offered unless it would cross a live rule printed
    between its members. *)

type factoring =
  | Exact  (** a bucket per declaration the members share *)
  | Default  (** a bucket per property the members write once *)

include Candidate_kind.S

val v : int -> factoring -> Subsets.t -> t
(** [v size factoring budget] has filed no rule, and searches the subsets
    [budget] takes; [size] sizes its tables. *)

val searched : Candidate_kind.core -> t -> ranks:int array Lazy.t -> bool
(** [searched core t ~ranks] is [true] when a graph built afresh from the rules
    the graph of [core] prints would accept no candidate of [t] the current
    graph has not offered. [ranks] gives each live rule its rank in the printed
    order. *)
