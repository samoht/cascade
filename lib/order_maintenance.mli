(** A total order over dense integer elements that answers which of two comes
    first in constant time while elements are inserted and moved.

    This is the order-maintenance problem (Dietz and Sleator, STOC 1987), with
    the list-labelling of Bender, Cole, Demaine, Farach-Colton and Zito ("Two
    simplified algorithms for maintaining order in a list", ESA 2002): every
    element carries an integer tag, tags increase along the order, and an
    insertion that finds no free tag between its neighbours relabels the
    smallest aligned tag range around them whose density is low enough. A
    relabel touches that range alone, never the whole order, and costs
    [O(log n)] amortized per insertion; the comparison reads two tags.

    Elements are the integers [0] to [n - 1] for some [n] the order grows to:
    {!Rule_graph} numbers its nodes that way. An element is either in the order
    or out of it; only one in it has a place. *)

type t
(** A mutable order. *)

val v : int -> t
(** [v n] is the order [0, 1, ..., n - 1]. *)

val mem : t -> int -> bool
(** [mem t x] is [true] when [x] has a place in [t]. *)

val compare : t -> int -> int -> int
(** [compare t a b] is negative when [a] comes before [b], zero when they are
    one element and positive otherwise. Both have a place in [t]. *)

val first : t -> int option
(** [first t] is the first element of [t], if any. *)

val next : t -> int -> int option
(** [next t x] is the element right after [x], if any. [x] has a place. *)

val prev : t -> int -> int option
(** [prev t x] is the element right before [x], if any. [x] has a place. *)

val exists_between : t -> int -> int -> (int -> bool) -> bool
(** [exists_between t a b f] is whether [f] holds of some element strictly
    between [a] and [b], asked in order and stopping at the first that says yes.
    [a] has a place; when [b] has none or comes before [a], every element after
    [a] is asked. *)

val insert_after : t -> int -> int -> unit
(** [insert_after t a x] places [x] right after [a]. [a] has a place and [x] has
    none. *)

val insert_before : t -> int -> int -> unit
(** [insert_before t b x] places [x] right before [b]. [b] has a place and [x]
    has none. *)

val insert_first : t -> int -> unit
(** [insert_first t x] places [x] before every element. [x] has none. *)

val remove : t -> int -> unit
(** [remove t x] takes [x] out of [t]. [x] has a place. *)

val relabels : t -> int
(** [relabels t] is how many elements relabelling has retagged so far, for tests
    of the amortized bound. *)
