(** Order maintenance: a total order over opaque elements that answers
    precedence queries in O(1) while supporting insertion and deletion.

    This is the {e order-maintenance problem} (Dietz & Sleator 1987; simplified
    by Bender, Cole, Demaine, Farach-Colton & Zito, ESA 2002): keep a sequence
    under [insert_after] / [remove] so that [compare] tells, in constant time,
    which of two live elements comes first.

    The incremental rule merger uses it for cascade precedence: rules keep a
    stable position handle even as merges delete some and factoring inserts new
    shared rules, so "does rule A come before rule B?" stays O(1) instead of
    re-deriving list indices after every edit.

    Elements are identified by a {!node} handle returned at insertion. A handle
    stays valid until it is {!remove}d; using a removed handle is an error. *)

type 'a t
(** A mutable ordered collection of ['a] elements. *)

type 'a node
(** A stable handle to one element, returned by {!add_last} / {!insert_after}.
*)

val v : unit -> 'a t
(** [v ()] is an empty order. *)

val is_empty : 'a t -> bool
(** [is_empty t] is [true] when [t] holds no live element. *)

val length : 'a t -> int
(** [length t] is the number of live elements. *)

val add_last : 'a t -> 'a -> 'a node
(** [add_last t x] appends [x] after every current element and returns its
    handle. *)

val insert_after : 'a t -> 'a node -> 'a -> 'a node
(** [insert_after t n x] inserts [x] immediately after [n] and returns its
    handle. [n] must be live. *)

val insert_before : 'a t -> 'a node -> 'a -> 'a node
(** [insert_before t n x] inserts [x] immediately before [n] and returns its
    handle. [n] must be live. *)

val remove : 'a t -> 'a node -> unit
(** [remove t n] deletes [n] from the order. [n] must be live; it must not be
    used afterwards. *)

val data : 'a node -> 'a
(** [data n] is the element [n] was created with. *)

val is_live : 'a node -> bool
(** [is_live n] is [false] once [n] has been removed. *)

val compare : 'a node -> 'a node -> int
(** [compare a b] is negative when [a] precedes [b], positive when [b] precedes
    [a], and [0] only when [a] and [b] are the same handle. O(1). Both handles
    must be live. *)

val to_list : 'a t -> 'a list
(** [to_list t] is the live elements in order. *)

val nodes : 'a t -> 'a node list
(** [nodes t] are the live handles in order. *)

val next : 'a node -> 'a node option
(** [next n] is the handle immediately after [n], or [None] at the end. *)

val prev : 'a node -> 'a node option
(** [prev n] is the handle immediately before [n], or [None] at the start. *)

val id : 'a node -> int
(** [id n] is a stable integer identity unique within the node's collection,
    assigned at creation and never reused. Unlike the precedence answered by
    {!compare}, [id] is invariant under insertions and deletions elsewhere, so
    it can key a map or priority queue of handles. *)
