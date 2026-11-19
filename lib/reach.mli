(** Reachable sets of the nodes of one {!Rule_graph}, one bit per node.

    Every set of one graph has the width its table of sets has room for, so two
    of them combine word for word; the functions taking two sets raise
    [Invalid_argument] on two of unequal widths. *)

val table : int -> Bytes.t array
(** [table count] is a table with room for [count] nodes, every set unset. *)

val v : int -> Bytes.t
(** [v count] is an empty set over [count] nodes, at its own width. *)

val row : Bytes.t array -> Bytes.t
(** [row sets] is an empty set of the width [sets] holds. *)

val is_set : Bytes.t -> bool
(** [is_set set] is whether [set] has been built, as against left unset in a
    table. *)

val grow : Bytes.t array -> int -> Bytes.t array
(** [grow sets count] is [sets] with room for [count] nodes, every built set
    widened to the new width; [sets] itself when it has room already. The room
    grows by a quarter at a time. *)

val mem : Bytes.t -> int -> bool
(** [mem set i] is whether node [i] is in [set]. *)

val add : Bytes.t -> int -> unit
(** [add set i] puts node [i] in [set]. *)

val meets : Bytes.t -> Bytes.t -> bool
(** [meets a b] is whether [a] and [b] share a node. *)

val missing : Bytes.t -> Bytes.t -> int list option
(** [missing a b] is the nodes of [a] that [b] lacks, or [None] when there are
    more of them than words in a set, past which {!union} is the cheaper way to
    add them. *)

val union : into:Bytes.t -> Bytes.t -> unit
(** [union ~into set] adds every node of [set] to [into]. *)
