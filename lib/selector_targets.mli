(** Which target browsers drop a selector: the releases of each engine from its
    evergreen floor on that do not parse it. Selectors 4 sec. 3.9 drops a whole
    selector list in a browser that does not parse one of its selectors, and
    sec. 4.2 and 4.3 drop it from a forgiving [:is()] or [:where()] alone. *)

type dropped
(** The releases of each engine that drop a selector. *)

val equal_dropped : dropped -> dropped -> bool
(** [equal_dropped a b] is whether the same releases make up [a] and [b]. *)

val dropped_by : Selector_intf.t -> dropped option
(** [dropped_by sel] is the releases that drop [sel], or [None] when the data
    has no answer for one of its nodes or when its list branches are dropped by
    different releases. A selector inside a forgiving [:is()] or [:where()]
    drops nothing outside it. *)

val parsed_by_none : Selector_intf.t -> bool
(** [parsed_by_none sel] is whether no release of any target engine, from its
    evergreen floor on, parses [sel]: each engine drops one of its branches in
    every release (Selectors 4 sec. 3.9), as the facts about prefixed names
    record. A node cascade has no answer for counts as parsed, and so does one
    the data records as unshipped, since it records a partial implementation as
    none. *)

val read_by_every_target : Selector_intf.t -> bool
(** [read_by_every_target sel] is whether every release of every engine from its
    floor on parses [sel]. A rewrite that moves [sel] into or out of a forgiving
    [:is()] keeps what each of them matches only then. *)
