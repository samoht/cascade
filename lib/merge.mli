(** Rule merge helpers. *)

val listable : Selector.t -> bool
(** [listable sel] is whether a selector list may hold [sel] beside another
    selector: whether the evergreen targets that drop [sel] are known, and drop
    each of its branches alike. A browser drops a whole list when it does not
    parse one of its selectors (Selectors 4 sec. 3.9), so a grouping pass lists
    only selectors {!compatible} with each other. *)

val parsed_by_none : Selector.t -> bool
(** [parsed_by_none sel] is whether no release of any target engine, from its
    evergreen floor on, parses [sel]: each engine drops one of its branches in
    every release (Selectors 4 sec. 3.9), as the facts about prefixed names
    record. A node cascade has no answer for counts as parsed, and so does one
    the data records as unshipped, since it records a partial implementation as
    none. *)

val key : Selector.t -> Selector.t list
(** Canonical key for same-target selector comparison. *)

val selector_list : Selector.t list -> Selector.t
(** Build a flat selector list, preserving a singleton selector. *)

val declarations_equal :
  same:(Declaration.declaration -> Declaration.declaration -> bool) ->
  Declaration.declaration list ->
  Declaration.declaration list ->
  bool
(** Compare declaration lists with physical-identity fast path. *)

type compatibility
(** What {!compatible} reads off a selector. *)

val compatibility : Selector.t -> compatibility
(** [compatibility sel] is what {!compatible} reads off [sel], to compare many
    times without reading [sel] again. *)

val equal_compatibility : compatibility -> compatibility -> bool
(** [equal_compatibility a b] is whether selectors of compatibility [a] and [b]
    are {!compatible}. It is false whenever one is not {!listable}. *)

val compatible : Selector.t -> Selector.t -> bool
(** [compatible sel1 sel2] is whether [sel1] and [sel2] can share one selector
    list: whether both are {!listable} and the same browsers at or above the
    evergreen floors drop each, so that every one of them reads the list as it
    reads them apart. A selector some floor drops may be read from a later
    release, so two such selectors list only when the data has every engine ship
    them in the same release. The relation is an equivalence on listable
    selectors. *)

val all_compatible : Selector.t list -> bool
(** [all_compatible sels] is whether every pair in [sels] is {!compatible}, read
    off each selector once rather than once per pair. *)

type run
(** A group of selectors under consideration for one selector list. *)

val empty_run : run
(** The group holding no selector. *)

val extend_run : run -> Selector.t -> run
(** [extend_run run sel] is [run] with [sel] added, reading [sel] once whatever
    the group already holds. *)

val run_compatible : run -> bool
(** Whether every pair in the group is {!compatible}. *)
