(** Suffix coverage table for same-selector shadowing. *)

type t
(** Properties written later, keyed by selector. *)

type written
(** What one selector has written later for it. *)

val empty : written
(** No properties written. *)

val v : unit -> t
(** [v ()] creates an empty coverage table. *)

val written : t -> Selector.t -> written
(** [written t selector] is what [t] holds for [selector], read in one lookup
    and answering for every declaration a rule at that selector carries. *)

val covered : floors:Support.targets -> written -> Declaration.t -> bool
(** [covered ~floors written decl] is [true] when [decl]'s property is
    definitely shadowed by a later declaration at the same or stronger
    importance that every browser at or above [floors] reading [decl] reads too
    ({!Declaration.fallback_for}). A later write a floor drops while it reads
    [decl] leaves [decl] as the fallback it paints. *)

val add : written -> Declaration.t list -> written
(** [add written decls] extends [written] with [decls]. *)

val record : t -> Selector.t -> written -> Declaration.t list -> unit
(** [record t selector written decls] stores [written] extended with [decls] as
    what is written later for [selector], in one store. *)
