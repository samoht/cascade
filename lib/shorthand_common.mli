(** Helpers shared by the shorthand composers: the walks over a {!Rule_index}
    that absorb longhand runs, and the tests every family applies to a run. *)

val preserve_list : 'a list -> 'a list -> 'a list
(** [preserve_list input output] is {!Common.List.preserve}. *)

val scope : Ctx.t -> Ctx.scope
(** [scope ctx] is {!Ctx.scope}. *)

val compose_fixed3_via_index :
  Rule_index.t ->
  try_compose:(Rule_index.t -> int -> Declaration.declaration option) ->
  unit
(** [compose_fixed3_via_index idx ~try_compose] absorbs every three-longhand run
    of [idx] that [try_compose] turns into a shorthand. *)

val compose_run_via_index :
  Rule_index.t ->
  try_compose:(Rule_index.t -> int -> (Declaration.declaration * int) option) ->
  unit
(** [compose_run_via_index idx ~try_compose] absorbs every run of [idx] that
    [try_compose] turns into a shorthand, consuming the number of positions it
    reports. Positions an earlier composer absorbed are skipped. *)

val take_run_at :
  Rule_index.t ->
  part_of:(Declaration.declaration -> 'a option) ->
  int ->
  (Declaration.declaration * 'a) list * int
(** [take_run_at idx ~part_of i] is the contiguous run of unabsorbed
    declarations starting at [i] that [part_of] classifies, paired with their
    parts, and the run's length. *)

val slots_written_after :
  slots_of:(Declaration.declaration -> int) -> Rule_index.t -> from:int -> int
(** [slots_written_after ~slots_of idx ~from] is the union of [slots_of] over
    the unabsorbed declarations of [idx] at [from] and after. *)

val same_importance : Declaration.declaration list -> bool
(** [same_importance decls] is [true] when every declaration of [decls] has the
    same importance. *)

val property_of_name : string -> Properties.any_property option
(** [property_of_name name] is the typed property [name] spells, if the reader
    types one. *)

val has_runtime_substitution : Declaration.declaration -> bool
(** [has_runtime_substitution d] is [true] when [d]'s value holds a [var()] or
    another runtime substitution. *)
