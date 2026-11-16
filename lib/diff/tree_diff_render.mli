(** The tree diff report: tree-style and flat rendering of rule and container
    changes under depth and entry budgets. *)

open Tree_diff_intf

val pp :
  ?expected:string ->
  ?actual:string ->
  ?color:bool ->
  ?depth:int ->
  ?entries:int ->
  Buffer.t ->
  t ->
  unit
(** [pp ?expected ?actual ?color ?depth ?entries buf t] writes the report of [t]
    into [buf], as [Tree_diff.pp] describes. *)

val pp_rule_diff_simple : Buffer.t -> rule_diff -> unit
(** [pp_rule_diff_simple buf rule] writes [rule] to [buf] in the one-line form
    tests compare against. *)
