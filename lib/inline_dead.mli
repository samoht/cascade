(** Dead-code elimination after substitution: the census of where each custom
    property is defined and referenced, the closure of live definitions, and the
    pass dropping every definition nothing reads. *)

val collect_scoped_refs :
  Stylesheet.t ->
  (Inline_scope.at_node list * Selector.t * string list) list
  * (Inline_scope.at_node list * Selector.t * string * string list) list
  * string list
(** [collect_scoped_refs stylesheet] is [(consumers, customs, runtime)]: the
    names each non-custom declaration or at-rule condition references with its
    at-rule path and selector, each custom definition with its path, selector,
    name and the names it references, and the names read by a [~runtime]
    reference. *)

val live_customs :
  consumers:(Inline_scope.at_node list * Selector.t * string list) list ->
  customs:(Inline_scope.at_node list * Selector.t * string * string list) list ->
  (Inline_scope.at_node list * Selector.t * string) list
(** [live_customs ~consumers ~customs] is the custom definitions a consumer
    reaches, directly or through other live definitions. *)

val cyclic_live_customs :
  consumers:(Inline_scope.at_node list * Selector.t * string list) list ->
  customs:(Inline_scope.at_node list * Selector.t * string * string list) list ->
  (Inline_scope.at_node list * Selector.t * string) list
(** [cyclic_live_customs ~consumers ~customs] is the live custom definitions
    that reach themselves through their references. *)

val strip_dead :
  keep:string list ->
  live_set:(Inline_scope.at_node list * Selector.t * string) list ->
  Stylesheet.t ->
  Stylesheet.t
(** [strip_dead ~keep ~live_set stmts] drops every custom definition that is
    neither named in [keep] nor in [live_set]. *)

val normalise_var_name : string -> string
(** [normalise_var_name name] is [name] with its leading [--]. *)

val mentioned_custom_names : Stylesheet.t -> string list
(** [mentioned_custom_names stylesheet] is every custom-property name
    [stylesheet] declares or references. *)

val var_census : Stylesheet.t -> (string, int) Hashtbl.t * string list
(** [var_census stylesheet] is the number of scopes defining each custom
    property, and the names referenced anywhere. *)
