(** The substitution pass: every [var()] a declaration holds is replaced by the
    value of the definition visible to it, and a reference whose definition
    lives elsewhere can be given a fallback. *)

val trim_components : Component.t list -> Component.t list
(** [trim_components cs] is [cs] without leading and trailing whitespace. *)

val var_fallbacks : (string -> string option) -> Stylesheet.t -> Stylesheet.t
(** [var_fallbacks lookup stylesheet] gives every [var(--name)] without a
    fallback the value [lookup "--name"] answers as its fallback. *)

val universal_selector : Selector.t
(** [universal_selector] is [*]. *)

val selector_for_parents : Selector.t list -> Selector.t
(** [selector_for_parents parents] is the innermost enclosing selector, or
    {!universal_selector} at top level. *)

val substitute :
  kept:string list ->
  scopes:Inline_scope.scope_set ->
  parents:Selector.t list ->
  at_path:Inline_scope.at_node list ->
  Stylesheet.t ->
  Stylesheet.t
(** [substitute ~kept ~scopes ~parents ~at_path stmts] replaces each [var()] in
    [stmts] with the definition [scopes] makes visible to it. A name in [kept]
    keeps its references live. *)
