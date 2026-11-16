(** Checkers shared by the selector test suites. *)

val check :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check s] reads [s] as a selector and checks its printed form. *)

val check_invalid : string -> string -> (unit -> unit) -> unit
(** [check_invalid name msg f] checks that [f] raises [Invalid_argument msg]. *)

val check_construct : string -> Cascade.Css.Selector.t -> unit
(** [check_construct expected sel] checks that [sel] minifies to [expected]. *)

val check_pretty_to : string -> string -> unit
(** [check_pretty_to expected input] checks the pretty form of [input]. *)

val check_spec_tuple :
  string ->
  Cascade.Css.Selector.specificity ->
  Cascade.Css.Selector.specificity ->
  unit
(** [check_spec_tuple name expected actual] compares two specificities. *)

val check_minified_to : string -> string -> unit
(** [check_minified_to expected input] checks that [input] minifies to
    [expected], reparses to its AST and keeps its specificity. *)

val check_minified_equiv : string -> unit
(** [check_minified_equiv input] checks that minifying [input] is idempotent and
    keeps its specificity. *)
