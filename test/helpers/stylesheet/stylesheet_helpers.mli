(** Helpers shared by the stylesheet test suites. *)

open Cascade

val minify : Css.t -> string
(** [minify sheet] is [sheet] optimised and printed minified. *)

val statement_of_rule : Css.Stylesheet.rule -> Css.statement
(** [statement_of_rule r] is the statement holding the rule [r]. *)

val check_rule :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_rule css] reads [css] as a rule and checks how it prints. *)

val decl_t : Css.Declaration.declaration Alcotest.testable
(** [decl_t] compares declarations structurally and shows them printed. *)

val check_import_rule :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_import_rule css] reads [css] as an [@import] rule and checks how it
    prints. *)

val check_layer_name :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_layer_name css] reads [css] as a layer name and checks how it prints.
*)

val check_declaration :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_declaration css] reads [css] as a declaration and checks how it
    prints. *)

val check_stylesheet :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_stylesheet css] reads [css] as a stylesheet and checks how it prints.
*)

val check :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check] is {!check_stylesheet}. *)

val assert_minify_and_optimize :
  string -> minified:string -> optimized:string -> unit
(** [assert_minify_and_optimize css ~minified ~optimized] checks that [css]
    prints as [minified] and, once optimised, as [optimized]. *)

val lenient_recover : string -> string -> string -> int -> unit
(** [lenient_recover name css expected min_warnings] checks that lenient parsing
    recovers [css] into [expected] with at least [min_warnings] warnings. *)

val specified_source_name : Css.Stylesheet.value_source -> string
(** [specified_source_name s] is the name the tests give the source [s]. *)

val check_specified : string -> string -> string -> Css.Stylesheet.value -> unit
(** [check_specified name value source actual] checks that [actual] holds
    [value] taken from [source]. *)

val pretty_preserves : string -> string list -> unit
(** [pretty_preserves css fragments] checks that pretty-printing [css] keeps
    every fragment of [fragments] verbatim. *)
