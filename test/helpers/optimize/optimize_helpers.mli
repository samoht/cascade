(** Helpers shared by the optimize test suites. *)

open Cascade

val hex_color : string -> Css.Values.color
(** [hex_color s] is the colour written [#s]. *)

val media_min_width : float -> Css.Media.t
(** [media_min_width px] is the media condition [(min-width: <px>px)]. *)

val minify : Css.t -> string
(** [minify sheet] is [sheet] optimised and printed minified. *)

val statement_of_rule : Css.Stylesheet.rule -> Css.statement
(** [statement_of_rule r] is the statement holding the rule [r]. *)

val rule_of_statement : Css.statement -> Css.Stylesheet.rule
(** [rule_of_statement s] is the rule [s] holds.

    @raise Failure if [s] is not a rule. *)

val optimized_string :
  ?scope:Css.Optimize.scope ->
  ?targets:Css.Optimize.targets ->
  ?enforce_spec:bool ->
  string ->
  string
(** [optimized_string css] reads [css] as a stylesheet, optimises it and prints
    it minified. *)

val minify_str : string -> string
(** [minify_str css] parses [css] leniently, optimises it and prints it
    minified. *)
