(** Helpers shared by the optimize test suites. *)

open Cascade

val hex_color : string -> Css.Values.color
(** [hex_color s] is the colour written [#s]. *)

val media_min_width : float -> Css.Media.t
(** [media_min_width px] is the media condition [(min-width: <px>px)]. *)

val minify : ?targets:Css.Optimize.targets -> Css.t -> string
(** [minify sheet] is [sheet] optimised for [targets] (default
    {!Css.Optimize.evergreen_targets}) and printed minified. *)

val nesting_targets : Css.Optimize.targets
(** [nesting_targets] are floors from which every target engine parses all of
    CSS Nesting, nested declarations rules included (MDN's browser-compat-data:
    Chrome 130, Firefox 132, Safari 18.2), so that the optimizer keeps and
    synthesises nesting. The default floors read none of it. *)

val statement_of_rule : Css.Stylesheet.rule -> Css.statement
(** [statement_of_rule r] is the statement holding the rule [r]. *)

val rule_of_statement : Css.statement -> Css.Stylesheet.rule
(** [rule_of_statement s] is the rule [s] holds.

    @raise Failure if [s] is not a rule. *)

val optimized_string :
  ?scope:Css.Optimize.scope ->
  ?objective:Css.Optimize.objective ->
  ?targets:Css.Optimize.targets ->
  ?enforce_spec:bool ->
  string ->
  string
(** [optimized_string css] reads [css] as a stylesheet, optimises it and prints
    it minified. *)

val minify_str : ?targets:Css.Optimize.targets -> string -> string
(** [minify_str css] parses [css] leniently, optimises it for [targets] (default
    {!Css.Optimize.evergreen_targets}) and prints it minified. *)
