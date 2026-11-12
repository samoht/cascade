(** Checkers and constructors shared by the context test suites. *)

val decl_t : Cascade.Css.Declaration.declaration Alcotest.testable
(** [decl_t] compares declarations and prints them as CSS. *)

val stylesheet_t : Cascade.Css.Stylesheet.t Alcotest.testable
(** [stylesheet_t] compares stylesheets and prints them minified. *)

val matches : string -> string -> bool
(** [matches pattern text] is [true] iff the Perl regexp [pattern] matches
    [text]. *)

val px : float -> Cascade.Css.Media.value
(** [px v] is the media value [v]px. *)

val rem : float -> Cascade.Css.Media.value
(** [rem v] is the media value [v]rem. *)

val ident : string -> Cascade.Css.Media.value
(** [ident s] is the media identifier [s]. *)

val feature : string -> Cascade.Css.Media.value -> Cascade.Css.Media.t
(** [feature] is {!Cascade.Css.Media.feature}. *)

val container_feature : string -> Cascade.Media.value -> Cascade.Css.Container.t
(** [container_feature] is {!Cascade.Css.Container.feature}. *)

val declaration_with_value :
  Cascade.Css.Declaration.declaration ->
  string ->
  Cascade.Css.Declaration.declaration
(** [declaration_with_value decl v] is [decl] with its value replaced by [v],
    keeping its importance. *)

val check_eval_preserves : string -> ctx:Cascade.Css.Context.t -> string -> unit
(** [check_eval_preserves name ~ctx input] checks that evaluating the
    declaration [input] under [ctx] leaves it unchanged. *)

val check_eval :
  string ->
  ctx:Cascade.Css.Context.t ->
  ?layer_order:string list ->
  ?layer:string ->
  expected:string ->
  string ->
  unit
(** [check_eval name ~ctx ~expected input] checks that evaluating the
    declaration [input] under [ctx] gives the declaration [expected]. *)

val check_eval_idempotent :
  string ->
  ctx:Cascade.Css.Context.t ->
  ?layer_order:string list ->
  ?layer:string ->
  string ->
  unit
(** [check_eval_idempotent name ~ctx input] checks that evaluating the
    declaration [input] twice under [ctx] gives what evaluating it once does. *)

val check_eval_context_extension :
  string ->
  ?layer_order:string list ->
  ?layer:string ->
  weak_ctx:Cascade.Css.Context.t ->
  strong_ctx:Cascade.Css.Context.t ->
  expected:string ->
  string ->
  unit
(** [check_eval_context_extension name ~weak_ctx ~strong_ctx ~expected input]
    checks that evaluating [input] under [strong_ctx] gives [expected], and that
    evaluating under [weak_ctx] first changes nothing. *)

val stylesheet_of_string : string -> Cascade.Css.Stylesheet.t
(** [stylesheet_of_string s] is the stylesheet [s] reads as; the test fails if
    it does not parse. *)

val check_eval_stylesheet :
  string ->
  ctx:Cascade.Css.Context.t ->
  ?layer_order:string list ->
  ?layer:string ->
  expected:string ->
  string ->
  unit
(** [check_eval_stylesheet name ~ctx ~expected input] checks that evaluating the
    stylesheet [input] under [ctx] gives the stylesheet [expected]. *)
